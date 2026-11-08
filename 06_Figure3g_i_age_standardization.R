# Figure 3g–i | 1990–2023 年中日韩胃癌宽年龄组内直接标准化及趋势图
# 从项目根目录运行 Rscript 06_Figure3g_i_age_standardization.R
# 输入 data/年龄段标准化所需的原始数据.xlsx；输出目录默认 results/Figure_3g-i/。
# 可通过 FIG3_DATA_DIR、FIG3_OUTPUT_DIR 修改目录。需要 R >= 4.1、ggplot2 >= 3.5、
# scales >= 1.3，以及 readxl、dplyr、stringr、writexl、cowplot。
# 若需要原分析的 27 组 Joinpoint 输入（CSV 和 Excel），运行前设置
# Sys.setenv(FIG3_EXPORT_JOINPOINT = "1")。它们使用与图中完全相同的点估计。
library(ggplot2)

data_dir <- Sys.getenv("FIG3_DATA_DIR", unset = "data")
out_dir <- Sys.getenv("FIG3_OUTPUT_DIR", unset = file.path("results", "Figure_3g-i"))
input <- file.path(data_dir, "年龄段标准化所需的原始数据.xlsx")
if (!file.exists(input)) stop("找不到细年龄层原始数据：", input)
years <- 1990:2023
country_colors <- c("China" = "#FFBC99", "Japan" = "#B5D5C5", "South Korea" = "#A6CFE2")
rate_names <- c(Incidence = "incidence", Deaths = "mortality", DALYs = "DALY")
groups <- c("15-49", "50-69", "70plus")

# 1. 沿用计算稿中引自“S1 Table. 2023 GBD world population age standard”的小数权重。
# 固定顺序为 15–19、20–24、…、90–94、95+。每个宽年龄组分别归一化。
age_weights <- data.frame(
  age_start = seq(15, 95, 5),
  std_weight = c(0.082891, 0.078012, 0.075914, 0.073217, 0.068281, 0.061474, 0.055113,
                 0.049131, 0.043459, 0.036822, 0.029851,
                 0.022653, 0.015976, 0.010973, 0.006045, 0.002467, 0.000785),
  age_group = rep(groups, c(7, 4, 6))
) |>
  dplyr::group_by(age_group) |>
  dplyr::mutate(weight = std_weight / sum(std_weight)) |>
  dplyr::ungroup()
weight_check <- age_weights |>
  dplyr::group_by(age_group) |>
  dplyr::summarise(n_ages = dplyr::n(), sum_original_weights = sum(std_weight),
                   sum_standardized_weights = sum(weight), .groups = "drop")
stopifnot(all(abs(weight_check$sum_standardized_weights - 1) < 1e-10))

# 2. 精确匹配细年龄层，避免将同样以 15 开头的“15-49 years”误当作“15-19 years”。
ages <- c(sprintf("%d-%d years", seq(15, 90, 5), seq(19, 94, 5)), "95+ years")
raw <- readxl::read_excel(input)
text_cols <- c("population_group", "cause", "metric", "sex", "location", "measure", "age")
missing_cols <- setdiff(c(text_cols, "year", "val"), names(raw))
if (length(missing_cols)) stop("缺少必要列：", paste(missing_cols, collapse = ", "))
fine_rates <- raw |>
  dplyr::mutate(
    dplyr::across(dplyr::all_of(text_cols), stringr::str_squish),
    location = dplyr::recode(location, "Republic of Korea" = "South Korea",
                             "Korea, Republic of" = "South Korea"),
    measure = dplyr::if_else(startsWith(measure, "DALYs"), "DALYs", measure)
  ) |>
  dplyr::filter(population_group == "All Population", cause == "Stomach cancer", metric == "Rate",
                sex == "Both", location %in% names(country_colors), measure %in% names(rate_names),
                year %in% years, age %in% ages) |>
  dplyr::transmute(location, measure, year = as.integer(year),
                   age_start = age_weights$age_start[match(age, ages)], val = as.numeric(val))

# 3. 检查完整的 国家 × 指标 × 年份 × 细年龄层 组合；任何缺失或重复都停止计算。
keys <- c("location", "measure", "year", "age_start")
expected <- length(country_colors) * length(rate_names) * length(years) * length(ages)
if (nrow(fine_rates) != expected || anyDuplicated(fine_rates[keys])) {
  stop("应有 5,202 个唯一的国家、指标、年份、细年龄层组合，请检查缺失或重复记录。")
}
if (any(!is.finite(fine_rates$val) | fine_rates$val < 0)) stop("率中含无效数值。")

# R_group = Σ(细年龄层率 × 组内权重)。单位沿用原始率，每 10 万人。
rates <- fine_rates |>
  dplyr::left_join(age_weights, by = "age_start") |>
  dplyr::group_by(location, measure, age_group, year) |>
  dplyr::summarise(val = sum(val * weight), sum_w = sum(weight), .groups = "drop") |>
  dplyr::arrange(location, measure, age_group, year)
stopifnot(nrow(rates) == 918L, all(abs(rates$sum_w - 1) < 1e-10))
rates <- dplyr::select(rates, -sum_w)

# 4. 分段线性纵轴只改变显示距离。阈值以上距离缩小为 1/8，标签仍为实际率。
# 发病率和死亡率阈值为 20；DALY 率按绘图稿最终生效的设置取 1,000。
compressed_axis <- function(cutoff) {
  scales::new_transform(
    name = paste0("piecewise_", cutoff),
    transform = function(x) ifelse(x <= cutoff, x, cutoff + (x - cutoff) / 8),
    inverse = function(x) ifelse(x <= cutoff, x, cutoff + (x - cutoff) * 8),
    domain = c(0, Inf)
  )
}
make_panel <- function(metric, tag) {
  d <- rates |>
    dplyr::filter(measure == .env$metric) |>
    dplyr::mutate(country = factor(location, levels = names(country_colors)),
                  age = factor(age_group, levels = groups, labels = c("15–49", "50–69", "≥70")))
  if (metric == "DALYs") {
    cutoff <- 1000
    top <- max(1000, ceiling(max(d$val) / 1000) * 1000)
    ticks <- c(seq(0, 800, 200), seq(1000, top, 1000))
  } else {
    cutoff <- 20
    top <- max(500, ceiling(max(d$val) / 50) * 50)
    ticks <- c(seq(0, 20, 4), 50, 100, 150, 200, 300, 400, 500, 600)
  }
  label <- rate_names[[metric]]
  ggplot(d, aes(year, val, colour = country, group = country)) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.6, stroke = 0.3) +
    facet_wrap(~age, nrow = 1) +
    scale_colour_manual(values = country_colors, name = "Country") +
    scale_x_continuous(breaks = seq(1990, 2023, 5), expand = expansion(mult = c(0.05, 0.02))) +
    scale_y_continuous(transform = compressed_axis(cutoff), limits = c(0, top),
                       breaks = ticks[ticks <= top], labels = scales::label_number(accuracy = 1, big.mark = " "),
                       expand = expansion(mult = c(0, 0.02))) +
    labs(tag = tag, title = paste("Within-group age-standardized", label, "rates"),
         x = "Year", y = paste0(tools::toTitleCase(label), " rate (per 100,000)")) +
    theme_classic(base_size = 12) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.position = "inside", legend.position.inside = c(0.02, 0.97),
          legend.justification.inside = c(0, 1),
          strip.background = element_rect(fill = "white", colour = "black"),
          strip.text = element_text(face = "bold"),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
          axis.line = element_line(colour = "black", linewidth = 0.4))
}
panels <- Map(make_panel, names(rate_names), c("g", "h", "i"))
figure <- cowplot::plot_grid(plotlist = panels, ncol = 1, align = "v", axis = "lr")

# 5. 标准化结果直接进入上面的图；Excel 保留原稿中的五张工作表。
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
writexl::write_xlsx(list(
  Standardized_rates = rates, GBD2023_age_weights = age_weights, Weight_check = weight_check,
  Results_2023 = dplyr::mutate(dplyr::filter(rates, year == 2023), val = round(val, 3)),
  Recent_2019_2023 = dplyr::filter(rates, year >= 2019)
), file.path(out_dir, "GBD2023_within_group_age_standardized_rates.xlsx"))
ggsave(file.path(out_dir, "Figure_3g-i.png"), plot = figure, width = 10.6, height = 15,
       units = "in", dpi = 600, bg = "white")

if (Sys.getenv("FIG3_EXPORT_JOINPOINT") == "1") {
  csv_dir <- file.path(out_dir, "Joinpoint_age_standardized_inputs")
  xlsx_dir <- file.path(out_dir, "Joinpoint_age_standardized_27_Excel")
  dir.create(csv_dir, showWarnings = FALSE)
  dir.create(xlsx_dir, showWarnings = FALSE)
  series <- split(rates, list(rates$location, rates$measure, rates$age_group), drop = TRUE)
  for (s in series) {
    stopifnot(identical(s$year, years))
    stem <- gsub("[^A-Za-z0-9_]+", "_", paste(s$location[1], s$measure[1], s$age_group[1], sep = "_"))
    values <- s[c("year", "val")]
    utils::write.csv(values, file.path(csv_dir, paste0(stem, ".csv")), row.names = FALSE)
    writexl::write_xlsx(values, file.path(xlsx_dir, paste0(stem, ".xlsx")))
  }
}
message("已导出标准化结果及 Figure 3g–i：", normalizePath(out_dir))
