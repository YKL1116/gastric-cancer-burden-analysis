# Figure 3a–c | 2023 年中日韩胃癌按性别、年龄分布的发病率、死亡率和 DALY 率
# 运行：Rscript 07_Figure3a_c_age_sex.R（从项目根目录运行，文件编码 UTF-8）
# 输入：data/年龄_性别_rate.xlsx，第一张工作表。输出：figures/Figure_3a-c_age_sex.png
# 可通过 FIG3ABC_DATA_DIR、FIG3ABC_OUTPUT_DIR 指定输入与输出目录。
# 需要 R >= 4.1、ggplot2 >= 3.5，以及 readxl、dplyr、cowplot。
# install.packages(c("readxl", "dplyr", "ggplot2", "cowplot"))
library(ggplot2)

input_path <- file.path(Sys.getenv("FIG3ABC_DATA_DIR", unset = "data"), "年龄_性别_rate.xlsx")
output_dir <- Sys.getenv("FIG3ABC_OUTPUT_DIR", unset = "figures")
if (!file.exists(input_path)) stop("找不到绘图用年龄别率工作簿：", input_path)
raw <- readxl::read_excel(input_path)
required <- c("measure", "metric", "cause", "location", "sex", "age", "year", "val")
missing <- setdiff(required, names(raw))
if (length(missing)) stop("工作簿缺少列：", paste(missing, collapse = ", "))

# 最终绘图工作簿已包含 0–14、15–39 岁组，直接取各组率，不重复合并五岁年龄层。
age_levels <- c("0-14", "15-39", sprintf("%d-%d", seq(40, 90, 5), seq(44, 94, 5)), "95+")
colors <- c("China" = "#FFCFAF", "Japan" = "#CFEBDD", "South Korea" = "#BFE4F5")
rate_labels <- c(Incidence = "incidence", Deaths = "mortality", DALYs = "DALY")

clean_age <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("岁及以上|岁以上|years?|yrs?|岁|[[:space:]]", "", x)
  x <- gsub("—|–|−|to|至", "-", x)
  dplyr::recode(x, "85-90" = "85-89", "95" = "95+", "≥95" = "95+",
                ">=95" = "95+", ">95" = "95+")
}

if ("population_group" %in% names(raw)) {
  raw <- dplyr::filter(raw, trimws(population_group) %in% c("All Population", "全部人口"))
}
rates <- raw |>
  dplyr::filter(year == 2023, trimws(cause) %in% c("胃癌", "Stomach cancer"),
                trimws(metric) %in% c("率", "Rate")) |>
  dplyr::transmute(
    measure = dplyr::recode(trimws(measure), "发病率" = "Incidence", "死亡" = "Deaths",
                            "伤残调整生命年" = "DALYs", "DALYs (Disability-Adjusted Life Years)" = "DALYs"),
    country = dplyr::recode(trimws(location), "中国" = "China", "日本" = "Japan",
                            "大韩民国" = "South Korea", "Republic of Korea" = "South Korea",
                            "Korea" = "South Korea"),
    sex = dplyr::recode(trimws(sex), "男" = "Male", "女" = "Female"),
    age = clean_age(age), rate = as.numeric(val)
  ) |>
  dplyr::filter(measure %in% names(rate_labels), country %in% names(colors),
                sex %in% c("Female", "Male"), age %in% age_levels) |>
  dplyr::mutate(
    age = factor(age, levels = age_levels), country = factor(country, levels = names(colors)),
    # 保留原稿绘图精度。原始率保存在 rate；取整仅用于 a、b 两图的色块宽度。
    bar_rate = dplyr::if_else(measure == "DALYs", rate, round(rate)),
    signed_rate = dplyr::if_else(sex == "Female", -bar_rate, bar_rate)
  )
key <- c("measure", "country", "sex", "age")
if (nrow(rates) != 252L || anyDuplicated(rates[key])) {
  stop("应有 3 指标 × 3 国家 × 2 性别 × 14 年龄组，每个组合各一条记录。")
}
if (any(!is.finite(rates$rate) | rates$rate < 0)) stop("原始率含无效值。")

# 按参考图镜像堆叠，零线向外依次为韩国、日本、中国。每个色块宽度为该国率；
# 三国采用不同人口分母，整个条形长度不表示三国合并率。
make_panel <- function(metric, xmin, xmax, tick_by) {
  d <- dplyr::filter(rates, measure == .env$metric)
  totals <- d |>
    dplyr::group_by(age, sex) |>
    dplyr::summarise(edge = sum(signed_rate), .groups = "drop")
  if (any(totals$edge < xmin | totals$edge > xmax)) stop(metric, "：固定坐标范围不足以显示全部色块。")
  ggplot(d, aes(x = signed_rate, y = age, fill = country)) +
    geom_col(width = 0.9, position = "stack", orientation = "y") +
    geom_vline(xintercept = 0, linewidth = 0.4) +
    annotate("text", x = c(xmin / 2, xmax / 2), y = Inf, label = c("Female", "Male"),
             vjust = -0.5, fontface = "bold", size = 4) +
    scale_x_continuous(breaks = seq(xmin, xmax, by = tick_by), labels = abs,
                       expand = expansion(mult = 0.02)) +
    scale_fill_manual(values = colors, breaks = names(colors), name = "Country") +
    coord_cartesian(xlim = c(xmin, xmax), clip = "off") +
    labs(x = sprintf("Age-specific %s rate (per 100,000)", rate_labels[[metric]]), y = "Age group") +
    theme_classic(base_size = 12) +
    theme(
      plot.margin = margin(22, 7, 5, 7),
      legend.position = "inside", legend.position.inside = c(0.015, 0.025),
      legend.justification.inside = c(0, 0), legend.background = element_blank(),
      legend.title = element_text(size = 10), legend.text = element_text(size = 9),
      legend.key.size = grid::unit(3.6, "mm"), legend.margin = margin(0, 0, 0, 0),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.6)
    )
}
p_a <- make_panel("Incidence", -1200, 2000, 400)
p_b <- make_panel("Deaths", -1500, 2000, 500)
p_c <- make_panel("DALYs", -12000, 18000, 4000)
figure3abc <- cowplot::plot_grid(p_a, p_b, p_c, nrow = 1, labels = letters[1:3],
                                label_fontface = "plain", align = "h", axis = "tb")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
output_file <- file.path(output_dir, "Figure_3a-c_age_sex.png")
ggsave(output_file, plot = figure3abc, width = 16, height = 4.8, units = "in", dpi = 600, bg = "white")
message("已导出：", normalizePath(output_file))
