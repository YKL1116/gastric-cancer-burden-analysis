# Figure 3d–f | 1990–2023 年中日韩胃癌按性别分层的年龄标准化率
# 在项目根目录运行 Rscript 08_Figure3d_f_sex_trends.R
# 输入：data/ASIR.xlsx、data/ASMR.xlsx、data/ASDALY.xlsx（默认读取第一张工作表）。
# 可用 FIG3DF_DATA_DIR 和 FIG3DF_OUTPUT_DIR 指定数据目录和图片输出目录。
# R >= 4.1，ggplot2 >= 3.5，以及 readxl、dplyr、scales、cowplot。
# install.packages(c("readxl", "dplyr", "ggplot2", "scales", "cowplot"))
library(ggplot2)

data_dir <- Sys.getenv("FIG3DF_DATA_DIR", unset = "data")
output_dir <- Sys.getenv("FIG3DF_OUTPUT_DIR", unset = "figures")
years <- 1990:2023
country_colors <- c("China" = "#FFBC99", "Japan" = "#B5D5C5", "South Korea" = "#A6CFE2")

# 每份工作簿只提取对应指标的男、女年度点估计。
read_rates <- function(file, measures) {
  path <- file.path(data_dir, file)
  if (!file.exists(path)) stop("找不到文件：", path)
  raw <- readxl::read_excel(path)
  cols <- c("measure", "cause", "age", "sex", "metric", "location", "year", "val")
  missing_cols <- setdiff(cols, names(raw))
  if (length(missing_cols)) stop(file, " 缺少列：", paste(missing_cols, collapse = ", "))

  d <- raw |>
    dplyr::filter(
      trimws(cause) %in% c("胃癌", "Stomach cancer"), trimws(measure) %in% .env$measures,
      trimws(age) %in% c("年龄标准化", "Age-standardized", "Age standardized", "Age-Standardized"),
      trimws(metric) %in% c("率", "Rate"), year %in% years
    ) |>
    dplyr::transmute(
      country = dplyr::recode(trimws(location), "中国" = "China", "日本" = "Japan",
                              "大韩民国" = "South Korea", "Korea" = "South Korea",
                              "Republic of Korea" = "South Korea"),
      sex = dplyr::recode(trimws(sex), "女" = "Female", "男" = "Male"),
      year = as.integer(year), val = as.numeric(val)
    ) |>
    dplyr::filter(country %in% names(country_colors), sex %in% c("Female", "Male")) |>
    dplyr::mutate(country = factor(country, levels = names(country_colors)),
                  sex = factor(sex, levels = c("Female", "Male"))) |>
    dplyr::arrange(country, sex, year)

  # 3 国 × 2 性别 × 34 年；缺年、重复或无效数值时停止，避免连出错误的曲线。
  counts <- table(d$country, d$sex, factor(d$year, levels = years))
  if (any(counts != 1L)) stop(file, "：每个国家和性别在 1990–2023 年应每年各有一行。")
  if (any(!is.finite(d$val) | d$val < 0)) stop(file, "：率中有缺失或无效值。")
  d
}

incidence <- read_rates("ASIR.xlsx", c("发病率", "Incidence"))
mortality <- read_rates("ASMR.xlsx", c("死亡", "Mortality", "Deaths"))
daly <- read_rates("ASDALY.xlsx", c("伤残调整生命年", "DALYs", "ASDALY", "ASDALYs",
                                    "DALYs (Disability-Adjusted Life Years)"))

# 同指标的女性与男性分面共用纵轴，图例置于左侧女性分面内。
standard_x <- expansion(mult = c(0.05, 0.02))
make_panel <- function(d, title, y_name, ymin, y_step, x_expand = standard_x) {
  if (min(d$val) < ymin) stop(title, "：最低数据点低于原稿纵轴下限。")
  ggplot(d, aes(year, val, colour = country, group = country)) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 1.6, stroke = 0.3) +
    facet_wrap(~sex, nrow = 1) +
    scale_colour_manual(values = country_colors, name = "Country") +
    scale_x_continuous(breaks = seq(min(years), max(years), by = 5), expand = x_expand) +
    scale_y_continuous(limits = c(ymin, NA), breaks = scales::breaks_width(y_step),
                       expand = expansion(mult = c(0, 0.05))) +
    labs(title = title, x = "Year", y = paste0(y_name, " (per 100,000)")) +
    theme_classic(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "inside", legend.position.inside = c(0.015, 0.975),
      legend.justification.inside = c(0, 1), legend.background = element_blank(),
      legend.key.height = grid::unit(4, "mm"), legend.margin = margin(0, 0, 0, 0),
      strip.background = element_rect(fill = "white", colour = "black"),
      strip.text = element_text(face = "bold"),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      axis.line = element_line(colour = "black", linewidth = 0.4)
    )
}

p_d <- make_panel(incidence, "Age-standardized incidence rate", "ASIR", 5, 10)
p_e <- make_panel(mortality, "Age-standardized mortality rate", "ASMR", 0, 10)
p_f <- make_panel(daly, "Age-standardized DALY rate", "ASDALY rate", 0, 200,
                  x_expand = expansion(mult = c(0, 0.02), add = c(1, 0)))

# 保留完整 Figure 3 中的位置：d、e 在上排，f 在下一排左侧。
figure <- cowplot::plot_grid(p_d, p_e, p_f, NULL, ncol = 2, labels = c("d", "e", "f", ""),
                             label_fontface = "plain", align = "hv", axis = "tblr")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_file <- file.path(output_dir, "Figure_3d-f_sex_trends.png")
ggsave(output_file, plot = figure, width = 14.4, height = 9.2, units = "in", dpi = 600, bg = "white")
message("已导出：", normalizePath(output_file))
