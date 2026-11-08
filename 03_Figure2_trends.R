# Figure 2a–c | 1990–2023 年中日韩胃癌年龄标化率趋势（男女合计）
# 在项目根目录运行：Rscript 03_Figure2_trends.R
# 将 ASIR.xlsx、ASMR.xlsx、ASDALY.xlsx 放入 data/。也可用环境变量
# FIGURE2_DATA_DIR 指定数据目录，FIGURE2_OUTPUT_DIR 指定输出目录。
# 依赖 R >= 4.1、ggplot2 >= 3.4，以及 readxl、dplyr、cowplot。
# install.packages(c("readxl", "dplyr", "ggplot2", "cowplot"))
library(ggplot2)

data_dir <- Sys.getenv("FIGURE2_DATA_DIR", unset = "data")
output_dir <- Sys.getenv("FIGURE2_OUTPUT_DIR", unset = "figures")
years <- 1990:2023
country_names <- c("中国" = "China", "日本" = "Japan", "大韩民国" = "South Korea")
country_colors <- c("China" = "#FFBC99", "Japan" = "#B5D5C5", "South Korea" = "#A6CFE2")

# 在每份工作簿的第一张表中筛出原图使用的点估计（val）。
# 核对每国每年恰有一条记录，避免跨过缺失年份连线或重复绘点。
read_series <- function(filename, measures) {
  path <- file.path(data_dir, filename)
  if (!file.exists(path)) stop("找不到数据文件：", path)
  raw <- readxl::read_excel(path)
  required <- c("age", "measure", "metric", "sex", "cause", "location", "year", "val")
  missing_cols <- setdiff(required, names(raw))
  if (length(missing_cols)) stop(filename, " 缺少列：", paste(missing_cols, collapse = ", "))

  series <- raw |>
    dplyr::filter(
      age %in% c("年龄标准化", "Age-standardized", "Age standardized", "Age-Standardized"),
      measure %in% .env$measures, metric %in% c("率", "Rate"), sex %in% c("合计", "Both"),
      cause %in% c("胃癌", "Stomach cancer"), location %in% names(country_names)
    ) |>
    dplyr::transmute(
      country = factor(unname(country_names[location]), levels = names(country_colors)),
      year = as.numeric(year), val = as.numeric(val)
    ) |>
    dplyr::arrange(country, year)
  if (any(!is.finite(series$val)) || any(!series$year %in% years)) {
    stop(filename, " 含无效点估计或不属于 1990–2023 年的记录，请核对。")
  }
  counts <- table(series$country, factor(series$year, levels = years))
  if (any(counts != 1L)) stop(filename, " 筛选后三国每年应各有一条记录，请核对缺失或重复行。")
  series
}

# ASDR 为投稿图 c 中使用的缩写，此处对应年龄标化 DALY 率。
incidence <- read_series("ASIR.xlsx", c("发病率", "Incidence"))
mortality <- read_series("ASMR.xlsx", c("死亡", "Deaths", "Mortality"))
daly <- read_series("ASDALY.xlsx", c("伤残调整生命年", "DALYs"))

# 保留原稿的纵轴取整、刻度间隔和下界留白。三图均按每三年标注横轴。
trend_plot <- function(series, abbreviation, rate_label, round_to, tick_by, min_pad, stroke = 0.5) {
  y_min <- floor(min(series$val) / round_to) * round_to
  y_max <- ceiling(max(series$val) / round_to) * round_to
  pad <- max(min_pad, (y_max - y_min) * 0.03)

  ggplot(series, aes(year, val, colour = country)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.8, stroke = stroke) +
    scale_colour_manual(values = country_colors, name = NULL) +
    scale_x_continuous(breaks = seq(min(years), max(years), by = 3),
                       expand = expansion(mult = c(0.03, 0.02), add = c(0.5, 0))) +
    scale_y_continuous(limits = c(y_min - pad, y_max), breaks = seq(y_min, y_max, by = tick_by),
                       expand = expansion(mult = c(0, 0.05))) +
    labs(x = "Year", y = paste0("Age-standardized ", rate_label, " rate (per 100,000)"),
         title = paste("Trends in", abbreviation, "of gastric cancer")) +
    theme_classic(base_size = 12) +
    theme(
      legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold"),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      axis.line = element_line(colour = "black", linewidth = 0.5),
      axis.ticks = element_line(colour = "black", linewidth = 0.4),
      axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.9)
    ) +
    coord_cartesian(clip = "off")
}

p_a <- trend_plot(incidence, "ASIR", "incidence", 10, 5, 1, stroke = 0.6)
p_b <- trend_plot(mortality, "ASMR", "mortality", 10, 5, 1)
p_c <- trend_plot(daly, "ASDR", "DALY", 100, 100, 5)

# 与参考图相同，a、b 在上，c 等宽居中。导出尺寸为本次复现设置。
top <- cowplot::plot_grid(p_a, p_b, nrow = 1, labels = c("a", "b"), label_fontface = "plain")
bottom <- cowplot::plot_grid(NULL, p_c, NULL, nrow = 1, rel_widths = c(1, 2, 1),
                             labels = c("", "c", ""), label_fontface = "plain")
figure2 <- cowplot::plot_grid(top, bottom, ncol = 1)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
ggsave(file.path(output_dir, "Figure_2a-c_trends.png"), plot = figure2, width = 12,
       height = 8.4, units = "in", dpi = 600, bg = "white")
