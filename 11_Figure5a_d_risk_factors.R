# Figure 5a–d | 胃癌危险因素：吸烟及高钠饮食，男女合计，1990–2023
# 在项目根目录运行：Rscript 11_Figure5a_d_risk_factors.R
# 输入为 data/ 下的 ASMR (percent).xlsx、DALY (percent).xlsx、
# ASMR (rate).xlsx、DALY (rate).xlsx。可设置 RISK_DATA_DIR、RISK_OUTPUT_DIR。
# R >= 4.1，ggplot2 >= 3.5，以及 readxl、dplyr、ggh4x、cowplot。
# install.packages(c("readxl", "dplyr", "ggplot2", "ggh4x", "cowplot"))
library(ggplot2)

input_dir <- Sys.getenv("RISK_DATA_DIR", unset = "data")
output_dir <- Sys.getenv("RISK_OUTPUT_DIR", unset = "figures")
years <- 1990:2023
risks <- c("Smoking", "Diet high in sodium")
country_colors <- c("China" = "#FFBC99", "Japan" = "#B5D5C5", "South Korea" = "#A6CFE2")
outcomes <- c("Deaths", "DALYs")

# 四份表分别读取并核对指标、单位。Percent 表的 Value 已是百分数；
# Rate 表的 Value 是每 10 万人归因率。两者均直接使用工作簿的点估计。
read_series <- function(file, metric, outcome) {
  path <- file.path(input_dir, file)
  if (!file.exists(path)) stop("找不到输入文件：", path)
  raw <- readxl::read_excel(path)
  required <- c("Measure", "Metric", "Risk", "Cause", "Location", "Age", "Sex", "Year", "Value")
  missing_cols <- setdiff(required, names(raw))
  if (length(missing_cols)) stop(file, " 缺少列：", paste(missing_cols, collapse = ", "))

  d <- raw |>
    dplyr::filter(Metric == .env$metric, Cause == "Stomach cancer", Age == "Age-standardized",
                  Sex == "Both sexes", Year %in% years, Risk %in% risks) |>
    dplyr::transmute(
      outcome = ifelse(startsWith(Measure, "DALYs"), "DALYs", Measure),
      risk = Risk, year = as.integer(Year), value = as.numeric(Value),
      country = dplyr::recode(Location, "Republic of Korea" = "South Korea", "Korea" = "South Korea")
    ) |>
    dplyr::filter(outcome == .env$outcome, country %in% names(country_colors)) |>
    dplyr::mutate(country = factor(country, levels = names(country_colors)),
                  outcome = factor(outcome, levels = outcomes))
  counts <- table(d$country, factor(d$risk, levels = risks), factor(d$year, levels = years))
  if (any(counts != 1L)) stop(file, "：三个国家、两项风险在 1990–2023 年应每年各有一条记录。")
  if (any(!is.finite(d$value) | d$value < 0) || (metric == "Percent" && any(d$value > 100))) {
    stop(file, " 存在无效的归因率或百分比。")
  }
  d
}
paf <- dplyr::bind_rows(read_series("ASMR (percent).xlsx", "Percent", "Deaths"),
                        read_series("DALY (percent).xlsx", "Percent", "DALYs"))
rates <- dplyr::bind_rows(read_series("ASMR (rate).xlsx", "Rate", "Deaths"),
                          read_series("DALY (rate).xlsx", "Rate", "DALYs"))

# a、c：每格为一个国家在对应年份的 PAF，保留两位小数。
# 每种风险独立定色阶；右侧图例从上向下由低到高，与参考图一致。
make_heatmap <- function(risk_name, low, high, color_breaks) {
  d <- dplyr::filter(paf, risk == risk_name)
  ggplot(d, aes(factor(year, levels = years), country, fill = value)) +
    geom_tile(colour = "white", linewidth = 0.25) +
    geom_text(aes(label = sprintf("%.2f", value)), size = 2) +
    facet_wrap(~ outcome, ncol = 1, labeller = as_labeller(
      c(Deaths = "Deaths (age-standardized PAF%)", DALYs = "DALYs (age-standardized PAF%)"))) +
    scale_x_discrete(breaks = as.character(seq(1990, 2023, 5)), expand = expansion(add = 0.5)) +
    scale_y_discrete(expand = expansion(add = 0.5)) +
    scale_fill_gradient(name = "PAF (%)", low = low, high = high, breaks = color_breaks,
                        guide = guide_colourbar(reverse = TRUE)) +
    labs(x = "Year", y = NULL, title = risk_name) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 12), panel.grid = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          strip.background = element_rect(fill = "white", colour = "black"),
          strip.text = element_text(face = "bold"),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5))
}

# b、d：死亡和 DALY 率分面，各用独立纵轴并从零开始，国家配色与前图一致。
make_rate_plot <- function(risk_name, death_step, daly_step) {
  d <- dplyr::filter(rates, risk == risk_name)
  y_axis <- function(step) {
    scale_y_continuous(limits = c(0, NA), breaks = scales::breaks_width(step),
                       expand = expansion(mult = c(0, 0.05)))
  }
  ggplot(d, aes(year, value, colour = country, group = country)) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 1.6, stroke = 0.3) +
    facet_wrap(~ outcome, nrow = 1, scales = "free_y", labeller = as_labeller(
      c(Deaths = "ASMR (attributable deaths rate)", DALYs = "ASDR (attributable DALY rate)"))) +
    ggh4x::facetted_pos_scales(y = list(y_axis(death_step), y_axis(daly_step))) +
    scale_x_continuous(breaks = seq(1990, 2023, 5), expand = expansion(mult = c(0, 0.02), add = c(1, 0))) +
    scale_colour_manual(values = country_colors, name = "Country") +
    labs(x = "Year", y = "Rate (per 100,000)") +
    theme_classic(base_size = 12) +
    theme(legend.position = "inside", legend.position.inside = c(0.99, 0.97),
          legend.justification.inside = c(1, 1), legend.background = element_blank(),
          strip.background = element_rect(fill = "white", colour = "black"),
          strip.text = element_text(face = "bold"),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
          axis.line = element_line(colour = "black", linewidth = 0.4))
}

p_a <- make_heatmap("Smoking", "#FFF1E6", "#D94801", c(12, 14, 16))
p_b <- make_rate_plot("Smoking", 2, 50)
p_c <- make_heatmap("Diet high in sodium", "#EAF4FF", "#2F80C0", seq(7.52, 7.60, 0.02))
p_d <- make_rate_plot("Diet high in sodium", 1, 30)
figure <- cowplot::plot_grid(p_a, p_b, p_c, p_d, ncol = 1, labels = letters[1:4],
                             label_fontface = "plain", label_x = 0.004,
                             rel_heights = c(0.9, 1.1, 0.9, 1.1))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
out <- file.path(output_dir, "Figure_5a-d_risk_factors.png")
ggsave(out, plot = figure, width = 11, height = 15, units = "in", dpi = 600, bg = "white")
message("已导出：", normalizePath(out))
