# Figure 4a–f、Table S44/S45 | 1990–2020、2020–2023 年胃癌绝对负担的 Shapley 分解
# 从项目根目录运行 Rscript 09_Figure4_TableS44_S45_decomposition.R
# 输入 data/数据.xlsx（胃癌数量）、data/人口.csv（人口数量）；输出 results/decomposition/。
# 可设置环境变量 DECOMP_DATA_DIR、DECOMP_OUTPUT_DIR。源文件编码 UTF-8。
# R >= 4.1；install.packages(c("readxl", "dplyr", "tidyr", "ggplot2", "cowplot", "openxlsx"))
library(ggplot2)
data_dir <- Sys.getenv("DECOMP_DATA_DIR", unset = "data")
out_dir <- Sys.getenv("DECOMP_OUTPUT_DIR", unset = file.path("results", "decomposition"))
years <- c(1990L, 2020L, 2023L)
countries <- c("China", "Japan", "South Korea")
outcomes <- c("Incidence", "Mortality", "DALYs")
ages <- c("<5", sprintf("%d-%d", seq(5, 90, 5), seq(9, 94, 5)), "95+")
components <- c("Population growth", "Aging", "Epidemiological change")
palette <- stats::setNames(c("#d62728", "#1f77b4", "#ff7f0e"), components)

# 1. 只用互斥年龄组、两性合计、数量数据。按国家、年份和年龄键连接人口及负担。
prepare_counts <- function(raw, is_population) {
  required <- c("measure", "metric", "sex", "location", "year", "age", "val")
  if (!is_population) required <- c(required, "cause")
  missing <- setdiff(required, names(raw))
  if (length(missing)) stop("输入缺少列：", paste(missing, collapse = ", "))
  if ("population_group" %in% names(raw)) {
    raw <- dplyr::filter(raw, population_group == "All Population")
  }
  raw <- raw |>
    dplyr::filter(metric == "Number", sex %in% c("Both", "Both sexes"), year %in% years) |>
    dplyr::mutate(country = dplyr::recode(location, "Republic of Korea" = "South Korea",
                                         "Korea" = "South Korea"),
                  age = sub(" years?$", "", trimws(age)), year = as.integer(year)) |>
    dplyr::filter(country %in% countries, age %in% ages)
  if (is_population) {
    dplyr::filter(raw, measure == "Population") |>
      dplyr::transmute(country, year, age, population = as.numeric(val))
  } else {
    dplyr::filter(raw, cause == "Stomach cancer") |>
      dplyr::mutate(measure = dplyr::recode(measure, "Deaths" = "Mortality",
                        "DALYs (Disability-Adjusted Life Years)" = "DALYs")) |>
      dplyr::filter(measure %in% outcomes) |>
      dplyr::transmute(country, year, age, measure, burden = as.numeric(val))
  }
}
pop <- prepare_counts(utils::read.csv(file.path(data_dir, "人口.csv"), fileEncoding = "UTF-8-BOM"), TRUE)
burden <- prepare_counts(readxl::read_excel(file.path(data_dir, "数据.xlsx")), FALSE)
keys <- c("country", "year", "age")
full_keys <- c(keys, "measure")
if (anyDuplicated(pop[keys]) || anyDuplicated(burden[full_keys])) stop("输入含重复的年龄层记录。")
if (any(!is.finite(pop$population) | pop$population <= 0) ||
    any(!is.finite(burden$burden) | burden$burden < 0)) stop("人口或负担数量无效。")
grid <- tidyr::expand_grid(country = countries, year = years, age = ages)
if (nrow(dplyr::anti_join(grid, pop, by = keys))) stop("缺少年龄别人口。")
missing <- dplyr::anti_join(tidyr::expand_grid(grid, measure = outcomes), burden, by = full_keys)
# 沿用本项目新版分解稿的处理，仅在下列三个儿童年龄层缺少死亡记录时补零。
# 在工作簿 Zero_filled 表逐条留痕；其他缺失均停止计算。
if (any(missing$measure != "Mortality" | !missing$age %in% c("<5", "5-9", "10-14"))) {
  print(missing)
  stop("存在预定补零范围以外的缺失，请核实原始数据。")
}
zero_filled <- dplyr::mutate(missing, burden = 0)
dat <- dplyr::bind_rows(burden, zero_filled) |>
  dplyr::left_join(pop, by = keys) |>
  dplyr::mutate(age = factor(age, levels = ages)) |>
  dplyr::arrange(country, measure, year, age)

# 2. 绝对负担 = 总人口 × Σ(年龄构成比 × 年龄别人均负担)。
# 依次将三个因素从基年替换到终年。每个因素的边际变化在 6 种替换顺序上取均值。
state_at <- function(d, y) {
  rows <- d[d$year == y, ]
  stopifnot(identical(as.character(rows$age), ages))
  list(P = sum(rows$population), A = rows$population / sum(rows$population),
       R = rows$burden / rows$population)
}
total <- function(s) s$P * sum(s$A * s$R)
orders <- list(c("P","A","R"), c("P","R","A"), c("A","P","R"),
               c("A","R","P"), c("R","P","A"), c("R","A","P"))
decompose <- function(country, measure, start, end) {
  d <- dat[dat$country == country & dat$measure == measure, ]
  base <- state_at(d, start)
  final <- state_at(d, end)
  effect <- c(P = 0, A = 0, R = 0)
  for (order in orders) {
    s <- base
    previous <- total(s)
    for (term in order) {
      s[[term]] <- final[[term]]
      current <- total(s)
      effect[term] <- effect[term] + current - previous
      previous <- current
    }
  }
  value <- unname(effect / length(orders))
  start_total <- sum(d$burden[d$year == start])
  end_total <- sum(d$burden[d$year == end])
  net <- end_total - start_total
  tol <- 1e-10 * max(1, start_total, end_total)
  stopifnot(abs(total(base) - start_total) < tol, abs(total(final) - end_total) < tol,
            abs(sum(value) - net) < tol)
  data.frame(country, measure, start, end, component = components, value, start_total, end_total,
             net, pct = if (net == 0) NA_real_ else 100 * value / net)
}
jobs <- tidyr::expand_grid(measure = outcomes, start = years[1:2], country = countries) |>
  dplyr::mutate(end = ifelse(start == 1990, 2020L, 2023L))
result <- dplyr::bind_rows(do.call(Map, c(list(f = decompose), as.list(jobs))))
stopifnot(nrow(result) == 54L)

# 3. 工作簿。表格最后一步按整数/两位小数显示；图、贡献百分比均使用未取整结果。
int_fmt <- function(x) formatC(x, format = "f", digits = 0, big.mark = ",")
display <- result |>
  dplyr::mutate(component = dplyr::recode(component, "Aging" = "Population aging"),
                cell = paste0(int_fmt(value), " (", ifelse(is.na(pct), "—", sprintf("%.2f%%", pct)), ")")) |>
  dplyr::select(start, Measure = measure, Country = country, net, component, cell) |>
  tidyr::pivot_wider(names_from = component, values_from = cell) |>
  dplyr::rename(`Overall difference` = net)
wb <- openxlsx::createWorkbook()
header <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#DFEBF4", wrapText = TRUE)
note <- paste("Percentage contributions were calculated relative to the net change in burden.",
              "Opposing contributions can yield percentages above 100% or below zero.")
for (i in 1:2) {
  sheet <- c("Table S44", "Table S45")[i]
  tab <- dplyr::select(dplyr::filter(display, start == years[i]), -start)
  openxlsx::addWorksheet(wb, sheet, gridLines = FALSE)
  title <- sprintf("%s. Decomposition of changes in absolute gastric cancer burden into contributions from population growth, population aging, and epidemiological change in China, Japan, and South Korea, %d–%d",
                   sheet, years[i], years[i+1])
  openxlsx::writeData(wb, sheet, title, startRow = 1)
  openxlsx::mergeCells(wb, sheet, cols = 1:6, rows = 1)
  openxlsx::addStyle(wb, sheet, openxlsx::createStyle(textDecoration = "bold", wrapText = TRUE),
                     rows = 1, cols = 1)
  openxlsx::setRowHeights(wb, sheet, 1, 34)
  openxlsx::writeData(wb, sheet, tab, startRow = 3, headerStyle = header)
  openxlsx::addStyle(wb, sheet, openxlsx::createStyle(numFmt = "#,##0"), cols = 3, rows = 4:12)
  openxlsx::setColWidths(wb, sheet, cols = 1:6, widths = c(16,18,21,27,27,30))
  openxlsx::writeData(wb, sheet, note, startRow = 14)
  openxlsx::mergeCells(wb, sheet, cols = 1:6, rows = 14)
  openxlsx::addStyle(wb, sheet, openxlsx::createStyle(wrapText = TRUE), rows = 14, cols = 1)
  openxlsx::setRowHeights(wb, sheet, 14, 32)
}
for (sheet in c("Numeric_results", "Zero_filled")) {
  openxlsx::addWorksheet(wb, sheet)
  openxlsx::writeData(wb, sheet, if (sheet == "Numeric_results") result else zero_filled, headerStyle = header)
  openxlsx::setColWidths(wb, sheet, cols = 1:10, widths = 21)
}

# 4. 双向堆叠显示三项绝对贡献，黑点是同一国家的净变化。坐标使用线性尺度。
make_panel <- function(metric, first_year, exponent) {
  d <- dplyr::filter(result, measure == metric, start == first_year) |>
    dplyr::mutate(country = factor(country, levels = countries),
                  component = factor(component, levels = components))
  dots <- dplyr::distinct(d, country, net)
  edges <- d |> dplyr::group_by(country) |>
    dplyr::summarise(left = sum(pmin(value, 0)), right = sum(pmax(value, 0)), .groups = "drop")
  limits <- range(edges$left, edges$right)
  limits <- limits + c(-1, 1) * 0.08 * max(1, diff(limits))
  label <- c(Incidence = "incident cases", Mortality = "deaths", DALYs = "DALYs")[[metric]]
  ggplot(d, aes(value, country, fill = component)) +
    geom_col(width = 0.60, orientation = "y") +
    geom_vline(xintercept = 0, linewidth = 0.35) +
    geom_point(data = dots, aes(net, country), inherit.aes = FALSE, size = 2.2) +
    scale_fill_manual(values = palette, breaks = components, name = NULL) +
    scale_x_continuous(breaks = pretty(limits, n = 6), labels = function(x) sprintf("%.1f", x / 10^exponent),
                       expand = expansion(0)) +
    coord_cartesian(xlim = limits) +
    labs(title = sprintf("Decomposition of changes in %s (%d–%d)", label, first_year, unique(d$end)),
         x = sprintf("Contribution to change in %s (×10^%d)", label, exponent), y = NULL) +
    theme_classic(base_size = 12) +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "top",
          legend.text = element_text(size = 10), axis.ticks.y = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.6))
}
panel_specs <- tidyr::expand_grid(metric = outcomes, first_year = years[1:2])
panel_specs$exponent <- c(5, 4, 5, 4, 7, 5)
panels <- do.call(Map, c(list(f = make_panel), as.list(panel_specs)))
figure4 <- cowplot::plot_grid(plotlist = panels, ncol = 2, labels = letters[1:6],
                              label_fontface = "plain", align = "hv", axis = "tblr")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
openxlsx::saveWorkbook(wb, file.path(out_dir, "Table_S44_S45_decomposition.xlsx"), overwrite = TRUE)
ggsave(file.path(out_dir, "Figure_4a-f_decomposition.png"), plot = figure4, width = 15,
       height = 10.8, units = "in", dpi = 600, bg = "white")
message("已导出两张表及 Figure 4a–f。补零记录数：", nrow(zero_filled))
