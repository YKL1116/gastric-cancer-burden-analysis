# Figure 2d–l | Joinpoint 导出结果：胃癌年龄标化发病率、死亡率及 DALY 率（1990–2023）
# 在项目根目录运行 Rscript 05_Figure2d_l_Joinpoint.R
# 输入：data/ 下 APC_ASIR_中国.xlsx、APC_ASIR_日本.xlsx、APC_ASIR_韩国.xlsx
#       及对应的 APC_ASMR_*.xlsx、APC_ASDALY_*.xlsx（共九份）。可设置环境变量 JOINPOINT_DATA_DIR、JOINPOINT_OUTPUT_DIR。
# 依赖：R >= 4.1，ggplot2 >= 3.4，readxl、cowplot。
# install.packages(c("readxl", "ggplot2", "cowplot"))
library(ggplot2)

data_dir <- Sys.getenv("JOINPOINT_DATA_DIR", unset = "data")
output_dir <- Sys.getenv("JOINPOINT_OUTPUT_DIR", unset = "figures")
country_file <- c("China" = "中国", "Japan" = "日本", "South Korea" = "韩国")
point_colors <- c("China" = "#FF8C66", "Japan" = "#A2C9B8", "South Korea" = "#A6CFE2")
line_colors <- list(
  "China" = c("#e73800", "#ff3f00", "#FF8C66", "#FFB199", "#FFD5CC"),
  "Japan" = c("#7FAE9E", "#B5D5C5", "#E6F2EC"),
  "South Korea" = c("#5B8FA8", "#7FB1C7", "#A6CFE2", "#CDE5F0", "#E9F3FA")
)
rate_labels <- c(ASIR = "incidence", ASMR = "mortality", ASDR = "DALY")

# 从工作表读取年度观测率、Joinpoint 已拟合的率、拐点位置和 APC。
# APC 列含数值及原始星号，没有精确 P 值。保留星号，不从拟合线反推检验结果。
read_export <- function(path) {
  if (!file.exists(path)) stop("找不到文件：", path)
  raw <- readxl::read_excel(path, .name_repair = "minimal")
  column <- function(aliases) {
    hits <- which(names(raw) %in% aliases)
    if (length(hits) != 1L) stop(basename(path), "：请核对列 ", aliases[1])
    raw[[hits]]
  }
  d <- data.frame(
    year = as.numeric(column(c("Year of diagnosis", "Year", "year"))),
    observed = as.numeric(column(c("Observed Age-Adjusted Rate/Trend", "Observed", "Observed Rate"))),
    fitted = as.numeric(column(c("Modeled Age-Adjusted Rate/Trend", "Modeled", "Fitted", "Trend"))),
    marker = trimws(as.character(column("Joinpoint location"))),
    apc = trimws(as.character(column("APC")))
  )
  d <- d[order(d$year), ]
  if (!identical(d$year, as.numeric(1990:2023)) ||
      any(!is.finite(d$observed)) || any(!is.finite(d$fitted))) {
    stop(basename(path), "：需要 1990–2023 年每年各一条有效的观测值与拟合值。")
  }
  d
}

build_panel <- function(metric, country) {
  # 图稿中年龄标化 DALY 率记为 ASDR，工作簿文件名使用 ASDALY。
  file_metric <- if (metric == "ASDR") "ASDALY" else metric
  file <- sprintf("APC_%s_%s.xlsx", file_metric, country_file[[country]])
  d <- read_export(file.path(data_dir, file))
  marked <- !is.na(d$marker) & nzchar(d$marker)
  bounds <- c(min(d$year), d$year[marked], max(d$year))
  n <- length(bounds) - 1L
  colors <- line_colors[[country]]
  tick_by <- 5
  if (metric == "ASDR") {
    tick_by <- if (country == "Japan") 100 else 200
    # 按原稿保留日本 ASDR 专用的三段绿色。
    if (country == "Japan") colors <- c("#6E9F8E", "#9CC5B3", "#C9E1D4")
  }
  if (any(diff(bounds) <= 0) || n > length(colors)) stop(file, "：请核对拐点与分段配色。")

  # 每段包括左右两个端点；相邻拟合线共用拐点。观测方块仍按原始年度数据绘制一次。
  parts <- lapply(seq_len(n), function(i) {
    rows <- d[d$year >= bounds[i] & d$year <= bounds[i + 1L], ]
    apc <- unique(stats::na.omit(rows$apc[!rows$year %in% d$year[marked]]))
    apc <- apc[nzchar(apc)]
    if (length(apc) != 1L) stop(file, "：时段 ", bounds[i], " 的 APC 缺失或不唯一。")
    value <- as.numeric(sub("[*]$", "", apc))
    if (!is.finite(value)) stop(file, "：无法读取 APC：", apc)
    star <- if (endsWith(apc, "*")) "*" else ""
    rows$segment <- sprintf("%d–%d  APC = %+.2f%%%s", bounds[i], bounds[i + 1L], value, star)
    rows
  })
  lines <- do.call(rbind, parts)
  labels <- unique(lines$segment)
  limits_y <- c(floor(min(d$observed, d$fitted) / tick_by), ceiling(max(d$observed, d$fitted) / tick_by)) * tick_by

  ggplot() +
    geom_line(data = lines, aes(year, fitted, colour = segment, group = segment), linewidth = 0.9) +
    geom_point(data = d, aes(year, observed), shape = 15, size = 2, colour = point_colors[[country]]) +
    scale_colour_manual(name = NULL, values = stats::setNames(colors[seq_len(n)], labels), breaks = labels) +
    guides(colour = guide_legend(ncol = 1)) +
    scale_x_continuous(breaks = seq(1990, 2023, by = 5), expand = expansion(mult = c(0.05, 0.01))) +
    scale_y_continuous(limits = limits_y, breaks = seq(limits_y[1], limits_y[2], by = tick_by)) +
    labs(title = sprintf("%s (%s)", metric, country), x = "Year",
         y = sprintf("Age-standardized %s rate (per 100,000)", rate_labels[[metric]])) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "top", legend.text = element_text(size = 10),
      legend.key.height = grid::unit(3.8, "mm"), legend.margin = margin(0, 0, 2, 0),
      panel.grid = element_blank(), axis.text.x = element_text(angle = 60, hjust = 1),
      axis.line = element_line(linewidth = 0.5, colour = "black"),
      axis.ticks = element_line(linewidth = 0.5, colour = "black")
    )
}

# 国家顺序与参考图相同，d–f 为 ASIR，g–i 为 ASMR，j–l 为 ASDR。
panels <- list()
for (metric in names(rate_labels)) {
  for (country in names(country_file)) {
    panels[[paste(metric, country)]] <- build_panel(metric, country)
  }
}
# 为三个国家预留等高的标题栏；三段与五段图例共存时，各列的标题和绘图区仍对齐。
bodies <- lapply(panels, function(p) p + theme(plot.title = element_blank()))
aligned <- cowplot::align_plots(plotlist = bodies, align = "hv", axis = "tblr")
cells <- lapply(seq_along(panels), function(i) {
  header <- cowplot::ggdraw() +
    cowplot::draw_label(panels[[i]]$labels$title, x = 0.53, size = 14, fontface = "bold") +
    cowplot::draw_label(letters[i + 3L], x = 0.012, hjust = 0, size = 15)
  cowplot::plot_grid(header, aligned[[i]], ncol = 1, rel_heights = c(0.085, 1))
})
panel_grid <- cowplot::plot_grid(plotlist = cells, ncol = 3)
figure <- cowplot::plot_grid(
  panel_grid,
  cowplot::ggdraw() + cowplot::draw_label("* indicates an APC marked as significant in the Joinpoint export.",
                                          x = 0.02, hjust = 0, size = 10),
  ncol = 1, rel_heights = c(1, 0.022)
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_file <- file.path(output_dir, "Figure_2d-l_Joinpoint.png")
ggsave(output_file, plot = figure, width = 15, height = 14.5, units = "in", dpi = 600, bg = "white")
message("已导出：", normalizePath(output_file))
