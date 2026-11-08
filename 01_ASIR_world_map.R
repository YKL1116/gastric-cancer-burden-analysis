# Figure 1a | GLOBOCAN 2024 胃癌年龄标化发病率（男女合计，每 100,000 人）
# 从项目根目录运行 Rscript 01_ASIR_world_map.R，或先设置环境变量 ASIR_PROJECT_DIR。
# 原始 Excel 放入 data/，图片写入 figures/。脚本采用 UTF-8 编码。
# 需要 R >= 4.1、ggplot2 >= 3.5，以及 readxl、dplyr、maps、cowplot、tidyr。
# 安装示例：install.packages(c("readxl", "dplyr", "maps", "ggplot2", "cowplot", "tidyr"))
library(ggplot2)

project_dir <- Sys.getenv("ASIR_PROJECT_DIR", unset = ".")
input_file <- file.path(project_dir, "data", "dataset-inc-both-sexes-in-2024-stomach.xlsx")
output_file <- file.path(project_dir, "figures", "Figure_1a_ASIR.png")
if (!file.exists(input_file)) stop("找不到发病率工作簿：", input_file)

# 1. 读取第一张工作表，用原稿的对照表匹配 maps::world 的区域名。
raw_data <- readxl::read_excel(input_file)
required_columns <- c("Label", "ASR (World)")
if (!all(required_columns %in% names(raw_data))) {
  stop("缺少必要列：", paste(setdiff(required_columns, names(raw_data)), collapse = ", "))
}

name_mapping <- c(
  "Bolivia (Plurinational State of)" = "Bolivia",
  "Bosnia Herzegovina" = "Bosnia and Herzegovina",
  "Brunei Darussalam" = "Brunei",
  "Congo, Republic of" = "Republic of Congo",
  "Congo, Democratic Republic of" = "Democratic Republic of the Congo",
  "Czechia" = "Czech Republic",
  "France (metropolitan)" = "France",
  "French Guyana" = "French Guiana",
  "The Republic of the Gambia" = "Gambia",
  "Gaza Strip and West Bank" = "Palestine",
  "France, Guadeloupe" = "Guadeloupe",
  "Iran, Islamic Republic of" = "Iran",
  "Côte d'Ivoire" = "Ivory Coast",
  "Korea, Democratic People's Republic of" = "North Korea",
  "Korea, Republic of" = "South Korea",
  "Lao People's Democratic Republic" = "Laos",
  "France, Martinique" = "Martinique",
  "Republic of Moldova" = "Moldova",
  "The Netherlands" = "Netherlands",
  "France, La Réunion" = "Reunion",
  "Russian Federation" = "Russia",
  "Viet Nam" = "Vietnam",
  "Eswatini" = "Swaziland",
  "Syrian Arab Republic" = "Syria",
  "Türkiye" = "Turkey",
  "United Kingdom" = "UK",
  "Tanzania, United Republic of" = "Tanzania",
  "United States of America" = "USA",
  "Curaçao" = "Curacao"
)

# 2. 左闭右开区间，14 归入最高组。调色板的名称与图例顺序一致。
breaks_fixed <- c(0, 2, 4, 6, 8, 10, 12, 14, Inf)
labels_fixed <- c("0–<2", "2–<4", "4–<6", "6–<8", "8–<10", "10–<12", "12–<14", "≥14")
rate_colors <- stats::setNames(
  c("#0d4e92", "#087fb5", "#c7e3ec", "#fdeb97", "#f89c59", "#ef3b2c", "#cb181d", "#67000d"),
  labels_fixed
)
map_colors <- c(rate_colors, "Data not available" = "grey80")

# 同一映射名称出现多次时，按原稿保留第一条有数值的记录。
ASIR_country <- raw_data |>
  dplyr::filter(Label != "Total") |>
  dplyr::transmute(
    Country = dplyr::recode(Label, !!!name_mapping, .default = Label),
    ASR = as.numeric(`ASR (World)`)
  ) |>
  dplyr::filter(!is.na(Country), !is.na(ASR)) |>
  dplyr::distinct(Country, .keep_all = TRUE) |>
  dplyr::mutate(ASR_group = cut(
    ASR, breaks = breaks_fixed, labels = labels_fixed,
    right = FALSE, include.lowest = TRUE, ordered_result = TRUE
  ))
print(dplyr::count(ASIR_country, ASR_group, .drop = FALSE))
if (anyNA(ASIR_country$ASR_group)) {
  warning("存在未能分组的 ASR，请检查原始数值。")
}

# 3. 拆分只用于地图匹配。两岛沿用同一国家的率值和组别。
islands <- ASIR_country |>
  dplyr::filter(Country == "Trinidad and Tobago") |>
  dplyr::select(ASR, ASR_group) |>
  tidyr::crossing(Country = c("Trinidad", "Tobago"))
ASIR_map <- ASIR_country |>
  dplyr::filter(Country != "Trinidad and Tobago") |>
  dplyr::bind_rows(islands)

world <- ggplot2::map_data("world") |>
  dplyr::filter(region != "Antarctica")
unmatched <- dplyr::anti_join(ASIR_map, dplyr::distinct(world, region),
                              by = c("Country" = "region"))
if (nrow(unmatched)) {
  message("以下数据行没有匹配到地图区域：")
  print(unmatched, n = Inf)
}
total <- world |>
  dplyr::left_join(ASIR_map, by = c("region" = "Country")) |>
  dplyr::mutate(ASR_group_plot = factor(
    dplyr::coalesce(as.character(ASR_group), "Data not available"),
    levels = names(map_colors), ordered = TRUE
  ))

# 4. 主图与放大图共用多边形和填色。保留原稿的经纬度绘图方式。
map_base <- ggplot(total, aes(x = long, y = lat, group = group, fill = ASR_group_plot)) +
  geom_polygon(colour = "black", linewidth = 0.12, show.legend = TRUE) +
  scale_fill_manual(values = map_colors, limits = names(map_colors), drop = FALSE) +
  theme_void()

# 后版图例：居中，两行五列，按行填充，标题居左，色键 0.45 cm。
world_map <- map_base +
  labs(fill = "ASIR per 100,000") +
  guides(fill = guide_legend(nrow = 2, ncol = 5)) +
  theme(
    legend.byrow = TRUE,
    legend.title.position = "left",
    legend.position = "bottom",
    legend.justification = "center",
    legend.box.just = "center",
    legend.box = "horizontal",
    legend.title = element_text(size = 10, hjust = 0.5),
    legend.text = element_text(size = 10, lineheight = 0.85),
    legend.key.size = grid::unit(0.45, "cm"),
    legend.spacing.x = grid::unit(0.08, "cm"),
    legend.box.margin = margin(-6, 0, 0, 0, unit = "pt")
  )

# 5. 局部标题按两版脚本原文。裁切坐标保留穿越窗口边界的多边形。
make_inset <- function(title, xlim, ylim) {
  map_base +
    coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE) +
    annotate("rect", xmin = xlim[1], xmax = xlim[2], ymin = ylim[1], ymax = ylim[2],
             fill = NA, colour = "black", linewidth = 0.5) +
    labs(title = title) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5,
                                margin = margin(b = 2, unit = "mm")),
      plot.title.position = "plot",
      plot.margin = margin(1, 1, 1, 1, unit = "mm")
    )
}
spacer <- ggplot() + theme_void()
bottom_row <- cowplot::plot_grid(
  spacer, make_inset("East Asia", c(122, 146), c(28, 48)),
  spacer, make_inset("Southeast Asia", c(90, 110), c(-2, 25)),
  spacer, make_inset("Central & South Asia", c(50, 85), c(15, 45)),
  spacer, make_inset("West Africa", c(-18, 15), c(4, 25)),
  spacer, make_inset("Eastern Europe", c(18, 42), c(44, 61)), spacer,
  ncol = 11, rel_widths = c(rep(c(0.1, 1), 5), 0.1)
)
final_plot <- cowplot::plot_grid(
  world_map, spacer, bottom_row, ncol = 1, rel_heights = c(1.8, 0.1, 0.6)
)

# 6. 原稿未记录手动导出的尺寸。这里固定单面板尺寸、分辨率及背景。
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
ggplot2::ggsave(filename = output_file, plot = final_plot, device = "png",
                width = 8.5, height = 6, units = "in", dpi = 600, bg = "white")
message("已导出：", normalizePath(output_file))
