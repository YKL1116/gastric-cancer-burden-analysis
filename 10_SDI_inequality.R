# Figure 3g–r、Tables S46/S47 | SDI 健康不平等：胃癌粗率、ASR 及逐地区剔除分析
# 从项目根目录运行 Rscript 10_SDI_inequality.R。五份原始 Excel 放在 data/。
# 可用 SDI_DATA_DIR、SDI_OUTPUT_DIR 指定输入和输出路径。需要 R >= 4.1。
# install.packages(c("readxl", "dplyr", "tidyr", "ggplot2", "ggrepel", "cowplot", "openxlsx"))
input_dir <- Sys.getenv("SDI_DATA_DIR", unset = "data")
output_dir <- Sys.getenv("SDI_OUTPUT_DIR", unset = file.path("results", "SDI_inequality"))
years <- c(1990L, 2020L, 2023L)
measures <- c("Incidence", "Deaths", "DALYs (Disability-Adjusted Life Years)")
read_book <- function(file) as.data.frame(readxl::read_excel(file.path(input_dir, file)))
# 34 项名单依据 GBD 2023 Asia Life Expectancy Collaborators, Nature (2026), Figure 3。
# doi:10.1038/s41586-026-10739-6；其中 Mauritius、Seychelles 属该研究的 Southeast Asia 组。Georgia 使用国家 ID 35。
crosswalk <- data.frame(
  location = c(
    "Japan",
    "China",
    "Taiwan",
    "Singapore",
    "Malaysia",
    "Democratic People's Republic of Korea",
    "Republic of Korea",
    "Sri Lanka",
    "Indonesia",
    "Lao People's Democratic Republic",
    "Myanmar",
    "Timor-Leste",
    "Philippines",
    "Brunei Darussalam",
    "Bangladesh",
    "Cambodia",
    "Viet Nam",
    "India",
    "Maldives",
    "Kyrgyzstan",
    "Georgia",
    "Thailand",
    "Pakistan",
    "Uzbekistan",
    "Tajikistan",
    "Kazakhstan",
    "Mauritius",
    "Bhutan",
    "Turkmenistan",
    "Armenia",
    "Azerbaijan",
    "Nepal",
    "Mongolia",
    "Seychelles"
  ),
  
  location_id = c(
    67, 6, 8, 69, 13, 7, 68, 17,
    11, 12, 15, 19, 16, 66, 161, 10,
    20, 163, 14, 37, 35, 18, 165, 41,
    39, 36, 183, 162, 40, 33, 34, 164,
    38, 186
  ),
  
  stringsAsFactors = FALSE
)
locations <- read_book("亚洲国家.xlsx")
if (nrow(locations) != 34 || !setequal(locations$location, crosswalk$location)) stop("34 项名单与原稿不一致。")
s0 <- read_book("IHME_GBD_2023_SDI_1950_2023_Y2025M10D12.xlsx")
id_year <- c("location_id", "year")
keys <- c(id_year, "measure")
grid <- tidyr::expand_grid(location_id = crosswalk$location_id, year = years)
full_grid <- tidyr::expand_grid(grid, measure = measures)

check_grid <- function(d, expected, label) {
  k <- names(expected)
  if (anyDuplicated(d[k]) || nrow(d) != nrow(expected) ||
      nrow(dplyr::anti_join(expected, d, by = k)) > 0L) stop(label, "：地区、年份或指标缺失/重复。")
}
sdi <- s0 |>
  dplyr::filter(covariate_name_short == "sdi", sex == "Both", age_group_name == "All Ages",
                year_id %in% years, location_id %in% crosswalk$location_id) |>
  dplyr::transmute(location_id, location = location_name, year = year_id, SDI = mean_value)
check_grid(sdi, grid, "SDI")
stopifnot(all(sdi$location == crosswalk$location[match(sdi$location_id, crosswalk$location_id)]),
          all(is.finite(sdi$SDI) & sdi$SDI >= 0 & sdi$SDI <= 1))

select_locations <- function(d) {
  d <- dplyr::filter(d, location %in% crosswalk$location, year %in% years, sex == "Both")
  if ("population_group" %in% names(d)) d <- dplyr::filter(d, population_group == "All Population")
  ids <- crosswalk$location_id[match(d$location, crosswalk$location)]
  if ("location_id" %in% names(d) && !isTRUE(all(d$location_id == ids))) stop("国家名称与 ID 不符。")
  d$location_id <- ids
  d
}
pop <- read_book("人口（全球）.xlsx") |> select_locations() |>
  dplyr::filter(measure == "Population", metric == "Number", age == "All ages") |>
  dplyr::transmute(location_id, year, Population = val)
check_grid(pop, grid, "人口")
b <- read_book("流行病学指标（全球）.xlsx") |> select_locations() |>
  dplyr::filter(cause == "Stomach cancer", age == "All ages", measure %in% measures,
                metric %in% c("Number", "Rate")) |>
  dplyr::select(location_id, year, measure, metric, val)
check_grid(b, tidyr::expand_grid(full_grid, metric = c("Number", "Rate")), "粗率及数量")
crude <- tidyr::pivot_wider(b, names_from = metric, values_from = val) |>
  dplyr::rename(Rate_GBD = Rate)
asr <- read_book("流行病学指标ASR（全球）.xlsx") |> select_locations() |>
  dplyr::mutate(measure = ifelse(startsWith(measure, "DALYs"), measures[3], measure),
                age_key = tolower(gsub("[^A-Za-z]", "", age))) |>
  dplyr::filter(cause == "Stomach cancer", age_key %in% c("agestandardized", "agestandardised"),
                metric == "Rate", measure %in% measures) |>
  dplyr::transmute(location_id, year, measure, ASR = val)
check_grid(asr, full_grid, "ASR")
panel <- crude |>
  dplyr::inner_join(pop, by = id_year) |> dplyr::inner_join(sdi, by = id_year) |>
  dplyr::inner_join(asr, by = keys) |>
  dplyr::mutate(Rate = 1e5 * Number / Population)
check_grid(panel, full_grid, "分析面板")
stopifnot(all(panel$Population > 0), all(is.finite(as.matrix(panel[c("Population","Number","Rate","ASR","Rate_GBD")]))),
          all(panel$Number >= 0 & panel$ASR >= 0),
          all(abs(panel$Rate - panel$Rate_GBD) <= 1e-6 * pmax(panel$Rate, panel$Rate_GBD, 1e-12)))

# 按 SDI 升序，共同 SDI 的地区使用同一人口区间中点。每次剔除后重新计算人口权重和排名。
# 粗率的 Number 是实际负担，ASR 分析使用 ASR × Population / 100000 作为浓度曲线的合成量。
rank_data <- function(d) {
  
  d <- d[
    order(d$SDI, d$location_id),
    ,
    drop = FALSE
  ]
  
  d$weight <- d$Population / sum(d$Population)
  
  # SDI 完全相同的国家共享该组人口区间的中点。
  # 无并列值时：
  # rank = cumsum(population_share) - population_share / 2
  groups <- aggregate(
    Population ~ SDI,
    data = d,
    FUN = sum
  )
  
  groups <- groups[order(groups$SDI), ]
  
  shares <- groups$Population / sum(groups$Population)
  
  groups$rank <- cumsum(shares) - shares / 2
  
  d$rank <- groups$rank[
    match(d$SDI, groups$SDI)
  ]
  
  d
}


# ----------------------------
# 10. SII、CIX 和浓度曲线
# ----------------------------

estimate <- function(d) {
  
  d <- rank_data(d)
  
  if (
    length(unique(d$rank)) < 2L ||
    sum(d$Number) <= 0
  ) {
    stop("rank 无法用于回归，或总 Number 为零。")
  }
  
  # 人口份额为描述性人口权重，不作为已知逆方差权重解释
  fit <- stats::lm(
    Rate ~ rank,
    data = d,
    weights = weight,
    na.action = na.fail
  )
  
  sii <- unname(coef(fit)["rank"])
  
  # 浓度曲线使用 Number；完全相同的 SDI 合并
  cc <- aggregate(
    cbind(Population, Number) ~ SDI,
    data = d,
    FUN = sum
  )
  
  cc <- cc[order(cc$SDI), ]
  
  curve <- data.frame(
    x = c(
      0,
      cumsum(cc$Population) / sum(cc$Population)
    ),
    y = c(
      0,
      cumsum(cc$Number) / sum(cc$Number)
    )
  )
  
  # CIX = 1 - 2 × 梯形面积
  cix <- 1 - sum(
    diff(curve$x) *
      (head(curve$y, -1) + tail(curve$y, -1))
  )
  
  # 等价公式交叉检查
  cix_check <-
    2 * sum(d$Number * d$rank) / sum(d$Number) - 1
  
  if (
    !is.finite(sii) ||
    abs(cix - cix_check) > 1e-10
  ) {
    stop("SII/CIX 估计检查失败。")
  }
  
  list(
    SII = sii,
    CIX = cix,
    fit = fit,
    data = d,
    curve = curve
  )
}
# 共用估计函数。ASR 合成量仅用于 ASR-CIX，不解释为实际病例、死亡或 DALY 数。
asr_panel <- dplyr::mutate(panel, Rate = ASR, Number = ASR * Population / 1e5)
annual <- function(d) {
  d |> dplyr::group_by(measure, year) |>
    dplyr::group_modify(function(.x, .y) {
      z <- estimate(.x)
      data.frame(n = nrow(.x), SII = z$SII, CIX = z$CIX)
    }) |> dplyr::ungroup()
}
contrasts <- function(tab) {
  pairs <- data.frame(from = c(1990L, 2020L, 1990L), to = c(2020L, 2023L, 2023L))
  tidyr::expand_grid(measure = measures, pairs) |>
    dplyr::left_join(tab, by = c("measure", "from" = "year")) |>
    dplyr::left_join(tab, by = c("measure", "to" = "year"), suffix = c("_from","_to")) |>
    dplyr::transmute(measure, from, to, delta_SII = SII_to - SII_from, delta_CIX = CIX_to - CIX_from)
}
analyse <- function(d, kind) {
  full <- annual(d)
  deltas <- contrasts(full)
  excluded <- lapply(crosswalk$location_id, function(id) {
    sub <- annual(d[d$location_id != id, ])
    stopifnot(all(sub$n == 33L))
    dplyr::mutate(sub, excluded_id = id, excluded_location = crosswalk$location[match(id, crosswalk$location_id)])
  }) |> dplyr::bind_rows()
  trials <- excluded |>
    dplyr::group_by(excluded_id, excluded_location) |>
    dplyr::group_modify(function(.x, .y) contrasts(.x)) |> dplyr::ungroup()
  summary <- trials |>
    dplyr::left_join(deltas, by = c("measure","from","to"), suffix = c("", "_full")) |>
    dplyr::group_by(measure, from, to) |>
    dplyr::summarise(
      SII = dplyr::first(delta_SII_full), SII_min = min(delta_SII), SII_max = max(delta_SII),
      SII_same = sum(sign(delta_SII) == sign(delta_SII_full)),
      CIX = dplyr::first(delta_CIX_full), CIX_min = min(delta_CIX), CIX_max = max(delta_CIX),
      CIX_same = sum(sign(delta_CIX) == sign(delta_CIX_full)), N = dplyr::n_distinct(excluded_id),
      .groups = "drop") |>
    dplyr::arrange(match(measure, measures), match(paste(from, to), c("1990 2020", "2020 2023", "1990 2023")))
  stopifnot(nrow(summary) == 9L, all(summary$N == 34L), nrow(trials) == 306L)
  list(full = full, summary = summary, trials = dplyr::mutate(trials, Analysis = kind),
       excluded = dplyr::mutate(excluded, Analysis = kind))
}
cr <- analyse(panel, "Crude")
ar <- analyse(asr_panel, "ASR")
points <- cr$full

# 粗率图沿用原稿的 SII 散点、经验浓度曲线、人口气泡及中日韩标签。
cols <- c(
  "1990" = "#6699FF",
  "2020" = "#990000",
  "2023" = "#FFBC99"
)

plot_measure <- function(m, plot_years) {
  
  stopifnot(
    length(plot_years) == 2L,
    !anyDuplicated(plot_years)
  )
  
  yy <- as.character(plot_years)
  scatter <- lines <- curves <- bubbles <- list()
  
  for (y in plot_years) {
    
    # 直接调用新版原有 estimate()，不改变计算方法
    z <- estimate(
      panel[panel$measure == m & panel$year == y, ]
    )
    
    k <- as.character(y)
    scatter[[k]] <- z$data
    
    # SII：使用原人口加权 lm 的预测值
    new <- data.frame(
      rank = seq(0, 1, length.out = 101)
    )
    new$Rate <- as.numeric(
      predict(z$fit, newdata = new)
    )
    new$year <- y
    lines[[k]] <- new
    
    # CIX：直接使用原经验浓度曲线
    cv <- z$curve
    cv$year <- y
    curves[[k]] <- cv
    
    # 人口气泡定位到对应的经验曲线节点
    # 同 SDI 国家共享合并组的右端点，气泡仍表示各国人口
    d <- z$data
    j <- match(d$SDI, sort(unique(d$SDI))) + 1L
    d$x <- z$curve$x[j]
    d$y <- z$curve$y[j]
    bubbles[[k]] <- d
    
    # 确认绘图使用的估计与原结果表一致
    st_y <- points[
      points$measure == m & points$year == y,
    ]
    
    stopifnot(
      nrow(st_y) == 1L,
      isTRUE(all.equal(
        z$SII, st_y$SII, tolerance = 1e-10
      )),
      isTRUE(all.equal(
        z$CIX, st_y$CIX, tolerance = 1e-10
      ))
    )
  }
  
  bind_year <- function(x) {
    d <- do.call(rbind, x)
    d$year <- factor(d$year, levels = yy)
    rownames(d) <- NULL
    d
  }
  
  scatter_data <- bind_year(scatter)
  line_data <- bind_year(lines)
  curve_data <- bind_year(curves)
  bubble_data <- bind_year(bubbles)
  
  # 使用 ID 筛选中日韩，显示名称与原图一致
  label_data <- function(d) {
    d <- d[d$location_id %in% c(6, 67, 68), ]
    d$label <- c("China", "Japan", "Korea")[
      match(d$location_id, c(6, 67, 68))
    ]
    d
  }
  
  lab_sii <- label_data(scatter_data)
  lab_cix <- label_data(bubble_data)
  
  st <- points[points$measure == m, ]
  st <- st[match(plot_years, st$year), ]
  
  display <- if (m == measures[3]) "DALYs" else m
  
  rate_label <- switch(
    display,
    Incidence = "Incidence rate (per 100,000)",
    Deaths = "Death rate (per 100,000)",
    DALYs = "DALY rate (per 100,000)"
  )
  
  fraction_label <- switch(
    display,
    Incidence = "Cumulative fraction of incidence",
    Deaths = "Cumulative fraction of deaths",
    DALYs = "Cumulative fraction of DALY"
  )
  
  # 两类图共同的旧版样式
  common_style <- function() {
    list(
      ggplot2::scale_color_manual(
        name = "year",
        values = cols[yy]
      ),
      ggplot2::scale_fill_manual(
        name = "year",
        values = cols[yy]
      ),
      ggplot2::scale_size_area(
        name = "Population\n(million)",
        breaks = c(200, 400, 600, 800, 1000, 1200),
        max_size = 6
      ),
      ggplot2::scale_x_continuous(
        limits = c(0, 1),
        breaks = c(0, 0.25, 0.50, 0.75, 1.00)
      ),
      ggplot2::theme_bw(),
      ggplot2::theme(
        legend.position = "right"
      ),
      ggplot2::guides(
        size = ggplot2::guide_legend(
          order = 1,
          override.aes = list(
            colour = "black",
            fill = "black",
            alpha = 1
          )
        ),
        colour = ggplot2::guide_legend(order = 2),
        fill = "none"
      )
    )
  }
  
  # ----------------------------
  # SII 图：点估计位于框内左上角
  # ----------------------------
  
  p1 <- ggplot2::ggplot(
    scatter_data,
    ggplot2::aes(
      rank, Rate,
      colour = year,
      fill = year,
      group = year
    )
  ) +
    ggplot2::geom_point(
      ggplot2::aes(size = Population / 1e6),
      alpha = 0.8,
      shape = 16
    ) +
    ggplot2::geom_line(
      data = line_data,
      ggplot2::aes(
        rank, Rate,
        colour = year,
        group = year
      ),
      inherit.aes = FALSE,
      linewidth = 0.8
    ) +
    ggplot2::geom_point(
      data = lab_sii,
      ggplot2::aes(
        rank, Rate,
        size = Population / 1e6,
        fill = year,
        colour = year
      ),
      shape = 21,
      stroke = 0,
      alpha = 1,
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +
    ggrepel::geom_text_repel(
      data = lab_sii,
      ggplot2::aes(
        rank, Rate,
        label = label,
        colour = year
      ),
      inherit.aes = FALSE,
      size = 3,
      box.padding = 0.35,
      point.padding = 0.25,
      segment.color = "grey50",
      min.segment.length = 0,
      seed = 20261101,
      max.overlaps = Inf,
      show.legend = FALSE
    ) +
    
    
    # 第一个年份：固定在图框内左上角
    ggplot2::annotate(
      "text",
      x = 0.01,
      y = Inf,
      label = sprintf(
        "%s: SII = %.3f",
        st$year[1], st$SII[1]
      ),
      hjust = 0,
      vjust = 1.5,
      size = 3,
      colour = unname(cols[yy[1]])
    ) +
    
    # 第二个年份：位于第一行下方
    ggplot2::annotate(
      "text",
      x = 0.01,
      y = Inf,
      label = sprintf(
        "%s: SII = %.3f",
        st$year[2], st$SII[2]
      ),
      hjust = 0,
      vjust = 3.3,
      size = 3,
      colour = unname(cols[yy[2]])
    ) +
    
    common_style() +
    ggplot2::labs(
      x = "Relative rank by SDI (Asia)",
      y = rate_label
    )
  # ----------------------------
  # CIX 浓度曲线：点估计位于框内右下角
  # ----------------------------
  
  p2 <- ggplot2::ggplot(
    bubble_data,
    ggplot2::aes(
      x, y,
      colour = year,
      fill = year,
      group = year
    )
  ) +
    
    # 底部灰色边框
    ggplot2::annotate(
      "segment",
      x = 0, xend = 1,
      y = 0, yend = 0,
      linewidth = 0.8,
      colour = "grey40"
    ) +
    
    # 右侧灰色边框
    ggplot2::annotate(
      "segment",
      x = 1, xend = 1,
      y = 0, yend = 1,
      linewidth = 0.8,
      colour = "grey40"
    ) +
    
    # 棕色平等线
    ggplot2::annotate(
      "segment",
      x = 0, xend = 1,
      y = 0, yend = 1,
      linetype = 1,
      linewidth = 0.7,
      colour = "#CD853F"
    ) +
    
    # 全部国家的人口气泡
    ggplot2::geom_point(
      ggplot2::aes(size = Population / 1e6),
      alpha = 0.75,
      shape = 16
    ) +
    
    # 中日韩气泡
    ggplot2::geom_point(
      data = lab_cix,
      ggplot2::aes(
        x, y,
        size = Population / 1e6,
        colour = year
      ),
      shape = 16,
      alpha = 1,
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +
    
    # 保留新版经验浓度曲线，不使用 GAM 平滑
    ggplot2::geom_path(
      data = curve_data,
      ggplot2::aes(
        x, y,
        colour = year,
        group = year
      ),
      inherit.aes = FALSE,
      linewidth = 0.8
    ) +
    
    # 中日韩文字标注
    ggrepel::geom_text_repel(
      data = lab_cix,
      ggplot2::aes(
        x, y,
        label = label,
        colour = year
      ),
      inherit.aes = FALSE,
      size = 3,
      box.padding = 0.35,
      point.padding = 0.2,
      segment.color = "grey50",
      min.segment.length = 0,
      seed = 20261101,
      max.overlaps = Inf,
      show.legend = FALSE
    ) +
    
    # 第一个年份的 CIX 点估计
    ggplot2::annotate(
      "text",
      x = 0.98,
      y = 0.07,
      label = sprintf(
        "%s: CIX = %.3f",
        st$year[1], st$CIX[1]
      ),
      hjust = 1,
      vjust = 0,
      size = 3,
      colour = unname(cols[yy[1]])
    ) +
    
    # 第二个年份的 CIX 点估计
    ggplot2::annotate(
      "text",
      x = 0.98,
      y = 0.02,
      label = sprintf(
        "%s: CIX = %.3f",
        st$year[2], st$CIX[2]
      ),
      hjust = 1,
      vjust = 0,
      size = 3,
      colour = unname(cols[yy[2]])
    ) +
    
    common_style() +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      breaks = c(0, 0.25, 0.50, 0.75, 1.00)
    ) +
    ggplot2::labs(
      x = "Cumulative fraction of population ranked by SDI (Asia)",
      y = fraction_label
    )
  
  list(
    SII = p1,
    CIX = p2
  )
}
# 每行两幅 SII（1990/2020、2020/2023）、两幅 CIX（同顺序），分别标 g–j、k–n、o–r。
plots <- list()
for (m in measures) {
  earlier <- plot_measure(m, c(1990, 2020))
  later <- plot_measure(m, c(2020, 2023))
  plots <- c(plots, list(earlier$SII, later$SII, earlier$CIX, later$CIX))
}
figure <- cowplot::plot_grid(plotlist = plots, ncol = 4, labels = letters[7:18],
                             label_fontface = "plain", align = "hv", axis = "tblr")

# Table S46 和 S47。保留底层数值；留一范围是 34 次剔除后时段差值的最小值、最大值。
display_measure <- function(x) c("Incidence", "Mortality", "DALYs")[match(x, measures)]
s46 <- ar$full |> dplyr::arrange(match(measure, measures), year) |>
  dplyr::transmute(Measure = display_measure(measure), Year = year, SII, `Concentration index` = CIX)
# 三位有效数字；绝对值超过 1,000 时保留到个位，与补充表精度一致。
fmt <- function(x) {
  vapply(x, function(v) {
    if (v == 0) return("0")
    decimals <- max(0L, 2L - floor(log10(abs(v))))
    sprintf("%.*f", as.integer(decimals), v)
  }, character(1))
}
s47_tab <- function(x) {
  x |> dplyr::transmute(Measure = display_measure(measure), Period = paste(from, to, sep = "–"),
    `ΔSII` = fmt(SII), `LOO range of ΔSII` = paste(fmt(SII_min), "to", fmt(SII_max)),
    `SII consistency` = paste0(SII_same, "/", N),
    `ΔCIX` = fmt(CIX), `LOO range of ΔCIX` = paste(fmt(CIX_min), "to", fmt(CIX_max)),
    `CIX consistency` = paste0(CIX_same, "/", N))
}
wb <- openxlsx::createWorkbook()
sheets <- list("S46 ASR" = s46, "S47 A Crude" = s47_tab(cr$summary), "S47 B ASR" = s47_tab(ar$summary),
               "S47 numeric" = dplyr::bind_rows(Crude = cr$summary, ASR = ar$summary, .id = "Analysis"),
               "Full estimates" = dplyr::bind_rows(Crude = cr$full, ASR = ar$full, .id = "Analysis"),
               "LOO changes" = rbind(cr$trials, ar$trials), "LOO annual" = rbind(cr$excluded, ar$excluded),
               "Analysis panel" = panel, "34 locations" = crosswalk)
for (sheet in names(sheets)) {
  openxlsx::addWorksheet(wb, sheet, gridLines = FALSE)
  x <- sheets[[sheet]]
  openxlsx::writeDataTable(wb, sheet, x, tableStyle = "TableStyleMedium2")
  openxlsx::setColWidths(wb, sheet, seq_len(ncol(x)), widths = "auto")
  openxlsx::freezePane(wb, sheet, firstRow = TRUE)
}
for (col in 3:4) openxlsx::addStyle(wb, "S46 ASR", openxlsx::createStyle(numFmt = c("0.00","0.0000")[col-2]),
                                     rows = 2:10, cols = col)
for (sheet in c("S47 A Crude", "S47 B ASR")) {
  openxlsx::writeData(wb, sheet, "Consistency counts exclusions preserving the full-sample direction. LOO ranges are descriptive, not confidence intervals.", startRow = 12)
  openxlsx::mergeCells(wb, sheet, cols = 1:8, rows = 12)
}
openxlsx::writeData(wb, "34 locations", "Study locations follow GBD 2023 Asia Life Expectancy Collaborators. Nature (2026), Fig. 3; doi:10.1038/s41586-026-10739-6.", startCol = 4)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
openxlsx::saveWorkbook(wb, file.path(output_dir, "Tables_S46_S47_SDI.xlsx"), overwrite = TRUE)
ggplot2::ggsave(file.path(output_dir, "Figure_3g-r_SDI_crude.png"), plot = figure,
                width = 22, height = 15.5, units = "in", dpi = 450, bg = "white")
