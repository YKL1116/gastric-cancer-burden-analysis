# Table 2 | Gastric cancer, 1990–2023 | Number 部分
# 项目根目录运行：Rscript 04_Table2_percentage_change.R
# R >= 4.1；依赖 readxl、dplyr、tidyr。安装：install.packages(c("readxl", "dplyr", "tidyr"))
# 三份 Number 工作簿放入 data/；可用 TABLE2_DATA_DIR 和 TABLE2_OUTPUT_DIR 指定目录。
# 原脚本第 474–555 行从胃癌(number).csv 拆分工作簿；第 557–635 行计算发病数变化。
# 下文将相同的端点算法用于死亡数和 DALY 总量，不使用年龄标化率或 AAPC。
data_dir <- Sys.getenv("TABLE2_DATA_DIR", unset = "data")
output_dir <- Sys.getenv("TABLE2_OUTPUT_DIR", unset = "results")
countries <- c("中国" = "China", "日本" = "Japan", "大韩民国" = "South Korea")
measures <- c("Incidence", "Mortality", "DALY")

read_endpoints <- function(filename, measure_labels, measure_title) {
  path <- file.path(data_dir, filename)
  if (!file.exists(path)) stop("找不到文件：", path)
  raw <- readxl::read_excel(path)
  required <- c("age", "sex", "cause", "metric", "location", "measure", "year", "val", "lower", "upper")
  missing_columns <- setdiff(required, names(raw))
  if (length(missing_columns)) stop(filename, " 缺少列：", paste(missing_columns, collapse = ", "))

  endpoints <- raw |>
    dplyr::filter(
      age %in% c("全部", "All ages"), sex %in% c("合计", "Both"),
      cause %in% c("胃癌", "Stomach cancer"), metric %in% c("数量", "Number", "number"),
      measure %in% .env$measure_labels, location %in% names(countries), year %in% c(1990, 2023)
    ) |>
    dplyr::transmute(
      Measure = measure_title, Country = factor(unname(countries[location]), levels = unname(countries)),
      year = as.integer(year), val = as.numeric(val), lower = as.numeric(lower), upper = as.numeric(upper)
    )
  n <- table(endpoints$Country, factor(endpoints$year, levels = c(1990, 2023)))
  if (any(n != 1L)) stop(filename, " 中每国在 1990 和 2023 年应各有一行，请核对。")
  values <- as.matrix(endpoints[c("val", "lower", "upper")])
  if (any(!is.finite(values) | values <= 0) ||
      any(endpoints$lower > endpoints$val | endpoints$val > endpoints$upper)) {
    stop(filename, " 的点估计或区间无效，请核对后再取对数。")
  }
  endpoints
}

endpoints <- dplyr::bind_rows(
  read_endpoints("Incidence_Number.xlsx", c("发病率", "Incidence"), "Incidence"),
  read_endpoints("Deaths_Number.xlsx", c("死亡", "Deaths"), "Mortality"),
  read_endpoints("DALY_Number.xlsx", c("伤残调整生命年", "DALYs"), "DALY")
)

# 用原始精度计算。由各年的边际 95% UI 反推对数标准差，合并时假设两年
# 对数估计的协方差为 0。这是原稿使用的近似；未使用 GBD 跨年份配对 draws。
sd_log <- function(lower, upper) (log(upper) - log(lower)) / (2 * 1.96)
results <- endpoints |>
  tidyr::pivot_wider(names_from = year, values_from = c(val, lower, upper)) |>
  dplyr::mutate(
    Measure = factor(Measure, levels = measures),
    log_ratio = log(val_2023) - log(val_1990),
    sd_ratio = sqrt(sd_log(lower_1990, upper_1990)^2 + sd_log(lower_2023, upper_2023)^2),
    change_pct = 100 * (val_2023 / val_1990 - 1),
    change_lower = 100 * expm1(log_ratio - 1.96 * sd_ratio),
    change_upper = 100 * expm1(log_ratio + 1.96 * sd_ratio)
  ) |>
  dplyr::arrange(Measure, Country)

# 最后一层才取整。数值型中间结果可在 results 对象中查看。
format_number <- function(x) formatC(x, digits = 0, format = "f", big.mark = ",")
number_ui <- function(value, lower, upper) {
  sprintf("%s (%s–%s)", format_number(value), format_number(lower), format_number(upper))
}
table2_number <- results |>
  dplyr::transmute(
    Measure, Country,
    `1990 (95% UI)` = number_ui(val_1990, lower_1990, upper_1990),
    `2023 (95% UI)` = number_ui(val_2023, lower_2023, upper_2023),
    `Change, % (95% UI)` = chartr("-", "−", sprintf("%.2f (%.2f to %.2f)", change_pct, change_lower, change_upper))
  )
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_file <- file.path(output_dir, "Table_2_Number_percentage_change.csv")
utils::write.csv(table2_number, output_file, row.names = FALSE, fileEncoding = "UTF-8")
message("已导出：", normalizePath(output_file))
