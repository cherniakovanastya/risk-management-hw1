# ============================================================
# Homework 1: Risk analysis for AAPL and S&P 500 (^GSPC)
# Period: the last 3 years from the script execution date
# ============================================================

# 1. Install missing packages and load libraries
required_packages <- c("quantmod", "PerformanceAnalytics", "ggplot2")
missing_packages <- required_packages[!(required_packages %in% rownames(installed.packages()))]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(quantmod)
  library(PerformanceAnalytics)
  library(ggplot2)
})

# The script should be run from the project root in RStudio.
if (!dir.exists("data")) dir.create("data", recursive = TRUE)

# 2. Download daily adjusted prices from Yahoo Finance
symbols <- c("AAPL", "^GSPC")
asset_names <- c("AAPL" = "Apple (AAPL)", "^GSPC" = "S&P 500 (^GSPC)")
end_date <- Sys.Date()
start_date <- seq(end_date, by = "-3 years", length.out = 2)[2]

message("Downloading data from ", start_date, " to ", end_date, "...")
price_list <- lapply(symbols, function(symbol) {
  x <- getSymbols(
    Symbols = symbol,
    src = "yahoo",
    from = start_date,
    to = end_date + 1,
    auto.assign = FALSE,
    warnings = FALSE
  )
  Ad(x)
})

prices <- do.call(merge, price_list)
colnames(prices) <- asset_names[symbols]

# Keep a local copy of downloaded prices
write.csv(
  data.frame(Date = index(prices), coredata(prices)),
  "data/adjusted_prices.csv",
  row.names = FALSE
)

# 3. Calculate logarithmic daily returns and remove NA values
returns <- na.omit(CalculateReturns(prices, method = "log"))
colnames(returns) <- asset_names[symbols]

write.csv(
  data.frame(Date = index(returns), coredata(returns)),
  "data/log_returns.csv",
  row.names = FALSE
)

# 4. Calculate VaR and Expected Shortfall
# PerformanceAnalytics reports risk as a positive loss value when invert = TRUE.
metric_rows <- lapply(colnames(returns), function(asset) {
  r <- returns[, asset, drop = FALSE]
  data.frame(
    Asset = asset,
    VaR_Historical_95 = as.numeric(VaR(r, p = 0.95, method = "historical", invert = TRUE)),
    VaR_Historical_99 = as.numeric(VaR(r, p = 0.99, method = "historical", invert = TRUE)),
    VaR_Gaussian_95 = as.numeric(VaR(r, p = 0.95, method = "gaussian", invert = TRUE)),
    VaR_Gaussian_99 = as.numeric(VaR(r, p = 0.99, method = "gaussian", invert = TRUE)),
    ES_Historical_95 = as.numeric(ES(r, p = 0.95, method = "historical", invert = TRUE)),
    Observations = NROW(r),
    stringsAsFactors = FALSE
  )
})
metrics <- do.call(rbind, metric_rows)

write.csv(metrics, "data/risk_metrics.csv", row.names = FALSE)

cat("\n--- RISK ANALYSIS RESULTS ---\n")
print(transform(
  metrics,
  VaR_Historical_95 = sprintf("%.4f%%", VaR_Historical_95 * 100),
  VaR_Historical_99 = sprintf("%.4f%%", VaR_Historical_99 * 100),
  VaR_Gaussian_95 = sprintf("%.4f%%", VaR_Gaussian_95 * 100),
  VaR_Gaussian_99 = sprintf("%.4f%%", VaR_Gaussian_99 * 100),
  ES_Historical_95 = sprintf("%.4f%%", ES_Historical_95 * 100)
), row.names = FALSE)

# 5. Prepare data for visualization
returns_df <- data.frame(
  Date = rep(index(returns), times = NCOL(returns)),
  Asset = rep(colnames(returns), each = NROW(returns)),
  Return = as.vector(coredata(returns))
)

# VaR lines are placed on the return axis as negative loss thresholds.
var_lines <- data.frame(
  Asset = rep(metrics$Asset, each = 2),
  Confidence = rep(c("Historical VaR 95%", "Historical VaR 99%"), times = NROW(metrics)),
  Threshold = -as.vector(t(metrics[, c("VaR_Historical_95", "VaR_Historical_99")]))
)

plot_var <- ggplot(returns_df, aes(x = Return)) +
  geom_histogram(aes(y = after_stat(density)), bins = 60,
                 fill = "#4C78A8", colour = "white", alpha = 0.75) +
  geom_density(colour = "#1F3552", linewidth = 0.8) +
  geom_vline(
    data = var_lines,
    aes(xintercept = Threshold, colour = Confidence, linetype = Confidence),
    linewidth = 0.9
  ) +
  facet_wrap(~ Asset, scales = "free_y", ncol = 1) +
  scale_colour_manual(values = c("Historical VaR 95%" = "#E45756",
                                 "Historical VaR 99%" = "#7A0019")) +
  labs(
    title = "Distribution of daily log returns and historical VaR",
    subtitle = paste0("Period: ", start_date, " to ", end_date),
    x = "Daily log return",
    y = "Density",
    colour = NULL,
    linetype = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

ggsave("data/var_chart.png", plot_var, width = 10, height = 8, dpi = 300)

# 6. Create a Markdown results table that can be copied into README.md
percent <- function(x) sprintf("%.4f%%", 100 * x)
readme_table <- c(
  "| Asset | Historical VaR 95% | Historical VaR 99% | Parametric VaR 95% | Parametric VaR 99% | Historical ES 95% |",
  "|---|---:|---:|---:|---:|---:|",
  apply(metrics, 1, function(x) paste0(
    "| ", x[["Asset"]], " | ",
    percent(as.numeric(x[["VaR_Historical_95"]])), " | ",
    percent(as.numeric(x[["VaR_Historical_99"]])), " | ",
    percent(as.numeric(x[["VaR_Gaussian_95"]])), " | ",
    percent(as.numeric(x[["VaR_Gaussian_99"]])), " | ",
    percent(as.numeric(x[["ES_Historical_95"]])), " |"
  ))
)
writeLines(readme_table, "data/readme_results_table.md")

cat("\nFiles created:\n")
cat("- data/adjusted_prices.csv\n")
cat("- data/log_returns.csv\n")
cat("- data/risk_metrics.csv\n")
cat("- data/readme_results_table.md\n")
cat("- data/var_chart.png\n")
