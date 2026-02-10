library(tidyverse)
library(purrr)

# --- Functional Pipeline with Custom Operators ---
`%>>%` <- function(lhs, rhs) rhs(lhs)

# --- S4 Class: Statistical Model Framework ---
setClass("ModelPipeline",
  representation(
    data = "data.frame",
    transforms = "list",
    model = "ANY",
    metrics = "list"
  )
)

setGeneric("fit", function(pipeline, formula, ...) standardGeneric("fit"))
setGeneric("evaluate", function(pipeline, ...) standardGeneric("evaluate"))

setMethod("fit", "ModelPipeline", function(pipeline, formula, method = "lm") {
  transformed <- reduce(pipeline@transforms, function(df, fn) fn(df), .init = pipeline@data)
  pipeline@model <- switch(method,
    "lm" = lm(formula, data = transformed),
    "glm" = glm(formula, data = transformed, family = binomial()),
    stop("Unknown method")
  )
  pipeline
})

# --- Bootstrap Confidence Intervals ---
bootstrap_ci <- function(data, statistic, R = 10000, conf = 0.95) {
  n <- nrow(data)
  boot_stats <- map_dbl(1:R, function(i) {
    idx <- sample.int(n, n, replace = TRUE)
    statistic(data[idx, ])
  })
  alpha <- (1 - conf) / 2
  list(
    estimate = statistic(data),
    ci_lower = quantile(boot_stats, alpha),
    ci_upper = quantile(boot_stats, 1 - alpha),
    se = sd(boot_stats)
  )
}

# --- Data Processing Pipeline ---
analyze_sales <- function(raw_data) {
  raw_data %>%
    mutate(
      date = as.Date(date),
      month = floor_date(date, "month"),
      log_revenue = log1p(revenue)
    ) %>%
    group_by(month, category) %>%
    summarise(
      total = sum(revenue, na.rm = TRUE),
      avg = mean(revenue, na.rm = TRUE),
      median = median(revenue, na.rm = TRUE),
      q95 = quantile(revenue, 0.95, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    group_by(category) %>%
    mutate(
      mom_growth = (total - lag(total)) / lag(total),
      rolling_avg = slider::slide_dbl(total, mean, .before = 2)
    ) %>%
    ungroup()
}
