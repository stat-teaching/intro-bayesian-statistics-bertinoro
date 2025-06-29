set.seed(1402)
n <- 100

randomize_case <- function(strings) {
  sapply(strings, function(str) {
    chars <- strsplit(str, "")[[1]]
    randomized_chars <- sapply(chars, function(ch) {
      if (grepl("[A-Za-z]", ch)) {
        if (runif(1) < 0.5) {
          toupper(ch)
        } else {
          tolower(ch)
        }
      } else {
        ch
      }
    })
    paste(randomized_chars, collapse = "")
  })
}

# Predictors
coffee <- rpois(n, lambda = 3)
spritz <- rpois(n, lambda = 2)
stats_knowledge <- round(runif(n, 0, 10), 1)

# Software used
software_used <- factor(
    sample(c("R", "SPSS", "Excel"), size = n, replace = TRUE),
    levels = c("R", "SPSS", "Excel")
)

# Baseline intercepts for software (on log scale)
intercept <- 3           # baseline intercept for R users
intercept_spss_excel <- 4  # higher baseline stress for SPSS & Excel

# Spritz slope (0 for R, negative for others)
spritz_slope <- ifelse(software_used == "R", 0, -0.1)

# Linear predictor (log scale)
lp <- ifelse(software_used == "R",
             intercept + 0.05 * coffee - 0.08 * stats_knowledge,
             intercept_spss_excel + 0.05 * coffee + spritz_slope * spritz - 0.08 * stats_knowledge)

mu <- exp(lp)  # mean stress

# Gamma shape and scale (shape = 20 → roughly Gaussian)
shape <- 20
scale <- mu / shape

stress <- rgamma(n, shape = shape, scale = scale)
stress <- pmin(stress, 100)

phd_data <- data.frame(
    stress,
    coffee,
    spritz,
    stats_knowledge,
    software_used
)

fit <- lm(stress ~ coffee + spritz * software_used + stats_knowledge, 
          data = phd_data)

phd_data$age <- round(runif(n, 23, 30))
phd_data$software_used <- randomize_case(as.character(phd_data$software_used))
phd_data <- filor::add_random_na(phd_data, n = 20, exclude_cols = c("stress"))
write.csv(phd_data, "data/phd.csv")
