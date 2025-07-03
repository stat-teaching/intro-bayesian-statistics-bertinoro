library(brms)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggeffects)
library(effects)

# https://vuorre.com/posts/2017-01-02-how-to-compare-two-groups-with-robust-bayesian-estimation-using-r-stan-and-brms/

N <- 100
g0 <- rnorm(N/2, 0, 1)
g1 <- rnorm(N/2, 0.5, 2)

y <- c(g0, g1)
x <- rep(0:1, each = N/2)

boxplot(y ~ x)


# a more model-like way to simulate the data, brms is going to fit this

b0_mu <- 0
b1_mu <- 0.5
b0_sigma <- log(1)
b1_sigma <- log(2)

y2 <- rnorm(
    N,
    mean = b0_mu + b1_mu * x,
    sd = exp(b0_sigma + b1_sigma * x)
)

dat <- data.frame(y, x)

# equal variance t.test
t.test(y ~ x, data = dat, var.equal = TRUE)

# unequal variance t.test, the default
t.test(y ~ x, data = dat, var.equal = FALSE)

fit_equal <- brm(
    y ~ x,
    data = dat,
    family = gaussian(link = "identity")
)

fit_unequal <- brm(
    bf(
        y ~ x + (1|id),
        sigma ~ x + (1|id)
    ),
    data = dat,
    family = gaussian(link = "identity")
)

fit_equal |> 
    as_draws_df() |> 
    pivot_longer(starts_with("b_")) |> 
    ggplot(aes(x = value)) +
    geom_histogram(bins = 50, fill = "dodgerblue", col = "black") +
    facet_wrap(~name, scales = "free")

fit_unequal |> 
    as_draws_df() |> 
    pivot_longer(starts_with("b_")) |> 
    mutate(value = ifelse(grepl("sigma", name), exp(value), value),
           param = ifelse(grepl("sigma", name), "sigma", "mu"),
           name = gsub("_sigma", "", name)) |> 
    ggplot(aes(x = value)) +
    geom_histogram(bins = 50, fill = "dodgerblue", col = "black") +
    facet_grid(param~name, scales = "free")

fit_gam <- gamlss(y ~ x, data = dat, sigma.formula = ~ x)
summary(fit_gam)
