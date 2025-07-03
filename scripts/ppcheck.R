library(brms)

N <- 200
x <- rep(0:1, each = N/2)
y <- rnorm(N, 3 * x, 1)
dat <- data.frame(y, x)
dat$x <- factor(dat$x)

fit0 <- brm(y ~ 1, backend = "cmdstanr", data = dat)
fit1 <- brm(y ~ x, backend = "cmdstanr", data = dat)

pp_check(fit0)
pp_check(fit1)





