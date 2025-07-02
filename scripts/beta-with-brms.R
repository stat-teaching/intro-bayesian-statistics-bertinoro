library(brms)

# https://www.andrewheiss.com/blog/2023/05/15/fancy-bayes-diffs-props/#bayesianly-with-brms

y <- rep(0:1, c(10, 3))
dat <- data.frame(y = y)

# priors
get_prior(y ~ 0 + Intercept, data = dat, family = bernoulli(link = "identity"))

fit <- brm(
    y ~ 0 + Intercept,
    data = dat,
    backend = "cmdstanr",
    prior = prior(beta(1, 1), class = "b", lb = 0, ub = 1),
    family = bernoulli(link = "identity")
)

get_prior(fit)

# more informative priors, centered on 0.5 but more precision

#curve(dbeta(x, 30, 30))

fit2 <- brm(
    y ~ 0 + Intercept,
    data = dat,
    backend = "cmdstanr",
    prior = prior(beta(30, 30), class = "b", lb = 0, ub = 1),
    family = bernoulli(link = "identity")
)

# informative and shifted

fit3 <- brm(
    y ~ 0 + Intercept,
    data = dat,
    backend = "cmdstanr",
    prior = prior(beta(70, 30), class = "b", lb = 0, ub = 1),
    family = bernoulli(link = "identity")
)

p <- as_draws_df(fit)$b_Intercept
p2 <- as_draws_df(fit2)$b_Intercept
p3 <- as_draws_df(fit3)$b_Intercept

par(mfrow = c(3, 1))

hist(p, xlim = c(0, 1), probability = TRUE, breaks = 50, main = "prior = beta(1,1)", col = "dodgerblue", ylim = c(0, 10))
curve(dbeta(x, 1, 1), add = TRUE)

hist(p2, xlim = c(0, 1), probability = TRUE, breaks = 50, main = "prior = beta(30,30)", col = "dodgerblue", ylim = c(0, 10))
curve(dbeta(x, 30, 30), add = TRUE)

hist(p3, xlim = c(0, 1), probability = TRUE, breaks = 50, main = "prior = beta(70,30)", col = "dodgerblue", ylim = c(0, 10))
curve(dbeta(x, 70, 30), add = TRUE)
