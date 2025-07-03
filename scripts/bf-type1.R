library(MASS)
library(furrr)
library(BayesFactor)
library(ggplot2)
library(tidyverse)

p <- c(1, 5, 10, 30, 50, 100) # number of tests
n <- 50 # sample size
d <- 0  # effect size
r <- 0  # correlation between tests
nsim <- 1e3

simTest <- function(p, d = 0, n, r, nsim = 1000){
    sign_pval <- sign_pval_corrected <- sign_bf <- rep(NA, nsim)
    for(i in 1:nsim){
        R <- filor::rmat(rep(r, filor::ncor(p)))
        Y <- MASS::mvrnorm(
            n = n,
            mu = rep(d, p),
            Sigma = R
        )
        pvals <- apply(Y, 2, function(x) t.test(x)$p.value)
        bfs <- apply(Y, 2, function(x) ttestBF(x)@bayesFactor$bf)
        sign_pval[i] <- any(pvals <= 0.05)
        sign_pval_corrected[i] <- any(p.adjust(pvals, method = "bonferroni") <= 0.05)
        sign_bf[i] <- any(abs(bfs) >= log(4))
    }
    data.frame(sign_pval, sign_pval_corrected, sign_bf)
}

if(!file.exists("scripts/res.rds")){
    plan(multicore(workers = 6))
    res <- future_map(p, function(x) simTest(x, 0, n, r, 1000), .progress = TRUE, .options = furrr_options(seed = TRUE))
    saveRDS(res, "res.rds")
} else{
    res <- readRDS("scripts/res.rds")
}

names(res) <- paste0("p", p)
resd <- lapply(res, function(x) apply(x, 2, mean))
resd <- dplyr::bind_rows(resd, .id = "ntest")

resd |> 
    mutate(ntest = parse_number(ntest)) |> 
    pivot_longer(2:4) |> 
    ggplot(aes(x = ntest, y = value)) +
    facet_wrap(~name) +
    geom_point() +
    geom_line()

