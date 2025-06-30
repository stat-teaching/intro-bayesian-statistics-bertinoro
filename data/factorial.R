set.seed(2025)

# Factors
group <- c("Control", "Experimental")
anxiety <- c("Low", "Medium", "High")
n_per_cell <- 20
sd <- 1  # standard deviation of noise

# Define means with weak interaction + main effects
cell_means <- expand.grid(Group = group, Anxiety = anxiety)
cell_means$Mean <- c(0.2, 0.0, -0.2,   # Control
                     0.6, 0.4,  0.1)   # Experimental

# Simulate data
data <- do.call(rbind, lapply(1:nrow(cell_means), function(i) {
  mu <- cell_means$Mean[i]
  data.frame(
    Group = cell_means$Group[i],
    Anxiety = cell_means$Anxiety[i],
    DV = rnorm(n_per_cell, mean = mu, sd = sd)
  )
}))

saveRDS(data, here::here("data", "clean", "factorial.rds"))