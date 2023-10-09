run_linear_plots_meta_analysis <- function(exposure = "x", outcome = "y", seed = 124, replicates = 10, u = FALSE, n = 25000) {


mu = c(0, 0, 0)
sd = c(1, 1, 1)
r = c(0.3, 0.02, 0.1)
varnames = c("g", "x", "y")


run_internal_replicates <- function(replicates) {
set.seed = rnorm(replicates)
value = rnorm(1, mean = 0, sd = 0.3)

mydata <- sim_mydata_outcomes_ma(n = n, 
                            mu = mu,
                            sd = sd,
                            r = r, 
                            varnames = varnames,
                            seed = seed,
                            value = value,
                            u = u) %>%
  # mutate(x = x + x*g*0.1) %>%
  mutate(y = x * value + rnorm(n))


z <- generate_all_sumstats_ma(mydata, exposure = exposure, outcome = outcome) %>%
  mutate(true = value)
z %>%
  mutate(exposure = exposure, outcome = outcome)
}


map_dfr(1:replicates, run_internal_replicates)



}




