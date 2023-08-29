generate_outcome_plots_replicates_with_selection <- function(exposure, outcome, seed = "123", reps = 10) {

  # set correlation of 0.5 and 5 % removed

  replicate_internally <- function(n_rep) {
  seed = seed + n_rep
  dat <-  sim_mydata_outcomes(n = 50000, seed = seed) %>%
    mutate(remove = rnorm_pre(x, mu = 0, sd = 1, r = 0.8)) %>%
    filter(remove > quantile(remove, 0.15)) 
    
  dat_with_both <- generate_all_sumstats(data = dat, exposure = exposure, outcome = outcome, k = 10)
  
  ## redefine data set
  
  
  ## define sumstats tibble
  sum_stats_dat = dat_with_both$ss
  
  sum_stats_dat %>%
    mutate(replicate = n_rep)
  }
  
  dat <-  sim_mydata_outcomes(n = 50000, seed = seed) %>%
    mutate(remove = rnorm_pre(x, mu = 0, sd = 1, r = 0.8)) %>%
    filter(remove > quantile(remove, 0.15)) 
  
  d <- map_dfr(1:reps, replicate_internally)
  make_figures_replicates(d,exposure,outcome,dat,reps, "figures/linear_selection/", "_linear_effect_with_selection_") 
}


