generate_outcome_plots_u_replicates <- function(exposure, outcome, seed = "123", reps = 10) {
  

  replicate_internally <- function(n_rep) {
  seed = seed + n_rep
  dat <-  sim_mydata_outcomes_u(n = 100000, seed = seed)
  dat_with_both <- generate_all_sumstats(data = dat, exposure = exposure, outcome = outcome, k = 10)
  
  ## redefine data set
  
  
  ## define sumstats tibble
  sum_stats_dat = dat_with_both$ss
  
  sum_stats_dat %>%
    mutate(replicate = n_rep)
  }
  
  d <- map_dfr(1:reps, replicate_internally)
  
  dat <-  sim_mydata_outcomes_u(n = 100000, seed = seed)
  make_figures_replicates(d,exposure,outcome,dat,reps, "figures/u_shaped/", "_u_effect_") 
}


