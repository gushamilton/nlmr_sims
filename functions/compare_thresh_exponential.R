compare_thresh_exponential <- function(exposure, outcome, seed = "123", reps = 10) {
  
  
  
  
  replicate_internally <- function(n_rep) {
    seed = seed + n_rep
    dat <-  sim_mydata_outcomes_thresh_big(n = 100000, seed = seed)
    dat_with_both <- generate_all_sumstats(data = dat, exposure = exposure, outcome = outcome, k = 50)
    
    ## redefine data set
    
    
    ## define sumstats tibble
    sum_stats_dat = dat_with_both$ss
    
    sum_stats_dat %>%
      mutate(replicate = n_rep)
  }
  
  d <- map_dfr(1:reps, replicate_internally)
  
  p3 = d %>%
    # filter(beta_mr < quantile(beta_mr,0.975)) %>%
    # filter(beta_mr > quantile(beta_mr,0.025)) %>%
    filter(strata_method != "full") %>% 
    arrange(strata) %>%
    mutate(lower = beta_mr - 1.96 * se_mr, upper = beta_mr + 1.96 * se_mr) %>% 
    ggplot(aes(x = strata, y = beta_mr)) +
    geom_boxplot() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
    
    theme_bw() +
    facet_wrap(~strata_method, scales = "free") +
    labs( title = "MR effect estimates across strata" ) +
    ylab("Betas from replicates (interquartile ranges)")
  return(p3)
  
}
