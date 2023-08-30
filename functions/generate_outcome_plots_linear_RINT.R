generate_outcome_plots_individual <- function(exposure, outcome, seed = "123") {
  
  dat <-  sim_mydata_outcomes(n = 100000, seed = seed)
  dat_with_both <- generate_all_sumstats(data = dat, exposure = exposure, outcome = outcome, k = 10)
  
  ## redefine data set
  dat_with_strat <- dat_with_both$wdata
  

  
  ## define sumstats tibble
  sum_stats_dat = dat_with_both$ss
  ## make figures

  p1 = sum_stats_dat %>%
    filter(strata_method != "full") %>% 
    arrange(strata) %>%
    mutate(lower = beta_gx - 1.96 * se_gx, upper = beta_gx + 1.96 * se_gx) %>% 
    ggplot(aes(x = strata, y = beta_gx)) +
    geom_hline(yintercept = sum_stats_dat$beta_gx[1], linetype = "dashed", color = "blue") +
    geom_point(size = 3) + 
    geom_errorbar(aes(ymin = lower, ymax = upper)) +
    theme_bw() +
    facet_wrap(~strata_method, scales = "free") +
    labs( title = "Gene-exposure effect estimates across strata" )
  
  
  p2 = sum_stats_dat%>%
    filter(strata_method != "full") %>% 
    arrange(strata) %>%
    mutate(lower = beta_gy - 1.96 * se_gy, upper = beta_gy + 1.96 * se_gy) %>% 
    ggplot(aes(x = strata, y = beta_gy)) +
    geom_hline(yintercept = sum_stats_dat$beta_gy[1], linetype = "dashed", color = "blue") +
    geom_point(size = 3) + 
    geom_errorbar(aes(ymin = lower, ymax = upper)) +
    theme_bw() +
    facet_wrap(~strata_method, scales = "free") +
    labs( title = "Gene-outcome effect estimates across strata" )
  
  p3 = sum_stats_dat %>%
    filter(strata_method != "full") %>% 
    arrange(strata) %>%
    mutate(lower = beta_mr - 1.96 * se_mr, upper = beta_mr + 1.96 * se_mr) %>% 
    ggplot(aes(x = strata, y = beta_mr)) +
    geom_hline(yintercept = sum_stats_dat$beta_mr[1], linetype = "dashed", color = "blue") +
    geom_point(size = 3) + 
    geom_errorbar(aes(ymin = lower, ymax = upper)) +
    theme_bw() +
    facet_wrap(~strata_method, scales = "free") +
    labs( title = "MR effect estimates across strata" )
  
  
  
  ######################
  ### FULL DATA SET
  ######################
  p4 = dat %>% ggplot(aes(x = .data[[exposure]] )) + 
    geom_histogram(fill = "grey", bins = 50) + 
    theme_bw() + labs(x = "exposure", title = paste0("Simulated exposure ", exposure ," distribution") )
  
  
  p5 = dat %>% ggplot(aes(x = .data[[outcome]] )) + 
    geom_histogram(fill = "grey", bins = 50) + 
    theme_bw() + labs(x = "outcome", title = paste0("Simulated outcome ", outcome ," distribution") )
  
  p6 = dat %>% ggplot(aes(x = .data[[exposure]], y = g)) +
    # geom_point(color = "grey") +
    geom_smooth(method = "gam", formula = y~ s(x), color = "blue") +
    geom_smooth(method = "lm", formula = y~ x, color = "red") +
    theme_bw() +
    labs(x = "exposure", y = "instrument", title = "gene exposure relationship")
  
  p7 = dat %>% ggplot(aes(x = .data[[outcome]], y = .data[[exposure]])) +
    # geom_point(color = "grey") +
    geom_smooth(method = "gam", formula = y~ s(x), color = "blue") +
    geom_smooth(method = "lm", formula = y~ x, color = "red") +
    theme_bw() +
    labs(x = "exposure", y = "outcome", title = "gene outcome relationship")
  
  
  master_plot = p1 / p2 / p3 / (p4  + p5) / ( p6 + p7)
  ggsave(master_plot, filename = paste0("figures/linear/", exposure,"_",outcome,"_linear_effect.pdf"), width = 13, height = 20)
}


