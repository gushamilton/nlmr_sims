master_plot_everything <- function(dat = dat, sum_stats_dat = sum_stats_dat, d = d, exposure = exposure, outcome = outcome) {
  
  ## make figures
  
  
  r2_gene_exposure <- round(sum_stats_dat$r2_gene_exposure[1], digits = 4)
  r2_gene_outcome <- round(sum_stats_dat$r2_gene_outcome[1], digits = 4)
  r2_exposure_outcome<- round(sum_stats_dat$r2_exposure_outcome[1], digits = 4)
  
  p_gene_exposure <- signif(sum_stats_dat$p_gx[1], digits = 4)
  p_gene_outcome <- signif(sum_stats_dat$p_gy[1], digits = 4)
  p_exposure_outcome<- signif(sum_stats_dat$p_mr[1], digits = 4)
  ## make figures
  
  p1 = d %>%
    filter(strata_method != "full") %>% 
    arrange(strata) %>%
    mutate(lower = beta_gx - 1.96 * se_gx, upper = beta_gx + 1.96 * se_gx) %>% 
    ggplot(aes(x = strata, y = beta_gx)) +
    geom_boxplot() +
    geom_hline(yintercept = sum_stats_dat$beta_gx[1], linetype = "dashed", color = "blue") +
    theme_bw() +
    facet_wrap(~strata_method, scales = "free") +
    labs( title = "Gene-exposure effect estimates across strata" )
  
  
  p2 = d%>%
    filter(strata_method != "full") %>% 
    arrange(strata) %>%
    mutate(lower = beta_gy - 1.96 * se_gy, upper = beta_gy + 1.96 * se_gy) %>% 
    ggplot(aes(x = strata, y = beta_gy)) +
    geom_boxplot() +
    geom_hline(yintercept = sum_stats_dat$beta_gy[1], linetype = "dashed", color = "blue") +
    
    theme_bw() +
    facet_wrap(~strata_method, scales = "free") +
    labs( title = "Gene-outcome effect estimates across strata" )
  
  p3 = d %>%
    filter(beta_mr < quantile(beta_mr,0.95)) %>%
    filter(beta_mr > quantile(beta_mr,0.05)) %>%
    filter(strata_method != "full") %>% 
    arrange(strata) %>%
    mutate(lower = beta_mr - 1.96 * se_mr, upper = beta_mr + 1.96 * se_mr) %>% 
    ggplot(aes(x = strata, y = beta_mr)) +
    geom_boxplot() +
    geom_hline(yintercept = sum_stats_dat$beta_mr[1], linetype = "dashed", color = "blue") +
    
    theme_bw() +
    facet_wrap(~strata_method, scales = "free") +
    labs( title = "MR effect estimates across strata (excluding top and bottom 2.5%)" )
  
  
  
  ######################
  ### FULL DATA SET
  ######################
  p4 = dat %>% ggplot(aes(x = .data[[exposure]] )) + 
    geom_histogram(fill = "grey", bins = 50) + 
    theme_bw() + labs(x = "exposure", title = paste0("Simulated exposure ", exposure ," distribution") )
  
  
  p5 = dat %>% ggplot(aes(x = .data[[outcome]] )) + 
    geom_histogram(fill = "grey", bins = 50) + 
    theme_bw() + labs(x = "outcome", title = paste0("Simulated outcome ", outcome ," distribution") )
  
  p6 = dat %>% ggplot(aes(y = .data[[exposure]], x = g)) +
    # geom_point(color = "grey") +
    geom_smooth(method = "gam", formula = y~ s(x), color = "blue") +
    geom_smooth(method = "lm", formula = y~ x, color = "red") +
    theme_bw() +
    labs(x = "instrument", y = "exposure", title = paste0("iv ~ exposure, R2 = ", r2_gene_exposure, " p = ", p_gene_exposure))
  
  p7 = dat %>% ggplot(aes(x = .data[[exposure]], y = .data[[outcome]])) +
    # geom_point(color = "grey") +
    geom_smooth(method = "gam", formula = y~ s(x), color = "blue") +
    geom_smooth(method = "lm", formula = y~ x, color = "red") +
    theme_bw() +
    labs(x = "exposure", y = "outcome", title = paste0("exposure ~ outcome, R2 = ", r2_exposure_outcome, " p = ", p_exposure_outcome))
  
  
  p8 = dat %>% ggplot(aes(x = .data[["g"]], y = .data[[outcome]])) +
    # geom_point(color = "grey") +
    geom_smooth(method = "gam", formula = y~ s(x), color = "blue") +
    geom_smooth(method = "lm", formula = y~ x, color = "red") +
    theme_bw() +
    labs(x = "instrument", y = "outcome", title = paste0("iv ~ outcome, R2 = ", r2_gene_outcome, " p = ", p_gene_outcome))
  master_plot =  (p4  + p5) /    p1 / p2 / p3  / ( p6 + p7 + p8)
  
  
  master_plot =  (p4  + p5) /    p1 / p2 / p3  / ( p6 + p7 + p8)
  return(master_plot)
}
