generate_outcome_plots_bmi <- function(exposure, outcome, seed = 12, reps = 10) {
  

  replicate_internally <- function(n_rep) {
  seed = seed + n_rep
  set.seed(seed)
  tmp <-  dat_bmi %>%
    as.data.frame() %>%
    drop_na() %>%
    sample_n(1e5, replace = T) %>%
    transmute(x = body_mass_index_bmi) %>%
    mutate(g = rnorm_pre(x, sd = 1, r = 0.065), ) %>%
    mutate(y = 0.05 * x + rnorm(1e5)) 

    
  dat_with_both <- generate_all_sumstats(data = tmp, exposure = exposure, outcome = outcome, k = 10)
  
  ## redefine data set
  
  
  ## define sumstats tibble
  sum_stats_dat = dat_with_both$ss
  
  sum_stats_dat %>%
    mutate(replicate = n_rep)
  }
  
  d <- map_dfr(1:reps, replicate_internally)
  
  sum_stats_dat <- d %>%
    group_by(strata, strata_method) %>%
    summarise_all(., mean) %>%
    ungroup()
  
  get_pvals = function(df){
    p_quadratic <- metafor::rma(beta_mr ~ mean, (se_mr)^2, method="FE", data = df, control=list(maxiter=1000))$pval[2]
    p_Q <- 1 - pchisq(metafor::rma(beta_mr, vi=(se_mr)^2, data = df, control=list(maxiter=1000))$QE, df=(9))
    tibble(quadratic_p = p_quadratic, Q_p = p_Q)
  }
  

  p_res <- d %>%
    filter(strata_method != "full") %>%
    
    group_by(strata_method, replicate) %>%
    nest() %>%
    mutate(res = map(data, get_pvals)) %>%
    unnest(res) %>%
    ungroup()
  
  ranked_ps <- p_res %>%
    filter(strata_method == "ranked") %>%
    mutate(quadratic_p = if_else(quadratic_p ==0, 1e-300, quadratic_p)) %>%
    mutate(Q_p = if_else(Q_p ==0, NA, Q_p))
  resid_ps <- p_res %>%
    filter(strata_method == "residual") %>%
    mutate(quadratic_p = if_else(quadratic_p ==0, 1e-300, quadratic_p)) %>%
    mutate(Q_p = if_else(Q_p ==0, NA, Q_p))
  
  
  ## make figures
  

  
  r2_gene_exposure <- round(sum_stats_dat$r2_gene_exposure[1], digits = 4)
  r2_gene_outcome <- round(sum_stats_dat$r2_gene_outcome[1], digits = 4)
  r2_exposure_outcome<- round(sum_stats_dat$r2_exposure_outcome[1], digits = 4)
  
  p_gene_exposure <- signif(sum_stats_dat$p_gx[1], digits = 4)
  p_gene_outcome <- signif(sum_stats_dat$p_gy[1], digits = 4)
  p_exposure_outcome<- signif(sum_stats_dat$p_xy[1], digits = 4)
  ## make figures
  
  dat <-  dat_bmi %>%
    drop_na() %>%
    sample_n(1e5, replace = T) %>%
    transmute(x = body_mass_index_bmi) %>%
    mutate(g = rnorm_pre(x, sd = 1, r = 0.06)) %>%
    mutate(y = 0.05 * x + rnorm(1e5)) %>%
    as.data.frame()
  
  
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
  p4 = dat %>% ggplot(aes(x = .data[[exposure]])) + 
    geom_histogram(fill = "grey", bins = 50) + 
    theme_bw() + labs(x = "exposure", title = paste0("BMI ", exposure ," distribution") )
  
  
  p5 = dat %>% ggplot(aes(x = .data[[outcome]])) + 
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
  
  
  p9 <-hwep::qqpvalue(resid_ps$quadratic_p, method = "ggplot2", return = T) + ggtitle("Quadratic P's (Residual)")
  
  p10 <- hwep::qqpvalue(resid_ps$quadratic_p, method = "ggplot2", return = T) + ggtitle("Quadratic P's (Ranked)")
  
  p11 <-hwep::qqpvalue(resid_ps$Q_p, method = "ggplot2", return = T) + ggtitle("Het P's (Residual)")
  
  p12 <- hwep::qqpvalue(resid_ps$Q_p, method = "ggplot2", return = T) + ggtitle("Het P's (Ranked)")
  
  master_plot =  (p4  + p5) /    p1 / p2 / p3  / ( p6 + p7 + p8) / (p10 + p9) / (p12+p11)
  ggsave(master_plot, filename = paste0("figures/bmi/", exposure,"_",outcome,"_linear_effect_", reps,"_reps.pdf"), width = 14, height = 22)
}





