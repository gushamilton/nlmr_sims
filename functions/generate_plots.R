generate_plots = function( ss_data = NULL,
                           simdata = NULL,
                           exposure = "x",
                           distribution_type = "normal" ){
  p1 = ss %>%
    filter(strata_method != "full") %>% 
    arrange(strata) %>%
    mutate(lower = beta - 1.96 * se, upper = beta + 1.96 * se) %>% 
    ggplot(aes(x = strata, y = beta)) +
    geom_hline(yintercept = ss$beta[1], linetype = "dashed", color = "blue") +
    geom_point(size = 3) + 
    geom_errorbar(aes(ymin = lower, ymax = upper)) +
    theme_bw() +
    facet_wrap(~strata_method, scales = "free") +
    labs( title = "Gene-exposure effect estimates across strata" )
  
  p2 = ss %>%
    filter(strata_method != "full") %>% 
    arrange(strata) %>%
    ggplot(aes(x = beta, y = sd)) +
    geom_point( aes(color = skew), size = 4, alpha = 0.8) +
    scale_colour_viridis_c(option = "plasma") +
    geom_smooth(method = "lm", formula = y~x, color = "black") +
    theme_bw() +
    facet_wrap(~strata_method, scales = "free") +
    labs( title = "Standard deviation on beta" )
  
  p3 = ss %>%
    filter(strata_method != "full") %>% 
    arrange(strata) %>%
    # ggplot(aes(x = beta, y = abs(skew)) ) +
    ggplot(aes(x = beta, y = skew ) ) +
    geom_point( aes(color = sd), size = 4, alpha = 0.8) +
    scale_colour_viridis_c() +
    geom_smooth(method = "lm", formula = y~x, color = "black") +
    theme_bw() +
    facet_wrap(~strata_method, scales = "free") +
    # labs( title = "Absolute skew on beta" )
    labs( title = "Skew on beta" )
  
  ######################
  ### FULL DATA SET
  ######################
  p4 = test %>% ggplot(aes(x = .data[[exposure]] )) + 
    geom_histogram(fill = "grey", bins = 50) + 
    theme_bw() + labs(x = "exposure", title = paste0("Simulated ", distribution_type ," distribution") )
  
  p5 = test %>% ggplot(aes(x = .data[[exposure]], y = g)) +
    # geom_point(color = "grey") +
    geom_smooth(method = "gam", formula = y~ s(x), color = "blue") +
    geom_smooth(method = "lm", formula = y~ x, color = "red") +
    theme_bw() +
    labs(x = "exposure", y = "instrument", title = "gene expsoure relationship")
  
  
  master_plot = (p4|p5) /p1 / p2 / p3
  return(master_plot)
}


