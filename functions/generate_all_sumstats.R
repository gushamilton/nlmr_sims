generate_all_sumstats = function(
  datain = NULL,
  exposure = "x",
  outcome = "y",
  k = 10){

  #####################
  ## make working data frame
  #####################
  ## 1) make IV free exposure
  ## 2) generate observed exposure strata
  ## 3) generate iv free exposure strata
  ## 4) generate doubly ranked strata
  wdata = datain %>% mutate(iv_free_x = iv_free_exposure( wdata = .,
                                            exposure = exposure,
                                            instrument = "g",
                                            covariates = NULL,
                                            exposure_mean_normalize = TRUE) ) %>% 
  mutate( obs_strata = generate_residualized_strata(iv_free_exposure = .data[[exposure]], k = k ) ) %>%
  mutate( residual = generate_residualized_strata(iv_free_exposure = iv_free_x, k = k ) ) %>% 
  mutate( ranked = generate_ranked_strata(instrument = g, exposure = .data[[exposure]], k = k) )

#####################
## Summary Statistics
## for the whole data set
#####################
f = as.formula( paste0(exposure, " ~ g") )
f2 = as.formula( paste0(outcome, " ~ g") )
fit = lm( f , data = wdata)
fit2 = lm(f2, data = wdata)
###
full_ss = data.frame( 
  strata = 0,
  v = var( wdata[, exposure] ),
  sd = sd( wdata[, exposure] ),
  skew = psych::skew( wdata[, exposure] ),
  kurtosis = psych::kurtosi( wdata[, exposure] ),
  min = min( wdata[, exposure] ),
  max = max( wdata[, exposure] ),
  range = max( wdata[, exposure] ) - min( wdata[, exposure] ),
  r2 = summary(fit)$r.sq,
  beta_gx = summary(fit)$coef[2,1] , 
  se_gx = summary(fit)$coef[2,2] , 
  p_gx = summary(fit)$coef[2,4] ,
  beta_gy = summary(fit2)$coef[2,1] , 
  se_gy = summary(fit2)$coef[2,2] , 
  p_gy = summary(fit2)$coef[2,4] ,
  W_gx = shapiro.test( sample( fit2$residual, 5000)  )$stat, 
  W_gy = shapiro.test( sample( fit2$residual, 5000)  )$stat, 
  strata_method = "full"
  )

####################################
## Ranked Strata Summary Statistics
####################################

ranked_ss = wdata %>%
  group_by(ranked) %>%
  summarize(v = var( .data[[exposure]] ), 
            sd = sd( .data[[exposure]] ),
            skew = psych::skew( .data[[exposure]] ),
            kurtosis = psych::kurtosi( .data[[exposure]] ),
            min = min( .data[[exposure]] ),
            max = max( .data[[exposure]] ),
            range = max( .data[[exposure]] ) - min( .data[[exposure]] ),
            r2 = cor.test( .data[[exposure]] ,g)$estimate^2,
            beta_gx = unlist( tidy(lm( .data[[exposure]] ~ g ))[2,2] ), 
            beta_gy = unlist( tidy(lm( .data[[outcome]] ~ g ))[2,2] ), 
            se_gx = unlist( tidy(lm( .data[[exposure]] ~ g ))[2,3] ), 
            se_gy = unlist( tidy(lm( .data[[outcome]] ~ g ))[2,3] ), 
            p_gx = unlist( tidy(lm( .data[[exposure]] ~ g ))[2,5] ),
            p_gy = unlist( tidy(lm( .data[[outcome]] ~ g ))[2,5] ),
            W_gx = shapiro.test( lm( .data[[exposure]] ~ g )$residual  )$stat,
            W_gy = shapiro.test( lm( .data[[outcome]] ~ g )$residual  )$stat
            ) 
colnames(ranked_ss)[1] = "strata"
ranked_ss$strata_method = "ranked"

residual_ss = wdata %>%
  group_by(residual) %>%
  summarize(v = var( .data[[exposure]] ), 
            sd = sd( .data[[exposure]] ),
            skew = psych::skew( .data[[exposure]] ),
            kurtosis = psych::kurtosi( .data[[exposure]] ),
            min = min( .data[[exposure]] ),
            max = max( .data[[exposure]] ),
            range = max( .data[[exposure]] ) - min( .data[[exposure]] ),
            r2 = cor.test( .data[[exposure]] ,g)$estimate^2,
            beta_gx = unlist( tidy(lm( .data[[exposure]] ~ g ))[2,2] ), 
            beta_gy = unlist( tidy(lm( .data[[outcome]] ~ g ))[2,2] ), 
            se_gx = unlist( tidy(lm( .data[[exposure]] ~ g ))[2,3] ), 
            se_gy = unlist( tidy(lm( .data[[outcome]] ~ g ))[2,3] ), 
            p_gx = unlist( tidy(lm( .data[[exposure]] ~ g ))[2,5] ),
            p_gy = unlist( tidy(lm( .data[[outcome]] ~ g ))[2,5] ),
            W_gx = shapiro.test( lm( .data[[exposure]] ~ g )$residual  )$stat,
            W_gy = shapiro.test( lm( .data[[outcome]] ~ g )$residual  )$stat
  ) 
  colnames(residual_ss)[1] = "strata"
  residual_ss$strata_method = "residual"

  ss_out = rbind(ranked_ss, residual_ss)
  ss_out = rbind(full_ss, ss_out) 
  
  ss_out <-ss_out %>%
    mutate(beta_mr = beta_gx/beta_gy, se_mr = se_gy/beta_gx)

  ss_out$strata = factor(ss_out$strata, levels = 0:nrow(ranked_ss) ) 
  
  out = list(wdata = wdata, 
             ss = ss_out)
  return(out)
}


