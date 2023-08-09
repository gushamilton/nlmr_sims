generate_gx_sumstats = function(
  datain = NULL,
  exposure = "x",
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
fit = lm( f , data = wdata)
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
  beta = summary(fit)$coef[2,1] , 
  se = summary(fit)$coef[2,2] , 
  p = summary(fit)$coef[2,4] ,
  W = shapiro.test( sample( fit$residual, 5000)  )$stat, 
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
            beta = unlist( tidy(lm( .data[[exposure]] ~ g ))[2,2] ), 
            se = unlist( tidy(lm( .data[[exposure]] ~ g ))[2,3] ), 
            p = unlist( tidy(lm( .data[[exposure]] ~ g ))[2,5] ),
            W = shapiro.test( lm( .data[[exposure]] ~ g )$residual  )$stat
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
            beta = unlist( tidy(lm( .data[[exposure]] ~ g ))[2,2] ), 
            se = unlist( tidy(lm( .data[[exposure]] ~ g ))[2,3] ), 
            p = unlist( tidy(lm( .data[[exposure]] ~ g ))[2,5] ),
            W = shapiro.test( lm( .data[[exposure]] ~ g )$residual  )$stat
            ) 
  colnames(residual_ss)[1] = "strata"
  residual_ss$strata_method = "residual"

  ss_out = rbind(ranked_ss, residual_ss)
  ss_out = rbind(full_ss, ss_out)

  ss_out$strata = factor(ss_out$strata, levels = 0:nrow(ranked_ss) ) 
  
  out = list(wdata = wdata, 
             ss = ss_out)
  return(out)
}


