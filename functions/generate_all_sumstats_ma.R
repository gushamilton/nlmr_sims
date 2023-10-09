generate_all_sumstats_ma = function(
  datain = NULL,
  exposure = "x",
  outcome = "y",
  k = 10){

  
  
  generate_strata_specific_stats <- function(z) {
    ma_mr <- meta::metagen(z$beta_mr, z$se_mr)
    ma_bx <- meta::metagen(z$beta_gx, z$se_gx)
    ma_by <-meta::metagen(z$beta_gy, z$se_gy)
    tibble(mr_beta_re = ma_mr$TE.random, mr_se_re = ma_mr$seTE.random,
           bx_beta_re = ma_bx$TE.random, bx_se_re = ma_bx$seTE.random,
           by_beta_re = ma_by$TE.random, by_se_re = ma_by$seTE.random,
           mr_beta_fe = ma_mr$TE.fixed, mr_se_fe = ma_mr$seTE.fixed,
           bx_beta_fe = ma_bx$TE.fixed, bx_se_fe = ma_bx$seTE.fixed,
           by_beta_fe = ma_by$TE.fixed, by_se_fe = ma_by$seTE.fixed,
    )
  }
  
  
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
f3 = as.formula(paste0(outcome, " ~ ", exposure) )
fit = lm( f , data = wdata)
fit2 = lm(f2, data = wdata)
fit3 = lm(f3, data = wdata)
###
full_ss = data.frame( 
  strata = 0,
  v = var( wdata[, exposure] ),
  sd = sd( wdata[, exposure] ),
  skew = psych::skew( wdata[, exposure] ),
  kurtosis = psych::kurtosi( wdata[, exposure] ),
  min = min( wdata[, exposure] ),
  max = max( wdata[, exposure] ),
  mean = mean( wdata[, exposure]),
  range = max( wdata[, exposure] ) - min( wdata[, exposure] ),
  r2_gene_exposure = summary(fit)$r.sq,
  r2_gene_outcome = summary(fit2)$r.sq,
  r2_exposure_outcome = summary(fit3)$r.sq,
  beta_gx = summary(fit)$coef[2,1] , 
  se_gx = summary(fit)$coef[2,2] , 
  p_gx = summary(fit)$coef[2,4] ,
  beta_gy = summary(fit2)$coef[2,1] , 
  se_gy = summary(fit2)$coef[2,2] , 
  p_gy = summary(fit2)$coef[2,4] ,
  p_xy = summary(fit3)$coef[2,4] ,
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
            mean = mean(.data[[exposure]]),
            range = max( .data[[exposure]] ) - min( .data[[exposure]] ),
            r2_gene_exposure = summary(lm( .data[[exposure]] ~ g ))$r.sq,
            r2_gene_outcome = summary(lm( .data[[outcome]] ~ g ))$r.sq,
            r2_exposure_outcome = summary(lm( .data[[outcome]] ~ .data[[exposure]] ))$r.sq,
            beta_gx = unlist( tidy(lm( .data[[exposure]] ~ g ))[2,2] ), 
            beta_gy = unlist( tidy(lm( .data[[outcome]] ~ g ))[2,2] ), 
            se_gx = unlist( tidy(lm( .data[[exposure]] ~ g ))[2,3] ), 
            se_gy = unlist( tidy(lm( .data[[outcome]] ~ g ))[2,3] ), 
            p_gx = unlist( tidy(lm( .data[[exposure]] ~ g ))[2,5] ),
            p_gy = unlist( tidy(lm( .data[[outcome]] ~ g ))[2,5] ),
            p_xy = unlist( tidy(lm( .data[[outcome]] ~ .data[[exposure]]))[2,5] ) 
            )  %>%
  mutate(beta_mr = beta_gy/beta_gx, se_mr = se_gy/beta_gx, p_mr = pnorm(abs(beta_mr/se_mr), lower.tail=F)
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
            mean = mean(.data[[exposure]]),
            range = max( .data[[exposure]] ) - min( .data[[exposure]] ),
            r2_gene_exposure = summary(lm( .data[[exposure]] ~ g ))$r.sq,
            r2_gene_outcome = summary(lm( .data[[outcome]] ~ g ))$r.sq,
            r2_exposure_outcome = summary(lm( .data[[outcome]] ~ .data[[exposure]] ))$r.sq,
            beta_gx = unlist( tidy(lm( .data[[exposure]] ~ g ))[2,2] ), 
            beta_gy = unlist( tidy(lm( .data[[outcome]] ~ g ))[2,2] ), 
            se_gx = unlist( tidy(lm( .data[[exposure]] ~ g ))[2,3] ), 
            se_gy = unlist( tidy(lm( .data[[outcome]] ~ g ))[2,3] ), 
            p_gx = unlist( tidy(lm( .data[[exposure]] ~ g ))[2,5] ),
            p_gy = unlist( tidy(lm( .data[[outcome]] ~ g ))[2,5] ),
            p_xy = unlist( tidy(lm( .data[[outcome]] ~ .data[[exposure]]))[2,5] ) 
  

  ) %>%
  mutate(beta_mr = beta_gy/beta_gx, se_mr = se_gy/beta_gx, p_mr = pnorm(abs(beta_mr/se_mr), lower.tail=F)
  )

  colnames(residual_ss)[1] = "strata"
  residual_ss$strata_method = "residual"
  
  
  #  GENERATE Q and P
  
ranked_overall <- generate_strata_specific_stats(ranked_ss) %>% mutate(method = "ranked")
residual_overall <- generate_strata_specific_stats(residual_ss) %>% mutate(method = "resid")
overall <- bind_rows(ranked_overall, residual_overall)
ss_out <- full_ss %>%
  bind_rows(full_ss) %>% 
  mutate(beta_mr = beta_gy/beta_gx, se_mr = se_gy/beta_gx, p_mr = pnorm(abs(beta_mr/se_mr), lower.tail=F)
  ) %>%
  select(contains(c("beta", "se"))) %>%
  bind_cols(overall)

  

  return(ss_out)
}


