pacman::p_load(tidyverse, faux, broom)
mydata <- faux::rnorm_multi(n = 100000, 
                            mu = c(0, 0, 0), 
                            sd = c(1, 1, 1), 
                            r = c(0.3, 0.02, 0.1), 
                            varnames = c("g", "x", "y"),
                            empirical = FALSE) %>%
  mutate(y = rnorm(100000) + 0.1 * x)


a = mydata$x
a = a / max(a)
mydata$x = pbeta(a, shape1 = 4, shape2 = 3)

f_models <- function(df) {
  m <- tidy(lm(x ~ g, data = df)) %>%
    mutate(model = "gx")
  m2 <- tidy(lm(y ~ g, data = df)) %>%
    mutate(model = "gy") 
  
  bind_rows(m,m2) %>%
    filter(term != "(Intercept)")
  
}
random_sample <- mydata %>%
  mutate(row = ntile(row_number(), 10)) %>%
  group_by(row) %>%
  nest() %>%
  mutate(res = map(data, f_models)) %>%
  unnest(res) %>%
  select(model, estimate, std.error) %>%
  pivot_wider(names_from = model, values_from = c(estimate, std.error)) %>%
  mutate(mr_se = std.error_gy/estimate_gx, mr_beta = estimate_gy/estimate_gx)

meta::metagen(random_sample$mr_beta, random_sample$mr_se) 

f_models(mydata)  %>%
  select(model, estimate, std.error) %>%
  pivot_wider(names_from = model, values_from = c(estimate, std.error)) %>%
  mutate(mr_se = std.error_gy/estimate_gx, mr_beta = estimate_gy/estimate_gx)




# RESIDUAL

m <- lm(x ~g, data= mydata)

residual <- mydata %>%
  mutate(row = ntile(m$residuals, 10)) %>%
  group_by(row) %>%
  nest() %>%
  mutate(res = map(data, f_models)) %>%
  unnest(res) %>%
  select(model, estimate, std.error) %>%
  pivot_wider(names_from = model, values_from = c(estimate, std.error)) %>%
  mutate(mr_se = std.error_gy/estimate_gx, mr_beta = estimate_gy/estimate_gx)

meta::metagen(residual$mr_beta, residual$mr_se)$seTE.fixed  

mydata <- faux::rnorm_multi(n = 100000, 
                            mu = c(0, 0, 0), 
                            sd = c(1, 1, 1), 
                            r = c(0.3, 0.02, 0.1), 
                            varnames = c("g", "x", "y"),
                            empirical = FALSE) %>%
  mutate(x = g*0.1*x + x) %>%
  mutate(y = rnorm(100000) + 0.01 * x)

f_models <- function(df) {
  m <- tidy(lm(x ~ g, data = df)) %>%
    mutate(model = "gx")
  m2 <- tidy(lm(y ~ g, data = df)) %>%
    mutate(model = "gy") 
  
  bind_rows(m,m2) %>%
    filter(term != "(Intercept)")
  
}
random_sample <- mydata %>%
  mutate(row = ntile(row_number(), 10)) %>%
  group_by(row) %>%
  nest() %>%
  mutate(res = map(data, f_models)) %>%
  unnest(res) %>%
  select(model, estimate, std.error) %>%
  pivot_wider(names_from = model, values_from = c(estimate, std.error)) %>%
  mutate(mr_se = std.error_gy/estimate_gx, mr_beta = estimate_gx/estimate_gy)



meta::metagen(random_sample$mr_beta, random_sample$mr_se)$seTE.fixed  

f_models(mydata)  %>%
  select(model, estimate, std.error) %>%
  pivot_wider(names_from = model, values_from = c(estimate, std.error)) %>%
  mutate(mr_se = std.error_gy/estimate_gx, mr_beta = estimate_gy/estimate_gx)



m <- lm(x ~g, data= mydata)

residual <- mydata %>%
  mutate(row = ntile(m$residuals, 10)) %>%
  group_by(row) %>%
  nest() %>%
  mutate(res = map(data, f_models)) %>%
  unnest(res) %>%
  select(model, estimate, std.error) %>%
  pivot_wider(names_from = model, values_from = c(estimate, std.error)) %>%
  mutate(mr_se = std.error_gy/estimate_gx, mr_beta = estimate_gy/estimate_gx)


meta::metagen(residual$mr_beta, residual$mr_se)$TE.fixed  



# ranked 

generate_ranked_strata <- function(instrument, exposure, k, seed = 310723){
  # haodong ranked strata method
  set.seed(seed)
  ###
  wdata = tibble(instrument = instrument, exposure = exposure) 
  ###
  wdata = wdata %>% mutate(z = rank(instrument, ties.method = "random")) %>%
    mutate(strata1 = floor((z-1)/k)+1) %>% 
    mutate(id = seq(exposure)) %>%
    arrange(exposure) %>%
    group_by(strata1) %>%
    mutate(x0q = rank(exposure, ties.method = "random") ) %>%
    arrange(id)
  ###
  bins = wdata %>% pull(x0q)
  
  return( as.factor(bins) )
}



residual <- mydata %>%
  mutate(row = generate_ranked_strata(g,x,10)) %>%
  group_by(row) %>%
  nest() %>%
  mutate(res = map(data, f_models)) %>%
  unnest(res) %>%
  select(model, estimate, std.error) %>%
  pivot_wider(names_from = model, values_from = c(estimate, std.error)) %>%
  mutate(mr_se = std.error_gy/estimate_gx, mr_beta = estimate_gx/estimate_gy) %>%
  arrange(row)
residual

meta::metagen(residual$mr_beta, residual$mr_se)$seTE.fixed  
