
dat_vitd <- vroom::vroom("~/data/non-linear/pheno/analysis_data.tsv.gz", col_select = c("vitd", "prs_vitd"))




p3vitd<- dat_vitd %>%
  
  mutate(strata = generate_ranked_strata(exposure = log10(vitd), k = 9, instrument = prs_vitd, seed = 123)) %>%
  ggplot(aes(x = log10(vitd), y = prs_vitd, colour = strata)) +
  geom_point(alpha = 0.01) + 
  geom_smooth(method = "lm") +
  # xlim(-3.5,3.5) +
  theme_bw() +
  xlab("Body mass index (rank-transformed)") +
  ylab("Polygenic risk score for BMI")

p3vitd_r <- dat_vitd %>%
  drop_na(vitd, prs_vitd) %>%
 mutate(vitd = RNOmni::RankNorm(log10(vitd))) %>%
  mutate(strata = generate_ranked_strata(exposure = log10(vitd), k = 9, instrument = prs_vitd, seed = 123)) %>%
  ggplot(aes(x = vitd, y = prs_vitd, colour = strata)) +
  geom_point(alpha = 0.01) + 
  geom_smooth(method = "lm") +
  # xlim(-3.5,3.5) +
  theme_bw() +
  xlab("Body mass index (rank-transformed)") +
  ylab("Polygenic risk score for BMI")



p3vitd_dr <-dat_vitd %>%
  drop_na(vitd, prs_vitd) %>%
  mutate(vitd = RNOmni::RankNorm(log10(vitd))) %>%
  mutate(strata = generate_ranked_strata(exposure = log10(vitd), k = 9, instrument = prs_vitd, seed = 123)) %>%
  group_by(strata) %>%
  mutate(vitd = RNOmni::RankNorm(vitd) + mean(vitd)) %>%  
  ggplot(aes(x = vitd, y = prs_vitd, colour = strata)) +
  geom_point(alpha = 0.01) + 
  geom_smooth(method = "lm") +
  # xlim(-3.5,3.5) +
  theme_bw() +
  xlab("Body mass index (rank-transformed)") +
  ylab("Polygenic risk score for BMI")



p3vitd + p3vitd_r + p3vitd_dr
ggsave("vitd_transformed.tiff", height = 4, width = 12, compression = "lzw") 


dat_vitd %>%
  drop_na(vitd, prs_vitd) %>%
  mutate(vitd = log10(vitd)) %>%
  mutate(strata = generate_ranked_strata(exposure = log10(vitd), k = 9, instrument = prs_vitd, seed = 123)) %>%
  mutate(strata = generate_ranked_strata(exposure = log10(vitd), k = 9, instrument = prs_vitd, seed = 123)) %>%
  group_by(strata) %>%
  mutate(vitd = RNOmni::RankNorm(vitd) + mean(vitd)) %>%
  nest() %>%
  mutate(res = map(data, .f = function(d) {tidy(lm(vitd~prs_vitd, data = d))})) %>%
  unnest(res) %>%
  filter(term == "prs_vitd") %>%
  arrange(strata)

dat_vitd %>%
  drop_na(vitd, prs_vitd) %>%
  mutate(vitd = log10(vitd)) %>%
  # mutate(vitd = RNOmni::RankNorm((vitd))) %>%
  mutate(strata = generate_ranked_strata(exposure = log10(vitd), k = 9, instrument = prs_vitd, seed = 123)) %>%
  group_by(strata) %>%
  mutate(vitd = RNOmni::RankNorm(vitd) + mean(vitd)) %>%
ggplot(aes(x = vitd)) +
  geom_density() +
  facet_wrap(~strata)


dat_bmi %>%
  drop_na(bmi_prs, body_mass_index_bmi) %>%
  
  mutate(iv_free = lm(body_mass_index_bmi ~ bmi_prs, data = .,)$resid) %>%
  mutate(strata = generate_residualized_strata(iv_free, k = 9)) %>%
  group_by(strata) %>%
  mutate(body_mass_index_bmi_s = RNOmni::RankNorm(body_mass_index_bmi[,1]) + mean(body_mass_index_bmi)) %>%
  ungroup() %>%
  mutate(scale_f = scale(body_mass_index_bmi_s)) %>%
  group_by(strata) %>%
  nest() %>%
  mutate(res = map(data, .f = function(d) {tidy(lm(scale_f~ bmi_prs, data = d))})) %>%
  unnest(res) %>%
  filter(term == "bmi_prs") %>%
  arrange(strata)

