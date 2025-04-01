
# Outcome: RAPI -----------------------------------------------------------

# (a) primary model

m1_nb <- MASS::glm.nb(
  rapi_score ~ cash_choice + sex + age + ses2b_factor + bis, 
  data = df)

m1_nb_summary <- tidy(m1_nb) %>%
  rename(b = estimate) %>% 
  mutate(across(where(is.numeric), .fns = ~ round(.x, 2)))

m1_nb_summary

cohens_d(rapi_score ~ cash_choice, data = df)

round(vif(m1_nb), 2) 

# (b) unadjusted model

m1_unadj <- MASS::glm.nb(
  rapi_score ~ cash_choice,
  data = df)

m1_unadj_summary <- tidy(m1_unadj) %>%
  rename(b = estimate) %>% 
  mutate(across(where(is.numeric), .fns = ~ round(.x, 2)))

m1_unadj_summary

# (c) exploratory model
# - add nicotine and cannabis use days as covariates

m1_exp <- MASS::glm.nb(
  rapi_score ~ cash_choice + sex + age + ses2b_factor + bis + cannabis_days + nicotine_days, 
  data = df)
check_outliers(m1_exp_3) 

m1_exp_summary <- tidy(m1_exp_3) %>%
  rename(b = estimate) %>% 
  mutate(across(where(is.numeric), .fns = ~ round(.x, 2)))

m1_exp_summary

# Outcome: AUD Binary -----------------------------------------------------------

# (a) primary model

m2_logreg <- glm(
  AUD_binary ~ cash_choice + sex + age + ses2b_factor + bis, 
  family = 'binomial',
  data = df)
check_outliers(m2_logreg) 

m2_logreg_summary <- tidy(m2_logreg) %>%
  rename(b = estimate) %>% 
  mutate(across(where(is.numeric), .fns = ~ round(.x, 2)))

m2_logreg_summary

round(exp(cbind(Odds_Ratio = coef(m2_logreg), confint(m2_logreg))), 2)

round(vif(m2_logreg), 2) 

# (b) unadjusted model

m2_unadj <- glm(
  AUD_binary ~ cash_choice, 
  family = 'binomial',
  data = df)

m2_unadj_summary <- tidy(m2_unadj) %>%
  rename(b = estimate) %>% 
  mutate(across(where(is.numeric), .fns = ~ round(.x, 2)))

m2_unadj_summary

round(exp(cbind(Odds_Ratio = coef(m2_unadj), confint(m2_unadj))), 2)

# (c) exploratory model
# - add nicotine and cannabis use days as covariates

m2_exp <- glm(
  AUD_binary ~ cash_choice + sex + age + ses2b_factor + bis + cannabis_days + nicotine_days, 
  family = 'binomial',
  data = df)

m2_exp_summary <- tidy(m2_exp) %>%
  rename(b = estimate) %>% 
  mutate(across(where(is.numeric), .fns = ~ round(.x, 2)))

m2_exp_summary

round(exp(cbind(Odds_Ratio = coef(m2_exp), confint(m2_exp))), 2)

# Outcome: AUD Severity -----------------------------------------------------------

# (a) primary model

m3_clm <- clm(
  AUD_severity ~ cash_choice + sex + age + ses2b_factor + bis, 
  data = df)

m3_clm_summary <- tidy(m3_clm) %>%
  rename(b = estimate) %>% 
  mutate(across(where(is.numeric), .fns = ~ round(.x, 2)))

m3_clm_summary

exp(m3_clm$beta) 
round(exp(confint(m3_clm)), 2) 

# (b) unadjusted model

m3_unadj <- clm(
  AUD_severity ~ cash_choice, 
  data = df)

m3_unadj_summary <- tidy(m3_unadj) %>%
  rename(b = estimate) %>% 
  mutate(across(where(is.numeric), .fns = ~ round(.x, 2)))

m3_unadj_summary

exp(m3_unadj$beta) 
round(exp(confint(m3_unadj)), 2) 

# (c) exploratory model
# - add nicotine and cannabis use days as covariates

m3_exp <- clm(
  AUD_severity ~ cash_choice + sex + age + ses2b_factor + bis + cannabis_days + nicotine_days, 
  data = df)

m3_exp_summary <- tidy(m3_exp) %>%
  rename(b = estimate) %>% 
  mutate(across(where(is.numeric), .fns = ~ round(.x, 2)))

m3_exp_summary

exp(m3_exp$beta) 
round(exp(confint(m3_exp)), 2) 

# (d) correlations as indicator of potential multicollinearity between
#     primary predictor and covariates

# - CCT x Sex: Phi Coefficient

table(df$cash_choice, df$sex)
psych::phi(matrix(c(19, 20, 14, 16), nrow = 2), digits = 3)

# CCT x Age: Point Biserial

cor.test(df$cash_choice_pb, df$age)

# - CCT x SES: Cramer's V

table(df$cash_choice, df$ses2b_factor)
rcompanion::cramerV(matrix(c(9, 6, 12, 14, 12, 16), nrow = 2))

# - CCT x BIS: Point Biserial

cor.test(df$cash_choice_pb, df$bis)

# Outcome: TLFB Drinking Days -----------------------------------------------------------

# (a) primary model

m4_nb <- MASS::glm.nb(
  drinking_days ~ cash_choice + sex + age + ses2b_factor + bis, 
  data = df)

m4_nb_summary <- tidy(m4_nb) %>%
  rename(b = estimate) %>% 
  mutate(across(where(is.numeric), .fns = ~ round(.x, 2)))

m4_nb_summary

cohens_d(drinking_days ~ cash_choice, data = df)

round(vif(m4_nb), 2) 

# (b) unadjusted model

m4_unadj <- MASS::glm.nb(
  drinking_days ~ cash_choice, 
  data = df)

m4_unadj_summary <- tidy(m4_unadj) %>%
  rename(b = estimate) %>% 
  mutate(across(where(is.numeric), .fns = ~ round(.x, 2)))

m4_unadj_summary

# (c) exploratory model
# - add nicotine and cannabis use days as covariates

m4_exp <- MASS::glm.nb(
  drinking_days ~ cash_choice + sex + age + ses2b_factor + bis + cannabis_days + nicotine_days, 
  data = df)

m4_exp_summary <- tidy(m4_exp_3) %>%
  rename(b = estimate) %>% 
  mutate(across(where(is.numeric), .fns = ~ round(.x, 2)))

m4_exp_summary

# Outcome: TLFB Drinks per Drinking Day -----------------------------------------------------------

# (a) primary model 

# initial model: linear

m5_linear <- lm(
  dpdd ~ cash_choice + sex + age + ses2b_factor + bis, 
  data = df)

m5_linear_summary <- tidy(m5_linear) %>%
  rename(b = estimate) %>% 
  mutate(across(where(is.numeric), .fns = ~ round(.x, 2)))

m5_linear_summary

check_heteroscedasticity(m5_linear)
round(vif(m5_linear), 2)

# final model: linear w/robust SE

m5_linear_robust <- lm_robust(
  dpdd ~ cash_choice + sex + age + ses2b_factor + bis, 
  data = df, 
  se_type = 'HC2')

m5_linear_robust_summary <- tidy(m5_linear_robust) %>%
  rename(b = estimate) %>% 
  mutate(across(where(is.numeric), .fns = ~ round(.x, 2)))

m5_linear_robust_summary

# (b) unadjusted model

m5_unadj <- lm_robust(
  dpdd ~ cash_choice, 
  data = df, 
  se_type = 'HC2')

m5_unadj_summary <- tidy(m5_unadj) %>%
  rename(b = estimate) %>% 
  mutate(across(where(is.numeric), .fns = ~ round(.x, 2)))

m5_unadj_summary

# (c) exploratory model
# - add nicotine and cannabis use days as covariates

m5_exp <- lm_robust(
  dpdd ~ cash_choice + sex + age + ses2b_factor + bis + cannabis_days + nicotine_days, 
  data = df, 
  se_type = 'HC2')

m5_exp_summary <- tidy(m5_exp_3) %>%
  rename(b = estimate) %>% 
  mutate(across(where(is.numeric), .fns = ~ round(.x, 2)))

m5_exp_summary

