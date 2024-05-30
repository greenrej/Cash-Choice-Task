
# Analysis examining the effect of a brief delay discounting task on alcohol
# use and alcohol-related consequences

# Outcome: RAPI -----------------------------------------------------------

# primary model
m1_nb <- MASS::glm.nb(
  rapi_score_rec ~ cash_choice + bis + sex + age +  ses2b_factor,
  df_final)

m1_nb_summary <- broom::tidy(m1_nb) %>%
  rename(b = estimate) %>% 
  mutate(across(where(is.numeric), .fns = ~ round(.x, 3)))
m1_nb_summary

check_singularity(m1_nb) 
check_outliers(m1_nb) 
vif(m1_nb)

# exploratory model
m1_nb_sex <- MASS::glm.nb(
  rapi_score_rec ~ cash_choice*sex + bis + age +  ses2b_factor,
  data = df_final)

m1_nb_sex_summary <- broom::tidy(m1_nb_sex) %>%
  rename(b = estimate) %>% 
  mutate(across(where(is.numeric), .fns = ~ round(.x, 3)))
m1_nb_sex_summary

# Outcome: AUD Status -----------------------------------------------------

# (1) Binary AUD Status

# primary model
m2_logreg <- glm(
  mini_binary ~ cash_choice + bis + sex + age +  ses2b_factor, 
  family = 'binomial',
  data = df_final)

confint(m2_logreg)

m2_logreg_summary <- broom::tidy(m2_logreg) %>%
  mutate(OR = exp(estimate)) %>% 
  rename(b = estimate) %>%
  mutate(across(where(is.numeric), .fns = ~ round(.x, 3)))
m2_logreg_summary

# exploratory model
m2_logreg_sex <- glm(
  mini_binary ~ cash_choice*sex + bis + age +  ses2b_factor, 
  data = df_final,
  family = 'binomial')

m2_logreg_sex_summary <- broom::tidy(m2_logreg_sex) %>%
  mutate(OR = exp(estimate)) %>% 
  rename(b = estimate) %>%
  mutate(across(where(is.numeric), .fns = ~ round(.x, 3)))
m2_logreg_sex_summary

# (2) Severity of AUD

# create 3-level severity variable (due to small cell sizes at higher severity)
df_final <- df_final %>% 
  mutate(
    # numeric
    mini_severity_3l = case_when(
      mini_severity == 'None' ~ 1,
      mini_severity == 'Mild' ~ 2,
      mini_severity == 'Moderate' ~ 3,
      mini_severity == 'Severe' ~ 3),
    # factor
    mini_severity_3l = as.factor(mini_severity_3l))
class(df_final$mini_severity_3l) 
levels(df_final$mini_severity_3l) 

# primary model
m3_clm <- clm(
  mini_severity_3l ~ cash_choice + bis + sex + age + ses2b_factor, 
  data = df_final)
summary(m3_clm) 
exp(m3_clm$beta) 
confint(m3_clm)

# exploratory model
m3_clm_sex <- clm(
  mini_severity_3l ~ cash_choice*sex + bis + sex + age + ses2b_factor, 
  data = df_final) 

# Outcome: TLFB Frequency and Quantity of Alcohol Use ---------------------

# (1) Frequency of alcohol use: drinking days 

# primary model
m4_nb <- MASS::glm.nb(
  drinking_days ~ cash_choice + bis + sex + age +  ses2b_factor,
  data = df_final)
m4_nb_summary <- broom::tidy(m4_nb) %>%
  rename(b = estimate) %>% 
  mutate(across(where(is.numeric), .fns = ~ round(.x, 3)))
m4_nb_summary

check_singularity(m4_nb)
check_outliers(m4_nb) 
vif(m4_nb)

# exploratory model
m4_nb_sex <- MASS::glm.nb(
  drinking_days ~ cash_choice*sex + bis + age +  ses2b_factor,
  data = df_final)

m4_nb_sex_summary <- broom::tidy(m4_nb_sex) %>%
  rename(b = estimate) %>% 
  mutate(across(where(is.numeric), .fns = ~ round(.x, 3)))
m4_nb_sex_summary

# (2) Quantity of alcohol use: drinks per drinking day 

# primary model
m5_robust <- lm_robust(
  dpdd ~ cash_choice + bis + sex + age +  ses2b_factor, 
  data = df_final) 

m5_robust_summary <- broom::tidy(m5_robust) %>%
  rename(b = estimate) %>% 
  mutate(across(where(is.numeric), .fns = ~ round(.x, 3)))
m5_robust_summary

# exploratory model
m5_robust_sex <- lm_robust(
  dpdd ~ cash_choice*sex + bis + age +  ses2b_factor,
  data = df_final)

m5_robust_sex_summary <- broom::tidy(m5_robust_sex) %>%
  rename(b = estimate) %>% 
  mutate(across(where(is.numeric), .fns = ~ round(.x, 3)))
m5_robust_sex_summary

