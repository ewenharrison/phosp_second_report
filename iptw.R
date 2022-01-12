explanatory = c("age_admission", 
                "crf1a_sex", 
                "crf1b_eth_5levels", 
                "imd_quintile",
                "crf3a_bmi",
                
                "crf1a_resp_support_4levels",
                
                # Comorbidities - # None of these are imputed as missing considered absent
                "no_comorbid",
                "no_comorbid_3levels",
                "crf1a_com_card", "crf1a_com_neupsy",
                "crf1a_com_res", "crf1a_com_rheu",
                "crf1a_com_gast", "crf1a_com_mer", 
                "crf1a_com_mh", "crf1a_com_diab"
                )

outcomes = c("psq_scale_fatigue_24hrs")

phosp_hosp %>% 
  left_join(phosp_12m %>% select(study_id, outcomes)) %>% 
  select(explanatory, outcomes) %>% 
  missing_glimpse()

study_id_12m = phosp_12m %>% 
  pull(study_id)

phosp_iptw = phosp_hosp %>% 
  left_join(phosp_12m %>% select(study_id, outcomes)) %>% 
  mutate(
    group_fu = case_when(
      study_id %in% study_id_12m ~ 1,
      TRUE ~ 0
    )
  ) %>% 
  select(group_fu, explanatory, outcomes) %>% 
  drop_na(group_fu, explanatory)

phosp_iptw  %>% 
  count(group_fu)

phosp_iptw %>% 
  summary_factorlist(NULL, explanatory)
# all present

# Numerator model
fit_num = phosp_iptw %>% 
  glmmulti("group_fu", 1)

# Demoninator model
fit_dem = phosp_iptw %>% 
  glmmulti("group_fu", explanatory)

fit_dem %>% 
  fit2df()

phosp_w = phosp_iptw %>% 
  mutate(
    p = predict(fit_num, type = "response"),
    pi = predict(fit_dem, type = "response"),
    weights = case_when(
      group_fu == 1 ~ p / pi,
        group_fu == 0 ~ (1-p) / (1-pi) 
    )
  )

sum(phosp_w$weights)
# Whoop whoop!

phosp_w  = phosp_w %>% 
  mutate(
    psq_scale_fatigue_24hrs_w = psq_scale_fatigue_24hrs * weights)


phosp_w %>% 
  select(psq_scale_fatigue_24hrs, psq_scale_fatigue_24hrs_w) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(name, value)) + 
  geom_boxplot() +
  theme_bw() + 
  labs(x = "") +
  ggtitle("Comparison of raw vs IPTW adjusted fatigue scores",
          "12 month follow-up patients adjusted to PHOSP discharge population")
