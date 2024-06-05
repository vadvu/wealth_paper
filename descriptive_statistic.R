library(gtsummary)

data %>%
  select(NVC_1.3_NONVIOL, unarmed.rev.q, 
         gdppc.gap.ln_l, 
         growth.gap.5_l, pop_ln_l, polity2_l, 
         Polity5_durable_ln_l,BESS_plus_WB_Urbanization_l,
         VDEM_v2x_execorr_l, Mean_year_schooling_interpolated_l,
         oil.pc.ln_l,sum.nvc.nonviol,cumsum.nvc.nonviol,
         sum.rev.q.nonviol,cumsum.unarmed.rev.q,
         region
  ) %>% 
  mutate(unarmed.rev.q = as.factor(unarmed.rev.q),
         cumsum.nvc.nonviol = as.factor(cumsum.nvc.nonviol),
         cumsum.unarmed.rev.q = as.factor(cumsum.unarmed.rev.q)
  ) %>% 
  tbl_summary(statistic = 
                list(
                  all_continuous() ~ "{mean} ([{min}-{max}], {sd})",
                  all_categorical() ~ "{n} ({p}%)"
                ),
              label = 
                list(
                  NVC_1.3_NONVIOL ~ "Nonviolent revolution (NAVCO 1.3)",
                  unarmed.rev.q ~ "Nonviolent revolution (Beissinger's + Goldstone et al. data)",
                  gdppc.gap.ln_l ~ "GDP pc (2017$, PPP), ln",
                  growth.gap.5_l ~ "Growth of GDP pc, 5-year average",
                  pop_ln_l ~ "Population (in thousands), ln",
                  polity2_l ~ "Polity score",
                  Polity5_durable_ln_l ~ "Regime durability (in years +1), ln",
                  BESS_plus_WB_Urbanization_l ~ "Urbanization (in %)",
                  VDEM_v2x_execorr_l ~ "Executive corruption index",
                  Mean_year_schooling_interpolated_l ~ "Mean years of schooling",
                  oil.pc.ln_l ~ "Oil production pc (kWh +0.1)",
                  sum.nvc.nonviol ~ "Nonviolent revolutions in the same region (NAVCO 1.3)",
                  cumsum.nvc.nonviol ~
                    "Number of revolutions in the country's history (since 1950) at year t (NAVCO 1.3)",
                  sum.rev.q.nonviol ~ 
                    "Nonviolent revolutions in the same region (Beissinger's + Goldstone et al. data)",
                  cumsum.unarmed.rev.q ~ 
                    "Number of revolutions in the country's history (since 1950) at year t 
                  (Beissinger's + Goldstone et al. data)",
                  region ~ "UN Subgerion classification"
                ),
              missing_text = "NA"
  ) %>% 
  as_gt() %>%
  gt::gtsave(filename = "tables/summary.html")
