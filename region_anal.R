m1_log.n.nvc <- glm(data = data, family = binomial(link = 'logit'), 
                    NVC_1.3_NONVIOL ~ gdppc.gap.ln_l + I(gdppc.gap.ln_l^2)+
                      growth.gap.5_l+
                      pop_ln_l+
                      polity2_l+I(polity2_l^2)+
                      Polity5_durable_ln_l+
                      BESS_plus_WB_Urbanization_l+
                      VDEM_v2x_execorr_l+
                      Mean_year_schooling_interpolated_l+
                      oil.pc.ln_l+
                      sum.nvc.nonviol+
                      sum.nvc.nonviol_l+
                      cumsum.nvc.nonviol+
                      region,
                    method = "brglmFit", type = "AS_mean")

vcov.m1_log.n.nvc <- vcovCL(m1_log.n.nvc, 
                            cluster = ~iso3c, 
                            type = "HC0")

k1 <- coeftest(m1_log.n.nvc, vcov. = vcov.m1_log.n.nvc)

mar1.n.nvc <- margins(m1_log.n.nvc, 
                      at = list("gdppc.gap.ln_l" = seq(
                        min(m1_log.n.nvc$model$gdppc.gap.ln_l),
                        max(m1_log.n.nvc$model$gdppc.gap.ln_l),
                        0.2)
                      ), 
                      type = "response", 
                      vcov = vcov.m1_log.n.nvc, 
                      variables = "gdppc.gap.ln_l") %>% 
  summary() %>% as.data.frame() %>% mutate(model = "nvc")

nvc.n.m <- ggplot(mar1.n.nvc, 
                  aes(x = as.numeric(gdppc.gap.ln_l), y = as.numeric(AME)))+
  geom_line(size = 1)+
  geom_ribbon(aes(ymin = as.numeric(lower), ymax = as.numeric(upper)), 
              alpha = 0.3, 
              size = 0.3, 
              linetype = "dotted")+
  geom_hline(yintercept = 0)+
  theme_classic()+
  ggtitle("NAVCO 1.3")+
  labs(y = "AME", x = "GDP pc, ln (t-1)")


pred.nvc <- prediction(m1_log.n.nvc,
                       at = list("gdppc.gap.ln_l" = seq(
                         min(m1_log.n.nvc$model$gdppc.gap.ln_l),
                         max(m1_log.n.nvc$model$gdppc.gap.ln_l),
                         0.2)
                       ), 
                       type = "response", 
                       vcov = vcov.m1_log.n.nvc, 
                       calculate_se = TRUE) %>% 
  summary() %>% 
  select(x = `at(gdppc.gap.ln_l)`, y = Prediction, se = SE)

##### linear navco

m1_log.n.nvc.linear <- glm(data = data, family = binomial(link = 'logit'), 
                           NVC_1.3_NONVIOL ~ gdppc.gap.ln_l+
                             growth.gap.5_l+
                             pop_ln_l+
                             polity2_l+I(polity2_l^2)+
                             Polity5_durable_ln_l+
                             BESS_plus_WB_Urbanization_l+
                             VDEM_v2x_execorr_l+
                             Mean_year_schooling_interpolated_l+
                             oil.pc.ln_l+
                             sum.nvc.nonviol+
                             sum.nvc.nonviol_l+
                             cumsum.nvc.nonviol+
                             region,
                           method = "brglmFit", type = "AS_mean")

vcov.m1_log.n.nvc.linear <- vcovCL(m1_log.n.nvc.linear, 
                                   cluster = ~iso3c, 
                                   type = "HC0")

k1.linear <- coeftest(m1_log.n.nvc.linear, vcov. = vcov.m1_log.n.nvc.linear)

##### bess data

m1_log.n.unrq <- glm(data = data, family = binomial(link = 'logit'), 
                     unarmed.rev.q ~ gdppc.gap.ln_l + I(gdppc.gap.ln_l^2)+
                       growth.gap.5_l+
                       pop_ln_l+
                       polity2_l+I(polity2_l^2)+
                       Polity5_durable_ln_l+
                       BESS_plus_WB_Urbanization_l+
                       VDEM_v2x_execorr_l+
                       Mean_year_schooling_interpolated_l+
                       oil.pc.ln_l+
                       sum.rev.q.nonviol+
                       sum.rev.q.nonviol_l+
                       cumsum.unarmed.rev.q+
                       region,
                     method = "brglmFit", type = "AS_mean")


m1_log.n.unrq.vcov <- vcovCL(m1_log.n.unrq, 
                             cluster = ~ iso3c, 
                             type = "HC0", fix = T)

k4 <- coeftest(m1_log.n.unrq, vcov. = m1_log.n.unrq.vcov)

mar1.n.unrq <- margins(m1_log.n.unrq, 
                       at = list("gdppc.gap.ln_l" = seq(
                         min(m1_log.n.unrq$model$gdppc.gap.ln_l),
                         max(m1_log.n.unrq$model$gdppc.gap.ln_l),
                         0.2)
                       ), 
                       type = "response", 
                       vcov = m1_log.n.unrq.vcov, 
                       variables = "gdppc.gap.ln_l") %>% 
  summary() %>% as.data.frame() %>% mutate(model = "unrq")

unrq.n.m <- ggplot(mar1.n.unrq, 
                   aes(x = as.numeric(gdppc.gap.ln_l), y = as.numeric(AME)))+
  geom_line(size = 1)+
  geom_ribbon(aes(ymin = as.numeric(lower), ymax = as.numeric(upper)), 
              alpha = 0.3, 
              size = 0.3, 
              linetype = "dotted")+
  geom_hline(yintercept = 0)+
  theme_classic()+
  ggtitle("Beissinger's data")+
  labs(y = "AME", x = "GDP pc, ln (t-1)")

pred.rev <- prediction(m1_log.n.unrq,
                       at = list("gdppc.gap.ln_l" = seq(
                         min(m1_log.n.nvc$model$gdppc.gap.ln_l),
                         max(m1_log.n.nvc$model$gdppc.gap.ln_l),
                         0.2)
                       ), 
                       type = "response", 
                       vcov = m1_log.n.unrq.vcov, 
                       calculate_se = TRUE) %>% 
  summary() %>% 
  select(x = `at(gdppc.gap.ln_l)`, y = Prediction, se = SE)




##### linear bess

m1_log.n.unrq.linear <- glm(data = data, family = binomial(link = 'logit'), 
                            unarmed.rev.q ~ gdppc.gap.ln_l+
                              growth.gap.5_l+
                              pop_ln_l+
                              polity2_l+I(polity2_l^2)+
                              Polity5_durable_ln_l+
                              BESS_plus_WB_Urbanization_l+
                              VDEM_v2x_execorr_l+
                              Mean_year_schooling_interpolated_l+
                              oil.pc.ln_l+
                              sum.rev.q.nonviol+
                              sum.rev.q.nonviol_l+
                              cumsum.unarmed.rev.q+
                              region,
                            method = "brglmFit", type = "AS_mean")


m1_log.n.unrq.vcov.linear <- vcovCL(m1_log.n.unrq.linear, 
                                    cluster = ~ iso3c, 
                                    type = "HC0", fix = T)

k4.linear <- coeftest(m1_log.n.unrq.linear, vcov. = m1_log.n.unrq.vcov.linear)


source("mi_regressions_do_region.R")
source("mi_aggregation_region.R")


