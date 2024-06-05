imp_reg <- function(x, data.new, name){
  mn <- update(x, data = data.new)
  vcovn <- vcovCL(mn, type = "HC0", cluster = data.new$iso3c)
  sen <- diag(sqrt(vcovn))
  coef <- mn$coefficients
  marn <- margins(mn,
                  at = list("gdppc.gap.ln_l" = seq(
                    min(x$model$gdppc.gap.ln_l),
                    max(x$model$gdppc.gap.ln_l),
                    0.2)
                  ), 
                  type = "response", 
                  vcov = vcovn, 
                  variables = "gdppc.gap.ln_l"
  ) %>% 
    summary() %>% 
    as.data.frame() %>% 
    mutate(model = name) %>% 
    select(gdppc.gap.ln_l, AME, SE, model)
  
  pred <- prediction(mn,
                     at = list("gdppc.gap.ln_l" = 
                                 seq(min(x$model$gdppc.gap.ln_l),
                                     max(x$model$gdppc.gap.ln_l),
                                     0.2)
                     ),
                     type = "response",
                     vcov = vcovn,
                     calculate_se = TRUE) %>%
    summary() %>%
    select(x = `at(gdppc.gap.ln_l)`, y = Prediction, se = SE) %>% 
    mutate(model = name)
  
  mod <- data.frame(var = names(coef), coef = coef, se = sen, model = name)
  
  return(list(model = mod, pred = pred, me = marn))
}



imp_reg_linear <- function(x, data.new, name){
  mn <- update(x, data = data.new)
  vcovn <- vcovCL(mn, type = "HC0", cluster = data.new$iso3c)
  sen <- diag(sqrt(vcovn))
  coef <- mn$coefficients
  mod <- data.frame(var = names(coef), coef = coef, se = sen, model = name)
  
  return(mod)
}