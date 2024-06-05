imp_ml.nvc <- function(data.new, name, values){
  mn <- imbalanced(NVC_1.3_NONVIOL ~ gdppc.gap.ln_l +
                     growth.gap.5_l+
                     pop_ln_l+
                     polity2_l+
                     Polity5_durable_ln_l+
                     BESS_plus_WB_Urbanization_l+
                     VDEM_v2x_execorr_l+
                     Mean_year_schooling_interpolated_l+
                     oil.pc.ln_l+
                     sum.nvc.nonviol+
                     sum.nvc.nonviol_l+
                     cumsum.nvc.nonviol+
                     year,
                   data = data.new %>% 
                     mutate(NVC_1.3_NONVIOL = as.factor(NVC_1.3_NONVIOL),
                            year = as.factor(year),
                            sum.nvc.nonviol = factor(sum.nvc.nonviol),
                            sum.nvc.nonviol_l = factor(sum.nvc.nonviol_l),
                            regn = as.factor(whoville::iso3_to_regions(iso3c, 
                                                                       region = "un_subregion", name = F))
                     ),
                   ntree = 3000,
                   mtry = 4,
                   nodesize = 1,
                   method = "rfq",
                   perf.type = "gmean",
                   splitrule = "entropy",
                   importance = "none",
                   fast = T, forest = T,
                   seed = 2024
  )
  partial.obj <- partial(mn,
                         partial.xvar = "gdppc.gap.ln_l",
                         partial.values = values
  )
  prd <- data.frame(y = colMeans(partial.obj$classOutput[[1]])[3,],
                    x = partial.obj$partial.values,
                    model = name)
  
  return(prd)
}



imp_ml.rev <- function(data.new, name, values){
  mn <- imbalanced(unarmed.rev.q ~ gdppc.gap.ln_l +
                     growth.gap.5_l+
                     pop_ln_l+
                     polity2_l+
                     Polity5_durable_ln_l+
                     BESS_plus_WB_Urbanization_l+
                     VDEM_v2x_execorr_l+
                     Mean_year_schooling_interpolated_l+
                     oil.pc.ln_l+
                     sum.nvc.nonviol+
                     sum.nvc.nonviol_l+
                     cumsum.nvc.nonviol+
                     year,
                   data = data.new %>% 
                     mutate(unarmed.rev.q = as.factor(unarmed.rev.q),
                            year = as.factor(year),
                            sum.rev.q.nonviol = factor(sum.rev.q.nonviol),
                            sum.rev.q.nonviol_l = factor(sum.rev.q.nonviol_l),
                            regn = as.factor(whoville::iso3_to_regions(iso3c, 
                                                                       region = "un_subregion", name = F))
                     ),
                   ntree = 3000,
                   mtry = 4,
                   nodesize = 1,
                   method = "rfq",
                   perf.type = "gmean",
                   splitrule = "entropy",
                   importance = "none",
                   seed = 2024, fast = T, forest = T
  )
  partial.obj <- partial(mn,
                         partial.xvar = "gdppc.gap.ln_l",
                         partial.values = values
  )
  prd <- data.frame(y = colMeans(partial.obj$classOutput[[1]])[3,],
                    x = partial.obj$partial.values,
                    model = name)
  
  return(prd)
}