

print(paste0("ML procedure started: ", Sys.time()))


# navco

ml1 <- imbalanced(NVC_1.3_NONVIOL ~ gdppc.gap.ln_l +
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
                 data = data,
                 ntree = 5000,
                 mtry = 4,
                 nodesize = 1,
                 method = "rfq",
                 perf.type = "gmean",
                 splitrule = "entropy",
                 importance = "none",
                 fast = F,
                 seed = 2024,
                 forest = T
                 )

partial.obj <- partial(ml1,
                       partial.xvar = "gdppc.gap.ln_l",
                       partial.values = seq(
                         min(ml1$xvar$gdppc.gap.ln_l),
                         max(ml1$xvar$gdppc.gap.ln_l),
                         0.1)
)

prd.nvc <- data.frame(y = colMeans(partial.obj$classOutput[[1]])[3,], 
                      x = partial.obj$partial.values)

ggplot(prd.nvc, aes(x,y))+
  geom_line(color = "red", linewidth = 0.8)+
  geom_point(size = 1, color = "black")+
  theme_classic()+
  scale_y_continuous(labels = scales::percent)



# bess

ml2 <- imbalanced(unarmed.rev.q ~ gdppc.gap.ln_l +
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
                  data = data %>% mutate(unarmed.rev.q = as.factor(unarmed.rev.q)),
                  ntree = 5000,
                  mtry = 4,
                  nodesize = 1,
                  method = "rfq",
                  perf.type = "gmean",
                  splitrule = "entropy",
                  importance = "none",
                  fast = F,
                  seed = 2024,
                  forest = T
)

partial.obj2 <- partial(ml2,
                       partial.xvar = "gdppc.gap.ln_l",
                       partial.values = seq(
                         min(ml2$xvar$gdppc.gap.ln_l),
                         max(ml2$xvar$gdppc.gap.ln_l),
                         0.1)
)

prd.bess <- data.frame(y = colMeans(partial.obj2$classOutput[[1]])[3,], 
                      x = partial.obj2$partial.values)

ggplot(prd.bess, aes(x,y))+
  geom_line(color = "red", linewidth = 0.8)+
  geom_point(size = 1, color = "black")+
  theme_classic()+
  scale_y_continuous(labels = scales::percent)

#imputations 

source("ML_fun.R")

print(paste0("NVC 50 imputed iterations started: ", Sys.time()))

values <- seq(min(ml1$xvar$gdppc.gap.ln_l),
              max(ml1$xvar$gdppc.gap.ln_l),
              0.1)

for (part in 1:2){
  print(part)
  
  dip = 25*part
  
  cl <- parallel::makeCluster(9)
  doParallel::registerDoParallel(cl)
  key1 <- foreach(i=list.files("imputed/")[(dip-24):dip], 
                  .packages = c("randomForestSRC", 
                                "dplyr", "tidyr", "whoville"), 
                  .combine = rbind) %dopar% {
                    
                    dat = read.csv( paste0("imputed/",i) ) %>% mutate(year = as.factor(year))
                    imp_ml.nvc(data.new = dat, name = i, values = values)
                  }
  parallel::stopCluster(cl)
  
  if(part == 1){
    rcf.nvc <- key1
  }else{
    rcf.nvc <- rbind(rcf.nvc, key1)
  }
  
  rm(key1)
}


write.csv(rcf.nvc, "imp_models/rcf.nvc.csv")

print(paste0("NVC 50 imputed iterations ended successfully: ", Sys.time()))


#####


print(paste0("BESS 50 imputed iterations started: ", Sys.time()))

values <- seq(min(ml2$xvar$gdppc.gap.ln_l),
              max(ml2$xvar$gdppc.gap.ln_l),
              0.1)

for (part in 1:2){
  print(part)
  
  dip = 25*part
  
  cl <- parallel::makeCluster(9)
  doParallel::registerDoParallel(cl)
  key1 <- foreach(i=list.files("imputed/")[(dip-24):dip], 
                  .packages = c("randomForestSRC", 
                                "dplyr", "tidyr", "whoville"), 
                  .combine = rbind) %dopar% {
                    
                    dat = read.csv( paste0("imputed/",i) ) %>% mutate(year = as.factor(year))
                    imp_ml.rev(data.new = dat, name = i, values = values)
                  }
  parallel::stopCluster(cl)
  
  if(part == 1){
    rcf.rev <- key1
  }else{
    rcf.rev <- rbind(rcf.rev, key1)
  }
  
  rm(key1)
}


write.csv(rcf.rev, "imp_models/rcf.rev.csv")

print(paste0("BESS 50 imputed iterations ended successfully: ", Sys.time()))


##### aggreg

rcf.nvc <- read.csv("imp_models/rcf.nvc.csv")

rcf.nvc <- rcf.nvc %>% 
  group_by(x) %>% 
  mutate(y.mean = sum(y)/50
  ) %>% 
  as.data.frame()

ml.nvc.1 <- ggplot(rcf.nvc, aes(x,y, group = model))+
  geom_line(color = "grey", alpha = 0.25)+
  geom_line(aes(y = y.mean, color = "0"), size = 0.3)+
  geom_point(aes(y = y.mean, color = "0"), size = 1)+
  geom_line(data = prd.nvc %>% mutate(model = "1"), 
            aes(x = x, y = y, color = "1"), size = 0.3)+
  geom_point(data = prd.nvc %>% mutate(model = "1"), 
             aes(x = x, y = y, color = "1"), size = 1)+
  theme_minimal()+
  scale_color_manual(values = c("black", "red"), 
                     labels = c(TeX("$Pr(y=1|x)_{imp}"),
                                TeX("$Pr(y=1|x)_{data}")
                     ), 
                     name = "")+
  scale_y_continuous(labels = scales::percent)+
  labs(y = TeX("$Pr(y=1|x)"),
       x = "GDP pc, ln")+
  ggtitle("NAVCO 1.3")






rcf.rev <- read.csv("imp_models/rcf.rev.csv")

rcf.rev <- rcf.rev %>% 
  group_by(x) %>% 
  mutate(y.mean = sum(y)/50
  ) %>% 
  as.data.frame()

ml.rev.1 <- ggplot(rcf.rev, aes(x,y, group = model))+
  geom_line(color = "grey", alpha = 0.25)+
  geom_line(aes(y = y.mean, color = "0"), size = 0.3)+
  geom_point(aes(y = y.mean, color = "0"), size = 1)+
  geom_line(data = prd.bess %>% mutate(model = "1"), 
            aes(x = x, y = y, color = "1"), size = 0.3)+
  geom_point(data = prd.bess %>% mutate(model = "1"), 
             aes(x = x, y = y, color = "1"), size = 1)+
  theme_minimal()+
  scale_color_manual(values = c("black", "red"), 
                     labels = c(TeX("$Pr(y=1|x)_{imp}"),
                                TeX("$Pr(y=1|x)_{data}")
                     ), 
                     name = "")+
  scale_y_continuous(labels = scales::percent)+
  labs(y = TeX("$Pr(y=1|x)"),
       x = "GDP pc, ln")+
  ggtitle("Beissinger's data")


ggpubr::ggarrange(ml.nvc.1, ml.rev.1, common.legend = T)

ggsave("plots/ml.comp.pred.png", dpi = 600, device = "png", units = "cm", width = 25, height = 12.5)
