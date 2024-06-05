library(MatchIt)
library(cobalt)

print("start of NVC matching process")
#### functions nvc
one.match <- function(x){
  #1
  ma1 <- matchit(NVC_1.3_NONVIOL ~ 
                   growth.gap.5_l + pop_ln_l + polity2_l + Polity5_durable_ln_l+
                   BESS_plus_WB_Urbanization_l+VDEM_v2x_execorr_l+
                   Mean_year_schooling_interpolated_l+
                   cumsum.nvc.nonviol,
                 method = "nearest", distance = "robust_mahalanobis", exact = c("year", "region"),
                 data = x, ratio = 2)
  ma1.balance <- bal.tab(ma1, thresholds = c(m = .2, v = 2), un = TRUE)
  m1 <- match.data(ma1)
  #2 
  ma2 <- matchit(NVC_1.3_NONVIOL ~ 
                   growth.gap.5_l + pop_ln_l + polity2_l + Polity5_durable_ln_l+
                   BESS_plus_WB_Urbanization_l+VDEM_v2x_execorr_l+
                   Mean_year_schooling_interpolated_l+
                   cumsum.nvc.nonviol,
                 method = "optimal", distance = "robust_mahalanobis", exact = c("year", "region"),
                 data = x, ratio = 2)
  ma2.balance <- bal.tab(ma2, thresholds = c(m = .2, v = 2), un = TRUE)
  m2 <- match.data(ma2)
  
  comb.balance = rbind(
    ma1.balance$Balance %>% mutate(model = "nearest"),
    ma2.balance$Balance %>% mutate(model = "optimal")
  )
  #comb
  typech <- c("nearest", "optimal")
  type = 0
  for(ma in list(m1,m2)){
    type = type + 1
    model <- glm(NVC_1.3_NONVIOL ~  gdppc.gap.ln_l + I(gdppc.gap.ln_l^2),
                 data = ma, family = "binomial")
    vc <- vcovHC(model)
    coef <- model$coefficients
    
    mar <- margins(model,
                   at = list("gdppc.gap.ln_l" = seq(
                     min(m1_log.n.nvc$model$gdppc.gap.ln_l),
                     max(m1_log.n.nvc$model$gdppc.gap.ln_l),
                     0.2)
                   ),
                   type = "response",
                   vcov = vc,
                   variables = "gdppc.gap.ln_l") %>% 
      summary() %>% 
      as.data.frame() %>% 
      mutate(model = typech[type])
    
    adjpred <- prediction(model,
                          at = list("gdppc.gap.ln_l" = seq(
                            min(m1_log.n.nvc$model$gdppc.gap.ln_l),
                            max(m1_log.n.nvc$model$gdppc.gap.ln_l),
                            0.2)
                          ),
                          type = "response",
                          vcov = vc,
                          calculate_se = TRUE) %>% 
      summary() %>% 
      select(x = `at(gdppc.gap.ln_l)`, y = Prediction, se = SE) %>% 
      mutate(model = typech[type])
    
    if(type == 1){
      comb.mar = mar
      comb.adjpred = adjpred
      comb.model = data.frame(var = names(coef),
                              coef = coef,
                              se = sqrt(diag(vc)),
                              model = typech[type])
    }else{
      comb.mar = rbind(comb.mar, mar)
      comb.adjpred = rbind(comb.adjpred, adjpred)
      comb.model = rbind(
        comb.model,
        data.frame(var = names(coef),
                   coef = coef,
                   se = sqrt(diag(vc)),
                   model = typech[type])
      )
    }
  }
  
  return(list(
    comb.mar,
    comb.adjpred,
    comb.model,
    comb.balance
  ))
  
}

####### nvc

library(parallel)
library(foreach)
library(doParallel)
library(MatchIt)
library(cobalt)

cl <- parallel::makeCluster(8)
doParallel::registerDoParallel(cl)
key1 <- foreach(i=list.files("imputed/"),
                .packages = c("dplyr", "tidyr", "margins","sandwich", "MatchIt", "cobalt")) %dopar% {
                  
                  dat = read.csv( paste0("imputed/",i) )%>% drop_na()
                  one.match(dat)
                }
parallel::stopCluster(cl)

for (i in 1:length(key1)){
  if(i == 1){
    all.me <- key1[[i]][[1]]
    all.coef <- key1[[i]][[3]]
    all.pred <- key1[[i]][[2]]
    all.balance <- key1[[i]][[4]]
  } else {
    all.me <- rbind(all.me, key1[[i]][[1]])
    all.coef <- rbind(all.coef, key1[[i]][[3]])
    all.pred <- rbind(all.pred, key1[[i]][[2]])
    all.balance <- rbind(all.balance, key1[[i]][[4]])
  }
}

write.csv(all.me, "imp_models/nvc.match.me_final.csv")
write.csv(all.coef, "imp_models/nvc.match.coef_final.csv")
write.csv(all.pred, "imp_models/nvc.match.pred_final.csv")
write.csv(all.balance, "imp_models/nvc.match.balance_final.csv")

print("NVC matching process completed successfully")


all.me <- read.csv("imp_models/nvc.match.me_final.csv")

math.me11.nvc <- all.me %>% 
  group_by(gdppc.gap.ln_l, model) %>% 
  mutate(ame.mean = sum(AME)/50,
         svq = (1+1/50)*sum( ( (AME-ame.mean)^2 )/49),
         var.mean = sum(SE^2)/50,
         se2 = var.mean+svq
  ) %>% 
  as.data.frame()

math.me11.nvc <- math.me11.nvc %>% distinct(gdppc.gap.ln_l, model, .keep_all	= T)


ggplot(math.me11.nvc, aes(gdppc.gap.ln_l, AME, group = model))+
  geom_ribbon(aes(ymin = ame.mean - 1.96*sqrt(se2), 
                  ymax = ame.mean + 1.96*sqrt(se2),
                  fill = model
  ), 
  alpha = 0.3, size = 0.3)+
  geom_line(aes(y = ame.mean, colour = model))+
  geom_hline(yintercept = 0)+
  jtools::theme_apa(legend.use.title = T)+
  labs(y = "AME", x = "GDP pc, ln (t-1)", fill = "Matching:", color = "Matching:")

ggsave("plots/matching.me.nvc_final.png", dpi = 600, device = "png", units = "cm", width = 25, height = 12.5)



all.pred <- read.csv("imp_models/nvc.match.pred_final.csv")

math.pred.nvc <- all.pred %>% 
  group_by(x, model) %>% 
  mutate(y.mean = sum(y)/50,
         svq = (1+1/50)*sum( ( (y-y.mean)^2 )/49),
         var.mean = sum(se^2)/50,
         se2 = var.mean+svq
  ) %>% 
  as.data.frame()

math.pred.nvc <- math.pred.nvc %>% distinct(x, model, .keep_all	= T)

ggplot(math.pred.nvc, aes(x, y.mean, group = model))+
  geom_ribbon(aes(ymin = ifelse(y.mean - 1.96*sqrt(se2)<0,0, y.mean - 1.96*sqrt(se2)), 
                  ymax = y.mean + 1.96*sqrt(se2),
                  fill = model
  ), 
  alpha = 0.3, size = 0.3)+
  geom_line(aes(y = y.mean, colour = model))+
  geom_hline(yintercept = 0)+
  jtools::theme_apa(legend.use.title = T)+
  labs(y = "Pr(Unarmed revolution)", x = "GDP pc, ln (t-1)", fill = "Matching:", color = "Matching:")+
  ylim( c(0, max(math.pred.nvc$y.mean + 1.96*sqrt(math.pred.nvc$se2)) )
        )


ggsave("plots/matching.pred.nvc_final.png", dpi = 600, device = "png", units = "cm", width = 25, height = 12.5)



#### table 
all.coef <- read.csv("imp_models/nvc.match.coef_final.csv")

math.coef.nvc <- all.coef %>% 
  group_by(var, model) %>% 
  mutate(coef.mean = sum(coef)/50,
         svq = (1+1/50)*sum( ( (coef-coef.mean)^2 )/49),
         var.mean = sum(se^2)/50,
         se2 = var.mean+svq
  ) %>% 
  as.data.frame()

math.coef.nvc <- math.coef.nvc %>% distinct(coef.mean, model, .keep_all	= T) %>% 
  select(-coef, -se) %>% 
  mutate(se = sqrt(se2)) %>% 
  select(-svq, -var.mean, -se2)




#########################################################



print("start of Bess matching process")
#### functions bess

one.match <- function(x){
  #1
  ma1 <- matchit(unarmed.rev.q ~ 
                   growth.gap.5_l + pop_ln_l + polity2_l + Polity5_durable_ln_l+
                   BESS_plus_WB_Urbanization_l+VDEM_v2x_execorr_l+
                   Mean_year_schooling_interpolated_l+
                   cumsum.unarmed.rev.q,
                 method = "nearest", distance = "robust_mahalanobis", exact = c("year", "region"),
                 data = x, ratio = 4)
  ma1.balance <- bal.tab(ma1, thresholds = c(m = .2, v = 2), un = TRUE)
  m1 <- match.data(ma1)
  #2 
  ma2 <- matchit(unarmed.rev.q ~ 
                   growth.gap.5_l + pop_ln_l + polity2_l + Polity5_durable_ln_l+
                   BESS_plus_WB_Urbanization_l+VDEM_v2x_execorr_l+
                   Mean_year_schooling_interpolated_l+
                   cumsum.unarmed.rev.q,
                 method = "optimal", distance = "robust_mahalanobis", exact = c("year", "region"),
                 data = x, ratio = 4)
  ma2.balance <- bal.tab(ma2, thresholds = c(m = .2, v = 2), un = TRUE)
  m2 <- match.data(ma2)
  
  comb.balance = rbind(
    ma1.balance$Balance %>% mutate(model = "nearest"),
    ma2.balance$Balance %>% mutate(model = "optimal")
  )
  #comb
  typech <- c("nearest", "optimal")
  type = 0
  for(ma in list(m1,m2)){
    type = type + 1
    model <- glm(unarmed.rev.q ~  gdppc.gap.ln_l + I(gdppc.gap.ln_l^2),
                 data = ma, family = "binomial")
    vc <- vcovHC(model)
    coef <- model$coefficients
    
    mar <- margins(model,
                   at = list("gdppc.gap.ln_l" = seq(
                     min(m1_log.n.unrq$model$gdppc.gap.ln_l),
                     max(m1_log.n.unrq$model$gdppc.gap.ln_l),
                     0.2)
                   ),
                   type = "response",
                   vcov = vc,
                   variables = "gdppc.gap.ln_l") %>% 
      summary() %>% 
      as.data.frame() %>% 
      mutate(model = typech[type])
    
    adjpred <- prediction(model,
                          at = list("gdppc.gap.ln_l" = seq(
                            min(m1_log.n.unrq$model$gdppc.gap.ln_l),
                            max(m1_log.n.unrq$model$gdppc.gap.ln_l),
                            0.2)
                          ),
                          type = "response",
                          vcov = vc,
                          calculate_se = TRUE) %>% 
      summary() %>% 
      select(x = `at(gdppc.gap.ln_l)`, y = Prediction, se = SE) %>% 
      mutate(model = typech[type])
    
    if(type == 1){
      comb.mar = mar
      comb.adjpred = adjpred
      comb.model = data.frame(var = names(coef),
                              coef = coef,
                              se = sqrt(diag(vc)),
                              model = typech[type])
    }else{
      comb.mar = rbind(comb.mar, mar)
      comb.adjpred = rbind(comb.adjpred, adjpred)
      comb.model = rbind(
        comb.model,
        data.frame(var = names(coef),
                   coef = coef,
                   se = sqrt(diag(vc)),
                   model = typech[type])
      )
    }
  }
  
  return(list(
    comb.mar,
    comb.adjpred,
    comb.model,
    comb.balance
  ))
  
}

####### bess

library(parallel)
library(foreach)
library(doParallel)
library(MatchIt)
library(cobalt)

cl <- parallel::makeCluster(8)
doParallel::registerDoParallel(cl)
key1 <- foreach(i=list.files("imputed/"),
                .packages = c("dplyr", "tidyr", "margins","sandwich", "MatchIt", "cobalt")) %dopar% {
                  
                  dat = read.csv( paste0("imputed/",i) )%>% drop_na()
                  one.match(dat)
                }
parallel::stopCluster(cl)

for (i in 1:length(key1)){
  if(i == 1){
    all.me <- key1[[i]][[1]]
    all.coef <- key1[[i]][[3]]
    all.pred <- key1[[i]][[2]]
    all.balance <- key1[[i]][[4]]
  } else {
    all.me <- rbind(all.me, key1[[i]][[1]])
    all.coef <- rbind(all.coef, key1[[i]][[3]])
    all.pred <- rbind(all.pred, key1[[i]][[2]])
    all.balance <- rbind(all.balance, key1[[i]][[4]])
  }
}

write.csv(all.me, "imp_models/rev.match.me_final.csv")
write.csv(all.coef, "imp_models/rev.match.coef_final.csv")
write.csv(all.pred, "imp_models/rev.match.pred_final.csv")
write.csv(all.balance, "imp_models/rev.match.balance_final.csv")


all.me <- read.csv("imp_models/rev.match.me_final.csv")

math.me11.nvc <- all.me %>% 
  group_by(gdppc.gap.ln_l, model) %>% 
  mutate(ame.mean = sum(AME)/50,
         svq = (1+1/50)*sum( ( (AME-ame.mean)^2 )/49),
         var.mean = sum(SE^2)/50,
         se2 = var.mean+svq
  ) %>% 
  as.data.frame()

math.me11.nvc <- math.me11.nvc %>% distinct(gdppc.gap.ln_l, model, .keep_all	= T)


ggplot(math.me11.nvc, aes(gdppc.gap.ln_l, AME, group = model))+
  geom_ribbon(aes(ymin = ame.mean - 1.96*sqrt(se2), 
                  ymax = ame.mean + 1.96*sqrt(se2),
                  fill = model
  ), 
  alpha = 0.3, size = 0.3)+
  geom_line(aes(y = ame.mean, colour = model))+
  geom_hline(yintercept = 0)+
  jtools::theme_apa(legend.use.title = T)+
  labs(y = "AME", x = "GDP pc, ln (t-1)", fill = "Matching:", color = "Matching:")

ggsave("plots/matching.me.rev_final.png", dpi = 600, device = "png", units = "cm", width = 25, height = 12.5)



all.pred <- read.csv("imp_models/rev.match.pred_final.csv")

math.pred.nvc <- all.pred %>% 
  group_by(x, model) %>% 
  mutate(y.mean = sum(y)/50,
         svq = (1+1/50)*sum( ( (y-y.mean)^2 )/49),
         var.mean = sum(se^2)/50,
         se2 = var.mean+svq
  ) %>% 
  as.data.frame()

math.pred.nvc <- math.pred.nvc %>% distinct(x, model, .keep_all	= T)

ggplot(math.pred.nvc, aes(x, y.mean, group = model))+
  geom_ribbon(aes(ymin = ifelse(y.mean - 1.96*sqrt(se2)<0,0, y.mean - 1.96*sqrt(se2)), 
                  ymax = y.mean + 1.96*sqrt(se2),
                  fill = model
  ), 
  alpha = 0.3, size = 0.3)+
  geom_line(aes(y = y.mean, colour = model))+
  geom_hline(yintercept = 0)+
  jtools::theme_apa(legend.use.title = T)+
  labs(y = "Pr(Unarmed revolution)", x = "GDP pc, ln (t-1)", fill = "Matching:", color = "Matching:")+
  ylim( c(0, max(math.pred.nvc$y.mean + 1.96*sqrt(math.pred.nvc$se2)) )
  )

ggsave("plots/matching.pred.rev_final.png", dpi = 600, device = "png", units = "cm", width = 25, height = 12.5)



#### table 
all.coef <- read.csv("imp_models/rev.match.coef_final.csv")

math.coef.rev <- all.coef %>% 
  group_by(var, model) %>% 
  mutate(coef.mean = sum(coef)/50,
         svq = (1+1/50)*sum( ( (coef-coef.mean)^2 )/49),
         var.mean = sum(se^2)/50,
         se2 = var.mean+svq
  ) %>% 
  as.data.frame()

math.coef.rev <- math.coef.rev %>% distinct(coef.mean, model, .keep_all	= T) %>% 
  select(-coef, -se) %>% 
  mutate(se = sqrt(se2)) %>% 
  select(-svq, -var.mean, -se2)


print("Bess matching process completed successfully")







##### combined table
math.coef.nvc$p <- ifelse(
  pnorm(abs(math.coef.nvc$coef.mean/math.coef.nvc$se), lower.tail = F)*2 < 0.001, "***", 
  ifelse(pnorm(abs(math.coef.nvc$coef.mean/math.coef.nvc$se), lower.tail = F)*2 < 0.01, "**",
         ifelse(pnorm(abs(math.coef.nvc$coef.mean/math.coef.nvc$se), lower.tail = F)*2<0.05, "*", ""))) 

math.coef.nvc[,4:5] <- lapply(math.coef.nvc[,4:5], function(x){round(x,2)})
math.coef.nvc$coef.mean <- paste0(math.coef.nvc$coef.mean, math.coef.nvc$p)
math.coef.nvc <- math.coef.nvc %>% select(-p)
math.coef.nvc <- cbind(math.coef.nvc[1:3,2:5], math.coef.nvc[4:6,3:5])



math.coef.rev$p <- ifelse(
  pnorm(abs(math.coef.rev$coef.mean/math.coef.rev$se), lower.tail = F)*2 < 0.001, "***", 
  ifelse(pnorm(abs(math.coef.rev$coef.mean/math.coef.rev$se), lower.tail = F)*2 < 0.01, "**",
         ifelse(pnorm(abs(math.coef.rev$coef.mean/math.coef.rev$se), lower.tail = F)*2<0.05, "*", ""))) 

math.coef.rev[,4:5] <- lapply(math.coef.rev[,4:5], function(x){round(x,2)})
math.coef.rev$coef.mean <- paste0(math.coef.rev$coef.mean, math.coef.rev$p)
math.coef.rev <- math.coef.rev %>% select(-p)
math.coef.rev <- cbind(math.coef.rev[1:3,2:5], math.coef.rev[4:6,3:5])

all.match.coef <- cbind(math.coef.nvc, math.coef.rev)

write.xlsx(all.match.coef[,-8], "tables/all.match.coef_final.xlsx")


###### combined balance

balance.nvc <- read.csv("imp_models/nvc.match.balance_final.csv")
balance.nvc$imp <- rep(1:27, 100)
balance.nvc <- balance.nvc %>% 
  filter(imp <=9) %>% 
  group_by(imp, model) %>% 
  mutate(mean.sm.before = mean(Diff.Un, na.rm = T),
         mean.sm.after = mean(Diff.Adj, na.rm = T),
         mean.vr.before = mean(V.Ratio.Un, na.rm = T),
         mean.vr.after = mean(V.Ratio.Adj, na.rm = T)
           ) %>% 
  as.data.frame() %>% 
  distinct(imp,model, .keep_all = T) %>% 
  select(X,model, "mean.sm.before", "mean.sm.after",  "mean.vr.before", "mean.vr.after")

balance.nvc[,-c(1:2)] <- lapply(balance.nvc[,-c(1:2)], function(x) round(x,2))



balance.bess <- read.csv("imp_models/rev.match.balance_final.csv")
balance.bess$imp <- rep(1:27, 100)
balance.bess <- balance.bess %>% 
  filter(imp <=9) %>% 
  group_by(imp, model) %>% 
  mutate(mean.sm.before = mean(Diff.Un, na.rm = T),
         mean.sm.after = mean(Diff.Adj, na.rm = T),
         mean.vr.before = mean(V.Ratio.Un, na.rm = T),
         mean.vr.after = mean(V.Ratio.Adj, na.rm = T)
  ) %>% 
  as.data.frame() %>% 
  distinct(imp,model, .keep_all = T) %>% 
  select(X,model, "mean.sm.before", "mean.sm.after",  "mean.vr.before", "mean.vr.after")

balance.bess[,-c(1:2)] <- lapply(balance.bess[,-c(1:2)], function(x) round(x,2))


all.balance <- cbind(balance.nvc, balance.bess[,-c(1:2)])

write.xlsx(all.balance, "tables/all.match.balance_final.xlsx")  
  
  
print("Whole matching process completed successfully")


