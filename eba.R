library(parallel)
library(foreach)
library(doParallel)

start.time = Sys.time()

#functions
gmeans <- function(x){
  df <- data.frame(tres = seq(0.01, 0.1, 0.005), gmean = NA)
  for(tres in df$tres){
    predicted_values<-ifelse(predict(x,type="response")>tres,1,0)
    actual_values<-x$model[,1]
    conf_matrix<-table(predicted_values,actual_values)
    conf_matrix[is.na(conf_matrix)] = 0
    sens <- conf_matrix[2,2]/(conf_matrix[1,2]+conf_matrix[2,2])
    spec <- conf_matrix[1,1]/(conf_matrix[1,1]+conf_matrix[2,1])
    df[df$tres==tres,2] <- (sens*spec)^0.5
  }
  return(df[which(df$gmean == max(df$gmean)),])
}

comb.many <- function(x,n){
  for(i in n){
    if(i == n[1]){
      comb <- list()
    }
    comb[[paste0(i)]] <- t(combn(x, i))
  }
  return(comb)
}


#### nvc

dep <- "NVC_1.3_NONVIOL"
always <- c("year", "pop_ln_l", "polity2_l", "polity2_l.sq")
interest <- c("gdppc.gap.ln_l")
control <- c("growth.gap.5_l", "Polity5_durable_ln_l", "BESS_plus_WB_Urbanization_l", 
             "VDEM_v2x_execorr_l", "v2xeg_eqdr_l",
             "Mean_year_schooling_interpolated_l", "WPP_15_24_share_15plus_l", 
             "oil.pc.ln_l", "discrimpop_l",
             "sum.nvc.nonviol","sum.nvc.nonviol_l","cumsum.nvc.nonviol")

k <- comb.many(control, 4:length(control))
print(
  paste0(
    "Number of models for nvc eba: ",
    lapply(k, function(x) length(x)) %>% unlist() %>% sum() * 2 * 50
    )
)


for(j in k){
  print(ncol(j))
  if(ncol(j) == ncol(k[[1]])){
    fin <- data.frame(var = NA, coef = NA, se = NA, gmean = NA)
    cl <- parallel::makeCluster(6)
    doParallel::registerDoParallel(cl)
  }
  fin.i <- foreach(i = 1:nrow(j), .combine = rbind,  
                   .packages = c("brglm2", 
                                 "clubSandwich", 
                                 "sandwich",
                                 "lmtest",
                                 "dplyr",
                                 "foreach")) %dopar% {
                                   dates <- list.files("imputed")
                                   comb.i <- j[i,]
                                   mi.res <- foreach(z = dates, .combine = rbind) %do% {
                                     dat.i <- read.csv(paste0("imputed/",z))
                                     dat.i$year = as.factor(dat.i$year)
                                     dat.i$polity2_l.sq <- (dat.i$polity2_l)^2
                                     mi <- glm(data = dat.i[,c(dep, always, interest, comb.i)],
                                               family = binomial(link = 'logit'),
                                               method = "brglmFit", type = "AS_mean",
                                               NVC_1.3_NONVIOL ~ gdppc.gap.ln_l + I(gdppc.gap.ln_l^2)+.)
                                     mi.c <- coeftest(mi, vcov. =
                                                        vcovCL(mi, cluster = dat.i$iso3c, type = "HC0"))
                                     gm <- gmeans(mi)
                                     eba.out <- data.frame(var = c("gdp1", "gdp2"),
                                                           coef = mi.c[2:3,1], 
                                                           se = mi.c[2:3,2],
                                                           gmean = gm[1,2])
                                     ### linear
                                     
                                     mi <- glm(data = dat.i[,c(dep, always, interest, comb.i)],
                                               family = binomial(link = 'logit'),
                                               method = "brglmFit", type = "AS_mean",
                                               NVC_1.3_NONVIOL ~ gdppc.gap.ln_l + .)
                                     mi.c <- coeftest(mi, vcov. =
                                                        vcovCL(mi, cluster = dat.i$iso3c, type = "HC0"))
                                     gm <- gmeans(mi)
                                     eba.out <- rbind(eba.out,data.frame(var = c("gdp"),
                                                                         coef = mi.c[2,1], 
                                                                         se = mi.c[2,2],
                                                                         gmean = gm[1,2]))
                                     
                                   }
                                   mi.res <- mi.res %>% 
                                     group_by(var) %>% 
                                     mutate(coef.m = sum(coef)/50,
                                            svq = (1+1/50)*sum( ( (coef-coef.m)^2 )/49),
                                            var.mean = sum(se^2)/50,
                                            se2 = sqrt(var.mean+svq), 
                                            gmean.m = sum(gmean)/50) %>%
                                     as.data.frame() %>% 
                                     distinct(coef.m, .keep_all	= T) %>% 
                                     select(var, coef = coef.m, se = se2, gmean = gmean.m)
                                   
                                   mi.res
                                 }
  fin <- rbind(fin, fin.i)
}
parallel::stopCluster(cl)  
  

fin1 <- fin %>% 
  drop_na() %>% 
  group_by(var) %>% 
  mutate(w = gmean/sum(gmean),
         b.mean = sum(coef*w),
         se.mean = sum( (se^2)*w)^0.5,
         p.mean =2*pnorm(-abs(b.mean/se.mean))) %>% 
  as.data.frame()

write.xlsx(fin1, "imp_models/eba_nvc_4_to_12_final.xlsx")

eba.nvc <- ggplot(fin1,aes(x = coef/se, fill = var))+
  geom_density(alpha = 0.5, kernel = "epanechnikov")+
  theme_classic()+
  geom_vline(xintercept = c(1.96, -1.96), linetype = "dotted")+
  labs(y = "Density",
       x = "z-statistic")+
  scale_fill_manual(labels = c("GDP pc, ln",
                               "GDP pc, ln, linear term",
                               "GDP pc, ln, squared term"),
                    values = c("orange", "lightgreen", "steelblue"),
                    name = "Variable:"
  )+
  xlim( c( -1 + min(fin1$coef/fin1$se), max(fin1$coef/fin1$se)+1 ) )

eba.nvc

ggsave("plots/eba_nvc_final.png", dpi = 600, device = "png", units = "cm", width = 25, height = 12.5)
  
  


dep <- "unarmed.rev.q"
always <- c("year", "pop_ln_l", "polity2_l", "polity2_l.sq")
interest <- c("gdppc.gap.ln_l")
control <- c("growth.gap.5_l", "Polity5_durable_ln_l", "BESS_plus_WB_Urbanization_l", 
             "VDEM_v2x_execorr_l", "v2xeg_eqdr_l",
             "Mean_year_schooling_interpolated_l", "WPP_15_24_share_15plus_l", 
             "oil.pc.ln_l", "discrimpop_l",
             "sum.nvc.nonviol","sum.nvc.nonviol_l","cumsum.nvc.nonviol")

k <- comb.many(control, 4:length(control))
print(
  paste0(
    "Number of models for bess eba: ",
    lapply(k, function(x) length(x)) %>% unlist() %>% sum() * 2 * 50
  )
)

for(j in k){
  print(ncol(j))
  if(ncol(j) == ncol(k[[1]])){
    fin <- data.frame(var = NA, coef = NA, se = NA, gmean = NA)
    cl <- parallel::makeCluster(6)
    doParallel::registerDoParallel(cl)
  }
  fin.i <- foreach(i = 1:nrow(j), .combine = rbind,  
                   .packages = c("brglm2", 
                                 "clubSandwich", 
                                 "sandwich",
                                 "lmtest",
                                 "dplyr",
                                 "foreach")) %dopar% {
                                   dates <- list.files("imputed")
                                   comb.i <- j[i,]
                                   mi.res <- foreach(z = dates, .combine = rbind) %do% {
                                     dat.i <- read.csv(paste0("imputed/",z))
                                     dat.i$year = as.factor(dat.i$year)
                                     dat.i$polity2_l.sq <- (dat.i$polity2_l)^2
                                     mi <- glm(data = dat.i[,c(dep, always, interest, comb.i)],
                                               family = binomial(link = 'logit'),
                                               method = "brglmFit", type = "AS_mean",
                                               unarmed.rev.q ~ gdppc.gap.ln_l + I(gdppc.gap.ln_l^2)+.)
                                     mi.c <- coeftest(mi, vcov. =
                                                        vcovCL(mi, cluster = dat.i$iso3c, type = "HC0"))
                                     gm <- gmeans(mi)
                                     eba.out <- data.frame(var = c("gdp1", "gdp2"),
                                                           coef = mi.c[2:3,1], 
                                                           se = mi.c[2:3,2],
                                                           gmean = gm[1,2])
                                     ### linear
                                     
                                     mi <- glm(data = dat.i[,c(dep, always, interest, comb.i)],
                                               family = binomial(link = 'logit'),
                                               method = "brglmFit", type = "AS_mean",
                                               unarmed.rev.q ~ gdppc.gap.ln_l + .)
                                     mi.c <- coeftest(mi, vcov. =
                                                        vcovCL(mi, cluster = dat.i$iso3c, type = "HC0"))
                                     gm <- gmeans(mi)
                                     eba.out <- rbind(eba.out,data.frame(var = c("gdp"),
                                                                         coef = mi.c[2,1], 
                                                                         se = mi.c[2,2],
                                                                         gmean = gm[1,2]))
                                     
                                   }
                                   mi.res <- mi.res %>% 
                                     group_by(var) %>% 
                                     mutate(coef.m = sum(coef)/50,
                                            svq = (1+1/50)*sum( ( (coef-coef.m)^2 )/49),
                                            var.mean = sum(se^2)/50,
                                            se2 = sqrt(var.mean+svq), 
                                            gmean.m = sum(gmean)/50) %>%
                                     as.data.frame() %>% 
                                     distinct(coef.m, .keep_all	= T) %>% 
                                     select(var, coef = coef.m, se = se2, gmean = gmean.m)
                                   
                                   mi.res
                                 }
  fin <- rbind(fin, fin.i)
}
parallel::stopCluster(cl)

fin1 <- fin %>% 
  drop_na() %>% 
  group_by(var) %>% 
  mutate(w = gmean/sum(gmean),
         b.mean = sum(coef*w),
         se.mean = sum( (se^2)*w)^0.5,
         p.mean =2*pnorm(-abs(b.mean/se.mean))) %>% 
  as.data.frame()

write.xlsx(fin1, "imp_models/eba_kor_4_to_12_final.xlsx")

eba.nvc <- ggplot(fin1,aes(x = coef/se, fill = var))+
  geom_density(alpha = 0.5, kernel = "epanechnikov")+
  theme_classic()+
  geom_vline(xintercept = c(1.96, -1.96), linetype = "dotted")+
  labs(y = "Density",
       x = "z-statistic")+
  scale_fill_manual(labels = c("GDP pc, ln",
                               "GDP pc, ln, linear term",
                               "GDP pc, ln, squared term"),
                    values = c("orange", "lightgreen", "steelblue"),
                    name = "Variable:"
  )+
  xlim( c( -1 + min(fin1$coef/fin1$se), max(fin1$coef/fin1$se)+1 ) )

eba.nvc

ggsave("plots/eba_kor_final.png", dpi = 600, device = "png", units = "cm", width = 25, height = 12.5)
  


nvc.eba <- openxlsx::read.xlsx("imp_models/eba_nvc_4_to_12_final.xlsx")
kor.eba <- openxlsx::read.xlsx("imp_models/eba_kor_4_to_12_final.xlsx")

nvc.eba <- nvc.eba %>% distinct(b.mean, .keep_all = T) %>% select(var, b.mean, se.mean, p.mean)
kor.eba <- kor.eba %>% distinct(b.mean, .keep_all = T) %>% select(var, b.mean, se.mean, p.mean)
eba.comb <- cbind(nvc.eba, kor.eba[,-1])
eba.comb[,-1] <- lapply(eba.comb[,-1], function(x){round(x,3)}) 

write.xlsx(eba.comb, file = "tables/eba_comb_final.xlsx")
  
end.time = Sys.time()

print(paste0("execution time for double eba is: ", end.time - start.time))
