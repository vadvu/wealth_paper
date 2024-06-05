library(parallel)
library(foreach)
library(doParallel)

#### navco

for (part in 1:2){
  print(part)
  
  dip = 25*part
  
  cl <- parallel::makeCluster(10)
  doParallel::registerDoParallel(cl)
  key1 <- foreach(i=list.files("imputed/")[(dip-24):dip], 
                  .packages = c("brglm2", "dplyr", "tidyr", "margins", "clubSandwich", "sandwich")) %dopar% {
                    
                    dat = read.csv( paste0("imputed/",i) ) %>% mutate(year = as.factor(year))
                    imp_reg(x = m1_log.n.nvc, 
                            data.new = dat,
                            name = i)
                  }
  parallel::stopCluster(cl)
  
  for(j in 1:length(key1)){
    if(j == 1){
      all.me.i <- key1[[j]]$me
      all.coef.i <- key1[[j]]$mode
      all.pred.i <- key1[[j]]$pred
    } else {
      all.me.i <- rbind(all.me.i, key1[[j]]$me)
      all.coef.i <- rbind(all.coef.i, key1[[j]]$model)
      all.pred.i <- rbind(all.pred.i, key1[[j]]$pred)
    }
  }
  
  if(part == 1){
    all.me <- all.me.i
    all.coef <- all.coef.i
    all.pred <- all.pred.i
  }else{
    all.me <- rbind(all.me, all.me.i)
    all.coef <- rbind(all.coef, all.coef.i)
    all.pred <- rbind(all.pred, all.pred.i)
  }
  
  rm(all.me.i, all.coef.i, all.pred.i, key1)
}

write.csv(all.me, "imp_models/all.me.nvc_region_final.csv")
write.csv(all.coef, "imp_models/all.coef.nvc_region_final.csv")
write.csv(all.pred, "imp_models/all.pred.nvc_region_final.csv")

#### linear navco

for (part in 1:2){
  print(part)
  
  dip = 25*part
  
  cl <- parallel::makeCluster(10)
  doParallel::registerDoParallel(cl)
  key1 <- foreach(i=list.files("imputed/")[(dip-24):dip], .combine = rbind,
                  .packages = c("brglm2", "dplyr", "tidyr", "margins", "clubSandwich", "sandwich")) %dopar% {
                    
                    dat = read.csv( paste0("imputed/",i) ) %>% mutate(year = as.factor(year))
                    imp_reg_linear(x = m1_log.n.nvc.linear, 
                                   data.new = dat,
                                   name = i)
                  }
  parallel::stopCluster(cl)
  
  if(part == 1){
    all.coef <- key1
  }else{
    all.coef <- rbind(all.coef, key1)
  }
  
}

write.csv(all.coef, "imp_models/all.coef.nvc.linear_region_final.csv")

#### bess 

library(parallel)
library(foreach)
library(doParallel)

for (part in 1:2){
  print(part)
  
  dip = 25*part
  
  cl <- parallel::makeCluster(10)
  doParallel::registerDoParallel(cl)
  key1 <- foreach(i=list.files("imputed/")[(dip-24):dip], 
                  .packages = c("brglm2", "dplyr", "tidyr", "margins", "clubSandwich", "sandwich")) %dopar% {
                    
                    dat = read.csv( paste0("imputed/",i) ) %>% mutate(year = as.factor(year))
                    imp_reg(x = m1_log.n.unrq, 
                            data.new = dat,
                            name = i)
                  }
  parallel::stopCluster(cl)
  
  for(j in 1:length(key1)){
    if(j == 1){
      all.me.i <- key1[[j]]$me
      all.coef.i <- key1[[j]]$model
      all.pred.i <- key1[[j]]$pred
    } else {
      all.me.i <- rbind(all.me.i, key1[[j]]$me)
      all.coef.i <- rbind(all.coef.i, key1[[j]]$model)
      all.pred.i <- rbind(all.pred.i, key1[[j]]$pred)
    }
  }
  
  if(part == 1){
    all.me <- all.me.i
    all.coef <- all.coef.i
    all.pred <- all.pred.i
  }else{
    all.me <- rbind(all.me, all.me.i)
    all.coef <- rbind(all.coef, all.coef.i)
    all.pred <- rbind(all.pred, all.pred.i)
  }
  
  rm(all.me.i, all.coef.i, all.pred.i,key1)
}

write.csv(all.me, "imp_models/all.me.rev_region_final.csv")
write.csv(all.coef, "imp_models/all.coef.rev_region_final.csv")
write.csv(all.pred, "imp_models/all.pred.rev_region_final.csv")

##### bess linear

for (part in 1:2){
  print(part)
  
  dip = 25*part
  
  cl <- parallel::makeCluster(10)
  doParallel::registerDoParallel(cl)
  key1 <- foreach(i=list.files("imputed/")[(dip-24):dip], .combine = rbind,
                  .packages = c("brglm2", "dplyr", "tidyr", "margins", "clubSandwich", "sandwich")) %dopar% {
                    
                    dat = read.csv( paste0("imputed/",i) ) %>% mutate(year = as.factor(year))
                    imp_reg_linear(x = m1_log.n.unrq.linear, 
                                   data.new = dat,
                                   name = i)
                  }
  parallel::stopCluster(cl)
  
  if(part == 1){
    all.coef <- key1
  }else{
    all.coef <- rbind(all.coef, key1)
  }
  
}

write.csv(all.coef, "imp_models/all.coef.rev.linear_region_final.csv")
