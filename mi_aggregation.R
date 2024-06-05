

#### aggregated ME navco

all.me.nvc <- read.csv("imp_models/all.me.nvc_final.csv")
all.me11.nvc <- all.me.nvc %>% 
  group_by(gdppc.gap.ln_l) %>% 
  mutate(ame.mean = sum(AME)/50,
         svq = (1+1/50)*sum( ( (AME-ame.mean)^2 )/49),
         var.mean = sum(SE^2)/50,
         se2 = var.mean+svq
  ) %>% 
  as.data.frame()

all.me11.nvc <- all.me11.nvc %>% distinct(gdppc.gap.ln_l, .keep_all	= T)

t1 <- ggplot(all.me11.nvc, aes(gdppc.gap.ln_l, AME, group = model))+
  geom_ribbon(data = mar1.n.nvc, aes(ymin = lower, ymax = upper), 
              alpha = 0.5, size = 0.3, fill = "pink")+
  geom_ribbon(aes(ymin = ame.mean - 1.96*sqrt(se2), 
                  ymax = ame.mean + 1.96*sqrt(se2)
  ), 
  alpha = 0.5, size = 0.3, fill = "lightblue")+
  geom_line(data = mar1.n.nvc, aes(gdppc.gap.ln_l, AME, color = "2"))+
  geom_line(aes(y = ame.mean, colour = "1"))+
  geom_hline(yintercept = 0)+
  scale_color_manual(values = c("blue", "red"), 
                     labels = c(
                       TeX("$ME_{imp} \\pm$ 95%CI"),
                       TeX("$ME_{data} \\pm$ 95%CI")
                     )
  )+
  jtools::theme_apa()+
  labs(x = "GDP pc, ln", y = "ME of GDP pc, ln")+
  ggtitle("NAVCO 1.3")



#### aggregated ME bess

all.me.rev <- read.csv("imp_models/all.me.rev_final.csv")

all.me11.rev0 <- all.me.rev %>% 
  group_by(gdppc.gap.ln_l) %>% 
  mutate(ame.mean = sum(AME)/50,
         svq = (1+1/50)*sum( ( (AME-ame.mean)^2 )/49),
         var.mean = sum(SE^2)/50,
         se2 = var.mean+svq
  ) %>% 
  as.data.frame()

all.me11.rev <- all.me11.rev0 %>% distinct(gdppc.gap.ln_l, .keep_all	= T)


t2 <- ggplot(all.me11.rev, aes(x = gdppc.gap.ln_l))+
  geom_ribbon(data = mar1.n.unrq, aes(ymin = lower, ymax = upper), 
              alpha = 0.5, size = 0.3, fill = "pink")+
  geom_ribbon(aes(ymin = ame.mean - 1.96*sqrt(se2), 
                  ymax = ame.mean + 1.96*sqrt(se2)
  ), 
  alpha = 0.5, size = 0.3, fill = "lightblue")+
  geom_line(data = mar1.n.unrq, aes(gdppc.gap.ln_l, AME, color = "2"))+
  geom_line(aes(y = ame.mean, colour = "1"))+
  geom_hline(yintercept = 0)+
  scale_color_manual(values = c("blue", "red"), 
                     labels = c(
                       TeX("$ME_{imp} \\pm$ 95%CI"),
                       TeX("$ME_{data} \\pm$ 95%CI")
                     )
  )+
  jtools::theme_apa()+
  labs(x = "GDP pc, ln", y = "ME of GDP pc, ln")+
  ggtitle("Beissinger's data")

##### combined ME
ggpubr::ggarrange(t1,t2, common.legend = T)

ggsave("plots/me.comp.MI_final.png", dpi = 600, device = "png", units = "cm", width = 25, height = 12.5)


##### aggregated tables 
### navco
all.coef.nvc <- read.csv("imp_models/all.coef.nvc_final.csv")

all.coef.nvc.table <- all.coef.nvc %>% 
  group_by(var) %>% 
  mutate(coef.mean = sum(coef)/50,
         svq = (1+1/50)*sum( ( (coef-coef.mean)^2 )/49),
         variance.mean = sum(se^2)/50,
         variance = variance.mean+svq,
         se.mean = sqrt(variance),
         z = coef.mean/se.mean,
         p = pnorm(abs(z), lower.tail = F),
         star = ifelse(p<0.001, "***", ifelse(p < 0.01, "**", ifelse(p<0.05, "*", "-")))) %>% 
  distinct(coef.mean, .keep_all	= T) %>% 
  as.data.frame() %>% 
  select(var, coef.mean, se.mean, z, p, star)

all.coef.nvc.initial <- data.frame(var = rownames(k1)[1:15], 
                                   coef = k1[1:15,1], 
                                   se = k1[1:15,2], 
                                   z.init = k1[1:15,3], 
                                   p = k1[1:15,4])
all.coef.nvc.initial <- all.coef.nvc.initial %>% 
  mutate(star.init = ifelse(p<0.001, "***", ifelse(p < 0.01, "**", ifelse(p<0.05, "*", "-"))))


all.coef.nvc.both <- cbind(all.coef.nvc.table[1:15,] %>% select(var, coef.mean, z, star), 
                           all.coef.nvc.initial %>% select(coef, z.init, star.init)) %>% 
  select(var, coef, coef.mean, z.init, z)

### bess
all.coef.rev <- read.csv("imp_models/all.coef.rev_final.csv")

all.coef.rev.table <- all.coef.rev %>% 
  group_by(var) %>% 
  mutate(coef.mean = sum(coef)/50,
         svq = (1+1/50)*sum( ( (coef-coef.mean)^2 )/49),
         variance.mean = sum(se^2)/50,
         variance = variance.mean+svq,
         se.mean = sqrt(variance),
         z = coef.mean/se.mean,
         p = pnorm(abs(z), lower.tail = F),
         star = ifelse(p<0.001, "***", ifelse(p < 0.01, "**", ifelse(p<0.05, "*", "-")))) %>% 
  distinct(coef.mean, .keep_all	= T) %>% 
  as.data.frame() %>% 
  select(var, coef.mean, se.mean, z, p, star)

all.coef.rev.initial <- data.frame(var = rownames(k4)[1:15], 
                                   coef = k4[1:15,1], 
                                   se = k4[1:15,2], 
                                   z.init = k4[1:15,3], 
                                   p = k4[1:15,4])
all.coef.rev.initial <- all.coef.rev.initial %>% 
  mutate(star.init = ifelse(p<0.001, "***", ifelse(p < 0.01, "**", ifelse(p<0.05, "*", "-"))))


all.coef.rev.both <- cbind(all.coef.rev.table[1:15,] %>% select(var, coef.mean, z, star), 
                           all.coef.rev.initial %>% select(coef, z.init, star.init)) %>% 
  select(var, coef, coef.mean, z.init, z)

#### linear navco
all.coef.nvc.lin <- read.csv("imp_models/all.coef.nvc.linear_final.csv")

all.coef.nvc.lin.table <- all.coef.nvc.lin %>% 
  group_by(var) %>% 
  mutate(coef.mean = sum(coef)/50,
         svq = (1+1/50)*sum( ( (coef-coef.mean)^2 )/49),
         variance.mean = sum(se^2)/50,
         variance = variance.mean+svq,
         se.mean = variance^0.5,
         z = coef.mean/se.mean,
         p = pnorm(abs(z), lower.tail = F),
         star = ifelse(p<0.001, "***", ifelse(p < 0.01, "**", ifelse(p<0.05, "*", "-")))) %>% 
  distinct(coef.mean, .keep_all	= T) %>% 
  as.data.frame() %>% 
  select(var, coef.mean, se.mean, z, p, star)

all.coef.nvc.initial.lin <- data.frame(var = rownames(k1.linear)[1:14], 
                                       coef = k1.linear[1:14,1], 
                                       se = k1.linear[1:14,2], 
                                       z.init = k1.linear[1:14,3], 
                                       p = k1.linear[1:14,4])
all.coef.nvc.initial.lin <- all.coef.nvc.initial.lin %>% 
  mutate(star.init = ifelse(p<0.001, "***", ifelse(p < 0.01, "**", ifelse(p<0.05, "*", "-"))))


all.coef.nvc.lin.both <- cbind(all.coef.nvc.lin.table[1:14,] %>% select(var, coef.mean, z, star), 
                               all.coef.nvc.initial.lin %>% select(coef, z.init, star.init)) %>% 
  select(var, coef, coef.mean, z.init, z)

#### linear bess 
all.coef.rev.lin <- read.csv("imp_models/all.coef.rev.linear_final.csv")

all.coef.rev.lin.table <- all.coef.rev.lin %>% 
  group_by(var) %>% 
  mutate(coef.mean = sum(coef)/50,
         svq = (1+1/50)*sum( ( (coef-coef.mean)^2 )/49),
         variance.mean = sum(se^2)/50,
         variance = variance.mean+svq,
         se.mean = variance^0.5,
         z = coef.mean/se.mean,
         p = pnorm(abs(z), lower.tail = F),
         star = ifelse(p<0.001, "***", ifelse(p < 0.01, "**", ifelse(p<0.05, "*", "-")))) %>% 
  distinct(coef.mean, .keep_all	= T) %>% 
  as.data.frame() %>% 
  select(var, coef.mean, se.mean, z, p, star)

all.coef.rev.initial.lin <- data.frame(var = rownames(k4.linear)[1:14], 
                                       coef = k4.linear[1:14,1], 
                                       se = k4.linear[1:14,2], 
                                       z.init = k4.linear[1:14,3], 
                                       p = k4.linear[1:14,4])
all.coef.rev.initial.lin <- all.coef.rev.initial.lin %>% 
  mutate(star.init = ifelse(p<0.001, "***", ifelse(p < 0.01, "**", ifelse(p<0.05, "*", "-"))))


all.coef.rev.lin.both <- cbind(all.coef.rev.lin.table[1:14,] %>% select(var, coef.mean, z, star), 
                               all.coef.rev.initial.lin %>% select(coef, z.init, star.init)) %>% 
  select(var, coef, coef.mean, z.init, z)



###### combined table 

library(flextable)

x = list(all.coef.rev.lin.both,
         all.coef.rev.both,
         all.coef.nvc.lin.both,
         all.coef.nvc.both)

for(i in 1:length(x)){
  x[[i]] <- x[[i]] %>% mutate(var = case_match(var,
                                               "sum.nvc.nonviol_l" ~ "sum_l", 
                                               "cumsum.nvc.nonviol" ~ "cumsum",
                                               "sum.nvc.nonviol" ~ "sum", 
                                               "cumsum.unarmed.rev.q" ~ "cumsum",
                                               "sum.rev.q.nonviol_l" ~ "sum_l", 
                                               "sum.rev.q.nonviol" ~ "sum", .default = var
  ))
}

source("tab.reg.R")

tab <- tab.reg(x)
tab
save_as_docx(tab, path = "tables/tab1_final.docx")


####### prediction
### navco
all.pred.nvc <- read.csv("imp_models/all.pred.nvc_final.csv")
all.pred.nvc <- all.pred.nvc %>% 
  group_by(x) %>% 
  mutate(y.mean = sum(y)/50,
         svq = (1+1/50)*sum( ( (y-y.mean)^2 )/49),
         var.mean = sum(se^2)/50,
         se2 = var.mean+svq
  ) %>% 
  as.data.frame()

all.pred.nvc.11 <- all.pred.nvc %>% distinct(x, .keep_all	= T)

pr.imp.nvc <- ggplot(all.pred.nvc.11, aes(x = x))+
  geom_ribbon(data = pred.nvc, aes(ymin = y-1.96*se, ymax = y+1.96*se), 
              alpha = 0.5, size = 0.3, fill = "pink")+
  geom_ribbon(aes(ymin = y.mean - 1.96*sqrt(se2), 
                  ymax = y.mean + 1.96*sqrt(se2)
  ), 
  alpha = 0.5, size = 0.3, fill = "lightblue")+
  geom_line(data = pred.nvc, aes(x, y, color = "2"))+
  geom_line(aes(y = y.mean, colour = "1"))+
  scale_color_manual(values = c("blue", "red"), 
                     labels = c(
                       TeX("$Pred_{imp} \\pm$ 95%CI"),
                       TeX("$Pred_{data} \\pm$ 95%CI")
                     )
  )+
  jtools::theme_apa()+
  labs(x = "GDP pc, ln", y = "Pr(Unarmed revolution)")+
  ggtitle("NAVCO 1.3")

#### bess
all.pred.rev <- read.csv("imp_models/all.pred.rev_final.csv")
all.pred.rev <- all.pred.rev %>% 
  group_by(x) %>% 
  mutate(y.mean = sum(y)/50,
         svq = (1+1/50)*sum( ( (y-y.mean)^2 )/49),
         var.mean = sum(se^2)/50,
         se2 = var.mean+svq
  ) %>% 
  as.data.frame()

all.pred.rev.11 <- all.pred.rev %>% distinct(x, .keep_all	= T)

pr.imp.rev <- ggplot(all.pred.rev.11, aes(x = x))+
  geom_ribbon(data = pred.rev, aes(ymin = y-1.96*se, ymax = y+1.96*se), 
              alpha = 0.5, size = 0.3, fill = "pink")+
  geom_ribbon(aes(ymin = y.mean - 1.96*sqrt(se2), 
                  ymax = y.mean + 1.96*sqrt(se2)
  ), 
  alpha = 0.5, size = 0.3, fill = "lightblue")+
  geom_line(data = pred.rev, aes(x, y, color = "2"))+
  geom_line(aes(y = y.mean, colour = "1"))+
  scale_color_manual(values = c("blue", "red"), 
                     labels = c(
                       TeX("$Pred_{imp} \\pm$ 95%CI"),
                       TeX("$Pred_{data} \\pm$ 95%CI")
                     )
  )+
  jtools::theme_apa()+
  labs(x = "GDP pc, ln", y = "Pr(Unarmed revolution)")+
  ggtitle("Beissinger's data")

ggpubr::ggarrange(pr.imp.nvc, pr.imp.rev, common.legend = T)

ggsave("plots/me.comp.pred_final.png", dpi = 600, device = "png", units = "cm", width = 25, height = 12.5)

