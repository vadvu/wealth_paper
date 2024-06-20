set.seed(2024)
data0 <- data %>% select(iso3c, year, region, #id
                         #DV
                         NVC_1.3_NONVIOL, unarmed.rev.q, 
                         #vars based on DV
                         "sum.nvc.nonviol","sum.rev.q.nonviol",
                         "sum.nvc.nonviol_l","sum.rev.q.nonviol_l",
                         "cumsum.nvc.nonviol","cumsum.unarmed.rev.q",
                         #other vars
                         polity2_l, "gdppc.gap.ln_l", "BESS_plus_WB_Urbanization_l",
                         "WPP_15_24_share_15plus_l", "Mean_year_schooling_interpolated_l", "Polity5_durable_ln_l",
                         "VDEM_v2x_execorr_l", "pop_ln_l", "v2xeg_eqdr_l",
                         oil.pc.ln_l, discrimpop_l, polity2_l.sq, growth.gap.5_l
                         )

vars = colnames(data0)[-c(1:5)]
bds <- as.matrix(
  rbind(
    c(which(colnames(data0)=="BESS_plus_WB_Urbanization_l"), 0, 100), #BESS_plus_WB_Urbanization_l
    c(which(colnames(data0)=="WPP_15_24_share_15plus_l"),0.05,0.65), #WPP_15_24_share_15plus_l
    c(which(colnames(data0)=="Mean_year_schooling_interpolated_l"), 0, 15), #Mean_year_schooling_interpolated_l
    c(which(colnames(data0)=="VDEM_v2x_execorr_l"),0,1), #VDEM_v2x_execorr_l
    c(which(colnames(data0)=="polity2_l"),-10,10), #polity
    c(which(colnames(data0)=="polity2_l.sq"),0,100), #polity sq
    c(which(colnames(data0)=="growth.gap.5_l"),-1,1), #5-year av growth with lag
    c(which(colnames(data0)=="Polity5_durable_ln_l"),0,5),
    c(which(colnames(data0)=="v2xeg_eqdr_l"),0,1), #inequality index 
    c(which(colnames(data0)=="discrimpop_l"),0,1) #share of disc pop
  )
)





cl =  parallel::makePSOCKcluster(9)
a.out1 <- amelia(data0,
                 m = 50,
                 ts = "year",
                 cs = "iso3c",
                 lags = vars,
                 leads = vars,
                 polytime = 3, intercs = F,
                 ords = c("polity2_l", "polity2_l.sq", 
                          "cumsum.unarmed.rev.q",
                          "cumsum.nvc.nonviol"), 
                 noms = c("sum.rev.q.nonviol_l", "sum.rev.q.nonviol", "sum.nvc.nonviol_l", "sum.nvc.nonviol"),
                 incheck = T,
                 startvals = 1,
                 idvars = c("NVC_1.3_NONVIOL",
                            "unarmed.rev.q",
                            "region"),
                 empri = 0.005*nrow(data0),
                 autopri = 0.05,
                 bounds = bds,
                 boot.type	 = "ordinary",
                 ncpus = 9,
                 max.resample = 10000, 
                 p2s = 1,
                 parallel = "snow",
                 cl = cl
)
parallel::stopCluster(cl)
summary(a.out1)
# plot(a.out1)


write.amelia(obj = a.out1, file.stem = "imputed/imp")


print("imp ok")
