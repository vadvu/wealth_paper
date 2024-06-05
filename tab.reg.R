library(flextable)
tab.reg <- function(x){
  var <- unique(unlist(lapply(x, function(i){unique(i[,1])})))
  wh <- which(var=="I(gdppc.gap.ln_l^2)")
  var[wh] <- var[3]
  var[3] <- "I(gdppc.gap.ln_l^2)"
  rows <- 2*length(var)
  for (j in 1:length(x)){
    tab <- data.frame(var = unlist(lapply(var, function(i){c(i,"")})))
    tab <- left_join(tab, x[[j]], by = "var")
    for(i in seq(2,nrow(tab),2)){
      tab[i,2:3] <- tab[i-1,4:5]
    }
    tab <- tab[,1:3]
    tab[,-1] <- round(tab[,-1],2)
    tab$p1 <- NA
    tab$p2 <- NA
    for(i in seq(2,nrow(tab),2)){
      if(is.na(tab[i,2])){
        next
      }else{
        tab[i,]$p1 <- 2*pnorm(abs(as.numeric(tab[i,2])), lower.tail = F)
        tab[i,]$p2 <- 2*pnorm(abs(as.numeric(tab[i,3])), lower.tail = F)
        tab[i,2] <- paste0("(", tab[i,2], ")", 
                           ifelse(tab[i,]$p1<0.001, "***", ifelse(tab[i,]$p1 < 0.01, "**", 
                                                                  ifelse(tab[i,]$p1<0.05, "*", "")))) 
        tab[i,3] <- paste0("(", tab[i,3], ")",
                           ifelse(tab[i,]$p2<0.001, "***", ifelse(tab[i,]$p2 < 0.01, "**", 
                                                                  ifelse(tab[i,]$p2<0.05, "*", "")))) 
      }
    }
    tab <- tab[,1:3]
    colnames(tab)[2:3] <- c(paste0("m",j), paste0("m",j," imp"))
    if(j==1){
      full_tab = tab
    }else{
      full_tab = cbind(full_tab, tab[,-1])
    }
  }
  full_tab[full_tab =="0"] <- "<0.001"
  full_tab[full_tab =="(0)"] <- "(<0.001)"
  full_tab.flex <- full_tab %>% 
    flextable() %>% 
    add_header_row(values = c("","NAVCO 1.3","Beissinger's data"),colwidths = c(1,4,4)) %>% 
    add_header_lines("Table X. ") %>% 
    add_footer_lines("Note: ***p<0.001, **p<0.01, *p<0.05; z-statistics are in parenthesis; year fixed effecs are included in all models; standard errors are clustered on countries; models with multiple imputations are based on 100 imputed models") %>% 
    align(j = -1, align = "center", part = "all") %>% 
    valign(valign = "center", part = "header")
  return(full_tab.flex)
}
