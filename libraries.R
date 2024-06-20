
wealth.pack <- c(
  #data handling
  "tidyr", 
  "dplyr", 
  
  #read/write data
  "openxlsx",
  "readr",
  
  #plots/tables
  "ggplot2",
  "jtools",
  "latex2exp",
  "gtsummary",
  "gt",
  
  #imputations
  "Amelia",
  
  #analysis
  "randomForestSRC",
  "brglm2",
  "margins",
  "sandwich",
  "lmtest",
  
  #parallel computations
  "parallel",
  "foreach",
  "doParallel"
  )

if ( length(which(wealth.pack %in% installed.packages())) == length(wealth.pack)){
  sapply(wealth.pack, require, character.only = TRUE)
  } else {
  
    need.to.install = which( !(wealth.pack %in% installed.packages()) )
    install.ask = readline(
      cat("The analysis require libraries that are not found on your computer: \n", 
          paste0(wealth.pack[need.to.install], collapse = "\n "),
          "\n",
          "Install them? (write yes/no)"
          )
      )
    if(install.ask == "yes"){
      for(i in need.to.install){install.packages(wealth.pack[i])}
    } else { 
      paste0("The analysis will not be done")
    }
}







