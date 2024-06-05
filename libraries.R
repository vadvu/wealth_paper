suppressWarnings({ 
  
  suppressMessages({
    library(tidyr) #data handling
    library(dplyr) #darta handling
    library(openxlsx) #read data
    library(readr) #read data
    
    library(ggplot2) #plots
    library(jtools) #plots
    library(latex2exp) #plots
    
    library(Amelia) #pachage for multiplt imputation with panel data
    
    library(randomForestSRC) #ML models for rare events, you dont need it
    library(csra) #github package
    library(brglm2) #for rare events logistic regression
    library(bgeva) #you dont need it 
    library(margins) #marginal effects
    library(sjPlot) #good tool for models vizualization 
    library(ggeffects) #to analyze models
    library(sandwich) #usual HC SE
    library(lmtest) #for testing models with user-specific SE
    library(clubSandwich) #for clustered SE
    
    library(parallel)
    library(foreach)
    library(doParallel)
  })
  
}) 
print("libraries ok")