  
  ## Switch back to version 4.1 or later
   
  cat( "\014" )
  rm(list=ls(all.names=TRUE))
  setwd("***/RES")   # set WD
  
  ## Load libraries
  library(tidyverse)
  library(lubridate)
  library(psych)
  library(janitor)

  
  for_results.1 <- read.csv("RES2_phtt_1.csv")  
  for_results.2 <- read.csv("RES2_phtt_2.csv")  
  for_results.3 <- read.csv("RES2_phtt_3.csv")  
  for_results.4 <- read.csv("RES2_phtt_4.csv") 
  for_results.5 <- read.csv("RES2_phtt_5.csv") 
  for_results.6 <- read.csv("RES2_phtt_6.csv") 
  for_results.7 <- read.csv("RES2_phtt_7.csv") 

  for_results_1 <- t(for_results.1)
  for_results_1 <- for_results_1 %>% row_to_names(row_number = 1)
  namelist_1 <- rownames(for_results_1)
  rowname_1 <- paste0("lag1_",namelist_1)
  rownames(for_results_1) <- rowname_1
  
  for_results_2 <- t(for_results.2)
  for_results_2 <- for_results_2 %>% row_to_names(row_number = 1)
  namelist_2 <- rownames(for_results_2)
  rowname_2 <- paste0("lag2_",namelist_2)
  rownames(for_results_2) <- rowname_2
  
  for_results_3 <- t(for_results.3)
  for_results_3 <- for_results_3 %>% row_to_names(row_number = 1)
  namelist_3 <- rownames(for_results_3)
  rowname_3 <- paste0("lag3_",namelist_3)
  rownames(for_results_3) <- rowname_3
  
  for_results_4 <- t(for_results.4)
  for_results_4 <- for_results_4 %>% row_to_names(row_number = 1)
  namelist_4 <- rownames(for_results_4)
  rowname_4 <- paste0("lag4_",namelist_4)
  rownames(for_results_4) <- rowname_4
  
  for_results_5 <- t(for_results.5)
  for_results_5 <- for_results_5 %>% row_to_names(row_number = 1)
  namelist_5 <- rownames(for_results_5)
  rowname_5 <- paste0("lag5_",namelist_5)
  rownames(for_results_5) <- rowname_5
  
  for_results_6 <- t(for_results.6)
  for_results_6 <- for_results_6 %>% row_to_names(row_number = 1)
  namelist_6 <- rownames(for_results_6)
  rowname_6 <- paste0("lag6_",namelist_6)
  rownames(for_results_6) <- rowname_6
  
  for_results_7 <- t(for_results.7)
  for_results_7 <- for_results_7 %>% row_to_names(row_number = 1)
  namelist_7 <- rownames(for_results_7)
  rowname_7 <- paste0("lag7_",namelist_7)
  rownames(for_results_7) <- rowname_7
  
  
  
 for_results <- rbind (for_results_1, for_results_2, for_results_3, for_results_4, for_results_5, for_results_6, for_results_7)
  for_results <- for_results [-1,]
  for_results <- for_results %>% as.data.frame() %>% tibble::rownames_to_column(var = "var")
  for_results <- for_results %>% dplyr::mutate(Waves = stringr::str_sub(var, -1))
  for_results <- for_results %>% dplyr:: rename(Coefficients = coefficients.Estimate, Standard_Error =	coefficients.Std.Err)

  for_results <- for_results %>% dplyr::mutate(Phase = if_else(stringr::str_detect(var, pattern="pos"),"Increasing", 
                                                       if_else(stringr::str_detect(var, pattern="neg"),"Decreasing", "NA")))
 

  for_results <- for_results %>% dplyr::mutate(Doses =  case_when( 
    stringr::str_detect(var, pattern="vaccine_1")~"1",　　# "1st_dose"
    stringr::str_detect(var, pattern="vaccine_2")~"2",    # "2nd_dose"
    stringr::str_detect(var, pattern="vaccine_3")~"3",    # "3rd_dose"
    stringr::str_detect(var, pattern="vaccine_rate_1")~"1",　　# "1st_dose"
    stringr::str_detect(var, pattern="vaccine_rate_2")~"2",    # "2nd_dose"
    stringr::str_detect(var, pattern="vaccine_rate_3")~"3",    # "3rd_dose"
    TRUE ~ "NA" 
  ) 
  )
  
  
  for_results <- for_results %>% dplyr::mutate(
    Variables = 
      case_when( 
        stringr::str_detect(var, pattern="case_ihs_pos_lag")~ "Cases",
        stringr::str_detect(var, pattern="case_ihs_neg_lag")~"Cases", 
        stringr::str_detect(var, pattern="W_case")~"Weighted_Cases",
        stringr::str_detect(var, pattern="W_emergency")~"Weighted_Emergency",
        stringr::str_detect(var, pattern="emergency")~"Emergency",
        stringr::str_detect(var, pattern="vaccine_rate_1")~"Vaccine",
        stringr::str_detect(var, pattern="vaccine_rate_2")~"Vaccine",
        stringr::str_detect(var, pattern="vaccine_rate_3")~"Vaccine",
        stringr::str_detect(var, pattern="W_vaccine_1")~"Weighted_Vaccine",
        stringr::str_detect(var, pattern="W_vaccine_2")~"Weighted_Vaccine",
        stringr::str_detect(var, pattern="W_vaccine_3")~"Weighted_Vaccine",
        stringr::str_detect(var, pattern="ihs_temperature_diff")~"Temperature",
        stringr::str_detect(var, pattern="ihs_precipitation")~"Precipitation",
        stringr::str_detect(var, pattern="ln_pop_dens")~"Population density",
        stringr::str_detect(var, pattern="ln_over65")~"Over65",
        stringr::str_detect(var, pattern="Tuesday_dummy")~"Tuesday_dummy",
        stringr::str_detect(var, pattern="Wednesday_dummy")~"Wednesday_dummy",
        stringr::str_detect(var, pattern="Thursday_dummy")~"Thursday_dummy",
        stringr::str_detect(var, pattern="Friday_dummy")~"Friday_dummy",
        stringr::str_detect(var, pattern="holidays_dummy")~"Holidays_dummy",
         TRUE ~ "NA" 
      ) 
  )
  

  #######################
  ### CHANGE HERE !!! ###
  #######################
  for_results <- for_results %>% dplyr::filter (Variables %in% c("Cases", "Weighted_Cases", "Emergency", "Weighted_Emergency", "Vaccine","Weighted_Vaccine",
                                                                 "Temperature", "Precipitation", "Population density", "Over65", 
                                                                 "Tuesday_dummy", "Wednesday_dummy", "Thursday_dummy", "Friday_dummy", "Holidays_dummy"))
  
  
  ## 95%CI
  for_results <- for_results %>% dplyr::mutate_at(vars(Coefficients, Standard_Error), as.numeric) 
  for_results <- for_results %>% dplyr::mutate(CI95_u = Coefficients + Standard_Error*1.96) %>% 
                                  mutate(CI95_l = Coefficients - Standard_Error*1.96) 
  
  
  ## beta hats 
  for_results_beta_CI_1 <- for_results %>% dplyr::filter(Variables == "Cases" & Phase == "Increasing" & Waves == 1 & as.numeric(coefficients.Pr..z.) <= 0.05)  
  for_results_beta_CI_1 %>% dplyr::summarise(max=max(Coefficients) , min=min(Coefficients))
  for_results_beta_CI_2 <- for_results %>% dplyr::filter(Variables == "Cases" & Phase == "Increasing" & Waves == 2 & as.numeric(coefficients.Pr..z.) <= 0.05)  
  for_results_beta_CI_2 %>% dplyr::summarise(max=max(Coefficients) , min=min(Coefficients))
  for_results_beta_CI_3 <- for_results %>% dplyr::filter(Variables == "Cases" & Phase == "Increasing" & Waves == 3 & as.numeric(coefficients.Pr..z.) <= 0.05)  
  for_results_beta_CI_3 %>% dplyr::summarise(max=max(Coefficients) , min=min(Coefficients))
  for_results_beta_CI_4 <- for_results %>% dplyr::filter(Variables == "Cases" & Phase == "Increasing" & Waves == 4 & as.numeric(coefficients.Pr..z.) <= 0.05)  
  for_results_beta_CI_4 %>% dplyr::summarise(max=max(Coefficients) , min=min(Coefficients))
  for_results_beta_CI_5 <- for_results %>% dplyr::filter(Variables == "Cases" & Phase == "Increasing" & Waves == 5 & as.numeric(coefficients.Pr..z.) <= 0.05)  
  for_results_beta_CI_5 %>% dplyr::summarise(max=max(Coefficients) , min=min(Coefficients)) 
  for_results_beta_CI_6 <- for_results %>% dplyr::filter(Variables == "Cases" & Phase == "Increasing" & Waves == 6 & as.numeric(coefficients.Pr..z.) <= 0.05)  
  for_results_beta_CI_6 %>% dplyr::summarise(max=max(Coefficients) , min=min(Coefficients))
  
  ## beta hats 
  for_results_beta_CD_1 <- for_results %>% dplyr::filter(Variables == "Cases" & Phase == "Decreasing" & Waves == 1 & as.numeric(coefficients.Pr..z.) <= 0.05)  
  for_results_beta_CD_1 %>% dplyr::summarise(max=max(Coefficients) , min=min(Coefficients))
  for_results_beta_CD_2 <- for_results %>% dplyr::filter(Variables == "Cases" & Phase == "Decreasing" & Waves == 2 & as.numeric(coefficients.Pr..z.) <= 0.05)  
  for_results_beta_CD_2 %>% dplyr::summarise(max=max(Coefficients) , min=min(Coefficients))
  for_results_beta_CD_3 <- for_results %>% dplyr::filter(Variables == "Cases" & Phase == "Decreasing" & Waves == 3 & as.numeric(coefficients.Pr..z.) <= 0.05)  
  for_results_beta_CD_3 %>% dplyr::summarise(max=max(Coefficients) , min=min(Coefficients))
  for_results_beta_CD_4 <- for_results %>% dplyr::filter(Variables == "Cases" & Phase == "Decreasing" & Waves == 4 & as.numeric(coefficients.Pr..z.) <= 0.05)  
  for_results_beta_CD_4 %>% dplyr::summarise(max=max(Coefficients) , min=min(Coefficients))
  for_results_beta_CD_5 <- for_results %>% dplyr::filter(Variables == "Cases" & Phase == "Decreasing" & Waves == 5 & as.numeric(coefficients.Pr..z.) <= 0.05)  
  for_results_beta_CD_5 %>% dplyr::summarise(max=max(Coefficients) , min=min(Coefficients)) 
  for_results_beta_CD_6 <- for_results %>% dplyr::filter(Variables == "Cases" & Phase == "Decreasing" & Waves == 6 & as.numeric(coefficients.Pr..z.) <= 0.05)  
  for_results_beta_CD_6 %>% dplyr::summarise(max=max(Coefficients) , min=min(Coefficients))
  
  ## beta hats 
  for_results_beta_WC_1 <- for_results %>% dplyr::filter(Variables == "Weighted_Cases" & Waves == 1 & as.numeric(coefficients.Pr..z.) <= 0.05)  
  for_results_beta_WC_1 %>% dplyr::summarise(max=max(Coefficients) , min=min(Coefficients))
  for_results_beta_WC_2 <- for_results %>% dplyr::filter(Variables == "Weighted_Cases" & Waves == 2 & as.numeric(coefficients.Pr..z.) <= 0.05)  
  for_results_beta_WC_2 %>% dplyr::summarise(max=max(Coefficients) , min=min(Coefficients))
  for_results_beta_WC_3 <- for_results %>% dplyr::filter(Variables == "Weighted_Cases" & Waves == 3 & as.numeric(coefficients.Pr..z.) <= 0.05)  
  for_results_beta_WC_3 %>% dplyr::summarise(max=max(Coefficients) , min=min(Coefficients))
  for_results_beta_WC_4 <- for_results %>% dplyr::filter(Variables == "Weighted_Cases" & Waves == 4 & as.numeric(coefficients.Pr..z.) <= 0.05)  
  for_results_beta_WC_4 %>% dplyr::summarise(max=max(Coefficients) , min=min(Coefficients))
  for_results_beta_WC_5 <- for_results %>% dplyr::filter(Variables == "Weighted_Cases" & Waves == 5 & as.numeric(coefficients.Pr..z.) <= 0.05)  
  for_results_beta_WC_5 %>% dplyr::summarise(max=max(Coefficients) , min=min(Coefficients)) 
  for_results_beta_WC_6 <- for_results %>% dplyr::filter(Variables == "Weighted_Cases" & Waves == 6 & as.numeric(coefficients.Pr..z.) <= 0.05)  
  for_results_beta_WC_6 %>% dplyr::summarise(max=max(Coefficients) , min=min(Coefficients))
  
  setwd("***")   # set WD
  write.csv(for_results, "results_retail.csv")     # Or switch to results_residential
    