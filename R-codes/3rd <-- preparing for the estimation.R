
  cat( "\014" )
  rm(list=ls(all.names=TRUE))
  setwd("***")   # set WD
  
  ## Load libraries
  library(tidyverse)
  library(psych)
 
  ## read data
  d_2020 <- read.csv("dd_2020_new.csv") 
  d_2021 <- read.csv("dd_2021_new.csv") 
  d_2022 <- read.csv("dd_2022_new.csv") 
  dd_new <- dplyr::bind_rows(d_2020, d_2021, d_2022)
  dd_new <- dd_new[,-1]
  dd_new <- dd_new %>% dplyr::filter(date >= as.Date("2020-02-22") & date <= as.Date("2022-06-20")) 
  dd <- dd_new %>% dplyr::arrange(pref_no)

  ## set dates
  dd <- dd %>% dplyr::arrange(pref_no)
  start <- "2020-02-22"                  ### google data is from 2020/02/15 so if we take diff7, start date will be 2020/02/22
  end <-  "2022-06-20" 
  
  ## filter 
  dd <- dd %>% dplyr::filter(as.Date(date) >=  as.Date(start) & as.Date(date)<=  as.Date(end))  
 
  ## descriptive statistics
  DESC <- cbind(data.frame(dd$diff7_retail, dd$diff7_residential, 
                           dd$pos_ihs_diff7_case, dd$neg_ihs_diff7_case, dd$W_ihs_diff7_case,
                           dd$emergency, dd$W_emergency, 
                           dd$vaccine_rate_1_diff7, dd$vaccine_rate_2_diff7, dd$vaccine_rate_3_diff7,
                           dd$W_vaccine_1, dd$W_vaccine_2, dd$W_vaccine_3,
                           dd$ihs_temperature_diff, dd$ihs_precipitation, dd$ln_pop_dens, dd$ln_over65,
                           dd$Tuesday_dummy, dd$Wednesday_dummy, dd$Thursday_dummy, dd$Friday_dummy, dd$holidays_dummy))
  des <- data.frame(describe(DESC))
  print(des,digits=3) 
  
 
  setwd("***")   # set WD
  write.csv(dd, "dd_newnew.csv")
  
  