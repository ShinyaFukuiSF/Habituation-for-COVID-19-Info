  
  ###############################
  ### SWITCH R version to 4.0 ###  
  ###############################
  ## because the 'phtt' library works with version 4.0 or earlier.
  
  cat( "\014" )
  rm(list=ls(all.names=TRUE))
  setwd("***")   # set WD
  
  ## Load libraries
  library(tidyverse)
  library(lubridate)
  library(phtt)    # conducting interactive effects model by Bai (2009)
 
  ## read data
  dd_new <- read.csv("dd_newnew.csv") 
  dd_new <- dd_new %>% dplyr::arrange(pref_no)
  
  setwd("***/RES")   # set another WD to acquire the results
  
  #############
  ## set lag ##
  #############
  lagmax <- 7
  
  for(i in 1:lagmax) { 
  k <- i-1
  v <- i+0
    
  dd_new <- dd_new %>% dplyr::group_by(pref_no) %>% mutate (     
    case_ihs_pos_lag = lag(pos_ihs_diff7_case, i),                                       
    case_ihs_neg_lag = lag(neg_ihs_diff7_case, i), 
    case_ihs_lag = lag(ihs_diff7_case, i),  
    W_case_ihs_lag = lag(W_ihs_diff7_case, i),   
    
    wave_1_lag = lag(wave_1, i),
    wave_2_lag = lag(wave_2, i),
    wave_3_lag = lag(wave_3, i),
    wave_4_lag = lag(wave_4, i),
    wave_5_lag = lag(wave_5, i),
    wave_6_lag = lag(wave_6, i),
 
    vaccine_rate_1_diff7_lag = lag(vaccine_rate_1_diff7, v),
    vaccine_rate_2_diff7_lag = lag(vaccine_rate_2_diff7, v),
    vaccine_rate_3_diff7_lag = lag(vaccine_rate_3_diff7, v),
    
    W_vaccine_1_lag = lag(W_vaccine_1, v),
    W_vaccine_2_lag = lag(W_vaccine_2, v),
    W_vaccine_3_lag = lag(W_vaccine_3, v),
   
    ) %>% rowwise %>% mutate_all(~replace(., is.na(.), 0)) 
  
    
  
  ###############
  ## set waves ## 
  ###############
  dd_new <- dd_new %>% dplyr::mutate (case_ihs_pos_lag_1 = wave_1_lag*case_ihs_pos_lag, case_ihs_pos_lag_2 = wave_2_lag*case_ihs_pos_lag, case_ihs_pos_lag_3 = wave_3_lag*case_ihs_pos_lag,
                                      case_ihs_pos_lag_4 = wave_4_lag*case_ihs_pos_lag, case_ihs_pos_lag_5 = wave_5_lag*case_ihs_pos_lag, case_ihs_pos_lag_6 = wave_6_lag*case_ihs_pos_lag,
                                      case_ihs_neg_lag_1 = wave_1_lag*case_ihs_neg_lag, case_ihs_neg_lag_2 = wave_2_lag*case_ihs_neg_lag, case_ihs_neg_lag_3 = wave_3_lag*case_ihs_neg_lag,
                                      case_ihs_neg_lag_4 = wave_4_lag*case_ihs_neg_lag, case_ihs_neg_lag_5 = wave_5_lag*case_ihs_neg_lag, case_ihs_neg_lag_6 = wave_6_lag*case_ihs_neg_lag)
  dd_new <- dd_new %>% dplyr::mutate (case_ihs_lag_1 = wave_1_lag*case_ihs_lag, case_ihs_lag_2 = wave_2_lag*case_ihs_lag, case_ihs_lag_3 = wave_3_lag*case_ihs_lag,
                                      case_ihs_lag_4 = wave_4_lag*case_ihs_lag, case_ihs_lag_5 = wave_5_lag*case_ihs_lag, case_ihs_lag_6 = wave_6_lag*case_ihs_lag)
  dd_new <- dd_new %>% dplyr::mutate (W_case_ihs_lag_1 = wave_1_lag*W_case_ihs_lag, W_case_ihs_lag_2 = wave_2_lag*W_case_ihs_lag, W_case_ihs_lag_3 = wave_3_lag*W_case_ihs_lag,
                                      W_case_ihs_lag_4 = wave_4_lag*W_case_ihs_lag, W_case_ihs_lag_5 = wave_5_lag*W_case_ihs_lag, W_case_ihs_lag_6 = wave_6_lag*W_case_ihs_lag)
  dd_new<- dd_new %>% dplyr::mutate (emergency_1 = wave_1*emergency, emergency_3 = wave_3*emergency,
                                     emergency_4 = wave_4*emergency, emergency_5 = wave_5*emergency)
  dd_new<- dd_new %>% dplyr::mutate (W_emergency_1 = wave_1*W_emergency, W_emergency_3 = wave_3*W_emergency,
                                     W_emergency_4 = wave_4*W_emergency, W_emergency_5 = wave_5*W_emergency)

  dd_new <- dd_new %>% dplyr::arrange(pref_no)
  start <- "2020-02-22"                  # google data is from 2020/02/15 so if we take diff7, start date will be 2020/02/22
  end <-  "2022-06-20" 
  
  ## count days
  x=interval(ymd(start),ymd(end))
  x= x %/% days(1)
  print(x)

  ## filter 
  dd_new <- dd_new %>% dplyr::filter(as.Date(date) >=  as.Date(start)+days(i) & as.Date(date)<=  as.Date(end))  
  

  dd <-  dd_new 
  dd <- dd %>% dplyr::arrange(pref_no)
  
  ## calculate T and N
  T<-mean(tapply(dd$pref_no, dd$pref_no, length))
  N <- nrow(dd)/T
  
  ## set matrix
  ## Y
  diff7_retail <- matrix(dd$diff7_retail, T, N)
  # diff7_transit <- matrix(dd$diff7_transit, T, N)
  # diff7_workplaces <- matrix(dd$diff7_workplaces, T, N)
  diff7_residential <- matrix(dd$diff7_residential, T, N)
  # diff7_grocery <- matrix(dd$diff7_grocery, T, N)
  # diff7_parks <- matrix(dd$diff7_parks, T, N)

　## infected case
  case_ihs_pos_lag_1  <- matrix(dd$case_ihs_pos_lag_1, T, N)
  case_ihs_pos_lag_2  <- matrix(dd$case_ihs_pos_lag_2, T, N)
  case_ihs_pos_lag_3  <- matrix(dd$case_ihs_pos_lag_3, T, N)
  case_ihs_pos_lag_4  <- matrix(dd$case_ihs_pos_lag_4, T, N)
  case_ihs_pos_lag_5  <- matrix(dd$case_ihs_pos_lag_5, T, N)
  case_ihs_pos_lag_6  <- matrix(dd$case_ihs_pos_lag_6, T, N)
  case_ihs_neg_lag_1  <- matrix(dd$case_ihs_neg_lag_1, T, N)
  case_ihs_neg_lag_2  <- matrix(dd$case_ihs_neg_lag_2, T, N)
  case_ihs_neg_lag_3  <- matrix(dd$case_ihs_neg_lag_3, T, N)
  case_ihs_neg_lag_4  <- matrix(dd$case_ihs_neg_lag_4, T, N)
  case_ihs_neg_lag_5  <- matrix(dd$case_ihs_neg_lag_5, T, N)
  case_ihs_neg_lag_6  <- matrix(dd$case_ihs_neg_lag_6, T, N)
  
  W_case_ihs_lag_1  <- matrix(dd$W_case_ihs_lag_1, T, N)
  W_case_ihs_lag_2  <- matrix(dd$W_case_ihs_lag_2, T, N)
  W_case_ihs_lag_3  <- matrix(dd$W_case_ihs_lag_3, T, N)
  W_case_ihs_lag_4  <- matrix(dd$W_case_ihs_lag_4, T, N)
  W_case_ihs_lag_5  <- matrix(dd$W_case_ihs_lag_5, T, N)
  W_case_ihs_lag_6  <- matrix(dd$W_case_ihs_lag_6, T, N) 

  ## emergency
  emergency_1  <- matrix(dd$emergency_1, T, N)
  emergency_3  <- matrix(dd$emergency_3, T, N)
  emergency_4  <- matrix(dd$emergency_4, T, N)
  emergency_5  <- matrix(dd$emergency_5, T, N)

  W_emergency_1 <- matrix(dd$W_emergency_1, T, N)
  W_emergency_3 <- matrix(dd$W_emergency_3, T, N)
  W_emergency_4 <- matrix(dd$W_emergency_4, T, N)
  W_emergency_5 <- matrix(dd$W_emergency_5, T, N)
  
  ## vaccine
  vaccine_rate_1_diff7_lag <- matrix(dd$vaccine_rate_1_diff7_lag, T, N)
  vaccine_rate_2_diff7_lag <- matrix(dd$vaccine_rate_2_diff7_lag, T, N)
  vaccine_rate_3_diff7_lag <- matrix(dd$vaccine_rate_3_diff7_lag, T, N)

  W_vaccine_1_lag <- matrix(dd$ W_vaccine_1_lag, T, N)
  W_vaccine_2_lag <- matrix(dd$ W_vaccine_2_lag, T, N)
  W_vaccine_3_lag <- matrix(dd$ W_vaccine_3_lag, T, N)
  
  ## define IHS
  ihs <- function(x) {            ### Instead of "log", here we apply inverse hyperbolic sine. Because the data takes 0s.
    y <- log(x + sqrt(x^2 + 1))
    return(y)
  }             
 
  ihs_temperature_diff <- matrix(dd$ihs_temperature_diff, T, N)
  ihs_precipitation <- matrix(dd$ihs_precipitation, T, N)	

  ## fixed effects
  ln_pop_dens <- matrix(dd$ln_pop_dens, T, N)
  ln_over65 <- matrix(dd$ln_over65 , T, N)	
 
  ## time fixed effects 
  Tuesday_dummy <- matrix(dd$Tuesday_dummy, T, N)
  Wednesday_dummy <- matrix(dd$Wednesday_dummy, T, N)
  Thursday_dummy <- matrix(dd$Thursday_dummy, T, N)
  Friday_dummy <- matrix(dd$Friday_dummy, T, N)
  holidays_dummy <- matrix(dd$holidays_dummy, T, N)
 
  ## spatial fixed effects
  Hokkaido <- matrix(dd$Hokkaido, T, N)
  Aomori <- matrix(dd$Aomori, T, N)
  Iwate <- matrix(dd$Iwate, T, N)
  Miyagi <- matrix(dd$Miyagi, T, N)
  Akita <- matrix(dd$Akita, T, N)
  Yamagata <- matrix(dd$Yamagata, T, N)
  Fukushima <- matrix(dd$Fukushima, T, N)
  Ibaraki <- matrix(dd$Ibaraki, T, N)
  Tochigi <- matrix(dd$Tochigi, T, N)
  Gunma <- matrix(dd$Gunma, T, N)
  Saitama <- matrix(dd$Saitama, T, N)
  Chiba <- matrix(dd$Chiba, T, N)
  Tokyo <- matrix(dd$Tokyo, T, N)
  Kanagawa <- matrix(dd$Kanagawa, T, N)
  Niigata <- matrix(dd$Niigata, T, N)
  Toyama <- matrix(dd$Toyama, T, N)
  Ishikawa <- matrix(dd$Ishikawa, T, N)
  Fukui <- matrix(dd$Fukui, T, N)
  Yamanashi <- matrix(dd$Yamanashi, T, N)
  Nagano <- matrix(dd$Nagano, T, N)
  Gifu <- matrix(dd$Gifu, T, N)
  Shizuoka <- matrix(dd$Shizuoka, T, N)
  Aichi <- matrix(dd$Aichi, T, N)
  Mie <- matrix(dd$Mie, T, N)
  Shiga <- matrix(dd$Shiga, T, N)
  Kyoto <- matrix(dd$Kyoto, T, N)
  Osaka <- matrix(dd$Osaka, T, N)
  Hyogo <- matrix(dd$Hyogo, T, N)
  Nara <- matrix(dd$Nara, T, N)
  Wakayama <- matrix(dd$Wakayama, T, N)
  Tottori <- matrix(dd$Tottori, T, N)
  Shimane <- matrix(dd$Shimane, T, N)
  Okayama <- matrix(dd$Okayama, T, N)
  Hiroshima <- matrix(dd$Hiroshima, T, N)
  Yamaguchi <- matrix(dd$Yamaguchi, T, N)
  Tokushima <- matrix(dd$Tokushima, T, N)
  Kagawa <- matrix(dd$Kagawa, T, N)
  Ehime <- matrix(dd$Ehime, T, N)
  Kochi <- matrix(dd$Kochi, T, N)
  Fukuoka <- matrix(dd$Fukuoka, T, N)
  Saga <- matrix(dd$Saga, T, N)
  Nagasaki <- matrix(dd$Nagasaki, T, N)
  Kumamoto <- matrix(dd$Kumamoto, T, N)
  Oita <- matrix(dd$Oita, T, N)
  Miyazaki <- matrix(dd$Miyazaki, T, N)
  Kagoshima <- matrix(dd$Kagoshima, T, N)
  Okinawa <- matrix(dd$Okinawa, T, N)
  
  ##################
  ### ESTIMATION ###
  ##################
  
  ## run model  
  Y <- diff7_retail  ## Or switch to diff7_residential,
                    ## Note that the extension model needs more other explanatory variables.
  
  fm1 <- Y ~   
    case_ihs_pos_lag_1 + case_ihs_pos_lag_2 + case_ihs_pos_lag_3 +
    case_ihs_pos_lag_4 + case_ihs_pos_lag_5 + case_ihs_pos_lag_6 +
    case_ihs_neg_lag_1 + case_ihs_neg_lag_2 + case_ihs_neg_lag_3 + 
    case_ihs_neg_lag_4 + case_ihs_neg_lag_5 + case_ihs_neg_lag_6 +
    W_case_ihs_lag_1 + W_case_ihs_lag_2 + W_case_ihs_lag_3 + 
    W_case_ihs_lag_4 + W_case_ihs_lag_5 + W_case_ihs_lag_6 +

    emergency_1 + emergency_3 + emergency_4 + emergency_5 + 
    W_emergency_1 + W_emergency_3 + W_emergency_4 + W_emergency_5 +
    
    vaccine_rate_1_diff7_lag + 
    vaccine_rate_2_diff7_lag + vaccine_rate_3_diff7_lag  + 
    W_vaccine_1_lag + W_vaccine_2_lag + W_vaccine_3_lag +
  
    ihs_temperature_diff + ihs_precipitation +  ln_pop_dens + ln_over65  +
    Tuesday_dummy + Wednesday_dummy + Thursday_dummy + Friday_dummy +
    holidays_dummy +

    Hokkaido	+	Aomori	+	Iwate	+	Miyagi	+	Akita	+	Yamagata	+	Fukushima	+	Ibaraki	+	Tochigi	+	Gunma	+	Saitama	+	Chiba	+	Tokyo	+	Kanagawa	+	Niigata	+	Toyama	+	Ishikawa	+	Fukui	+	Yamanashi	+	Nagano	+	Gifu	+	Shizuoka	+	Aichi	+	Mie	+	Shiga	+	Kyoto	+	Osaka	+	Hyogo	+	Nara	+	Wakayama	+	Tottori	+	Shimane	+	Okayama	+	Hiroshima	+	Yamaguchi	+	Tokushima	+	Kagawa	+	Ehime	+	Kochi	+	Fukuoka	+	Saga	+	Nagasaki	+	Kumamoto	+	Oita	+	Miyazaki	+	Kagoshima	+	Okinawa
  
  ## ESTIMATION
  res <- Eup (formula = fm1, additive.effects = c("none"), dim.criterion="IC1", max.iteration = 500, convergence = 1e-06) # additive.effects = c("twoways"),  c("none")
  
  res_out <- summary(res, error.type=8)
  RES <- res_out[2]
  nrowdd <- nrow(dd)
  nrow <- c(nrow,list(nrowdd))
  dim <-res[14] 
  dim2 <-res[16] 
  checkSp <- checkSpecif(res, level = 0.01)
  check <- checkSp[2]
  resid <- res$residuals
 
  #OptDim.obj <- OptDim(Obj = retail_mgm7, criteria = c("PC1", "PC2", "PC3", "BIC3", "IC1",
  #                                                     "IC2", "IC3", "IPC1", "IPC2", "IPC3", "ABC.IC1", "ABC.IC2",
  #                                                     "KSS.C", "ED", "ER", "GR"), standardize = TRUE)
  
  RES <- RES
  write.csv(RES, "RES_index.csv")
 
  ## get output
  RES2 <- read.csv("RES_index.csv")
  
  file.remove("RES_index.csv")
  
  RES2 <- t(RES2)
  
  colnames(RES2) <- RES2[1,]
  RES2 <- RES2[-1,]

  RES2_phtt <- RES2
  n <- length(res$slope.para)
  lag <- as.vector(rep(c(i), each=n+1))
  lag_t <- t(lag)
  RES2_phtt <- rbind(RES2_phtt,lag)

    write.csv(RES2_phtt, paste("RES2_phtt_", i, ".csv", sep=""))
    write.csv(dim, paste("dim_", i, ".csv", sep=""))
    write.csv(dim2, paste("dim2_", i, ".csv", sep=""))
    write.csv(check,paste("check_", i, ".csv", sep=""))
    
  }  
 
 