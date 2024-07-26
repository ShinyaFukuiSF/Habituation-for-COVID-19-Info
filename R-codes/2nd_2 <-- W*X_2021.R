
cat( "\014" )
rm(list=ls(all.names=TRUE))
setwd("***/weights_2021")   # set WD

## Load libraries
library(tidytable)
library(tidyverse)

## read data
dd_2021 <- read.csv("dd_2021.csv") 

# start<- "2020-02-21"
# end <- "2021-01-03"
start<- "2021-01-04"
end <- "2022-01-02"
#start<- "2022-01-03"
#end <- "2022-06-20"
int <- as.numeric(as.Date(end)-as.Date(start))+1


WXA <- list()
for(i in 1:int) {              
  dd_2021_for <- dd_2021 %>% dplyr::filter(date ==  ymd(start)+days(i)-1)
  week_x <- lubridate::isoweek(ymd(start)+days(i)-1)
  weight_file <- paste("weight_index", "_", week_x , ".csv", sep="")
  dwa <- read.csv(weight_file, row.names=1)       
  dwa1 <- as.matrix(dwa)
  XA <- as.matrix(dd_2021_for$ihs_diff7_case)                                 # X = ihs_diff7_case    
  WXA2 <- dwa1%*%XA
  WXA<-c(WXA,list(WXA2))
}


WXD <- list()
for(l in 1:int) {              
  dd_2021_for <- dd_2021 %>% dplyr::filter(date ==  ymd(start)+days(l)-1)
  week_x <- lubridate::isoweek(ymd(start)+days(l)-1)
  weight_file <- paste("weight_index", "_", week_x , ".csv", sep="")
  dwd <- read.csv(weight_file, row.names=1)       
  dwd1 <- as.matrix(dwd)
  XD <- as.matrix(dd_2021_for$emergency)                                 # X = emergency         
  WXD2 <- dwd1%*%XD
  WXD<-c(WXD,list(WXD2))
}



WXH <- list()
for(p in 1:int) {              
  dd_2021_for <- dd_2021 %>% dplyr::filter(date ==  ymd(start)+days(p)-1)
  week_x <- lubridate::isoweek(ymd(start)+days(p)-1)
  weight_file <- paste("weight_index", "_", week_x , ".csv", sep="")
  dwh <- read.csv(weight_file, row.names=1)       
  dwh1 <- as.matrix(dwh)
  XH <- as.matrix(dd_2021_for$vaccine_rate_1_diff7)                                 # X =	vaccine_rate_1_diff7            
  WXH2 <- dwh1%*%XH
  WXH<-c(WXH,list(WXH2))
}

WXI <- list()
for(q in 1:int) {              
  dd_2021_for <- dd_2021 %>% dplyr::filter(date ==  ymd(start)+days(q)-1)
  week_x <- lubridate::isoweek(ymd(start)+days(q)-1)
  weight_file <- paste("weight_index", "_", week_x , ".csv", sep="")
  dwi <- read.csv(weight_file, row.names=1)       
  dwi1 <- as.matrix(dwi)
  XI <- as.matrix(dd_2021_for$vaccine_rate_2_diff7)                                 # X =	vaccine_rate_2_diff7            
  WXI2 <- dwi1%*%XI
  WXI<-c(WXI,list(WXI2))
}

WXJ <- list()
for(r in 1:int) {              
  dd_2021_for <- dd_2021 %>% dplyr::filter(date ==  ymd(start)+days(r)-1)
  week_x <- lubridate::isoweek(ymd(start)+days(r)-1)
  weight_file <- paste("weight_index", "_", week_x , ".csv", sep="")
  dwj <- read.csv(weight_file, row.names=1)       
  dwj1 <- as.matrix(dwj)
  XJ <- as.matrix(dd_2021_for$vaccine_rate_3_diff7)                                 # X =	vaccine_rate_3_diff7            
  WXJ2 <- dwj1%*%XJ
  WXJ<-c(WXJ,list(WXJ2))
}


for(z in 1:int ) {
  # load and merge each xts block
  week_x <- lubridate::isoweek(ymd(start)+days(z)-1)
  weight_file <- paste("weight_index", "_", week_x , ".csv", sep="")
  dwz <- read.csv(weight_file, row.names=1)   
  if( z == 1 )
    WXZ = dwz
  else
    WXZ = rbind(WXZ, dwz)                                         # These are for spatial fixed effects
}

WXA <- as.matrix(unlist(WXA))
WXD <- as.matrix(unlist(WXD))
WXH <- as.matrix(unlist(WXH))
WXI <- as.matrix(unlist(WXI))
WXJ <- as.matrix(unlist(WXJ))

dd_2021_new <- dd_2021 %>% dplyr::mutate(W_ihs_diff7_case = WXA) %>% mutate(W_emergency = WXD) %>% 
                                  mutate(W_vaccine_1 = WXH) %>% mutate(W_vaccine_2 = WXI) %>% 
                                  mutate(W_vaccine_3 = WXJ)  
dd_2021_new <- dd_2021_new[,-1]
rownames(WXZ) <- NULL

dd_2021_new <- cbind(dd_2021_new,WXZ)
setwd("***")   # set WD
write.csv(dd_2021_new, "dd_2021_new.csv")



