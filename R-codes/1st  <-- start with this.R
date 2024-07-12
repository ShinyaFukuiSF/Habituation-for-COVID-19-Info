###########################################################################################################
### There are 5 steps for the estimation.
### 1. 1st  <--- start with this (R version is 4.1) (read and create data)
### 2. 2nd <-- vaccine & severe_cases & controls (add vaccine, controls, and fixed effects)
### 3. 3rd_1,2,3  <-- W*X_2020, W*X_2021, and W*X_2022 (to make W*X)
###      Weights are calculated in advance.        
### 4. 4th <-- before estimation (Combining and organising data)
### 5. 5th <-- estimation using phtt  (change R version to 4.0)  (estimation)
###########################################################################################################

cat( "\014" )
rm(list=ls(all.names=TRUE))
setwd("***") # set WD

# Load libraries (Some libraries may not be used)
library(spdep)
library(splm)
library(plm)
library(tidyr)
library(dplyr)
library(tidyverse)
library(stringr)
library(lubridate)
library(psych)
library(mFilter)
library(incidence)

###################
### google data ###
###################
google <- read.csv("google_20220816.csv") 　　                      # read Google human mobility data
google <- google %>% dplyr::mutate_at(vars(-Pref, -pref_code, -iso_3166_2_code,	-pref_no, -date), as.numeric) 

###########################
### make holidays dummy ###
###########################
## Pulling source files to create Japanese holidays.
source("https://raw.githubusercontent.com/logics-of-blue/website/master/010_forecast/20190714_R%E8%A8%80%E8%AA%9E%E3%81%AB%E3%81%8A%E3%81%91%E3%82%8B%E6%97%A5%E6%9C%AC%E3%81%AE%E7%A5%9D%E6%97%A5%E5%88%A4%E5%AE%9A/jholiday.R", encoding="utf-8")
## Source files are available here.
## https://logics-of-blue.com/determination-of-japanese-national-holidays-with-r/
google <- google %>% dplyr::mutate_at(vars(date), as.Date) 
google <- google %>% dplyr::mutate(
  holidays_dummy = 
    case_when( 
      lubridate::wday(date)== 1 ~ "1",            # Sunday
      lubridate::wday(date)== 7 ~ "1",            # Saturday
      is.jholiday(target_date = date)== TRUE ~ "1", 　　# Set Japanese holidays
      ## Set lantan festival (O-bon) and New Year’s holiday
      date ==  ymd("2020-08-13") ~ "1",
      date ==  ymd("2020-08-14") ~ "1", 
      date ==  ymd("2020-08-15") ~ "1", 
      date ==  ymd("2020-08-16") ~ "1", 
      date ==  ymd("2020-12-29") ~ "1",
      date ==  ymd("2020-12-30") ~ "1", 
      date ==  ymd("2020-12-31") ~ "1", 
      date ==  ymd("2021-01-01") ~ "1", 
      date ==  ymd("2021-01-02") ~ "1", 
      date ==  ymd("2021-01-03") ~ "1", 
      date ==  ymd("2021-08-13") ~ "1",
      date ==  ymd("2021-08-14") ~ "1", 
      date ==  ymd("2021-08-15") ~ "1", 
      date ==  ymd("2021-08-16") ~ "1", 
      date ==  ymd("2021-12-29") ~ "1",
      date ==  ymd("2021-12-30") ~ "1", 
      date ==  ymd("2021-12-31") ~ "1", 
      date ==  ymd("2022-01-01") ~ "1", 
      date ==  ymd("2022-01-02") ~ "1", 
      date ==  ymd("2022-01-03") ~ "1", 
      TRUE ~ "0" 
    ) 
)

###########################
### make weekdays dummy ###
###########################
google <- google %>% dplyr::mutate_at(vars(date), as.Date) 
google <- google %>% dplyr::mutate(Tuesday_dummy = if_else(holidays_dummy == 1, 0, if_else(lubridate::wday(date) == 3, 1, 0)))
google <- google %>% dplyr::mutate_at(vars(date), as.Date) 
google <- google %>% dplyr::mutate(Wednesday_dummy = if_else(holidays_dummy == 1, 0, if_else(lubridate::wday(date) == 4, 1, 0)))
google <- google %>% dplyr::mutate_at(vars(date), as.Date) 
google <- google %>% dplyr::mutate(Thursday_dummy = if_else(holidays_dummy == 1, 0, if_else(lubridate::wday(date) == 5, 1, 0)))
google <- google %>% dplyr::mutate_at(vars(date), as.Date) 
google <- google %>% dplyr::mutate(Friday_dummy = if_else(holidays_dummy == 1, 0, if_else(lubridate::wday(date) == 6, 1, 0)))

### as.numeric
google <- google %>% dplyr::mutate_at(vars(-Pref, -pref_code, -iso_3166_2_code,	-pref_no, -date), as.numeric) 
### moving average of week_dummy
google <- google %>% dplyr::group_by(pref_no) %>% mutate(
  lag1_holidays = lag(holidays_dummy, n=1),
  lag2_holidays = lag(holidays_dummy, n=2),
  lag3_holidays = lag(holidays_dummy, n=3),
  lag4_holidays = lag(holidays_dummy, n=4),
  lag5_holidays = lag(holidays_dummy, n=5),
  lag6_holidays = lag(holidays_dummy, n=6)
)  %>% rowwise %>% mutate(holidays_ma7 = mean(c(holidays_dummy, lag1_holidays, lag2_holidays, lag3_holidays, lag4_holidays, lag5_holidays, lag6_holidays)))
# retail
### moving average (geometric mean) of retail ###
google <- google %>% dplyr::group_by(pref_no) %>% mutate(retail_100 = retail+100)
google <- google %>% dplyr::group_by(pref_no) %>% mutate(
  lag1_re = lag(retail_100, n=1),
  lag2_re = lag(retail_100, n=2),
  lag3_re = lag(retail_100, n=3),
  lag4_re = lag(retail_100, n=4),
  lag5_re = lag(retail_100, n=5),
  lag6_re = lag(retail_100, n=6)
)  %>% rowwise %>% mutate(retail_mgm7 = (geometric.mean(c(retail_100, lag1_re, lag2_re, lag3_re, lag4_re, lag5_re, lag6_re))-100))
### difference in rate of change (percentage point)
google <- google %>% dplyr::group_by(pref_no) %>% mutate(diff_retail_mgm7 = (retail_mgm7 - lag(retail_mgm7, n=1))) %>% rowwise
### difference in rate of change (percentage point, 7days)
google <- google %>% dplyr::group_by(pref_no) %>% mutate(diff7_retail_mgm7 = (retail_mgm7 - lag(retail_mgm7, n=7))) %>% rowwise
### difference from the previous week (no mgm-7)
google <- google %>% dplyr::group_by(pref_no) %>% mutate(diff7_retail = (retail - lag(retail, n=7))) %>% rowwise
# transit
### moving average (geometric mean) of transit ###
google <- google %>% dplyr::group_by(pref_no) %>% mutate(transit_100 = transit+100)
google <- google %>% dplyr::group_by(pref_no) %>% mutate(
  lag1_tr = lag(transit_100, n=1),
  lag2_tr = lag(transit_100, n=2),
  lag3_tr = lag(transit_100, n=3),
  lag4_tr = lag(transit_100, n=4),
  lag5_tr = lag(transit_100, n=5),
  lag6_tr = lag(transit_100, n=6)
)  %>% rowwise %>% mutate(transit_mgm7 = (geometric.mean(c(transit_100, lag1_tr, lag2_tr, lag3_tr, lag4_tr, lag5_tr, lag6_tr))-100))
### difference in rate of change (percentage point)
google <- google %>% dplyr::group_by(pref_no) %>% mutate(diff_transit_mgm7 = (transit_mgm7 - lag(transit_mgm7, n=1))) %>% rowwise
### difference in rate of change (percentage point, 7days)
google <- google %>% dplyr::group_by(pref_no) %>% mutate(diff7_transit_mgm7 = (transit_mgm7 - lag(transit_mgm7, n=7))) %>% rowwise
### difference from the previous week (no mgm-7)
google <- google %>% dplyr::group_by(pref_no) %>% mutate(diff7_transit = (transit - lag(transit, n=7))) %>% rowwise
# workplaces
### moving average (geometric mean) of workplaces ###
google <- google %>% dplyr::group_by(pref_no) %>% mutate(workplaces_100 = workplaces+100)
google <- google %>% dplyr::group_by(pref_no) %>% mutate(
  lag1_wk = lag(workplaces_100, n=1),
  lag2_wk = lag(workplaces_100, n=2),
  lag3_wk = lag(workplaces_100, n=3),
  lag4_wk = lag(workplaces_100, n=4),
  lag5_wk = lag(workplaces_100, n=5),
  lag6_wk = lag(workplaces_100, n=6)
)  %>% rowwise %>% mutate(workplaces_mgm7 = (geometric.mean(c(workplaces_100, lag1_wk, lag2_wk, lag3_wk, lag4_wk, lag5_wk, lag6_wk))-100))
### difference in rate of change (percentage point)
google <- google %>% dplyr::group_by(pref_no) %>% mutate(diff_workplaces_mgm7 = (workplaces_mgm7 - lag(workplaces_mgm7, n=1))) %>% rowwise
### difference in rate of change (percentage point, 7days)
google <- google %>% dplyr::group_by(pref_no) %>% mutate(diff7_workplaces_mgm7 = (workplaces_mgm7 - lag(workplaces_mgm7, n=7))) %>% rowwise
### difference from the previous week (no mgm-7)
google <- google %>% dplyr::group_by(pref_no) %>% mutate(diff7_workplaces = (workplaces - lag(workplaces, n=7))) %>% rowwise
# grocery
### moving average (geometric mean) of grocery ###
google <- google %>% dplyr::group_by(pref_no) %>% mutate(grocery_100 = grocery+100)
google <- google %>% dplyr::group_by(pref_no) %>% mutate(
  lag1_gr = lag(grocery_100, n=1),
  lag2_gr = lag(grocery_100, n=2),
  lag3_gr = lag(grocery_100, n=3),
  lag4_gr = lag(grocery_100, n=4),
  lag5_gr = lag(grocery_100, n=5),
  lag6_gr = lag(grocery_100, n=6)
)  %>% rowwise %>% mutate(grocery_mgm7 = (geometric.mean(c(grocery_100, lag1_gr, lag2_gr, lag3_gr, lag4_gr, lag5_gr, lag6_gr))-100))
### difference in rate of change (percentage point)
google <- google %>% dplyr::group_by(pref_no) %>% mutate(diff_grocery_mgm7 = (grocery_mgm7 - lag(grocery_mgm7, n=1))) %>% rowwise
### difference in rate of change (percentage point, 7days)
google <- google %>% dplyr::group_by(pref_no) %>% mutate(diff7_grocery_mgm7 = (grocery_mgm7 - lag(grocery_mgm7, n=7))) %>% rowwise
### difference from the previous week (no mgm-7)
google <- google %>% dplyr::group_by(pref_no) %>% mutate(diff7_grocery = (grocery - lag(grocery, n=7))) %>% rowwise
# parks	
### moving average (geometric mean) of parks ###
google <- google %>% dplyr::group_by(pref_no) %>% mutate(parks_100 = parks+100)
google <- google %>% dplyr::group_by(pref_no) %>% mutate(
  lag1_pa = lag(parks_100, n=1),
  lag2_pa = lag(parks_100, n=2),
  lag3_pa = lag(parks_100, n=3),
  lag4_pa = lag(parks_100, n=4),
  lag5_pa = lag(parks_100, n=5),
  lag6_pa = lag(parks_100, n=6)
)  %>% rowwise %>% mutate(parks_mgm7 = (geometric.mean(c(parks_100, lag1_pa, lag2_pa, lag3_pa, lag4_pa, lag5_pa, lag6_pa))-100))
### difference in rate of change (percentage point)
google <- google %>% dplyr::group_by(pref_no) %>% mutate(diff_parks_mgm7 = (parks_mgm7 - lag(parks_mgm7, n=1))) %>% rowwise
### difference in rate of change (percentage point, 7days)
google <- google %>% dplyr::group_by(pref_no) %>% mutate(diff7_parks_mgm7 = (parks_mgm7 - lag(parks_mgm7, n=7))) %>% rowwise
### difference from the previous week (no mgm-7)
google <- google %>% dplyr::group_by(pref_no) %>% mutate(diff7_parks = (parks - lag(parks, n=7))) %>% rowwise
# residential
### moving average (geometric mean) of residential ###
google <- google %>% dplyr::group_by(pref_no) %>% mutate(residential_100 = residential+100)
google <- google %>% dplyr::group_by(pref_no) %>% mutate(
  lag1_res = lag(residential_100, n=1),
  lag2_res = lag(residential_100, n=2),
  lag3_res = lag(residential_100, n=3),
  lag4_res = lag(residential_100, n=4),
  lag5_res = lag(residential_100, n=5),
  lag6_res = lag(residential_100, n=6)
)  %>% rowwise %>% mutate(residential_mgm7 = (geometric.mean(c(residential_100, lag1_res, lag2_res, lag3_res, lag4_res, lag5_res, lag6_res))-100))
### difference in rate of change (percentage point)
google <- google %>% dplyr::group_by(pref_no) %>% mutate(diff_residential_mgm7 = (residential_mgm7 - lag(residential_mgm7, n=1))) %>% rowwise
### difference in rate of change (percentage point, 7days)
google <- google %>% dplyr::group_by(pref_no) %>% mutate(diff7_residential_mgm7 = (residential_mgm7 - lag(residential_mgm7, n=7))) %>% rowwise
### difference from the previous week (no mgm-7)
google <- google %>% dplyr::group_by(pref_no) %>% mutate(diff7_residential = (residential - lag(residential, n=7))) %>% rowwise

##########################
### infected case data ###
##########################
case <- read.csv("cases_20220816.csv", fileEncoding = "cp932")        # read infection case data 
case <- case %>% dplyr::mutate_all( ~gsub(.,pattern="北海道",replacement = "Hokkaido")) %>% 
  mutate_all( ~gsub(.,pattern="青森県",replacement = "Aomori")) %>% 
  mutate_all( ~gsub(.,pattern="岩手県",replacement = "Iwate")) %>%                                
  mutate_all( ~gsub(.,pattern="宮城県",replacement = "Miyagi")) %>%                                
  mutate_all( ~gsub(.,pattern="秋田県",replacement = "Akita")) %>%                                 
  mutate_all( ~gsub(.,pattern="山形県",replacement = "Yamagata")) %>% 
  mutate_all( ~gsub(.,pattern="福島県",replacement = "Fukushima")) %>% 
  mutate_all( ~gsub(.,pattern="茨城県",replacement = "Ibaraki")) %>%                                
  mutate_all( ~gsub(.,pattern="栃木県",replacement = "Tochigi")) %>%  
  mutate_all( ~gsub(.,pattern="群馬県",replacement = "Gunma")) %>%            
  mutate_all( ~gsub(.,pattern="埼玉県",replacement = "Saitama")) %>%            
  mutate_all( ~gsub(.,pattern="千葉県",replacement = "Chiba")) %>%            
  mutate_all( ~gsub(.,pattern="東京都",replacement = "Tokyo")) %>%            
  mutate_all( ~gsub(.,pattern="神奈川県",replacement = "Kanagawa")) %>%            
  mutate_all( ~gsub(.,pattern="新潟県",replacement = "Niigata")) %>%            
  mutate_all( ~gsub(.,pattern="富山県",replacement = "Toyama")) %>%            
  mutate_all( ~gsub(.,pattern="石川県",replacement = "Ishikawa")) %>%            
  mutate_all( ~gsub(.,pattern="福井県",replacement = "Fukui")) %>%            
  mutate_all( ~gsub(.,pattern="山梨県",replacement = "Yamanashi")) %>%            
  mutate_all( ~gsub(.,pattern="長野県",replacement = "Nagano")) %>%            
  mutate_all( ~gsub(.,pattern="岐阜県",replacement = "Gifu")) %>%            
  mutate_all( ~gsub(.,pattern="静岡県",replacement = "Shizuoka")) %>%            
  mutate_all( ~gsub(.,pattern="愛知県",replacement = "Aichi")) %>%            
  mutate_all( ~gsub(.,pattern="三重県",replacement = "Mie")) %>%            
  mutate_all( ~gsub(.,pattern="滋賀県",replacement = "Shiga")) %>%            
  mutate_all( ~gsub(.,pattern="京都府",replacement = "Kyoto")) %>%            
  mutate_all( ~gsub(.,pattern="大阪府",replacement = "Osaka")) %>%            
  mutate_all( ~gsub(.,pattern="兵庫県",replacement = "Hyogo")) %>%   
  mutate_all( ~gsub(.,pattern="奈良県",replacement = "Nara")) %>%   
  mutate_all( ~gsub(.,pattern="和歌山県",replacement = "Wakayama")) %>%   
  mutate_all( ~gsub(.,pattern="鳥取県",replacement = "Tottori")) %>%   
  mutate_all( ~gsub(.,pattern="島根県",replacement = "Shimane")) %>%   
  mutate_all( ~gsub(.,pattern="岡山県",replacement = "Okayama")) %>%   
  mutate_all( ~gsub(.,pattern="広島県",replacement = "Hiroshima")) %>%   
  mutate_all( ~gsub(.,pattern="山口県",replacement = "Yamaguchi")) %>%   
  mutate_all( ~gsub(.,pattern="徳島県",replacement = "Tokushima")) %>%   
  mutate_all( ~gsub(.,pattern="香川県",replacement = "Kagawa")) %>%   
  mutate_all( ~gsub(.,pattern="愛媛県",replacement = "Ehime")) %>%   
  mutate_all( ~gsub(.,pattern="高知県",replacement = "Kochi")) %>%   
  mutate_all( ~gsub(.,pattern="福岡県",replacement = "Fukuoka")) %>%   
  mutate_all( ~gsub(.,pattern="佐賀県",replacement = "Saga")) %>%   
  mutate_all( ~gsub(.,pattern="長崎県",replacement = "Nagasaki")) %>%   
  mutate_all( ~gsub(.,pattern="熊本県",replacement = "Kumamoto")) %>%   
  mutate_all( ~gsub(.,pattern="大分県",replacement = "Oita")) %>%   
  mutate_all( ~gsub(.,pattern="宮崎県",replacement = "Miyazaki")) %>%   
  mutate_all( ~gsub(.,pattern="鹿児島県",replacement = "Kagoshima")) %>%   
  mutate_all( ~gsub(.,pattern="沖縄県",replacement = "Okinawa"))  

### as. numeric, as.Date
case <- case %>% dplyr::mutate_at(vars(-date_case, -pref), as.numeric) 
case <- case %>% dplyr::mutate_at(vars(date_case), as.Date) 
### case per 1000 population 
jinko <- read.csv("jinko.csv")  #, colClasses=c("character","numeric"))       # population data for prefectures
jinko <- jinko %>% dplyr::mutate(jinko_2020_mil = jinko_2020/1000)    # population is to be 1 million unit.            
case <- case %>% dplyr::left_join(jinko,by = "pref") %>% mutate(newcases_per = newcases/jinko_2020_mil) 
case <- case %>% dplyr::mutate(death_per = death/jinko_2020_mil)        # new cases per capita

# arrange
case <- case %>% dplyr::arrange(pref_no)
### moving average of newcases
case <- case %>% dplyr::group_by(pref_no) %>%  mutate(
  lag1_ca = lag(newcases),
  lag2_ca = lag(newcases, 2),
  lag3_ca = lag(newcases, 3),
  lag4_ca = lag(newcases, 4),
  lag5_ca = lag(newcases, 5),
  lag6_ca = lag(newcases, 6)
) %>% rowwise %>% dplyr::mutate(case_ma7 = mean(c(newcases, lag1_ca, lag2_ca, lag3_ca, lag4_ca, lag5_ca, lag6_ca)))
### moving average of newcases_per
case <- case %>% dplyr::group_by(pref_no) %>% mutate(
  lag1_ca_per = lag(newcases_per , n=1),
  lag2_ca_per = lag(newcases_per , n=2),
  lag3_ca_per = lag(newcases_per , n=3),
  lag4_ca_per = lag(newcases_per , n=4),
  lag5_ca_per = lag(newcases_per , n=5),
  lag6_ca_per = lag(newcases_per , n=6)
) %>% rowwise  %>% dplyr::mutate(case_per_ma7 = mean(c(newcases_per, lag1_ca_per, lag2_ca_per, lag3_ca_per, lag4_ca_per, lag5_ca_per, lag6_ca_per)))
### moving average of death
case <- case %>% dplyr::group_by(pref_no) %>%  mutate(
  lag1_de = lag(death),
  lag2_de = lag(death, 2),
  lag3_de = lag(death, 3),
  lag4_de = lag(death, 4),
  lag5_de = lag(death, 5),
  lag6_de = lag(death, 6)
) %>% rowwise %>% dplyr::mutate(death_ma7 = mean(c(death, lag1_de, lag2_de, lag3_de, lag4_de, lag5_de, lag6_de)))
### moving average of death_per
case <- case %>% dplyr::group_by(pref_no) %>% mutate(
  lag1_de_per = lag(death_per , n=1),
  lag2_de_per = lag(death_per , n=2),
  lag3_de_per = lag(death_per , n=3),
  lag4_de_per = lag(death_per , n=4),
  lag5_de_per = lag(death_per , n=5),
  lag6_de_per = lag(death_per , n=6)
) %>% rowwise  %>% dplyr::mutate(death_per_ma7 = mean(c(death_per, lag1_de_per, lag2_de_per, lag3_de_per, lag4_de_per, lag5_de_per, lag6_de_per)))

### growth rate

 ihs <- function(x) {                           ### Instead of "log", here I apply inverse hyperbolic sine. Because the data takes 0s.
    y <- log(x + sqrt(x^2 + 1))
    return(y)
  }             

# converting with ihs 
case <- case %>% dplyr::mutate(ihs_case_per_ma7 = ihs(case_per_ma7)) 
case <- case %>% dplyr::mutate(ihs_case_ma7 = ihs(case_ma7))
case <- case %>% dplyr::mutate(ihs_case_per = ihs(newcases_per))
case <- case %>% dplyr::mutate(ihs_case = ihs(newcases)) 

# take a difference to approximate the growth rate
case <- case %>% dplyr::group_by(pref_no) %>% mutate(ihs_diff_case_per_ma7 = (ihs_case_per_ma7 - lag(ihs_case_per_ma7, n=1))) %>% rowwise
case <- case %>% dplyr::group_by(pref_no) %>% mutate(ihs_diff_case_ma7 = (ihs_case_ma7 - lag(ihs_case_ma7, n=1))) %>% rowwise
# take a difference from the previous week (ma7)
case <- case %>% dplyr::group_by(pref_no) %>% mutate(ihs_diff7_case_per_ma7 = (ihs_case_per_ma7 - lag(ihs_case_per_ma7, n=7))) %>% rowwise
case <- case %>% dplyr::group_by(pref_no) %>% mutate(ihs_diff7_case_ma7 = (ihs_case_ma7 - lag(ihs_case_ma7, n=7))) %>% rowwise
# take a difference from the previous week (daily)
case <- case %>% dplyr::group_by(pref_no) %>% mutate(ihs_diff7_case_per = (ihs_case_per - lag(ihs_case_per, n=7))) %>% rowwise
case <- case %>% dplyr::group_by(pref_no) %>% mutate(ihs_diff7_case = (ihs_case - lag(ihs_case, n=7))) %>% rowwise

#make a DUMMY
case <- case %>% dplyr::mutate(dummy = rep(1,length(nrow(case))))


##############################
### the state of emergency ###
##############################
emergency <- read.csv("emergency.csv")
emergency <- emergency %>% tidyr::pivot_longer(cols = c (Hokkaido:Okinawa), names_to = "pref", values_to = "emergency")  
emergency <- emergency %>% dplyr::mutate(
       pref_no = 
       case_when( 
       pref ==  "Hokkaido"~ "1",
       pref ==  "Aomori"~ "2",
       pref ==  "Iwate"~ "3",
       pref ==  "Miyagi"~ "4",
       pref ==  "Akita"~ "5",
       pref ==  "Yamagata"~ "6",
       pref ==  "Fukushima"~ "7",
       pref ==  "Ibaraki"~ "8",
       pref ==  "Tochigi"~ "9",
       pref ==  "Gunma"~ "10",
       pref ==  "Saitama"~ "11",
       pref ==  "Chiba"~ "12",
       pref ==  "Tokyo"~ "13",
       pref ==  "Kanagawa"~ "14",
       pref ==  "Niigata"~ "15",
       pref ==  "Toyama"~ "16",
       pref ==  "Ishikawa"~ "17",
       pref ==  "Fukui"~ "18",
       pref ==  "Yamanashi"~ "19",
       pref == "Nagano" ~ "20",
       pref ==  "Gifu"~ "21",
       pref ==  "Shizuoka"~ "22",
       pref ==  "Aichi"~ "23",
       pref ==  "Mie"~ "24",
       pref ==  "Shiga"~ "25",
       pref ==  "Kyoto"~ "26",
       pref ==  "Osaka"~ "27",
       pref ==  "Hyogo"~ "28",
       pref ==  "Nara"~ "29",
       pref ==  "Wakayama"~ "30",
       pref ==  "Tottori"~ "31",
       pref ==  "Shimane"~ "32",
       pref ==  "Okayama"~ "33",
       pref ==  "Hiroshima"~ "34",
       pref ==  "Yamaguchi"~ "35",
       pref ==  "Tokushima"~ "36",
       pref ==  "Kagawa"~ "37",
       pref == "Ehime"~ "38",
       pref ==  "Kochi"~ "39",
       pref ==  "Fukuoka"~ "40",
       pref ==  "Saga"~ "41",
       pref ==  "Nagasaki"~ "42",
       pref ==  "Kumamoto"~ "43",
       pref ==  "Oita"~ "44",
       pref ==  "Miyazaki"~ "45",
       pref ==  "Kagoshima"~ "46",
       pref ==  "Okinawa"~ "47",
       TRUE ~ "NA" 
       ) 
)


################################
###### SELECT & set dates ######
################################
case <- case %>%
  dplyr::select(pref, pref_no, date_case, 
                
                case_ma7,  ihs_case_ma7, ihs_diff_case_ma7, ihs_diff7_case_ma7, 
                case_per_ma7, ihs_case_per_ma7, ihs_diff_case_per_ma7,  ihs_diff7_case_per_ma7, 
                ihs_case, ihs_diff7_case, ihs_case_per, ihs_diff7_case_per,
                dummy) %>%
   filter(date_case >= as.Date("2020-02-21") & date_case <= as.Date("2022-6-20"))   ### or "2020-02-27"?
case <- case %>% arrange(as.Date(date_case),pref_no)

google <- google %>%
  dplyr::select(pref_code, date, pref_no, 
                retail, retail_mgm7, diff_retail_mgm7, diff7_retail_mgm7, diff7_retail, 
                transit, transit_mgm7, diff_transit_mgm7, diff7_transit_mgm7, diff7_transit,
                workplaces, workplaces_mgm7, diff_workplaces_mgm7, diff7_workplaces_mgm7, diff7_workplaces,
                grocery, grocery_mgm7, diff_grocery_mgm7, diff7_grocery_mgm7, diff7_grocery, 
                parks, parks_mgm7, diff_parks_mgm7, diff7_parks_mgm7, diff7_parks,
                residential, residential_mgm7, diff_residential_mgm7, diff7_residential_mgm7, diff7_residential,
                Tuesday_dummy, Wednesday_dummy, Thursday_dummy, Friday_dummy, holidays_dummy, holidays_ma7) %>%
  filter(date >= as.Date("2020-02-21") & date <= as.Date("2022-6-20"))
google <- google %>% arrange(as.Date(date),pref_no)

emergency <- emergency %>%
  dplyr::select(pref, pref_no, date, emergency) %>% 
  filter(date >= as.Date("2020-02-21") & date <= as.Date("2022-6-20")) %>% arrange(as.Date(date)) %>% 
         rename (date_emer = date, pref_emer = pref)

#################
### join data ###
#################
google <- google %>% dplyr::mutate_at(vars(pref_no), as.integer) 
case <- case %>% dplyr::mutate_at(vars(pref_no),  as.integer) 
emergency <- emergency %>% dplyr::mutate_at(vars(pref_no),  as.integer) 

google <- google %>% dplyr::mutate_at(vars(date), as.Date) 
case <- case %>% dplyr::mutate_at(vars(date_case), as.Date) 
emergency <- emergency %>% dplyr::mutate_at(vars(date_emer),  as.Date) 

dd <- dplyr::inner_join(google, case, by = c("pref_no" = "pref_no", "date" = "date_case"))
dd <- dplyr::inner_join(dd, emergency, by = c("pref_no" = "pref_no", "date" = "date_emer"))


#################
### set waves ###
#################
dd_wave <- dd %>% dplyr::select(date, pref_no, pref)
wave <- read.csv("wave.csv")

# wave_1
wave_list_1 <- list() 
for (i in 1:47){
  
  s_1 <-  "2020/2/21"
  wave_e_1 <- wave %>% dplyr::select(pref_no, end_1)
  e_1 <-  wave_e_1[wave_e_1$pref_no == i, 2] 
  
  dd_wave_1 <- dd_wave %>% dplyr::filter(pref_no == i)
  wave_wave_1 <- dd_wave_1 %>% dplyr::mutate(wave_1 = if_else(date >= as.Date(s_1) & date <= as.Date(e_1),1,0))
  wave_list_1 <- bind_rows(wave_list_1, list(wave_wave_1))
}
wave_list_1 <- wave_list_1 %>% dplyr::select(-pref)
dd <- dplyr::inner_join(dd, wave_list_1, by = c("pref_no" = "pref_no", "date" = "date"))

# wave_2
wave_list_2 <- list() 
for (i in 1:47){
  
  wave_s_2 <- wave %>% dplyr::select(pref_no, start_2)
  s_2 <-  wave_s_2[wave_s_2$pref_no == i, 2]
  wave_e_2 <- wave %>% dplyr::select(pref_no, end_2)
  e_2 <-  wave_e_2[wave_e_2$pref_no == i, 2] 
  
  dd_wave_2 <- dd_wave %>% dplyr::filter(pref_no == i)
  wave_wave_2 <- dd_wave_2 %>% dplyr::mutate(wave_2 = if_else(date >= as.Date(s_2) & date <= as.Date(e_2),1,0))
  wave_list_2 <- bind_rows(wave_list_2, list(wave_wave_2))
}
wave_list_2 <- wave_list_2 %>% dplyr::select(-pref)
dd <- dplyr::inner_join(dd, wave_list_2, by = c("pref_no" = "pref_no", "date" = "date"))

# wave_3
wave_list_3 <- list() 
for (i in 1:47){
  
  wave_s_3 <- wave %>% dplyr::select(pref_no, start_3)
  s_3 <-  wave_s_3[wave_s_3$pref_no == i, 2]
  wave_e_3 <- wave %>% dplyr::select(pref_no, end_3)
  e_3 <-  wave_e_3[wave_e_3$pref_no == i, 2] 
  
  dd_wave_3 <- dd_wave %>% dplyr::filter(pref_no == i)
  wave_wave_3 <- dd_wave_3 %>% dplyr::mutate(wave_3 = if_else(date >= as.Date(s_3) & date <= as.Date(e_3),1,0))
  wave_list_3 <- bind_rows(wave_list_3, list(wave_wave_3))
}
wave_list_3 <- wave_list_3 %>% dplyr::select(-pref)
dd <- dplyr::inner_join(dd, wave_list_3, by = c("pref_no" = "pref_no", "date" = "date"))

# wave_4
wave_list_4 <- list() 
for (i in 1:47){
  
  wave_s_4 <- wave %>% dplyr::select(pref_no, start_4)
  s_4 <-  wave_s_4[wave_s_4$pref_no == i, 2]
  wave_e_4 <- wave %>% dplyr::select(pref_no, end_4)
  e_4 <-  wave_e_4[wave_e_4$pref_no == i, 2] 
  
  dd_wave_4 <- dd_wave %>% dplyr::filter(pref_no == i)
  wave_wave_4 <- dd_wave_4 %>% dplyr::mutate(wave_4 = if_else(date >= as.Date(s_4) & date <= as.Date(e_4),1,0))
  wave_list_4 <- bind_rows(wave_list_4, list(wave_wave_4))
}
wave_list_4 <- wave_list_4 %>% dplyr::select(-pref)
dd <- dplyr::inner_join(dd, wave_list_4, by = c("pref_no" = "pref_no", "date" = "date"))

# wave_5
wave_list_5 <- list() 
for (i in 1:47){
  
  wave_s_5 <- wave %>% dplyr::select(pref_no, start_5)
  s_5 <-  wave_s_5[wave_s_5$pref_no == i, 2]
  wave_e_5 <- wave %>% dplyr::select(pref_no, end_5)
  e_5 <-  wave_e_5[wave_e_5$pref_no == i, 2] 
  
  dd_wave_5 <- dd_wave %>% dplyr::filter(pref_no == i)
  wave_wave_5 <- dd_wave_5 %>% dplyr::mutate(wave_5 = if_else(date >= as.Date(s_5) & date <= as.Date(e_5),1,0))
  wave_list_5 <- bind_rows(wave_list_5, list(wave_wave_5))
}
wave_list_5 <- wave_list_5 %>% dplyr::select(-pref)
dd <- dplyr::inner_join(dd, wave_list_5, by = c("pref_no" = "pref_no", "date" = "date"))

# wave_6
wave_list_6 <- list() 
for (i in 1:47){
  
  wave_s_6 <- wave %>% dplyr::select(pref_no, start_6)
  s_6 <-  wave_s_6[wave_s_6$pref_no == i, 2]
  e_6 <- "2022/6/20"
  
  dd_wave_6 <- dd_wave %>% dplyr::filter(pref_no == i)
  wave_wave_6 <- dd_wave_6 %>% dplyr::mutate(wave_6 = if_else(date >= as.Date(s_6) & date <= as.Date(e_6),1,0))
  wave_list_6 <- bind_rows(wave_list_6, list(wave_wave_6))
}
wave_list_6 <- wave_list_6 %>% dplyr::select(-pref)
dd <- dplyr::inner_join(dd, wave_list_6, by = c("pref_no" = "pref_no", "date" = "date"))

###########################
### positive & negative ### 
###########################

### Hereafter, positive correspondents to the increasing phase, and negative correspondents to the decreasing phase

# pos
pos_list <- list() 
for (i in 1:47){
  
  s_1 <-  "2020/2/21"
  wave_p_1 <- wave %>% dplyr::select(pref_no, peak_1)
  p_1 <-  wave_p_1[wave_p_1$pref_no == i, 2]
  
  wave_s_2 <- wave %>% dplyr::select(pref_no, start_2)
  s_2 <-  wave_s_2[wave_s_2$pref_no == i, 2]
  wave_p_2 <- wave %>% dplyr::select(pref_no, peak_2)
  p_2 <-  wave_p_2[wave_p_2$pref_no == i, 2]
  
  wave_s_3 <- wave %>% dplyr::select(pref_no, start_3)
  s_3 <-  wave_s_3[wave_s_3$pref_no == i, 2]
  wave_p_3 <- wave %>% dplyr::select(pref_no, peak_3)
  p_3 <-  wave_p_3[wave_p_3$pref_no == i, 2]
  
  wave_s_4 <- wave %>% dplyr::select(pref_no, start_4)
  s_4 <-  wave_s_4[wave_s_4$pref_no == i, 2]
  wave_p_4 <- wave %>% dplyr::select(pref_no, peak_4)
  p_4 <-  wave_p_4[wave_p_4$pref_no == i, 2]
  
  wave_s_5 <- wave %>% dplyr::select(pref_no, start_5)
  s_5 <-  wave_s_5[wave_s_5$pref_no == i, 2]
  wave_p_5 <- wave %>% dplyr::select(pref_no, peak_5)
  p_5 <-  wave_p_5[wave_p_5$pref_no == i, 2]
  
  wave_s_6 <- wave %>% dplyr::select(pref_no, start_6)
  s_6 <-  wave_s_6[wave_s_6$pref_no == i, 2]
  wave_p_6 <- wave %>% dplyr::select(pref_no, peak_6)
  p_6 <-  wave_p_6[wave_p_6$pref_no == i, 2]
  
  dd_wave_pos <- dd_wave %>% dplyr::filter(pref_no == i)
  pos_pos <- dd_wave_pos %>% dplyr::mutate(pos = 
                                               if_else(date >= as.Date(s_1) & date <= as.Date(p_1) |
                                                       date >= as.Date(s_2) & date <= as.Date(p_2) | 
                                                       date >= as.Date(s_3) & date <= as.Date(p_3) |   
                                                       date >= as.Date(s_4) & date <= as.Date(p_4) |   
                                                       date >= as.Date(s_5) & date <= as.Date(p_5) | 
                                                       date >= as.Date(s_6) & date <= as.Date(p_6)  ,1,0))
  pos_list <- bind_rows(pos_list, list(pos_pos))
}
pos_list <- pos_list %>% dplyr::select(-pref)
dd <- dplyr::inner_join(dd, pos_list, by = c("pref_no" = "pref_no", "date" = "date"))

# neg
neg_list <- list() 
for (i in 1:47){
  
  wave_p_1 <- wave %>% dplyr::select(pref_no, peak_1)
  p_1 <-  wave_p_1[wave_p_1$pref_no == i, 2]
  wave_e_1 <- wave %>% dplyr::select(pref_no, end_1)
  e_1 <-  wave_e_1[wave_e_1$pref_no == i, 2] 
  
  wave_p_2 <- wave %>% dplyr::select(pref_no, peak_2)
  p_2 <-  wave_p_2[wave_p_2$pref_no == i, 2]
  wave_e_2 <- wave %>% dplyr::select(pref_no, end_2)
  e_2 <-  wave_e_2[wave_e_2$pref_no == i, 2] 
  
  wave_p_3 <- wave %>% dplyr::select(pref_no, peak_3)
  p_3 <-  wave_p_3[wave_p_3$pref_no == i, 2]
  wave_e_3 <- wave %>% dplyr::select(pref_no, end_3)
  e_3 <-  wave_e_3[wave_e_3$pref_no == i, 2] 
  
  wave_p_4 <- wave %>% dplyr::select(pref_no, peak_4)
  p_4 <-  wave_p_4[wave_p_4$pref_no == i, 2]
  wave_e_4 <- wave %>% dplyr::select(pref_no, end_4)
  e_4 <-  wave_e_4[wave_e_4$pref_no == i, 2] 
  
  wave_p_5 <- wave %>% dplyr::select(pref_no, peak_5)
  p_5 <-  wave_p_5[wave_p_5$pref_no == i, 2]
  wave_e_5 <- wave %>% dplyr::select(pref_no, end_5)
  e_5 <-  wave_e_5[wave_e_5$pref_no == i, 2] 
  
  wave_p_6 <- wave %>% dplyr::select(pref_no, peak_6)
  p_6 <-  wave_p_6[wave_p_6$pref_no == i, 2]
  e_6 <-  "2022/6/20"
  
    dd_wave_neg <- dd_wave %>% dplyr::filter(pref_no == i)
  neg_neg <- dd_wave_neg %>% dplyr::mutate(neg = if_else((date >= as.Date(p_1) +1 & date <= as.Date(e_1)) |
                                                           (date >= as.Date(p_2) +1 & date <= as.Date(e_2)) | 
                                                           (date >= as.Date(p_3) +1 & date <= as.Date(e_3)) |   
                                                           (date >= as.Date(p_4) +1 & date <= as.Date(e_4)) |   
                                                           (date >= as.Date(p_5) +1 & date <= as.Date(e_5)) | 
                                                           (date >= as.Date(p_6) +1 & date <= as.Date(e_6))  ,1,0))
  neg_list <- bind_rows(neg_list, list(neg_neg))
}
neg_list<- neg_list %>% dplyr::select(-pref)
dd <- dplyr::inner_join(dd, neg_list, by = c("pref_no" = "pref_no", "date" = "date"))

############# set pos & neg ##############
dd <- dd %>% dplyr::mutate(pos_ihs_diff_case_ma7 = pos*ihs_diff_case_ma7)
dd <- dd %>% dplyr::mutate(neg_ihs_diff_case_ma7 = neg*ihs_diff_case_ma7)
dd <- dd %>% dplyr::mutate(pos_ihs_diff7_case_ma7 = pos*ihs_diff7_case_ma7)
dd <- dd %>% dplyr::mutate(neg_ihs_diff7_case_ma7 = neg*ihs_diff7_case_ma7)
dd <- dd %>% dplyr::mutate(pos_ihs_diff7_case = pos*ihs_diff7_case)
dd <- dd %>% dplyr::mutate(neg_ihs_diff7_case = neg*ihs_diff7_case)
dd <- dd %>% dplyr::mutate(pos_ihs_diff_death_ma7 = pos*ihs_diff_death_ma7)
dd <- dd %>% dplyr::mutate(neg_ihs_diff_death_ma7 = neg*ihs_diff_death_ma7)
dd <- dd %>% dplyr::mutate(pos_ihs_diff7_death_ma7 = pos*ihs_diff7_death_ma7)
dd <- dd %>% dplyr::mutate(neg_ihs_diff7_death_ma7 = neg*ihs_diff7_death_ma7)
dd <- dd %>% dplyr::mutate(pos_ihs_diff7_death = pos*ihs_diff7_death)
dd <- dd %>% dplyr::mutate(neg_ihs_diff7_death = neg*ihs_diff7_death)



setwd("***")  # set WD
write.csv(dd, "dd.csv")
