# Unravelling habituation for COVID-19-related information: A panel data study in Japan 
## (published in _PLOS ONE_)
## by Shinya Fukui

# Data
### Some of the data are available through the external websites.
- The daily human mobility data for each prefecture are obtained from Google’s COVID-19 Community Mobility Reports [Google's page](https://www.google.com/covid19/mobility/).
- The daily data on COVID-19 vaccination of each prefecture are obtained from the number of COVID-19 vaccines administered by the Ministry of Health, Labour and Welfare (MHLW) (page is in Japanese; file is in JSON format (NDJSON)) [MHLW's page](https://www.mhlw.go.jp/stf/seisakunitsuite/bunya/kenkou_iryou/kenkou/kekkaku-kansenshou/yobou-sesshu/syukeihou_00002.html).

# R codes
The R codes page is now under construction (July 16, 2024) and will be completed in a month.

# Terms of Use

Users (hereinafter referred to as the User or Users depending on context) of the content on this web site (hereinafter referred to as the “Content”) are required to conform to the terms of use described herein (hereinafter referred to as the Terms of Use). Furthermore, use of the Content constitutes agreement by the User with the Terms of Use. The contents of the Terms of Use are subject to change without prior notice.

## Copyright
The copyright of the developed R codes belongs to Shinya Fukui.

## Copyright of Third Parties
-	The daily human mobility data for each prefecture are obtained from Google’s COVID-19 Community Mobility Reports [Google’s page](https://www.google.com/covid19/mobility/). 
-	Data on the daily number of newly infected cases of COVID-19 are originally obtained from NHK (NIPPON HOSO KYOKAI; Japan Broadcasting Corporation). (Currently available from the Ministry of Health, Labour and Welfare (MHLW). The data shows -Information on COVID-19 infections - (in Japanese; note that local governments have made retrospective corrections regarding infected cases in some cases.) [MHLW’s page](https://covid19.mhlw.go.jp).
-	The declarations of a state of emergency data are obtained from the Cabinet Secretariat’s COVID-19 Information and Resources (Currently available from the Available from Cabinet Agency for Infectious Disease Crisis Management (CAICM). About COVID-19 – Others (in Japanese) [CAICM’s page](https://www.caicm.go.jp/information/citizen/corona/index.html).
-	The daily data on COVID-19 vaccination of each prefecture are originally obtained from Digital Agency. Open data on the vaccination status of COVID-19 vaccines. (Currently available from the MHLW. The number of COVID-19 vaccines administered, open data. (in Japanese). (page is in Japanese; file is in JSON format (NDJSON)) [MHLW's page](https://www.mhlw.go.jp/stf/seisakunitsuite/bunya/kenkou_iryou/kenkou/kekkaku-kansenshou/yobou-sesshu/syukeihou_00002.html).
-	Shinya Fukui built the weight matrices based on the the Travel Data from Vital Signs of Economy-Regional Economy and Society Analyzing System (V-RESAS) provided by the Cabinet Secretariat and the Cabinet Office, Government of Japan (note that V-RESAS is no longer available to the public [Cabinet Office’s page](https://resas-portal.go.jp/medias-import/A000005_20240301_v-resas_notice.pdf)). The Travel Data from V-RESAS are constructed from Agoop Corporation’s Current Population Data, which is based on GPS data obtained with user consent from specific smartphone applications and makes demographic data using day/night population data.
-	Temperature and precipitation data are obtained from the Japan Meteorological Agency (JMA) [JAM’s page](https://www.data.jma.go.jp/gmd/risk/obsdl/index.php). 
-	The population density per square kilometre of inhabitable land area and the percentage of the population over 65 years old—are extracted from the Regional Statistics Database (System of Social and Demographic Statistics) from the Statistics Bureau of Japan (SBJ) [SBJ’s page](https://www.e-stat.go.jp/regional-statistics/ssdsview). 
-	Users must confirm the terms of use of the Google, NHK, MHLW, Cabinet Secretariat, CAICM, Cabinet Office, Agoop corp., JMA, and the SBJ, prior to using the Content.

## Licence
The developed codes are released under the MIT Licence.

## Disclaimer
-	Shinya Fukui makes the utmost effort to maintain, but nevertheless does not guarantee, the accuracy, completeness, integrity, usability, and recency of the Content.
-	Shinya Fukui and any organization to which Shinya Fukui belongs hereby disclaim responsibility and liability for any loss or damage that may be incurred by Users as a result of using the Content.
-	Shinya Fukui and any organization to which Shinya Fukui belongs are neither responsible nor liable for any loss or damage that a User of the Content may cause to any third party as a result of using the Content. The Content may be modified, moved or deleted without prior notice.

# Author
### Shinya Fukui
Visiting researcher, Graduate School of Economics, Osaka Metropolitan University, Sakai, Osaka, Japan
