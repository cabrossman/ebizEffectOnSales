################## required packages################################
print("Loading Libraries")
#packages <- c("RJDBC","plm","sqldf","MatchIt","dplyr","stringr","tidyr","reshape2","lmtest","sandwich","AER","systemfit")
#install.packages(packages)
require(RJDBC)
library(plm)
library(sqldf)
library(MatchIt)
library(dplyr)
library(stringr)
library(tidyr)
library(reshape2)
library(lmtest)
library(sandwich)
library(AER)
library(systemfit)

################ Get Data ####################################
print("Establishing Connections")
CLASSPATH <- "C:\\Users\\aai8260\\Desktop\\JAVA\\ojdbc6.jar"
driver <- JDBC("oracle.jdbc.driver.OracleDriver",classPath= CLASSPATH," ")
UID = "AAI8260"

#APPEX
URL = "jdbc:oracle:thin:@a05129.sys.ds.wolseley.com:1550:APXPRD00"
PASS = "J321C#i"
con <- dbConnect(driver,URL,UID,PASS)

#DWFEI
URL2 = "jdbc:oracle:thin:@nnldwp01.sys.ds.wolseley.com:1521:DWFEI"
PASS2 = "PA55W0RD"
con2 <- dbConnect(driver,URL2,UID,PASS2)

#################read in SQL###############################
print("Getting DiD Data")

path <- 'S:\\eCommerce\\E-Commerce\\Analysis & Reporting\\Omni-Channel\\eBiz effect on FEI sls\\effect of silver_bronze\\eBiz_EFFECT_BRONZE.sql'
path2 <- 'S:\\eCommerce\\E-Commerce\\Analysis & Reporting\\Omni-Channel\\eBiz effect on FEI sls\\effect of silver_bronze\\eBiz_EFFECT_SILVER.sql'
SQL <- readChar(path, file.info(path)$size)
SQL2 <- readChar(path2, file.info(path2)$size)
mydata_BRONZE <- dbGetQuery(con,SQL)
mydata_SILVER <- dbGetQuery(con,SQL2)

###################### change necessary fields to factors ##########################
print("Converting Chars to Factors")
for(i in 1:length(mydata_BRONZE)){
  if(length(levels(factor(mydata_BRONZE[,i]))) < 5){mydata_BRONZE[,i] <- factor(mydata_BRONZE[,i])}
  if(class(mydata_BRONZE[,i]) == 'character'){mydata_BRONZE[,i] <- factor(mydata_BRONZE[,i])}
}
print("Converting Chars to Factors")
for(i in 1:length(mydata2)){
  if(length(levels(factor(mydata_SILVER[,i]))) < 5){mydata_SILVER[,i] <- factor(mydata_SILVER[,i])}
  if(class(mydata_SILVER[,i]) == 'character'){mydata_SILVER[,i] <- factor(mydata_SILVER[,i])}
}

###################### ANALYZE DATA ##########################
#mydata2 <- sqldf('select ZKEY from mydata where TREND = 1 and EBIZ_CURRENT_CUST = 1')
#mydata <- sqldf(' select a.* from mydata a left join mydata2 b on a.ZKEY = b.ZKEY WHERE b.ZKEY is null')
#mydata2 <- sqldf('select distinct ZKEY from mydata')
#mydata2$prob <- runif(nrow(mydata2))
#mydata <- sqldf(' select a.* from mydata a join mydata2 b on a.ZKEY = b.ZKEY where b.prob <= .25')
#f1 <- log(CUST_SLS_TM) ~ EBIZ_CURRENT_CUST + log(BRANCH_SLS_TM) + TREND + YEARMONTH
#mydata_SILVER <- sqldf(' select * from mydata where PROPLUS = 1 AND EVER_EBIZ_CUST = 1
#                  UNION ALL 
#                select * from mydata where EVER_EBIZ_CUST = 0')
#mydata_BRONZE <- sqldf(' select * from mydata where PROPLUS = 0 AND EVER_EBIZ_CUST = 1
#                  UNION ALL 
#                select * from mydata where EVER_EBIZ_CUST = 0')
#rm(mydata)
f1 <- log(CUST_SLS_TM) ~ EBIZ_CURRENT_CUST + log(BRANCH_SLS_TM) + TREND + YEARMONTH
f2 <- log(CUST_SLS_TM) ~ EBIZ_CURRENT_CUST + PROPLUS_CURRENT_CUST + EBIZ_CURRENT_CUST*PROPLUS_CURRENT_CUST + log(BRANCH_SLS_TM) + TREND + YEARMONTH
plm_Interaction <- plm(f2,data = mydata, index = c("ZKEY","YEARMONTH"), model="within"); summary(plm_Interaction)
plm_SILVER <- plm(f1,data = mydata_SILVER, index = c("ZKEY","YEARMONTH"), model="within"); summary(plm_SILVER)
plm_BRONZE <- plm(f1,data = mydata_BRONZE, index = c("ZKEY","YEARMONTH"), model="within"); summary(plm_BRONZE)

###################### CORRECT STANDARD ERRORS ##########################
coefSTDerr <- function(dataframe,model){
  G <- length(unique(dataframe$ACCOUNT_NAME))
  N <- length(dataframe$ACCOUNT_NAME)
  dfa <- (G/(G - 1)) * (N - 1)/ model$df.residual
  c_vcov <- dfa*vcovHC(model, type = "HC0", cluster = "group", adjust = T)
  return(coeftest(model, vcov = c_vcov))
}
coefSTDerr(mydata_SILVER,plm1)