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

path3 <- 'S:\\eCommerce\\E-Commerce\\Analysis & Reporting\\Omni-Channel\\eBiz effect on FEI sls\\s2s_share.sql'
SQL3 <- readChar(path3, file.info(path3)$size)
mydata_s2s <- dbGetQuery(con,SQL3)

###################### change necessary fields to factors ##########################
print("Converting Chars to Factors")
for(i in 1:length(mydata_s2s)){
  if(length(levels(factor(mydata_s2s[,i]))) < 5){mydata_s2s[,i] <- factor(mydata_s2s[,i])}
  if(class(mydata_s2s[,i]) == 'character'){mydata_s2s[,i] <- factor(mydata_s2s[,i])}
}

###################### ANALYZE DATA ##########################
mydata2_s2s <- sqldf('select ZKEY from mydata_s2s where TREND = 1 and s2s_CURRENT_CUST = 1')
mydata_s2s <- sqldf(' select a.* from mydata_s2s a left join mydata2_s2s b on a.ZKEY = b.ZKEY WHERE b.ZKEY is null')
#mydata2_s2s <- sqldf('select distinct ZKEY from mydata_s2s')
#mydata2_s2s$prob <- runif(nrow(mydata2_s2s))
#mydata_s2s <- sqldf(' select a.* from mydata_s2s a join mydata2_s2s b on a.ZKEY = b.ZKEY where b.prob <= .25')
f3 <- log(CUST_SLS_TM) ~ S2S_CURRENT_CUST + log(BRANCH_SLS_TM) + TREND + YEARMONTH
plm3 <- plm(f3,data = mydata_s2s, index = c("ZKEY","YEARMONTH"), model="within"); summary(plm3)


###################### CORRECT STANDARD ERRORS ##########################
coefSTDerr <- function(dataframe,model){
  G <- length(unique(dataframe$ACCOUNT_NAME))
  N <- length(dataframe$ACCOUNT_NAME)
  dfa <- (G/(G - 1)) * (N - 1)/ model$df.residual
  c_vcov <- dfa*vcovHC(model, type = "HC0", cluster = "group", adjust = T)
  return(coeftest(model, vcov = c_vcov))
}
plm3_se <- coefSTDerr(mydata_s2s,plm3); plm3_se