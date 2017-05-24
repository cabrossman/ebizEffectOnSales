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

path2 <- 'S:\\eCommerce\\E-Commerce\\Analysis & Reporting\\Omni-Channel\\eBiz effect on FEI sls\\fol_share.sql'
SQL2 <- readChar(path2, file.info(path2)$size)
mydata_fol <- dbGetQuery(con,SQL2)

###################### change necessary fields to factors ##########################
print("Converting Chars to Factors")
for(i in 1:length(mydata_fol)){
  if(length(levels(factor(mydata_fol[,i]))) < 5){mydata_fol[,i] <- factor(mydata_fol[,i])}
  if(class(mydata_fol[,i]) == 'character'){mydata_fol[,i] <- factor(mydata_fol[,i])}
}

###################### ANALYZE DATA ##########################
mydata2_fol <- sqldf('select ZKEY from mydata_fol where TREND = 1 and FOL_CURRENT_CUST = 1')
mydata_fol <- sqldf(' select a.* from mydata_fol a left join mydata2_fol b on a.ZKEY = b.ZKEY WHERE b.ZKEY is null')
#mydata2_fol <- sqldf('select distinct ZKEY from mydata_FOL')
#mydata2_fol$prob <- runif(nrow(mydata2_fol))
#mydata_fol <- sqldf(' select a.* from mydata_fol a join mydata2_fol b on a.ZKEY = b.ZKEY where b.prob <= .25')
f2 <- log(CUST_SLS_TM) ~ FOL_CURRENT_CUST + log(BRANCH_SLS_TM) + TREND + YEARMONTH
plm2 <- plm(f2,data = mydata_fol, index = c("ZKEY","YEARMONTH"), model="within"); summary(plm2)


###################### CORRECT STANDARD ERRORS ##########################
coefSTDerr <- function(dataframe,model){
  G <- length(unique(dataframe$ACCOUNT_NAME))
  N <- length(dataframe$ACCOUNT_NAME)
  dfa <- (G/(G - 1)) * (N - 1)/ model$df.residual
  c_vcov <- dfa*vcovHC(model, type = "HC0", cluster = "group", adjust = T)
  return(coeftest(model, vcov = c_vcov))
}
plm2_se <- coefSTDerr(mydata_fol,plm2); plm2_se