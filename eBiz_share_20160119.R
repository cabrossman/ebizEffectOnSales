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

path <- 'S:\\eCommerce\\E-Commerce\\Analysis & Reporting\\Omni-Channel\\eBiz effect on FEI sls\\eBiz_share.sql'
#path <- 'S:\\eCommerce\\E-Commerce\\Analysis & Reporting\\Omni-Channel\\eBiz effect on FEI sls\\Effect on GP\\eBiz_EFFECT_GP.sql'
SQL <- readChar(path, file.info(path)$size)
mydata <- dbGetQuery(con,SQL)

###################### change necessary fields to factors ##########################
print("Converting Chars to Factors")
for(i in 1:length(mydata)){
  if(length(levels(factor(mydata[,i]))) < 5){mydata[,i] <- factor(mydata[,i])}
  if(class(mydata[,i]) == 'character'){mydata[,i] <- factor(mydata[,i])}
}

###################### ANALYZE DATA ##########################
mydata2 <- sqldf('select ZKEY from mydata where TREND = 1 and EBIZ_CURRENT_CUST = 1')
mydata <- sqldf(' select a.* from mydata a left join mydata2 b on a.ZKEY = b.ZKEY WHERE b.ZKEY is null')
#mydata2 <- sqldf('select distinct ZKEY from mydata')
#mydata2$prob <- runif(nrow(mydata2))
#mydata <- sqldf(' select a.* from mydata a join mydata2 b on a.ZKEY = b.ZKEY where b.prob <= .25')
f1 <- log(CUST_SLS_TM) ~ EBIZ_CURRENT_CUST + log(BRANCH_SLS_TM) + TREND + YEARMONTH
plm1 <- plm(f1,data = mydata, index = c("ZKEY","YEARMONTH"), model="within"); summary(plm1)


###################### CORRECT STANDARD ERRORS ##########################
coefSTDerr <- function(dataframe,model){
  G <- length(unique(dataframe$ACCOUNT_NAME))
  N <- length(dataframe$ACCOUNT_NAME)
  dfa <- (G/(G - 1)) * (N - 1)/ model$df.residual
  c_vcov <- dfa*vcovHC(model, type = "HC0", cluster = "group", adjust = T)
  return(coeftest(model, vcov = c_vcov))
}
plm1_se <- coefSTDerr(mydata,plm1); plm1_se