################## required packages################################
print("Loading Libraries")
#packages <- c("RJDBC","plm","sqldf","MatchIt","dplyr","stringr","tidyr","reshape2")
#install.packages(packages)
require(RJDBC)
library(plm)
library(sqldf)
library(MatchIt)
library(dplyr)
library(stringr)
library(tidyr)
library(reshape2)

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
print("Getting Matching 1 Data")
path <- 'S:\\eCommerce\\E-Commerce\\Analysis & Reporting\\Omni-Channel\\eBiz effect on FEI sls\\matchingSQL.sql'
SQL <- readChar(path, file.info(path)$size)
mydata <- dbGetQuery(con,SQL)


###################### change necessary fields to factors ##########################
print("Converting Chars to Factors")
for(i in 1:length(mydata)){
  if(length(levels(factor(mydata[,i]))) < 5){mydata[,i] <- factor(mydata[,i])}
  if(class(mydata[,i]) == 'character'){mydata[,i] <- factor(mydata[,i])}
}


############################ matching #################################################

#note original filters exclude customer that do not match the following criteria in SQL
#where x.CNT_YM > 5 
#and x.FEI_SLS > 10000 
#and x.TOT_ORDERS > 10 
#and x.TOT_LINES > 50  
print("remove NA values")
mydata <- na.omit(mydata)
f1 <- EVER_EBIZ_CUST ~ DOM_TYPE_II + OS_HA + CREDIT_BIN + CNT_YM + I(log(SLS_PER_MONTH)) + I(log(SLS_PER_ORDER)) + I(log(SLS_PER_MONTH*SLS_PER_ORDER)) + PCT_JOB_SLS
f2 <- EVER_FOL_CUST ~ DOM_TYPE_II + OS_HA + CREDIT_BIN + CNT_YM + I(log(SLS_PER_MONTH)) + I(log(SLS_PER_ORDER)) + I(log(SLS_PER_MONTH*SLS_PER_ORDER)) + PCT_JOB_SLS
f3 <- EVER_S2S_CUST ~ DOM_TYPE_II + OS_HA + CREDIT_BIN + CNT_YM + I(log(SLS_PER_MONTH)) + I(log(SLS_PER_ORDER)) + I(log(SLS_PER_MONTH*SLS_PER_ORDER)) + PCT_JOB_SLS
f4 <- EVER_SILVER_FEI ~ DOM_TYPE_II + OS_HA + CREDIT_BIN + CNT_YM + I(log(SLS_PER_MONTH)) + I(log(SLS_PER_ORDER)) + I(log(SLS_PER_MONTH*SLS_PER_ORDER)) + PCT_JOB_SLS
f5 <- EVER_BRONZE_FEI ~ DOM_TYPE_II + OS_HA + CREDIT_BIN + CNT_YM + I(log(SLS_PER_MONTH)) + I(log(SLS_PER_ORDER)) + I(log(SLS_PER_MONTH*SLS_PER_ORDER)) + PCT_JOB_SLS
f6 <- HVAC_STAND_ALONE ~ DOM_TYPE_II + OS_HA + CREDIT_BIN + CNT_YM + I(log(SLS_PER_MONTH)) + I(log(SLS_PER_ORDER)) + I(log(SLS_PER_MONTH*SLS_PER_ORDER)) + PCT_JOB_SLS
f7 <- LYON ~ DOM_TYPE_II + OS_HA + CREDIT_BIN + CNT_YM + I(log(SLS_PER_MONTH)) + I(log(SLS_PER_ORDER)) + I(log(SLS_PER_MONTH*SLS_PER_ORDER)) + PCT_JOB_SLS


print("do the matching")
m.out1 <- matchit(f1, data = mydata,method = "nearest", ratio = 1)
m.out2 <- matchit(f2, data = mydata,method = "nearest", ratio = 1)
m.out3 <- matchit(f3, data = mydata,method = "nearest", ratio = 1)
m.out4 <- matchit(f4, data = mydata,method = "nearest", ratio = 1)
m.out5 <- matchit(f5, data = mydata,method = "nearest", ratio = 1)
m.out6 <- matchit(f6, data = mydata,method = "nearest", ratio = 1)
m.out7 <- matchit(f7, data = mydata,method = "nearest", ratio = 1)
#summary(m.out1)
#plot(m.out1, type = "jitter")
#plot(m.out1, type = "hist")
#plot(m.out2, type = "jitter")
#plot(m.out2, type = "hist")
#plot(m.out3, type = "jitter")
#plot(m.out3, type = "hist")
plot(m.out4, type = "hist")
plot(m.out5, type = "hist")
plot(m.out6, type = "hist")
plot(m.out7, type = "hist")

print("getMatchedData")
m.data1 <- match.data(m.out1)
m.data2 <- match.data(m.out2)
m.data3 <- match.data(m.out3)
m.data4 <- match.data(m.out4)
m.data5 <- match.data(m.out5)
m.data6 <- match.data(m.out6)
m.data7 <- match.data(m.out7)

print("write to table")
dbWriteTable(con2, m.data1$ZKEY, name = "MATCHED_ZKEYS_EBIZ", append=TRUE, row.names=FALSE, overwrite=TRUE)
dbSendUpdate(con2,"ALTER TABLE AAI8260.MATCHED_ZKEYS_EBIZ RENAME COLUMN VALUE TO ZKEY")
dbSendUpdate(con2,"grant select on AAI8260.MATCHED_ZKEYS_EBIZ to public")

dbWriteTable(con2, m.data2$ZKEY, name = "MATCHED_ZKEYS_FOL", append=TRUE, row.names=FALSE, overwrite=TRUE)
dbSendUpdate(con2,"ALTER TABLE AAI8260.MATCHED_ZKEYS_FOL RENAME COLUMN VALUE TO ZKEY")
dbSendUpdate(con2,"grant select on AAI8260.MATCHED_ZKEYS_FOL to public")

dbWriteTable(con2, m.data3$ZKEY, name = "MATCHED_ZKEYS_S2S", append=TRUE, row.names=FALSE, overwrite=TRUE)
dbSendUpdate(con2,"ALTER TABLE AAI8260.MATCHED_ZKEYS_S2S RENAME COLUMN VALUE TO ZKEY")
dbSendUpdate(con2,"grant select on AAI8260.MATCHED_ZKEYS_S2S to public")

dbWriteTable(con2, m.data4$ZKEY, name = "MATCHED_ZKEYS_SILVER", append=TRUE, row.names=FALSE, overwrite=TRUE)
dbSendUpdate(con2,"ALTER TABLE AAI8260.MATCHED_ZKEYS_SILVER RENAME COLUMN VALUE TO ZKEY")
dbSendUpdate(con2,"grant select on AAI8260.MATCHED_ZKEYS_SILVER to public")

dbWriteTable(con2, m.data5$ZKEY, name = "MATCHED_ZKEYS_BRONZE", append=TRUE, row.names=FALSE, overwrite=TRUE)
dbSendUpdate(con2,"ALTER TABLE AAI8260.MATCHED_ZKEYS_BRONZE RENAME COLUMN VALUE TO ZKEY")
dbSendUpdate(con2,"grant select on AAI8260.MATCHED_ZKEYS_BRONZE to public")

dbWriteTable(con2, m.data6$ZKEY, name = "MATCHED_ZKEYS_HVAC_SA", append=TRUE, row.names=FALSE, overwrite=TRUE)
dbSendUpdate(con2,"ALTER TABLE AAI8260.MATCHED_ZKEYS_HVAC_SA RENAME COLUMN VALUE TO ZKEY")
dbSendUpdate(con2,"grant select on AAI8260.MATCHED_ZKEYS_HVAC_SA to public")

dbWriteTable(con2, m.data7$ZKEY, name = "MATCHED_ZKEYS_LYON", append=TRUE, row.names=FALSE, overwrite=TRUE)
dbSendUpdate(con2,"ALTER TABLE AAI8260.MATCHED_ZKEYS_LYON RENAME COLUMN VALUE TO ZKEY")
dbSendUpdate(con2,"grant select on AAI8260.MATCHED_ZKEYS_LYON to public")