

# Read csv file from Zillow on median sales price by county
msp <- read.csv("http://files.zillowstatic.com/research/public/County/County_MedianSoldPrice_AllHomes.csv")
head(msp)
msp.va <- subset(msp,StateCodeFIPS=='51')
name <- msp.va$RegionName
# transpose all but the first column (name)
tmsp.va <- as.data.frame(t(msp.va[,-1]))
length(tmsp.va)
colnames(tmsp.va) <- name
rownames(tmsp.va) <- NULL
tmsp.va <- tmsp.va[-c(1,2,3,4), ]
sapply(tmsp.va ,class)
tmsp.va$Fluvanna <- as.numeric(as.character(tmsp.va$Fluvanna))

tmsp.va$period = seq(as.Date("1996/4/1"), as.Date("2015/2/1"),"month")

msp.fluvan <- tmsp.va[1:226,c("period","Fluvanna")]

#### Seasonal Decomposition
msp.fl <- tmsp.va[1:226,"Fluvanna"]
fl.ts <- approxfun(1:226, msp.fl)(1:226)
fl.ts <- ts(fl.ts, start = c(1996, 4), end = c(2015, 1), frequency = 12)
m <- decompose(fl.ts)
print(m)

n <- stl(fl.ts,s.window="periodic",robust=TRUE)    #t.window=12,
plot(n)
acf(n$time.series[,3])
print(n)
#AnomalyDetection (https://github.com/twitter/AnomalyDetection)

fl.df <- data.frame( as.Date(time(fl.ts)),Y=as.matrix(fl.ts))



install.packages("devtools")
devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)

res = AnomalyDetectionTs(fl.df, max_anoms=0.02,direction='both', plot=TRUE)
res = AnomalyDetectionVec(fl.df$Y, max_anoms=0.02,  period=20,direction='both', plot=TRUE)
res$plot

########################################################################

rownames(msp.fluvan) <- NULL
sapply(msp.fluvan,class)

msp.fluvan$timestamp <- as.POSIXlt(msp.fluvan$period, format="%Y/%m/%d", tz="GMT")

fluvan.ts <- as.data.frame(msp.fluvan[,c("timestamp","Fluvanna")])

## deal with NA's

fluvan.ts$Fluvanna <- approxfun(1:226, fluvan.ts$Fluvanna)(1:226)

head(fluvan.ts)

library(ggplot2) 

qplot(period,Fluvanna,data=fluvan.ts)

ggplot(tmsp.va) + geom_line (aes(x=period, y=Fluvanna))




### Monthly lumber price index (http://future.aae.wisc.edu/data/monthly_values/by_area/3367?area=US&grid=true&tab=prices)
lumber<- read.csv("http://future.aae.wisc.edu/data/monthly_values/by_area/3367.csv?area=US&vector=1",header=FALSE,skip=3)
lumber<- subset( lumber, select = -c(V1,V3) )
lumber.ts <- ts(lumber, start = c(1996, 1), end = c(2015, 3), frequency = 12)

stl.lumber <- stl(lumber.ts,s.window="periodic",robust=TRUE)    
plot(stl.lumber)
### Univariate forecast models
require(forecast)
plot(forecast(ets(lumber.ts), 10))

fit <- arima(lumber.ts, order=c(1,0,0), list(order=c(2,1,0), period=12))
fore <- predict(fit, n.ahead=24)
L <- fore$pred - 2*fore$se
U <- fore$pred  + 2*fore$se

ts.plot(lumber.ts, fore$pred, U, L, col=c(1,2,4,4), lty = c(1,1,2,2))
### Monthly building permits
### http://socds.huduser.org/permits/output_monthly.odb

       
#################################################################################################
# Read excel file on interest rates from FHFA & Median county income from Census
################################################################################################
### Method 1
require(RCurl)
require(gdata)

url <- "http://www.fhfa.gov/DataTools/Downloads/Documents/Historical-Summary-Tables/MIRS_Table26_Monthly_2012.xls"
irate <- read.xls(url)
head(irate)


# Bedford county VA tax assessment
# http://bedfordco.dreamhosters.com/BedfordTax.xlsx
# Richmond transfers and sales http://www.richmondgov.com/Assessor/ReportsPage.aspx


#############################################################################################
#####      Foreclosure risk
#####      http://www.foreclosure-response.org/maps_and_data/hmda_downloads.html
#####      Historical data http://www.foreclosure-response.org/maps_and_data/hmda_data_2010.html
#####      Vacancy rates from USPS http://www.huduser.org/portal/usps/index.html
#############################################################################################
urlfrorg <- "http://www.foreclosure-response.org/assets/hmda2011/hmda_state_VA_2011.xls"
hmda11 <- read.xls(urlfrorg)
head (hmda11)


### Method 2
install.packages("XLConnect")
require(XLConnect)

tmp13 = tempfile(fileext = ".xls")
download.file(url = "http://www.census.gov/did/www/saipe/downloads/estmod13/est13ALL.xls", destfile = tmp13)
tmp12 = tempfile(fileext = ".xls")
download.file(url = "http://www.census.gov/did/www/saipe/downloads/estmod13/est12ALL.xls", destfile = tmp12)
tmp11 = tempfile(fileext = ".xls")
download.file(url = "http://www.census.gov/did/www/saipe/downloads/estmod13/est11ALL.xls", destfile = tmp11)
tmp10 = tempfile(fileext = ".xls")
download.file(url = "http://www.census.gov/did/www/saipe/downloads/estmod13/est11ALL.xls", destfile = tmp10)

medinc13 <- readWorksheetFromFile(tmp13,sheet=1, startRow = 4)
head(medinc13)
medinc12 <- readWorksheetFromFile(tmp12,sheet=1, startRow = 4)
head(medinc12)


#################################################################################################
#Read file from Zip
# Use downloader library
#################################################################################################
require(downloader)
download("http://files.zillowstatic.com/research/public/County.zip", dest="dataset.zip", mode="wb") 
unzip ("dataset.zip", list=TRUE)
turnover <- read.csv(unzip ("dataset.zip", files = "County/County_Turnover_AllHomes.csv"))
turnover.va <- subset(turnover,StateCodeFIPS=='51')
library(reshape)
mturnover.va <- melt(turnover.va, id=c("RegionName","MunicipalCodeFIPS")) 
head(mturnover.va)
#######################################################################
## Example 2 -- ZIP file with SAS data 
## Housing affordability data (every 2 years)
## http://www.huduser.org/portal/datasets/hads/hads.html
#######################################################################
download("http://www.huduser.org/portal/datasets/hads/hads2011(SAS).zip", dest="HUDdata.zip", mode="wb") 
unzip ("HUDdata.zip", list=TRUE)
library(sas7bdat)
hads2011 <- read.sas7bdat(unzip ("HUDdata.zip"))

#######################################################################
## Example 3 -- ZIP file with MS Access data 
## SOCDS Building Permits Database
## http://socds.huduser.org/permits/help.htm
#######################################################################
### USE RODBS to read access file
install.packages("RODBC")
library(RODBC)
vignette("RODBC")
#################################
download("http://socds.huduser.org/permits/bpdata4web.zip", dest="HUDdpermits.zip", mode="wb") 
db <- unzip ("HUDdpermits.zip")
con = odbcConnectAccess2007(db)
sqlTables(con, tableType = "TABLE")$TABLE_NAME
# Get variables names
sqlColumns(con, "Place_monthlycumF")$COLUMN_NAME
permitbymo = sqlFetch(con, "Place_monthlycumF")
# Close connections
odbcCloseAll()

perm.va <- subset(permitbymo,state=='51' & series=='1')
perm.fl <- subset(perm.va,county=='65')
perm.fl <- perm.fl[,c("year", "month", "permits")]
library(plyr)
perm.fl = arrange(perm.fl,year,month)
perm.fl <- perm.fl[,"permits"]

permfl.ts <- ts(perm.fl, start = c(1997, 1), end = c(2013, 12), frequency = 12)
stl.permfl <- stl(permfl.ts,s.window="periodic",robust=TRUE)    
plot(stl.permfl)

#################################################################################################
#Read file from URL JSON
# Use rjson
#################################################################################################
install.packages("rjson")
library("rjson")
json <- fromJSON(getURL('https://api.consumerfinance.gov/data/hmda.json',ssl.verifypeer=FALSE))


#################################################################################################
#Read file from Google sheets
# Use the googlesheets package, a Google Sheets R API by Jenny Bryan
#https://github.com/jennybc/googlesheets
#################################################################################################
install.packages("devtools")
library("devtools")
devtools::install_github("jennybc/googlesheets")
library("googlesheets")
suppressMessages(library("dplyr"))

#################################################################################################
# Get drom data repositories
# Quandl 
# St Louis FED (FRED)
#################################################################################################

require(Quandl)
mydata = Quandl("FRED/GDPPOT", start_date="2005-01-03",end_date="2013-04-10",type="xts")
#  or alternatively use quantmod
install.packages("quantmod")
require(quantmod)


## St Louis FED
## with API Key
devtools::install_github("jcizel/FredR")
require(FredR)
require(ggplot2)
api.key = 'd696e67c986a8c6cc318a551bf2166c3'
fred <- FredR(api.key)
str(fred,1)
gdp.series <- fred$series.search("GDP")
va.starts <- fred$series.search("VABP1FH")
### Create a data frame of monthly housing starts
vastart <- fred$series.observations(series_id = 'VABP1FH')
#plot the result
vastart %>%
select(
    date,
    value
) %>%
mutate(
    date = as.Date(date),
    value = as.numeric(value)
) ->
    dt


qplot(data = dt, x = date, y = value, geom = 'line')

################################################################
# Use fread from the data.table package
install.packages("data.table")
library(data.table)

read.url <- function(url, ...){
  tmpFile <- tempfile()
  download.file(url, destfile = tmpFile, method = "curl")
  url.data <- fread(tmpFile, ...)
  return(url.data)
}

read.url("https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/246663/pmgiftsreceivedaprjun13.csv")

