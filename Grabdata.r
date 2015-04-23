
### preparations ------------------------------------------------------------------------------

# clear workspace
rm(list=ls(all=TRUE))

# install and load packages
pkgs <- c("RCurl", "XML", "stringr", "httr", "plyr", "ggplot2","Quandl", "RODBC","rvest"
          "FredR","fpp","zoo", "tsoutliers", "forecast", "readxl", "httr","quantmod" )


# install.packages(pkgs)
lapply(pkgs, library, character.only=T)

install.packages("devtools")
devtools::install_github("jcizel/FredR")
# Look at documentation
vignette("RODBC")

#################################################################################################
# Get from data repositories
# Quandl 
# St Louis FED (FRED)
#################################################################################################

mydata = Quandl("FRED/GDPPOT", start_date="2005-01-03",end_date="2013-04-10",type="xts")
#  or alternatively use quantmod
## St Louis FED
## with API Key
api.key = 'd696e67c986a8c6cc318a551bf2166c3'
fred <- FredR(api.key)
str(fred,1)
gdp.series <- fred$series.search("GDP")
va.starts <- fred$series.search("VABP1FH")
cpi <-  fred$series.search("CPIAUCSL")
mort <- fred$series.search("MORTG")
### Create a data frame of monthly housing starts
vastart <- fred$series.observations(series_id = 'VABP1FH')
### Create a data frame of 30-Year Conventional Mortgage Rate #########################
mortg  <- fred$series.observations(series_id = 'MORTG')
mortg <- subset(mortg,date >= '1996-01-01')
mortg$value <- as.numeric(mortg$value )/100
### COnvert interest rate to monthly payment per $1,000 borrowed, 30-year fixed
mortg$mopay <- (mortg$value*(1+mortg$value)^30)/ (((1+mortg$value)^30) -1)*1000
plot(log(mortg$mopay))
### Create a data frame of monthly cpi ################################################
cpiindx <- fred$series.observations(series_id = 'CPIAUCSL')
cpiindx <- subset(cpiindx,date >= '1996-01-01')
#### Reference 1-1-2010 217.488=100
base = 217.488
cpiindx$value <-  as.numeric(cpiindx$value) / base
# make time-series
cpi.ts1 <- ts(cpiindx, start = c(1996, 1), end = c(2015, 2), frequency = 12)
cpi.ts2 <- cpi.ts1[!is.na(cpi.ts1)]
cpi.ts <- ts(cpi.ts2, start = c(1996, 1), end = c(2015, 2), frequency = 12)
######################################################################################
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
#################################################################################################
#################################################################################################
# Read csv file from NBER on marginal tax rates
#http://users.nber.org/~taxsim/marginal-tax-rates/
#################################################################################################
avgtax <- msp <- read.csv("http://users.nber.org/~taxsim/marginal-tax-rates/at.csv")
head(avgtax)
avgtax.va <- subset(avgtax,state2let=="VA")


#################################################################################################
#################################################################################################
# Read csv file from Zillow on median sales price by county
#################################################################################################
msp <- read.csv("http://files.zillowstatic.com/research/public/County/County_MedianSoldPrice_AllHomes.csv",stringsAsFactors = FALSE)
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

### data frame with NA's
msp.fl <- tmsp.va[1:226,"Fluvanna"]
### Clean data and remove outliers ##########################
# http://robjhyndman.com/hyndsight/forecast5/
##############################################################

plot(msp.fl, main="Median Sales Price, Fluvanna County")
lines(, col='red')

## create time series object
fl.ts <- tsclean(msp.fl)
fl.ts <- ts(fl.ts, start = c(1996, 4), end = c(2015, 1), frequency = 12)

### Inflation adjustment

msp.merge <- merge(fl.ts = as.zoo(fl.ts), cpi.ts = as.zoo(cpi.ts))

msp.merge$fl_real <- msp.merge$fl.ts/(msp.merge$cpi.ts)
msp.merge <- msp.merge[,-c(1:2)]

fl_real.ts <- ts(msp.merge, start = c(1996, 1), end = c(2015, 3), frequency = 12)

## Data examine #################################################################################
#  http://yunus.hacettepe.edu.tr/~iozkan/eco665/archgarch.html
plot(log(fl_real.ts), main="Log of Real Median Sales Price, Fluvanna County")
# stationary in mean
tsdisplay(diff(log(fl_real.ts)))
seasonplot(fl_real.ts,col=rainbow(12),year.labels=TRUE)
###################################################################################################

ts.plot(fl.ts,fl_real.ts, gpars = list(col = c("black", "red")))

#### Seasonal Decomposition
decom.fl <- decompose(fl.ts)
print(decom.fl)

n <- stl(fl.ts,s.window="periodic",robust=TRUE)    #t.window=12,
plot(n)
acf(n$time.series[,3])
print(n)
#AnomalyDetection (https://github.com/twitter/AnomalyDetection)
fl.df <- data.frame( as.Date(time(fl.ts)),Y=as.matrix(fl.ts))
########################################################################
## a univariate time series model  double exponential smooth with a damped trend.

fit1 <- ets(log(fl_real.ts))
accuracy(fit1)
plot(forecast(fit1,level=c(50,80,95)))
plot(forecast(fit1,fan=TRUE))

plot(holt(log(fl_real.ts), damped=TRUE, exponential=TRUE))
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

#################################################################################################
# Read csv file from Zillow on forclosures per 10000 by county
#################################################################################################
forcl <- read.csv("http://files.zillowstatic.com/research/public/County/County_HomesSoldAsForeclosures-Ratio_AllHomes.csv")
head(forcl)
forcl.va <- subset(forcl,StateCodeFIPS=='51')
name <- forcl.va$RegionName
# transpose all but the first column (name)
tforcl.va <- as.data.frame(t(forcl.va[,-1]))
length(tforcl.va)
colnames(tforcl.va) <- name
rownames(tforcl.va) <- NULL
tforcl.va <- tforcl.va[-c(1,2,3,4), ]
sapply(tforcl.va ,class)
tforcl.va$Fluvanna

<- as.numeric(as.character(tforcl.va$Fluvanna))


### Monthly lumber price index (http://future.aae.wisc.edu/data/monthly_values/by_area/3367?area=US&grid=true&tab=prices)
lumber<- read.csv("http://future.aae.wisc.edu/data/monthly_values/by_area/3367.csv?area=US&vector=1",header=FALSE,skip=3)
lumber<- subset( lumber, select = -c(V1,V3) )
lumber.ts <- ts(lumber, start = c(1996, 1), end = c(2015, 3), frequency = 12)

#### Created deflated time-series

m <- merge(lumber.ts = as.zoo(lumber.ts), cpi.ts = as.zoo(cpi.ts))

m$real <- m$lumber.ts/m$cpi.ts
m <- m[,-c(1,2)]
lumber_real.ts <- ts(m, start = c(1996, 1), end = c(2015, 3), frequency = 12)

## Detect outliers #################################################################################
# http://www.jalobe.com:8080/blog/tsoutliers/
####################################################################################################

lnlr.ts <- log(lumber_real.ts)
ce <- calendar.effects(lnlr.ts)
lumber.outlier <- tso(y = lnlr.ts, xreg = ce, cval = 3.5, 
                      types = c("AO", "LS", "TC", "SLS"), maxit = 1, 
                      remove.method = "bottom-up", tsmethod = "arima", 
                      args.tsmethod = list(order = c(0, 1, 1), 
                                           seasonal = list(order = c(0, 1, 1))))
print(lumber.outlier)

res2 <- tso(y = lnlr.ts, xreg = ce, 
    types = c("AO", "LS", "TC", "SLS"), 
    maxit = 1, remove.method = "bottom-up", tsmethod = "auto.arima", 
    args.tsmethod = list(allowdrift = FALSE, D = 1, ic = "bic"))

####################################################################################################
plot(lumber_real.ts)
ts.plot(lumber.ts,lumber_real.ts, gpars = list(col = c("black", "red")))

stl.lumber <- stl(lumber.ts,s.window="periodic",robust=TRUE)    
plot(stl.lumber)

### Test for stationarity  #############################################################################################
###  First differences are the change between one observation and the next. Seasonal differences are the change between
###  one year to the next. (http://www.statosphere.com.au/check-time-series-stationary-r/)
###  http://people.duke.edu/~rnau/whatuse.htm
### Seasonal differences
seasonplot(lumber.ts) 

plot(diff(log(lumber.ts),12), xlab="Year",
     ylab="Annual change in monthly log lumber sales")

Acf(diff(log(lumber.ts),12))
Pacf(diff(log(lumber.ts),12))
#  The Ljung-Box test examines whether there is significant evidence for non-zero correlations at lags 1-20. 
Box.test(diff(log(lumber.ts),12),lag=20,type="Ljung-Box")
# The Augmented Dickey–Fuller (ADF) t-statistic test: small p-values suggest the data is stationary
# https://www.otexts.org/fpp/8/1
adf.test(diff(log(lumber.ts),12), alternative = "stationary")
# Another popular unit root test is the Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test. This reverses the hypotheses,
# so the null-hypothesis is that the data are stationary.
kpss.test(diff(log(lumber.ts),12))
# determining whether seasonal differencing is required is nsdiffs() which uses seasonal unit root tests 
x <- lumber.ts
     ns <- nsdiffs(x)
          if(ns > 0) {
          xstar <- diff(x,lag=frequency(x),differences=ns)
          } else {
            xstar <- x
          }
          nd <- ndiffs(xstar)
          if(nd > 0) {
          xstar <- diff(xstar,differences=nd)
          }
#######################################################################################################################
### Univariate forecast models

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


url <- "http://www.fhfa.gov/DataTools/Downloads/Documents/Historical-Summary-Tables/MIRS_Table26_Monthly_2012.xls"
GET(url, write_disk("temp.xls", overwrite=TRUE))
irate <- read_excel("temp.xls", col_names = FALSE, skip=11)
head(irate)


# Bedford county VA tax assessment
# http://bedfordco.dreamhosters.com/BedfordTax.xlsx
# Richmond transfers and sales http://www.richmondgov.com/Assessor/ReportsPage.aspx

#################################################################################################
# Read excel file on 30-year commitment rate from the Fannie Mae and Freddie Mac (monthly, US)
# http://www.netegrate.com/index_files/Research%20Library/Catalogue/Economics/Housing/Modeling%20Long-range%20Dependence%20in%20U.S.%20Housing%20Prices.pdf
################################################################################################

url4 <- "http://www.freddiemac.com/pmms/docs/30yr_pmmsmnth.xls"
crate30 <- read.xls(url4)
head(crate30)

#################################################################################################
# Read excel file on Single-Family Mortgage Originations, 1990 – 2011 Q2  
# http://www.fhfa.gov/DataTools/Downloads/Pages/Current-Market-Data.aspx

#http://www.fhfa.gov/DataTools/Downloads/Documents/Market-Data/SF_Mortgage_Originations_1990-2011Q2.xls

#############################################################################################
#####      Foreclosure risk
#####      http://www.foreclosure-response.org/maps_and_data/hmda_downloads.html
#####      Historical data http://www.foreclosure-response.org/maps_and_data/hmda_data_2010.html
#####      Vacancy rates from USPS http://www.huduser.org/portal/usps/index.html
#############################################################################################


urlfrorg <- "http://www.foreclosure-response.org/assets/hmda2011/hmda_state_VA_2011.xls"
GET(urlfrorg, write_disk("temp1.xls", overwrite=TRUE))
frorg <- read_excel("temp1.xls") #, col_names = FALSE, skip=11)
head(frorg)


### Method 2
install.packages("XLConnect")
require(XLConnect)

temp.irate = tempfile(fileext = ".xls")
download.file(url = "http://www.fhfa.gov/DataTools/Downloads/Documents/Historical-Summary-Tables/MIRS_Table26_Monthly_2012.xlss",
              destfile = temp.irate)

mrate <- readWorksheetFromFile(temp.irate,sheet=1, startRow = 12)


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

#################################################################################################
# Scrape data from webpages
# Use rvest
# http://www.charlottesvillerealestatebuzz.com/market-report/fluvanna-county-va-real-estate-market-report-june-2013/
#################################################################################################
### Create loop for monthly reports by year
year <- seq(2013,2015,1)
m <- length(year) 


months <- month.name
n <- length(months) 
links <- vector("list", length = n)
result <- vector("list", length = n)

for (j in 1:m){

for(i in 1:n){
  print(i) # keep track of what the function is up to
  # get all html on each page of the a-z index pages
  
  links[[i]]  <- htmlParse(paste0("http://www.charlottesvillerealestatebuzz.com/market-report/fluvanna-county-va-real-estate-market-report-", months[i], "-",year[j],"/"),encoding = "UTF-8")
  xpath <- '//li'
  result[[i]] <- xpathSApply(links[[i]], xpath,xmlValue)
  result[[i]] <- result[[i]][c(13:17)]
  # clean up numbers
  result[[i]] <- str_replace_all( result[[i]] , "\\$", "")
  result[[i]] <- str_replace_all( result[[i]] , "%", "")
  result[[i]] <- str_replace_all( result[[i]] , ",", "")
  
  result[[i]] <- as.data.frame(do.call(rbind, strsplit(result[[i]], ": ")),ncol=2)
  result[[i]] <- setNames(result[[i]], c("item","value"))
}
  
  
 
total[[j]]  <- cbind(result[[1]],result[[2]]$value,result[[3]]$value,result[[4]]$value,result[[5]]$value,
            result[[6]]$value,result[[7]]$value,result[[8]]$value,result[[9]]$value,result[[10]]$value,
            result[[11]]$value,result[[12]]$value )

}

