
# Read csv file from Zillow on median sales price by county
msp <- read.csv("http://files.zillowstatic.com/research/public/County/County_MedianSoldPrice_AllHomes.csv")
head(msp)
msp.va <- subset(msp,StateCodeFIPS=='51')
name <- msp.va$RegionName
# transpose all but the first column (name)
tmsp.va <- as.data.frame(t(msp.va[,-1]))
colnames(tmsp.va) <- name
tmsp.va <- tmsp.va[-c(2,3,4,5), ]
tmsp.va$period = seq(as.Date("1996/4/1"), as.Date("2015/2/1"),"month")

library(ggplot2) 

qplot(period,Fluvanna,data=tmsp.va)

ggplot(tmsp.va) + geom_line (aes(x=period, y=Fluvanna))
       
       
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
