#--------------------------------------------------------------------#
# Hypothesis:  Distressed home sales (foreclosure, short-sales) have
# had a large and sustained impact on the sales price of exisiting 
# homes in Fluvanna county Virginia since the market collapse in 2008.
#--------------------------------------------------------------------#
# install and load packages
pkgs <- c("plyr","rvest","XML","forecast")

#c("RCurl", "XML", "stringr", "httr", "plyr", "ggplot2","rvest","forecast",
#          "zoo", "httr", "mgcv","gamair", "knitr", "markdown", "dplyr","xml2","googleVis") 

install.packages(pkgs)
lapply(pkgs, library, character.only=T)

#vignette("googleVis")

library(devtools)
devtools::install_github("google/CausalImpact")
library(CausalImpact)

#################################################################################################
# Scrape data from webpages
# Use rvest
# http://www.charlottesvillerealestatebuzz.com/market-report/fluvanna-county-va-real-estate-market-report-june-2013/
#################################################################################################
### Create loop for monthly reports by year
year <- seq(2012,2014,1)
m <- length(year) 
total <- vector("list", length = m)
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
    
    if(length(result[[i]]) >= 17) {
      result[[i]] <- result[[i]][c(13:17)]
      # clean up numbers
      result[[i]] <- str_replace_all( result[[i]] , "\\$", "")
      result[[i]] <- str_replace_all( result[[i]] , "%", "")
      result[[i]] <- str_replace_all( result[[i]] , ",", "")
      
      result[[i]] <- as.data.frame(do.call(rbind, strsplit(result[[i]], ": ")))
      result[[i]][,2] <- as.numeric(as.character(result[[i]][,2] )) 
      result[[i]] <- setNames(result[[i]], c("item","value"))
    }
    else {
      items <- c("Total Home Units Sold","Average Sales Price","Median Sales Price",
                 "Average List to Sales Price", "Average Days on Market")
      result[[i]] <- data.frame(item = items, value = 1:5)
      
    }
  }
  
  
  
  total[[j]]  <- cbind(result[[1]],result[[2]]$value,result[[3]]$value,result[[4]]$value,result[[5]]$value,
                       result[[6]]$value,result[[7]]$value,result[[8]]$value,result[[9]]$value,result[[10]]$value,
                       result[[11]]$value,result[[12]]$value )
  
}
# transpose all but the first column (item)
hmr12 <- as.data.frame(t(total[[1]][,-1]))
colnames(hmr12) <- total[[1]]$item
rownames(hmr12) <- NULL
head(hmr12)
sapply(hmr12 ,class)
hmr12.ts <- ts(hmr12, start = c(2012, 1), end = c(2012, 12), frequency = 12)

hmr13 <- as.data.frame(t(total[[2]][,-1]))
colnames(hmr13) <- total[[2]]$item
rownames(hmr13) <- NULL
head(hmr13)
sapply(hmr13 ,class)
hmr13.ts <- ts(hmr13, start = c(2013, 1), end = c(2013, 12), frequency = 12)


hmr14 <- as.data.frame(t(total[[3]][,-1]))
colnames(hmr14) <- total[[3]]$item
rownames(hmr14) <- NULL
head(hmr14)
sapply(hmr14 ,class)
hmr14.ts <- ts(hmr14, start = c(2014, 1), end = c(2014, 12), frequency = 12)

hmrall <- rbind_all(list(hmr12,hmr13,hmr14))
# Fix July 2012
hmrall[7,1] <- 28
hmrall[7,2] <- 195605
hmrall[7,3] <- 184781
hmrall[7,4] <- 94.64
hmrall[7,5] <- 90
hmrall[7,]

#################################################################################################
#  COMPARE WITH ZILLOW DATA
#################################################################################################
# Read csv file from Zillow on median sales price by county
#################################################################################################
msp <- read.csv("http://files.zillowstatic.com/research/public/County/County_MedianSoldPrice_AllHomes.csv",stringsAsFactors = FALSE)
head(msp)
msp.va <- subset(msp,StateCodeFIPS=='51')
msp.va <- msp.va[,-c(2,3, 5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26), ]
name <- msp.va$RegionName
# transpose all but the first column (name)
tmsp.va <- as.data.frame(t(msp.va[,-1]))
length(tmsp.va)
colnames(tmsp.va) <- name
rownames(tmsp.va) <- NULL
tmsp.va <- tmsp.va[-c(1), ]
sapply(tmsp.va ,class)
tmsp.va$Fluvanna <- as.numeric(as.character(tmsp.va$Fluvanna))
tmsp.va$period = seq(as.Date("1998/1/1"), as.Date("2015/3/3"),"month")

msp.fluvan <- tmsp.va[1:207,c("period","Fluvanna")]
msp.fl <- tmsp.va[1:207,"Fluvanna"]
################################################
## create time series object
## deal with NA's
## why missing--no sales that month?
#################################################
fl.ts <- tsclean(msp.fl)
##### Use later in analysis
flvec  = unclass(fl.ts)

fl.ts <- ts(fl.ts, start = c(1996, 4), end = c(2015, 1), frequency = 12)
#subset the time series using window commd
fl14.ts <- window(fl.ts, start = c(2014,1), end = c(2014, 12))
fl13.ts <- window(fl13.ts, start = c(2013,1), end = c(2013, 12))

fit = stl(house2, s.window="periodic")

################################################################################################
#pdf("Compare1.pdf",width=10,height=8)
ts.plot(fl14.ts,hmr14.ts[,3],gpars = list(col = c("black", "red")))
#dev.off()
### Use Goodle charts
df1 <- data.frame(Y=as.matrix(fl14.ts),date= as.Date(fl14.ts))
df1$month.of.year <- seq(1:12)
df1$year=2014
df2 <- data.frame(Y=as.matrix(fl13.ts),date= as.Date(fl13.ts))
df2$month.of.year <- seq(1:12)
df2$year=2013
df3 <- rbind_all(list(df1,df2))
df3$time <- seq(1:24)
################################################################################################
df4 <- data.frame(Y=as.matrix(hmr14.ts), date= as.Date(hmr14.ts))
df4$month.of.year <- seq(1:12)
df4$year=2014
df5 <- data.frame(Y=as.matrix(hmr13.ts), date= as.Date(hmr13.ts))
df5$month.of.year <- seq(1:12)
df5$year=2013
df6 <- rbind_all(list(df4,df5))
df6$time <- seq(1:24)
#################################################################################################
#####  Plot using Google charts
#Line1 <- gvisLineChart(df1)
#plot(Line1)
#### 

#################################################################################################
#  COMPARE WITH HUD
#################################################################################################
# https://www.onecpd.info/onecpd/assets/File/FY-2014-HOME-Homeownership-Value-Limits.xlsx
#################################################################################################
#  Exploratory Data Analysis (EDA)
#  Exploratory analysis for time series mainly involves visualization with time series
#  plots, decomposition of the series into deterministic and stochastic parts, and
#  studying the dependency structure in the data. 
#  There has been comparatively little research on how to support the
#  more elaborate tasks typically associated with the exploratory visual
#  analysis of time-series, e.g., visualizing derived values, identifying
#  correlations, or identifying anomalies beyond obvious outliers. 
#        
#################################################################################################
# Interative = Dygraphs (http://www.htmlwidgets.org/showcase_dygraphs.html)
# http://walkerke.github.io/2014/12/dygraphs/
#################################################################################################
install.packages("dygraph")
library(dygraphs)

dygraph(msp.fl, main = "Fluvanna County Median Sales Prices") %>% 
  dyRangeSelector(dateWindow = c("1998-01-01", "2014-12-01"))

### Deflate ?????????????
#################################################################################################
# Looking at the time-series
plot(fl.ts)
#  Not surprisingly for monthly economic data, the series shows both seasonal
#  variation and a non-linear trend.
#  What happensif you deflate the series?
#################################################################################################
# is there any stochastic cyclic behavior? 
 plot(fl.ts[1:206], fl.ts[2:207], pch=20, col = c("blue","red")) 
 title("Scatterplot of Zillow Median Monthly Sales Price Data with Lag 1") 
# compute the value of the Pearson correlation coefficient:
  cor(fl.ts[1:206], fl.ts[2:207]) 
#  there seems to be homogeneity, in that there are not two disticnt groups over the time period
#  there is a positive correlation between successive measurements as one might expect in 
#  monthly house sales and the notion of "comps"
#  the series seems to always have the possibility of “reverting to the other side of the mean”,
#  a property which is common to stationary series
#################################################################################################
# Is thee serial correlation
lret.mspfl <- diff (log(fl.ts))
plot(lret.mspfl)
#  The SMI log-returns are a close approximation to the relative change (percent
#  values) with respect to the previous month.  There appears to be no dependency which could be
#  exploited to predict next months sales price based on the current month and/or previous months.
#  There does appear to be some volatility clustering 
#################################################################################################
# TREND ANALYSIS
# http://stats.stackexchange.com/questions/9506/stl-trend-of-time-series-using-r
#################################################################################################
### Zillow data
mod <- gamm(Y ~ s(month.of.year, bs = "cc") + s(time, bs = "cr"),
            data = df3, method = "REML",
            correlation = corAR1(form = ~ 1 | year),
            knots = list(month.of.year = c(0, 12)))

summary(mod$gam)
plot(mod$gam, pages = 1)
pred <- predict(mod$gam, newdata = df3, type = "terms")
ptemp <- attr(pred, "constant") + pred[,2]
plot(Y ~ time, data = df3, type = "l",
     xlab = "Obs",
     ylab = "Median Sales Price")
lines(ptemp ~ time, data = df3, col = "red", lwd = 2)
####  MLS data
##plot(Y.Median.Sales.Price  ~ date, data = df2, type = "l")
mod2 <- gamm(Y.Median.Sales.Price ~ s(month.of.year, bs = "cc") + s(time, bs = "cr"),
             data = df6, method = "REML",
             correlation = corAR1(form = ~ 1 | year),
             knots = list(month.of.year = c(0, 12)))

summary(mod2$gam)
plot(mod2$gam, pages = 1)
pred2 <- predict(mod2$gam, newdata = df6, type = "terms")
ptemp2 <- attr(pred2, "constant") + pred2[,2]
plot(Y.Median.Sales.Price ~ time, data = df6, type = "l",
     xlab = "Obs",
     ylab = "Median Sales Price")
lines(ptemp2 ~ time, data = df6, col = "red", lwd = 2)

## how much of a decline
tail(pred[,2], 1) - head(pred[,2], 1)

#################################################################################################
## Trend on entire series using STL
fit = stl(fl.ts , s.window="periodic")
plot(fit)

### By changing the smoothing parameter in fit2 with the t.window argument, we have put slightly more of
### the variation in the trend and less in the seasonal component
fit2 = stl(fl.ts, s.window="periodic", t.window=15)
plot(fit2)
#################################################################################################
#An R package for causal inference using Bayesian structural time-series models
# Before building the model, you can specify the pre and post-intervention periods.
# http://google.github.io/CausalImpact/CausalImpact.html
# http://static.googleusercontent.com/media/research.google.com/en/us/pubs/archive/41854.pdf
# https://groups.google.com/forum/#!forum/causalimpact
#################################################################################################
##### Use foreclosre sales percent as explnatory variable (Note only have VA not county)
forpct <- read.csv("http://files.zillowstatic.com/research/public/State/State_PctTransactionsThatArePreviouslyForeclosuredHomes_AllHomes.csv",stringsAsFactors = FALSE)
head(forpct)
forpct.va  <- subset(forpct,RegionName=='Virginia')
name <- forpct.va$RegionName
# transpose all but the first column (name)
tforpct.va  <- as.data.frame(t(forpct.va [,-1]))
length(tforpct.va)
colnames(tforpct.va) <- name
rownames(tforpct.va) <- NULL
sapply(tforpct.va ,class)
length(tforpct.va)
#################################################################################################
#  Use inventory as explnatory variable (ONLY HAVE 2010-FORWARD)
#################################################################################################

inven <- read.csv("http://files.zillowstatic.com/research/public/County/InventoryMeasure_County_Public.csv")
head(inven)
invenfl <- subset(inven,RegionName=='Fluvanna')
name <- invenfl$RegionName
invenfl$RegionName <- NULL
invenfl$StateFullName <- NULL
invenfl$MSA <- NULL
# transpose all but the first column (name)

tinvenfl  <- as.data.frame(t(invenfl [,-1]))
colnames(tinvenfl) <- name
rownames(tinvenfl) <- NULL

sapply(tinvenfl ,class)
tmsp.va$Fluvanna <- as.numeric(as.character(tmsp.va$Fluvanna))
length(tinvenfl)

#################################################################################################
#Use Vacancy Rates by state 1986-2013
#http://www.census.gov/housing/hvs/files/annual14/ann14t_4.xls
#################################################################################################
#library(gdata)
#download.file("http://www.census.gov/housing/hvs/files/annual14/ann14t_4.xls", destfile="file.xls")
#vacrate <- read.xls("file.xls", header=TRUE, pattern="Rank")

#  Read from google sheet
# ENABLE "Access for less secure apps" at
#https://www.google.com/settings/security/lesssecureapps
#install.packages("RGoogleDocs", repos = "http://www.omegahat.org/R", type="source")
#library(RGoogleDocs)
#gpasswd <- "XXXXXXXXXX"  #Fill in when needed
#gmail <- "flynnchessie@gmail.com"
#auth = getGoogleAuth(gmail, gpasswd)
#sheets.con = getGoogleDocsConnection(getGoogleAuth(gmail, gpasswd, service = "wise"))
#a = getDocs(sheets.con)
#names(a)
#ts = getWorksheets(a$VacancyRateVA, sheets.con)
#names(ts)
#vacrate <- as.data.frame(sheetAsMatrix(ts$Sheet1, header = TRUE, as.data.frame = TRUE, trim = TRUE))
#vacrate <- subset(vacrate,vacrate[1] > 1997)

#################################################################################################
time.points <- seq.Date(as.Date("1998-01-01"), by = "month", length.out = 207)
data <- zoo(cbind(flvec, tforpct.va$Virginia), time.points)
colnames(data)[2] = "forpct" 
head(data)
### Drop 2105 observations?
data2 <- window(data, start = as.Date("1998-01-01"), end = as.Date("2014-12-01"))
pre.period <- as.Date(c("1998-01-01", "2007-12-01"))
post.period <- as.Date(c("2008-01-01", "2014-12-01"))
### Are the Zillow data already seasonally adjusted?  See above comparison.
impact <- CausalImpact(data2, pre.period, post.period,model.args = list(nseasons = 12, season.duration = 1))
plot(impact)
summary(impact)
summary(impact, "report")
#################################################################################################
# Replace 2012-2014 with MLS Market report data
flvec2 <- flvec
flvec2[172:207] <- hmrall[[3]]
data3 <- zoo(cbind(flvec2, tforpct.va$Virginia), time.points)
colnames(data3)[2] = "forpct" 
colnames(data3)[1] = "flvec" 
head(data3)
data4 <- window(data3, start = as.Date("1998-01-01"), end = as.Date("2014-12-01"))
#### Compare the series for 2012-2015
data12 <- window(data2, start = as.Date("2012-01-01"), end = as.Date("2014-12-01"))
data14 <- window(data4, start = as.Date("2012-01-01"), end = as.Date("2014-12-01"))
plot(cbind(data12[,1],data14[,1]), screen = 1, col = c("red","black"))
###################################################

pre.period <- as.Date(c("1998-01-01", "2007-12-01"))
post.period <- as.Date(c("2008-01-01", "2014-12-01"))
### Are the Zillow data already seasonally adjusted?  See above comparison.
impact2 <- CausalImpact(data4, pre.period, post.period,model.args = list(nseasons = 12, season.duration = 1))
#plot(impact2)
#summary(impact2)
#summary(impact2, "report")

################################################################################################
# Use vacancy rate as explanatory variable


