# install and load packages
pkgs <- c("RCurl", "XML", "stringr", "httr", "plyr", "ggplot2","rvest",
          "zoo", "httr" )


# install.packages(pkgs)
lapply(pkgs, library, character.only=T)

install.packages("forecast")
library(forecast)

#################################################################################################
# Scrape data from webpages
# Use rvest
# http://www.charlottesvillerealestatebuzz.com/market-report/fluvanna-county-va-real-estate-market-report-june-2013/
#################################################################################################
### Create loop for monthly reports by year
year <- seq(2013,2014,1)
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
  result[[i]] <- result[[i]][c(13:17)]
  # clean up numbers
  result[[i]] <- str_replace_all( result[[i]] , "\\$", "")
  result[[i]] <- str_replace_all( result[[i]] , "%", "")
  result[[i]] <- str_replace_all( result[[i]] , ",", "")
  
  result[[i]] <- as.data.frame(do.call(rbind, strsplit(result[[i]], ": ")))
  result[[i]][,2] <- as.numeric(as.character(result[[i]][,2] )) 
  result[[i]] <- setNames(result[[i]], c("item","value"))
}
  
  
 
total[[j]]  <- cbind(result[[1]],result[[2]]$value,result[[3]]$value,result[[4]]$value,result[[5]]$value,
            result[[6]]$value,result[[7]]$value,result[[8]]$value,result[[9]]$value,result[[10]]$value,
            result[[11]]$value,result[[12]]$value )

}
# transpose all but the first column (item)
hmr13 <- as.data.frame(t(total[[1]][,-1]))
colnames(hmr13) <- total[[1]]$item
rownames(hmr13) <- NULL
head(hmr13)
sapply(hmr13 ,class)
hmr13.ts <- ts(hmr13, start = c(2013, 1), end = c(2013, 12), frequency = 12)

hmr14 <- as.data.frame(t(total[[2]][,-1]))
colnames(hmr14) <- total[[2]]$item
rownames(hmr14) <- NULL
head(hmr14)
sapply(hmr14 ,class)
hmr14.ts <- ts(hmr14, start = c(2014, 1), end = c(2014, 12), frequency = 12)

#################################################################################################
#  COMPARE WITH ZILLOW DATA
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
tmsp.va$period = seq(as.Date("1996/4/1"), as.Date("2015/3/1"),"month")

msp.fluvan <- tmsp.va[1:226,c("period","Fluvanna")]
msp.fl <- tmsp.va[1:226,"Fluvanna"]
## create time series object
fl.ts <- tsclean(msp.fl)
fl.ts <- ts(fl.ts, start = c(1996, 4), end = c(2015, 1), frequency = 12)
################################################################################################
pdf("Compare1.pdf",width=10,height=8)
ts.plot(fl.ts,hmr14.ts[,3],gpars = list(col = c("black", "red")))
dev.off()
