#Read csv file from Zillow on median sales price by county
msp <- read.csv(url("http://files.zillowstatic.com/research/public/County/County_MedianSoldPrice_AllHomes.csv"))
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