#################################################################################################################
#  Wavelet analysis of the relationship between corn and oil prices
#################################################################################################################
pkgs <- c("plyr","rvest","XML","forecast", "ggplot2","EIAdata",
          "zoo", "RCurl","downloader", "Quandl", "biwavelet") 
#install.packages(pkgs)
lapply(pkgs, library, character.only=T)
# EIA key
key <- "4D23B300D12325CC0D958C40612FE6C3"
#################################################################################################################
# Time-series contrained to 1986 becuse of oil price
#################################################################################################################
## Get monthly corn prices from 
# http://future.aae.wisc.edu/data/monthly_values/by_area/2052?area=US&grid=true&tab=feed

source <- getURL("http://future.aae.wisc.edu/data/monthly_values/by_area/2052.csv?area=US&vector=1")
corn <- read.csv(text = source,skip=3,header=FALSE)
corn = corn[,-c(1,3)]
length(corn)
corn.ts <- ts(corn, start = c(1908, 1), end = c(2015, 4), frequency = 12)
plot(corn.ts)
corn.new <- window(corn.ts, start = c(1974,1), end = c(2015, 1))

## Get monthly soybean prices 
source1 <- getURL("http://future.aae.wisc.edu/data/monthly_values/by_area/2051.csv?area=US&vector=1")
soyb <- read.csv(text = source1,skip=3,header=FALSE)
soyb = soyb[,-c(1,3)]
length(soyb)
soyb.ts <- ts(soyb, start = c(1908, 1), end = c(2015, 4), frequency = 12)
plot(soyb.ts)
soyb.new <- window(soyb.ts, start = c(1986,1), end = c(2014, 12))

## Milk price
source2 <- getURL("http://future.aae.wisc.edu/data/monthly_values/by_area/10.csv?area=US&vector=1")
milk <- read.csv(text = source2,skip=3,header=FALSE)
milk = milk[,-c(1,3)]
length(milk)
milk.ts <- ts(milk, start = c(1908, 1), end = c(2015, 4), frequency = 12)
plot(milk.ts)
milk.new <- window(milk.ts, start = c(1986,1), end = c(2014, 12))

## Oil Prices
Quandl.auth("FMBHNU7sk11B3B5JBSSD")
oil = Quandl("FRED/DCOILWTICO", collapse= 'monthly', start_date="1986-01-01",end_date="2014-12-01",type="xts")
plot(oil)

## Fertilizer Prices
fert = Quandl("INDEXMUNDI/COMMODITY_DAPFERTILIZER", collapse= 'monthly', start_date="1986-01-01",end_date="2015-01-01",type="xts")
plot(fert)

## Get energy data from EIA
### U.S. Crude Oil and Natural Gas Active Well Service Rigs in operation, Monthly
### PET.E_ERTS0_XRS_NUS_C.M 
rigs = getEIA(ID = "PET.E_ERTS0_XRS_NUS_C.M", key = key)

#### U.S. Crude Oil Domestic Acquisition Cost by Refiners, Monthly
#### PET.R1200____3.M

dacr = getEIA(ID = "PET.R1200____3.M", key = key)
##########################################################
# wavelet analysis
##########################################################
# http://cran.r-project.org/web/packages/biwavelet/biwavelet.pdf

oilp = as.numeric(oil[,1])
cornp = as.numeric(corn.new)
fertp = as.numeric(fert[,1])

rigs = as.numeric(rigs[,1])
rigs = rigs[-c(1:12)]
rigs = rigs[-c(493:494)]
dacr = as.numeric(dacr[,1])
dacr = dacr[-c(493:498)]
# compute returns

oil.ret = diff(log(oilp)) * 100
corn.ret = diff(log(cornp)) * 100
fert.ret = diff(log(fertp)) * 100
lh = length(oil.ret)
dacr.ret = diff(log(dacr)) * 100
lr = length(dacr.ret)


tsdisplay(oil.ret)
tsdisplay(corn.ret)
tsdisplay(fert.ret)

tsdisplay(dacr.ret)

par(mfrow=c(1,1))

v1 = seq(as.Date("1974/1/1"), as.Date("2014/12/1"), "months")

# bind in time dimension
x1 = cbind(1:lr,corn.ret)
x2 = cbind(1:lh,oil.ret)
x3 = cbind(1:lh,fert.ret)

x4 = cbind(1:lr,dacr.ret)
x5 = cbind(1:lr,rigs)
# wavelet coherence
wtc.fx1oil = wtc(x1,x4, nrands=1000)
#wtc.fx3oil = wtc(x3,x2, nrands=1000)

wtc.fx5oil = wtc(x5,x4, nrands=1000)
## plot the results
par(oma=c(0, 0, 0, 1), mar=c(5, 4, 4, 5) + 0.1)
plot(wtc.fx5oil, plot.cb=TRUE,plot.phase=TRUE,
     ylim=c(2,32),main="Wavelet coherence of rigs and oil")

plot(wtc.fx1oil, plot.cb=TRUE,plot.phase=TRUE,
     ylim=c(2,32),main="Wavelet coherence of corn and oil")

#The stronger the relationship (coherence) between the two variables,
#the more red the colour. Both period and time are measured in months.
#The cone of influence is indicated by the dashed white line beyond
#which the estimates are unreliable. It is also, in some cases, possible
#to assign directionality to the relationship. This is done through the
#use of phase arrows. The phase arrows are interpreted as follows, where
#X is corn and Y is oil.

#The strongest coherence occurs on the time scale between months 70
#and 80 (1989-1990). Between8-16 months on the period scale,
#the phase arrows are mostly pointing up. This indicates that oil is
#leading corn. 

plot(wtc.fx3oil, plot.cb=TRUE,plot.phase=TRUE,
     ylim=c(2,32),main="Wavelet coherence of Fertilizer and oil")


##################################################################
# Plot partial wavelet coherence and phase difference 

li = length(oilp)
y  = cbind(1:li,oilp)
z1 = cbind(1:li,cornp)
z2 = cbind(1:li,fertp)

pwtc.yz1=pwtc(y, z1, z2, nrands=0)
pwtc.yz2=pwtc(y, z2, z1, nrands=0)

par(mfrow=c(2,1), oma=c(4, 0, 0, 1), mar=c(1, 4, 4, 5), mgp = c(1.5, 0.5, 0))
plot(pwtc.yz1, xlab="", plot.cb=TRUE, main="Partial wavelet coherence of y and z1 | z2")
plot(pwtc.yz2, plot.cb=TRUE, main="Partial wavelet coherence of y and z2 | z1")

##################################################################
# Compute dissimilarity between multiple wavelet spectra
li = length(oilp)
m1 = cbind(1:li,cornp)
m2 = cbind(1:li,oilp)
m3 = cbind(1:li,fertp)

## Compute wavelet spectra
wt.t1=wt(m1)
wt.t2=wt(m2)
wt.t3=wt(m3)

w.arr=array(NA, dim=c(3, NROW(wt.t1$wave), NCOL(wt.t1$wave)))
w.arr[1, , ]=wt.t1$wave
w.arr[2, , ]=wt.t2$wave
w.arr[3, , ]=wt.t3$wave
## Compute dissimilarity and distance matrices
w.arr.dis=wclust(w.arr)
plot(hclust(w.arr.dis$dist.mat, method="ward"), sub="", main="",
     ylab="Dissimilarity", hang=-1)
