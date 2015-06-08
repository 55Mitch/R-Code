# install and load packages
pkgs <- c("RCurl", "XML", "stringr", "httr", "plyr", "ggplot2","rvest","R.utils",
          "zoo", "httr", "mgcv","gamair", "knitr", "markdown", "dplyr","downloader") 

# install.packages(pkgs)
lapply(pkgs, library, character.only=T)



#################################################################################################
# Scrape data from BLS webpages
#################################################################################################
year <- seq(1998,2014,1)
n <- length(year) 
flwage <- vector("list", length = n)

for (i in 1:n)
{
  
  # define the url 
  url  <- paste0("http://www.bls.gov/cew/data/files/",year[i],"/csv/",year[i],"_qtrly_by_area.zip")
 
  
  file <- paste0("flwage",year[i],".zip")
  
  # download the zip file and store in the "file" object 
  #download(url[[i]], file[[i]],mode="wb")
  
  download(url,file, mode="wb")
  
  
  fname <-  paste0(year[i],".q1-q4.by_area/",year[i],".q1-q4 51065 Fluvanna County, Virginia.csv")
  
  
  flwage[[i]] = read.csv(unzip (file, files= fname),header = TRUE) 
                               
  flwage[[i]] = flwage[[i]][1:4,18]
  
 
 
}


flwage14 =  as.data.frame(t(flwage[[n]]))
flwage13 =  as.data.frame(t(flwage[[n-1]]))
flwage12 =  as.data.frame(t(flwage[[n-2]]))
flwage11 =  as.data.frame(t(flwage[[n-3]]))
flwage10 =  as.data.frame(t(flwage[[n-4]]))
flwage09 =  as.data.frame(t(flwage[[n-5]]))
flwage08 =  as.data.frame(t(flwage[[n-6]]))
flwage07 =  as.data.frame(t(flwage[[n-7]]))
flwage06 =  as.data.frame(t(flwage[[n-8]]))
flwage05 =  as.data.frame(t(flwage[[n-9]]))
flwage04 =  as.data.frame(t(flwage[[n-10]]))
flwage03 =  as.data.frame(t(flwage[[n-11]]))
flwage02 =  as.data.frame(t(flwage[[n-12]]))
flwage01 =  as.data.frame(t(flwage[[n-13]]))
flwage00 =  as.data.frame(t(flwage[[n-14]]))
flwage99 =  as.data.frame(t(flwage[[n-15]]))
flwage98 =  as.data.frame(t(flwage[[n-16]]))

flwage.dat = rbind(t(flwage98),t(flwage99),t(flwage00),t(flwage01),t(flwage02),t(flwage03),t(flwage04),
                   t(flwage05),t(flwage06),t(flwage07),t(flwage08),t(flwage09),t(flwage10),
                   t(flwage11),t(flwage12),t(flwage13),t(flwage14))


flwage.ts = ts(flwage.dat, start = c(1998, 1), end = c(2014, 4), frequency = 4)

