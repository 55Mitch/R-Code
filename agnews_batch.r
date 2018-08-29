# loading packages #########################################################
if (!require("pacman")) install.packages("pacman")

pacman::p_load(XML,tm,tm.plugin.webmining,tm.plugin.sentiment,rvest,
               googlesheets,httr,dplyr)
############################################################################
### Google News sentiment

agheadlines = read_html("https://www.google.com/search?q=U.S.+agriculture&rlz=1C1CHBF_enUS782US782&biw=1920&bih=974&source=lnt&tbs=cdr%3A1%2Ccd_min%3A3%2F19%2F2018%2Ccd_max%3A5%2F21%2F2018&tbm=nws") %>%
  html_nodes(".r") %>% 
  html_text()
agheadlines
#write.csv(agheadlines, file = "agheadlines_7172018.csv",row.names=FALSE)
glimpse(agheadlines)



searchTerm = "U.S. Agriculture"
ag.yahoonews <- WebCorpus(YahooNewsSource(searchTerm))
#ag.yahoonews <- corpus.update(ag.yahoonews)
glimpse(ag.yahoonews)
############################################################################
cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}
###
#install_github("mannau/tm.plugin.sentiment")

agyahoonews.score = score(ag.yahoonews)
agnewscore = meta(agyahoonews.score)

####
#The important column to look at is “polarity”, with 0 being neutral sentiment,
#0.3333 being slightly positive, -0.3333 slightly negative, 1 being positive and
#-1 being negative. Polarity is determined by the difference between
#“pos_refs_per_ref” (positive sentiment reference) and “neg_refs_per_ref”
#(negative sentiment reference).
avgpolar = mean(agnewscore$polarity)
print(avgpolar)
############################################################################

## load a pre-existing token #####################
suppressMessages(gs_auth(token = "Gsuite_token.rds", verbose = FALSE))

## load sheet #####################
sentout =  gs_key ('11gYgEFv9e2kLIGrg8cGpesebNDpCx2mgdgyOy-4Yf0A')

sentout <- sentout %>% 
  gs_add_row(ws="Results",input = avgpolar)
  