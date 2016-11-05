# var googleTrends = require('google-trends-api')
# 
# var options = {
#   geo: 'country code or name',
#   date: 'yyyymm',
#   keywords: ['some', 'list', 'of', 'keywords'],
#   category: 'some category',
#   timePeriod: {
#     type: enumerated string 'hour', 'day', 'month', or 'year'
#     value: number
#   }
# }
# 
# googleTrends.apiMethod(options)
# .then(function(results){
#   console.log("Here are your google trend results!", results);
# })
# .catch(function(err){
#   console.log("there was an error :(", err);
# });

# devtools::install_github("PMassicotte/gtrendsR")

library(gtrendsR)
library(zoo)
library(xts)
library(tidyr)

usr <- "fabrizio.piasini@twonil.com"
psw <- "pantarei88"
gconnect(usr, psw) 

# start<- today() 
# 
# end <- today()
# 
# comp <- data.frame()
# 
# while(start > '2014-01-01') {
#   
#   start<- start - 90 
# 
#   lang_trend <- gtrends(c("Wix", 'Weebly', "Shopify", "GoDaddy", "WordPress"), geo = 'AU', start_date = start, end_date = end)
# 
#   comp_long <- lang_trend$trend[,which(names(lang_trend$trend) %in% c('start', 'location', 'keyword', 'hits'))]
# 
#   comp <- rbind(comp, spread(comp_long, key = 'keyword', value = 'hits') )
#   
#   end <- end - 90
#   
#   closeAllConnections()
#   
#   print(start)
# }

# plot(lang_trend)

lang_trend <- gtrends(query = c("Wix.com"), geo = 'AU', start_date = '2014-01-01')$trend

lang_trend <- rbind(lang_trend, gtrends(query = c("Weebly"), geo = 'AU', start_date = '2014-01-01')$trend )

lang_trend <- rbind(lang_trend, gtrends(query = c("Shopify"), geo = 'AU', start_date = '2014-01-01')$trend )

lang_trend <- rbind(lang_trend, gtrends(query = c("WordPress.com"), geo = 'AU', start_date = '2014-01-01')$trend )

lang_trend <- rbind(lang_trend, gtrends(query = c("GoDaddy"), geo = 'AU', start_date = '2014-01-01')$trend )


# append(x = lang_trend, gtrends(query = c("Weebly", "Shopify", "WordPress.com", ), geo = 'AU', start_date = '2014-01-01'))

head(lang_trend$trend[,2:4])
head(lang_trend$trend)

df.zoo <- read.zoo(file = lang_trend$trend)
