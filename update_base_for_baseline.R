library(MASS)
library(DAAG)
library(tidyr)
library(lubridate)

setwd("C:\\Users\\Fabrizio Piasini\\Google Drive\\wix_au\\expl")


df <- create_data_frame(kpi_file = "wix_au_kpi.csv", TV_file = "wix_au_tv_new.csv", other_marketing_file = 'wix_au_marketing.csv')

old <- read.csv(file = 'base.csv', stringsAsFactors = F)

write.csv(file = paste0(today(),"base.csv"), x = old, row.names = F)


write.csv(file = 'base.csv',df[,c('date', 'Mon', 'Tues', 'Wed', 'Thur', 'Fri', 'Sat', 'winter_holidays', 'ext_comps_day', 'bs_comp2')], row.names = F)


base = read.csv("base.csv",stringsAsFactors = F)
names(base)
head(base)
# base$date = as.Date(base$date,"%m/%d/%Y")
base$date = as.Date(base$date)
library(RMySQL)
for (i in 1:3) {
  db_ms = dbConnect(MySQL(), user = "fabrizio.piasini", password = "bksHL4SZ09dV", dbname="ms", host='bvs.db.twonil.com', port=3306)
  if (is.null(db_ms)) {db_ms=dbConnect(MySQL(), user = "fabrizio.piasini", password = "bksHL4SZ09dV", dbname="ms", host='bvs.db.twonil.com', port=3306)}
  else break
}

paste(names(base)[2:length(base)],"='double(20,10)',",collapse="")
dbWriteTable(db_ms,name="base_wix_au_14", value=base, 
             field.types=list(date ='date', Mon ='double(20,10)',Tues ='double(20,10)',Wed ='double(20,10)',
                              Thur ='double(20,10)',Fri ='double(20,10)',Sat ='double(20,10)',winter_holidays ='double(20,10)',
                              ext_comps_day ='double(20,10)',bs_comp2 ='double(20,10)'),
             row.names=FALSE, overwrite=TRUE)

