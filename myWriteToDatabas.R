


# write base into database
# setwd("~/Projects_Fang/Athena/Blue Apron/yilan")
setwd("C:\\Users\\Fabrizio Piasini\\Google Drive\\wix_au\\expl")

base = read.csv("base.csv",stringsAsFactors = F)
names(base)
head(base)
base$date = as.Date(base$date,"%m/%d/%Y")
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

?dbWriteTable

map = read.csv("map_1026.csv",stringsAsFactors = F)
names(map)
tail(map)
library(RMySQL)
for (i in 1:3) {
  db_ms = dbConnect(MySQL(), user = "fabrizio.piasini", password = "bksHL4SZ09dV", dbname="ms", host='bvs.db.twonil.com', port=3306)
  if (is.null(db_ms)) {db_ms=dbConnect(MySQL(), user = "fabrizio.piasini", password = "bksHL4SZ09dV", dbname="ms", host='bvs.db.twonil.com', port=3306)}
  else break
}

dbWriteTable(db_ms, name = "map",
             value = map, 
             field.types=list(model_id = 'int(11)',Client='varchar(20)',Variable = 'varchar(50)', Interaction = 'text',
                              Group = 'varchar(50)',Decay_Rate='double(20,10)',Diminishing_Return='double(20,10)',Log = 'int(1)',Coef='double(20,10)'),
             row.names=FALSE, overwrite=TRUE)

