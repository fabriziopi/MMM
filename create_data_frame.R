create_data_frame = function (kpi_file, TV_file ,other_marketing_file)

{
  
  require(lubridate)
  library(DBI)
  library(RMySQL)
  
  library(gtrendsR)
  library(zoo)
  library(xts)
  
  LM <<- T
  ROBUST <<- F
  GLS <<- F
  LASSO <<- F
  STEP <<- F
  
  COMP_NEW <<- T
  BRAND_NEW <<- T
  
  # setwd("C:\\Users\\Fabrizio Piasini\\Desktop\\wix_au\\expl")
  setwd("C:\\Users\\Fabrizio Piasini\\Google Drive\\wix_au\\expl")
  # 
  #option 1. database
  client = "wix"
  myQueryForKPI = paste0(readLines("kpi_query.txt"),collapse = " ")
  myQueryForMarketing = paste0(readLines("marketing_query.txt"),collapse = " ")
  myQueryForTV = paste0(readLines("tv_query.txt"),collapse = " ")
  #
  #disconnect existing
  all.cons = dbListConnections(MySQL())
  for (con in all.cons)
    dbDisconnect(con)

  # connect to the database to get the data
  db_client = dbConnect(MySQL(), user = "fabrizio.piasini", password = "bksHL4SZ09dV", dbname="", host=paste(client,'.db.twonil.com',sep=""), port=3306)
  kpi = dbSendQuery(db_client, myQueryForKPI) #takes some time for this one

  db_client = dbConnect(MySQL(), user = "fabrizio.piasini", password = "bksHL4SZ09dV", dbname="", host=paste(client,'.db.twonil.com',sep=""), port=3306)
  marketing = dbSendQuery(db_client, myQueryForMarketing)

  db_bvs = dbConnect(MySQL(), user = "fabrizio.piasini", password = "bksHL4SZ09dV", dbname='', host='bvs.db.twonil.com', port=3306)
  TV = dbSendQuery(db_bvs, myQueryForTV)

  kpi = fetch(kpi, n=-1)

  marketing_file = read.csv(other_marketing_file,stringsAsFactors = F)
  colnames(marketing_file)[1] <- 'date'
  marketing_file$date <- as.Date(marketing_file$date, format = "%m/%d/%Y")


  
  ###CSV data pull
  
  # TV = read.csv(TV_file,stringsAsFactors = F)
  # TV$date <- as.Date(TV$date, "%m/%d/%Y")
  # kpi = read.csv(kpi_file,stringsAsFactors = F)
  # marketing = read.csv(other_marketing_file,stringsAsFactors = F)
  # colnames(marketing)[1] <- 'date'
  # marketing$date <- as.Date(marketing$date, format = "%m/%d/%Y")

  colnames(kpi)[1] = "date"
  colnames(kpi)[2] = "kpi"
  # kpi$date=as.Date(kpi$date, format="%m/%d/%Y", tz = 'UTC')
  # colnames(TV)[1] = "date"
  
  
  kpi$date=as.Date(kpi$date)

  # kpi$date=as.Date(as.POSIXct(kpi$date, tz = 'UTC'))
  # kpi$date <- mdy(kpi$date)

  #please note that sometime Historical TV might change in the database
  TV = fetch(TV, n=-1)
  colnames(TV)[1] = "date"
  # TV$date=as.Date(TV$date, format="%m/%d/%Y", tz = 'UTC')
  TV$date=as.Date(TV$date)


  marketing = fetch(marketing, n=-1)
  colnames(marketing)[1] = "date"
  # marketing$date=as.Date(marketing$date, format="%m/%d/%Y", tz = 'UTC')
  marketing$date=as.Date(marketing$date)

  start_yt_test <- as.Date('2015-03-13')
  i <- start_yt_test

  while (i <= as.Date('2015-04-01')) {

    marketing$YouTube_Branding_spend[which(marketing$date == i)] <- marketing_file$YouTube_Branding_spend[which(marketing_file$date == i)]
    marketing$YouTube_Branding_imps[which(marketing$date == i)] <- marketing_file$YouTube_Branding_imps[which(marketing_file$date == i)]

    i <- i + 1
  }

  spend <<- merge(marketing, TV, by = "date", all.x = TRUE)
  
  spend[is.na(spend)] <- 0
  
  
  
  write.csv(spend, "spending.csv", row.names = FALSE)
  
  spend <<- spend[which(spend$date == '2014-01-01'):nrow(spend),]
  
  
  data = merge(kpi, marketing, by = "date", all.x = TRUE)
  data = merge(data, TV, by = "date", all.x = TRUE)
  
  # 
  # seasonality = read.csv("seasonality.csv",stringsAsFactors = F)
  # seasonality$date = as.Date(seasonality$date,"%m/%d/%Y")
  # 
  # data = merge(data, seasonality, by = "date", all.x = TRUE)
  data$TV3060_spend <- rowSums(cbind(data$TV30_spend, data$TV60_spend))
  data$TV3060_imp <- rowSums(cbind(data$TV30_imp, data$TV60_imp))
  
  data$TV_imp <- rowSums(cbind(data$TV15_imp, data$TV3060_imp))
  
  
  data[is.na(data)] = 0
  
  # data$trend <- 1:nrow(data)
  
  # # option 2. flat file
  # data = read.csv("data.csv",stringsAsFactors = F)
  # data$date = as.Date(data$date,format = "%m/%d/%Y")
  
  data$weekday = wday(data$date)
  data$Sun = ifelse(data$weekday==1,1,0)
  data$Mon = ifelse(data$weekday==2,1,0)
  data$Tues = ifelse(data$weekday==3,1,0)
  data$Wed = ifelse(data$weekday==4,1,0)
  data$Thur = ifelse(data$weekday==5,1,0)
  data$Fri = ifelse(data$weekday==6,1,0)
  data$Sat = ifelse(data$weekday==7,1,0)
  
  data$MonTue <- ifelse(data$weekday %in% c(2,3),1,0)
  
  # data$y2015 <- ifelse(year(data$date) == 2015,1,0)
  data$y2016 <- ifelse(year(data$date) == 2016,1,0)
  
  
  # data$workday = rowSums(data[,which(colnames(data) == 'Mon'):which(colnames(data) == 'Fri')])
  # data$y2015 = ifelse(year(data$date)>=2015,1,0)
  data$Mon2016 = data$Mon*data$y2016
  data$Tues2016 = data$Tues*data$y2016
  data$Wed2016 = data$Wed*data$y2016
  data$Thur2016 = data$Thur*data$y2016
  data$Fri2016 = data$Fri*data$y2016
  data$Sat2016 = data$Sat*data$y2016
  data$weekday <- NULL
  
  data$month1 <- ifelse(month(data$date) == 1,1,0)
  data$month2 <- ifelse(month(data$date) == 2,1,0)
  data$month3 <- ifelse(month(data$date) == 3,1,0)
  data$month4 <- ifelse(month(data$date) == 4,1,0)
  data$month5 <- ifelse(month(data$date) == 5,1,0)
  data$month6 <- ifelse(month(data$date) == 6,1,0)
  data$month7 <- ifelse(month(data$date) == 7,1,0)
  data$month8 <- ifelse(month(data$date) == 8,1,0)
  data$month9 <- ifelse(month(data$date) == 9,1,0)
  data$month10 <- ifelse(month(data$date) == 10,1,0)
  data$month11 <- ifelse(month(data$date) == 11,1,0)
  data$month12 <- ifelse(month(data$date) == 12,1,0)
  
  data$summer <-ifelse(month(data$date) %in% c(6,7,8),1,0)
  data$autumn <- ifelse(month(data$date) %in% c(9,10,11),1,0)
  
  # data$seasonality <- (0.41448*data$seasonality_2011 + (1-0.41448)*data$seasonality_2012)
  
  
  
  
  # data$FB_Imps2014 <- ifelse(year(data$date) == 2014, data$FB_Imps,0)
  # data$USEM_spend2016 <- ifelse(year(data$date) == 2016, data$USEM_spend,0)
  
  
  # data$HarmonyDay = ifelse( format(data$date, "%m-%d") == "03-22" ,1,0)
  # data$HarmonyDay2015 = ifelse( format(data$date, "%m-%d-%Y") %in% c("03-21-2015","03-22-2015","03-23-2015") ,1,0)
  # data$AustraliaDay = ifelse( format(data$date, "%m-%d") %in% c("01-27","01-26") ,1,0)
  # data$LaborDay = ifelse( format(data$date, "%m-%d") %in% c("10-03"),1,0)
  # data$MelbourneCupDay = ifelse( format(data$date, "%m-%d") %in% c("11-01"),1,0)
  
  
  # data$holgroup = ifelse( format(data$date, "%m-%d") %in% (c("04-21","04-06","01-27","01-26","01-01")),1,0)
  # data$MotherDay = ifelse( format(data$date, "%m-%d") %in% (c("05-11","05-12")),1,0)
  # data$QueenBDay = ifelse( format(data$date, "%m-%d") %in% (c("09-28","09-29","09-30")),1,0)
  data$NewYear = ifelse( format(data$date, "%m-%d") %in% c("01-02", "01-01", "12-31", "12-30","12-29","12-28") ,1,0)
  data$ChristmasDay = ifelse( format(data$date, "%m-%d") %in% c("12-25","12-24") ,1,0)
  # data$GoodFridayDay = ifelse( format(data$date, "%m-%d") %in% c("03-25") ,1,0)
  # data$EasterDay = ifelse( format(data$date, "%m-%d") %in% c("03-27", "03-28"),1,0)
  
  data$winter_holidays <- ifelse(format(data$date, "%m-%d") %in% c("12-24","12-25","12-26","12-27","12-28","12-29","12-30",
                                                                   "12-31","01-01") > 0,1,0)
  # data$startSchoolHoliday = ifelse( format(data$date, "%m-%d") %in% c("03-24", "04-01", "04-08", "04-15", "07-01", "07-08",
  #                                                                     "07-25", "09-16", "09-23", "09-30", "12-03", "12-15", "12-16", "12-20", "12-21") ,1,0)
  # 
  # data$endSchoolHoliday = ifelse( format(data$date, "%m-%d") %in% c("01-22", "01-25", "02-01", "02-03", "02-08", "02-10",
  #                                                                 # "04-11", "04-26", "04-27", "05-02", "07-11", "07-18", "07-19", "10-04", "10-10",
  #                                                                  "10-17"),1,0)
  
  # data$FatherDay <- ifelse( format(data$date, "%m-%d") %in% c("09-04") ,1,0)
  # data$ValentineDay <- ifelse( format(data$date, "%m-%d") %in% c("02-14") ,1,0)
  # data$StPatrickDay <- ifelse( format(data$date, "%m-%d") %in% c("03-17") ,1,0)
  # data$AnzackDay <- ifelse( format(data$date, "%m-%d") %in% c("04-25") ,1,0)
  # 
  
  
  # data$issueUSEM <- ifelse( format(data$date, "%Y-%m-%d") %in% c("2016-02-18") ,1,0)
  # data$issueBS_sp <- ifelse( format(data$date, "%Y-%m-%d") %in% c("2016-02-24") ,1,0)
  # data$issueBS_imps <- ifelse( format(data$date, "%Y-%m-%d") %in% c("2016-03-15","2016-03-16") ,1,0)
  # data$issueGDN <- ifelse( format(data$date, "%Y-%m-%d") %in% c("2015-11-19") ,1,0)
  
  data$YouTube_All_Imps <- data$YouTube_Branding_imps +data$YouTube_Performance_imps
  data$Youtube_test_Imps <- ifelse(format(data$date, "%Y-%m") %in% c("2015-03"),(data$YouTube_Branding_imps + data$YouTube_Performance_imps),0)
  
  
  # data$Outliers = ifelse(data$date %in% as.Date(c("2015-09-21","2015-09-22","2015-09-23")),1,0)
  # data$issue = ifelse(data$date %in% as.Date(c('2016-01-11','2016-01-12','2016-01-13','2016-01-14','2016-01-15','2016-01-18','2016-01-19','2016-01-20','2016-01-21','2016-01-24','2016-01-25','2016-01-26','2016-01-27','2016-01-28')),1,0)
  
  #competitors trend
  
  if(COMP_NEW) {
    
    ### get GTrends by CSV
    # comp <- read.csv("comp_wix_au.csv", stringsAsFactors = F)
    # colnames(comp)[1] <- "date"
    # comp$date <- as.Date(comp$date, format = "%m/%d/%Y")
    
    ### get GTrends with query
    
    usr <- "fabrizio.piasini@twonil.com"
    psw <- "pantarei88"
    gconnect(usr, psw)

    # lang_trend <- gtrends(query = c("Wix.com"), geo = 'AU', start_date = '2014-01-01')$trend
    #
    # lang_trend <- rbind(lang_trend, gtrends(query = c("Weebly"), geo = 'AU', start_date = '2014-01-01')$trend )
    #
    # lang_trend <- rbind(lang_trend, gtrends(query = c("Shopify"), geo = 'AU', start_date = '2014-01-01')$trend )
    #
    # lang_trend <- rbind(lang_trend, gtrends(query = c("WordPress.com"), geo = 'AU', start_date = '2014-01-01')$trend )
    #
    # lang_trend <- rbind(lang_trend, gtrends(query = c("GoDaddy"), geo = 'AU', start_date = '2014-01-01')$trend )

    lang_trend <- gtrends(query = c("Wix.com"), geo = 'AU')$trend

    lang_trend <- rbind(lang_trend, gtrends(query = c("Weebly"), geo = 'AU')$trend )

    lang_trend <- rbind(lang_trend, gtrends(query = c("Shopify"), geo = 'AU')$trend )

    lang_trend <- rbind(lang_trend, gtrends(query = c("WordPress.com"), geo = 'AU')$trend )

    lang_trend <- rbind(lang_trend, gtrends(query = c("GoDaddy"), geo = 'AU')$trend )

    comp_long <- lang_trend[,2:4]

    comp <- spread(comp_long, key = 'keyword', value = 'hits')
    
    
    
    
    colnames(comp) <- tolower(colnames(comp))
    
    
    # comp[,-1] <- comp[,-1] / 7 
    
    # comp$date <- as.Date(comp$date, format = "%m/%d/%Y")
    colnames(comp)[1] <- "date"
    comp$date <- as.Date(comp$date)
    
    fit.princomp <- princomp(comp[,2:ncol(comp)], cor = TRUE)
    
    summary(fit.princomp)
    
    loadings(fit.princomp) # pc loadings
    #plot(fit.princomp,type="lines") # scree plot 
    #fit.princomp$scores # the principal components
    #biplot(fit.princomp)
    
    
    comp_PCA1 <- data.frame(fit.princomp$scores[,1])
    comp_PCA2 <- data.frame(fit.princomp$scores[,2])
    comp_PCA3 <- data.frame(fit.princomp$scores[,3])
    comp_PCA4 <- data.frame(fit.princomp$scores[,4])
    comp_PCA5 <- data.frame(fit.princomp$scores[,5])
    
    
    comps <- cbind(comp$date, comp_PCA1, comp_PCA2, comp_PCA3, comp_PCA4, comp_PCA5)
    
    colnames(comps)<- c("dates","pc1", "pc2", "pc3", "pc4", "pc5")
    
    complete_comps <- data.frame()
    
    start_day <- min(comp$date) - 6
    end_day <- max(comp$date)
    
    ext_comps_day <- vector('numeric')
    ext_comps_day2 <- vector('numeric')
    ext_comps_day3 <- vector('numeric')
    ext_comps_day4 <- vector('numeric')
    ext_comps_day5 <- vector('numeric')
    
    
    for (n in 1:nrow(comps)) {
      
      whole_week_values <- rep(comps$pc1[n],7)
      ext_comps_day <- c(ext_comps_day, whole_week_values)
      
      whole_week_values <- rep(comps$pc2[n],7)
      ext_comps_day2 <- c(ext_comps_day2, whole_week_values)
      
      whole_week_values <- rep(comps$pc3[n],7)
      ext_comps_day3 <- c(ext_comps_day3, whole_week_values)
      
      whole_week_values <- rep(comps$pc4[n],7)
      ext_comps_day4 <- c(ext_comps_day4, whole_week_values)
      
      whole_week_values <- rep(comps$pc5[n],7)
      ext_comps_day5 <- c(ext_comps_day5, whole_week_values)
      
    }
    
    calendar <- data.frame(date = seq(start_day, end_day, 1))
    
    ext_comps_day <- data.frame(ext_comps_day)
    
  
    
    complete_comps <- cbind(calendar, ext_comps_day, ext_comps_day2, ext_comps_day3, ext_comps_day4, ext_comps_day5)
    
    
    comp_PCA <- cbind(comp_PCA1, comp_PCA2)
    
    #comp_PCA <- cbind(comp_PCA, date = as.Date(comp$, format = "%m/%d/%Y") )
    
    
    write.csv(comp_PCA, "WIX_AU_comps.csv", row.names = F)
    
  }
  
  
  seasonality.df <- read.csv('seasonality_wixau.csv')
  names(seasonality.df) <- c('date', 'seasonality')
  
  seasonality.df$date <- as.Date(seasonality.df$date, format = "%m/%d/%Y")
  
  seasonality.df <- as.data.frame(seasonality.df)
  
  data <- merge(data, complete_comps, by='date', all.x = TRUE )
  
  data <- merge(data, seasonality.df, by='date', all.x = TRUE )
  
  data$TVsyn <- data$TV3060_imp*data$seasonality
  
  data$TV15syn <- data$TV15_imp*data$seasonality
  
  
  
  for (i in 1:7) {
    
    if(is.na(data$ext_comps_day[nrow(data)-i+1])) {
      
      data$ext_comps_day[nrow(data)-i+1] <- data$ext_comps_day[nrow(data) - 7]
    }
    
  }
  
  # data <- data[1:min(which(is.na(data$ext_comps_day))) -1 ,]
  
  # 
  # if(BRAND_NEW){
  #   
  #   brand <- read.csv("brand.csv", stringsAsFactors = F)
  #   
  #   brand$date <- mdy(brand$X)
  #   brand$X <- NULL
  #   
  # }
  
  
  # data <- merge(data, brand, by='date', all.x = TRUE )
  data[is.na(data)] = 0
  
  fit.princomp <- princomp( dplyr::select(data, BS_Imps, BS_spend), cor = TRUE)
  
  bs_comp1 <- data.frame(bs_comp1 = fit.princomp$scores[,1])*(-1)

  bs_comp2 <- data.frame(bs_comp2 = fit.princomp$scores[,2])

  data <- cbind(data, bs_comp1, bs_comp2 )
  # 
  write.csv(data,"wix_au_clean_data.csv")
  # 
  # # z <- as.matrix(data[,c(3, 7, 9, 12, 14, 17, 25)])
  # # z <- as.matrix(data[,c(17, 25)])
  # z <- as.matrix(data[,c(26)])
  z <- as.matrix(data[,which(names(data) == 'TV_imp')])
  # 
  P <- (diag(dim(z)[1]) - z%*%ginv(t(z)%*%z)%*%t(z) )
  # 
  # # x <- as.matrix(data$ext_comps_day)
  # # 
  # data$ext_comps_day <- P%*%as.matrix(data$ext_comps_day)
  data$ext_comps_day2 <- P%*%as.matrix(data$ext_comps_day2)
  data$ext_comps_day3 <- P%*%as.matrix(data$ext_comps_day3)
  data$ext_comps_day4 <- P%*%as.matrix(data$ext_comps_day4)
  data$ext_comps_day5 <- P%*%as.matrix(data$ext_comps_day5)
  # 
  # 
  # data$ext_comps_day <- rollmean(data$ext_comps_day, 7, fill = 'extend')
  # 
  data$bs_comp1 <- P%*%as.matrix(data$bs_comp1)
  # 
  # data$bs_comp2 <- P%*%as.matrix(data$bs_comp2)
  # 
  # # df$u2 <- P %*% as.matrix(df$USEM_spend)
  # 
  # if (any(is.na(data$ext_comps_day))){
  # data <- data[1:min(which(is.na(data$ext_comps_day))) -1 ,]
  # }

  # data <- data[1:which(data$date == '2015-12-13'), ]


  # data <- data[1:which(df$date == end_day), ]
  # data <- data[1:which(data$date == '2016-10-22'), ]
  
  data[is.na(data)] = 0
  
  if(mean(data$ext_comps_day) > 0) {
    data$ext_comps_day <- data$ext_comps_day * (-1)
  }
  
  if((data$bs_comp2[1]) > 0) {
    data$bs_comp2 <- data$bs_comp2 * (-1)
  }
  
  write.csv(data,"wix_au_filtered_data.csv")
  
  
  # data$
  
  return(data)
  
  
}
