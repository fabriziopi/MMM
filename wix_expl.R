library(lubridate) # date format
library(car) # vif
library(lmtest) # dwtest bptest
library(DEoptim) #optimization
library(googleVis) #visualization
library(RMySQL) #connect to mySQL database
library(psych)
library(devtools)
library(broom)
library(ggplot2)
library(reshape)
library(xts)
library(stats)
library(lmtest)
library(lasso2)
library(sandwich)
library(MASS)


LM <<- F
ROBUST <<- T
GLS <<- F
LASSO <<- F
STEP <<- F

COMP_NEW <<- T
BRAND_NEW <<- T

ramp = function(date, l, x0, alpha, beta) {
  
}


adstock = function(data,decay.rate){
  a = stats::filter(data,(1-decay.rate),method="recursive",side = 1)
  return(a)
}

mape <- function(v1,v2){ # mean absolute percentage error
  return(mean(abs(v1-v2)/v1))
} # 10%

mae <- function(v1,v2){ # mean absolute percentage error
  return(mean(abs(v1-v2)))
} # 10%

smape = function(v1,v2) {
  return(mean(abs(v1-v2)/(0.5*(abs(v1)+abs(v2)))))
}


# setwd("C:\\Users\\Fabrizio Piasini\\Desktop\\wix_au\\expl")
setwd("C:\\Users\\Fabrizio Piasini\\Google Drive\\wix_au\\expl")
# 
# #option 1. database
# client = "wix"
# myQueryForKPI = paste0(readLines("KPI.txt"),collapse = " ")
# myQueryForMarketing = paste0(readLines("Marketing_Investment.txt"),collapse = " ")
# myQueryForTV = paste0(readLines("TV.txt"),collapse = " ")
# 
# #disconnect existing 
# all.cons = dbListConnections(MySQL())
# for (con in all.cons)
#   dbDisconnect(con)
# 
# #connect to the database to get the data
# db_client = dbConnect(MySQL(), user = "samantha", password = "t9UA1et1Em7K", dbname="", host=paste(client,'.db.twonil.com',sep=""), port=3306)
# kpi = dbSendQuery(db_client, myQueryForKPI) #takes some time for this one
# 
# db_client = dbConnect(My?SQL(), user = "samantha", password = "t9UA1et1Em7K", dbname="", host=paste(client,'.db.twonil.com',sep=""), port=3306)
# marketing = dbSendQuery(db_client, myQueryForMarketing)
# 
# db_bvs = dbConnect(MySQL(), user = "samantha", password = "t9UA1et1Em7K", dbname='', host='bvs.db.twonil.com', port=3306)
# TV = dbSendQuery(db_bvs, myQueryForTV)

#kpi = fetch(kpi, n=-1)


TV = read.csv("wix_au_tv_new.csv",stringsAsFactors = F)
kpi = read.csv("wix_au_kpi_new.csv",stringsAsFactors = F)
marketing = read.csv("wix_au_marketing.csv",stringsAsFactors = F)

colnames(kpi)[1] = "date"
colnames(kpi)[2] = "kpi"
# kpi$date=as.Date(kpi$date, format="%m/%d/%Y", tz = 'UTC')
# kpi$date=as.Date(as.POSIXct(kpi$date, tz = 'UTC'))
kpi$date <- mdy(kpi$date)


#please note that sometime Historical TV might change in the database
#TV = fetch(TV, n=-1)
colnames(TV)[1] = "date"
TV$date=as.Date(TV$date, format="%m/%d/%Y", tz = 'UTC')

#marketing = fetch(marketing, n=-1)
colnames(marketing)[1] = "date"
marketing$date=as.Date(marketing$date, format="%m/%d/%Y", tz = 'UTC')

spend <- merge(marketing, TV, by = "date", all.x = TRUE)

spend[is.na(spend)] <- 0

write.csv(spend, "spending.csv", row.names = FALSE)

data = merge(kpi, marketing, by = "date", all.x = TRUE)
data = merge(data, TV, by = "date", all.x = TRUE)

# 
# seasonality = read.csv("seasonality.csv",stringsAsFactors = F)
# seasonality$date = as.Date(seasonality$date,"%m/%d/%Y")

# data = merge(data, seasonality, by = "date", all.x = TRUE)
# data$TV3060_spend <- rowSums(cbind(data$TV30_spend, data$TV60_spend))

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




data$FB_Imps2014 <- ifelse(year(data$date) == 2014, data$FB_Imps,0)
data$USEM_spend2016 <- ifelse(year(data$date) == 2016, data$USEM_spend,0)


data$HarmonyDay = ifelse( format(data$date, "%m-%d") == "03-22" ,1,0)
data$HarmonyDay2015 = ifelse( format(data$date, "%m-%d-%Y") %in% c("03-21-2015","03-22-2015","03-23-2015") ,1,0)
data$AustraliaDay = ifelse( format(data$date, "%m-%d") %in% c("01-27","01-26") ,1,0)
data$LaborDay = ifelse( format(data$date, "%m-%d") %in% c("10-03"),1,0)
data$MelbourneCupDay = ifelse( format(data$date, "%m-%d") %in% c("11-01"),1,0)


# data$holgroup = ifelse( format(data$date, "%m-%d") %in% (c("04-21","04-06","01-27","01-26","01-01")),1,0)
data$MotherDay = ifelse( format(data$date, "%m-%d") %in% (c("05-11","05-12")),1,0)
data$QueenBDay = ifelse( format(data$date, "%m-%d") %in% (c("09-28","09-29","09-30")),1,0)
data$NewYear = ifelse( format(data$date, "%m-%d") %in% c("01-02", "01-01", "12-31", "12-30","12-29","12-28") ,1,0)
data$ChristmasDay = ifelse( format(data$date, "%m-%d") %in% c("12-25","12-24") ,1,0)
data$GoodFridayDay = ifelse( format(data$date, "%m-%d") %in% c("03-25") ,1,0)
data$EasterDay = ifelse( format(data$date, "%m-%d") %in% c("03-27", "03-28"),1,0)

data$winter_holidays <- ifelse(format(data$date, "%m-%d") %in% c("12-24","12-25","12-26","12-27","12-28","12-29","12-30",
                                                                 "12-31","01-01") > 0,1,0)
data$startSchoolHoliday = ifelse( format(data$date, "%m-%d") %in% c("03-24", "04-01", "04-08", "04-15", "07-01", "07-08",
                                                                    "07-25", "09-16", "09-23", "09-30", "12-03", "12-15", "12-16", "12-20", "12-21") ,1,0)

data$endSchoolHoliday = ifelse( format(data$date, "%m-%d") %in% c("01-22", "01-25", "02-01", "02-03", "02-08", "02-10",
                                                                    "04-11", "04-26", "04-27", "05-02", "07-11", "07-18", "07-19", "10-04", "10-10",
                                                                    "10-17"),1,0)

data$FatherDay <- ifelse( format(data$date, "%m-%d") %in% c("09-04") ,1,0)
data$ValentineDay <- ifelse( format(data$date, "%m-%d") %in% c("02-14") ,1,0)
data$StPatrickDay <- ifelse( format(data$date, "%m-%d") %in% c("03-17") ,1,0)
data$AnzackDay <- ifelse( format(data$date, "%m-%d") %in% c("04-25") ,1,0)



data$issueUSEM <- ifelse( format(data$date, "%Y-%m-%d") %in% c("2016-02-18") ,1,0)
data$issueBS_sp <- ifelse( format(data$date, "%Y-%m-%d") %in% c("2016-02-24") ,1,0)
data$issueBS_imps <- ifelse( format(data$date, "%Y-%m-%d") %in% c("2016-03-15","2016-03-16") ,1,0)
data$issueGDN <- ifelse( format(data$date, "%Y-%m-%d") %in% c("2015-11-19") ,1,0)



# data$Outliers = ifelse(data$date %in% as.Date(c("2015-09-21","2015-09-22","2015-09-23")),1,0)
# data$issue = ifelse(data$date %in% as.Date(c('2016-01-11','2016-01-12','2016-01-13','2016-01-14','2016-01-15','2016-01-18','2016-01-19','2016-01-20','2016-01-21','2016-01-24','2016-01-25','2016-01-26','2016-01-27','2016-01-28')),1,0)

#competitors trend

if(COMP_NEW) {
  
  comp <- read.csv("comp_wix_au.csv", stringsAsFactors = F)
  
  colnames(comp) <- tolower(colnames(comp))
  
  colnames(comp)[1] <- "date"
  
  comp$date <- as.Date(comp$date, format = "%m/%d/%Y")
  
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
  
data <- merge(data, complete_comps, by='date', all.x = TRUE )
  
data <- data[1:min(which(is.na(data$ext_comps_day))) -1 ,]

if(BRAND_NEW){

  brand <- read.csv("brand.csv", stringsAsFactors = F)
  
  brand$date <- mdy(brand$X)
  brand$X <- NULL
  
}
  

data <- merge(data, brand, by='date', all.x = TRUE )

fit.princomp <- princomp( dplyr::select(data, BS_Imps, BS_spend), cor = TRUE)

bs_comp1 <- data.frame(fit.princomp$scores[,1])*(-1)

data <- merge(data, bs_comp1, by='date', all.x = TRUE )


data[is.na(data)] = 0




# data$ext_comps_day2015 <- ifelse(year(data$date) == 2015, data$ext_comps_day,0)
# 
# data$ext_comps_day2016 <- ifelse(year(data$date) == 2016, data$ext_comps_day,0)
# 
# data$USEM_Imps_2016 <- ifelse(year(data$date) == 2016, data$USEM_Imps^2,0)

#write.csv(data,"WIX_AU_clean.csv",row.names=F)


# define training period
#data2=subset(data,date>=as.Date('2014-07-01')&date<='2016-02-21'&date!="2015-02-09"
#             &date!="2015-02-10"&!(date>='2015-03-15'&date<'2015-04-06'))



#run the code below for new model iteration

if (STEP) {
  
  vars.step = c(
    # "Mon",
    # "Tues",
    "MonTue",
    "Wed",
    "Thur",
    "Fri",
    "Sat",
    # "Sun",
    # "workday",
    "month1",
    "month2",
    "month3",
    "month4",
    "month5",
    "month6",
    "month7",
    "month8",
    "month9",
    "month10",
    "summer",
    "autumn",
    "month11",
    "month12",
    # "holgroup",
    "MotherDay",
    "QueenBDay",
    "HarmonyDay",
    "HarmonyDay2015",
    "AustraliaDay",
    "ChristmasDay",
    "NewYear",
    "winter_holidays",
    "ext_comps_day",
    "GoodFridayDay",
    "EasterDay",
    "LaborDay",
    "MelbourneCupDay", 
    "startSchoolHoliday",
    "endSchoolHoliday",
    "FatherDay",
    "ValentineDay",
    "StPatrickDay",
    "AnzackDay",
    "issueUSEM",
    "issueBS_sp",
    "issueBS_imps",
    "issueGDN",
    "brand_comp",
    "ext_comps_day2",
    "ext_comps_day3",
    "ext_comps_day4",
    # "trend",
    # "GDN_Imps",
    "GDN_spend",
    # "USEM_Imps",
    "USEM_spend",
    "USEM_spend2016",
    "TV15_imp",
    "TV30_imp",
    "TV60_imp",
    "TV60_spend",
    "YouTube_Branding_Imps",
    "YouTube_Performance_Imps",
    "FB_Imps",
    "FB_Imps2014",
    # "FB_spend",
    "radio_spend",
    "BS_Imps"
    # "BS_spend"
    
  )

}


n_adstock = sum(grepl("adstock", vars))
n_mkt <- sum(grepl("para", vars))
n_pow <- n_mkt - n_adstock

n_pars = n_adstock*2 + n_pow

# n_pars = 5

length.base <- length(vars) - n_mkt

#mkt_vars <- c("GDN_spend", "USEM_spend", "tv_English_spend", "Youtube_Branding_Imps", "FB_spend", "BS_spend")

end.2015 <- which(data$date == '2015-12-31')

start_day <- which(data$date == "2014-01-01")

data <- data[start_day:nrow(data),]


train.length <- round(0.8*nrow(data))
# train.length <- which(data$date == "2016-06-16")
# train.length <- which(data$date == "2016-05-16")
# train.length <- end.2015#which(data$date == '2016-02-29')#round(0.85*nrow(data))
# train.length <- which(data$date == "2015-08-31")
# train.length <- which(data$date == "2015-04-31")


full_data <<- data

train.data <<- data[1:train.length,]

if(GLS){
  gls.train.data <- train.data
}

test.data <- data[train.length:nrow(data),]


if (STEP){
  
  vars.lm.step = paste("kpi"," ~ ", paste(vars.step, collapse="+"),sep="")
  
  fit.step <- lm(as.formula(vars.lm.step), data = train.data)
  
  step.lm <- step(object = fit.step)
  
  summary(step.lm)
  step.lm$anova
  
  vars.step.final <- attr(step.lm$terms,'term.labels')
  
}

vars.lm <- paste('kpi'," ~ ", paste(vars, collapse="+"),sep="")



if(!STEP) {
  
  
  max.rsquared=function(data, kpi, para, vars, length.base, full_data) {
    
    
    ### LM fit
    if(LM){
      # vars.lm=paste(kpi," ~ ", paste(vars, collapse="+"),sep="")
      # vars.lm <- paste('kpi'," ~ ", paste(vars, collapse="+"),sep="")
      
      fit.form <- paste("fit <<- lm(", vars.lm, ", data = ",deparse(substitute(train.data)),")")
      eval(parse(text = fit.form))
      
    } else if(ROBUST) {
      # vars.lm <- paste('kpi'," ~ ", paste(vars, collapse="+"),sep="")
      rfit.form <- paste("fit.bisquare = rlm(", vars.lm, ", data = ",deparse(substitute(train.data)),", psi = psi.bisquare)")
      eval(parse(text = rfit.form))
      fit <- fit.bisquare
    }
    
    #calculate if there exists negative base and coefficients for marketing channels
    neg.coef <<- sum(coef(fit)[(length.base+2):length(coef(fit))] < 0)
    
    #calculate the negative base using model decomp
    design <<- model.matrix(as.formula(fit$call[[2]]), data=train.data)
  
    decomp <<- t(t(design) * coef(fit))
    decomp <<- data.frame(decomp)
    min.base=min(rowSums(decomp[,c(1:(length.base+1))]))
    
    base <- rowSums(decomp[,c(1:(length.base+1))])
    
    neg.base_tot <<- sum(base < 0)
    
    neg.base <- ifelse(min.base <0,1,0)
    
    # print(neg.base)
    # print(-summary(fit)$adj.r.squared 
    # +neg.coef*100000+neg.base*20000)
    #penalized if base is negative and some negative coef
    # 
    # # LM return
    # return(-summary(fit)$adj.r.squared+neg.coef*100000+neg.base_tot*1000)
  
    # print(mape(fit$fitted.values, train.data$kpi))
    
    #LASSO return with MAPE
    # 
    # mape <- function(v1,v2){ # mean absolute percentage error
    #   return(mean(abs(v1-v2)/v1))
    # } # 10%
    # 
    # return(mape(fit$fitted.values, gls.train.data$kpi)+neg.coef*10+neg.base_tot*1)
    # 
  
    # LM return with AIC
    
    # test.mae <<- mae(data$kpi[train.length:nrow(full_data)] , predict(fit, full_data)[train.length:nrow(full_data)])
    
    return(mae(full_data$kpi[train.length:nrow(full_data)] , predict(fit, full_data)[train.length:nrow(full_data)])+neg.coef*100000+neg.base_tot*10)
  
  }

  
  
  vars=paste0(vars)
  kpi = "kpi"
  length.base=(length(vars) - n_mkt)
  
  
  if (GLS == T){
    
    ## GLS DEoptim
    fit.da=DEoptim(max.rsquared, lower=rep(0.4,n_pars), upper=rep(0.95,n_pars),
                   vars=vars.gls, length.base=(length(vars.gls) - n_mkt), kpi="kpi",
                   data=gls.train.data, full_data = full_data,
                   DEoptim.control(itermax=150,NP=n_pars*10, parallelType = 1, parVar = list('gls.train.data','vars.gls', 'length.base', 'kpi', 'adstock'))
                   )
    } else {
      
      fit.da=DEoptim(max.rsquared, lower=rep(0.1,n_pars), upper=rep(0.95,n_pars),
                     vars=vars.lm, length.base=(length(vars) - n_mkt), kpi="kpi",
                     data=train.data, full_data = full_data,
                     DEoptim.control(itermax=150,NP=n_pars*10, parallelType = 1, parVar = list('train.data','vars','vars.lm', 'length.base', 'kpi',
                                                                                               'adstock', 'rlm', 'psi.bisquare', 'GLS', 'full_data',
                                                                                               'LASSO', 'ROBUST', 'LM', 'vars.lm', 'mae', 'train.length'))
      )
      
    }
  
  
  para = fit.da$optim$bestmem
  
  # para = c(0.43, 0.37, 0.95, 0.95, 0.95, 0.95, 0.06, 0.68)
  
  fit.da$optim$bestval > 0
  
  ### Build transform variable
  
  
  regressors <- names(data)[names(data) %in% vars.names]
  
  trans_data <- data[,names(data) %in% regressors]
  
  trans_data$kpi <- data$kpi  
  
  # trans_data$GDN_Imps <-data$GDN_Imps^para[1]
  trans_data$GDN_spend <-data$GDN_spend^para[1]
  
  trans_data$USEM_Imps <- data$USEM_Imps^para[10]
  trans_data$USEM_spend <- data$USEM_spend^para[2]
  
  # trans_data$USEM_spend2016 <- data$USEM_spend2016^para[11]
  
  trans_data$TV15_imp <-adstock(data$TV15_imp,para[3])^para[4]
  trans_data$TV30_imp <-adstock(data$TV30_imp,para[5])^para[6]
  trans_data$TV60_imp <-adstock(data$TV60_imp,para[7])^para[8]
  
  # trans_data$TV60_spend <-adstock(data$TV60_spend,para[9])^para[10]
  # 
  # trans_data$YouTube_Branding_Imps <- data$YouTube_Branding_Imps^para[13]
  # trans_data$YouTube_Performance_Imps <-adstock(data$YouTube_Performance_Imps,para[8])^para[9]
  # 
  trans_data$FB_spend <- data$FB_spend^para[11]
  trans_data$FB_Imps <- data$FB_Imps^para[9]
  # trans_data$FB_Imps2014 <- data$FB_Imps2014^para[12]
  
  # trans_data$radio_spend <- adstock(data$radio_spend,para[13])^para[14]
  
  # trans_data$BS_Imps <- data$BS_Imps^para[6]
  # trans_data$BS_Spend <- data$BS_spend^para[7]
  
  
  trans_data$date <- format(data$date, format = "%m/%d/%Y")
  
  
  write.csv(trans_data, "out_model.csv", row.names = FALSE)

}
