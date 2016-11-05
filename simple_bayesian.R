library(dabayes)
library(damodel)
library(shinystan)
library(ggmcmc)
library(ggplot2)
library(scales)
library(rstan)
library(modeest)


setwd("C:\\Users\\Fabrizio Piasini\\Desktop\\wix_au\\expl")

### STAN modeling

model <- "
data {
int<lower=1> N;
vector<lower=0>[N] email;
vector<lower=0>[N] NDP;
vector<lower=0>[N] Actives;
}
parameters {
real<lower=0> beta_intercept;
real<lower=0> beta_email;
real<lower=0> beta_NDP;
real<lower=0> sigma;
}
model {
sigma ~ normal(0, 100);
beta_intercept ~ normal(1000, 3000);
beta_email ~ normal(1.7, 0.5);
Actives ~ normal(beta_intercept+beta_email*email+beta_NDP*NDP, sigma);
}
generated quantities {
vector[N] ActivesTilde;
for(i in 1:N){
ActivesTilde[i] <- normal_rng(beta_intercept+beta_email*email[i]+beta_NDP*NDP[i], sigma);
}
}"

### STAN OVER


# ### read and clean data - START
# 
# df <- read.csv("wix_us_media_data.csv", stringsAsFactors = F)
# df[is.na(df)] = 0
# df[df == '\\N'] = 0
# names(df) <- tolower(names(df))
# df$date <- as.Date(df$date, format = "%m/%d/%Y")
# 
# kpif <- read.csv("wix_us_kpi.csv", stringsAsFactors = F)
# kpif$date <- as.Date(kpif$date, format = "%m/%d/%Y")
# names(kpif) <- tolower(names(kpif))
# 
# kpif$kpi8 <- as.numeric(kpif$kpi8)
# 
# wix <- merge(df, kpif, by="date")
# 
# wix$tv30_imp <- as.numeric(wix$tv30_imp)
# wix$tv30_spend <- as.numeric(wix$tv30_spend)
# 
# wix$weekday <- weekdays(wix$date)
# 
# weekday <- factor(wix$weekday, levels = c( "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday") )
# dummies <- model.matrix(~weekday)
# dummies <- dummies[, c(2:7)]
# wix <- cbind(wix, dummies)
# 
# #wix$trend1=log(c(1:nrow(wix)))
# #wix$trend2=log(wix$trend1 + 1)
# wix$trend3=c(1:nrow(wix))
# 
# #wix$weekend=ifelse(wix$weekday %in% c("Sunday", "Saturday"), 1, 0)
# 
# wix$month=month(wix$date)
# wix$year=year(wix$date)
# 
# ### read and clean data - END

# dates <- data.frame(seq(1:957))

df <- read.csv("WIX_AU_clean.csv", stringsAsFactors = F)

dates$days <- data.frame(seq.Date( as.Date("2014/01/01"), as.Date("2016/08/14"), by = 'days') )

dates <- dates[,2]
colnames(dates) <- "date"
df <- merge(dates, wix, by = "date", all = TRUE)
rownames(df) <- df$date

df[is.na(df)] = 0

bModelList <- BayesModelList$new()

obj <- "kpi"
bModelList <- BayesModelList$new()
tmpBModel <- bayesObjectiveModelFct(responseVar = obj, myDataframe = df)
bModelList$addModel(tmpBModel)

tmpBModel$addVariable(linearVar(predictorVar = 'tv15_spend', bayesParam = normalParamFixed(mean = .01, sd = .001, lowerBound = 0)))
tmpBModel$addVariable(linearVar(predictorVar = 'tv30_spend', bayesParam = normalParamFixed(mean = .01, sd = .001, lowerBound = 0)))
tmpBModel$addVariable(linearVar(predictorVar = 'tv60_spend', bayesParam = normalParamFixed(mean = .01, sd = .001, lowerBound = 0)))
tmpBModel$addVariable(linearVar(predictorVar = 'radio_spend', bayesParam = normalParamFixed(mean = 1, sd = 2, lowerBound = 0)))
tmpBModel$addVariable(linearVar(predictorVar = 'weekdayMonday', bayesParam = normalParamFixed(mean = 1000, sd = 150)))
tmpBModel$addVariable(linearVar(predictorVar = 'weekdaySaturday', bayesParam = normalParamFixed(mean = -1000, sd = 150)))
tmpBModel$addVariable(linearVar(predictorVar = 'weekdayFriday', bayesParam = normalParamFixed(mean = -1000, sd = 150)))
tmpBModel$addVariable(linearVar(predictorVar = 'weekdayThursday', bayesParam = normalParamFixed(mean = 865, sd = 150)))
tmpBModel$addVariable(linearVar(predictorVar = 'weekdayTuesday', bayesParam = normalParamFixed(mean = 1500, sd = 150)))
tmpBModel$addVariable(linearVar(predictorVar = 'weekdayWednesday', bayesParam = normalParamFixed(mean = 1250, sd = 150)))


library(dafast)
library(dautility)

# sfit<-bModelList$fit(myDatafra me = df, mc.cores = 2, nChains = 4, samplePriorsOnly = TRUE,
#                      numSavedSteps = 1000, warmupSteps = 500, saveStanFile = 'bayesmodel.Stan')
# 
# sfitponlydf<-bModelList$stanfitdf



sfit<-bModelList$fit(myDataframe = df, mc.cores = 2, numSavedSteps = 1000, warmupSteps = 500, nChains = 4, saveStanFile = 'bayesmodel.Stan')

sfitdf<-bModelList$stanfitdf

plotPriorVSPost(sfitdf, "kpi9_beta_tv30_spend", sfitponlydf, showAll = T)
