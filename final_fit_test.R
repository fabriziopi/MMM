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
library(MASS)
library(lattice)
library(corrplot)
library(GGally)

df_fit <- read.csv("out_model.csv", stringsAsFactors = FALSE)

df_fit$date <- ymd(df_fit$date)

df_fit_complete <- df_fit

train.length <- (0.8*nrow(df_fit))

df_fit <- df_fit[1:train.length,]


# df_fit$date <- ymd(df_fit$date)


# fb_check <- data[,grep('FB',names(data))]
# fb_check$year <-  as.factor(year(fb_check$date))
# ggpairs(fb_check, mapping = aes(color = year), columns = c('FB_spend', 'FB_Imps'))

n_adstock = sum(grepl("adstock", vars))
n_mkt <- sum(grepl("para", vars))
n_pow <- n_mkt - n_adstock

n_pars = n_adstock*2 + n_pow

# n_pars = 5

length.base <- length(vars) - n_mkt



vars.names.lm <- paste0(vars.names, collapse= "+")

f <- paste("kpi~", vars.names.lm)

# f.reduced <- 
# rfit.bisquare_reduced <- rlm(as.formula(f.reduced), df_fit, psi = psi.bisquare)

# fit.lax <- lasso2::l1ce(as.formula(f),df_fit, standardize = F)


fit <- lm(as.formula(f), df_fit )
# fit.gls <- glm(f, df_fit, family = 'gaussian')

rfit.hampel <- rlm(as.formula(f), df_fit, psi = psi.hampel)
rfit.huber <- rlm(as.formula(f), df_fit, psi = psi.huber)
rfit.bisquare <- rlm(as.formula(f), df_fit, psi = psi.bisquare)




# 
# summary(fit.gls)

# waldtest(rfit.bisquare, rfait.bisquare_reduced)

# par(mfrow = c(2,2))

# plot(fit.gls)

# df_fit$predicted <- predict(fit.gls, df_fit)
# 
# df_fit_complete$predicted <- predict(fit.gls, df_fit_complete)
# 
# df_fit$predicted <- predict(fit.lax, df_fit)
# 
# df_fit_complete$predicted <- predict(fit.lax, df_fit_complete)

# df_fit$predicted <- predict(rfit.bisquare, df_fit)
# 
# df_fit_complete$predicted <- predict(rfit.bisquare, df_fit_complete)

df_fit$predicted <- predict(fit, df_fit)

df_fit_complete$predicted <- predict(fit, df_fit_complete)

par(mfrow = c(2,2))

# plot(rfit.bisquare)

# summary(fit)
# summary(rfit.hampel)
# summary(rfit.huber)
summary(rfit.bisquare)

fits <- list(fit, rfit.bisquare, rfit.huber, rfit.hampel)

design <- model.matrix(as.formula(f), data=df_fit)

decomp <- t(t(design) * coef(rfit.bisquare))
decomp <- data.frame(decomp)

#length.base <- sum(grepl("imp",vars.lm))

base <- rowSums(decomp[,1:(length.base+1)])

stack <- decomp[,-c(1:(length.base+1))]

stack <- cbind(base, stack)

rm <- rollmean(stack, 7)

rm_date <- cbind(date = df_fit$date[1:nrow(rm)], as.data.frame(rm))

stack <- cbind(date = df_fit$date, stack)
# 
# melted <- melt(stack, id = 'date', variable_name = 'channel')
# 
# ggplot(melted, aes(x = date, y = value, fill=channel)) +geom_area()

melted_rm <- melt(rm, variable_name = 'channel')
  

spend_data <- spend[,grepl("spend", colnames(spend))]

spend_data[is.na(spend_data)] <- 0

tot_spend_channel <- colSums(spend_data)

tot_lift_channel <- colSums(decomp)


tot_lift_channel_mkt <- tot_lift_channel[(length.base+1):length(tot_lift_channel)]

cat('\nStart Day:', as.character.Date(df_fit$date[1]))
cat('\nEnd Day: ', as.character.Date(df_fit$date[nrow(df_fit)]))

tv15_tot_lift <- sum(tot_lift_channel_mkt[grepl("TV15", names(tot_lift_channel_mkt))])
tv15_tot_spend <- sum(tot_spend_channel[grepl("TV15", names(tot_spend_channel))])
cat('\n\nTV15 cost per', round(cpr_tv15 <- tv15_tot_spend / tv15_tot_lift))

tv30_tot_lift <- sum(tot_lift_channel_mkt[grepl("TV30", names(tot_lift_channel_mkt))])
tv30_tot_spend <- sum(tot_spend_channel[grepl("TV30", names(tot_spend_channel))])
cat('\nTV30 cost per', round(cpr_tv30 <- tv30_tot_spend / tv30_tot_lift))

tv60_tot_lift <- sum(tot_lift_channel_mkt[grepl("TV60", names(tot_lift_channel_mkt))])
tv60_tot_spend <- sum(tot_spend_channel[grepl("TV60", names(tot_spend_channel))])
cat('\nTV60 cost per', round(cpr_tv60 <- tv60_tot_spend / tv60_tot_lift))

tv_tot_lift <- sum(tot_lift_channel_mkt[grepl("TV", names(tot_lift_channel_mkt))])
tv_tot_spend <- sum(tot_spend_channel[grepl("TV", names(tot_spend_channel))])
cat('\nTV cost per', round(cpr_tv <- tv_tot_spend / tv_tot_lift))

bs_tot_lift <- sum(tot_lift_channel_mkt[grepl("BS", names(tot_lift_channel_mkt))])
bs_tot_spend <- sum(tot_spend_channel[grepl("BS", names(tot_spend_channel))])
cat('\nBS cost per', round(cpr_bs <- bs_tot_spend / bs_tot_lift))

usem_tot_lift <- sum(tot_lift_channel_mkt[grepl("USEM", names(tot_lift_channel_mkt))])
usem_tot_spend <- sum(tot_spend_channel[grepl("USEM", names(tot_spend_channel))])
cat('\nUSEM cost per', round(cpr_usem <- usem_tot_spend / usem_tot_lift))

fb_tot_lift <- sum(tot_lift_channel_mkt[grepl("FB", names(tot_lift_channel_mkt))])
fb_tot_spend <- sum(tot_spend_channel[grepl("FB", names(tot_spend_channel))])
cat('\nFB cost per', round(cpr_fb <- fb_tot_spend / fb_tot_lift))

# shapiro.test(rfit.bisquare$residuals)
# durbinWatsonTest(rfit.bisquare$residuals)
# bptest(rfit.bisquare)

# regressors <- df_fit[,names(df_fit) %in% vars.names]

# corrplot(cor_matrix <- cor(cbind(as.data.frame(fit.gls$residuals), regressors)), method = 'number')
cat('\n\ntraining: ',round(mape(df_fit_complete$kpi[1:train.length], df_fit_complete$predicted[1:train.length]),3))
cat('\noverall: ',round(mape(df_fit_complete$kpi, df_fit_complete$predicted),3))
cat('\ntest: ', round(mape(df_fit_complete$kpi[train.length:nrow(df_fit_complete)] , df_fit_complete$predicted[train.length:nrow(df_fit_complete)]),3))

mae(df_fit_complete$kpi[1:train.length], df_fit_complete$predicted[1:train.length])
mae(df_fit_complete$kpi, df_fit_complete$predicted)
mae(df_fit_complete$kpi[train.length:nrow(df_fit_complete)] , df_fit_complete$predicted[train.length:nrow(df_fit_complete)])


# lapply(fits, AIC)

df_fit_sel <- dplyr::select(df_fit_complete, c(date, kpi, predicted))
df_fit_sel_weekly <- as.data.frame(rollmean( dplyr::select(df_fit_sel, -date ) , 7))
df_fit_sel_weekly$date <- seq(from = data$date[1], by = 1, length.out = nrow(df_fit_sel_weekly))
df_fit_sel_melt <- melt(df_fit_sel,id.vars = 'date')
df_fit_sel_melt_weekly <- melt(df_fit_sel_weekly,id.vars = 'date')

ggplot(df_fit_sel_melt, aes(x=date, y=value, color = variable )) + geom_line(size = 1)
ggplot(df_fit_sel_melt_weekly, aes(x=date, y=value, color = variable))+ geom_line(size = 1.5)
ggplot(melted_rm, aes(x = X1, y = value, fill=X2)) +geom_area()

coeftest(fit, vcov = vcovHAC(fit))

df_fit_gvis <- dplyr::mutate(df_fit_sel, type = as.factor(ifelse(df_fit_complete$date < df_fit_complete$date[train.length], 'training', 'test' )))
# 
plot(gvisLineChart(df_fit_gvis[,], xvar = "date", y = c("kpi", "predicted"), options = list(width = 1600, height = 450) ))
# 
plot(gvisLineChart(df_fit_sel_weekly, xvar = "date", y = c("kpi", "predicted"), options = list(width = 1600, height = 450) ))
# # 
plot(gvisAreaChart(rm_date, options = list(isStacked = T, width = 1600, height = 450)))

# plot(Line)

# lapply(fits, BIC)

### LOG PART

# df_fit_log <- 