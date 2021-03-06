
find_best_pars = function (start_day, train.length.percentage, vars, df, max_iter = 150) 

{

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
  
  
  LM <<- T
  ROBUST <<- F
  GLS <<- F
  LASSO <<- F
  STEP <<- F
  
  COMP_NEW <<- T
  BRAND_NEW <<- F
  
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

  
  kpi <<- 'kpi'
  
  n_adstock = sum(grepl("adstock", vars))
  n_mkt <- sum(grepl("para", vars))
  n_pow <- n_mkt - n_adstock
  
  # n_pars = n_adstock*2 + n_pow
  
  n_pars = 12
  
  length.base <- length(vars) - n_mkt
  
  #mkt_vars <- c("GDN_spend", "USEM_spend", "tv_English_spend", "Youtube_Branding_Imps", "FB_spend", "BS_spend")
  
  end.2015 <- which(df$date == '2015-12-31')
  
  # start_day <- which(df$date == "2014-10-01")
  
  df <- df[which(df$date == start_day):nrow(df),]

  train.length <<- round(train.length.percentage*nrow(df))
  # train.length <- which(df$date == "2016-06-16")
  # train.length <- which(df$date == "2016-05-16")
  # train.length <- end.2015#which(df$date == '2016-02-29')#round(0.85*nrow(df))
  # train.length <- which(df$date == "2015-08-31")
  # train.length <- which(df$date == "2015-04-31")
  
  
  full_df <<- df
  
  train.df <<- df[1:train.length,]
  
  if(GLS){
    gls.train.df <- train.df
  }
  
  test.df <- df[train.length:nrow(df),]
  
  

  
  vars.lm <<- paste('kpi'," ~ ", paste(vars, collapse="+"),sep="")
  
  
  
  if(!STEP) {
    
    
    max.rsquared=function(df, kpi, para, vars.lm, length.base, full_df) {
      
      
      ### LM fit
      if(LM){
        # vars.lm=paste(kpi," ~ ", paste(vars, collapse="+"),sep="")
        # vars.lm <- paste('kpi'," ~ ", paste(vars, collapse="+"),sep="")
        
        fit.form <- paste("fit <<- lm(", vars.lm, ", data = ",deparse(substitute(train.df)),")")
        eval(parse(text = fit.form))
        
      } else if(ROBUST) {
        # vars.lm <- paste('kpi'," ~ ", paste(vars, collapse="+"),sep="")
        rfit.form <- paste("fit.bisquare <<- rlm(", vars.lm, ", data = ",deparse(substitute(train.df)),", psi = psi.bisquare)")
        eval(parse(text = rfit.form))
        fit <<- fit.bisquare
      }
      
      #calculate if there exists negative base and coefficients for marketing channels
      neg.coef <<- sum(coef(fit)[(length.base+2):length(coef(fit))] < 0)
      
      #calculate the negative base using model decomp
      design <<- model.matrix(as.formula(fit$call[[2]]), data=train.df)
      
      decomp <<- t(t(design) * coef(fit))
      decomp <<- data.frame(decomp)
      
      design_full <<- model.matrix(as.formula(fit$call[[2]]), data=full_df)
      
      decomp_full <<- t(t(design_full) * coef(fit))
      decomp_full <<- data.frame(decomp_full)
      
      
      min.base=min(rowSums(decomp[,c(1:(length.base+1))]))
      
      base <- rowSums(decomp[,c(1:(length.base+1))])
      
      neg.base_tot <<- sum(base < 0)
      
      neg.base <- ifelse(min.base <0,1,0)
      

      spend_data <- spend[,grepl("spend", colnames(spend))]
      
      spend_data[is.na(spend_data)] <- 0
      
      # spend_data <- spend_data[which(spend$date == df$date[1]):nrow(spend_data),]
      
      
      
      tot_spend_channel <- colSums(spend_data)
      
      tot_lift_channel <- colSums(decomp_full)
      
      tot_lift_channel_mkt <- tot_lift_channel[(length.base+2):length(tot_lift_channel)]
      
      tv_tot_lift <- sum(tot_lift_channel_mkt[grepl("TV", names(tot_lift_channel_mkt))])
      
      tv_tot_spend <- sum(tot_spend_channel[grepl("TV", names(tot_spend_channel))])
      cpr_tv <- tv_tot_spend / tv_tot_lift
      
      tv15_tot_lift <- sum(tot_lift_channel_mkt[grepl("TV15", names(tot_lift_channel_mkt))])
      tv15_tot_spend <- sum(tot_spend_channel[grepl("TV15", names(tot_spend_channel))])
      cpr_tv15 <- tv15_tot_spend / tv15_tot_lift
    
      fb_tot_lift <- sum(tot_lift_channel_mkt[grepl("FB", names(tot_lift_channel_mkt))])
      fb_tot_spend <- sum(tot_spend_channel[grepl("FB", names(tot_spend_channel))])
      cpr_fb <- fb_tot_spend / fb_tot_lift
      
      usem_tot_lift <- sum(tot_lift_channel_mkt[grepl("USEM", names(tot_lift_channel_mkt))])
      usem_tot_spend <- sum(tot_spend_channel[grepl("USEM", names(tot_spend_channel))])
      cpr_usem <- usem_tot_spend / usem_tot_lift
      
      test_mape <- mape(full_df$kpi[train.length:nrow(full_df)] , predict(fit, full_df)[train.length:nrow(full_df)])
      
      train_mape <- mape(full_df$kpi[1:train.length] , predict(fit, full_df)[1:train.length])
      
      # regressors <- names(df)[names(df) %in% vars.names]
      # 
      # trans_df <- df[,names(df) %in% regressors]
      # 
      # trans_df$kpi <- df$kpi  
      
      # trans_df$GDN_Imps <-df$GDN_Imps^para[17]
      
      # trans_df$GDN_spend <-df$GDN_spend^para[1]
      # trans_df$GDN_spend <- adstock(df$GDN_spend, para[13])^para[1]
      
      
      # trans_df$USEM_Imps <- df$USEM_Imps^para[13]
      
      # trans_df$USEM_spend <- df$USEM_spend^para[2]
      # trans_df$USEM_spend <- adstock(df$USEM_spend, para[14])^para[2]
      
      
      # trans_df$USEM_spend2016 <- df$USEM_spend2016^para[11]
      
      # trans_df$TV15_imp <-adstock(df$TV_imp,para[3])^para[4]
      
      # trans_df$TV15_imp <-adstock(df$TV15_imp,para[3])^para[4]
      # trans_df$TV15_imp <-adstock(df$TV15_spend,para[3])^para[4]
      # trans_df$TV30_imp <-adstock(df$TV30_imp,para[5])^para[6]
      # trans_df$TV60_imp <-adstock(df$TV60_imp,para[7])^para[8]
      # trans_df$TV3060_imp <-adstock(df$TV3060_imp,para[5])^para[6]
      # trans_df$TV3060_spend <-adstock(df$TV3060_spend,para[5])^para[6]
      # trans_df$TV60_spend <-adstock(df$TV60_spend,para[5])^para[6]
      
      
      # 
      # trans_df$YouTube_Branding_Imps <- df$YouTube_Branding_Imps^para[7]
      # trans_df$YouTube_Performance_Imps <-adstock(df$YouTube_Performance_Imps,para[8])^para[9]
      # trans_df$Youtube_test_Imps <-adstock(df$Youtube_test_Imps,para[10])^para[11]
      
      # trans_df$YouTube_Performance_spend <-adstock(df$YouTube_Performance_spend,para[10])^para[11]
      # trans_df$YouTube_All_Imps <-adstock(df$YouTube_All_Imps,para[7])^para[8]
      
      
      # trans_df$FB_spend <- df$FB_spend^para[12]
      
      # train_control <- trainControl(method = 'cv', number = 10)
      # grid <- expand.grid(.fL=c(0), .usekernel=c(FALSE))
      # vars.names.lm <- paste0(vars.names, collapse= "+")
      # 
      # model <- train(as.formula(paste0('kpi~',vars.names.lm)), data=trans_df, trControl=train_control, method="lm")

      return( 000*test_mape+ 100*train_mape + neg.coef*100000+neg.base_tot*0.1 -0*coef(summary(fit))[13,3] -.0*coef(summary(fit))[14,3] +1*abs((cpr_tv-24)/24)^2+0*abs((cpr_tv15-34)/34)^2 + 0.*(abs((cpr_usem -20)/20))^2 )
      # return( 000*test_mape+ 100*train_mape + neg.coef*100000+neg.base_tot*3 -0*coef(summary(fit))[13,3] -.0*coef(summary(fit))[14,3] +1*abs((cpr_tv-24)/24)^2+ 0.*(abs((cpr_usem -20)/20))^2 )
      
      # return(model$results$RMSE)
    }
  
    
    
    vars=paste0(vars)
    kpi = "kpi"
    length.base <<- (length(vars) - n_mkt)
    
    
    if (GLS == T){
      
      ## GLS DEoptim
      fit.da=DEoptim(max.rsquared, lower=rep(0.4,n_pars), upper=rep(0.8,n_pars),
                     vars=vars.gls, length.base=(length(vars.gls) - n_mkt), kpi="kpi",
                     df=gls.train.df, full_df = full_df,
                     DEoptim.control(itermax=max_iter,NP=n_pars*10, parallelType = 1, parVar = list('gls.train.df','vars.gls', 'length.base', 'kpi', 'adstock'))
      )
    } else {
      
      max <- 0.85
      
      uplim <- c(max,max,1,1,1,1,max,1,max,1,1,max)
      
      fit.da=DEoptim(max.rsquared, lower=rep(0.1, n_pars), upper=uplim,
                     vars.lm=vars.lm, length.base=(length(vars) - n_mkt), kpi="kpi",
                     df=as.data.frame(train.df), full_df = full_df,
                     DEoptim.control(itermax=max_iter,NP=n_pars*10, parallelType = 1, parVar = list('train.df','vars','vars.lm', 'length.base', 'kpi',
                                                                                               'adstock', 'rlm', 'psi.bisquare', 'GLS', 'full_df',
                                                                                               'LASSO', 'ROBUST', 'LM', 'spend', 'mae', 'train.length',
                                                                                               'vars.names', 'mape'))
      )
      
    }
    
    # lower=c(0.1,0.1,0.1,0.4,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1)
    para = fit.da$optim$bestmem
    
    # para <- c(0.992, 0.969, 0.737, 0.983, 0.978, 0.989, 0.253, 0.155 ,0.942 ,0.137, 0.295, 0.969)
    
    
    # para = c(0.43, 0.37, 0.95, 0.95, 0.95, 0.95, 0.06, 0.68)
    
    fit.da$optim$bestval > 0
    
    ### Build transform variable
    
    
    regressors <- names(df)[names(df) %in% vars.names]
    
    trans_df <- df[,names(df) %in% regressors]
    
    trans_df$kpi <- df$kpi  
    
    # trans_df$GDN_Imps <-df$GDN_Imps^para[17]

    trans_df$GDN_spend <-df$GDN_spend^para[1]
    # trans_df$GDN_spend <- adstock(df$GDN_spend, para[13])^para[1]
    
    
    # trans_df$USEM_Imps <- df$USEM_Imps^para[13]
    
    trans_df$USEM_spend <- df$USEM_spend^para[2]
    # trans_df$USEM_spend <- adstock(df$USEM_spend, para[14])^para[2]
    
    
    # trans_df$USEM_spend2016 <- df$USEM_spend2016^para[11]
    
    # trans_df$TV_imp <-adstock(df$TV_imp,para[3])^para[4]
    
    # trans_df$TV15_imp <-adstock(df$TV_imp,para[3])^para[4]
    
    trans_df$TV15_imp <-adstock(df$TV15_imp,para[3])^para[4]
    # trans_df$TV15_imp <-adstock(df$TV15_spend,para[3])^para[4]
    # trans_df$TV30_imp <-adstock(df$TV30_imp,para[5])^para[6]
    # trans_df$TV60_imp <-adstock(df$TV60_imp,para[7])^para[8]
    trans_df$TV3060_imp <-adstock(df$TV3060_imp,para[5])^para[6]
    # trans_df$TV3060_spend <-adstock(df$TV3060_spend,para[5])^para[6]
    # trans_df$TV60_spend <-adstock(df$TV60_spend,para[5])^para[6]
    
    
    # 
    trans_df$YouTube_Branding_imps <- df$YouTube_Branding_imps^para[7]
    trans_df$YouTube_Performance_imps <-adstock(df$YouTube_Performance_imps,para[8])^para[9]
    trans_df$Youtube_test_Imps <-adstock(df$Youtube_test_Imps,para[10])^para[11]
    
    # trans_df$YouTube_Performance_spend <-adstock(df$YouTube_Performance_spend,para[10])^para[11]
    # trans_df$YouTube_All_Imps <-adstock(df$YouTube_All_Imps,para[7])^para[8]
    
    
    trans_df$FB_spend <- df$FB_spend^para[12]
    
    # trans_df$FB_Imps <- df$FB_Imps^para[14]
    # trans_df$FB_Imps2014 <- df$FB_Imps2014^para[12]
    
    # trans_df$radio_spend <- adstock(df$radio_spend,para[13])^para[14]
    # 
    # trans_df$BS_Imps <- df$BS_Imps^para[15]
    # trans_df$BS_Spend <- df$BS_spend^para[16]
    
    
    
    # lars.result <- lars(x = as.matrix(trans_df[, 1:(ncol(trans_df)-1)]), y = trans_df[, ncol(trans_df)], type = 'lar')
    # summary(lars.result)
    # print(lars.result)
    # cv.lars(x = as.matrix(trans_df[, 1:(ncol(trans_df)-1)]), y = trans_df[, ncol(trans_df)], type = 'lasso')
    # coef(lars.result) 
    # plot(lars.result)
    # 
    trans_df$date <- format(df$date, format = "%Y/%m/%d")
    
    
    write.csv(trans_df, "out_model.csv", row.names = FALSE)
    
  }
  
  return(para)
}
  
