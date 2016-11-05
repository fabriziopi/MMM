library(MASS)
library(DAAG)
library(tidyr)
library(sandwich)

fit.mae <- vector("numeric")

train.size <- .99

df <- create_data_frame(kpi_file = "wix_au_kpi.csv", TV_file = "wix_au_tv_new.csv", other_marketing_file = 'wix_au_marketing.csv')

para <- find_best_pars('2014-01-01', train.size, vars, df, max_iter = 150)

para <- c(0.992, 0.969, 0.737, 0.983, 0.978, 0.989, 0.253, 0.155 ,0.942 ,0.137, 0.295, 0.969)
# 
fitoff <- lm(formula = kpi ~ Mon+Tues+Wed+Thur+Fri+Sat+winter_holidays
             +ext_comps_day+bs_comp2+I(GDN_spend^para[1])+I(USEM_spend^para[2])
             +I(YouTube_Branding_imps^para[7])+I(adstock(YouTube_Performance_imps,para[8])^para[9])
             +I(adstock(Youtube_test_Imps, para[10])^para[11])+I(FB_spend^para[12]),
             offset = 0.10871839*I(adstock(TV15_imp,para[3])^para[4])+0.32960165*I(adstock(TV3060_imp,para[5])^para[6]), data = df)

# fitoff <- lm(formula = kpi ~ Mon+Tues+Wed+Thur+Fri+Sat+winter_holidays
#              +seasonality+bs_comp2+I(GDN_spend^para[1])+I(USEM_spend^para[2])
#              +I(YouTube_Branding_imps^para[7])+I(adstock(YouTube_Performance_imps,para[8])^para[9])
#              +I(adstock(Youtube_test_Imps, para[10])^para[11])+I(FB_spend^para[12]),
#              offset = 0.10871839*I(adstock(TV15_imp,para[3])^para[4])+0.32960165*I(adstock(TV3060_imp,para[5])^para[6]), data = df)



# cv <- cv.lm(df, fitoff, m = 5, printit = FALSE)
# cat('\nCV:', attributes(cv)$ms)


fit.mae <- append(fit.mae, get_fit_results(train.size, coefCheck = T, plot_charts = F))

# cat('\nGDN Diminishing return:', attributes(cv)$ms)
# cat('\nTV15 Decay rate:', attributes(cv)$ms)



# 
# coeff <- 0.1
# 
# while (coeff < .89) {
# 
# 
# coeff_start <- coeff
# coeff_end <- coeff + 0.2
# 
# start <- round(coeff_start*1014)
# 
# end <- round(coeff_end*1014)
# 
# print(cor(resid[start:end],df[start:end,c(3,5,6,7,9,12,14,17,25,82,90)]))
# 
# coeff <- coeff + 0.1
# 
# }
# 
# print(cor(resid[930:990],df[930:990,c(3,5,6,7,9,12,14,17,25,82,90)]))
# print(cor(resid[1:1014],df[1:1014,c(3,5,6,7,9,12,14,17,25,82,90)]))
# print(cor(resid[517:554],df[517:554,c(3,5,6,7,9,12,14,17,25,82,90)]))
# print(cor(resid[888:955],df[888:955,c(3,5,6,7,9,12,14,17,25,82,90)]))
