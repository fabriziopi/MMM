library(psych)

comp <- read.csv("comp_wix_au.csv", stringsAsFactors = F)

fit.princomp <- princomp(comp[,2:ncol(comp)], cor = TRUE)

summary(fit.princomp)

loadings(fit.princomp) # pc loadings 
#plot(fit.princomp,type="lines") # scree plot 
fit.princomp$scores # the principal components
#biplot(fit.princomp)
