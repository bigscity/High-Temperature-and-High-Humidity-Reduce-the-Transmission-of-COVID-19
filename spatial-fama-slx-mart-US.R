# Fama-macbeth with spatial 
library(raster)
library(spatialreg)
library(sandwich)
library(lmtest)

data<-read.csv('./data/US_reg_data.csv')
data<-data.frame(data)
nna<-data[!is.na(data['err_fulltime']),]

# step 1:

st <- min(nna$day) # st = 19
ed <- max(nna$day) # ed = 41

reg.eq2 <- rt ~ t6 + h6 + p_dns_norm + p_over65_norm + gini_norm + se_factor + p_icubeds_norm + m50i6 + home6 + lat_norm + lon_norm + Matern(1| lat_norm+lon_norm)

betas <- matrix(0, ed-st+1, ncol=12)
rnames <- c()
for (day in st:ed) {
  subdata <- nna[nna['day'] == day, ]
  rnames <- c(rnames, day)
  reg2 <- fitme(reg.eq2, data=subdata, family='gaussian', control.dist=list(dist.method="Earth"))
  betas[day - st+1,] <- reg2$fixef
  
  subdata$yhat <- predict(reg2)
  subdata$err_fulltime <- subdata$rt - subdata$yhat
  
  nu <- reg2$ranFix$corrPars$`1`$nu
  rho <- reg2$ranFix$corrPars$`1`$rho
  
  w <- MaternCorr(rdist.earth(subdata[, c('lat_norm', 'lon_norm')]), nu=nu, rho=rho)
  len<-length(subdata[,1])
  w<-as.matrix(w)
  ww <- mat2listw(w, style='M')
  
  len<-length(subdata[,1])
  err<-c()
  for (i in 1:len){
    err<-c(err, as.numeric(as.character(subdata['err_fulltime'][i, 1])))
  }
  
  mor<-moran.test(err, ww, randomisation=TRUE, alternative = 'two.sided')
  print(c(day, mor$p.value))
}

betas_df2 <- data.frame(betas)
colnames(betas_df2) <- names(reg2$fixef)
rownames(betas_df2) <- rnames
write.csv(betas_df2, './data/US_reg_betas_spamm.csv', row.names = TRUE)
