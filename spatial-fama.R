# Fama-macbeth with spatial 
library(raster)
library(spatialreg)
library(sandwich)
library(lmtest)

stateadj<-read.csv('./data/states_adj_pair.csv')
stateadj<-data.frame(stateadj)

testadj <- function(r, A) {
  sum(A$x==r[1] & A$y==r[2]) +sum(A$y==r[1] & A$x==r[2]) +sum(r[1] == r[2])
}


data<-read.csv('./data/US_reg_data.csv')
data<-data.frame(data)
nna<-data[!is.na(data['err_fulltime']),]

# step 1:

st <- min(nna$day) 
ed <- max(nna$day) 
reg.eq1 <- rt ~ t6 + h6 + p_dns_norm + p_over65_norm + gini_norm + se_factor + p_icubeds_norm + m50i6 + home6 + lat_norm + lon_norm

betas <- matrix(0, ed-st+1, ncol=13)
ws <- list()
for (day in 1:42) {
  subdata <- nna[nna['day'] == day-1, ]
  
  len<-length(subdata[,1])
  w<-matrix(0, len, len)
  stateabbr <- subdata$state_abbr
  for (i in 1:len){
    for (j in 1:len){
      s1 = stateabbr[i]
      s2 = stateabbr[j]
      isadj <- testadj(c(s1, s2), stateadj)
      if (isadj == 0){
        w[i, j] <- 0
      }
      else {
        w[i, j] <- 1
      }
    }
  }
  ws[[day]] <- w
  ww<-mat2listw(w, style='B')
  reg <- lagsarlm(reg.eq1, data = subdata, ww)
  betas[day,] <- coef(reg)
  print(day)
}

betas_df2 <- data.frame(betas)
colnames(betas_df2) <- names(coef(reg))

# step 2:

fm <- lm(t6 ~ 1, data=betas_df2)
coeftest(fm, vcov=NeweyWest(fm, lag=7))

fm <- lm(h6 ~ 1, data=betas_df2)
coeftest(fm, vcov=NeweyWest(fm, lag=7))
write.csv(betas_df2, './data/US_reg_betas.csv', row.names = TRUE)
# Save and use Stata for analysis

final_coef <- colMeans(betas_df2)
aicadj <- c()
aicdist <- c()
for (day in 1:42) {
  subdata <- nna[nna['day'] == day-1, ]
  
  ww<-mat2listw(ws[[day]], style='B')
  reg <- lagsarlm(reg.eq1, data = subdata, ww)
  reg2 <- fitme(reg.eq2, data=subdata, family='gaussian', control.dist=list(dist.method="Earth"))
  aicr2 <- AIC(reg2)
  print(c(day, reg$AIC_lm.model, aicr2[[1]]))
  aicadj <- c(aicadj, reg$AIC_lm.model)
  aicdist <- c(aicdist, aicr2[[1]])
}
