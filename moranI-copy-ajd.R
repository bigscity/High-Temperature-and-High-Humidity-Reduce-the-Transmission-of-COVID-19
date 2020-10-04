distance<-function(lat1, lon1, lat2, lon2){
	dis<-sqrt((lat1 - lat2)^2 + (lon1 - lon2)^2)
	return(dis)
}

epsilon = 1e-6
Delta = 1e6

stateadj<-read.csv('./data/states_adj_pair.csv')
stateadj<-data.frame(stateadj)

testadj <- function(r, A) {
  sum(A$x==r[1] & A$y==r[2]) +sum(A$y==r[1] & A$x==r[2]) +sum(r[1] == r[2])
}

data<-read.csv('./data/US_reg_data_infr2.csv')
data<-data.frame(data)
nna<-data[!is.na(data['err_fulltime']),]
len<-length(nna[,1])
w<-matrix(0, len, len)
stateabbr <- nna$state_abbr
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

library(raster)
library(spdep)
ww<-mat2listw(w, style='B')
err<-c()
for (i in 1:len){
  err<-c(err, as.numeric(as.character(nna['err_fulltime'][i, 1])))
}
moran.test(err, ww, randomisation=TRUE, alternative = 'two.sided')