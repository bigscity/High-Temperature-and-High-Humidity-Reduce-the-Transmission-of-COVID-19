distance<-function(lat1, lon1, lat2, lon2){
	dis<-sqrt((lat1 - lat2)^2 + (lon1 - lon2)^2)
	return(dis)
}

epsilon = 1e-6
Delta = 1e6

data<-read.csv('./data/US_reg_data_infr2.csv')
data<-data.frame(data)
nna<-data[!is.na(data['err_fulltime']),]
len<-length(nna[,1])
w<-matrix(0, len, len)
lats <- nna$lat_norm
lons <- nna$lon_norm
for (i in 1:len){
	for (j in 1:len) {
		lat1 = lats[i]
	  	lon1 = lons[i]
	  	lat2 = lats[j]
	  	lon2 = lons[j]
	  	dis<-distance(lat1, lon1, lat2, lon2)
	  	if (dis == 0) {
			dis<-Delta
		}
		w[i, j] <- 1 / (dis + epsilon)
	}
}

library(raster)
library(spdep)
ww<-mat2listw(w, style='M')
err<-c()
for (i in 1:len){
  err<-c(err, as.numeric(as.character(nna['err_fulltime'][i, 1])))
}
moran.test(err, ww, randomisation=TRUE, alternative = 'two.sided')