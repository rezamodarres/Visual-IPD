# R code for Iris data (Figure 6) in Graphical Comparison of High Dimensional Distributions. 
# To appear in International Statistical Review.
# Comparing 3 groups of Iris using the distribution of IPDs

#library(pbivnorm)
library(MASS)
rm(list = ls()) #clear workspace
attach(iris)
dm=cbind(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
setosa=dm[1:50,]
versicolor=dm[51:100,]
virginica=dm[101:150]
nx=50
ny=50
nz=50
n=nx+ny+nz
mx=nx*(nx-1)/2
my=ny*(ny-1)/2
mz=nz*(nz-1)/2
m=n*(n-1)/2
mxy=nx*ny
mxz=nx*nz
myz=ny*nz
d=4
samplex <- t(setosa)
sampley <- t(versicolor)
samplez <- t(virginica)
count=1
ipdx=rep(0,mx)
for(i in 1:(nx-1)){
  for(j in (i+1):nx){
    ipdx[count]=sqrt((t(samplex[,i]-samplex[,j]) %*% (samplex[,i]-samplex[,j])))
    count <- count+1
  }
}
count=1
ipdy=rep(0,my)
for(i in 1:(ny-1)){
  for(j in (i+1):ny){
    ipdy[count]=sqrt((t(sampley[,i]-sampley[,j]) %*% (sampley[,i]-sampley[,j])))
    count <- count+1
  }
}
count=1
ipdz=rep(0,mz)
for(i in 1:(nz-1)){
  for(j in (i+1):nz){
    ipdz[count]=sqrt((t(samplez[,i]-samplez[,j]) %*% (samplez[,i]-samplez[,j])))
    count <- count+1
  }
}
count=1
ipdxy=rep(0,mxy)
for(i in 1:nx){
  for(j in 1:ny){
    ipdxy[count]=sqrt((t(samplex[,i]-sampley[,j]) %*% (samplex[,i]-sampley[,j])))
    count <- count+1
  }
}
count=1
ipdxz=rep(0,mxz)
for(i in 1:nx){
  for(j in 1:nz){
    ipdxz[count]=sqrt((t(samplex[,i]-samplez[,j]) %*% (samplex[,i]-samplez[,j])))
    count <- count+1
  }
}
count=1
ipdyz=rep(0,myz)
for(i in 1:ny){
  for(j in 1:nz){
    ipdyz[count]=sqrt((t(sampley[,i]-samplez[,j]) %*% (sampley[,i]-samplez[,j])))
    count <- count+1
  }
}
ipd=c(ipdx,ipdy,ipdz,ipdxy,ipdxz,ipdyz)
ipdxbar=sum(ipdx)/mx
ipdybar=sum(ipdy)/my
ipdzbar=sum(ipdz)/mz
ipdxybar=sum(ipdxy)/mxy
ipdxzbar=sum(ipdxz)/mxz
ipdyzbar=sum(ipdyz)/myz
ipdbars=c(ipdxbar,ipdybar,ipdzbar,ipdxybar,ipdxzbar,ipdyzbar)
show(ipdbars)
minipd=min(ipd)
maxipd=max(ipd)
deltaint=100
unit=(maxipd-minipd)/deltaint
delta=seq(minipd,maxipd,unit)
cdfx=rep(0,deltaint)
cdfy=cdfx
cdfxy=cdfx
cdfz=cdfx
cdfxz=cdfx
cdfyz=cdfx
for (i in 1:deltaint){
   cdfx[i]=sum(ipdx<=delta[i])/mx
   cdfy[i]=sum(ipdy<=delta[i])/my
   cdfxy[i]=sum(ipdxy<=delta[i])/mxy
   cdfz[i]=sum(ipdz<=delta[i])/mz 
   cdfxz[i]=sum(ipdxz<=delta[i])/mxz
   cdfyz[i]=sum(ipdyz<=delta[i])/myz
   }
plot(cdfx,type="l",lty=1, col="red", lwd=1,ylab="ECDF", xlab = expression(paste(delta)))
title(main = "ECDFs of Iris Data")
lines(cdfy,lty=1,lwd=1,col="green")
lines(cdfxy,lty=1,lwd=1,col="blue")
lines(cdfz,lty=1,lwd=1,col="brown")
lines(cdfxz,lty=1,lwd=1,col="black")
lines(cdfyz,lty=1,lwd=1,col="pink")

       
 