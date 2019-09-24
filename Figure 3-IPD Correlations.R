# R code for Figure 3 in Graphical Comparison of High Dimensional Distributions. 
# To appear in International Statistical Review.
# Display of F=G using the distribution of IPDs
library(mvtnorm)
#library(pbivnorm)
library(MASS)
rm(list = ls()) #clear workspace
set.seed(61415926)
      nx=50 
      ny=50 
      n=nx+ny
      mx=nx*(nx-1)/2
      my=ny*(ny-1)/2
      m=n*(n-1)/2
      mxy=nx*ny
      p=1000
      samplex <- matrix(0,p,nx)
      sampley <- matrix(0,p,ny)
      rho1=0.5
      rho2=0.7 
      sigx=matrix(rho1,nrow=p, ncol=p)
      sigy=matrix(rho2,nrow=p, ncol=p)
      diag(sigx)=1
      diag(sigy)=1
      samplex <- t(rmvnorm(nx, mean=rep(0,p), sigma=sigx)) 
      sampley <- t(rmvnorm(nx, mean=rep(0,p), sigma=sigy)) 
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
      ipdxy=rep(0,mxy)
      for(i in 1:nx){
        for(j in 1:ny){
          ipdxy[count]=sqrt((t(samplex[,i]-sampley[,j]) %*% (samplex[,i]-sampley[,j])))
          count <- count+1
        }
      }
      ipdxbar=mean(ipdx)
      ipdybar=mean(ipdy)
      ipdxybar=mean(ipdxy)
      cat('ipdxy=', ipdxybar,'ipdx=', ipdxbar,'ipdy=', ipdybar,fill=TRUE)
       ipd=c(ipdx,ipdy,ipdxy)
       minipd=min(ipd)
       maxipd=max(ipd)
       s=100
       unit=(maxipd-minipd)/s
       delta=seq(minipd,maxipd,unit)
       cdfx=rep(0,s)
       cdfy=cdfx
       cdfxy=cdfx
       for (i in 1:s){
         cdfx[i]=sum(ipdx<=delta[i])/mx
         cdfy[i]=sum(ipdy<=delta[i])/my
         cdfxy[i]=sum(ipdxy<=delta[i])/mxy      
       }
       plot(cdfx,type="l",lty=1, col="red", lwd=1, ylab="ECDF",xlab=expression(paste(delta)))
       title(main = expression(paste("ECDF of IPDS when, ", rho, "x=0.5, ", rho,"y=0.7")))
       lines(cdfy,lty=1,lwd=1,col="black")
       lines(cdfxy,lty=1,lwd=1, col="blue")
       
       
 