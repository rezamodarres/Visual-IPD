# R code for Figures 1,2 and 4 in Graphical Comparison of High Dimensional Distributions. 
# To appear in International Statistical Review.
# Display of F=G using the distribution of IPDs

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
      d=10000
      samplex <- matrix(0,d,nx)
      sampley <- matrix(0,d,ny)
      df=2
      for (i in 1:d){ 
        # To obtain Figure 1:
         samplex[i,] <- rnorm(n = nx,mean = 0,sd = 1) 
         sampley[i,] <- rnorm(n = ny,mean = 0.5 ,sd = 1) 
        # To obtain Figure 2a:
        # samplex[i,] <- rt(nx,df) 
        # sampley[i,] <- sqrt(2)*rt(ny,df)
        # To obtain Figure 4:
        # samplex[i,]=rbinom(nx, 1, .5)
        # sampley[i,]=rbinom(ny, 1, .6)
        } 
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
     # hist(ipdx)
     # hist(ipdy)
     # hist(ipdxy)
      ipdxbar=mean(ipdx)
      ipdybar=mean(ipdy)
      ipdxybar=mean(ipdxy)
      cat('Mean ipdxy=', ipdxybar,'Mean ipdx=', ipdxbar,'Mean ipdy=', ipdybar,fill=TRUE)
      ipd=c(ipdx,ipdy,ipdxy)
      minipd=min(ipd)
      maxipd=max(ipd)
      s=100
      unit=(maxipd-minipd)/s
      delta=seq(minipd,maxipd,unit)
      cdfx=rep(0,s)
      cdfy=rep(0,s)
      cdfxy=rep(0,s)
      for (i in 1:s){
       cdfx[i]=sum(ipdx<=delta[i])/mx
       cdfy[i]=sum(ipdy<=delta[i])/my
       cdfxy[i]=sum(ipdxy<=delta[i])/mxy      
      }
      plot(cdfx,type="l",lty=1, col="red", lwd=1, ylab="ECDF", 
            xlab = expression(paste(delta)))
      title(main = "ECDFs of within and between IPDs")
      lines(cdfy,lty=1,lwd=1,col="black")
      lines(cdfxy,lty=1,lwd=1, col="blue")
 