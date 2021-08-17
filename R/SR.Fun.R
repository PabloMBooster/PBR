"SR.Fun" <- function (type = c("Ricker","BevertonHolt"))#, param = 1, msg = FALSE) 
{
  type <- match.arg(type)
  switch(type, Ricker= {
    
    fit=lm(log(rt/st)~st)
    a <- fit$coef[1]
    b <- fit$coef[2]
    St <- seq(0,max(st),by=(max(st)/(length(st)-1))/100)
    Rt = exp(a)*St*exp(b*St)
    R_max    <- max(Rt)
    St_max   <- St[which.max(Rt)]
    St_lower <- St[round(which.max(Rt)*0.9)]
    St_upper <- St[round(which.max(Rt)*1.2)]
    
    
    #Rmax <- D(exp(a)*St*exp(b*St),"St")
    #par(mfrow=c(1,2))
    #plot(st,log(rt/st),xlab="Stock Desovante",ylab="Log(Rt/St)",pch=16,col=1)
    #points(st,fitted(fit),pch="O",col=3)
    #text(st,log(rt/st),yr,cex=0.6,pos=4)
    #png("grafica.png",width=600,height=600)
    plot(st,rt,xlab="Stock Desovante",ylab="Reclutamiento", ylim= c(0,max(rt)),
         xlim = c(0,max(st)),
         pch=16,col=1,cex=1.2,cex.lab=1,cex.axis=1)
    lines(St,Rt,lwd=3,col=2)
    #text(3200,13000,expression(R == alpha * SSB * e^(beta * SSB)),cex=1)
    points(x = St_max,y = R_max, pch = 3, cex = 2, col = 3, lwd = 3)
    text(x = St_max,y = R_max, labels = paste0(round(St_max,1)," t"), pos = 3)
    #dev.off()
    #text(st,rt,yr,cex=0.6,pos=1)#
    #print(Rt)
    x <- c(0,St_max)
    y <- c(0,R_max)
    lines(x,y,lwd=2,col=4)
    
    mFmed = R_max/St_max
    
    return(list(fit=summary(fit), aic=AIC(fit),St_max = St_max, St_lower=St_lower,St_upper=St_upper, mFmed = mFmed))#,Rmax=Rmax))
    
  }, BevertonHolt = {
    
    #fit=lm(log(rt/st)~log(a)-log(1+b*st),start=list(a=1,b=1),model=T,
           # control=list(maxiter=1000))
    fit=nls(log(rt/st)~log(a)-log(1+b*st),start=list(a=2,b=0.0001),model=T,
           control=list(maxiter=1000))
    a <- coefficients(fit)[1]
    b <- coefficients(fit)[2]
    
    St <- seq(0,max(st),by=(max(st)/(length(st)-1))/100)
    Rt=a*St/(1+b*St)
    St_max   <- St[which.max(Rt)]
    St_lower <- St[round(which.max(Rt)*0.9)]
    St_upper <- St[round(which.max(Rt)*1.2)]
    
    par(mfrow=c(1,2))
    plot(st,log(rt/st),xlab="Stock Desovante",ylab="Log(Rt/St)",pch=16,col=1)
    points(st,fitted(fit),pch="O",col=3)
    text(st,log(rt/st),yr,cex=0.6,pos=4)
    plot(st,rt,xlab="Stock Desovante",ylab="Reclutamiento",pch=16,col=1)
    lines(St,Rt,type="l",lwd=2,col=2)
    text(st,rt,yr,cex=0.6,pos=1)
    #text(3200,13000,expression(R == alpha * SSB * e^(beta * SSB)),cex=1)
    return(list(fit=summary(fit), aic=AIC(fit),St_max = St_max, St_lower=St_lower,St_upper=St_upper))
  })
}