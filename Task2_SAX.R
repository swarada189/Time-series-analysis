library(TSclust)


## plot the original data
sc <- read.table('D:\\UoA\\Data Mining\\Time Series1\\synthetic_control.data',header=F, sep="")
#idx <- c(1,101,201,301,401,501)
ts1 <- t(sc[1,])
ts2 <- t(sc[101,])
ts3 <- t(sc[201,])
ts4 <- t(sc[301,])
ts5 <- t(sc[401,])
ts6 <- t(sc[501,])

ts.length= dim(ts1)[1]					#60 x 6 matrix
ts.max <- max(max(ts1),max(ts2),max(ts3),max(ts4),max(ts5),max(ts6))		
ts.min <- min(min(ts1),min(ts2),min(ts3),min(ts4),min(ts5),min(ts6))

##PLOT the series
mycolors = rainbow(6)
dev.new()							#new geaphical device(window)
plot(NULL, xlim=c(1,ts.length), ylim=c(ts.min,ts.max), main="6 classes of Time Series", xlab="Time", ylab="Syn Control Chart")
lines(ts1, lwd=2, col=mycolors[1])
points(ts1, pch=18, cex=1.5,lwd=2 ,col=mycolors[1])
lines(ts2, lwd=2, col=mycolors[2])
points(ts2, pch=18, cex=1.5,lwd=2 ,col=mycolors[2])
lines(ts3, lwd=2, col=mycolors[3])
points(ts3, pch=18, cex=1.5, lwd=2, col=mycolors[3])
lines(ts4, lwd=2, col=mycolors[4])
points(ts4, pch=18, cex=1.5, lwd=2, col=mycolors[4])
lines(ts5, lwd=2, col=mycolors[5])
points(ts5, pch=18, cex=1.5, lwd=2, col=mycolors[5])
lines(ts6, lwd=2, col=mycolors[6])
points(ts6, pch=18, cex=1.5, lwd=2, col=mycolors[6])


ts1.norm <- (ts1 - mean(ts1))/sd(ts1)
ts2.norm <- (ts2 - mean(ts2))/sd(ts2)
ts3.norm <- (ts3 - mean(ts3))/sd(ts3)
ts4.norm <- (ts4 - mean(ts4))/sd(ts4)
ts5.norm <- (ts5 - mean(ts5))/sd(ts5)
ts6.norm <- (ts6 - mean(ts6))/sd(ts6)

## PAA 
paa1 <- PAA(ts(ts1.norm),5)
paa2 <- PAA(ts(ts2.norm),5)
paa3 <- PAA(ts(ts3.norm),5)
paa4 <- PAA(ts(ts4.norm),5)
paa5 <- PAA(ts(ts5.norm),5)
paa6 <- PAA(ts(ts6.norm),5)

## plot PAA
dev.new()
par(mfrow=c(2,3))
plot(ts1.norm,type="l")
p <- rep(paa1,each=length(ts1.norm)/length(paa1))
lines(p,col=mycolors[1])

plot(ts2.norm,type="l")
p <- rep(paa2,each=length(ts2.norm)/length(paa2))
lines(p,col=mycolors[2])

plot(ts3.norm,type="l")
p <- rep(paa3,each=length(ts3.norm)/length(paa3))
lines(p,col=mycolors[3])

plot(ts4.norm,type="l")
p <- rep(paa4,each=length(ts4.norm)/length(paa4))
lines(p,col=mycolors[4])

plot(ts5.norm,type="l")
p <- rep(paa5,each=length(ts5.norm)/length(paa5))
lines(p,col=mycolors[5])

plot(ts6.norm,type="l")
p <- rep(paa6,each=length(ts6.norm)/length(paa6))
lines(p,col=mycolors[6])

##SAX
sax1 <- convert.to.SAX.symbol(ts(ts1.norm),5)
sax2 <- convert.to.SAX.symbol(ts(ts2.norm),5)
sax3 <- convert.to.SAX.symbol(ts(ts3.norm),5)
sax4 <- convert.to.SAX.symbol(ts(ts4.norm),5)
sax5 <- convert.to.SAX.symbol(ts(ts5.norm),5)
sax6 <- convert.to.SAX.symbol(ts(ts6.norm),5)

##plot SAX
dev.new()
par(mfrow=c(2,3))
SAX.plot(as.ts(cbind(ts1.norm,ts2.norm,ts3.norm,ts4.norm,ts5.norm,ts6.norm)),w=5,alpha=5)

#SAX.plot(as.ts(ts1.norm),w=5,alpha=5)
#SAX.plot(as.ts(ts2.norm),w=5,alpha=5)
#SAX.plot(as.ts(ts3.norm),w=5,alpha=5)
#SAX.plot(as.ts(ts4.norm),w=5,alpha=5)
#SAX.plot(as.ts(ts5.norm),w=5,alpha=5)
#SAX.plot(as.ts(ts6.norm),w=5,alpha=5)
