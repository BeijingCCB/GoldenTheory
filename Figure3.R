pdf("Figure_3.pdf",width=12.5,height=3.45*2,fonts=c("serif","Palatino"))

par(mar=c(2,0,0,0),oma=c(3.5,10.5,3,5))
par(mfrow=c(2,3))


coE <- log(read.csv("DJ_D_co.csv",header = F))[,-1]
coS <- log(read.csv("DJ_J_co.csv",header=F))[,-1]

moE <- log(read.csv("D_mo.csv",header=F))[,-1]
moS <- log(read.csv("J_mo.csv",header=F))[,-1]

dat <- list(ep.p=coE,sp.p=coS,ep.pi=moE,sp.pi=moS)

xx <- matrix(NA,nrow=100,ncol=14)
yy <- matrix(NA,nrow=100,ncol=14)
xx1 <- matrix(NA,nrow=100,ncol=14)
library(shape)

for(i in 1:100){
  for(j in 1:14){
    yy[i,j] <- 1-min(c(dat$sp.p[i,j],dat$ep.p[i,j]))/max(c(dat$sp.p[i,j],dat$ep.p[i,j]))
  }
}

for(i in 1:100){
  for(j in 1:14){
    tmp <- max(c(dat$sp.p[i,j],dat$ep.p[i,j]))
    index <- which(tmp==c(dat$sp.p[i,j],dat$ep.p[i,j]))
    if(index==1){
      xx[i,j] <- tmp/dat$sp.pi[i,j]
    }else{
      xx[i,j] <- tmp/dat$ep.pi[i,j]
    }
  }
}


for(i in 1:100){
  for(j in 1:14){
    tmp <- min(c(dat$sp.p[i,j],dat$ep.p[i,j]))
    index <- which(tmp==c(dat$sp.p[i,j],dat$ep.p[i,j]))
    if(index==1){
      xx1[i,j] <- tmp/dat$sp.pi[i,j]
    }else{
      xx1[i,j] <- tmp/dat$ep.pi[i,j]
    }
  }
}

es <- yy
nxx <- xx/xx1

splag <- nxx[,1:2]
splin <- nxx[,3:5]
spa <- nxx[,-c(1:5)]



coE <- log(read.csv("DP_D_co.csv",header = F))[,-1]
coS <- log(read.csv("DP_P_co.csv",header=F))[,-1]

moE <- log(read.csv("D_mo.csv",header=F))[,-1]
moS <- log(read.csv("P_mo.csv",header=F))[,-1]


dat <- list(ep.p=coE,sp.p=coS,ep.pi=moE,sp.pi=moS)

xx <- matrix(NA,nrow=100,ncol=14)
yy <- matrix(NA,nrow=100,ncol=14)
xx1 <- matrix(NA,nrow=100,ncol=14)

for(i in 1:100){
  for(j in 1:14){
    yy[i,j] <- 1-min(c(dat$sp.p[i,j],dat$ep.p[i,j]))/max(c(dat$sp.p[i,j],dat$ep.p[i,j]))
  }
}

for(i in 1:100){
  for(j in 1:14){
    tmp <- max(c(dat$sp.p[i,j],dat$ep.p[i,j]))
    index <- which(tmp==c(dat$sp.p[i,j],dat$ep.p[i,j]))
    if(index==1){
      xx[i,j] <- tmp/dat$sp.pi[i,j]
    }else{
      xx[i,j] <- tmp/dat$ep.pi[i,j]
    }
  }
}


for(i in 1:100){
  for(j in 1:14){
    tmp <- min(c(dat$sp.p[i,j],dat$ep.p[i,j]))
    index <- which(tmp==c(dat$sp.p[i,j],dat$ep.p[i,j]))
    if(index==1){
      xx1[i,j] <- tmp/dat$sp.pi[i,j]
    }else{
      xx1[i,j] <- tmp/dat$ep.pi[i,j]
    }
  }
}

es <- yy
nxx <- xx/xx1

splag <- nxx[,1:2]
splin <- nxx[,3:5]
spa <- nxx[,-c(1:5)]

plot(NA,NA,type="n",lwd=3,col="#1C86EE55",xlab=" ",
     ylab=" ",cex.lab=1.5,mgp = c(2.7, 1, 0),xaxt="n", yaxt="n",xaxs="i", yaxs="i",
     xlim=c(0.45,2.2),ylim=c(-0.05,0.63))
a1 <- unlist(c(splag));b1 <- unlist(c(es[,c(1:2)]))
#segments(-10,1,1,1,lwd=1,col="#9BCD9B")
#Arrows(0.6,370*0.12,1.4,370*0.12,col="#9BCD9B",code=3,arr.type="triangle")
points(a1,b1,cex=2,col="#FFA07A")
segments(1,-100,1,1000,lwd=1,col="#9BCD9B");
a1.o <- order(a1)
a22 <- a1[a1.o]; b22 <- b1[a1.o]
b221 <- b22;a221 <- a22
lo21 <- lm(b221 ~ poly(a221,2))
lines(a221,predict(lo21,data.frame(x=a221)),col="red",lwd=3)
text(0.25+(2.2-0.25)*0.75,0.63*0.93,"r=0.381",cex=1.8,font=3)
#mtext("Lag",3,line=0.5,cex=1.6)
#mtext(expression(I[al]),2,line=4.6,cex=1.6,font=1)
axis(1,seq(0.5,2,0.5),rep("",4),las=1,cex.axis=1.7,tck=-0.05,mgp=c(2.5,1.8,0))
axis(2,seq(-0.2,0.8,0.2),seq(-0.2,0.8,0.2),las=1,cex.axis=2,tck=-0.05,mgp=c(2.5,1.6,0))
#text(0.8,370*0.16,"Cooperation",cex=1.2,font=2);text(1.2,370*0.16,"Competition",cex=1.2,font=2)
mtext("A",3,line=-1.2,cex=1.6,font=1,adj=-0.25)
mtext(expression(paste("Mathematical Descriptor"," (",Z["al"],")",sep="")),2,line=7,cex=1.8,adj=1.7)

plot(NA,NA,type="n",lwd=3,col="#1C86EE55",xlab=" ",
     ylab=" ",cex.lab=1.5,mgp = c(2.7, 1, 0),xaxt="n", yaxt="n",xaxs="i", yaxs="i",
     xlim=c(0.45,2.2),ylim=c(-0.05,0.63))

#segments(1,-10,1,10,lwd=1,col="#9BCD9B");segments(-10,1,1,1,lwd=1,col="#9BCD9B")
#Arrows(0.6,370*0.12,1.4,370*0.12,col="#9BCD9B",code=3,arr.type="triangle")

points(unlist(c(splin)),unlist(c(es[,c(3:5)])),cex=2,col="#FFA07A")
segments(1,-100,1,1000,lwd=1,col="#9BCD9B");
a2 <- unlist(c(splin));b2 <- unlist(c(es[,c(3:5)]))
a2.o <- order(a2)
a22 <- a2[a2.o]; b22 <- b2[a2.o]
b221 <- b22;a221 <- a22
lo21 <- lm(b221 ~ poly(a221,2))
lines(a221,predict(lo21,data.frame(x=a221)),col="red",lwd=3)
#mtext("Linear",3,line=0.5,cex=1.6)
axis(1,seq(0.5,2,0.5),rep("",4),las=1,cex.axis=1.7,tck=-0.05,mgp=c(2.5,1.8,0))
#mtext(expression(paste("Strength of Altruism"," (",A[l],")",sep="")),1,line=4.1,cex=1.6)

text(0.45+(2.2-0.45)*0.75,0.63*0.93,"r=0.342",cex=1.8,font=3)
#text(0.8,370*0.16,"Cooperation",cex=1.2,font=2);text(1.2,370*0.16,"Competition",cex=1.2,font=2)


plot(NA,NA,type="n",lwd=3,col="#1C86EE55",xlab=" ",
     ylab=" ",cex.lab=1.5,mgp = c(2.7, 1, 0),xaxt="n", yaxt="n",xaxs="i", yaxs="i",
     xlim=c(0.43,2.73),ylim=c(-0.05,0.63))

#segments(1,-10,1,10,lwd=1,col="#9BCD9B");segments(-10,1,1,1,lwd=1,col="#9BCD9B")
#Arrows(0.6,370*0.12,1.4,370*0.12,col="#9BCD9B",code=3,arr.type="triangle")

points(unlist(c(spa)),unlist(c(es[,-c(1:5)])),cex=2,col="#FFA07A")
segments(1,-100,1,1000,lwd=1,col="#9BCD9B");
#mtext("Asymptotic",3,line=0.5,cex=1.6)
axis(1,seq(0.5,2.5,0.5),rep("",5),las=1,cex.axis=1.7,tck=-0.05,mgp=c(2.5,1.8,0))
axis(4,seq(-0.2,0.8,0.2),seq(-0.2,0.8,0.2),las=1,cex.axis=2,tck=-0.05,mgp=c(2.5,1.6,0))
text(0.43+(2.73-0.43)*0.75,0.63*0.93,"r=0.462",cex=1.8,font=3)
a2 <- unlist(c(spa));b2 <- unlist(c(es[,-c(1:5)]))
a2.o <- order(a2)
a22 <- a2[a2.o]; b22 <- b2[a2.o]
b221 <- b22[-length(b22)];a221 <- a22[-length(b22)]
lo21 <- lm(b221 ~ poly(a221,2))
lines(a221,predict(lo21,data.frame(x=a221)),col="red",lwd=3)
#text(0.77,370*0.16,"Cooperation",cex=1.2,font=2);text(1.23,370*0.16,"Competition",cex=1.2,font=2)



coE <- log(read.csv("JP_J_co.csv",header = F))[,-1]
coS <- log(read.csv("JP_P_co.csv",header=F))[,-1]

moE <- log(read.csv("J_mo.csv",header=F))[,-1]
moS <- log(read.csv("P_mo.csv",header=F))[,-1]


dat <- list(ep.p=coE,sp.p=coS,ep.pi=moE,sp.pi=moS)


xx <- matrix(NA,nrow=100,ncol=14)
yy <- matrix(NA,nrow=100,ncol=14)
xx1 <- matrix(NA,nrow=100,ncol=14)
library(shape)

for(i in 1:100){
  for(j in 1:14){
    yy[i,j] <- 1-min(c(dat$sp.p[i,j],dat$ep.p[i,j]))/max(c(dat$sp.p[i,j],dat$ep.p[i,j]))
  }
}

for(i in 1:100){
  for(j in 1:14){
    tmp <- max(c(dat$sp.p[i,j],dat$ep.p[i,j]))
    index <- which(tmp==c(dat$sp.p[i,j],dat$ep.p[i,j]))
    if(index==1){
      xx[i,j] <- tmp/dat$sp.pi[i,j]
    }else{
      xx[i,j] <- tmp/dat$ep.pi[i,j]
    }
  }
}


for(i in 1:100){
  for(j in 1:14){
    tmp <- min(c(dat$sp.p[i,j],dat$ep.p[i,j]))
    index <- which(tmp==c(dat$sp.p[i,j],dat$ep.p[i,j]))
    if(index==1){
      xx1[i,j] <- tmp/dat$sp.pi[i,j]
    }else{
      xx1[i,j] <- tmp/dat$ep.pi[i,j]
    }
  }
}

es <- yy
nxx <- xx/xx1

splag <- nxx[,1:2]
splin <- nxx[,3:5]
spa <- nxx[,-c(1:5)]

plot(NA,NA,type="n",lwd=3,col="#1C86EE55",xlab=" ",
     ylab=" ",cex.lab=1.5,mgp = c(2.7, 1, 0),xaxt="n", yaxt="n",xaxs="i", yaxs="i",
     xlim=c(0.45,2.2),ylim=c(-0.05,0.63))
a1 <- unlist(c(splag));b1 <- unlist(c(es[,c(1:2)]))
#segments(-10,1,1,1,lwd=1,col="#9BCD9B")
#Arrows(0.6,370*0.12,1.4,370*0.12,col="#9BCD9B",code=3,arr.type="triangle")
points(a1,b1,cex=2,col="purple")
#segments(1,-100,1,1000,lwd=1,col="#9BCD9B");
a1.o <- order(a1)
a22 <- a1[a1.o]; b22 <- b1[a1.o]
b221 <- b22;a221 <- a22
lo21 <- lm(b221 ~ poly(a221,2))
lines(a221,predict(lo21,data.frame(x=a221)),col="red",lwd=3)
text(0.45+(2.2-0.45)*0.75,0.63*0.93,"r=0.517",cex=1.8,font=3)
#mtext("Lag",3,line=0.5,cex=1.6)
#mtext(expression(z[al]),2,line=4.6,cex=1.6,font=1)
axis(1,seq(0.5,2,0.5),c(0.5,"1.0",1.5,"2.0"),las=1,cex.axis=1.7,tck=-0.05,mgp=c(2.5,1.8,0))
axis(2,seq(-0.2,0.6,0.2),seq(-0.2,0.6,0.2),las=1,cex.axis=2,tck=-0.05,mgp=c(2.5,1.6,0))
#text(0.8,370*0.16,"Cooperation",cex=1.2,font=2);text(1.2,370*0.16,"Competition",cex=1.2,font=2)
mtext("B",3,line=-1.2,cex=1.6,font=1,adj=-0.25)

plot(NA,NA,type="n",lwd=3,col="#1C86EE55",xlab=" ",
     ylab=" ",cex.lab=1.5,mgp = c(2.7, 1, 0),xaxt="n", yaxt="n",xaxs="i", yaxs="i",
     xlim=c(0.45,2.2),ylim=c(-0.05,0.63))

#segments(1,-10,1,10,lwd=1,col="#9BCD9B");segments(-10,1,1,1,lwd=1,col="#9BCD9B")
#Arrows(0.6,370*0.12,1.4,370*0.12,col="#9BCD9B",code=3,arr.type="triangle")

points(unlist(c(splin)),unlist(c(es[,c(3:5)])),cex=2,col="purple")
#segments(1,-100,1,1000,lwd=1,col="#9BCD9B");
a2 <- unlist(c(splin));b2 <- unlist(c(es[,c(3:5)]))
a2.o <- order(a2)
a22 <- a2[a2.o]; b22 <- b2[a2.o]
b221 <- b22;a221 <- a22
lo21 <- lm(b221 ~ poly(a221,2))
lines(a221,predict(lo21,data.frame(x=a221)),col="red",lwd=3)
#mtext("Linear",3,line=0.5,cex=1.6)
axis(1,seq(0.5,2,0.5),c(0.5,"1.0",1.5,"2.0"),las=1,cex.axis=1.7,tck=-0.05,mgp=c(2.5,1.8,0))
mtext(expression(paste("Strength of Altruism"," (",A[l],")",sep="")),1,line=4.2,cex=1.6)

text(0.45+(2.2-0.45)*0.75,0.63*0.93,"r=0.406",cex=1.8,font=3)
#text(0.8,370*0.16,"Cooperation",cex=1.2,font=2);text(1.2,370*0.16,"Competition",cex=1.2,font=2)


plot(NA,NA,type="n",lwd=3,col="#1C86EE55",xlab=" ",
     ylab=" ",cex.lab=1.5,mgp = c(2.7, 1, 0),xaxt="n", yaxt="n",xaxs="i", yaxs="i",
     xlim=c(0.43,2.73),ylim=c(-0.05,0.63))

#segments(1,-10,1,10,lwd=1,col="#9BCD9B");segments(-10,1,1,1,lwd=1,col="#9BCD9B")
#Arrows(0.6,370*0.12,1.4,370*0.12,col="#9BCD9B",code=3,arr.type="triangle")

points(unlist(c(spa)),unlist(c(es[,-c(1:5)])),cex=2,col="purple")
#segments(1,-100,1,1000,lwd=1,col="#9BCD9B");
#mtext("Asymptotic",3,line=0.5,cex=1.6)
axis(1,seq(0.5,2.5,0.5),c(0.5,"1.0",1.5,"2.0",2.5),las=1,cex.axis=1.7,tck=-0.05,mgp=c(2.5,1.8,0))
axis(4,seq(-0.2,0.8,0.2),seq(-0.2,0.8,0.2),las=1,cex.axis=2,tck=-0.05,mgp=c(2.5,1.6,0))
text(0.43+(2.73-0.43)*0.75,0.63*0.93,"r=0.488",cex=1.8,font=3)
a2 <- unlist(c(spa));b2 <- unlist(c(es[,-c(1:5)]))
a2.o <- order(a2)
a22 <- a2[a2.o]; b22 <- b2[a2.o]
b221 <- b22[-length(b22)];a221 <- a22[-length(b22)]
lo21 <- lm(b221 ~ poly(a221,2))
lines(a221,predict(lo21,data.frame(x=a221)),col="red",lwd=3)
#text(0.77,370*0.16,"Cooperation",cex=1.2,font=2);text(1.23,370*0.16,"Competition",cex=1.2,font=2)
dev.off()
