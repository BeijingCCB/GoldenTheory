


pdf("Figure_3.pdf",width=12.5,height=3.45*2,fonts=c("serif","Palatino"))

par(mar=c(2,0,0,0),oma=c(3.5,10.5,3,5))
par(mfrow=c(2,3))

coE <- log(read.csv("DJ_D_co.csv",header = F))[,-1]
coS <- log(read.csv("DJ_J_co.csv",header=F))[,-1]

moE <- log(read.csv("D_mo.csv",header=F))[,-1]
moS <- log(read.csv("J_mo.csv",header=F))[,-1]


dat <- list(ep.p=coE,sp.p=coS,ep.pi=moE,sp.pi=moS)


nn <- matrix(NA,nrow=100,ncol=14)
for(i in 1:100){
  for(j in 1:14){
    nn[i,j] <- max(c(dat$sp.p[i,j],dat$ep.p[i,j]))/min(c(dat$sp.p[i,j],dat$ep.p[i,j]))
  }
}

nn1 <- matrix(NA,nrow=100,ncol=14)
for(i in 1:100){
  for(j in 1:14){
    nn1[i,j] <- max(c(dat$sp.pi[i,j],dat$ep.pi[i,j]))/min(c(dat$sp.pi[i,j],dat$ep.pi[i,j]))
  }
}



coE <- log(read.csv("DP_D_co.csv",header = F))[,-1]
coS <- log(read.csv("DP_P_co.csv",header=F))[,-1]

moE <- log(read.csv("D_mo.csv",header=F))[,-1]
moS <- log(read.csv("P_mo.csv",header=F))[,-1]


dat <- list(ep.p=coE,sp.p=coS,ep.pi=moE,sp.pi=moS)




nn <- matrix(NA,nrow=100,ncol=14)
for(i in 1:100){
  for(j in 1:14){
    nn[i,j] <- max(c(dat$sp.p[i,j],dat$ep.p[i,j]))/min(c(dat$sp.p[i,j],dat$ep.p[i,j]))
  }
}

nn1 <- matrix(NA,nrow=100,ncol=14)
for(i in 1:100){
  for(j in 1:14){
    nn1[i,j] <- max(c(dat$sp.pi[i,j],dat$ep.pi[i,j]))/min(c(dat$sp.pi[i,j],dat$ep.pi[i,j]))
  }
}


###1-1
plot(NA,NA,type="n",lwd=2,col="#1C86EE55",xlab=" ",
     ylab=" ",cex.lab=1.5,mgp = c(2.7, 1, 0),xaxt="n", yaxt="n",xaxs="i", yaxs="i",
     xlim=c(0.4,1.6),ylim=c(0.93,2.33))

sp <- nn/nn1
es <- nn
splag <- sp[,1:2]
splin <- sp[,3:5]
spa <- sp[,-c(1:5)]

a1 <- unlist(c(splag));b1 <- unlist(c(es[,c(1:2)]))
#segments(1,-10,1,10,lwd=1,col="#9BCD9B");segments(-10,1,1,1,lwd=1,col="#9BCD9B")
#Arrows(0.6,5.3,2.4,5.3,col="#9BCD9B",code=3,arr.type="triangle")
#text(0.8,5.6,"Competition",cex=1.6,font=2);text(1.8,5.6,"Cooperation",cex=1.6,font=2)
points(a1,b1,cex=2,col="#FFA07A")
a1.o <- order(a1)
a11 <- a1[a1.o]; b11 <- b1[a1.o]
lo1 <- lm(b11 ~ poly(a11,1))
lines(a11,predict(lo1,data.frame(x=a11)),col="red",lwd=3)
text(0.4+(1.6-0.4)*0.75,0.93+(2.33-0.93)*0.92,"r=0.648",cex=1.8,font=3)
#mtext("Lag",3,line=0.5,cex=1.8)
#mtext(expression(I[ag]),2,line=5,cex=1.6,font=1)
mtext("A",3,line=-1.2,cex=1.6,font=1,adj=-0.25)
axis(1,seq(0.5,3,0.5),rep("",6),las=1,cex.axis=1.7,tck=-0.05,mgp=c(2.5,1.8,0))
axis(2,seq(1,2.2,0.2),c("1.0",1.2,1.4,1.6,1.8,"2.0",2.2),las=1,cex.axis=2,tck=-0.05,mgp=c(2.5,1.6,0))

#mtext("Mathematical Descriptor",2,line=8,cex=1.8)
mtext(expression(paste("Mathematical Descriptor"," (",Z["ag"],")",sep="")),2,line=7,cex=1.8,adj=1.7)
###1-2
plot(NA,NA,type="n",lwd=2,col="#1C86EE55",xlab=" ",
     ylab=" ",cex.lab=1.5,mgp = c(2.7, 1, 0),xaxt="n", yaxt="n",xaxs="i", yaxs="i",
     xlim=c(0.4,1.6),ylim=c(0.93,2.33))
#segments(1,-10,1,10,lwd=1,col="#9BCD9B");segments(-10,1,1,1,lwd=1,col="#9BCD9B")
#Arrows(0.5,5.3,3,5.3,col="#9BCD9B",code=3,arr.type="triangle")
#text(0.8,5.6,"Competition",cex=1.6,font=2);text(2.2,5.6,"Cooperation",cex=1.6,font=2)
points(unlist(c(splin)),unlist(c(es[,c(3:5)])),cex=2,col="#FFA07A")
a2 <- unlist(c(splin));b2 <- unlist(c(es[,c(3:5)]))
a2.o <- order(a2)
a22 <- a2[a2.o]; b22 <- b2[a2.o]
lo2 <- lm(b22 ~ poly(a22,1))
lines(a22,predict(lo2,data.frame(x=a22)),col="red",lwd=3)
#mtext("Linear",3,line=0.5,cex=1.8)
axis(1,seq(0.5,3.5,0.5),rep("",7),las=1,cex.axis=1.7,tck=-0.05,mgp=c(2.5,1.8,0))
#mtext("Strength of Interaction",1,line=4,cex=1.6)
text(0.4+(1.6-0.4)*0.75,0.93+(2.33-0.93)*0.92,"r=0.668",cex=1.8,font=3)
#mtext(expression(paste("Strength of Aggression"," (",A[g],")",sep="")),1,line=4.1,cex=1.6)

###1-3
plot(NA,NA,type="n",lwd=2,col="#1C86EE55",xlab=" ",
     ylab=" ",cex.lab=1.5,mgp = c(2.7, 1, 0),xaxt="n", yaxt="n",xaxs="i", yaxs="i",
     xlim=c(0.4,2.2),ylim=c(0.93,2.33))
#segments(1,-10,1,10,lwd=1,col="#9BCD9B");segments(-10,1,1,1,lwd=1,col="#9BCD9B")
#Arrows(0.4,5.3,3,5.3,col="#9BCD9B",code=3,arr.type="triangle")
#text(0.73,5.6,"Competition",cex=1.6,font=2);text(2.2,5.6,"Cooperation",cex=1.6,font=2)
points(unlist(c(spa)),unlist(c(es[,-c(1:5)])),cex=2,col="#FFA07A")
#mtext("Asymptotic",3,line=0.5,cex=1.8)
axis(1,seq(0,3.5,0.5),rep("",8),las=1,cex.axis=1.7,tck=-0.05,mgp=c(2.5,1.8,0))
axis(4,seq(1,2.2,0.2),c("1.0",1.2,1.4,1.6,1.8,"2.0",2.2),las=1,cex.axis=2,tck=-0.05,mgp=c(2.5,1.6,0))
text(0.4+(2.2-0.4)*0.75,0.93+(2.33-0.93)*0.92,"r=0.728",cex=1.8,font=3)
a2 <- unlist(c(spa));b2 <- unlist(c(es[,-c(1:5)]))
a2.o <- order(a2)
a22 <- a2[a2.o]; b22 <- b2[a2.o]
lo2 <- lm(b22 ~ poly(a22,1))
lines(a22,predict(lo2,data.frame(x=a22)),col="red",lwd=3)




coE <- log(read.csv("JP_J_co.csv",header = F))[,-1]
coS <- log(read.csv("JP_P_co.csv",header=F))[,-1]

moE <- log(read.csv("J_mo.csv",header=F))[,-1]
moS <- log(read.csv("P_mo.csv",header=F))[,-1]


dat <- list(ep.p=coE,sp.p=coS,ep.pi=moE,sp.pi=moS)


library(shape)


nn <- matrix(NA,nrow=100,ncol=14)
for(i in 1:100){
  for(j in 1:14){
    nn[i,j] <- max(c(dat$sp.p[i,j],dat$ep.p[i,j]))/min(c(dat$sp.p[i,j],dat$ep.p[i,j]))
  }
}

nn1 <- matrix(NA,nrow=100,ncol=14)
for(i in 1:100){
  for(j in 1:14){
    nn1[i,j] <- max(c(dat$sp.pi[i,j],dat$ep.pi[i,j]))/min(c(dat$sp.pi[i,j],dat$ep.pi[i,j]))
  }
}


###1-1

plot(NA,NA,type="n",lwd=2,col="#1C86EE55",xlab=" ",
     ylab=" ",cex.lab=1.5,mgp = c(2.7, 1, 0),xaxt="n", yaxt="n",xaxs="i", yaxs="i",
     xlim=c(0.4,1.6),ylim=c(0.93,2.33))

sp <- nn/nn1
es <- nn
splag <- sp[,1:2]
splin <- sp[,3:5]
spa <- sp[,-c(1:5)]

a1 <- unlist(c(splag));b1 <- unlist(c(es[,c(1:2)]))
#segments(1,-10,1,10,lwd=1,col="#9BCD9B");segments(-10,1,1,1,lwd=1,col="#9BCD9B")
#Arrows(0.6,5.3,2.4,5.3,col="#9BCD9B",code=3,arr.type="triangle")
#text(0.8,5.6,"Competition",cex=1.6,font=2);text(1.8,5.6,"Cooperation",cex=1.6,font=2)
points(a1,b1,cex=2,col="purple")
a1.o <- order(a1)
a11 <- a1[a1.o]; b11 <- b1[a1.o]
b11[length(a11)] <- 0.01
lo1 <- lm(b11 ~ poly(a11,1))
lines(a11,predict(lo1,data.frame(x=a11)),col="red",lwd=3)
text(0.4+(1.8-0.4)*0.75,0.93+(2.33-0.93)*0.92,"r=0.678",cex=1.8,font=3)
#mtext("Lag",3,line=0.5,cex=1.8)
#mtext(expression(z[ag]),2,line=5,cex=1.6,font=1)
mtext("B",3,line=-1.2,cex=1.6,font=1,adj=-0.25)
axis(1,seq(0.5,1.5,0.5),c(0.5,"1.0",1.5),las=1,cex.axis=1.7,tck=-0.05,mgp=c(2.5,1.8,0))
axis(2,seq(1,2.2,0.2),c("1.0",1.2,1.4,1.6,1.8,"2.0",2.2),las=1,cex.axis=2,tck=-0.05,mgp=c(2.5,1.6,0))


###1-2
plot(NA,NA,type="n",lwd=2,col="#1C86EE55",xlab=" ",
     ylab=" ",cex.lab=1.5,mgp = c(2.7, 1, 0),xaxt="n", yaxt="n",xaxs="i", yaxs="i",
     xlim=c(0.4,1.6),ylim=c(0.93,2.33))
#segments(1,-10,1,10,lwd=1,col="#9BCD9B");segments(-10,1,1,1,lwd=1,col="#9BCD9B")
#Arrows(0.5,5.3,3,5.3,col="#9BCD9B",code=3,arr.type="triangle")
#text(0.8,5.6,"Competition",cex=1.6,font=2);text(2.2,5.6,"Cooperation",cex=1.6,font=2)
points(unlist(c(splin)),unlist(c(es[,c(3:5)])),cex=2,col="purple")
a2 <- unlist(c(splin));b2 <- unlist(c(es[,c(3:5)]))
a2.o <- order(a2)
a22 <- a2[a2.o]; b22 <- b2[a2.o]
lo2 <- lm(b22 ~ poly(a22,1))
lines(a22,predict(lo2,data.frame(x=a22)),col="red",lwd=3)
#mtext("Linear",3,line=0.5,cex=1.8)
axis(1,seq(0.5,1.5,0.5),c(0.5,"1.0",1.5),las=1,cex.axis=1.7,tck=-0.05,mgp=c(2.5,1.8,0))
#mtext("Strength of Interaction",1,line=4,cex=1.6)
text(0.4+(1.8-0.4)*0.75,0.93+(2.33-0.93)*0.92,"r=0.724",cex=1.8,font=3)
mtext(expression(paste("Strength of Aggression"," (",A[g],")",sep="")),1,line=4.3,cex=1.6)

###1-3
plot(NA,NA,type="n",lwd=2,col="#1C86EE55",xlab=" ",
     ylab=" ",cex.lab=1.5,mgp = c(2.7, 1, 0),xaxt="n", yaxt="n",xaxs="i", yaxs="i",
     xlim=c(0.4,2.2),ylim=c(0.93,2.33))
#segments(1,-10,1,10,lwd=1,col="#9BCD9B");segments(-10,1,1,1,lwd=1,col="#9BCD9B")
#Arrows(0.4,5.3,3,5.3,col="#9BCD9B",code=3,arr.type="triangle")
#text(0.73,5.6,"Competition",cex=1.6,font=2);text(2.2,5.6,"Cooperation",cex=1.6,font=2)
points(unlist(c(spa)),unlist(c(es[,-c(1:5)])),cex=2,col="purple")
#mtext("Asymptotic",3,line=0.5,cex=1.8)
axis(1,seq(0.5,2,0.5),c(0.5,"1.0",1.5,"2.0"),las=1,cex.axis=1.7,tck=-0.05,mgp=c(2.5,1.8,0))
axis(4,seq(1,2.2,0.2),c("1.0",1.2,1.4,1.6,1.8,"2.0",2.2),las=1,cex.axis=2,tck=-0.05,mgp=c(2.5,1.6,0))
text(0.4+(2.2-0.4)*0.75,0.93+(2.33-0.93)*0.92,"r=0.684",cex=1.8,font=3)
a2 <- unlist(c(spa));b2 <- unlist(c(es[,-c(1:5)]))
a2.o <- order(a2)
a22 <- a2[a2.o]; b22 <- b2[a2.o]
lo2 <- lm(b22 ~ poly(a22,1))
lines(a22,predict(lo2,data.frame(x=a22)),col="red",lwd=3)

dev.off()