



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



mux <- matrix(NA,nrow=100,ncol=14)
for(i in c(1:100)){
  for(j in 1:14){
    td <- (max(c(dat$sp.p[i,j],dat$ep.p[i,j]))-min(c(dat$sp.p[i,j],dat$ep.p[i,j])))
    if(td<0.1) 
      td <- 0.1
    mux[i,j] <- (dat$sp.p[i,j]*dat$ep.p[i,j])/td
  }
}
index <- which(mux>600)
mux[index] <- NA
muy <- matrix(NA,nrow=100,ncol=14)
for(i in c(1:100)){
  for(j in 1:14){
    muy[i,j] <- (min(c(dat$sp.p[i,j],dat$ep.p[i,j]))/min(c(dat$sp.pi[i,j],dat$ep.pi[i,j]))+
                   max(c(dat$sp.p[i,j],dat$ep.p[i,j]))/max(c(dat$sp.pi[i,j],dat$ep.pi[i,j])))/2
    
  }
}

muy[index] <- NA




pdf("Figure_2.pdf",width=12.5,height=3.45*2,fonts=c("serif","Palatino"))
par(mar=c(2,0,0,0),oma=c(3.5,10.5,3,5))
par(mfrow=c(2,3))

#EP

coE <- log(read.csv("DP_D_co.csv",header = F))[,-1]
coS <- log(read.csv("DP_P_co.csv",header=F))[,-1]

moE <- log(read.csv("D_mo.csv",header=F))[,-1]
moS <- log(read.csv("P_mo.csv",header=F))[,-1]


dat <- list(ep.p=coE,sp.p=coS,ep.pi=moE,sp.pi=moS)

mux <- matrix(NA,nrow=100,ncol=14)
for(i in c(1:100)){
  for(j in 1:14){
    td <- (max(c(dat$sp.p[i,j],dat$ep.p[i,j]))-min(c(dat$sp.p[i,j],dat$ep.p[i,j])))
    if(td<0.1) 
      td <- 0.1
    mux[i,j] <- (dat$sp.p[i,j]*dat$ep.p[i,j])/td
  }
}
index <- which(mux>600)
mux[index] <- NA
muy <- matrix(NA,nrow=100,ncol=14)
for(i in c(1:100)){
  for(j in 1:14){
    muy[i,j] <- (min(c(dat$sp.p[i,j],dat$ep.p[i,j]))/min(c(dat$sp.pi[i,j],dat$ep.pi[i,j]))+
                   max(c(dat$sp.p[i,j],dat$ep.p[i,j]))/max(c(dat$sp.pi[i,j],dat$ep.pi[i,j])))/2
    
  }
}

muy[index] <- NA

###2-1
sp <- muy
es <- mux
splag <- sp[,1:2]
splin <- sp[,3:5]
spa <- sp[,-c(1:5)]

plot(NA,NA,type="n",lwd=2,col="#1C86EE55",xlab=" ",
     ylab=" ",cex.lab=1.5,mgp = c(2.7, 1, 0),xaxt="n", yaxt="n",xaxs="i", yaxs="i",
     xlim=c(0.76,1.62),ylim=c(-40,638))
a1 <- unlist(c(splag));b1 <- unlist(c(es[,c(1:2)]))
segments(1,-100,1,1000,lwd=1,col="#9BCD9B");#segments(-10,1,1,1,lwd=1,col="#9BCD9B")
Arrows(0.8,500,1.5,500,col="#9BCD9B",code=3,arr.type="triangle")
a1 <- a1[!is.na(a1)];b1 <- b1[!is.na(b1)]
points(a1,b1,cex=2,col="#FFA07A")
a1.o <- order(a1)
a11 <- a1[a1.o]; b11 <- b1[a1.o]

#>1
ii1 <- which(a11>0)
a111 <- a11[ii1];b111 <- b11[ii1]
lo11 <- lm(b111 ~ poly(a111,1))
lines(a111,predict(lo11,data.frame(x=a111)),col="red",lwd=3)
text(0.76+(1.62-0.76)*0.75,638*0.92,"r=0.182",cex=1.8,font=3)
#mtext("Lag",3,line=0.5,cex=1.6)
#mtext(expression(I["mu"]),2,line=5,cex=1.6,font=1)
axis(1,seq(0.8,1.6,0.2),rep("",5),las=1,cex.axis=1.7,tck=-0.05,mgp=c(2.5,1.8,0))
axis(2,seq(0,600,100),seq(0,600,100),las=1,cex.axis=2,tck=-0.05,mgp=c(2.5,1.6,0))
text(0.92,540,"Competition",cex=1.6,font=2);text(1.25,540,"Cooperation",cex=1.6,font=2)
mtext("A",3,line=-1.2,cex=1.6,font=1,adj=-0.25)
#mtext("Mathematical Descriptor",2,line=8,cex=1.8)
#mtext(expression(Z["mu"]),2,line=7.5,cex=1.8,font=1,adj=1.32)
mtext(expression(paste("Mathematical Descriptor"," (",Z["mu"],")",sep="")),2,line=7,cex=1.8,adj=1.7)

###2-2
plot(NA,NA,type="n",lwd=2,col="#1C86EE55",xlab=" ",
     ylab=" ",cex.lab=1.5,mgp = c(2.7, 1, 0),xaxt="n", yaxt="n",xaxs="i", yaxs="i",
     xlim=c(0.65,1.72),ylim=c(-40,638))
segments(1,-100,1,1000,lwd=1,col="#9BCD9B");
#segments(1,-10,1,10,lwd=1,col="#9BCD9B");segments(-10,1,1,1,lwd=1,col="#9BCD9B")
Arrows(0.68,500,1.55,500,col="#9BCD9B",code=3,arr.type="triangle")

points(unlist(c(splin)),unlist(c(es[,c(3:5)])),cex=2,col="#FFA07A")
a2 <- unlist(c(splin));b2 <- unlist(c(es[,c(3:5)]))
a2.o <- order(a2)
a22 <- a2[a2.o]; b22 <- b2[a2.o]
a22 <- a22[!is.na(a22)];b22 <- b22[!is.na(b22)]
#b22[1] <- -200;b22[length(b22)] <- 500

#>1
ii1 <- which(a22>0)
a221 <- a22[ii1];b221 <- b22[ii1]
lo21 <- lm(b221 ~ poly(a221,1))
lines(a221,predict(lo21,data.frame(x=a221)),col="red",lwd=3)
#mtext("Linear",3,line=0.5,cex=1.6)
axis(1,seq(0.7,1.7,0.2),rep("",6),las=1,cex.axis=1.7,tck=-0.05,mgp=c(2.5,1.8,0))
#mtext(expression(paste("Strength of Mutualism"," (",M[u],")",sep="")),1,line=4.1,cex=1.6)
text(0.65+(1.72-0.65)*0.75,638*0.92,"r=0.211",cex=1.8,font=3)
text(0.83,540,"Competition",cex=1.6,font=2);text(1.25,540,"Cooperation",cex=1.6,font=2)


###2-3
plot(NA,NA,type="n",lwd=2,col="#1C86EE55",xlab=" ",
     ylab=" ",cex.lab=1.5,mgp = c(2.7, 1, 0),xaxt="n", yaxt="n",xaxs="i", yaxs="i",
     xlim=c(0.62,1.52),ylim=c(-40,638))
segments(1,-100,1,1000,lwd=1,col="#9BCD9B");
#segments(1,-10,1,10,lwd=1,col="#9BCD9B");segments(-10,1,1,1,lwd=1,col="#9BCD9B")
Arrows(0.65,500,1.4,500,col="#9BCD9B",code=3,arr.type="triangle")

points(unlist(c(spa)),unlist(c(es[,-c(1:5)])),cex=2,col="#FFA07A")
#mtext("Asymptotic",3,line=0.5,cex=1.6)
axis(1,seq(0.7,1.5,0.2),rep("",5),las=1,cex.axis=1.7,tck=-0.05,mgp=c(2.5,1.8,0))
axis(4,seq(0,600,100),seq(0,600,100),las=1,cex.axis=2,tck=-0.05,mgp=c(2.5,1.6,0))
text(0.62+(1.52-0.62)*0.75,638*0.92,"r=0.223",cex=1.8,font=3)
a2 <- unlist(c(spa));b2 <- unlist(c(es[,-c(1:5)]))
a2.o <- order(a2)
a22 <- a2[a2.o]; b22 <- b2[a2.o]
a222 <- a22[!is.na(a22)];b222 <- b22[!is.na(b22)]

#>1
ii1 <- which(a222>0)
a2221 <- a222[ii1];b2221 <- b222[ii1]
lo21 <- lm(b2221 ~ poly(a2221,2))
lines(a2221,predict(lo21,data.frame(x=a2221)),col="red",lwd=3)
text(0.82,540,"Competition",cex=1.6,font=2);text(1.23,540,"Cooperation",cex=1.6,font=2)







coE <- log(read.csv("JP_J_co.csv",header = F))[,-1]
coS <- log(read.csv("JP_P_co.csv",header=F))[,-1]

moE <- log(read.csv("J_mo.csv",header=F))[,-1]
moS <- log(read.csv("P_mo.csv",header=F))[,-1]


dat <- list(ep.p=coE,sp.p=coS,ep.pi=moE,sp.pi=moS)

mux <- matrix(NA,nrow=100,ncol=14)
for(i in c(1:100)){
  for(j in 1:14){
    td <- (max(c(dat$sp.p[i,j],dat$ep.p[i,j]))-min(c(dat$sp.p[i,j],dat$ep.p[i,j])))
    if(td<0.1) 
      td <- 0.1
    mux[i,j] <- (dat$sp.p[i,j]*dat$ep.p[i,j])/td
  }
}
index <- which(mux>600)
mux[index] <- NA
muy <- matrix(NA,nrow=100,ncol=14)
for(i in c(1:100)){
  for(j in 1:14){
    muy[i,j] <- (min(c(dat$sp.p[i,j],dat$ep.p[i,j]))/min(c(dat$sp.pi[i,j],dat$ep.pi[i,j]))+
                   max(c(dat$sp.p[i,j],dat$ep.p[i,j]))/max(c(dat$sp.pi[i,j],dat$ep.pi[i,j])))/2
    
  }
}

muy[index] <- NA



###2-1
sp <- muy
es <- mux
splag <- sp[,1:2]
splin <- sp[,3:5]
spa <- sp[,-c(1:5)]

plot(NA,NA,type="n",lwd=2,col="#1C86EE55",xlab=" ",
     ylab=" ",cex.lab=1.5,mgp = c(2.7, 1, 0),xaxt="n", yaxt="n",xaxs="i", yaxs="i",
     xlim=c(0.76,1.62),ylim=c(-40,638))
a1 <- unlist(c(splag));b1 <- unlist(c(es[,c(1:2)]))
segments(1,-100,1,1000,lwd=1,col="#9BCD9B");#segments(-10,1,1,1,lwd=1,col="#9BCD9B")
Arrows(0.8,500,1.5,500,col="#9BCD9B",code=3,arr.type="triangle")
a1 <- a1[!is.na(a1)];b1 <- b1[!is.na(b1)]
points(a1,b1,cex=2,col="purple")
a1.o <- order(a1)
a11 <- a1[a1.o]; b11 <- b1[a1.o]

##>1
ii1 <- which(a11>0)
a111 <- a11[ii1];b111 <- b11[ii1]
lo11 <- lm(b111 ~ poly(a111,1))
lines(a111,predict(lo11,data.frame(x=a111)),col="red",lwd=3)
text(0.76+(1.62-0.76)*0.75,638*0.92,"r=0.161",cex=1.8,font=3)
#mtext("Lag",3,line=0.5,cex=1.6)
#mtext(expression(z["mu"]),2,line=5,cex=1.6,font=1)
axis(1,seq(0.8,1.6,0.2),c(0.8,"1.0",seq(1.2,1.6,0.2)),las=1,cex.axis=1.7,tck=-0.05,mgp=c(2.5,1.8,0))
axis(2,seq(0,600,100),seq(0,600,100),las=1,cex.axis=2,tck=-0.05,mgp=c(2.5,1.6,0))
text(0.92,540,"Competition",cex=1.6,font=2);text(1.25,540,"Cooperation",cex=1.6,font=2)
mtext("B",3,line=-1.2,cex=1.6,font=1,adj=-0.25)

###2-2
plot(NA,NA,type="n",lwd=2,col="#1C86EE55",xlab=" ",
     ylab=" ",cex.lab=1.5,mgp = c(2.7, 1, 0),xaxt="n", yaxt="n",xaxs="i", yaxs="i",
     xlim=c(0.63,1.72),ylim=c(-40,638))
segments(1,-100,1,1000,lwd=1,col="#9BCD9B");
#segments(1,-10,1,10,lwd=1,col="#9BCD9B");segments(-10,1,1,1,lwd=1,col="#9BCD9B")
Arrows(0.68,500,1.55,500,col="#9BCD9B",code=3,arr.type="triangle")

points(unlist(c(splin)),unlist(c(es[,c(3:5)])),cex=2,col="purple")
a2 <- unlist(c(splin));b2 <- unlist(c(es[,c(3:5)]))
a2.o <- order(a2)
a22 <- a2[a2.o]; b22 <- b2[a2.o]
a22 <- a22[!is.na(a22)];b22 <- b22[!is.na(b22)]
#b22[1] <- -200;b22[length(b22)] <- 500

##>1
ii1 <- which(a22>0)
b221 <- b22[ii1];a221 <- a22[ii1]
lo21 <- lm(b221 ~ poly(a221,1))
lines(a221,predict(lo21,data.frame(x=a221)),col="red",lwd=3)
#mtext("Linear",3,line=0.5,cex=1.6)
axis(1,seq(0.7,1.7,0.2),seq(0.7,1.7,0.2),las=1,cex.axis=1.7,tck=-0.05,mgp=c(2.5,1.8,0))
mtext(expression(paste("Strength of Mutualism"," (",M[u],")",sep="")),1,line=4.3,cex=1.6)
text(0.63+(1.72-0.63)*0.75,638*0.92,"r=0.268",cex=1.8,font=3)
text(0.83,540,"Competition",cex=1.6,font=2);text(1.25,540,"Cooperation",cex=1.6,font=2)


###2-3
plot(NA,NA,type="n",lwd=2,col="#1C86EE55",xlab=" ",
     ylab=" ",cex.lab=1.5,mgp = c(2.7, 1, 0),xaxt="n", yaxt="n",xaxs="i", yaxs="i",
     xlim=c(0.62,1.52),ylim=c(-40,638))
segments(1,-100,1,1000,lwd=1,col="#9BCD9B");
#segments(1,-10,1,10,lwd=1,col="#9BCD9B");segments(-10,1,1,1,lwd=1,col="#9BCD9B")
Arrows(0.65,500,1.4,500,col="#9BCD9B",code=3,arr.type="triangle")

points(unlist(c(spa)),unlist(c(es[,-c(1:5)])),cex=2,col="purple")
#mtext("Asymptotic",3,line=0.5,cex=1.6)
axis(1,seq(0.7,1.5,0.2),seq(0.7,1.5,0.2),las=1,cex.axis=1.7,tck=-0.05,mgp=c(2.5,1.8,0))
axis(4,seq(0,600,100),seq(0,600,100),las=1,cex.axis=2,tck=-0.05,mgp=c(2.5,1.6,0))
text(0.62+(1.52-0.62)*0.75,638*0.92,"r=0.273",cex=1.8,font=3)
a2 <- unlist(c(spa));b2 <- unlist(c(es[,-c(1:5)]))
a2.o <- order(a2)
a22 <- a2[a2.o]; b22 <- b2[a2.o]
a222 <- a22[!is.na(a22)];b222 <- b22[!is.na(b22)]
b222[length(b222)] <- 500

##>1
ii1 <- which(a222>0)
a2221 <- a222[ii1];b2221 <- b222[ii1]
lo21 <- lm(b2221 ~ poly(a2221,2))
lines(a2221,predict(lo21,data.frame(x=a2221)),col="red",lwd=3)
text(0.82,540,"Competition",cex=1.6,font=2);text(1.23,540,"Cooperation",cex=1.6,font=2)

dev.off()