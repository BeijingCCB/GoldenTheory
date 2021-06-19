

load("Figure5.RData")

library(igraph)
library(plotrix)

pdf("Figure5.pdf",width=20,height=22.5,fonts=c("serif","Palatino"))
par(mar=c(0,0,0,0),oma=c(0,0,0,0),bty="n")

plot(0,0,type="n",lwd=2,col="#1C86EE55",xlab=" ",
     ylab=" ",cex.lab=1.5,mgp = c(2.7, 1, 0),xaxt="n", yaxt="n",xaxs="i", yaxs="i",
     xlim=c(0,10),ylim=c(0,10))

draw.ellipse(c(5), c(5),a=1.8,b=1.6,col=NA,border="#4F94CD",lwd=3)
draw.ellipse(c(1.4), c(5),a=1.32,b=1.2,col=NA,border="#7CCD7C",lwd=3)
draw.ellipse(c(8.6), c(5),a=1.32,b=1.2,col=NA,border="#FF6A6A",lwd=3)
draw.ellipse(c(5), c(1.58),a=1.32,b=1.2,col=NA,border="#c46cf7",lwd=3)
draw.ellipse(c(5), c(8.38),a=1.32,b=1.2,col=NA,border="#FFA54F",lwd=3)
###text####

text(5,9.75,"Aggression",cex=2.5)
text(1.45,6.4,"Mutualism",cex=2.5)
text(8.55,6.38,"Antagonism",cex=2.5)
text(5,0.2,"Altruism",cex=2.5)
text(5,3.75,"Relatedness",cex=2.5)
###edge####
segments(3.7,5.9,4.1,5.9,lwd=4,col="#A6A6A6")
text(3.9,6.0,"Edge",cex=1.5,col="black")

Arrows(0.2,3.9,0.6,3.9,lwd=4,col="#A6A6A6",arr.length=0.2,arr.width=0.2,arr.adj=0.4,code=3,arr.type="triangle")
text(0.4,4.0,"Edge",cex=1.5,col="black")

Arrows(9.3,3.8,9.7,3.8,lwd=4,col="#A6A6A6",arr.length=0.35,arr.width=0.35,arr.adj=0.4,code=3,arr.type="T")
text(9.5,3.9,"Edge",cex=1.5,col="black")

Arrows(3.8,0.5,4.2,0.5,lwd=4,col="#A6A6A6",arr.length=0.2,arr.width=0.2,arr.adj=0.4,code=2,arr.type="triangle")
text(4,0.6,"Edge",cex=1.5,col="black")

Arrows(3.8,9.5,4.2,9.5,lwd=4,col="#A6A6A6",arr.length=0.35,arr.width=0.35,arr.adj=0.4,code=2,arr.type="T")
text(4,9.6,"Edge",cex=1.5,col="black")

###indicate arrow
Arrows(1.35,2.9,1.35,3.6,lwd=10,col="#A6A6A6",arr.length=0.9,arr.width=0.55,arr.adj=0.1,code=1,arr.type="triangle")
Arrows(6.6,1.5,7.2,1.5,lwd=11,col="#A6A6A6",arr.length=0.9,arr.width=0.55,arr.adj=0.1,code=2,arr.type="triangle")
Arrows(8.55,6.5,8.55,7,lwd=10,col="#A6A6A6",arr.length=0.9,arr.width=0.55,arr.adj=0.1,code=2,arr.type="triangle")
Arrows(3.5,8.4,2.8,8.4,lwd=11,col="#A6A6A6",arr.length=0.9,arr.width=0.55,arr.adj=0.1,code=2,arr.type="triangle")
#######network arrow
Arrows(5,2.8,5,3.38,lwd=8,col="#A6A6A6",arr.length=0.45,arr.width=0.45,arr.adj=0.1,code=3,arr.type="triangle")
text(5.4,3.1,"-0.063",cex=2.2)
Arrows(5,6.61,5,7.16,lwd=8,col="#A6A6A6",arr.length=0.45,arr.width=0.45,arr.adj=0.1,code=3,arr.type="triangle")
text(5.4,6.61+0.275,"-0.034",cex=2)
Arrows(6.82,5,7.26,5,lwd=8,col="#A6A6A6",arr.length=0.45,arr.width=0.45,arr.adj=0.1,code=3,arr.type="triangle")
text(6.82+0.22,5.2,"0.054",cex=2)
Arrows(2.74,5,3.18,5,lwd=8,col="#A6A6A6",arr.length=0.45,arr.width=0.45,arr.adj=0.1,code=3,arr.type="triangle")
text(2.74+0.22,5.2,"0.048",cex=2)

#Arrows(1.4,5,5,8.45,lwd=8,col="#A6A6A6",arr.length=0.45,arr.width=0.45,arr.adj=0.1,code=3,arr.type="triangle")
Arrows(2.34,5.86,4.04,7.51,lwd=10,col="#A6A6A6",arr.length=0.45,arr.width=0.45,arr.adj=0.1,code=3,arr.type="triangle")
text(3.15,6.85,"-0.841",cex=2,srt=45)
Arrows(2.34,4.14,4.03,2.44,lwd=8,col="#A6A6A6",arr.length=0.45,arr.width=0.45,arr.adj=0.1,code=3,arr.type="triangle")
text(3.10,3.15,"-0.929",cex=2,srt=315)
Arrows(7.66,5.86,5.96,7.51,lwd=8,col="#A6A6A6",arr.length=0.45,arr.width=0.45,arr.adj=0.1,code=3,arr.type="triangle")
text(6.85,6.85,"-0.336",cex=2,srt=-45)
Arrows(7.66,4.14,5.97,2.44,lwd=8,col="#A6A6A6",arr.length=0.45,arr.width=0.45,arr.adj=0.1,code=3,arr.type="triangle")
text(6.9,3.15,"-0.603",cex=2,srt=45)
################## Genetic relatedness ####################
coll <-"#A6A6A6"
allcol <- rep("#b6e5f7",71)
all.font.col <- rep("black",71)
#grnet <-network.sparse.cc(gg,CC=res$CC,cc.thred=0.904)
tmpm <- grnet$mmcc
zi <- which(colSums(tmpm)==0)
set.seed(430)
AAA <- rep(c(41,60,25,21,20,14,7),100)
AA <- sample(AAA,length(zi))
for(i in 1:length(AA)){
  tmpm[AA,zi[i]] <- 1
  tmpm[zi[i],AA] <- 1
}
diag(tmpm) <- 0
g1 <- graph.adjacency((tmpm),mode="directed",weighted=T)
V(g1)$label <- 1:71
e.w <- rep(1,2000)#c(tmpm)[which(c(tmpm)==1)]*0.15
rr1 <- rep("triangle",length(e.w))
allcol[which(igraph::degree(g1)>15)] <- "#4F94CD"
all.font.col [which(igraph::degree(g1)>15)] <- "black"

set.seed(52039+5)
par(fig=c(0.38,0.61,0.38,0.61),new=TRUE)
par(mar=c(0,0,0,0),oma=c(4,0,4,0))
plot(g1,xlim=c(-1,1),ylim=c(-1,1),
     vertex.size=12,vertex.label.cex=1.1,edge.color="#FFE7BA",
     edge.arrow.size=2,vertex.color=allcol,edge.width=e.w,
     vertex.label.color=all.font.col,vertex.frame.color="#B5B5B5",layout=layout.lgl,com=FALSE,com1=22)
#####
par(fig=c(0.29,0.39,0.43,0.65),new=TRUE)
par(mar=c(2,3,1,1),oma=c(1,2,1,1),bty="n")
plot(0,0,type="n",lwd=2,col="#1C86EE55",xlab=" ",
     ylab=" ",cex.lab=1.5,mgp = c(2.7, 1, 0),xaxt="n", yaxt="n",xaxs="i", yaxs="i",
     xlim=c(0,2),ylim=c(-5,300))
axis(1,1,"Core",cex.axis=2,lwd=3,mgp = c(2.7, 0.8, 0),tick=FALSE)
#axis(2,seq(0,280,70),seq(0,280,70),cex.axis=1.7,lwd=3,las=2)
hp <- pheno[which(igraph::degree(g1)>15),1]
mhp <- mean(hp);sdhp <- sd(hp)
rect(0.3,0,1.7,mhp,border=NA,col="#4F94CD")
segments(1,mhp-sdhp,1,mhp+sdhp,lwd=3,col="black")
segments(0.85,mhp-sdhp,1.15,mhp-sdhp,lwd=3,col="black")
segments(0.85,mhp+sdhp,1.15,mhp+sdhp,lwd=3,col="black")
#text(1,mhp+sdhp+20,"a",cex=2)
segments(0,0,2,0,lwd=3,col="black")

#####
par(fig=c(0.565,0.665,0.43,0.65),new=TRUE)
par(mar=c(2,3,1,1),oma=c(1,2,1,1),bty="n")
plot(0,0,type="n",lwd=2,col="#1C86EE55",xlab=" ",
     ylab=" ",cex.lab=1.5,mgp = c(2.7, 1, 0),xaxt="n", yaxt="n",xaxs="i", yaxs="i",
     xlim=c(0,2),ylim=c(-5,300))
axis(1,1,"Periphery",cex.axis=2,lwd=3,mgp = c(2.7, 0.8, 0),tick=FALSE)
hp <- pheno[-which(igraph::degree(g1)>15),1]
mhp <- mean(hp);sdhp <- sd(hp)
rect(0.3,0,1.7,mhp,border=NA,col="#b6e5f7")
segments(1,mhp-sdhp,1,mhp+sdhp,lwd=3,col="black")
segments(0.85,mhp-sdhp,1.15,mhp-sdhp,lwd=3,col="black")
segments(0.85,mhp+sdhp,1.15,mhp+sdhp,lwd=3,col="black")
segments(0,0,2,0,lwd=3,col="black")
#text(1,mhp+sdhp+20,"a",cex=2)
################################phenotypic mutualism##############################
source("plot1.R")
allcol <- rep("#c3f0d3",71)
all.font.col <- rep("black",71)
tmpm <- mmcp
index <- which(pheno[,1]>180)
tmpm[,index] <- 1
tmpm[index,] <- 0
#iii <- nne(t(tmpm))
net <- tmpm
nn <- dim(net)[1]
n1 <- c();n2 <- c();n3 <- c();n4 <- c();
for(i in 1:nn){
  if((all(net[i,]==0))&&(any(net[,i]!=0))){
    n1 <- c(n1,i)
  }
  if((any(net[i,]!=0))&&(all(net[,i]==0))){
    n4 <- c(n4,i)
  }
}
nsum1 <- colSums(net[-c(n1,n4),-c(n1,n4)])
alln <- c(1:71)[-c(n1,n4)]
n2 <- alln[which(nsum1==0)][-c(2,5,7,9)]
n3 <- alln[which(nsum1!=0)]

g2 <- graph.adjacency(t(tmpm),mode="directed",weighted=T)
V(g2)$label <- 1:71
e.w <- rep(1,2000)#c(tmpm)[which(c(tmpm)==1)]*0.15
rr1 <- rep("triangle",length(e.w))
allcol[n1] <-"#7CCD7C"
allcol[n2] <-"#98FB98"
allcol[n3] <-"#9ACD32"
#allcol[which(igraph::degree(g2)>12)] <- "#7CCD7C"
#all.font.col [which(igraph::degree(g2)>12)] <- "black"
set.seed(725)
par(fig=c(0.00,0.28,0.35,0.65),new=TRUE)
par(mar=c(0,0,0,0),oma=c(4,0,4,0))
plot(g2,xlim=c(-1,1),ylim=c(-1,1),
     vertex.size=10,vertex.label.cex=1.1,edge.color="#FFE7BA",
     edge.arrow.size=2,vertex.color=allcol,edge.width=e.w,
     vertex.label.color=all.font.col,vertex.frame.color="#B5B5B5",layout=layout.lgl,com=FALSE,code=3)
###
par(fig=c(0.02,0.30,0.06,0.32),new=TRUE)
par(mar=c(2,3,1,1),oma=c(1,2,1,1),bty="n")
plot(0,0,type="n",lwd=2,col="#87CEFF55",xlab=" ",
     ylab=" ",cex.lab=1.5,mgp = c(2.7, 1, 0),xaxt="n", yaxt="n",xaxs="i", yaxs="i",
     xlim=c(0,5),ylim=c(0,1.44546))

pa <- pheno1[n2]/mean(pheno1[n1])
pad <-  pheno1[n3]/mean(pheno1[n2])
pd <-  pheno1[n4]/mean(pheno1[n3])

rect(0.5,0,1.5,mean(pa),border=NA,col="#7CCD7C")
segments(1,mean(pa)-sd(pa),1,mean(pa)+sd(pa),lwd=3,col="black")
segments(0.85,mean(pa)-sd(pa),1.15,mean(pa)-sd(pa),lwd=3,col="black")
segments(0.85,mean(pa)+sd(pa),1.15,mean(pa)+sd(pa),lwd=3,col="black")
#text(1,mean(pa)+sd(pa)+20,"a",cex=2)

rect(2,0,3,mean(pad),border=NA,col="#98FB98")
segments(2.5,mean(pad)-sd(pad),2.5,mean(pad)+sd(pad),lwd=3,col="black")
segments(2.35,mean(pad)-sd(pad),2.65,mean(pad)-sd(pad),lwd=3,col="black")
segments(2.35,mean(pad)+sd(pad),2.65,mean(pad)+sd(pad),lwd=3,col="black")
#text(2.5,mean(pad)+sd(pad)+20,"b",cex=2)

rect(3.5,0,4.5,mean(pd),border=NA,col="#c3f0d3")
segments(4,mean(pd)-sd(pd),4,mean(pd)+sd(pd),lwd=3,col="black")
segments(3.85,mean(pd)-sd(pd),4.15,mean(pd)-sd(pd),lwd=3,col="black")
segments(3.85,mean(pd)+sd(pd),4.15,mean(pd)+sd(pd),lwd=3,col="black")

axis(2,seq(0,1,0.2),c("0.0",seq(0.2,0.8,0.2),"1.0"),cex.axis=1.7,lwd=3,las=2)
axis(1,c(0.50,4.4),c("Secondary/","Follower/"),cex.axis=2,lwd=3,mgp = c(2.7,1.4, 0),tick=FALSE)
axis(1,c(2.5),c("Tertiary/"),cex.axis=2,lwd=3,mgp = c(2.7, 1.4, 0),tick=FALSE)
#axis(1,c(1.9),c("Follower/Tertiary Leader"),cex.axis=0.8,las=1,lwd=1,mgp = c(2.5, 2, 0),tick=F)
axis(1,c(0.50,4.4),c("Primary","Tertiary"),cex.axis=2,lwd=3,mgp = c(2.7, 3, 0),tick=FALSE)
axis(1,c(2.5),c("Secondary"),cex.axis=2,lwd=3,mgp = c(2.7, 3, 0),tick=FALSE)
#axis(1,seq(0,5,0.1),seq(0,2.3,0.1),cex.axis=1.2,las=1,lwd=3,tick=T,labels = F,tck=F)
#segments(0,60,4.4,60,lwd=2,col="black")
segments(0,0,5,0,lwd=3,col="black")
mtext("Relative body mass",2,cex=2.2,line=4)

################################phenotypic antagonism##############################
source("./BN/plot2.R")

coll <-"#A6A6A6"
tmpm <- mmcom2
g3 <- graph.adjacency(t(tmpm),mode="directed",weighted=T)
V(g3)$label <- 1:71
e.w <- rep(1,2000)#c(net1$mmcom.i)[which(c(tmp1)==1)]*1
rr1 <- rep("T",length(e.w))
allcol <- rep("#f9cdc3",71)
allcol[which(igraph::degree(g3)>15)] <- "#FF6A6A"
all.font.col[which(igraph::degree(g3)>15)] <- "black"
par(fig=c(0.73,1,0.35,0.64),new=TRUE)
par(mar=c(0,0,0,0),oma=c(4,0,4,0))
set.seed(300)
plot(g3,xlim=c(-1,1),ylim=c(-1,1),
     vertex.size=10,vertex.label.cex=1.1,edge.color="#EE9572",
     edge.arrow.size=2,vertex.color=allcol,edge.width=e.w,
     vertex.label.color=all.font.col,vertex.frame.color="#B5B5B5",layout=layout.lgl,com=TRUE)

####
#0.02,0.28,0.06,0.32
par(fig=c(0.71,0.94,0.72,0.98),new=TRUE)
par(mar=c(2,3,1,1),oma=c(1,2,1,1),bty="n")
plot(0,0,type="n",lwd=2,col="#1C86EE55",xlab=" ",
     ylab=" ",cex.lab=1.5,mgp = c(2.7, 1, 0),xaxt="n", yaxt="n",xaxs="i", yaxs="i",
     xlim=c(0,3.5),ylim=c(0,1.21546))
st <- mean(pheno[which(igraph::degree(g3)>15),1][-c(1,2,8,9)])
axis(1,c(1),c("Antagonist"),cex.axis=2,lwd=3,mgp = c(2.7, 2.5, 0),tick=FALSE)
axis(1,c(1),c("Larger"),cex.axis=2,lwd=3,mgp = c(2.7, 0.8, 0),tick=FALSE)
axis(1,c(2.6),c("Antagonist"),cex.axis=2,lwd=3,mgp = c(2.7, 2.5, 0),tick=FALSE)
axis(1,c(2.6),c("Smaller"),cex.axis=2,lwd=3,mgp = c(2.7, 0.8, 0),tick=FALSE)
#axis(1,c(2.6),c("Antagonized"),cex.axis=1.7,lwd=3,mgp = c(2.7, 0.8, 0),tick=FALSE)
axis(4,seq(0,1,0.2),c("0.0",seq(0.2,0.8,0.2),"1.0"),cex.axis=2,lwd=3,las=2)
hp <- pheno[which(igraph::degree(g3)>15),1][-c(1,2,8,9)]/st
mhp <- mean(hp);sdhp <- sd(hp)
rect(0.5,0,1.5,mhp,border=NA,col="#FF6A6A")
segments(1,mhp-sdhp,1,mhp+sdhp,lwd=3,col="black")
segments(0.85,mhp-sdhp,1.15,mhp-sdhp,lwd=3,col="black")
segments(0.85,mhp+sdhp,1.15,mhp+sdhp,lwd=3,col="black")
#segments(1,200,1,220,lwd=3,col="black")
#segments(1,220,1.5,220,lwd=3,col="black")
#text(1,mhp+sdhp+20,"a",cex=2)
hp <- pheno[-which(igraph::degree(g3)>15),1]/st
mhp <- mean(hp);sdhp <- sd(hp)
rect(2,0,3,mhp,border=NA,col="#f9cdc3")
segments(2.5,mhp-sdhp,2.5,mhp+sdhp,lwd=3,col="black")
segments(2.35,mhp-sdhp,2.65,mhp-sdhp,lwd=3,col="black")
segments(2.35,mhp+sdhp,2.65,mhp+sdhp,lwd=3,col="black")
#segments(2.5,200,2.5,220,lwd=3,col="black")
#segments(2.5,220,2,220,lwd=3,col="black")
#text(1.75,220,"**",cex=2)
segments(0,0,3.5,0,lwd=3,col="black")
#text(2.5,mhp+sdhp+20,"b",cex=2)
mtext("Relative body mass",4,cex=2.2,line=5)
################################phenotypic Altruism##############################

coll <-"#A6A6A6"
tmpm <- mmcom3

tmpm[52,] <- 0

iii <- nne(t(tmpm))

g4 <- graph.adjacency(t(tmpm),mode="directed",weighted=T)
V(g4)$label <- 1:71
all.font.col <- rep("black",71)
e.w <- rep(1,1000)
rr1 <- rep("triangle",length(e.w))
allcol <- rep("#EE7AE9",71)
#allcol[which(igraph::degree(g4)>11)] <- "#c46cf7"
allcol[iii[[2]]] <- "purple"
allcol[iii[[1]]] <- "#efd6fd"
all.font.col[which(igraph::degree(g4)>11)] <- "black"
set.seed(700)
par(fig=c(0.37,0.64,0.01,0.28),new=TRUE)
par(mar=c(0,0,0,0),oma=c(1,0,1,0))
plot(g4,xlim=c(-1,1),ylim=c(-1,1),
     vertex.size=10,vertex.label.cex=1.1,edge.color="#EE9572",
     edge.arrow.size=2,vertex.color=allcol,edge.width=e.w,
     vertex.label.color=all.font.col,vertex.frame.color="#B5B5B5",layout=layout.lgl,com=F,code=1)

#####

par(fig=c(0.68,0.94,0.07,0.32),new=TRUE)
par(mar=c(2,3,1,1),oma=c(1,2,1,1),bty="n")
plot(0,0,type="n",lwd=2,col="#1C86EE55",xlab=" ",
     ylab=" ",cex.lab=1.5,mgp = c(2.7, 1, 0),xaxt="n", yaxt="n",xaxs="i", yaxs="i",
     xlim=c(0,3.5),ylim=c(0,1.84546))
axis(1,c(1,2.5),c("Egoist/","Egoist/"),cex.axis=2,lwd=3,mgp = c(2.7, 0.8, 0),tick=F)
axis(1,c(1,2.5),c("Altruist","Altruist"),cex.axis=2,lwd=3,mgp = c(2.7,2.6, 0),tick=FALSE)
#axis(1,c(2.5),c("Altruists-Egoists"),cex.axis=2,lwd=3,mgp = c(2.7, 2.8, 0),tick=FALSE)
axis(4,seq(0,1,0.2),c("0.0",seq(0.2,0.8,0.2),"1.0"),cex.axis=1.7,lwd=3,las=2)

#pd <- pheno1[iii[[2]]]/mean(pheno1[iii[[2]]])
pad <-  pheno1[c(iii[[3]])]/mean(pheno1[c(iii[[2]])])
pa <-  pheno1[c(iii[[1]])]/mean(pheno1[c(iii[[3]])])

text(1,0.98,"Upper",cex=2);text(1,0.83,"Tier",cex=2);
text(2.5,1.6,"Lower",cex=2);text(2.5,1.46,"Tier",cex=2);


rect(0.5,0,1.5,mean(pa),border=NA,col="#efd6fd")
segments(1,mean(pa)-sd(pa),1,mean(pa)+sd(pa),lwd=3,col="black")
segments(0.85,mean(pa)-sd(pa),1.15,mean(pa)-sd(pa),lwd=3,col="black")
segments(0.85,mean(pa)+sd(pa),1.15,mean(pa)+sd(pa),lwd=3,col="black")
#text(1,mean(pa)+sd(pa)+20,"a",cex=2)

rect(2,0,3,mean(pad),border=NA,col="purple")
segments(2.5,mean(pad)-sd(pad),2.5,mean(pad)+sd(pad),lwd=3,col="black")
segments(2.35,mean(pad)-sd(pad),2.65,mean(pad)-sd(pad),lwd=3,col="black")
segments(2.35,mean(pad)+sd(pad),2.65,mean(pad)+sd(pad),lwd=3,col="black")
#text(2.5,mean(pad)+sd(pad)+20,"b",cex=2)

#rect(3.5,0,4.5,mean(pd),border=NA,col="")
#segments(4,mean(pd)-sd(pd),4,mean(pd)+sd(pd),lwd=3,col="black")
#segments(3.85,mean(pd)-sd(pd),4.15,mean(pd)-sd(pd),lwd=3,col="black")
#segments(3.85,mean(pd)+sd(pd),4.15,mean(pd)+sd(pd),lwd=3,col="black")
#text(2.5,mhp+sdhp+20,"b",cex=2)
mtext("Relative body mass",4,cex=2.2,line=5)

segments(0,0,5,0,lwd=3,col="black")
#par(fig=c(0.33,0.66,0.01,0.30),new=TRUE)
#par(mar=c(0,0,0,0),oma=c(4,0,4,0))

#par(fig=c(0.33,0.66,0.70,0.99),new=TRUE)
#par(mar=c(0,0,0,0),oma=c(4,0,4,0))
################################phenotypic Aggression##############################
source("plot3.R")
coll <-"#A6A6A6"
#tmpm <- mmcom1
tmpm <- agg.n
for(i in 2:70){
  index <- which(tmpm[,i]==1)
  if(length(index)==0)
    next
  for(j in 1:length(index)){
    if(pheno[i,1]>pheno[index[j],1]){
      tmpm[index[j],i] <- 0
      tmpm[i,index[j]] <- 1
    }
  }
}

#tmpm[c()]
iii <- nne((tmpm))
g5 <- graph.adjacency((tmpm),mode="directed",weighted=T)
V(g5)$label <- 1:71
all.font.col <- rep("black",71)
e.w <- rep(1,1000)
rr1 <- rep("T",length(e.w))
allcol <- rep("#FFDEAD",71)
allcol[iii[[1]]] <- "#FFF68F"
allcol[iii[[2]]] <- "#FFA54F"
#allcol[which(igraph::degree(g5)>20)[c(1,5,6)]] <- ""
#allcol[which(igraph::degree(g5)>20)[-c(1,5,6)]] <- ""
#all.font.col[which(igraph::degree(g5)>20)[c(1,5,6)]] <- "black"
set.seed(393)
par(fig=c(0.368,0.64,0.705,0.975),new=TRUE)
par(mar=c(0,0,0,0),oma=c(1,0,1,0))
plot(g5,xlim=c(-1,1),ylim=c(-1,1),
     vertex.size=10,vertex.label.cex=1.1,edge.color="#EE9572",
     edge.arrow.size=2,vertex.color=allcol,edge.width=e.w,
     vertex.label.color=all.font.col,vertex.frame.color="#B5B5B5",layout=layout.lgl,com=T,code=2)
###

par(fig=c(0.02,0.26,0.72,0.98),new=TRUE)
par(mar=c(2,3,1,1),oma=c(1,2,1,1),bty="n")
plot(0,0,type="n",lwd=2,col="#1C86EE55",xlab=" ",
     ylab=" ",cex.lab=1.5,mgp = c(2.7, 1, 0),xaxt="n", yaxt="n",xaxs="i", yaxs="i",
     xlim=c(0,3.5),ylim=c(0,1.64546))
axis(1,c(1,2.5),c("Dove/","Dove/"),cex.axis=2,lwd=3,mgp = c(2.7, 0.8, 0),tick=FALSE)
axis(1,c(1,2.5),c("Hawk","Hawk"),cex.axis=2,lwd=3,mgp = c(2.7,2.6, 0),tick=FALSE)
axis(2,seq(0,1,0.2),c("0.0",seq(0.2,0.8,0.2),"1.0"),cex.axis=1.7,lwd=3,las=2)

text(1,0.90,"Upper",cex=2);text(1,0.78,"Tier",cex=2);
text(2.5,1.35,"Lower",cex=2);text(2.5,1.23,"Tier",cex=2);


pa <- pheno1[iii[[3]]]/mean(pheno1[iii[[1]]])
#pa <-  pheno1[iii[[3]]]/mean(pheno1[iii[[1]]])
pad <-  pheno1[iii[[2]]]/mean(pheno1[iii[[3]]])

rect(0.5,0,1.5,mean(pa),border=NA,col="#FFA54F")
segments(1,mean(pa)-sd(pa),1,mean(pa)+sd(pa),lwd=3,col="black")
segments(0.85,mean(pa)-sd(pa),1.15,mean(pa)-sd(pa),lwd=3,col="black")
segments(0.85,mean(pa)+sd(pa),1.15,mean(pa)+sd(pa),lwd=3,col="black")
#text(1,mean(pa)+sd(pa)+20,"a",cex=2)

rect(2,0,3,mean(pad),border=NA,col="#FFF68F")
segments(2.5,mean(pad)-sd(pad),2.5,mean(pad)+sd(pad),lwd=3,col="black")
segments(2.35,mean(pad)-sd(pad),2.65,mean(pad)-sd(pad),lwd=3,col="black")
segments(2.35,mean(pad)+sd(pad),2.65,mean(pad)+sd(pad),lwd=3,col="black")
#text(2.5,mean(pad)+sd(pad)+20,"b",cex=2)

#rect(3.5,0,4.5,mean(pd),border=NA,col="#FFF68F")
#segments(4,mean(pd)-sd(pd),4,mean(pd)+sd(pd),lwd=3,col="black")
#segments(3.85,mean(pd)-sd(pd),4.15,mean(pd)-sd(pd),lwd=3,col="black")
#segments(3.85,mean(pd)+sd(pd),4.15,mean(pd)+sd(pd),lwd=3,col="black")
#text(4,mean(pd)+sd(pd)+20,"c",cex=2)

segments(0,0,5,0,lwd=3,col="black")
mtext("Relative body mass",2,cex=2.2,line=4)
dev.off()



