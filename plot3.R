igraph.Arrows <- function (x1, y1, x2, y2,
                           code=2,
                           size= 1,     
                           width= 1.2/4/cin,
                           open=TRUE,
                           sh.adj=0.1, 
                           sh.lwd=1,
                           sh.col=if(is.R()) par("fg") else 1,
                           sh.lty=1,
                           h.col=sh.col,
                           h.col.bo=sh.col,
                           h.lwd=sh.lwd,
                           h.lty=sh.lty,
                           curved=FALSE,com=TRUE,com1=11)
## Author: Andreas Ruckstuhl, refined by Rene Locher
## Version: 2005-10-17
{
  cin <- size * par("cin")[2]
  width <- width * (1.2/4/cin)
  uin <- if (is.R()) 
    1/xyinch()
  else par("uin")
  x <- sqrt(seq(0, cin^2, length = floor(35 * cin) + 2))
  delta <-  sqrt(h.lwd)*par("cin")[2]*0.005      ## has been 0.05
  x.arr <- c(-rev(x), -x)
  wx2 <- width * x^2
  y.arr <- c(-rev(wx2 + delta), wx2 + delta)
  deg.arr <- c(atan2(y.arr, x.arr), NA)
  r.arr <- c(sqrt(x.arr^2 + y.arr^2), NA)
  
  ## backup
  bx1 <- x1 ; bx2 <- x2 ; by1 <- y1 ; by2 <- y2
  
  ## shaft
  lx <- length(x1)
  r.seg <- rep(cin*sh.adj, lx)
  theta1 <- atan2((y1 - y2) * uin[2], (x1 - x2) * uin[1])
  th.seg1 <- theta1 + rep(atan2(0, -cin), lx)
  theta2 <- atan2((y2 - y1) * uin[2], (x2 - x1) * uin[1])
  th.seg2 <- theta2 + rep(atan2(0, -cin), lx)
  x1d <- y1d <- x2d <- y2d <- 0
  if (code %in% c(1,3)) {
    x2d <- r.seg*cos(th.seg2)/uin[1]
    y2d <- r.seg*sin(th.seg2)/uin[2]
  }
  if (code %in% c(2,3)) {
    x1d <- r.seg*cos(th.seg1)/uin[1]
    y1d <- r.seg*sin(th.seg1)/uin[2]
  }
  if (is.logical(curved) && all(!curved)) {
    #segments(x1+x1n.x2 <- cosa*(d-0.02*d)+c.x1[i];n.y2 <- sina*(d-0.02*d)+c.y1[i]d, y1+y1d, x2+x2d, y2+y2d, lwd=sh.lwd, col=sh.col, lty=sh.lty)
    phi <- atan2(y1-y2, x1-x2)
    r <- sqrt( (x1-x2)^2 + (y1-y2)^2 )
    lc.x <- x2 + 2/3*r*cos(phi)
    lc.y <- y2 + 2/3*r*sin(phi)
  } else {
    if (is.numeric(curved)) {
      lambda <- curved
    } else {
      lambda <- as.logical(curved) * 0.5
    }
    c.x1 <- x1+x1d
    c.y1 <- y1+y1d
    c.x2 <- x2+x2d
    c.y2 <- y2+y2d
    for(i in 1:length(c.x1)){
      d <- sqrt((c.y2[i]-c.y1[i])^2+(c.x2[i]-c.x1[i])^2)
      sina <- (c.y2[i]-c.y1[i])/d;cosa <- (c.x2[i]-c.x1[i])/d
      #cat(length(c.x1),"\n")
      n.x2 <- cosa*(d-0.04*d)+c.x1[i];n.y2 <- sina*(d-0.08*d)+c.y1[i]
      n.x1 <- cosa*(d-0.99*d)+c.x1[i];n.y1 <- sina*(d-0.92*d)+c.y1[i]
      if(com1==11){
        
        if(com){
          n.x2 <- cosa*(d-0.01*d)+c.x1[i];n.y2 <- sina*(d-0.01*d)+c.y1[i]
          n.x1 <- cosa*(d-0.99*d)+c.x1[i];n.y1 <- sina*(d-0.99*d)+c.y1[i]
          Arrows(n.x1,n.y1, n.x2, n.y2,lwd=e.w[i],col=coll,arr.type=rr1[i],
                 arr.length=0.35,arr.width=0.35,arr.adj=0.4,code=1)
        }else{
          n.x2 <- cosa*(d-0.01*d)+c.x1[i];n.y2 <- sina*(d-0.01*d)+c.y1[i]
          n.x1 <- cosa*(d-0.99*d)+c.x1[i];n.y1 <- sina*(d-0.99*d)+c.y1[i]
          Arrows(n.x1,n.y1, n.x2, n.y2,lwd=e.w[i],col=coll,arr.type=rr1[i],##aaaaa970"
                 arr.length=0.2,arr.width=0.2,arr.adj=0.4,code=2)
        }
      }else{
        n.x2 <- cosa*(d-0.01*d)+c.x1[i];n.y2 <- sina*(d-0.01*d)+c.y1[i]
        n.x1 <- cosa*(d-0.99*d)+c.x1[i];n.y1 <- sina*(d-0.99*d)+c.y1[i]
        segments(n.x1,n.y1, n.x2, n.y2,lwd=e.w[i],col=coll)
      }
    }
    
    midx <- (x1+x2)/2
    midy <- (y1+y2)/2  
    spx <- midx - lambda * 1/2 * (c.y2-c.y1)
    spy <- midy + lambda * 1/2 * (c.x2-c.x1)
    sh.col <- rep(sh.col, length=length(c.x1))
    sh.lty <- rep(sh.lty, length=length(c.x1))
    sh.lwd <- rep(sh.lwd, length=length(c.x1))
    lc.x <- lc.y <- numeric(length(c.x1))
    for (i in seq_len(length(c.x1))) {
      spl <- xspline(x=c(c.x1[i],spx[i],c.x2[i]),
                     y=c(c.y1[i],spy[i],c.y2[i]), shape=1, draw=FALSE)
      #lines(spl, lwd=sh.lwd[i], col=sh.col[i], lty=sh.lty[i])
      if (code %in% c(2,3)) {
        x1[i] <- spl$x[3*length(spl$x)/4]
        y1[i] <- spl$y[3*length(spl$y)/4]
      }
      if (code %in% c(1,3)) {
        x2[i] <- spl$x[length(spl$x)/4]
        y2[i] <- spl$y[length(spl$y)/4]
      }
      lc.x[i] <- spl$x[2/3 * length(spl$x)]
      lc.y[i] <- spl$y[2/3 * length(spl$y)]
    }
  }
  
  ## forward arrowhead
  if (code %in% c(2,3)) {    
    theta <- atan2((by2 - y1) * uin[2], (bx2 - x1) * uin[1])
    Rep <- rep(length(deg.arr), lx)
    p.x2 <- rep(bx2, Rep)
    p.y2 <- rep(by2, Rep)
    ttheta <- rep(theta, Rep) + rep(deg.arr, lx)
    r.arr <- rep(r.arr, lx)  
    #if(open) lines((p.x2 + r.arr * cos(ttheta)/uin[1]),
    #(p.y2 + r.arr*sin(ttheta)/uin[2]), 
    #lwd=h.lwd, col = h.col.bo, lty=h.lty) else
    #polygon(p.x2 + r.arr * cos(ttheta)/uin[1], p.y2 + r.arr*sin(ttheta)/uin[2], 
    #col = h.col, lwd=h.lwd,
    #border=h.col.bo, lty=h.lty)
  }
  
  ## backward arrow head
  if (code %in% c(1,3)) {
    x1 <- bx1; y1 <- by1
    tmp <- x1 ; x1 <- x2 ; x2 <- tmp
    tmp <- y1 ; y1 <- y2 ; y2 <- tmp
    theta <- atan2((y2 - y1) * uin[2], (x2 - x1) * uin[1])
    lx <- length(x1)
    Rep <- rep(length(deg.arr), lx)
    p.x2 <- rep(x2, Rep)
    p.y2 <- rep(y2, Rep)
    ttheta <- rep(theta, Rep) + rep(deg.arr, lx)
    r.arr <- rep(r.arr, lx)
    
    #if(open) lines((p.x2 + r.arr * cos(ttheta)/uin[1]),
    #(p.y2 + r.arr*sin(ttheta)/uin[2]), 
    #lwd=h.lwd, col = h.col.bo, lty=h.lty) else
    #polygon(p.x2 + r.arr * cos(ttheta)/uin[1], p.y2 + r.arr*sin(ttheta)/uin[2], 
    #col = h.col, lwd=h.lwd,
    #border=h.col.bo, lty=h.lty)
  }
  
  list(lab.x=lc.x, lab.y=lc.y)
  
}