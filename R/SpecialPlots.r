
# clumsy plot code to be revised


## plots: PlotFaces (Chernoff-Faces) ====

# aus TeachingDemos, Author: H. P. Wolf
# updated with newer version, edited and simplified by 0.99.24

# Source aplpack, Author: H. P. Wolf

PlotFaces <- function(xy = rbind(1:3,5:3,3:5,5:7), which.row, fill = FALSE, nr, nc,
                       scale = TRUE, byrow = FALSE, main, labels,
                       col = "white") {

  ncolors <- nrow(xy)

  col <- matrix(rep(col, length.out=nrow(xy) * 6), ncol=nrow(xy))

  col.nose <- col[1, ]
  col.eyes <- col[2, ]
  col.hair <- col[3, ]
  col.face <- col[4, ]
  col.lips <- col[5, ]
  col.ears <- col[6, ]

  n <- nrow(xy)

  if(missing(nr)) nr <- n^0.5
  if(missing(nc)) nc <- n^0.5

  opar <- par(mfrow=c(ceiling(c(nr, nc))),
              oma=rep(6, 4),
              mar=rep(.7, 4))
  on.exit(par(opar))


  spline <- function(a, y, m=200, plot=FALSE) {
    n <- length(a)
    h <- diff(a)
    dy <- diff(y)
    sigma <- dy/h
    lambda <- h[-1] / (hh <- h[-1] + h[-length(h)])
    mu <- 1-lambda
    d <- 6 * diff(sigma)/hh
    tri.mat <- 2 * diag(n-2)
    tri.mat[2 + (0:(n-4))*(n-1)] <- mu[-1]
    tri.mat[(1:(n-3)) * (n-1)] <- lambda[-(n-2)]
    M <- c(0,solve(tri.mat) %*% d, 0)
    x <- seq(from=a[1], to=a[n], length=m)
    anz.kl  <-  hist(x, breaks=a, plot=FALSE)$counts

    adj <- function(i) i-1

    i <- rep(1:(n-1), anz.kl) + 1
    S.x <- M[i-1]*(a[i]-x)^3 / (6*h[adj(i)])  +
      M[i] * (x-a[i-1])^3 / (6*h[adj(i)])  +
      (y[i-1] - M[i-1] * h[adj(i)]^2 /6) * (a[i]-x)/ h[adj(i)] +
      (y[i] - M[i] * h[adj(i)]^2 /6) * (x-a[i-1]) / h[adj(i)]

    if(plot){
      plot(x, S.x, type="l")
      points(a, y)
    }

    return(cbind(x, S.x))

  }

  n.char <- 15
  xy <- rbind(xy)

  if(byrow) xy <- t(xy)

  # if(any(is.na(xy))){
  #   if(na.rm){
  #     xy <- xy[!apply(is.na(xy),1,any),,drop=FALSE]
  #     if(nrow(xy)<3) {print("not enough data points"); return()}
  #     print("Warning: NA elements have been removed!!")
  #   }else{
  #     xy.means <- colMeans(xy,na.rm=TRUE)
  #     for(j in 1:length(xy[1,])) xy[is.na(xy[,j]),j] <- xy.means[j]
  #     print("Warning: NA elements have been exchanged by mean values!!")
  #   }
  # }

  if(!missing(which.row) && all(!is.na(match(which.row,1:dim(xy)[2])) ))
    xy <- xy[, which.row, drop=FALSE]
  mm <- dim(xy)[2]
  n <- dim(xy)[1]
  xnames <- dimnames(xy)[[1]]
  if(is.null(xnames)) xnames <- as.character(1:n)
  if(!missing(labels)) xnames <- labels

  if(scale){
    xy <- apply(xy,2,function(x){
      x <- x-min(x); x <- if(max(x)>0) 2*x/max(x)-1 else x })
  } else xy[] <- pmin(pmax(-1,xy),1)

  xy <- rbind(xy)
  n.c <- dim(xy)[2]

  # expand input matrix xy by replication of cols
  xy <- xy[,(rows.orig <- h <- rep(1:mm,ceiling(n.char/mm))),drop=FALSE]
  if(fill) xy[,-(1:n.c)] <- 0

  face.orig <- list(
    eye   = rbind(c(12,0),c(19,8),c(30,8),c(37,0),c(30,-8),c(19,-8),c(12,0)),
    iris  = rbind(c(20,0),c(24,4),c(29,0),c(24,-5),c(20,0)),
    lipso = rbind(c(0,-47),c( 7,-49), lipsiend=c(16,-53), c( 7,-60),c(0,-62)),
    lipsi = rbind(c(7,-54),c(0,-54)),
    nose  = rbind(c(0,-6),c(3,-16),c(6,-30),c(0,-31)),
    shape = rbind(c(0,44),c(29,40),c(51,22),hairend=c(54,11),earsta=c(52,-4),
                  earend=c(46,-36),c(38,-61),c(25,-83),c(0,-89)),
    ear   = rbind(c(60,-11), c(57,-30)),                # add earsta,earend
    hair  = rbind(hair1=c(72,12), hair2=c(64,50), c(36,74), c(0,79)) # add hairend
  )
  lipso.refl.ind <- 4:1
  lipsi.refl.ind <- 1
  nose.refl.ind  <- 3:1
  hair.refl.ind  <- 3:1
  shape.refl.ind <- 8:1
  shape.xnotnull <- 2:8
  nose.xnotnull  <- 2:3

  face.list <- list()
  for(ind in 1:n){
    factors <- xy[ind,]
    face <- face.orig

    m <- mean(face$lipso[,2])
    face$lipso[,2] <- m+(face$lipso[,2]-m)*(1+0.7*factors[4])
    face$lipsi[,2] <- m+(face$lipsi[,2]-m)*(1+0.7*factors[4])
    face$lipso[,1] <- face$lipso[,1]*(1+0.7*factors[5])
    face$lipsi[,1] <- face$lipsi[,1]*(1+0.7*factors[5])
    face$lipso["lipsiend",2] <- face$lipso["lipsiend",2]+20*factors[6]

    m <- mean(face$eye[,2])
    face$eye[,2]  <- m+(face$eye[,2] -m)*(1+0.7*factors[7])
    face$iris[,2] <- m+(face$iris[,2]-m)*(1+0.7*factors[7])
    m <- mean(face$eye[,1])
    face$eye[,1]  <- m+(face$eye[,1] -m)*(1+0.7*factors[8])
    face$iris[,1] <- m+(face$iris[,1]-m)*(1+0.7*factors[8])


    m <- min(face$hair[,2])
    face$hair[,2] <- m+(face$hair[,2]-m)*(1+0.2*factors[9])
    m <- 0
    face$hair[,1] <- m+(face$hair[,1]-m)*(1+0.2*factors[10])
    m <- 0
    face$hair[c("hair1","hair2"),2] <- face$hair[c("hair1","hair2"),2]+50*factors[11]

    m <- mean(face$nose[,2])
    face$nose[,2] <- m+(face$nose[,2]-m)*(1+0.7*factors[12])
    face$nose[nose.xnotnull,1] <- face$nose[nose.xnotnull,1]*(1+factors[13])

    m <- mean(face$shape[c("earsta","earend"),1])
    face$ear[,1] <- m+(face$ear[,1]-m)* (1+0.7*factors[14])
    m <- min(face$ear[,2])
    face$ear[,2] <- m+(face$ear[,2]-m)* (1+0.7*factors[15])

    face <- lapply(face,function(x){ x[,2] <- x[,2]*(1+0.2*factors[1]);x})
    face <- lapply(face,function(x){ x[,1] <- x[,1]*(1+0.2*factors[2]);x})
    face <- lapply(face,function(x){ x[,1] <- ifelse(x[,1]>0,
                                                     ifelse(x[,2] > -30, x[,1],
                                                            pmax(0,x[,1]+(x[,2]+50)*0.2*sin(1.5*(-factors[3])))),0);x})


    invert <- function(x) cbind(-x[,1], x[,2])

    face.obj <- list(
      eyer  = face$eye,
      eyel  = invert(face$eye),
      irisr = face$iris,
      irisl = invert(face$iris),
      lipso = rbind(face$lipso,invert(face$lipso[lipso.refl.ind,])),
      lipsi = rbind(face$lipso["lipsiend",], face$lipsi,
                    invert(face$lipsi[lipsi.refl.ind,, drop=FALSE]),
                    invert(face$lipso["lipsiend",, drop=FALSE])),
      earr  = rbind(face$shape["earsta",], face$ear, face$shape["earend",]),
      earl  = invert(rbind(face$shape["earsta",], face$ear, face$shape["earend",])),
      nose  = rbind(face$nose,invert(face$nose[nose.refl.ind,])),
      hair  = rbind(face$shape["hairend",],face$hair,invert(face$hair[hair.refl.ind,]),
                    invert(face$shape["hairend",,drop=FALSE])),
      shape = rbind(face$shape,invert(face$shape[shape.refl.ind,]))
    )
    face.obj$lipsi <- rbind(face.obj$lipsi, Rev(face.obj$lipsi, margin = 1))

    face.list <- c(face.list, list(face.obj))

    plot(1, type="n", xlim=c(-105, 105) * 1.1, axes=FALSE,
         ylab="", ylim=c(-105, 105) * 1.3, xlab="")

    title(xnames[ind])

    f <- 1+(ncolors-1) * (factors+1)/2 # translate factors into color numbers

    xtrans <- function(x){x}
    ytrans <- function(y){y}

    for(obj.ind in seq(face.obj)[c(10:11, 1:9)]) {

      x  <- face.obj[[obj.ind]][, 1]
      y <- face.obj[[obj.ind]][, 2]
      xx <- spline(1:length(x), x, 40, FALSE)[, 2]
      yy <- spline(1:length(y), y, 40, FALSE)[, 2]

      lines(xx, yy)

      if(obj.ind == 10)
        polygon(xtrans(xx), ytrans(yy), col=col.hair[ind], xpd=NA) # hair
      if(obj.ind==11)
        polygon(xtrans(xx), ytrans(yy), col=col.face[ind], xpd=NA) # face

      xx <- xtrans(xx)
      yy <- ytrans(yy)

      if(obj.ind %in% 1:2) polygon(xx,yy,col="#eeeeee") # eyes without iris
      if(obj.ind %in% 3:4) polygon(xx,yy,col=col.eyes[ind], xpd=NA) # eyes:iris
      if(obj.ind %in% 9)   polygon(xx,yy,col=col.nose[ind], xpd=NA)# nose
      if(obj.ind %in% 5:6) polygon(xx,yy,col=col.lips[ind], xpd=NA)  # lips
      if(obj.ind %in% 7:8) polygon(xx,yy,col=col.ears[ind], xpd=NA)# ears

    }

  }

  if(!missing(main)){
    par(opar)
    par(mfrow=c(1,1))
    mtext(main, 3, 3, TRUE, 0.5)
    title(main)
  }

  info <- c(
    "height of face",
    "width of face",
    "structure of face",
    "height of mouth",
    "width of mouth",
    "smiling",
    "height of eyes",
    "width of eyes",
    "height of hair",
    "width of hair",
    "style of hair",
    "height of nose",
    "width of nose",
    "width of ear",
    "height of ear")
  var.names <- dimnames(xy)[[2]]
  if(0==length(var.names))
    var.names <- paste("Var",rows.orig,sep="")
  info <- data.frame("modified item"=info, "variable"=var.names[1:length(info)])

  names(face.list) <- xnames
  out <- list(faces=face.list, info=info,xy=t(xy))
  class(out) <- "faces"
  invisible(out)

}



## plots: PlotBag ====


####################################"
# the source code for the function
# from Hans Peter Wolf
#
# http://www.wiwi.uni-bielefeld.de/~wolf/software/R-wtools/bagplot/bagplot.R
#
#
##start:##


PlotBagPairs <- function(dm, trim = 0.0, main, numeric.only = TRUE,
                         factor = 3, approx.limit = 300, pch = 16,
                         cex = 0.8, precision = 1, col.loophull = "#aaccff",
                         col.looppoints = "#3355ff", col.baghull = "#7799ff",
                         col.bagpoints = "#000088", ...){
  if(missing(main)) main <- paste(deparse(substitute(dm)),"/ trim =",round(trim,3))
  if(length(trim) == 1) trim <- rep(trim, ncol(dm))
  if(numeric.only){
    dm <- dm[, idx <- sapply(1:ncol(dm), function(x) is.numeric(dm[,x]))]
    trim <- trim[idx]
  }
  for(j in 1:ncol(dm)){
    x <- dm[,j]
    if(!is.numeric(x)) x <- as.numeric(x)
    if( trim[j] > 0) {
      na.idx <- is.na(x)
      xlim <- quantile(x[!na.idx], c(trim[j] , 1-trim[j]))
      x[ na.idx |  x < xlim[1] | xlim[2] < x ] <- NA
    }
    dm[,j] <- x
  }
  # DM0 <<- dm
  h.fn <- function(x,y){
    idx <- !is.na(x) & !is.na(y)
    x <- x[ idx ]; y <- y[ idx ]
    BP <- PlotBag(x,y,add=TRUE,factor = factor, approx.limit = approx.limit, pch = pch,
                  cex = cex, precision = precision, col.loophull = col.loophull,
                  col.looppoints = col.looppoints, col.baghull = col.baghull,
                  col.bagpoints = col.bagpoints, verbose=FALSE)
    # BP <<- BP ### for debugging
  }
  par(mfrow=c(1,1))
  pairs(dm, panel = h.fn, ...)
  mtext(main, line=2.5)
  dm
}

#0:
##start:##
compute.bagplot <- function(x,y,
                            factor=3, # expanding factor for bag to get the loop
                            na.rm=FALSE, # should NAs removed or exchanged
                            approx.limit=300, # limit
                            dkmethod=2, # in 1:2; method 2 is recommended
                            precision=1, # controls precision of computation
                            verbose=FALSE,debug.plots="no" # tools for debugging
){

  "bagplot, version 2012/12/05, peter wolf"


  # define some functions
  win<-function(dx,dy){  atan2(y=dy,x=dx) }
  out.of.polygon<-function(xy,pg){  # 121026
    xy<-matrix(xy,ncol=2)
    # check trivial case
    if(nrow(pg)==1)  return(xy[,1]==pg[1] & xy[,2]==pg[2])
    # store number of points of xy and polygon
    m<-nrow(xy); n<-nrow(pg)
    # find small value relative to polygon
    limit <- -abs(1E-10*diff(range(pg)))
    # find vectors that are orthogonal to segments of polygon
    pgn<-cbind(diff(c(pg[,2],pg[1,2])),-diff(c(pg[,1],pg[1,1])))
    # find center of gravity of xy
    S<-colMeans(xy)
    # compute negative distances of polygon to center of gravity of xy
    dxy<-cbind(S[1]-pg[,1],S[2]-pg[,2])
    # unused: S.in.pg<-all(limit<apply(dxy*pgn,1,sum))
    if( !all( limit < apply(dxy*pgn,1,sum) ) ){
      pg<-pg[n:1,]; pgn<--pgn[n:1,]
    }
    # initialize result
    in.pg<-rep(TRUE,m)
    for(j in 1:n){
      dxy<-xy-matrix(pg[j,],m,2,byrow=TRUE)
      in.pg<-in.pg & limit<(dxy%*%pgn[j,])
    }
    return(!in.pg)
  }
  cut.z.pg<-function(zx,zy,p1x,p1y,p2x,p2y){
    a2<-(p2y-p1y)/(p2x-p1x); a1<-zy/zx
    sx<-(p1y-a2*p1x)/(a1-a2); sy<-a1*sx
    sxy<-cbind(sx,sy)
    h<-any(is.nan(sxy))||any(is.na(sxy))||any(Inf==abs(sxy))
    if(h){ # print("NAN found"); print(cbind(a1,a2,zx,zy,sxy,p2x-p1x))
      if(!exists("verbose")) verbose<-FALSE
      if(verbose) cat("special")
      # zx is zero ### 121030
      h<-0==zx
      sx<-ifelse(h,zx,sx); sy<-ifelse(h,p1y-a2*p1x,sy)
      # points on line defined by line segment
      a1 <- ifelse( abs(a1) == Inf, sign(a1)*123456789*1E10, a1) # 121030
      a2 <- ifelse( abs(a2) == Inf, sign(a2)*123456789*1E10, a2)
      # points on line defined by line segment
      h<-0==(a1-a2) & sign(zx)==sign(p1x)
      sx<-ifelse(h,p1x,sx); sy<-ifelse(h,p1y,sy)
      h<-0==(a1-a2) & sign(zx)!=sign(p1x)
      sx<-ifelse(h,p2x,sx); sy<-ifelse(h,p2y,sy)
      # line segment vertical
      #   & center NOT ON line segment
      h<-p1x==p2x & zx!=p1x & p1x!=0
      sx<-ifelse(h,p1x,sx); sy<-ifelse(h,zy*p1x/zx,sy)
      #   & center ON line segment
      h<-p1x==p2x & zx!=p1x & p1x==0
      sx<-ifelse(h,p1x,sx); sy<-ifelse(h,0,sy)
      #   & center NOT ON line segment & point on line     ### 121126
      h<-p1x==p2x & zx==p1x & p1x!=0 # & sign(zy)==sign(p1y)
      sx<-ifelse(h,zx,sx); sy<-ifelse(h,zy,sy)
      #   & center ON line segment & point on line
      h<-p1x==p2x & zx==p1x & p1x==0 & sign(zy)==sign(p1y)
      sx<-ifelse(h,p1x,sx); sy<-ifelse(h,p1y,sy)
      h<-p1x==p2x & zx==p1x & p1x==0 & sign(zy)!=sign(p1y)
      sx<-ifelse(h,p1x,sx); sy<-ifelse(h,p2y,sy)
      #  points identical to end points of line segment
      h<-zx==p1x & zy==p1y; sx<-ifelse(h,p1x,sx); sy<-ifelse(h,p1y,sy)
      h<-zx==p2x & zy==p2y; sx<-ifelse(h,p2x,sx); sy<-ifelse(h,p2y,sy)
      # point of z is center
      h<-zx==0 & zy==0; sx<-ifelse(h,0,sx); sy<-ifelse(h,0,sy)
      sxy<-cbind(sx,sy)
    } # end of special cases
    #if(verbose){ print(rbind(a1,a2));print(cbind(zx,zy,p1x,p1y,p2x,p2y,sxy))}
    if(!exists("debug.plots")) debug.plots<-"no"
    if(debug.plots=="all"){
      segments(sxy[,1],sxy[,2],zx,zy,col="red")
      segments(0,0,sxy[,1],sxy[,2],col="green",lty=2) ##!!
      points(sxy,col="red")
    }
    return(sxy)
  }
  find.cut.z.pg<-function(z,pg,center=c(0,0),debug.plots="no"){
    if(!is.matrix(z)) z<-rbind(z)
    if(1==nrow(pg)) return(matrix(center,nrow(z),2,TRUE))
    n.pg<-nrow(pg); n.z<-nrow(z)
    z<-cbind(z[,1]-center[1],z[,2]-center[2])
    pgo<-pg; pg<-cbind(pg[,1]-center[1],pg[,2]-center[2])
    if(!exists("debug.plots")) debug.plots<-"no"
    if(debug.plots=="all"){
      plot(rbind(z,pg,0),bty="n"); points(z,pch="p")
      lines(c(pg[,1],pg[1,1]),c(pg[,2],pg[1,2]))}
    # find angles of pg und z
    apg<-win(pg[,1],pg[,2])
    apg[is.nan(apg)]<-0; a<-order(apg); apg<-apg[a]; pg<-pg[a,]
    az<-win(z[,1],z[,2])
    # find line segments
    segm.no<-apply((outer(apg,az,"<")),2,sum)
    segm.no<-ifelse(segm.no==0,n.pg,segm.no)
    next.no<-1+(segm.no %% length(apg))
    # compute cut points
    cuts<-cut.z.pg(z[,1],z[,2],pg[segm.no,1],pg[segm.no,2],
                   pg[next.no,1],pg[next.no,2])
    # rescale
    cuts<-cbind(cuts[,1]+center[1],cuts[,2]+center[2])
    return(cuts)
  }
  # find.cut.z.pg(EX,  EX1,center=CE)
  hdepth.of.points<-function(tp){
    # 121030 second parameter n has been removed
    # if(!exists("precision")) precision <- 1 ### 121203
    # return(find.hdepths.tp(tp, xy, 181*precision)) ### 121202
    n.tp<-nrow(tp)
    tphdepth<-rep(0,n.tp); dpi<-2*pi-0.000001
    for(j in 1:n.tp) {
      dx<-tp[j,1]-xy[,1]; dy<-tp[j,2]-xy[,2]
      a<-win(dx,dy)+pi; h<-a<10; a<-a[h]; ident<-sum(!h)
      init<-sum(a < pi); a.shift<-(a+pi) %% dpi
      minusplus<-c(rep(-1,length(a)),rep(1,length(a))) #### 070824
      h<-cumsum(minusplus[order(c(a,a.shift))])
      tphdepth[j]<-init+min(h)+1 # +1 because of the point itself!!
      # tphdepth[j]<-init+min(h)+ident; cat("SUMME",ident)
    }
    tphdepth
  }

  find.hdepths.tp <- function(tp, data, number.of.directions=181){ # 121130
    # standardize dimensions
    xy <- as.matrix(data); tp <- as.matrix(rbind(tp)); n.tp <- dim(tp)[1]
    for( j in 1:2) {
      xy[,j] <- xy[,j] - (h <- min(xy[,j], na.rm=TRUE))
      tp[,j] <- tp[,j] -  h
      if( 0 < (h <- max(xy[,j], na.rm=TRUE))){
        xy[,j] <- xy[,j]/h; tp[,j] <- tp[,j]/h
      }
    }
    ##loop over directions##
    phi    <- c(seq(0,180,length=number.of.directions)[-1]*(2*pi/360))
    sinphi <- c(sin(phi),1); cosphi <- c(cos(phi),0)
    RM1 <- round(digits=6,rbind(cosphi,sinphi))
    hdtp <- rep(length(xy[,1]),length(tp[,1]))
    for( j in seq(along=sinphi)){ #print(j)
      xyt <- xy %*% RM1[,j]; tpt <- (tp %*% RM1[,j])[]
      xyt <- xyt[!is.na(xyt)] #; tpt <- sort(tpt)
      hdtp <- pmin(hdtp,(rank( c(tpt,xyt), ties.method="min"))[1:n.tp]
                   -rank( tpt,ties.method="min")
                   ,rank(-c(tpt,xyt), ties.method="min")[1:n.tp]
                   -rank(-tpt,ties.method="min")
      )
    }
    hdtp
  }


  expand.hull<-function(pg,k){
    if( 1 >= nrow(pg) ) return(pg) ## 121026 ## 121123 <= statt ==

    resolution<-floor(20*precision)
    pg0<-xy[hdepth==1,]
    pg0<-pg0[chull(pg0[,1],pg0[,2]),]
    end.points<-find.cut.z.pg(pg,pg0,center=center,debug.plots=debug.plots)
    lam<-((0:resolution)^1)/resolution^1

    pg.new<-pg
    for(i in 1L:nrow(pg)){
      tp<-cbind(pg[i,1]+lam*(end.points[i,1]-pg[i,1]),
                pg[i,2]+lam*(end.points[i,2]-pg[i,2]))
      # hd.tp<-hdepth.of.points(tp)
      hd.tp<-find.hdepths.tp(tp,xy)
      ind<-max(sum(hd.tp>=k),1)
      if(ind<length(hd.tp)){  # hd.tp[ind]>k &&
        tp<-cbind(tp[ind,1]+lam*(tp[ind+1,1]-tp[ind,1]),
                  tp[ind,2]+lam*(tp[ind+1,2]-tp[ind,2]))
        # hd.tp<-hdepth.of.points(tp)
        hp.tp<-find.hdepths.tp(tp,xy)
        ind<-max(sum(hd.tp>=k),1)
      }
      pg.new[i,]<-tp[ind,]
    }
    pg.new<-pg.new[chull(pg.new[,1],pg.new[,2]),]
    # cat("depth pg.new", hdepth.of.points(pg.new))
    # cat("depth pg.new", find.hdepths.tp(pg.new,xy))

    pg.add<-0.5*(pg.new+rbind(pg.new[-1,],pg.new[1,]))
    # end.points<-find.cut.z.pg(pg,pg0,center=center)
    end.points<-find.cut.z.pg(pg.add,pg0,center=center) #### 070824
    for(i in 1L:nrow(pg.add)){
      tp<-cbind(pg.add[i,1]+lam*(end.points[i,1]-pg.add[i,1]),
                pg.add[i,2]+lam*(end.points[i,2]-pg.add[i,2]))
      # hd.tp<-hdepth.of.points(tp)
      hd.tp<-find.hdepths.tp(tp,xy)
      ind<-max(sum(hd.tp>=k),1)
      if(ind<length(hd.tp)){ # hd.tp[ind]>k &&
        tp<-cbind(tp[ind,1]+lam*(tp[ind+1,1]-tp[ind,1]),
                  tp[ind,2]+lam*(tp[ind+1,2]-tp[ind,2]))
        # hd.tp<-hdepth.of.points(tp)
        hd.tp<-find.hdepths.tp(tp,xy)
        ind<-max(sum(hd.tp>=k),1)
      }
      pg.add[i,]<-tp[ind,]
    }
    # cat("depth pg.add", hdepth.of.points(pg.add))

    pg.new<-rbind(pg.new,pg.add)
    pg.new<-pg.new[chull(pg.new[,1],pg.new[,2]),]
  }
  cut.p.sl.p.sl<-function(xy1,m1,xy2,m2){
    sx<-(xy2[2]-m2*xy2[1]-xy1[2]+m1*xy1[1])/(m1-m2)
    sy<-xy1[2]-m1*xy1[1]+m1*sx
    if(!is.nan(sy)) return( c(sx,sy) )
    if(abs(m1)==Inf) return( c(xy1[1],xy2[2]+m2*(xy1[1]-xy2[1])) )
    if(abs(m2)==Inf) return( c(xy2[1],xy1[2]+m1*(xy2[1]-xy1[1])) )
  }
  pos.to.pg<-function(z,pg,reverse=FALSE){
    if(reverse){
      int.no<-apply(outer(pg[,1],z[,1],">="),2,sum)
      zy.on.pg<-pg[int.no,2]+pg[int.no,3]*(z[,1]-pg[int.no,1])
    }else{
      int.no<-apply(outer(pg[,1],z[,1],"<="),2,sum)
      zy.on.pg<-pg[int.no,2]+pg[int.no,3]*(z[,1]-pg[int.no,1])
    }
    #### ifelse(z[,2]<zy.on.pg, "lower","higher") ##### 121004
    result <- ifelse(z[,2]<zy.on.pg, "lower","higher") ####
    return(result)
    if( all(result=="lower") ){
      result <- ifelse(((z[,2] - zy.on.pg)/max(z[,2] - zy.on.pg)+1e-10) < 0,
                       "lower","higher")
    }
    if( all(result=="higher") ){
      result <- ifelse(((z[,2] - zy.on.pg)/max(z[,2] - zy.on.pg)-1e-10) < 0,
                       "lower","higher")
    }
    print(result)
    return(result)
  }
  find.polygon.center<-function(xy){
    #### if(missing(xy)){n<-50;x<-rnorm(n);y<-rnorm(n); xy<-cbind(x,y)}
    #### xy<-xy[chull(xy),]
    if(length(xy)==2) return(xy[1:2])
    if(nrow(xy)==2) return(colMeans(xy)) #### 121009
    #### partition polygon into triangles
    n<-length(xy[,1]); mxy<-colMeans(xy)
    xy2<-rbind(xy[-1,],xy[1,]); xy3<-cbind(rep(mxy[1],n),mxy[2])
    #### determine areas and centers of gravity of triangles
    S<-(xy+xy2+xy3)/3
    F2<-abs((xy[,1]-xy3[,1])*(xy2[,2]-xy3[,2])-
              (xy[,2]-xy3[,2])*(xy2[,1]-xy3[,1]))
    #### compute center of gravity of polygon
    lambda<-F2/sum(F2)
    SP<-colSums(cbind(S[,1]*lambda,S[,2]*lambda))
    return(SP)
  }
  # check input
  xydata<-if(missing(y)) x else cbind(x,y)
  if(is.data.frame(xydata)) xydata<-as.matrix(xydata)
  if(any(is.na(xydata))){
    if(na.rm){ xydata<-xydata[!apply(is.na(xydata),1,any),,drop=FALSE]
    print("Warning: NA elements have been removed!!")
    }else{ #121129
      xy.medians<-apply(xydata,2,function(x) median(x, na.rm=TRUE))
      # colMeans(xydata,na.rm=TRUE)
      for(j in 1:ncol(xydata)) xydata[is.na(xydata[,j]),j]<-xy.medians[j]
      print("Warning: NA elements have been exchanged by median values!!")
    }
  }
  # if(nrow(xydata)<3) {print("not enough data points"); return()} ### 121008
  if(length(xydata)<4) {print("not enough data points"); return()}
  if((length(xydata)%%2)==1) {print("number of values isn't even"); return()}
  if(!is.matrix(xydata)) xydata<-matrix(xydata,ncol=2,byrow=TRUE)
  # select sample in case of a very large data set
  very.large.data.set<-nrow(xydata) > approx.limit
  # use of random number generator may disturb simulation
  # therefore we now use a systematical part of the data 20120930
  #### OLD: set.seed(random.seed<-13)  #### SEED
  if(very.large.data.set){
    #### OLD: ind<-sample(seq(nrow(xydata)),size=approx.limit)
    step<-(n<-nrow(xydata))/approx.limit; ind <- round(seq(1,n,by=step))
    xy<-xydata[ind,]
  } else xy<-xydata
  n<-nrow(xy)
  points.in.bag<-floor(n/2)
  # if jittering is needed
  # the following two lines can be activated
  #xy<-xy+cbind(rnorm(n,0,.0001*sd(xy[,1])),
  #             rnorm(n,0,.0001*sd(xy[,2])))
  if(verbose) cat("end of initialization")

  prdata<-prcomp(xydata)
  is.one.dim<-(0 == max(prdata[[1]])) || (min(prdata[[1]])/max(prdata[[1]]))<0.00001 # 121129
  if(is.one.dim){
    if(verbose) cat("data set one dimensional")
    center<-colMeans(xydata)
    res<-list(xy=xy,xydata=xydata,prdata=prdata,
              is.one.dim=is.one.dim,center=center)
    class(res)<-"bagplot"
    return(res)
  }
  if(verbose) cat("data not linear")

  if(nrow(xydata)<=4) {
    if(verbose) cat("only three or four data points")
    center<-colMeans(xydata)
    res<-list(xy=xy,xydata=xydata,prdata=prdata,hdepths=rep(1,n),hdepth=rep(1,n),
              is.one.dim=is.one.dim,center=center,hull.center=NULL,
              hull.bag=NULL,hull.loop=NULL,pxy.bag=NULL,pxy.outer=xydata,
              pxy.outlier=NULL,exp.dk=xydata)
    class(res)<-"bagplot"
    return(res)
  }

  xym<-apply(xy,2,mean); xysd<-apply(xy,2,sd)
  xyxy<-cbind((xy[,1]-xym[1])/xysd[1],(xy[,2]-xym[2])/xysd[2])

  dx<-(outer(xy[,1],xy[,1],"-"))
  dy<-(outer(xy[,2],xy[,2],"-"))
  alpha<-atan2(y=dy,x=dx); diag(alpha)<-1000
  for(j in 1:n) alpha[,j]<-sort(alpha[,j])
  alpha<-alpha[-n,] ; m<-n-1
  #### quick look inside, just for check
  if(debug.plots=="all"){
    plot(xy,bty="n"); xdelta<-abs(diff(range(xy[,1]))); dx<-xdelta*.3
    for(j in 1:n) {
      p<-xy[j,]; dy<-dx*tan(alpha[,j])
      segments(p[1]-dx,p[2]-dy,p[1]+dx,p[2]+dy,col=j)
      text(p[1]-xdelta*.02,p[2],j,col=j)
    }
  }
  if(verbose) print("end of computation of angles")

  hdepth<-rep(0,n); dpi<-2*pi-0.000001; mypi<-pi-0.000001
  minusplus<-c(rep(-1,m),rep(1,m))
  if(FALSE){
    for(j in 1:n) {
      a<-alpha[,j]+pi; h<-a<10; a<-a[h]; init<-sum(a < mypi) # hallo
      a.shift<-(a+pi) %% dpi
      minusplus<-c(rep(-1,length(a)),rep(1,length(a))) #### 070824
      h<-cumsum(minusplus[order(c(a,a.shift))])
      hdepth[j]<-init+min(h)+1 # or do we have to count identical points?
      # hdepth[j]<-init+min(h)+sum(xy[j,1]==xy[,1] & xy[j,2]==xy[,2])
    }
  }
  find.hdepths <- function(xy, number.of.directions=181){ # 121126

    xy <- as.matrix(xy)
    for( j in 1:2) {
      xy[,j] <- xy[,j] - min(xy[,j])
      if( 0 < (h <- max(xy[,j]))) xy[,j] <- xy[,j] / max(xy[,j])
    }

    phi    <- c(seq(0,180,length=number.of.directions)[-1]*(2*pi/360))
    sinphi <- c(sin(phi),1); cosphi <- c(cos(phi),0)
    RM1 <- round(digits=6,rbind(cosphi,sinphi))
    hd <- rep(h<-length(xy[,1]),h)
    for( j in seq(along=sinphi)){
      xyt <- xy %*% RM1[,j]
      hd <- pmin(hd,rank(xyt,ties.method="min"), rank(-xyt,ties.method="min"))
    }
    #  xyt <- xy %*% RM1
    #  hd2 <- cbind(apply(xyt, 2, rank, ties.method="min"),
    #               apply(-xyt,2, rank, ties.method="min"))
    #  hd2 <- apply(hd2, 1, min)
    hd
  }
  hdepth <- find.hdepths(xy,181*precision)
  if(verbose){print("end of computation of hdepth:"); print(hdepth)}
  #### quick look inside, just for a check
  if(debug.plots=="all"){
    plot(xy,bty="n")
    xdelta<-abs(diff(range(xy[,1]))); dx<-xdelta*.1
    for(j in 1:n) {
      a<-alpha[,j]+pi; a<-a[a<10]; init<-sum(a < pi)
      a.shift<-(a+pi) %% dpi
      minusplus<-c(rep(-1,length(a)),rep(1,length(a))) #### 070824
      h<-cumsum(minusplus[ao<-(order(c(a,a.shift)))])
      no<-which((init+min(h)) == (init+h))[1]
      p<-xy[j,]; dy<-dx*tan(alpha[,j])
      segments(p[1]-dx,p[2]-dy,p[1]+dx,p[2]+dy,col=j,lty=3)
      dy<-dx*tan(c(sort(a),sort(a))[no])
      segments(p[1]-5*dx,p[2]-5*dy,p[1]+5*dx,p[2]+5*dy,col="black")
      text(p[1]-xdelta*.02,p[2],hdepth[j],col=1) # cex=2.5 assumes suitable fonts
    }
  }

  hd.table<-table(sort(hdepth))
  d.k<-cbind(dk=rev(cumsum(rev(hd.table))),
             k =as.numeric(names(hd.table)))
  k.1<-sum( points.in.bag < d.k[,1] )
  # if(nrow(d.k)>1){ ### version 09/2005, error in data set 1 of Meuleman
  # instead of >2 now >k.1 ### 070827
  # if(nrow(d.k)>k.1){ k<-d.k[k.1+1,2] } else { k<-d.k[k.1,2] }
  # this statement will not have an effect because of the next one:
  k<-d.k[k.1,2]+1 # 121004 increment depth by one not by looking for next depth
  if(verbose){cat("numbers of members of dk:"); print(hd.table); print(d.k)}
  if(verbose){cat("end of computation of k, k=",k,"k.1:",k.1)}
  # D.K<<-d.k; K.1<<-k.1; EX<<-exp.dk; EX.1<<-exp.dk.1; PDK<<-pdk; HDEPTH<<-hdepth

  center<-apply(xy[which(hdepth==max(hdepth)),,drop=FALSE],2,mean)
  hull.center<-NULL
  if(3<nrow(xy)&&length(hd.table)>0){
    n.p<-floor(1.5*c(32,16,8)[1+(n>50)+(n>200)]*precision)
    # limit.hdepth.to.check <- sort(hdepth, decreasing = TRUE)[min(nrow(xy),6)]
    # 121126
    h <- unique(sort(hdepth, decreasing = TRUE))
    limit.hdepth.to.check <- sort(h)[min(length(h),3)]
    h<-cands<-xy[limit.hdepth.to.check <= hdepth,,drop=FALSE]
    # h<-cands<-xy[rev(order(hdepth))[1:(min(nrow(xy),6))],]
    cands<-cands[chull(cands[,1],cands[,2]),]; n.c<-nrow(cands)
    if(is.null(n.c))cands<-h

    xyextr<-rbind(apply(cands,2,min),apply(cands,2,max))
    ## xydel<-2*(xyextr[2,]-xyextr[1,])/n.p ## unused
    if( (xyextr[2,1]-xyextr[1,1]) < 0.2*(h <- diff(range(xy[,1])))){
      xyextr[1:2,1] <- mean(xyextr[,1]) + c(-.1,.1) * h }            #### 121203
    if( (xyextr[2,2]-xyextr[1,2]) < 0.2*(h <- diff(range(xy[,2])))){
      xyextr[1:2,2] <- mean(xyextr[,2]) + c(-.1,.1) * h }            #### 121203
    if(verbose){cat("xyextr: looking for maximal depth"); print(xyextr) }
    h1<-seq(xyextr[1,1],xyextr[2,1],length=n.p)
    h2<-seq(xyextr[1,2],xyextr[2,2],length=n.p)
    tp<-cbind(as.vector(matrix(h1,n.p,n.p)), #      [1:n.p^2],
              as.vector(matrix(h2,n.p,n.p,TRUE))) # [1:n.p^2])
    # tphdepth<-max(hdepth.of.points(tp))-1
    tphdepth<-max(find.hdepths.tp(tp,xy))
    # if(verbose) { TP<<-tp; TPD<<-find.hdepths.tp(tp,xy) }
    if(verbose) cat("points(TP,pch=c(letters,LETTERS)[TPD+1])")
    # if max of testpoint is smaller than max depth of points take that max!
    if(verbose){ cat("depth of testpoints"); print(summary(tphdepth)) } # 121126
    tphdepth<-max(tphdepth,d.k[,2]) # 121004

    # define direction for hdepth search
    num<-floor(2*c(417,351,171,85,67,43)[sum(n>c(1,50,100,150,200,250))]*precision)
    num.h<-floor(num/2); angles<-seq(0,pi,length=num.h)
    ang<-tan(pi/2-angles)
    kkk<-tphdepth
    if(verbose){cat("max-hdepth found:"); print(kkk)}
    if(verbose) cat("find polygon with max depth")
    ia<-1; a<-angles[ia]; xyt<-xyxy%*%c(cos(a),-sin(a)); xyto<-order(xyt)
    # initial for upper part
    ind.k<-xyto[kkk]; cutp<-c(xyxy[ind.k,1],-10)
    dxy<-diff(range(xyxy))
    pg<-rbind(c(cutp[1],-dxy,Inf),c(cutp[1],dxy,NA))
    # initial for lower part
    ind.kk<-xyto[n+1-kkk]; cutpl<-c(xyxy[ind.kk,1],10)
    # pgl<-rbind(c(cutpl[1],dxy,Inf),c(cutpl[1],-dxy,NA))
    pgl<-rbind(c(cutpl[1],dxy,-Inf),c(cutpl[1],-dxy,NA))
    # the sign of inf doesn't matter
    if(debug.plots=="all"){ plot(xyxy,type="p",bty="n")
      text(xy,,1:n,col="blue")
      hx<-xy[ind.k,c(1,1)]; hy<-xy[ind.k,c(2,2)]
      segments(hx,hy,c(10,-10),hy+ang[ia]*(c(10,-10)-hx),lty=2)
      text(hx+rnorm(1,,.1),hy+rnorm(1,,.1),ia)
    }
    if(verbose) cat("start of computation of the directions: ","kkk=",kkk) # 121030
    for(ia in seq(angles)[-1]){

      # determine critical points pnew and pnewl of direction a
      # if(verbose) cat("ia",ia,angles[ia])
      # 121030
      a<-angles[ia]; angtan<-ang[ia]; xyt<-xyxy%*%c(cos(a),-sin(a)); xyto<-order(xyt)
      ind.k <-xyto[kkk]; ind.kk<-xyto[n+1-kkk]; pnew<-xyxy[ind.k,]; pnewl<-xyxy[ind.kk,]
      # if(verbose) if( 1 < sum(xyt == xyt[ind.k]) )print("WARNING: some points identical")
      if(debug.plots=="all") points(pnew[1],pnew[2],col="red")
      # new limiting lines are defined by pnew / pnewl and slope a
      # find segment of polygon that is cut by new limiting line and cut
      # if(ia>200) { #<show pg pgl>#; points(pnew[1],pnew[2],col="magenta",cex=6) }
      if( abs(angtan)>1e10){ if(verbose) cat("kkk",kkk,"x=c case")
        # case of vertical slope #print(pg);print(pnew);print(xyt);lines(pg,col="red",lwd=3)
        # number of points left of point pnew that limit the polygon
        pg.no<-sum(pg[,1]<pnew[1])
        if( 0 < pg.no ){
          # the polygon (segment pg.no) has to be cut at x==pnew[1]
          cutp <- c(pnew[1], pg [pg.no, 2]+pg [pg.no, 3]*(pnew [1]-pg [pg.no ,1]))
          pg<- rbind(pg[1:pg.no,],  c(cutp,angtan), c(cutp[1]+dxy,  cutp[2] +angtan*dxy,NA))
        } else {
          if(verbose) cat("!!! case degenerated UPPER polygon: pg.no==0")
          # the limiting point pnew is above the beginning of the polygon
          # therefore, the polygon reduces to line
          pg <- rbind(pg[1,], c(pg[2,1:2],NA))
        }
        pg.nol<-sum(pgl[,1]>=pnewl[1])
        if( 0 < pg.nol ){ #??2 ### 121204
          cutpl<-c(pnewl[1],pgl[pg.nol,2]+pgl[pg.nol,3]*(pnewl[1]-pgl[pg.nol,1]))
          pgl<-rbind(pgl[1:pg.nol,],c(cutpl,angtan),c(cutpl[1]-dxy, cutpl[2]-angtan*dxy,NA))
        } else {
          if(verbose) cat("!!! case degenerated LOWER polygon: pgl.no==0")
          pgl <- rbind(pgl[1,], c(pgl[2,1:2],NA))
        }
      }else{ # if(verbose) cat("kkk",kkk,"normal case")
        # normal case upper polygon
        pg.inter<-pg[,2]-angtan*pg[,1]; pnew.inter<-pnew[2]-angtan*pnew[1]
        pg.no<-sum(pg.inter<pnew.inter)
        if(is.na(pg[pg.no,3])) pg[pg.no,3] <- -Inf # 121129 NaN/Na error
        cutp<-cut.p.sl.p.sl(pnew,ang[ia],pg[pg.no,1:2],pg[pg.no,3])
        pg<- rbind(pg[1:pg.no,],  c(cutp,angtan), c(cutp[1]+dxy,  cutp[2] +angtan*dxy,NA))
        # normal case lower polygon
        pg.interl<-pgl[,2]-angtan*pgl[,1]; pnew.interl<-pnewl[2]-angtan*pnewl[1]
        pg.nol<-sum(pg.interl>pnew.interl)
        if(is.na(pgl[pg.nol,3])) pgl[pg.nol,3] <- Inf # 121129 NaN/Na error
        cutpl<-cut.p.sl.p.sl(pnewl,angtan,pgl[pg.nol,1:2],pgl[pg.nol,3])
        pgl<-rbind(pgl[1:pg.nol,],c(cutpl,angtan),c(cutpl[1]-dxy, cutpl[2]-angtan*dxy,NA))
      }
      # if(kkk==KKK && ia == 51) { cat("ENDE: pgl"); print(pgl) }
      # update pg, pgl completed
      # PG<<-pg;PG.NO<<-pg.no;CUTP<<-cutp;DXY<<-dxy;PNEW<<-pnew;PGL<<-pgl;PG.NOL<<-pg.nol
      #
      # cat("angtan",angtan,"pg.no",pg.no,"pkt:",pnew)
      # if(ia==stopp) lines(pg,type="b",col="green")
      if(debug.plots=="all"){
        points(pnew[1],pnew[2],col="red")
        hx<-xyxy[ind.k,c(1,1)]; hy<-xyxy[ind.k,c(2,2)]
        segments(hx,hy,c(10,-10),hy+ang[ia]*(c(10,-10)-hx),lty=2)
        # text(hx+rnorm(1,,.1),hy+rnorm(1,,.1),ia)
        # print(pg)
        # if(ia==stopp) lines(pgl,type="b",col="green")
        points(cutpl[1],cutpl[2],col="red")
        hx<-xyxy[ind.kk,c(1,1)]; hy<-xyxy[ind.kk,c(2,2)]
        segments(hx,hy,c(10,-10),hy+ang[ia]*(c(10,-10)-hx),lty=2)
        #  text(hx+rnorm(1,,.1),hy+rnorm(1,,.1),ia)
        # print(pgl)
      }
      ##show pg pgl##
    }
    # if(verbose) PG <<- pg; PGL <<- pgl
    if(2<nrow(pg) && 2<nrow(pgl)){

      # plot(xyxy[,1:2],xlim=c(-.5,+.5),ylim=c(-.5,.50))
      # lines(pg,type="b",col="red"); lines(pgl,type="b",col="blue")
      # remove first and last points and multiple points #<show pg pgl>#
      limit<-1e-10
      # pg <-pg [c(TRUE,(abs(diff(pg [,1]))>limit)|(abs(diff(pg [,2]))>limit)),] old#
      idx <- c(TRUE,(abs(diff(pg [,1]))>limit)|(abs(diff(pg [,2]))>limit)) # 121008
      if(any(idx==FALSE)){
        pg <-pg[idx,]; pg[,3] <- c(diff(pg[,2])/diff(pg[,1]), NA)
      }
      # old reduction which caused some errors:
      # pgl<-pgl[c(TRUE,(abs(diff(pgl[,1]))>limit)|(abs(diff(pgl[,2]))>limit)),] error##
      # pgl<-pgl[c(     (abs(diff(pgl[,1]))>limit)|(abs(diff(pgl[,2]))>limit),TRUE),] old#
      idx <-      c(     (abs(diff(pgl[,1]))>limit)|(abs(diff(pgl[,2]))>limit),TRUE)#121008
      if(any(idx==FALSE)){
        pgl<-pgl[idx,]; pgl[,3] <- c(diff(pgl[,2])/diff(pgl[,1]), NA)
      }
      # add some tolerance in course of numerical problems
      pgl[,2]<-pgl[,2] - .00001  # 121004
      # show pg pgl>>
      pg<- pg [-nrow(pg ),][-1,,drop=FALSE]
      pgl<-pgl[-nrow(pgl),][-1,,drop=FALSE]
      # determine position according to the other polygon
      #   cat("relative position: lower polygon")
      indl<-pos.to.pg(round(pgl,digits=10),round(pg,digits=10))  # 121126
      #   cat("relative position: upper polygon")
      indu<-pos.to.pg(round(pg,digits=10),round(pgl,digits=10),TRUE)
      sr<-sl<-NULL # ; ##show pg pgl>>
      # right region
      if(indu[(npg<-nrow(pg))]=="lower" & indl[1]=="higher"){
        # cat("in if of right region: the upper polynom is somewhere lower")
        #  checking from the right: last point of lower polygon that is NOT ok
        rnuml<-which(indl=="lower")[1]-1
        #  checking from the left: last point of upper polygon that is ok
        rnumu<-npg+1-which(rev(indu=="higher"))[1]
        #  special case all points of lower polygon are upper
        if(is.na(rnuml)) rnuml<-sum(pg[rnumu,1]<pgl[,1])
        #  special case all points of upper polygon are lower
        if(is.na(rnumu)) rnumu<-sum(pg[,1]<pgl[rnuml,1])
        xyl<-pgl[rnuml,]; xyu<-pg[rnumu,]
        # cat("right"); print(rnuml); print(xyl)
        # cat("right"); print(rnumu); print(xyu)
        sr<-cut.p.sl.p.sl(xyl[1:2],xyl[3],xyu[1:2],xyu[3])
      }
      # left region
      if(indl[(npgl<-nrow(pgl))]=="higher"&indu[1]=="lower"){
        # cat("in if of left region: the upper polynom is somewhere lower")
        #  checking from the right: last point of lower polygon that is ok
        lnuml<-npgl+1-which(rev(indl=="lower"))[1]
        #  checking from the left: last point of upper polygon that is NOT ok
        lnumu<-which(indu=="higher")[1]-1
        #  special case all points of lower polygon are upper
        if(is.na(lnuml)) lnuml<-sum(pg[lnumu,1]<pgl[,1])
        #  special case all points of upper polygon are lower
        if(is.na(lnumu)) lnumu<-sum(pg[,1]<pgl[lnuml,1])
        xyl<-pgl[lnuml,]; xyu<-pg[lnumu,]
        # cat("left"); print(lnuml); print(xyl)
        # cat("left"); print(lnumu); print(xyu)
        sl<-cut.p.sl.p.sl(xyl[1:2],xyl[3],xyu[1:2],xyu[3])
      }
      # if(kkk==2){ ##show pg pgl##; INDU<<-indu; INDL<<-indl; PGL<<-pgl; PGU<<-pg}
      pg<-rbind(pg [indu=="higher",1:2,drop=FALSE],sr,
                pgl[indl=="lower", 1:2,drop=FALSE],sl)
      if(debug.plots=="all") lines(rbind(pg,pg[1,]),col="red")
      if(!any(is.na(pg)))  pg<-pg[chull(pg[,1],pg[,2]),]
      # if(kkk==7){ PG <<- pg }
    } else {
      if(2<nrow(pgl)){ #121204
        pg <- rbind(pg[2,1:2],pgl[-c(1,length(pgl[,1])),1:2])
      } else {
        pg <- rbind(pg [-c(1,length(pg [,1])),1:2],pgl[2,1:2])
        # rbind(pgl[2,1:2],pg[2,1:2])
      }
    }
    if(verbose) cat("END of computation of the directions")
    hull.center<-cbind(pg[,1]*xysd[1]+xym[1],pg[,2]*xysd[2]+xym[2])
    if(!any(is.na(hull.center))) center<-find.polygon.center(hull.center) else
      hull.center <- rbind(center)       # 121126
    if(verbose){ cat("CENTER"); print(center) }
    if(verbose){cat("hull.center",hull.center); print(table(tphdepth)) }
  }
  # if(verbose) cat("center depth:",hdepth.of.points(rbind(center))-1)
  if(verbose) cat("center depth:",find.hdepths.tp(rbind(center),xy)-1)
  if(verbose){print("end of computation of center"); print(center)}
  if(dkmethod==1){

    # inner hull of bag
    xyi<-xy[hdepth>=k,,drop=FALSE] # cat("dim XYI", dim(xyi))
    # 121028 some corrections for strange k situations
    if(0 < length(xyi)) pdk<-xyi[chull(xyi[,1],xyi[,2]),,drop=FALSE]
    # outer hull of bag
    if( k > 1 ){
      xyo<-xy[hdepth>=(k-1),,drop=FALSE]
      pdk.1<-xyo[chull(xyo[,1],xyo[,2]),,drop=FALSE]
    } else pdk.1 <- pdk
    if(0 == length(xyi)) pdk <- pdk.1
    if(verbose)cat("hull computed: pdk, pdk.1:")
    if(verbose){print(pdk); print(pdk.1) }
    if(debug.plots=="all"){
      plot(xy,bty="n")
      h<-rbind(pdk,pdk[1,]); lines(h,col="red",lty=2)
      h<-rbind(pdk.1,pdk.1[1,]);lines(h,col="blue",lty=3)
      points(center[1],center[2],pch=8,col="red")
    }
    exp.dk<-expand.hull(pdk,k)
    exp.dk.1<-expand.hull(exp.dk,k-1) # pdk.1,k-1,20)
  }else{

    # define direction for hdepth search
    num<-floor(2*c(417,351,171,85,67,43)[sum(n>c(1,50,100,150,200,250))]*precision)
    num.h<-floor(num/2); angles<-seq(0,pi,length=num.h)
    ang<-tan(pi/2-angles)
    # standardization of data set xyxy is used
    kkk<-k
    if(verbose) print("find polygon with depth something higher than that of the bag")
    if( kkk <= max(d.k[,2]) ){ # inner one ### 121030

      ia<-1; a<-angles[ia]; xyt<-xyxy%*%c(cos(a),-sin(a)); xyto<-order(xyt)
      # initial for upper part
      ind.k<-xyto[kkk]; cutp<-c(xyxy[ind.k,1],-10)
      dxy<-diff(range(xyxy))
      pg<-rbind(c(cutp[1],-dxy,Inf),c(cutp[1],dxy,NA))
      # initial for lower part
      ind.kk<-xyto[n+1-kkk]; cutpl<-c(xyxy[ind.kk,1],10)
      # pgl<-rbind(c(cutpl[1],dxy,Inf),c(cutpl[1],-dxy,NA))
      pgl<-rbind(c(cutpl[1],dxy,-Inf),c(cutpl[1],-dxy,NA))
      # the sign of inf doesn't matter
      if(debug.plots=="all"){ plot(xyxy,type="p",bty="n")
        text(xy,,1:n,col="blue")
        hx<-xy[ind.k,c(1,1)]; hy<-xy[ind.k,c(2,2)]
        segments(hx,hy,c(10,-10),hy+ang[ia]*(c(10,-10)-hx),lty=2)
        text(hx+rnorm(1,,.1),hy+rnorm(1,,.1),ia)
      }
      if(verbose) cat("start of computation of the directions: ","kkk=",kkk) # 121030
      for(ia in seq(angles)[-1]){

        # determine critical points pnew and pnewl of direction a
        # if(verbose) cat("ia",ia,angles[ia])
        # 121030
        a<-angles[ia]; angtan<-ang[ia]; xyt<-xyxy%*%c(cos(a),-sin(a)); xyto<-order(xyt)
        ind.k <-xyto[kkk]; ind.kk<-xyto[n+1-kkk]; pnew<-xyxy[ind.k,]; pnewl<-xyxy[ind.kk,]
        # if(verbose) if( 1 < sum(xyt == xyt[ind.k]) )print("WARNING: some points identical")
        if(debug.plots=="all") points(pnew[1],pnew[2],col="red")
        # new limiting lines are defined by pnew / pnewl and slope a
        # find segment of polygon that is cut by new limiting line and cut
        # if(ia>200) { #<show pg pgl>#; points(pnew[1],pnew[2],col="magenta",cex=6) }
        if( abs(angtan)>1e10){ if(verbose) cat("kkk",kkk,"x=c case")
          # case of vertical slope #print(pg);print(pnew);print(xyt);lines(pg,col="red",lwd=3)
          # number of points left of point pnew that limit the polygon
          pg.no<-sum(pg[,1]<pnew[1])
          if( 0 < pg.no ){
            # the polygon (segment pg.no) has to be cut at x==pnew[1]
            cutp <- c(pnew[1], pg [pg.no, 2]+pg [pg.no, 3]*(pnew [1]-pg [pg.no ,1]))
            pg<- rbind(pg[1:pg.no,],  c(cutp,angtan), c(cutp[1]+dxy,  cutp[2] +angtan*dxy,NA))
          } else {
            if(verbose) cat("!!! case degenerated UPPER polygon: pg.no==0")
            # the limiting point pnew is above the beginning of the polygon
            # therefore, the polygon reduces to line
            pg <- rbind(pg[1,], c(pg[2,1:2],NA))
          }
          pg.nol<-sum(pgl[,1]>=pnewl[1])
          if( 0 < pg.nol ){ #??2 ### 121204
            cutpl<-c(pnewl[1],pgl[pg.nol,2]+pgl[pg.nol,3]*(pnewl[1]-pgl[pg.nol,1]))
            pgl<-rbind(pgl[1:pg.nol,],c(cutpl,angtan),c(cutpl[1]-dxy, cutpl[2]-angtan*dxy,NA))
          } else {
            if(verbose) cat("!!! case degenerated LOWER polygon: pgl.no==0")
            pgl <- rbind(pgl[1,], c(pgl[2,1:2],NA))
          }
        }else{ # if(verbose) cat("kkk",kkk,"normal case")
          # normal case upper polygon
          pg.inter<-pg[,2]-angtan*pg[,1]; pnew.inter<-pnew[2]-angtan*pnew[1]
          pg.no<-sum(pg.inter<pnew.inter)
          if(is.na(pg[pg.no,3])) pg[pg.no,3] <- -Inf # 121129 NaN/Na error
          cutp<-cut.p.sl.p.sl(pnew,ang[ia],pg[pg.no,1:2],pg[pg.no,3])
          pg<- rbind(pg[1:pg.no,],  c(cutp,angtan), c(cutp[1]+dxy,  cutp[2] +angtan*dxy,NA))
          # normal case lower polygon
          pg.interl<-pgl[,2]-angtan*pgl[,1]; pnew.interl<-pnewl[2]-angtan*pnewl[1]
          pg.nol<-sum(pg.interl>pnew.interl)
          if(is.na(pgl[pg.nol,3])) pgl[pg.nol,3] <- Inf # 121129 NaN/Na error
          cutpl<-cut.p.sl.p.sl(pnewl,angtan,pgl[pg.nol,1:2],pgl[pg.nol,3])
          pgl<-rbind(pgl[1:pg.nol,],c(cutpl,angtan),c(cutpl[1]-dxy, cutpl[2]-angtan*dxy,NA))
        }
        # if(kkk==KKK && ia == 51) { cat("ENDE: pgl"); print(pgl) }
        # update pg, pgl completed
        # PG<<-pg;PG.NO<<-pg.no;CUTP<<-cutp;DXY<<-dxy;PNEW<<-pnew;PGL<<-pgl;PG.NOL<<-pg.nol
        #### ***********************
        #### cat("angtan",angtan,"pg.no",pg.no,"pkt:",pnew)
        # if(ia==stopp) lines(pg,type="b",col="green")
        if(debug.plots=="all"){
          points(pnew[1],pnew[2],col="red")
          hx<-xyxy[ind.k,c(1,1)]; hy<-xyxy[ind.k,c(2,2)]
          segments(hx,hy,c(10,-10),hy+ang[ia]*(c(10,-10)-hx),lty=2)
          # text(hx+rnorm(1,,.1),hy+rnorm(1,,.1),ia)
          # print(pg)
          # if(ia==stopp) lines(pgl,type="b",col="green")
          points(cutpl[1],cutpl[2],col="red")
          hx<-xyxy[ind.kk,c(1,1)]; hy<-xyxy[ind.kk,c(2,2)]
          segments(hx,hy,c(10,-10),hy+ang[ia]*(c(10,-10)-hx),lty=2)
          #  text(hx+rnorm(1,,.1),hy+rnorm(1,,.1),ia)
          # print(pgl)
        }
        ##show pg pgl##
      }
      # if(verbose) PG <<- pg; PGL <<- pgl
      if(2<nrow(pg) && 2<nrow(pgl)){

        # plot(xyxy[,1:2],xlim=c(-.5,+.5),ylim=c(-.5,.50))
        # lines(pg,type="b",col="red"); lines(pgl,type="b",col="blue")
        # remove first and last points and multiple points #<show pg pgl>#
        limit<-1e-10
        # pg <-pg [c(TRUE,(abs(diff(pg [,1]))>limit)|(abs(diff(pg [,2]))>limit)),] old#
        idx <- c(TRUE,(abs(diff(pg [,1]))>limit)|(abs(diff(pg [,2]))>limit)) # 121008
        if(any(idx==FALSE)){
          pg <-pg[idx,]; pg[,3] <- c(diff(pg[,2])/diff(pg[,1]), NA)
        }
        # old reduction which caused some errors:
        # pgl<-pgl[c(TRUE,(abs(diff(pgl[,1]))>limit)|(abs(diff(pgl[,2]))>limit)),] error##
        # pgl<-pgl[c(     (abs(diff(pgl[,1]))>limit)|(abs(diff(pgl[,2]))>limit),TRUE),] old#
        idx <-      c(     (abs(diff(pgl[,1]))>limit)|(abs(diff(pgl[,2]))>limit),TRUE)#121008
        if(any(idx==FALSE)){
          pgl<-pgl[idx,]; pgl[,3] <- c(diff(pgl[,2])/diff(pgl[,1]), NA)
        }
        # add some tolerance in course of numerical problems
        pgl[,2]<-pgl[,2] - .00001  #### 121004
        # show pg pgl>>
        pg<- pg [-nrow(pg ),][-1,,drop=FALSE]
        pgl<-pgl[-nrow(pgl),][-1,,drop=FALSE]
        # determine position according to the other polygon
        #   cat("relative position: lower polygon")
        indl<-pos.to.pg(round(pgl,digits=10),round(pg,digits=10))  # 121126
        #   cat("relative position: upper polygon")
        indu<-pos.to.pg(round(pg,digits=10),round(pgl,digits=10),TRUE)
        sr<-sl<-NULL # ; ##show pg pgl>>
        # right region
        if(indu[(npg<-nrow(pg))]=="lower" & indl[1]=="higher"){
          # cat("in if of right region: the upper polynom is somewhere lower")
          #  checking from the right: last point of lower polygon that is NOT ok
          rnuml<-which(indl=="lower")[1]-1
          #  checking from the left: last point of upper polygon that is ok
          rnumu<-npg+1-which(rev(indu=="higher"))[1]
          #  special case all points of lower polygon are upper
          if(is.na(rnuml)) rnuml<-sum(pg[rnumu,1]<pgl[,1])
          #  special case all points of upper polygon are lower
          if(is.na(rnumu)) rnumu<-sum(pg[,1]<pgl[rnuml,1])
          xyl<-pgl[rnuml,]; xyu<-pg[rnumu,]
          # cat("right"); print(rnuml); print(xyl)
          # cat("right"); print(rnumu); print(xyu)
          sr<-cut.p.sl.p.sl(xyl[1:2],xyl[3],xyu[1:2],xyu[3])
        }
        # left region
        if(indl[(npgl<-nrow(pgl))]=="higher"&indu[1]=="lower"){
          # cat("in if of left region: the upper polynom is somewhere lower")
          #  checking from the right: last point of lower polygon that is ok
          lnuml<-npgl+1-which(rev(indl=="lower"))[1]
          #  checking from the left: last point of upper polygon that is NOT ok
          lnumu<-which(indu=="higher")[1]-1
          #  special case all points of lower polygon are upper
          if(is.na(lnuml)) lnuml<-sum(pg[lnumu,1]<pgl[,1])
          #  special case all points of upper polygon are lower
          if(is.na(lnumu)) lnumu<-sum(pg[,1]<pgl[lnuml,1])
          xyl<-pgl[lnuml,]; xyu<-pg[lnumu,]
          # cat("left"); print(lnuml); print(xyl)
          # cat("left"); print(lnumu); print(xyu)
          sl<-cut.p.sl.p.sl(xyl[1:2],xyl[3],xyu[1:2],xyu[3])
        }
        # if(kkk==2){ ##show pg pgl##; INDU<<-indu; INDL<<-indl; PGL<<-pgl; PGU<<-pg}
        pg<-rbind(pg [indu=="higher",1:2,drop=FALSE],sr,
                  pgl[indl=="lower", 1:2,drop=FALSE],sl)
        if(debug.plots=="all") lines(rbind(pg,pg[1,]),col="red")
        if(!any(is.na(pg)))  pg<-pg[chull(pg[,1],pg[,2]),]
        # if(kkk==7){ PG <<- pg }
      } else {
        if(2<nrow(pgl)){ #121204
          pg <- rbind(pg[2,1:2],pgl[-c(1,length(pgl[,1])),1:2])
        } else {
          pg <- rbind(pg [-c(1,length(pg [,1])),1:2],pgl[2,1:2])
          # rbind(pgl[2,1:2],pg[2,1:2])
        }
      }
      if(verbose) cat("END of computation of the directions")
      exp.dk<-cbind(pg[,1]*xysd[1]+xym[1],pg[,2]*xysd[2]+xym[2])
    } else {
      exp.dk <- NULL
    }
    if( 1 < kkk ) kkk<-kkk-1 # outer one
    if(verbose) print("find polygon with depth a little bit lower than that of the bag")
    ia<-1; a<-angles[ia]; xyt<-xyxy%*%c(cos(a),-sin(a)); xyto<-order(xyt)
    # initial for upper part
    ind.k<-xyto[kkk]; cutp<-c(xyxy[ind.k,1],-10)
    dxy<-diff(range(xyxy))
    pg<-rbind(c(cutp[1],-dxy,Inf),c(cutp[1],dxy,NA))
    # initial for lower part
    ind.kk<-xyto[n+1-kkk]; cutpl<-c(xyxy[ind.kk,1],10)
    # pgl<-rbind(c(cutpl[1],dxy,Inf),c(cutpl[1],-dxy,NA))
    pgl<-rbind(c(cutpl[1],dxy,-Inf),c(cutpl[1],-dxy,NA))
    # the sign of inf doesn't matter
    if(debug.plots=="all"){ plot(xyxy,type="p",bty="n")
      text(xy,,1:n,col="blue")
      hx<-xy[ind.k,c(1,1)]; hy<-xy[ind.k,c(2,2)]
      segments(hx,hy,c(10,-10),hy+ang[ia]*(c(10,-10)-hx),lty=2)
      text(hx+rnorm(1,,.1),hy+rnorm(1,,.1),ia)
    }
    if(verbose) cat("start of computation of the directions: ","kkk=",kkk) # 121030
    for(ia in seq(angles)[-1]){

      # determine critical points pnew and pnewl of direction a
      # if(verbose) cat("ia",ia,angles[ia])
      # 121030
      a<-angles[ia]; angtan<-ang[ia]; xyt<-xyxy%*%c(cos(a),-sin(a)); xyto<-order(xyt)
      ind.k <-xyto[kkk]; ind.kk<-xyto[n+1-kkk]; pnew<-xyxy[ind.k,]; pnewl<-xyxy[ind.kk,]
      # if(verbose) if( 1 < sum(xyt == xyt[ind.k]) )print("WARNING: some points identical")
      if(debug.plots=="all") points(pnew[1],pnew[2],col="red")
      # new limiting lines are defined by pnew / pnewl and slope a
      # find segment of polygon that is cut by new limiting line and cut
      # if(ia>200) { #<show pg pgl>#; points(pnew[1],pnew[2],col="magenta",cex=6) }
      if( abs(angtan)>1e10){ if(verbose) cat("kkk",kkk,"x=c case")
        # case of vertical slope #print(pg);print(pnew);print(xyt);lines(pg,col="red",lwd=3)
        # number of points left of point pnew that limit the polygon
        pg.no<-sum(pg[,1]<pnew[1])
        if( 0 < pg.no ){
          # the polygon (segment pg.no) has to be cut at x==pnew[1]
          cutp <- c(pnew[1], pg [pg.no, 2]+pg [pg.no, 3]*(pnew [1]-pg [pg.no ,1]))
          pg<- rbind(pg[1:pg.no,],  c(cutp,angtan), c(cutp[1]+dxy,  cutp[2] +angtan*dxy,NA))
        } else {
          if(verbose) cat("!!! case degenerated UPPER polygon: pg.no==0")
          # the limiting point pnew is above the beginning of the polygon
          # therefore, the polygon reduces to line
          pg <- rbind(pg[1,], c(pg[2,1:2],NA))
        }
        pg.nol<-sum(pgl[,1]>=pnewl[1])
        if( 0 < pg.nol ){ ##??2 ### 121204
          cutpl<-c(pnewl[1],pgl[pg.nol,2]+pgl[pg.nol,3]*(pnewl[1]-pgl[pg.nol,1]))
          pgl<-rbind(pgl[1:pg.nol,],c(cutpl,angtan),c(cutpl[1]-dxy, cutpl[2]-angtan*dxy,NA))
        } else {
          if(verbose) cat("!!! case degenerated LOWER polygon: pgl.no==0")
          pgl <- rbind(pgl[1,], c(pgl[2,1:2],NA))
        }
      }else{ # if(verbose) cat("kkk",kkk,"normal case")
        # normal case upper polygon
        pg.inter<-pg[,2]-angtan*pg[,1]; pnew.inter<-pnew[2]-angtan*pnew[1]
        pg.no<-sum(pg.inter<pnew.inter)
        if(is.na(pg[pg.no,3])) pg[pg.no,3] <- -Inf # 121129 NaN/Na error
        cutp<-cut.p.sl.p.sl(pnew,ang[ia],pg[pg.no,1:2],pg[pg.no,3])
        pg<- rbind(pg[1:pg.no,],  c(cutp,angtan), c(cutp[1]+dxy,  cutp[2] +angtan*dxy,NA))
        # normal case lower polygon
        pg.interl<-pgl[,2]-angtan*pgl[,1]; pnew.interl<-pnewl[2]-angtan*pnewl[1]
        pg.nol<-sum(pg.interl>pnew.interl)
        if(is.na(pgl[pg.nol,3])) pgl[pg.nol,3] <- Inf # 121129 NaN/Na error
        cutpl<-cut.p.sl.p.sl(pnewl,angtan,pgl[pg.nol,1:2],pgl[pg.nol,3])
        pgl<-rbind(pgl[1:pg.nol,],c(cutpl,angtan),c(cutpl[1]-dxy, cutpl[2]-angtan*dxy,NA))
      }
      # if(kkk==KKK && ia == 51) { cat("ENDE: pgl"); print(pgl) }
      # update pg, pgl completed
      # PG<<-pg;PG.NO<<-pg.no;CUTP<<-cutp;DXY<<-dxy;PNEW<<-pnew;PGL<<-pgl;PG.NOL<<-pg.nol
      #### ---**************************
      # cat("angtan",angtan,"pg.no",pg.no,"pkt:",pnew)
      # if(ia==stopp) lines(pg,type="b",col="green")
      if(debug.plots=="all"){
        points(pnew[1],pnew[2],col="red")
        hx<-xyxy[ind.k,c(1,1)]; hy<-xyxy[ind.k,c(2,2)]
        segments(hx,hy,c(10,-10),hy+ang[ia]*(c(10,-10)-hx),lty=2)
        # text(hx+rnorm(1,,.1),hy+rnorm(1,,.1),ia)
        # print(pg)
        # if(ia==stopp) lines(pgl,type="b",col="green")
        points(cutpl[1],cutpl[2],col="red")
        hx<-xyxy[ind.kk,c(1,1)]; hy<-xyxy[ind.kk,c(2,2)]
        segments(hx,hy,c(10,-10),hy+ang[ia]*(c(10,-10)-hx),lty=2)
        #  text(hx+rnorm(1,,.1),hy+rnorm(1,,.1),ia)
        # print(pgl)
      }
      ##show pg pgl##
    }
    # if(verbose) PG <<- pg; PGL <<- pgl
    if(2<nrow(pg) && 2<nrow(pgl)){

      # plot(xyxy[,1:2],xlim=c(-.5,+.5),ylim=c(-.5,.50))
      # lines(pg,type="b",col="red"); lines(pgl,type="b",col="blue")
      # remove first and last points and multiple points #<show pg pgl>#
      limit<-1e-10
      # pg <-pg [c(TRUE,(abs(diff(pg [,1]))>limit)|(abs(diff(pg [,2]))>limit)),] old#
      idx <- c(TRUE,(abs(diff(pg [,1]))>limit)|(abs(diff(pg [,2]))>limit)) # 121008
      if(any(idx==FALSE)){
        pg <-pg[idx,]; pg[,3] <- c(diff(pg[,2])/diff(pg[,1]), NA)
      }
      # old reduction which caused some errors:
      # pgl<-pgl[c(TRUE,(abs(diff(pgl[,1]))>limit)|(abs(diff(pgl[,2]))>limit)),] error##
      # pgl<-pgl[c(     (abs(diff(pgl[,1]))>limit)|(abs(diff(pgl[,2]))>limit),TRUE),] old#
      idx <-      c(     (abs(diff(pgl[,1]))>limit)|(abs(diff(pgl[,2]))>limit),TRUE)#121008
      if(any(idx==FALSE)){
        pgl<-pgl[idx,]; pgl[,3] <- c(diff(pgl[,2])/diff(pgl[,1]), NA)
      }
      # add some tolerance in course of numerical problems
      pgl[,2]<-pgl[,2] - .00001  #### 121004
      # show pg pgl>>
      pg<- pg [-nrow(pg ),][-1,,drop=FALSE]
      pgl<-pgl[-nrow(pgl),][-1,,drop=FALSE]
      # determine position according to the other polygon
      #   cat("relative position: lower polygon")
      indl<-pos.to.pg(round(pgl,digits=10),round(pg,digits=10))  # 121126
      #   cat("relative position: upper polygon")
      indu<-pos.to.pg(round(pg,digits=10),round(pgl,digits=10),TRUE)
      sr<-sl<-NULL # ; ##show pg pgl>>
      # right region
      if(indu[(npg<-nrow(pg))]=="lower" & indl[1]=="higher"){
        # cat("in if of right region: the upper polynom is somewhere lower")
        #  checking from the right: last point of lower polygon that is NOT ok
        rnuml<-which(indl=="lower")[1]-1
        #  checking from the left: last point of upper polygon that is ok
        rnumu<-npg+1-which(rev(indu=="higher"))[1]
        #  special case all points of lower polygon are upper
        if(is.na(rnuml)) rnuml<-sum(pg[rnumu,1]<pgl[,1])
        #  special case all points of upper polygon are lower
        if(is.na(rnumu)) rnumu<-sum(pg[,1]<pgl[rnuml,1])
        xyl<-pgl[rnuml,]; xyu<-pg[rnumu,]
        # cat("right"); print(rnuml); print(xyl)
        # cat("right"); print(rnumu); print(xyu)
        sr<-cut.p.sl.p.sl(xyl[1:2],xyl[3],xyu[1:2],xyu[3])
      }
      # left region
      if(indl[(npgl<-nrow(pgl))]=="higher"&indu[1]=="lower"){
        # cat("in if of left region: the upper polynom is somewhere lower")
        #  checking from the right: last point of lower polygon that is ok
        lnuml<-npgl+1-which(rev(indl=="lower"))[1]
        #  checking from the left: last point of upper polygon that is NOT ok
        lnumu<-which(indu=="higher")[1]-1
        #  special case all points of lower polygon are upper
        if(is.na(lnuml)) lnuml<-sum(pg[lnumu,1]<pgl[,1])
        #  special case all points of upper polygon are lower
        if(is.na(lnumu)) lnumu<-sum(pg[,1]<pgl[lnuml,1])
        xyl<-pgl[lnuml,]; xyu<-pg[lnumu,]
        # cat("left"); print(lnuml); print(xyl)
        # cat("left"); print(lnumu); print(xyu)
        sl<-cut.p.sl.p.sl(xyl[1:2],xyl[3],xyu[1:2],xyu[3])
      }
      # if(kkk==2){ ##show pg pgl##; INDU<<-indu; INDL<<-indl; PGL<<-pgl; PGU<<-pg}
      pg<-rbind(pg [indu=="higher",1:2,drop=FALSE],sr,
                pgl[indl=="lower", 1:2,drop=FALSE],sl)
      if(debug.plots=="all") lines(rbind(pg,pg[1,]),col="red")
      if(!any(is.na(pg)))  pg<-pg[chull(pg[,1],pg[,2]),]
      # if(kkk==7){ PG <<- pg }
    } else {
      if(2<nrow(pgl)){ #121204
        pg <- rbind(pg[2,1:2],pgl[-c(1,length(pgl[,1])),1:2])
      } else {
        pg <- rbind(pg [-c(1,length(pg [,1])),1:2],pgl[2,1:2])
        # rbind(pgl[2,1:2],pg[2,1:2])
      }
    }
    if(verbose) cat("END of computation of the directions")
    exp.dk.1<-cbind(pg[,1]*xysd[1]+xym[1],pg[,2]*xysd[2]+xym[2])
    if(is.null(exp.dk)) exp.dk <- exp.dk.1
    # EX.1 <<- exp.dk.1; EX   <<- exp.dk
    if(verbose) print("End of find hulls, method two")
  }

  # if(max(d.k[,2])==k.1||nrow(d.k)==1) lambda<-0 else {  ### 121027
  if(nrow(d.k)==k.1 || nrow(d.k)==1) lambda<-0 else {  # 121126
    ind <- sum(d.k[,2] <= k.1) # complicated, may be wrong in case of missing depths
    ind <- k.1 # 121123
    ndk.1 <- d.k[ ind, 1]
    ndk   <- d.k[ ind+1, 1] # number inner
    #         (halve - number inner)/(number outer - number inner)
    lambda  <-(n/2-ndk)             /(ndk.1   - ndk)
    # lambda<-(n/2-d.k[k.1+1,1])    /(d.k[k.1,1]-d.k[k.1+1,1]) ### old
    # cat(n/2, ndk,ndk.1, "k.1",k.1,"ind",ind)
  }
  if(verbose) cat("lambda",lambda)

  cut.on.pdk.1<-find.cut.z.pg(exp.dk,  exp.dk.1,center=center)
  # print("HALLO"); print(cut.on.pdk.1)
  cut.on.pdk  <-find.cut.z.pg(exp.dk.1,exp.dk,  center=center)
  # expand inner polgon exp.dk
  h1<-(1-lambda)*exp.dk+lambda*cut.on.pdk.1
  # shrink outer polygon exp.dk.1
  h2<-(1-lambda)*cut.on.pdk+lambda*exp.dk.1
  h<-rbind(h1,h2);
  h<-h[!is.nan(h[,1])&!is.nan(h[,2]),]
  hull.bag<-h[chull(h[,1],h[,2]),]
  # if(verbose){
  #   plot(xy); lines(exp.dk,col="red"); lines(exp.dk.1,col="blue");
  #   segments(cut.on.pdk[,1],cut.on.pdk[,2],exp.dk.1[,1],exp.dk.1[,2],col="red")
  #   segments(cut.on.pdk.1[,1],cut.on.pdk.1[,2],exp.dk[,1],exp.dk[,2],col="blue",lwd=3)
  #   points(cut.on.pdk.1,col="blue"); cat("cut.on.pdk.1"); print(cut.on.pdk.1)
  #   points(cut.on.pdk,col="red"); cat("cut.on.pdk"); print(cut.on.pdk)
  #   lines(hull.bag,col="green")
  # }
  if(verbose)cat("bag completed:")
  #if(verbose) print(hull.bag)
  if(debug.plots=="all"){   lines(hull.bag,col="red") }

  hull.loop<-cbind(hull.bag[,1]-center[1],hull.bag[,2]-center[2])
  hull.loop<-factor*hull.loop
  hull.loop<-cbind(hull.loop[,1]+center[1],hull.loop[,2]+center[2])
  if(verbose) cat("loop computed")

  if(!very.large.data.set){
    pxy.bag    <-xydata[hdepth>= k   ,,drop=FALSE]
    pkt.cand   <-xydata[hdepth==(k-1),,drop=FALSE]
    pkt.not.bag<-xydata[hdepth< (k-1),,drop=FALSE]
    if( 0 < length(pkt.cand) && 0 < length(hull.bag) ){
      outside<-out.of.polygon(pkt.cand,hull.bag)
      if(sum(!outside)>0)
        pxy.bag    <-rbind(pxy.bag,     pkt.cand[!outside,])
      if(sum( outside)>0)
        pkt.not.bag<-rbind(pkt.not.bag, pkt.cand[ outside,])
    }
  }else {
    extr<-out.of.polygon(xydata,hull.bag)
    pxy.bag    <-xydata[!extr,]
    pkt.not.bag<-xydata[extr,,drop=FALSE]
  }
  if(length(pkt.not.bag)>0){
    extr<-out.of.polygon(pkt.not.bag,hull.loop)
    pxy.outlier<-pkt.not.bag[extr,,drop=FALSE]
    if(0==length(pxy.outlier)) pxy.outlier<-NULL
    pxy.outer<-pkt.not.bag[!extr,,drop=FALSE]
  }else{
    pxy.outer<-pxy.outlier<-NULL
  }
  if(verbose) cat("points of bag, outer points and outlier identified")

  hull.loop<-rbind(pxy.outer,hull.bag)
  hull.loop<-hull.loop[chull(hull.loop[,1],hull.loop[,2]),]
  if(verbose) cat("end of computation of loop")

  res<-list(
    center=center,
    hull.center=hull.center,
    hull.bag=hull.bag,
    hull.loop=hull.loop,
    pxy.bag=pxy.bag,
    pxy.outer=if(length(pxy.outer)>0) pxy.outer else NULL,
    pxy.outlier=if(length(pxy.outlier)>0) pxy.outlier else NULL,
    hdepths=hdepth,
    is.one.dim=is.one.dim,
    prdata=prdata,
    # random.seed=random.seed,  ###SEED
    xy=xy,xydata=xydata
  )
  if(verbose) res<-c(res,list(exp.dk=exp.dk,exp.dk.1=exp.dk.1,hdepth=hdepth))
  class(res)<-"bagplot"
  return(res)
}

plot.bagplot <- function(x,
                         show.outlier=TRUE,# if TRUE outlier are shown
                         show.whiskers=TRUE, # if TRUE whiskers are shown
                         show.looppoints=TRUE, # if TRUE points in loop are shown
                         show.bagpoints=TRUE, # if TRUE points in bag are shown
                         show.loophull=TRUE, # if TRUE loop is shown
                         show.baghull=TRUE, # if TRUE bag is shown
                         add=FALSE, # if TRUE graphical elements are added to actual plot
                         pch=16,cex=.4, # to define further parameters of plot
                         verbose=FALSE, # tools for debugging
                         col.loophull="#aaccff", # Alternatives: #ccffaa, #ffaacc
                         col.looppoints="#3355ff", # Alternatives: #55ff33, #ff3355
                         col.baghull="#7799ff", # Alternatives: #99ff77, #ff7799
                         col.bagpoints="#000088", # Alternatives: #008800, #880000
                         transparency=FALSE,...
){
  if(missing(x)) return(
    "bagplot, version 2012/12/05, peter wolf"
  )
  # transparency flag and color flags have been proposed by wouter
  if (transparency==TRUE) {
    col.loophull = paste(col.loophull, "99", sep="")
    col.baghull = paste(col.baghull, "99", sep="")
  }

  win<-function(dx,dy){  atan2(y=dy,x=dx) }

  cut.z.pg<-function(zx,zy,p1x,p1y,p2x,p2y){
    a2<-(p2y-p1y)/(p2x-p1x); a1<-zy/zx
    sx<-(p1y-a2*p1x)/(a1-a2); sy<-a1*sx
    sxy<-cbind(sx,sy)
    h<-any(is.nan(sxy))||any(is.na(sxy))||any(Inf==abs(sxy))
    if(h){ # print("NAN found"); print(cbind(a1,a2,zx,zy,sxy,p2x-p1x))
      if(!exists("verbose")) verbose<-FALSE
      if(verbose) cat("special")
      # zx is zero ### 121030
      h<-0==zx
      sx<-ifelse(h,zx,sx); sy<-ifelse(h,p1y-a2*p1x,sy)
      # points on line defined by line segment
      a1 <- ifelse( abs(a1) == Inf, sign(a1)*123456789*1E10, a1) # 121030
      a2 <- ifelse( abs(a2) == Inf, sign(a2)*123456789*1E10, a2)
      # points on line defined by line segment
      h<-0==(a1-a2) & sign(zx)==sign(p1x)
      sx<-ifelse(h,p1x,sx); sy<-ifelse(h,p1y,sy)
      h<-0==(a1-a2) & sign(zx)!=sign(p1x)
      sx<-ifelse(h,p2x,sx); sy<-ifelse(h,p2y,sy)
      # line segment vertical
      #   & center NOT ON line segment
      h<-p1x==p2x & zx!=p1x & p1x!=0
      sx<-ifelse(h,p1x,sx); sy<-ifelse(h,zy*p1x/zx,sy)
      #   & center ON line segment
      h<-p1x==p2x & zx!=p1x & p1x==0
      sx<-ifelse(h,p1x,sx); sy<-ifelse(h,0,sy)
      #   & center NOT ON line segment & point on line     ### 121126
      h<-p1x==p2x & zx==p1x & p1x!=0 # & sign(zy)==sign(p1y)
      sx<-ifelse(h,zx,sx); sy<-ifelse(h,zy,sy)
      #   & center ON line segment & point on line
      h<-p1x==p2x & zx==p1x & p1x==0 & sign(zy)==sign(p1y)
      sx<-ifelse(h,p1x,sx); sy<-ifelse(h,p1y,sy)
      h<-p1x==p2x & zx==p1x & p1x==0 & sign(zy)!=sign(p1y)
      sx<-ifelse(h,p1x,sx); sy<-ifelse(h,p2y,sy)
      #  points identical to end points of line segment
      h<-zx==p1x & zy==p1y; sx<-ifelse(h,p1x,sx); sy<-ifelse(h,p1y,sy)
      h<-zx==p2x & zy==p2y; sx<-ifelse(h,p2x,sx); sy<-ifelse(h,p2y,sy)
      # point of z is center
      h<-zx==0 & zy==0; sx<-ifelse(h,0,sx); sy<-ifelse(h,0,sy)
      sxy<-cbind(sx,sy)
    } # end of special cases
    #if(verbose){ print(rbind(a1,a2));print(cbind(zx,zy,p1x,p1y,p2x,p2y,sxy))}
    if(!exists("debug.plots")) debug.plots<-"no"
    if(debug.plots=="all"){
      segments(sxy[,1],sxy[,2],zx,zy,col="red")
      segments(0,0,sxy[,1],sxy[,2],col="green",lty=2) ##!!
      points(sxy,col="red")
    }
    return(sxy)
  }

  find.cut.z.pg<-function(z,pg,center=c(0,0),debug.plots="no"){
    if(!is.matrix(z)) z<-rbind(z)
    if(1==nrow(pg)) return(matrix(center,nrow(z),2,TRUE))
    n.pg<-nrow(pg); n.z<-nrow(z)
    z<-cbind(z[,1]-center[1],z[,2]-center[2])
    pgo<-pg; pg<-cbind(pg[,1]-center[1],pg[,2]-center[2])
    if(!exists("debug.plots")) debug.plots<-"no"
    if(debug.plots=="all"){
      plot(rbind(z,pg,0),bty="n"); points(z,pch="p")
      lines(c(pg[,1],pg[1,1]),c(pg[,2],pg[1,2]))}
    # find angles of pg und z
    apg<-win(pg[,1],pg[,2])
    apg[is.nan(apg)]<-0; a<-order(apg); apg<-apg[a]; pg<-pg[a,]
    az<-win(z[,1],z[,2])
    # find line segments
    segm.no<-apply((outer(apg,az,"<")),2,sum)
    segm.no<-ifelse(segm.no==0,n.pg,segm.no)
    next.no<-1+(segm.no %% length(apg))
    # compute cut points
    cuts<-cut.z.pg(z[,1],z[,2],pg[segm.no,1],pg[segm.no,2],
                   pg[next.no,1],pg[next.no,2])
    # rescale
    cuts<-cbind(cuts[,1]+center[1],cuts[,2]+center[2])
    return(cuts)
  }
  # find.cut.z.pg(EX,  EX1,center=CE)

  center<-hull.center<-hull.bag<-hull.loop<-pxy.bag<-pxy.outer<-pxy.outlier<-NULL
  # random.seed <-
  hdepths<-is.one.dim<-prdata<-xy<-xydata<-exp.dk<-exp.dk.1<-hdepth<-NULL
  tphdepth<-tp<-NULL
  #090216
  bagplotobj<-x
  for(i in seq(along=bagplotobj))
    eval(parse(text=paste(names(bagplotobj)[i],"<-bagplotobj[[",i,"]]")))
  if(is.one.dim){

    if(!verbose) cat("data set one dimensional") # 121202
    ROT<-round(prdata[[2]],digits=5); IROT<-round(solve(ROT),digits=5)
    if(!add){ ## 121008 ## 121130
      plot(xydata,type="n",bty="n",pch=16,cex=1, ...) # xlim=xlim, ylim=ylim, ...)
    }
    # find five points for box and whiskers
    usr <- par()$usr; xlim <- usr[1:2]; ylim <- usr[3:4]
    mins <- usr[c(1,3)]; ranges <- usr[c(2,4)] - mins
    if(ROT[1,1]==0){ #  cat("FALL senkrecht")
      xydata <- cbind( mean(usr[1:2])  ,xydata[,2])
      boxplotres<-boxplot(xydata[,2],plot=FALSE)
      five<-cbind(mean(usr[1:2]),boxplotres$stat)
      dx <- 0.1*(xlim[2]-xlim[1]); dy <- 0
      idx.out <- if(0<length(boxplotres$out)) match(boxplotres$out, xydata[,2] ) else NULL
    }
    if(ROT[1,2]==0){ #  cat("FALL waagerecht")
      xydata <- cbind( xydata[,1], mean(usr[3:4]))
      boxplotres<-boxplot(xydata[,1],plot=FALSE)
      five<-cbind(boxplotres$stat,mean(usr[3:4]))
      dx <- 0; dy <- 0.1*(ylim[2]-ylim[1]) # 1/5 of del.y
      idx.out <- if(0<length(boxplotres$out)) match(boxplotres$out, xydata[,1] ) else NULL
    }
    if(ROT[1,2]!=0 && ROT[1,1]!=0){
      xytr<-xydata%*%ROT
      boxplotres<-boxplot(xytr[,1],plot=FALSE)
      five<-cbind(boxplotres$stat,xytr[1,2])%*%IROT
      # find small vector for box height
      vec <- five[5,] - five[1,]
      vec.ortho <- c(vec[2],-vec[1]) * ranges / par()$pin
      xy.delta <- vec.ortho * par()$pin[2:1] * ranges # plot region inches
      xy.delta <- xy.delta / sqrt( sum(xy.delta * xy.delta) )
      xy.delta <- xy.delta * .15 / ( sqrt(sum(abs(par()$pin*xy.delta/ranges)^2) ))
      dx <- xy.delta[1]; dy <- xy.delta[2]
      idx.out <- if(0<length(boxplotres$out)) match(boxplotres$out, xytr ) else NULL
    }
    # construct segments
    # whiskers
    segments(five[h<-c(1,5),1],five[h,2],five[h<-c(2,4),1],five[h,2], # col=col.looppoints,
             lwd=2)
    points(five[c(1,5),], cex=1, col=col.looppoints,pch=16)
    # box
    #segments(five[h<-2:4,1] + dx, five[h,2] + dy, five[h,1] - dx, five[h,2] - dy,
    #         col=col.bagpoints,lwd=2)
    #segments(five[2,1] + (h<-c(-1,1))*dx, five[2,2] + h*dy,
    #         five[4,1] + h*dx, five[4,2] + h*dy,
    #         col=col.bagpoints,lwd=2)
    polygon(five[c(2,4,4,2,2),1] + c(dx,dx,-dx,-dx,dx),
            five[c(2,4,4,2,2),2] + c(dy,dy,-dy,-dy,dy),
            col=col.baghull,lwd=1)
    # median
    segments(five[h<-3  ,1] + dx, five[h,2] + dy,
             five[h,1] - dx, five[h,2] - dy,col="red",lwd=3)
    # Outlier
    if(0 < length(idx.out) && !is.na(idx.out[1])){
      points(xydata[idx.out,,drop=FALSE], cex=1, pch=16,col="red")
    }
    #  segments(five[3,1],five[3,2],five[3,1]+1*vec.ortho[1],
    #           five[3,2]+100*vec.ortho[2],col="green",lwd=5)
    #  segments(five[3,1],five[3,2],five[3,1]+1*vec1[1],
    #           five[3,2]+1*vec1[2],col="red",lwd=5)
    #  points(five,cex=2,col="green")
    return("one dimensional boxplot plottet")
  } else {

    if(!add) plot(xydata,type="n",pch=pch,cex=cex,bty="n",...)
    if(verbose) text(xy[,1],xy[,2],paste(as.character(hdepth))) # cex=2 needs fonts
    # loop: --************
    if(show.loophull){ # fill loop
      h<-rbind(hull.loop,hull.loop[1,]); lines(h[,1],h[,2],lty=1)
      polygon(hull.loop[,1],hull.loop[,2],col=col.loophull)
    }
    if(show.looppoints && 0 < length(pxy.outer)){ # points in loop
      points(pxy.outer[,1],pxy.outer[,2],col=col.looppoints,pch=pch,cex=cex)
    }
    # bag: --*****************
    if(show.baghull && 0 < length(hull.bag)){ # fill bag
      h<-rbind(hull.bag,hull.bag[1,]); lines(h[,1],h[,2],lty=1)
      polygon(hull.bag[,1],hull.bag[,2],col=col.baghull)
    }
    if(show.bagpoints && 0 < length(pxy.bag)){ # points in bag
      points(pxy.bag[,1],pxy.bag[,2],col=col.bagpoints,pch=pch,cex=cex)
    }
    # whiskers
    if(show.whiskers && 0 < length(pxy.outer)){
      debug.plots<-"not"
      if((n<-length(xy[,1]))<15){
        segments(xy[,1],xy[,2],rep(center[1],n),rep(center[2],n),
                 col="red")
      }else{
        pkt.cut<-find.cut.z.pg(pxy.outer,hull.bag,center=center)
        segments(pxy.outer[,1],pxy.outer[,2],pkt.cut[,1],pkt.cut[,2],
                 col="red")
      }
    }
    # outlier: ---**********************
    if(show.outlier && 0 < length(pxy.outlier)){ # points in loop
      points(pxy.outlier[,1],pxy.outlier[,2],col="red",pch=pch,cex=cex)
    }
    # center:
    if(exists("hull.center") && 2 < length(hull.center)){
      h<-rbind(hull.center,hull.center[1,]); lines(h[,1],h[,2],lty=1)
      polygon(hull.center[,1],hull.center[,2],col="orange")
    }
    if(!is.one.dim) points(center[1],center[2],pch=8,col="red")
    if(verbose && 0 < length(exp.dk.1) ){
      h<-rbind(exp.dk,exp.dk[1,]); lines(h,col="blue",lty=2)
      h<-rbind(exp.dk.1,exp.dk.1[1,]); lines(h,col="black",lty=2, lwd=3)
      if(exists("tphdepth") && 0<length(tphdepth))
        text(tp[,1],tp[,2],as.character(tphdepth),col="green")
      text(xy[,1],xy[,2],paste(as.character(hdepth)))  # cex=2 needs special fonts
      points(center[1],center[2],pch=8,col="red")
    }
    "bagplot plottet"
  }
}

# find.hdepths <- function(xy, number.of.directions=181){ ### 121126

# xy <- as.matrix(xy)
# for( j in 1:2) {
# xy[,j] <- xy[,j] - min(xy[,j])
# if( 0 < (h <- max(xy[,j]))) xy[,j] <- xy[,j] / max(xy[,j])
# }

# phi    <- c(seq(0,180,length=number.of.directions)[-1]*(2*pi/360))
# sinphi <- c(sin(phi),1); cosphi <- c(cos(phi),0)
# RM1 <- round(digits=6,rbind(cosphi,sinphi))
# hd <- rep(h<-length(xy[,1]),h)
# for( j in seq(along=sinphi)){
# xyt <- xy %*% RM1[,j]
# hd <- pmin(hd,rank(xyt,ties.method="min"), rank(-xyt,ties.method="min"))
# }
# ###  xyt <- xy %*% RM1
# ### hd2 <- cbind(apply(xyt, 2, rank, ties.method="min"),
# ###               apply(-xyt,2, rank, ties.method="min"))
# ###  hd2 <- apply(hd2, 1, min)
# hd
# }
# find.hdepths.tp <- function(tp, data, number.of.directions=181){ ### 121130
# ### standardize dimensions ###
# xy <- as.matrix(data); tp <- as.matrix(rbind(tp)); n.tp <- dim(tp)[1]
# for( j in 1:2) {
# xy[,j] <- xy[,j] - (h <- min(xy[,j], na.rm=TRUE))
# tp[,j] <- tp[,j] -  h
# if( 0 < (h <- max(xy[,j], na.rm=TRUE))){
# xy[,j] <- xy[,j]/h; tp[,j] <- tp[,j]/h
# }
# }
# ##loop over directions##
# phi    <- c(seq(0,180,length=number.of.directions)[-1]*(2*pi/360))
# sinphi <- c(sin(phi),1); cosphi <- c(cos(phi),0)
# RM1 <- round(digits=6,rbind(cosphi,sinphi))
# hdtp <- rep(length(xy[,1]),length(tp[,1]))
# for( j in seq(along=sinphi)){ #print(j)
# xyt <- xy %*% RM1[,j]; tpt <- (tp %*% RM1[,j])[]
# xyt <- xyt[!is.na(xyt)] #; tpt <- sort(tpt)
# hdtp <- pmin(hdtp,(rank( c(tpt,xyt), ties.method="min"))[1:n.tp]
# -rank( tpt,ties.method="min")
# ,rank(-c(tpt,xyt), ties.method="min")[1:n.tp]
# -rank(-tpt,ties.method="min")
# )
# }
# hdtp
# }

# hdepth<-function(xy,data){
# ###function to compute the h-depths of points

# win<-function(dx,dy){  atan2(y=dy,x=dx) }
# if(missing(data)) data <- xy
# tp <- xy; xy <- data
# n.tp<-nrow(tp); n <- length(xy[,1])
# tphdepth<-rep(0,n.tp); dpi<-2*pi-0.000001
# for(j in 1:n.tp) {
# ### compute difference of coordinates of tp j and data
# dx<-tp[j,1]-xy[,1]; dy<-tp[j,2]-xy[,2]
# ### remove data points that are identical to tp j
# h <- tp[j,1] != xy[,1] & tp[j,2] != xy[,2]
# dx <- dx[h]; dy <- dy[h]; n <- length(dx)
# minusplus<-c(rep(-1,n),rep(1,n)) #### 070824
# ### compute angles of slopes of lines through tp j and data
# a<-win(dx,dy)+pi; h<-a<10; a<-a[h]; ident<-sum(!h)
# ### count number of angles that are lower than pi == points above tp j
# init<-sum(a < pi); a.shift<-(a+pi) %% dpi
# ### count points relative to the tp j in halve planes
# h<-cumsum(minusplus[order(c(a,a.shift))])
# ### find minimum number of points in a halve plane
# tphdepth[j]<-init+min(h)+1 ### +1 because of the point itself!!
# ### tphdepth[j]<-init+min(h)+ident; cat("SUMME",ident)
# }
# tphdepth
# }

# hdepth <- find.hdepths.tp #121202

PlotBag <- function(x, y,
                    factor=3, # expanding factor for bag to get the loop
                    na.rm=FALSE, # should 'NAs' values be removed or exchanged
                    approx.limit=300, # limit
                    show.outlier=TRUE,# if TRUE outlier are shown
                    show.whiskers=TRUE, # if TRUE whiskers are shown
                    show.looppoints=TRUE, # if TRUE points in loop are shown
                    show.bagpoints=TRUE, # if TRUE points in bag are shown
                    show.loophull=TRUE, # if TRUE loop is shown
                    show.baghull=TRUE, # if TRUE bag is shown
                    create.plot=TRUE, # if TRUE a plot is created
                    add=FALSE, # if TRUE graphical elements are added to actual plot
                    pch=16,cex=.4, # some graphical parameters
                    dkmethod=2, # in 1:2; there are two methods for approximating the bag
                    precision=1, # controls precision of computation
                    verbose=FALSE,debug.plots="no", # tools for debugging
                    col.loophull="#aaccff", # Alternatives: #ccffaa, #ffaacc
                    col.looppoints="#3355ff", # Alternatives: #55ff33, #ff3355
                    col.baghull="#7799ff", # Alternatives: #99ff77, #ff7799
                    col.bagpoints="#000088", # Alternatives: #008800, #880000
                    transparency=FALSE, ... # to define further parameters of plot
){
  if(missing(x)) return(
    "bagplot, version 2012/12/05, peter wolf"
  )
  bo<-compute.bagplot(x=x,y=y,factor=factor,na.rm=na.rm,
                      approx.limit=approx.limit,dkmethod=dkmethod,
                      precision=precision,verbose=verbose,debug.plots=debug.plots)
  if(create.plot){
    plot(bo,
         show.outlier=show.outlier,
         show.whiskers=show.whiskers,
         show.looppoints=show.looppoints,
         show.bagpoints=show.bagpoints,
         show.loophull=show.loophull,
         show.baghull=show.baghull,
         add=add,pch=pch,cex=cex,
         verbose=verbose,
         col.loophull=col.loophull,
         col.looppoints=col.looppoints,
         col.baghull=col.baghull,
         col.bagpoints=col.bagpoints,
         transparency=transparency, ...
    )
  }
  invisible(bo)
}


# New interface

# PlotBag <- function(x, y,
#                     outl = list(pch, cex, col, bg),
#                     looppoints = list(pch, cex, col, bg),
#                     bagpoints = list(pch, cex, col, bg),
#                     loophull = list(lwd, col, border),
#                     baghull = list(lwd, col, border),
#                     factor=3, # expanding factor for bag to get the loop
#                     na.rm=FALSE, # should 'NAs' values be removed or exchanged
#                     approx.limit=300, # limit
#                     add=FALSE, # if TRUE graphical elements are added to actual plot
#                     dkmethod=2, # in 1:2; there are two methods for approximating the bag
#                     precision=1, # controls precision of computation
#                     verbose=FALSE,debug.plots="no", # tools for debugging
#                     transparency=FALSE,
#                     ... # to define further parameters of plot
#                     )
# #
# outlty, outlwd, outpch, outcex, outcol, outbg
#
#





# faces<-function(xy,which.row,fill=FALSE,face.type=1,
#                 nrow.plot,ncol.plot,scale=TRUE,byrow=FALSE,main,
#                 labels,print.info = TRUE,na.rm = FALSE,
#                 ncolors=20,
#                 col.nose=rainbow(ncolors),                   # nose
#                 col.eyes=rainbow(ncolors,start=0.6,end=0.85),# eyes
#                 col.hair=terrain.colors(ncolors),            # hair
#                 col.face=heat.colors(ncolors),               # face
#                 col.lips=rainbow(ncolors,start=0.0,end=0.2), # lips
#                 col.ears=rainbow(ncolors,start=0.0,end=0.2), # ears
#
#                 plot.faces=TRUE){  # 070831 pwolf
#   if((demo<-missing(xy))){
#     xy<-rbind(
#               c(1,3,5),c(3,5,7),
#               c(1,5,3),c(3,7,5),
#               c(3,1,5),c(5,3,7),
#               c(3,5,1),c(5,7,3),
#               c(5,1,3),c(7,3,5),
#               c(5,3,1),c(7,5,3),
#               c(1,1,1),c(4,4,4),c(7,7,7)
#     )
#     labels<-apply(xy,1,function(x) paste(x,collapse="-"))
#   }
