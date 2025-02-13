
## Date functions  ====


HmsToMinute <- function(x){
  Hour(x)*60 + Minute(x) + Second(x)/60
}


HmsToSec <- function(x) {
  
  hms <- as.character(x)
  z <- sapply(data.frame(do.call(rbind, strsplit(hms, ":"))),
              function(x) { as.numeric(as.character(x)) })
  z[,1] * 3600 + z[,2] * 60 + z[,3]
}



SecToHms <- function(x, digits=NULL) {
  
  x <- as.numeric(x)
  
  h <- floor(x/3600)
  m <- floor((x-h*3600)/60)
  s <- floor(x-(m*60 + h*3600))
  b <- x-(s + m*60 + h*3600)
  
  if(is.null(digits)) digits <- ifelse(all(b < sqrt(.Machine$double.eps)),0, 2)
  if(digits==0) f <- "" else f <- gettextf(paste(".%0", digits, "d", sep=""), round(b*10^digits, 0))
  
  gettextf("%02d:%02d:%02d%s", h, m, s, f)
  
}



IsDate <- function(x, what=c('either','both','timeVaries')) {
  
  what <- match.arg(what)
  cl <- class(x) # was oldClass 22jun03
  if(!length(cl)) return(FALSE)
  
  dc <- c('POSIXt','POSIXct','dates','times','chron','Date')
  dtc <- c('POSIXt','POSIXct','chron')
  switch(what,
         either = any(cl %in% dc),
         both = any(cl %in% dtc),
         timeVaries = {
           # original: if('chron' %in% cl || !.R.) { ### chron or S+ timeDate
           if('chron' %in% cl) { # chron ok, but who cares about S+?
             y <- as.numeric(x)
             length(unique(round(y - floor(y), 13L))) > 1
           } else {
             length(unique(format(x, '%H%M%S'))) > 1
           }
         }
  )
  
}


IsWeekend <- function(x) {
  x <- as.POSIXlt(x)
  x$wday > 5L | x$wday < 1L
}


Year <-  function(x){
  UseMethod("Year")
}

Year.default <- function(x){ as.POSIXlt(x)$year + 1900L }


IsLeapYear <- function(x){
  
  if(!IsWhole(x))
    .Call("_DescTools_isLeapYearDate", x, PACKAGE="DescTools")
  else 
    .Call("_DescTools_isLeapYearInt", x, PACKAGE="DescTools")
  
}




Month <- function(x, fmt = c("m", "mm", "mmm"), 
                  lang = DescToolsOptions("lang"), stringsAsFactors = TRUE) {
  UseMethod("Month")
}


Month.ym <- function(x, fmt = c("m", "mm", "mmm"), 
                     lang = DescToolsOptions("lang"), stringsAsFactors = TRUE) {
  # unclass(x - Year(x) * 100)   
  x <- as.Date(x)
  NextMethod()
}

Month.default <- function(x, fmt = c("m", "mm", "mmm"), 
                          lang = DescToolsOptions("lang"), stringsAsFactors = TRUE) {
  
  res <- as.POSIXlt(x)$mon + 1L
  
  switch(match.arg(arg = fmt, choices = c("m", "mm", "mmm")),
         m = { res },
         mm = {
           # res <- as.integer(format(x, "%m"))
           switch(match.arg(arg = lang, choices = c("local", "engl")),
                  local = {
                    # months in current locale:  format(ISOdate(2000, 1:12, 1), "%b")
                    res <- ordered(res, levels=1L:12L, labels=format(ISOdate(2000L, 1L:12L, 1L), "%b"))
                  },
                  engl = {
                    res <- ordered(res, levels=1L:12L, labels=month.abb)
                  })
           if(!stringsAsFactors) res <- as.character(res)
         },
         mmm = {
           # res <- as.integer(format(x, "%m"))
           switch(match.arg(arg = lang, choices = c("local", "engl")),
                  local = {
                    # months in current locale:  format(ISOdate(2000, 1:12, 1), "%b")
                    res <- ordered(res, levels=1L:12L, labels=format(ISOdate(2000L, 1L:12L, 1L), "%B"))
                  },
                  engl = {
                    res <- ordered(res, levels=1L:12L, labels=month.name)
                  })
           if(!stringsAsFactors) res <- as.character(res)
         })
  return(res)
}




Week <- function(x, method = c("iso", "us")){
  
  # dd <- seq(as.Date("1970-01-01"), as.Date("2030-01-01"), by="days")
  # 
  # identical(Week(dd, "us"), as.integer(lubridate::week(dd)))
  # microbenchmark::microbenchmark(
  #   DescTools = Week(dd, "us"),
  #   lubridate = lubridate::week(dd)
  # )
  # 
  # identical(Week(dd, "iso"), as.integer(lubridate::isoweek(dd)))
  # microbenchmark::microbenchmark(
  #   DescTools = Week(dd, "iso"),
  #   lubridate = lubridate::isoweek(dd)
  # )
  # --> Superfast!!
  
  # cast x to date, such as being able to handle POSIX-Dates automatically
  x <- as.Date(x)
  
  method <- match.arg(method, c("iso", "us"))
  switch(method,
         "iso" = {
           wn <- .Call("_DescTools_isoWeek", x, PACKAGE="DescTools")
         },
         "us"={
           wn <- .Call("_DescTools_usWeek", x, PACKAGE="DescTools")
         }
  )
  return(wn)
  
}


# Day <- function(x){ as.integer(format(as.Date(x), "%d") ) }
Day <- function(x){ as.POSIXlt(x)$mday }


# Accessor for Day, as defined by library(lubridate)
"Day<-" <- function(x, value) { x <- x + (value - Day(x)) }


Weekday <- function (x, fmt = c("d", "dd", "ddd"), lang = DescToolsOptions("lang"), stringsAsFactors = TRUE) {
  
  # x <- as.Date(x)
  res <- as.POSIXlt(x)$wday
  res <- replace(res, res==0, 7)
  
  switch(match.arg(arg = fmt, choices = c("d", "dd", "ddd")),
         d = { res },
         dd = {
           # weekdays in current locale, Sunday : Saturday, format(ISOdate(2000, 1, 2:8), "%A")
           switch(match.arg(arg = lang, choices = c("local", "engl")),
                  local = {
                    # months in current locale:  format(ISOdate(2000, 1:12, 1), "%b")
                    res <- ordered(res, levels=1:7, labels=format(ISOdate(2000, 1, 3:9), "%a"))
                  },
                  engl = {
                    res <- ordered(res, levels=1:7, labels=day.abb)
                  })
           if(!stringsAsFactors) res <- as.character(res)
         },
         ddd = {
           # weekdays in current locale, Sunday : Saturday, format(ISOdate(2000, 1, 2:8), "%A")
           switch(match.arg(arg = lang, choices = c("local", "engl")),
                  local = {
                    # months in current locale:  format(ISOdate(2000, 1:12, 1), "%b")
                    res <- ordered(res, levels=1:7, labels=format(ISOdate(2000, 1, 3:9), "%A"))
                  },
                  engl = {
                    res <- ordered(res, levels=1:7, labels=day.name)
                  })
           if(!stringsAsFactors) res <- as.character(res)
         })
  return(res)
}



CountWorkDays <- function(from, to, 
                          holiday=NULL, nonworkdays=c("Sat","Sun")) {
  
  
  .workDays <- function(from, to, 
                        holiday=NULL, nonworkdays=c("Sat","Sun")) {
    d <- as.integer(to - from)
    w <- (d %/% 7)
    
    res <- w * (7-length(nonworkdays)) + 
      sum(Weekday(seq(from + w*7,  to, 1), fmt="dd", lang="engl") %nin% nonworkdays)
    
    if(!is.null(holiday)){
      # count holidays in period
      h <- holiday[holiday %[]% c(from, to)]
      res <- res - sum(Weekday(h, fmt="dd", lang="engl") %nin% nonworkdays)
    }
    
    return(res)
    
  }
  
  
  ll <- Recycle(from=from, to=to)  
  
  res <- integer(attr(ll, "maxdim"))
  for(i in 1:attr(ll, "maxdim"))
    res[i] <- .workDays(ll$from[i], ll$to[i], holiday=holiday, nonworkdays=nonworkdays) 
  
  return(res)
  
}



Quarter <- function (x) {
  # Berechnet das Quartal eines Datums
  # y <- as.numeric( format( x, "%Y") )
  # paste(y, "Q", (as.POSIXlt(x)$mon)%/%3 + 1, sep = "")
  # old definition is counterintuitive...
  return((as.POSIXlt(x)$mon) %/% 3L + 1L)
}



Today <- function() Sys.Date()

Now <- function() Sys.time()

Hour <- function(x) {
  # strptime(x, "%H")
  as.POSIXlt(x)$hour
}

Minute <- function(x) {
  #  strptime(x, "%M")
  as.POSIXlt(x)$min
}

Second <- function(x) {
  #  strptime(x, "%S")
  as.POSIXlt(x)$sec
}


Timezone <- function(x) {
  as.POSIXlt(x)$zone
}


YearMonth <- function(x){
  # returns the yearmonth representation of a date x
  # x <- as.POSIXlt(x)
  # return(as.ym((x$year + 1900L)*100L + x$mon + 1L))
  
  return(.Call("_DescTools_usYearmonth", x, PACKAGE="DescTools")) 
  
}


YearWeek <- function(x, method = c("iso", "us")){
  
  # cast x to date, such as being able to handle POSIX-Dates automatically
  x <- as.Date(x)
  
  method <- match.arg(method, c("iso", "us"))
  switch(method,
         "iso" = {
          res <- .Call("_DescTools_isoYearweek", x, PACKAGE="DescTools") 

         },
         "us"={
           res <- .Call("_DescTools_usYearweek", x, PACKAGE="DescTools") 
         }
  )
  
  return(res)
  
}



YearDay <- function(x) {
  # return(as.integer(format(as.Date(x), "%j")))
  
  # As ?POSIXlt reveals, a $yday suffix to a POSIXlt date (or even a vector of such) 
  # will convert to day of year. 
  # Beware that POSIX counts Jan 1 as day 0, so you might want to add 1 to the result.
  return(as.POSIXlt(x)$yday + 1L)
}



Year.ym  <- function(x){  unclass(round((x/100)))   }


# define a new class ym ("yearmonth")
as.ym <- function(x){
  
  # expects a YYYYMM format
  res <- structure(as.integer(x), class = c("ym", "num"))
  res[!((y <- round(x/100)) %[]% c(1000, 3000) & 
          (x - y * 100) %[]% c(1, 12))]   <- NA_integer_
  return(res)
}

print.ym <- function(x, ...) {
  # do not print the class attributes
  print(unclass(x), ...)
}


as.Date.ym <- function(x, d=1, ...){
  as.Date(gsub("([0-9]{4})([0-9]{2})([0-9]{2})", "\\1-\\2-\\3", 
               x*100 + d))
}




DiffDays360 <- function(start_d, end_d, method=c("eu","us")){
  
  # source: http://en.wikipedia.org/wiki/360-day_calendar
  start_d <- as.Date(start_d)
  end_d <- as.Date(end_d)
  
  d1 <- Day(start_d)
  m1 <- Month(start_d)
  y1 <- Year(start_d)
  d2 <- Day(end_d)
  m2 <- Month(end_d)
  y2 <- Year(end_d)
  
  method = match.arg(method)
  switch(method,
         "eu" = {
           if(Day(start_d)==31L) start_d <- start_d-1L
           if(Day(end_d)==31L) end_d <- end_d-1L
         }
         , "us" ={
           if( (Day(start_d+1L)==1L & Month(start_d+1L)==3L) &
               (Day(end_d+1L)==1L & Month(end_d+1L)==3L)) d2 <- 30L
           if( d1==31L ||
               (Day(start_d+1L)==1L & Month(start_d+1L)==3L)) {
             d1 <- 30L
             if(d2==31L) d2 <- 30L
           }
           
         }
  )
  
  return( (y2-y1)*360L + (m2-m1)*30L + d2-d1)
  
}


LastDayOfMonth <- function(x){
  z <- AddMonths(x, 1L)
  Day(z) <- 1L
  return(z - 1L)
}



YearDays <- function (x) {
  # return the number of days in the specific year of x
  x <- as.POSIXlt(x)
  x$mon[] <- x$mday[] <- x$sec[] <- x$min <- x$hour <- 0
  x$year <- x$year + 1
  return(as.POSIXlt(as.POSIXct(x))$yday + 1)
}


MonthDays <- function (x) {
  # return the number of days in the specific month of x
  x <- as.POSIXlt(x)
  x$mday[] <- x$sec[] <- x$min <- x$hour <- 0
  x$mon <- x$mon + 1
  return(as.POSIXlt(as.POSIXct(x))$mday)
}



AddMonths <- function (x, n, ...) {
  UseMethod("AddMonths")
}


AddMonths.default <- function (x, n, ...) {
  
  .addMonths <- function (x, n) {
    
    # ref: http://stackoverflow.com/questions/14169620/add-a-month-to-a-date
    # Author: Antonio
    
    # no ceiling
    res <- sapply(x, seq, by = paste(n, "months"), length = 2L)[2L,]
    # sapply kills the Date class, so recreate down the road
    
    # ceiling
    DescTools::Day(x) <- 1L
    res_c <- sapply(x, seq, by = paste(n + 1L, "months"), length = 2L)[2L,] - 1L
    
    # use ceiling in case of overlapping
    res <- pmin(res, res_c)
    
    return(res)
    
  }
  
  x <- as.Date(x, ...)
  
  res <- mapply(.addMonths, x, n)
  # mapply (as sapply above) kills the Date class, so recreate here
  # and return res in the same class as x
  class(res) <- "Date"
  
  return(res)
  
}



AddMonths.ym <- function (x, n, ...) {
  
  .addMonths <- function (x, n) {
    
    if (x %[]% c(100001L, 999912L)) {
      
      # Author: Roland Rapold
      # YYYYMM
      y <- x %/% 100L
      m <- x - y * 100L
      res <- (y - 10L + ((m + n + 120L - 1L) %/% 12L)) * 100L +
        ((m + n + 120L - 1L) %% 12L) + 1L
      
    } else if (x %[]% c(10000101L, 99991231L)) {
      
      # YYYYMMDD
      res <- DescTools::AddMonths(x = as.Date(as.character(x), "%Y%m%d"), n = n)
      res <- DescTools::Year(res)*10000L + DescTools::Month(res)*100L + Day(res)
    }
    
    return(res)
    
  }
  
  res <- mapply(.addMonths, x, n)
  
  return(res)
  
}



Zodiac <- function(x, lang = c("engl","deu"), stringsAsFactors = TRUE) {
  
  switch(match.arg(lang, choices=c("engl","deu"))
         , engl = {z <- c("Capricorn","Aquarius","Pisces","Aries","Taurus","Gemini","Cancer","Leo","Virgo","Libra","Scorpio","Sagittarius","Capricorn") }
         , deu =  {z <- c("Steinbock","Wassermann","Fische","Widder","Stier","Zwillinge","Krebs","Loewe","Jungfrau","Waage","Skorpion","Schuetze","Steinbock") }
  )
  
  # i <- cut(DescTools::Month(x)*100 + DescTools::Day(x),
  #          breaks=c(0,120,218,320,420,520,621,722,822,923,1023,1122,1221,1231))
  i <- cut(DescTools::Month(x) * 100 + DescTools::Day(x), 
           breaks = c(0, 120, 218, 320, 420, 520, 621, 
                      722, 823, 922, 1023, 1122, 1222, 1231), 
           right=FALSE, include.lowest = TRUE)
  
  if(stringsAsFactors){
    res <- i
    levels(res) <- z
  } else {
    res <- z[i]
  }
  return(res)
}


axTicks.POSIXct <- function (side, x, at, format, labels = TRUE, ...) {
  
  # This is completely original R-code with one exception:
  # Not an axis is drawn but z are returned.
  
  mat <- missing(at) || is.null(at)
  if (!mat)
    x <- as.POSIXct(at)
  else x <- as.POSIXct(x)
  range <- par("usr")[if (side %% 2L)
    1L:2L
    else 3L:4L]
  d <- range[2L] - range[1L]
  z <- c(range, x[is.finite(x)])
  attr(z, "tzone") <- attr(x, "tzone")
  if (d < 1.1 * 60) {
    sc <- 1
    if (missing(format))
      format <- "%S"
  }
  else if (d < 1.1 * 60 * 60) {
    sc <- 60
    if (missing(format))
      format <- "%M:%S"
  }
  else if (d < 1.1 * 60 * 60 * 24) {
    sc <- 60 * 60
    if (missing(format))
      format <- "%H:%M"
  }
  else if (d < 2 * 60 * 60 * 24) {
    sc <- 60 * 60
    if (missing(format))
      format <- "%a %H:%M"
  }
  else if (d < 7 * 60 * 60 * 24) {
    sc <- 60 * 60 * 24
    if (missing(format))
      format <- "%a"
  }
  else {
    sc <- 60 * 60 * 24
  }
  if (d < 60 * 60 * 24 * 50) {
    zz <- pretty(z/sc)
    z <- zz * sc
    z <- .POSIXct(z, attr(x, "tzone"))
    if (sc == 60 * 60 * 24)
      z <- as.POSIXct(round(z, "days"))
    if (missing(format))
      format <- "%b %d"
  }
  else if (d < 1.1 * 60 * 60 * 24 * 365) {
    z <- .POSIXct(z, attr(x, "tzone"))
    zz <- as.POSIXlt(z)
    zz$mday <- zz$wday <- zz$yday <- 1
    zz$isdst <- -1
    zz$hour <- zz$min <- zz$sec <- 0
    zz$mon <- pretty(zz$mon)
    m <- length(zz$mon)
    M <- 2 * m
    m <- rep.int(zz$year[1L], m)
    zz$year <- c(m, m + 1)
    zz <- lapply(zz, function(x) rep(x, length.out = M))
    zz <- .POSIXlt(zz, attr(x, "tzone"))
    z <- as.POSIXct(zz)
    if (missing(format))
      format <- "%b"
  }
  else {
    z <- .POSIXct(z, attr(x, "tzone"))
    zz <- as.POSIXlt(z)
    zz$mday <- zz$wday <- zz$yday <- 1
    zz$isdst <- -1
    zz$mon <- zz$hour <- zz$min <- zz$sec <- 0
    zz$year <- pretty(zz$year)
    M <- length(zz$year)
    zz <- lapply(zz, function(x) rep(x, length.out = M))
    z <- as.POSIXct(.POSIXlt(zz))
    if (missing(format))
      format <- "%Y"
  }
  if (!mat)
    z <- x[is.finite(x)]
  keep <- z >= range[1L] & z <= range[2L]
  z <- z[keep]
  if (!is.logical(labels))
    labels <- labels[keep]
  else if (identical(labels, TRUE))
    labels <- format(z, format = format)
  else if (identical(labels, FALSE))
    labels <- rep("", length(z))
  
  # axis(side, at = z, labels = labels, ...)
  # return(list(at=z, labels=labels))
  return(z)
}



axTicks.Date <- function(side = 1, x, ...) {
  ##  This functions is almost a copy of axis.Date
  x <- as.Date(x)
  range <- par("usr")[if (side%%2)
    1L:2L
    else 3:4L]
  range[1L] <- ceiling(range[1L])
  range[2L] <- floor(range[2L])
  d <- range[2L] - range[1L]
  z <- c(range, x[is.finite(x)])
  class(z) <- "Date"
  if (d < 7)
    format <- "%a"
  if (d < 100) {
    z <- structure(pretty(z), class = "Date")
    format <- "%b %d"
  }
  else if (d < 1.1 * 365) {
    zz <- as.POSIXlt(z)
    zz$mday <- 1
    zz$mon <- pretty(zz$mon)
    m <- length(zz$mon)
    m <- rep.int(zz$year[1L], m)
    zz$year <- c(m, m + 1)
    z <- as.Date(zz)
    format <- "%b"
  }
  else {
    zz <- as.POSIXlt(z)
    zz$mday <- 1
    zz$mon <- 0
    zz$year <- pretty(zz$year)
    z <- as.Date(zz)
    format <- "%Y"
  }
  keep <- z >= range[1L] & z <= range[2L]
  z <- z[keep]
  z <- sort(unique(z))
  class(z) <- "Date"
  z
}


