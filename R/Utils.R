



# unexported utility functions


.NotThere <- function(object, ...){
  warning(gettextf('Sorry, no method implemented for class "%s",', paste(class(object), collapse=", ")))
  NA_real_
}




.BootCI <- function(DATA, FUN, conf.level = 0.95, sides = c("two.sided", "left", "right"), ...){
  
  sides <- match.arg(sides, choices = c("two.sided", "left",
                                        "right"), several.ok = FALSE)
  if (sides != "two.sided")
    conf.level <- 1 - 2 * (1 - conf.level)
  
  btype <- match.arg(DescTools::InDots(..., arg = "type", default = "perc"),
                     choices = c("norm","basic", "stud", "perc", "bca"))
  
  boot.fun <- boot::boot( DATA,
                          FUN,
                          R = DescTools::InDots(..., arg = "R", default = 999),
                          parallel = DescTools::InDots(..., arg = "parallel", default = "no")
  )
  
  ci <- boot::boot.ci(boot.fun, conf = conf.level, type = btype)
  
  if (btype == "norm") {
    res <- c(est = boot.fun$t0[1], lwr.ci = ci[[4]][2],
             upr.ci = ci[[4]][3])
  } else {
    res <- c(est = boot.fun$t0[1], lwr.ci = ci[[4]][4],
             upr.ci = ci[[4]][5])
  }
  
  if (sides == "left")
    res[3] <- Inf
  else if (sides == "right")
    res[2] <- -Inf
  
  return(res)
  
}




RSessionAlive <- function() {
  
  # returns the duration of the running R session in hours

  shell(gettextf("powershell New-TimeSpan -Start (get-process RStudio).StartTime > %s",
                 tmp <- tempfile(tmpdir=Sys.getenv("USERPROFILE"))))
  res <- readLines(tmp)
  file.remove(tmp)
  hh <- StrVal(res[10], as.numeric = TRUE)

  Now() - as.difftime(hh, units="hours")
  
}


RTempdirAlive <- function() {
  
  # returns the age of R temporary directory in hours

  tempdirs = list.dirs(Sys.getenv("TEMP"), recursive=FALSE)
  rtempdir = tempdirs[grepl("Rtmp", tempdirs)]

  if(length(rtempdir) == 0L) return("Rtmp does not exist!")

  file.info(rtempdir)$ct
  
}


.getDescToolsPath <- function() system.file("extdata", package="DescTools")



