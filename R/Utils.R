


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

