
#' Return Downloaded File in XL- or CSV Format
#' 
#' The function searches for a file in the downloads folder and returns the
#' file, provided it is in a readable Excel- or data format.
#' 
#' 
#' @param fname the filename, if no extension specified the first file found
#' will be used.
#' @param \dots are passed on to the open functions
#' \code{\link[readxl]{readxl}}() or \code{\link[readr]{read_table}()}.
#' @return the file
#' @author Andri Signorell <andri@@signorell.net>
#' @examples
#' \dontrun{
#' Downloads("yourfilename")
#' Downloads("yourfilename.xls")
#' Downloads("yourfilename.txt", sep=",", header=TRUE)
#' }
#' 


Downloads <- function(fname, ...){
  
  downloads_path <- fs::path_home("Downloads")

  if((ext <- tools::file_ext(fname)) == "") {
    
    # no extension provided, try to find one ...
    found <- grep(fname, list.files(downloads_path), 
                  fixed = TRUE, value=TRUE)
    
    if(length(found)>0){
      # return first found element
      fname <- found[1]
      ext <- tools::file_ext(found)
      
      # message("No extension specified, found file: ", fname)
      cli::cli_alert_info(gettextf("No extension specified, found file: %s\n\n", fname))
      
    } else {
      # file not found
      stop(gettextf("File %s does not exist!", fname))
    }
  }
  
  if(file.exists(gettextf("%s/%s", downloads_path, fname))) {
  
      if(grepl("xls", tools::file_ext(fname))) {
        res <- as.data.frame(readxl::read_excel(paste(downloads_path, fname, sep="/"), ...))
        
      } else {
        res <- readr::read_table(file = gettextf("%s/%s", downloads_path, fname), ...)
      }
    
  } else {
    stop(gettextf("File %s/%s does not exist!", downloads_path, fname))
  }
    
  return(res)
  
}  



# Alternatives:
#   
# library(rappdirs)
# downloads_path <- file.path(rappdirs::user_data_dir(), "Downloads")
# 
# downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads")
# 
# DownloadsFolder <- function() {
#   
#   if (Sys.info()['sysname'] == "Windows"){
#     
#     # folder <- dirname("~")
#     folder <- readRegistry("SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Explorer\\Shell Folders\\", 
#                            "HCU")$"{374DE290-123F-4565-9164-39C4925E467B}"  
#   } else {
#     
#     folder <- path.expand("~")
#     folder <- file.path(folder, "Downloads")
#     folder <- paste0(folder, .Platform$file.sep)
#   }
#   
#   return(folder)
#   
# }

















