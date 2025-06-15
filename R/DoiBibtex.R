

# Leave this undocumented for some time...

# DOI 

# path <- "C:/Users/andri/OneDrive/Dokumente/PhD/Literatur/Paper 1/"
# ff <- list.files(path = path, full.names = TRUE)

DoiFromPdf <- function(path) {
  
  .DoiFromPdf <- function(path) {
    txt <- tryCatch(
      pdftools::pdf_text(path)[1],
      warning = function(w){return(NA_character_)},
      error = function(e){return(NA_character_)}
    )
    
    DoiFromTxt(txt, as_vector = TRUE)[1]
  }
  
  sapply(path, .DoiFromPdf)
  
}


DoiFromTxt <- function(string, as_vector = TRUE) {
  
  DOIREGEX <- "(i?)(10[.][0-9]{4,}(?:[.][0-9]+)*/(?:(?![%\"#? ])\\S)+)"
  
  # out <- stringi::stri_extract_all_regex(string, pattern = DOIREGEX)
  out <- regmatches(string, gregexpr(DOIREGEX, string, perl = TRUE))
  
  if (as_vector)
    out <- unlist(out)
  
  return(out)
}


DoiLookup <- function(doi){
  
  DOIREGEX <- "(i?)(10[.][0-9]{4,}(?:[.][0-9]+)*/(?:(?![%\"#? ])\\S)+)"
  
  .is_doi <- function(string) grepl(DOIREGEX, string, perl=TRUE)
  
  # .doi_lookup_crossref <- function(doi) {
  #   if (!.is_doi(doi)) return(NA_character_)
  #   
  #   tryCatch(
  #     rcrossref::cr_cn(doi = doi),
  #     warning = function(w){message("Crossref: ", w); return(NULL)},
  #     error = function(e){message("Crossref: ", e); return(NULL)}
  #   )
  # }
  
  
  .doi_lookup_direct <- function(doi) {
    
    if (!.is_doi(doi)) return(NA_character_)
    bib <- httr::content(
      httr::GET(paste0("http://dx.doi.org/", doi),
                httr::add_headers(Accept = "application/x-bibtex; encoding=UTF-8"),
                httr::config(followlocation = TRUE)
      ),
      as = "text", encoding = "UTF-8")
    if (grepl("DOI not found", bib)) return(NA_character_)
    return(bib)
  }
  
  if (!.is_doi(doi)) return(NA_character_)
  
  # # First, try crossref
  # ref <- .doi_lookup_crossref(doi)
  # 
  # # If no result found, try dx.doi.org directly
  # if(is.null(ref)){
  #   ref <- tryCatch({
  #     message("No result from crossref, querying dx.doi.org directly")
  #     ref <- .doi_lookup_direct(doi)
  #   },
  #   warning = function(w){message("dx.doi.org: ", w); return(NULL)},
  #   error = function(e){message("dx.doi.org: ", e); return(NULL)}
  #   )
  # }
  
  ref <- tryCatch({
    ref <- .doi_lookup_direct(doi)
  },
  warning = function(w){message("dx.doi.org: ", w); return(NULL)},
  error = function(e){message("dx.doi.org: ", e); return(NULL)}
  )
  
  return(ref)
  
}

FormatBibtex <- function(x, fmt="eje") {
  
  if(fmt != "eje") return( NA_character_ )
  
  if(is.na(x) || x == "") return( NA )
  
  entry_type <- ifelse(grepl("^@book", x), "book", "article")
  
  x <- gsub("[\n\r]", " ", x)
  x <- gsub("^@\\w+\\{[^,]+,", "", x)
  x <- gsub("}$", "", x)
  
  keyvals <- unlist(strsplit(x, ",(?=\\s*\\w+\\s*=)", perl = TRUE))
  fields <- list()
  
  for (kv in keyvals) {
    parts <- unlist(strsplit(kv, "=", fixed = TRUE))
    if (length(parts) == 2) {
      key <- trimws(parts[1])
      val <- trimws(parts[2])
      val <- gsub('^[\'"{]*(.*?)[\'"}]*$', "\\1", val)
      fields[[key]] <- val
    }
  }
  
  # Titel bereinigen
  title <- gsub("\\s+", " ", fields[["title"]])
  title <- trimws(title)
  
  # DOI-Link
  doi_url <- if (!is.null(fields[["DOI"]])) 
               paste0(" https://doi.org/", fields[["DOI"]]) 
             else 
               ""
  
  # Autoren
  authors_raw <- fields[["author"]]
  authors_split <- unlist(strsplit(authors_raw, " and "))
  authors <- sapply(authors_split, function(x) {
    x <- trimws(x)
    if (entry_type == "book") {
      return(x)  # books: no initials
    } else if (grepl(",", x)) {
      parts <- unlist(strsplit(x, ",\\s*"))
      lname <- parts[1]
      inits <- gsub("\\s+", "", gsub("([A-Za-z])[a-z]*", "\\1", parts[2]))
      paste0(lname, " ", inits)
    } else {
      parts <- unlist(strsplit(x, "\\s+"))
      lname <- tail(parts, 1)
      inits <- gsub("\\s+", "", gsub("([A-Za-z])[a-z]*", "\\1", head(parts, -1)))
      paste0(lname, " ", inits)
    }
  })
  author_str <- paste(authors, collapse = ", ")
  
  if (entry_type == "book") {
    address <- if (!is.null(fields[["address"]])) fields[["address"]] else ""
    publisher <- fields[["publisher"]]
    citation <- sprintf("%s. %s. %s: %s; %s.%s",
                        author_str,
                        title,
                        address,
                        publisher,
                        fields[["year"]],
                        doi_url)
  } else {
    journal <-  gsub("·", ".", fields[["journal"]])
    month <- if (!is.null(fields[["month"]])) 
                tools::toTitleCase(fields[["month"]]) else ""
    pages <- if (!is.null(fields[["pages"]])) 
                gsub("–", "-", fields[["pages"]]) else "eLocation"
    volume <- if (!is.null(fields[["volume"]])) 
                fields[["volume"]] else ""
    number <- if (!is.null(fields[["number"]])) 
                paste0("(", fields[["number"]], ")") else ""
    
    citation <- sprintf("%s. %s. *%s*. %s%s;%s%s:%s.%s",
                        author_str,
                        title,
                        journal,
                        fields[["year"]],
                        ifelse(month != "", paste0(" ", month), ""),
                        volume,
                        number,
                        pages,
                        doi_url)
  }
  
  return(citation)
}

# 
# FormatBibtex(DoiLookup(DoiFromPdf(ff[2])))
# 
# dois <- DoiFromPdf(ff)
# bibs <- sapply(na.omit(dois)[1:4], DoiLookup)

# undebug(DoiLookup)

