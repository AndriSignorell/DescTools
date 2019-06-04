.onLoad <- function(libname, pkgname) {

  # cat("message from .onLoad via cat\n")

  # this does not work:!!!
  #
  # options(lastWord = NULL)
  # options(lastPP = NULL)

  # fmt <- getOption("fmt", default = list(
  #     per = structure(list(digits=1, fmt="%"), name="per", label="Percentage number format", class="fmt")
  #   , abs = structure(list(digits=0, big.mark="'"), name="abs", label="Number format for counts", class="fmt")
  #   ,	num = structure(list(digits=3, big.mark="'"), name="num", label="Number format for floating points", class="fmt")
  # ) )
  #
  # # assign("fmt", fmt, dtls.glob)
  #
  # # print(fmt)
  #
  # assign("fmt", fmt, envir = .GlobalEnv)
  # assign("fmt", fmt, envir = as.environment("Package:DescTools"))

  # does not work
  # .env <- new.env()
  # attach(.env)
  # assign(x = "fmt", value = fmt, envir = .env)

  # packageStartupMessage("Hello all!", " ", domain = "DescTools", appendLF = FALSE)
  invisible()
}
