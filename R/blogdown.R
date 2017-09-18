#' Sets the knitr options that I like
#'
#' Sets chunk options most often used. Does not work, yet.
#' @export

blg_opts_knitr_chunk <- function() {

  knitr::opts_chunk$set(echo =  FALSE)
  knitr::opts_chunk$set(fig.height = 7)
  knitr::opts_chunk$set(message    = FALSE)
  knitr::opts_chunk$set(warning    = FALSE)

  }


#' Sets the knitr options that I like
#'
#' Sets hook options most often used. Does not work, yet.
#' @export

blg_opts_knitr_hooks <- function () {

  # ---- knitr_Options_Setup
  org_inline <- knitr::knit_hooks$get("inline")
  org_plot   <- knitr::knit_hooks$get("plot")

  # ---- Hooks_Setup
  knitr::knit_hooks$set(inline = function(x) {
    if (is.numeric(x) | is.integer(x)) return(formatC(x, format = "d", big.mark = ",")) else
      if (is.character(x)) return(stringi::stri_trans_totitle(x)) else
        return(x)
  })

  mod_inline <- knitr::knit_hooks$get("inline")

  }

#' Sets the R options that I like
#'
#' Sets R options most often used. Does not work, yet.
#' @export

blg_opts_R <- function() {

  # ---- R_Options_Setup
  org_scipen <- getOption("scipen")
  options(scipen = 100)

  org_digits <- getOption("digits")
  options(digits = 2)

  }