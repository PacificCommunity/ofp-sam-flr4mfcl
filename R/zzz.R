# zzz.R
# FLR4MFCL/R/zzz.R


.onAttach <- function(lib,pkg) {
  pkgdesc <- packageDescription("FLR4MFCL")
  builddate <- gsub(';.*$', '', pkgdesc$Packaged)
  if(length(builddate) == 0)
    builddate <- date()
  packageStartupMessage(paste("FLR4MFCL (Version ", pkgdesc$Version, ", packaged: ", builddate, ")", sep = ""))
}

# ac
ac <- function(x, ...)
  as.character(x, ...)

# an
an <- function(x, ...)
  as.numeric(x, ...)
