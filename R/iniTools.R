#' Diff Ini Stepwise
#'
#' Show differences in the \verb{ini} file between stepwise model runs.
#'
#' @param stepdir directory containing model runs in subdirectories.
#' @param models optional vector of directory names to manually specify stepwise
#'        models to compare.
#' @param labels optional vector of short labels to describe the stepwise
#'        models.
#' @param \dots passed to \code{diffFlags}.
#'
#' @details
#' Generally, the user only needs to specify \code{stepdir}. If this top
#' directory contains stepwise model runs as subdirectories, then the default
#' values of \code{models} and \code{labels} will infer the correct paths and
#' model names.
#'
#' If the stepwise model runs are not organized in a straightforward way, the
#' \code{models} and \code{labels} arguments can be passed explicitly.
#'
#' @return
#' A data frame showing the differences in the \verb{ini} file between stepwise
#' model runs.
#'
#' @seealso
#' \code{\link{summary_ini}} summarizes the contents of an \verb{ini} file.
#'
#' \code{\link{read.MFCLIni}} reads an \verb{ini} file.
#'
#' @examples
#' \dontrun{
#' yft_dir <- "//penguin/assessments/yft/2020_review/analysis/stepwise"
#' yft_diffs <- diffIniStepwise(yft_dir)
#' yft_diffs
#'
#' # Unusual directory structure of BET 2020 stepwise models
#' bet_dir <- "//penguin/assessments/bet/2020/2020_stepwise"
#' bet_models <- file.path(dir(bet_dir, full.names=TRUE), "10N")
#' bet_labels <- dir(bet_dir)
#' bet_diffs <- diffIniStepwise(bet_dir, bet_models, bet_labels)
#' bet_diffs
#' }
#'
#' @export

diffIniStepwise <- function(stepdir, models=dir(stepdir, full.names=TRUE),
                            labels=basename(models), ...)
{
  # Read ini files
  inifiles <- unname(sapply(models, function(x)
    dir(x, pattern="\\.ini$", full.names=TRUE)[1]))  # first ini file, if many
  out <- lapply(inifiles, summary_ini)
  out <- do.call(rbind, out)

  # Add 'model' and 'same'
  out <- data.frame(model=labels, out)
  out$same <- ""
  for(i in 2:nrow(out))
    out$same[i] <- if(identical(out$md5sum[i], out$md5sum[i-1])) "" else "diff"

  out
}


#' Summarize Ini
#'
#' Summarize the contents of an \verb{ini} file.
#'
#' @param inifile \verb{ini} filename.
#' @param \dots passed to \code{read.MFCLIni}.
#'
#' @return
#' A data frame summarizing the contents of the \verb{ini} file.
#'
#' @seealso
#' \code{\link{diffIniStepwise}} shows differences in the \verb{ini} file
#' between stepwise model runs.
#'
#' \code{\link{read.MFCLIni}} reads an \verb{ini} file.
#'
#' @examples
#' inifile <- system.file("extdata/skj.ini", package="FLR4MFCL")
#' summary_ini(inifile)
#'
#' @importFrom tools md5sum
#'
#' @export

# Implementation note: analogous to setMethod("summary", "MFCLIni"), except this
# function operates directly on the file to get bytes, lines, and md5sum.

summary_ini <- function(inifile, ...)
{
  # Read file
  ini <- read.MFCLIni(inifile, ...)

  # Extract single quantities
  m1 <- m(ini)
  m2 <- age_pars(ini)[5, 1]
  l1 <- growth(ini)["Lmin", "est"]
  l2 <- growth(ini)["Lmax", "est"]
  k <- growth(ini)["k", "est"]
  sd1 <- sd_length_at_age(ini)[1]
  sd2 <- sd_length_dep(ini)[1]
  a <- lw_params(ini)[1]
  b <- lw_params(ini)[2]
  bytes <- file.info(inifile)$size
  lines <- length(readLines(inifile))
  md5sum <- substring(unname(md5sum(inifile)), 1, 7)

  data.frame(m1, m2, l1, l2, k, sd1, sd2, a, b, bytes, lines, md5sum)
}
