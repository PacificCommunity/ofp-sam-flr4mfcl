#' Correlation Filter
#'
#' Filter correlations to show the highest correlations.
#'
#' @param x correlations as a three-column data frame.
#' @param level filter level, a value from 0 to 1.
#' @param sort whether to sort the resulting data frame by absolute correlation.
#'
#' @return Data frame containing all correlations \code{|r| >= level}.
#'
#' @seealso
#' \code{\link{read.MFCLCor}} reads a correlation matrix from a file.
#'
#' \code{\link{mat2MFCLCor}} converts a correlation matrix to a data frame.
#'
#' \code{\link{corLabel}} labels correlations and produces a frequency table.
#'
#' @examples
#' data(cordf)
#' corFilter(cordf)
#' corFilter(cordf, level=0.99)
#' corFilter(cordf, level=0.99, sort=FALSE)
#'
#' @export

corFilter <- function(x, level=0.9, sort=TRUE)
{
  if(!is.data.frame(x) || ncol(x) != 3)
    stop("'x' must be a three-column data frame")

  ## Filter
  x <- x[abs(x$Corr) >= level,]

  ## Sort
  if(sort)
    x <- x[order(-abs(x$Corr)),]
  row.names(x) <- NULL

  x
}

#' Correlation Labels
#'
#' Label correlations and produce a frequency table.
#'
#' @param x correlations as a three-column data frame.
#'
#' @return Data frame with label counts.
#'
#' @seealso
#' \code{\link{read.MFCLCor}} reads a correlation matrix from a file.
#'
#' \code{\link{mat2MFCLCor}} converts a correlation matrix to a data frame.
#'
#' \code{\link{corFilter}} filters correlations to show the highest
#' correlations.
#'
#' @examples
#' data(cordf)
#' corLabel(cordf)
#'
#' @export

corLabel <- function(x)
{
  if(!is.data.frame(x) || ncol(x) != 3)
    stop("'x' must be a three-column data frame")

  ## Convert to labels
  A <- character(length(x$Corr))
  A[x >= -1.000 & x < -0.999] <- "---"  # [)
  A[x >= -0.999 & x < -0.990] <- "--"   # [)
  A[x >= -0.990 & x < -0.900] <- "-"    # [)
  A[x >= -0.900 & x <= 0.900] <- "."    # ()
  A[x >   0.900 & x <= 0.990] <- "+"    # (]
  A[x >   0.990 & x <= 0.999] <- "++"   # (]
  A[x >   0.999 & x <= 1.000] <- "+++"  # (]
  lab <- c("+++", "++", "+", ".", "-", "--", "---")
  rng <- c("(0.999, 1]", "(0.99, 0.999]", "(0.9, 0.99]", "[-0.9, 0.9]",
           "(-0.9, -0.99]", "(-0.99, -0.999]", "(-0.999, -1]")
  A <- ordered(A, levels=lab)

  ## Prepare frequency table
  out <- as.data.frame(unclass(table(A)))
  out <- data.frame(Range=rng, Label=lab, Freq=out[[1]])

  out
}

#' Correlation Matrix to Data Frame
#'
#' Convert a correlation matrix to a three-column data frame.
#'
#' @param x a correlation matrix (of class \code{matrix}).
#' @param names parameter names.
#'
#' @return
#' Data frame containing three columns: \code{Par1}, \code{Par2}, and
#' \code{Corr}.
#'
#' @seealso
#' \code{\link{read.MFCLCor}} reads a correlation matrix from a file.
#'
#' \code{\link{corFilter}} filters correlations to show the highest
#' correlations.
#'
#' \code{\link{corLabel}} labels correlations and produces a frequency table.
#'
#' @examples
#' data(cormat)
#' mat2MFCLCor(cormat)
#' mat2MFCLCor(cormat, names=c("alpha","beta","gamma","delta","epsilon"))
#'
#' @export

mat2MFCLCor <- function(x, names=rownames(x))
{
  if(!is.matrix(x))
    stop("'x' must be a matrix")
  if(nrow(x) != ncol(x))
    stop("'x' must be a square matrix, with nrow = ncol")

  ## Get names from rownames, if not supplied
  if(is.null(names))
  {
    names <- if(!is.null(rownames(x))) rownames(x) else as.character(1:ncol(x))
  }

  ## Convert to data frame
  x <- data.frame(Par1=names[col(x)[lower.tri(x)]],
                  Par2=names[row(x)[lower.tri(x)]],
                  Corr=x[lower.tri(x)])

  x
}

#' Read Correlation Matrix
#'
#' Read a correlation matrix from a file and store as a three-column data frame.
#'
#' @param x a filename containing a correlation matrix.
#' @param convert whether to convert to data frame, using \code{mat2MFCLCor}.
#' @param names parameter names, passed to \code{\link{mat2MFCLCor}}.
#'
#' @return
#' Data frame containing three columns: \code{Par1}, \code{Par2}, and
#' \code{Corr}.
#'
#' @seealso
#' \code{\link{mat2MFCLCor}} converts a correlation matrix to a data frame.
#'
#' \code{\link{corFilter}} filters correlations to show the highest
#' correlations.
#'
#' \code{\link{corLabel}} labels correlations and produces a frequency table.
#'
#' @examples
#' \dontrun{
#' read.MFCLCor("skj_pos_hess_cor")
#' }
#'
#' @export

read.MFCLCor <- function(x, convert=TRUE, names=NULL)
{
  ## Get correlations
  if(!is.character(x) || !file.exists(x))
    stop("'x' must be a filename")
  x <- read.table(x, header=TRUE, row.names=1, check.names=FALSE)
  x <- as.matrix(x)

  ## Convert to data frame
  if(convert)
    x <- mat2MFCLCor(x, names=names)

  x
}

#' @docType data
#'
#' @name cordf
#'
#' @title Correlation Data Frame
#'
#' @description Correlations as a three-column data frame for examples.
#'
#' @usage
#' cordf
#'
#' @format
#' Data frame containing three columns: \code{Par1}, \code{Par2}, and
#' \code{Corr}, showing the correlation between parameter pairs.
#'
#' @seealso
#' \code{\link{cormat}} is a correlation matrix.
#'
#' \code{\link{corFilter}} filters correlations to show the highest
#' correlations.
#'
#' \code{\link{corLabel}} labels correlations and produces a frequency table.
#'
#' @examples
#' data(cordf)
#' cordf
#' corFilter(cordf)
#' corLabel(cordf)

NA

#' @docType data
#'
#' @name cormat
#'
#' @title Correlation Matrix
#'
#' @description Correlation matrix for examples.
#'
#' @usage
#' cormat
#'
#' @format
#' Matrix with 5 rows and 5 columns, showing the correlation between parameter
#' pairs.
#'
#' @seealso
#' \code{\link{cordf}} is a three-column data frame of correlations.
#'
#' \code{\link{mat2MFCLCor}} converts a correlation matrix to a data frame.
#'
#' @examples
#' data(cormat)
#' cormat
#' mat2MFCLCor(cormat)

NA
