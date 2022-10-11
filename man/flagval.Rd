
\name{flagval}
\alias{flagval}
\title{flagval}
\usage{
flagval(x, flagtype, flag, ...)
}
\arguments{
\item{x}{an object of class \linkS4class{MFCLPar}.}

\item{flagtype}{Numeric flag type identifier.}

\item{flag}{Numeric flag number.}

\item{\dots}{Currently ignored.}
}
\value{
data.frame of flag settings
}
\description{
Returns a data.frame of flag settings for the specified flag type and flag numbers.

The following numeric identifiers are used for the flag types: 

      1 - parest flags;

      2 - age flags;

     -1 - fish flags;

 -10001 - tag flags;

-100001 - region flags.

}

\examples{
\dontrun{
# parest flag 20
flagval(par, 1, 20)

# set parest flag 20 to a new value
flagval(par, 1, 20) <- 12

# fish flag 3 for fishery 7
flagval(par, -7, 3)

# fish flags 3 and 7 for fisheries 1:11
flagval(par, -c(1:11), c(3,7))

# changing the value of fish flags 3 and 7 for fisheries 1:11
flagval(par, -c(1:11), c(3,7)) <- c(36, 3)

# tag flags
flagval(par, -10001, 1:n_tag_groups(frq))
}
}
\seealso{
\code{\link{flagSummary}} \code{\link{flagDiff}}
}
