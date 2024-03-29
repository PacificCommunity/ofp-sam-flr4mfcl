% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.MFCLFrq.r
\name{read.MFCLFrq2}
\alias{read.MFCLFrq2}
\title{MFCL frq2 file reader - Mufdagr friendly}
\usage{
read.MFCLFrq2(frqfile)
}
\arguments{
\item{frqfile}{A character string giving the name and path of the frq file to be read}
}
\value{
An object of class MFCLFrq
}
\description{
Reads the entire contents of the frq file
}
\examples{
\dontrun{
read.MFCLFrq2(paste(system.file('data', package='FLR4MFCL'), 'skj.frq', sep='/'))
}

}
