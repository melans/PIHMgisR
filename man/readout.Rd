% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readout.R
\name{readout}
\alias{readout}
\title{Read output file from PIHM}
\usage{
readout(keyword, path = get("outpath", envir = .pihm), ASCII = FALSE,
  file = file.path(path, paste0(get("PRJNAME", envir = .pihm), ".",
  keyword, ".dat")))
}
\arguments{
\item{keyword}{keyword of the output file. File name format is: projectname.keyword.dat}

\item{path}{path of the outputfile}

\item{ASCII}{If TRUE, convert the binary file to ASCII format (.csv)}

\item{file}{Full path of output file.}
}
\value{
A TimeSeries data. This list require support of xts packages.
}
\description{
Read output file from PIHM
}
\keyword{Could}
\keyword{and}
\keyword{be}
\keyword{for}
\keyword{format.}
\keyword{mesh}
\keyword{output.}
\keyword{read}
\keyword{reading}
\keyword{river}
\keyword{used}
