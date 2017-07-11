\name{cellLineNames-data}
\alias{cellLineNames-data}
\alias{cellLineNames}
\alias{sf2Names}
\alias{rppaNames}
\alias{illuNames}
\alias{illuType}
\docType{data}
\title{Cell Line Names}
\description{
  This dataset contains vectors of cell line names that are used to
  demonstrate how to use the \pkg{NameNeedle} package.
}
\usage{
data(cellLineNames)
}
\format{
  This dataset contains four objects: three character vectors (\code{sf2Names},
  \code{rppaNames}, and \code{illuNames}) and one factor (\code{illuType}).
}
\details{
  The three character vectors, \code{sf2Names}, \code{rppaNames}, and
  \code{illuNames} contain the names of cell lines used in three different
  but related experiments.  The factor, \code{illuType}, indicates whether
  the cell lines named in the \code{illuNames} vector were derived from
  lung cancer (with the value "\code{Lung}") or from head and neck cancer
  ("\code{HNSCC}"). 
}
\examples{
data(cellLineNames)
head(rppaNames)
head(sf2Names)
head(illuNames)
summary(illuType)
}
\keyword{datasets}

