\name{needles}
\alias{needles}
\alias{needleScores}
\alias{defaultNeedleParams}
\title{
  Needleman-Wunsch simple global alignment algorithm
}
\description{
  The Needleman-Wunsch simple gap algorithm was one of the first methods
  introduced for global alignment of biological sequences. The same algorithm
  can be used to match cell line names or sample names from two related data
  sets; we provide examples in the documentation, using data that accompanies
  this package.
  
  While the \pkg{NameNeedle} package can be used for biological sequence
  alignment, the \pkg{Biostrings} package from Bioconductor contains much more
  sophisticated tools for that problem.
}
\usage{
needles(pattern, subject, params=defaultNeedleParams)
needleScores(pattern, subjects, params=defaultNeedleParams)
defaultNeedleParams
}
\arguments{
  \item{pattern}{character string to be matched}
  \item{subject}{character string to be matched against}
  \item{subjects}{character vector where matches are sought}
  \item{params}{
    list containing four required components. The default values are
    specified by the object \code{defaultNeedleParams}, which contains
    the following values: \preformatted{   
    $ MATCH   : num 1
    $ MISMATCH: num -1
    $ GAP     : num -1
    $ GAPCHAR : chr "*"
}}}
\details{
  The Needleman-Wunsch global alignment algorithm was one of the first
  algorithms used to align \acronym{DNA}, \acronym{RNA}, or protein sequences.
  The basic algorithm uses dynamic programming to find an optimal alignment
  between two sequences, with parameters that specify penalties for
  mismatches and gaps and a reward for exact matches. More elaborate
  algorithms (not implemented here) make use of matrices with different
  penalties depending on different kinds of mismatches. The version
  implemented here is based on the Perl implementation in the first
  section of Chapter 3 of the book \cite{BLAST}. 
}
\value{
  The \code{needles} function returns a list with five components:
    \item{score}{The raw alignment score.}
    \item{align1}{The final (optimal) alignment for the \code{pattern}.}
    \item{align2}{The final (optimal) alignment for the \code{subject}.}
    \item{sm}{The score matrix.}
    \item{dm}{The backtrace matrix.}

  The \code{needleScores} function returns a numeric vector the same
  length as the \code{subjects} argument, with each entry equal to the
  corresponding raw alignment score.
}
\references{
  Needleman SB, Wunsch CD.\cr
  \emph{A general method applicable to the search for similarities in the
  amino acid sequence of two proteins.}\cr
  J Mol Biol 1970, 48(3):443--453.

  Korf I, Yandell M, Bedell J.\cr
  \emph{BLAST.}\cr
  O'Reilly Media, 2003.

  Wang J, Byers LA, Yordy JS, Liu W, Shen L, Baggerly KA, Giri U, Myers
  JN, Ang KK, Story MD, Girard L, Minna JD, Heymach JV, Coombes KR.\cr
  \emph{Blasted cell line names.}\cr
  Cancer Inform. 2010; 9:251--5.
}
\author{
  Kevin R. Coombes \email{krc@silicovore.com},
  P. Roebuck \email{proebuck@mdanderson.org}
}
\seealso{
  The \pkg{Biostrings} package from Bioconductor used to contain a function
  called \code{needwunQS} that provided a simple gap implementation of
  Needleman-Wunsch, similar to the one presented here. That function has been
  deprecated in favor of a more elaborate interface called
  \code{\link[Biostrings]{pairwiseAlignment}} that incorporates a variety of
  other alignment methods in addition. While \code{pairwiseAlignment} is much
  more useful for applications to biological sequences, it is serious overkill
  for the application we have in mind for matching cell line or other sample
  names.
}
\examples{
data(cellLineNames)
myParam <- defaultNeedleParams
myParam$MATCH <- 2
myParam$MISMATCH <- -2
needles(sf2Names[2], illuNames[1], myParam)
scores <- needleScores(sf2Names[6], illuNames, myParam)
w <- which(scores == max(scores))
w
sf2Names[6]

needles(sf2Names[6], illuNames[w], myParam)
}
\keyword{character}

