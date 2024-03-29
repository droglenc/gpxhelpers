% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/walks.R
\name{walkSummary}
\alias{walkSummary}
\title{Summarize tracks that form a walk.}
\usage{
walkSummary(walkdat, dropType = FALSE, dropOwner = FALSE)
}
\arguments{
\item{walkdat}{A data frame that contains tracks that form a contiguous \dQuote{walk}. Usually made with \code{\link{walkMaker}}.}

\item{dropType}{A logical for whether the \code{Type} variable should be dropped from the summary table.}

\item{dropOwner}{A logical for whether the \code{Owner} variable should be dropped from the summary table.}
}
\value{
Returns a \code{kable} table object.
}
\description{
Create a table that summarizes tracks in a data frame that form a contiguous \dQuote{walk}.
}
\details{
NONE YET
}
\examples{
## None yet.

}
\seealso{
\code{\link{walkMap}}
}
\author{
Derek H. Ogle
}
\keyword{manip}
