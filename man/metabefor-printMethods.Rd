\name{metabeforPrintMethods}
\alias{metabeforPrintMethods}
\alias{print.listsToDataframe}
\alias{print.extractionVerification}
\title{
metabefor print methods
}
\description{
These methods print the userfriendlyscience objects.
}
\usage{
\method{print}{listsToDataframe}(x, ...)
\method{print}{extractionVerification}(x, variableOverview = x$variableOverview, ...)
}
\arguments{
  \item{x}{
The object to print
}
  \item{variableOverview}{
  Whether to show all extracted operationalisations (variables), associations, and univariate results.
}
  \item{...}{
Addition arguments that are passed on to the print functions when it's called.
}
}
\value{
These printing methods return nothing, but print stuff.
}
\examples{

#print.sdConfInt(sdConfInt(sd=4, n=20));

}
\keyword{ utilities }