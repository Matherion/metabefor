\name{sysrev}
\alias{sysrev}
\alias{sysrev.read.bibtex}
\alias{sysrev.export}
\alias{listsToDataframe}
\alias{identifierColumns}
\alias{extractionVerification}
\title{
sysrev functions
}
\description{
These functions are meant for use in systematic reviews.
}
\usage{
sysrev.read.bibtex(filename, encoding="unknown")
sysrev.export(libraryObject, filename, drop=NULL, keep=NULL,
              sep="\t", row.names = FALSE, ...)
extractionVerification(extractionObject, variableOverview = FALSE)
listsToDataframe(listOfLists, identifierColumnPattern = "variable")
identifierColumns(sysRevDataFrame)
}
\arguments{
  \item{filename}{
  The path and filename to read from or write to
}
  \item{encoding}{
  When reading bibtex files, you might want to specify the encoding (passed on to 'readLines').
}
  \item{libraryObject}{
  An object, created with sysrev.read.bibtex, with references.
}
  \item{drop}{
  Character vector with names of fields to drop when exporting (rest is kept). Cannot be used at the same time as 'keep'.
}
  \item{keep}{
  Character vector with names of fields to keep when exporting (rest is dropped). Cannot be used at the same time as 'drop'.
}
  \item{sep}{
  Separator to use when writing exported file (defaults to tab).
}
  \item{row.names}{
  Whether to write rownames in the export (passed on to write.table).
}
  \item{extractionObject}{
  The sysRev extraction object, which can be used to extract data for systematic reviews or meta-analyses. It must have class 'sysRevExtractionObject'.
}
  \item{variableOverview}{
  Whether to show all extracted operationalisations (variables), associations, and univariate results.
}
  \item{listOfLists}{
  A list that contains one or more lists, that are to be converted into rows of a dataframe. Each list can have different elements.
}
  \item{identifierColumnPattern}{
  The pattern that identifies the columns in the dataframe producted by listOfLists that identify each row. For example, the columns (elements in the original object) that contain the names of the variables that are extracted.
}
  \item{sysRevDataFrame}{
  A dataframe to which to apply the pattern that identifies the identifying columns (see the identifierColumnPattern argument of listOfLists).
}
  \item{...}{
  Additional arguments passed on to write.table().
}
}
\details{

sysrev.read.bibtex and sysrev.export are for importing bibtex files, and after manipulation, exporting them to a 'spreadsheet format'.

extractionVerification takes a sysRevExtractionObject and verifies whether there were no inconsistencies in the data extraction. This object contains a number of lists, specifically one called 'variable', one called 'univariate' and one called 'association'. See the test 'sysRevExampleTemplate.r' for an example. When conducting a systematic review or meta-analysis, Data Extractors complete a series of text files like this one to extract the data. These are then compiled into the final dataset.

listsToDataframe is a helper function for extractionVerification, which converts a list of lists into a dataframe.

identifierColumns is a helper function for extractionVerification as well, and returns the columns in a dataframe that were specified as identifier columns when listsToDataframe was called (see the argument explanation for more information).

}
\value{

sysrev.read.bibtex returns a BibTeX Library object (a collection of references).

sysrev.export returns nothing (but writes a file).

extractionVerification returns an object with three dataframes (one with the information about the extracted variables; one with information about the extracted univariate results; and one with information about the extracted associations) and lists of extracted variables names.

listsToDataframe returns a dataframe, and identifierColumns returns a character vector.

}
\examples{

\dontrun{
bibTexObject <- sysrev.read.bibtex("BibTex library.bib");
sysrev.export(bibTexObject, "references in table.csv");
}

}
\keyword{ utilities }
