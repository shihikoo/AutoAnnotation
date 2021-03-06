% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AutoAnnotationFunctions.R
\name{GetData}
\alias{GetData}
\title{GetData}
\usage{
GetData(myData)
}
\arguments{
\item{myData}{Either a dataset, or a link to the dataset}
}
\value{
Data in the format of data frame or NULL for anything illegal
}
\description{
The function will first judge wether the input data is a dataset (matrix, data.frame, list, array) or a link.
For a dataset, it converts the input data to a data.frame.
For a link, it checks the type of the file and read accordingly. Acceptable extensions include 'txt', 'tsv', 'csv'
}
