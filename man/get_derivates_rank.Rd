% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iota_version_2.R
\name{get_derivates_rank}
\alias{get_derivates_rank}
\title{Check for sufficient condition}
\usage{
get_derivates_rank(pattern_shape, aem, categorial_sizes)
}
\arguments{
\item{pattern_shape}{\code{Matrix} generated by the function
\code{\link{get_patterns}} containing the observed patterns in the data.}

\item{aem}{\code{Matrix} in the aem form.}

\item{categorial_sizes}{\code{Vector} containing the sizes of the categories.
This is the amount of a category on all cases.}
}
\value{
Function returns \code{TRUE} if the rank equals the number of
independently estimated parameters (degrees of freedom). \code{FALSE} in all
other cases.
}
\description{
Auxiliary function written in \code{R} for checking if the rank of the
derivatives equals the number of independently estimated parameters. This
condition must be fulfilled if the model should be identified.
}
