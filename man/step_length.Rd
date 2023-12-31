% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/steps.R
\name{step_length}
\alias{step_length}
\title{Step Length}
\usage{
step_length(x)
}
\arguments{
\item{x}{matrix or data.frame with location coordinates.}
}
\value{
The step length of each relocation.
}
\description{
Step Length
}
\details{
Step length for time \eqn{t} is calculated as:
\eqn{\sqrt{(x_t - x_{t-1})^2 + (y_t - y_{t-1})^2}}
}
