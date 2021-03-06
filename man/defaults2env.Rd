% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/defaults2env.R
\name{defaults2env}
\alias{defaults2env}
\title{Bring the default values of a function into the global environment}
\usage{
defaults2env(fun, ..., clean.env = FALSE)
}
\arguments{
\item{fun}{A function.}

\item{...}{Arguments to call the function with, for example to debug under certain settings, or to set options with no defaults.}

\item{clean.env}{Also runs \code{rm(list = ls())} on the environment you've called this function from, so you can debug with a clean environment.}
}
\description{
This function takes a function and populates the global environment with objects as though you'd just called that function.
}
\details{
This is for the purpose of debugging a function you've written, especially one with many options, as it allows you to step through the code of that function step-by-step as though it's just been called.

Note two things: First, this will only populate options included in the explicit function call. \code{...} defaults and options will not be filled in. Second, this function won't work if you have a curly brace as part of the function call.
}
\examples{

if(interactive()) {
# Note that some of the options here are actually ... and will not be brought to environment
defaults2env(help, verbose = FALSE, clean.env = TRUE)
}

}
