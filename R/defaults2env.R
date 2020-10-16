#' Bring the default values of a function into the global environment
#'
#' This function takes a function and populates the global environment with objects as though you'd just called that function.
#'
#' This is for the purpose of debugging a function you've written, especially one with many options, as it allows you to step through the code of that function step-by-step as though it's just been called.
#'
#' Note two things: First, this will only populate options included in the explicit function call. \code{...} defaults and options will not be filled in. Second, this function won't work if you have a curly brace as part of the function call.
#'
#' @param fun A function.
#' @param ... Arguments to call the function with, for example to debug under certain settings, or to set options with no defaults.
#' @param clean.env Also runs \code{rm(list = ls())} on the environment you've called this function from, so you can debug with a clean environment.
#' @examples
#'
#' # Note that some of the options here are actually ... and will not be brought to environment
#' defaults2env(help, verbose = FALSE, clean.env = TRUE)
#'
#' @export

defaults2env <- function(fun, ..., clean.env = FALSE) {

  if (clean.env) {
    rm(list = ls(envir = parent.env(environment())), envir = parent.env(environment()))
  }

  # Get the function call information
  funstr <- fun %>%
    utils::capture.output() %>%
    paste(collapse = ' ') %>%
    stringr::str_split('\\{') %>%
    `[[`(1) %>%
    `[`(1)

  # Create a new function with the same call that just returns the information as a list
  f <- funstr %>%
    paste('{ return(as.list(environment())) }') %>%
    parse(text = .) %>%
    eval()

  funlist <- f(...)

  # And finally send that list to the parnet environment
  list2env(funlist, envir = parent.env(environment()))

  return(NULL)
}

