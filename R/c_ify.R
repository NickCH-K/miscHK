#' Print a vector formatted as valid R code for a vector
#'
#' This function takes a string, such as \code{"a b c"}, and returns code that will produce that vector in R, such as \code{c("a","b","c")}.
#'
#' This is handy when copy/pasting vectors from R console output or other languages (like Stata) into your code.
#'
#' @param s A string
#' @param from The formatting of the origin string. This can be \code{'stata'} or \code{'r'} (\code{a b c}), \code{'r_char'} (\code{"a" "b" "c"}), \code{'python'} (\code{[a, b, c]}), or \code{'py_char'} (\code{['a', 'b', 'c']}). Any format can be supported using custom \code{from_} options below.
#' @param from_start Text at the beginning of the string to remove before separating the elements. Or just give a number to remove that many characters.
#' @param from_sep Character(s) separating each element.
#' @param from_end Text at the end of the string to remove before separating the elements.  Or just give a number to remove that many characters.
#' @param string Set to \code{TRUE} to set the \code{to_} defaults to produce an R character vector, or \code{FALSE} to set the \code{to_} defaults to produce an R numeric vector.
#' @param to_start Text at the beginning of the output string.
#' @param to_sep Character(s) separating each element in the output.
#' @param to_end Text at the end of the output string.
#' @param trim Merge all multiple-whitespaces into single whitespaces before separating the elements, handy if \code{from_sep = ' '}.
#' @param silent Suppress the message to console and just return the result as a string.
#' @examples
#'
#' c_ify('1 2 3')
#' c_ify('[1, 2, 3]', from = 'python')
#' c_ify('var1 var2 var3', from = 'stata')
#' c_ify('%%%a_b_c$$$', from_start = '%%%', from_sep = '_', from_end = '$$$', string = TRUE)
#'
#' @export

c_ify <- function(s, from = c('r', 'r_char', 'stata', 'python', 'py_char'),
                  from_start = NULL, from_sep = NULL, from_end = NULL,
                  string = from %in% c('r_char', 'py_char', 'stata'),
                  to_start = NULL, to_sep = NULL, to_end = NULL,
                  trim = TRUE, silent = FALSE) {

  from <- from[1]

  # Fill in "from" defaults
  if (is.null(from_start)) {
    if (from %in% c('stata', 'r')) {
      from_start = ''
    } else if (from == 'r_char') {
      from_start = '"'
    } else if (from == 'python') {
      from_start = '['
    } else if (from == 'py_char') {
      from_start = "['"
    }
  }
  if (is.null(from_sep)) {
    if (from %in% c('stata', 'r')) {
      from_sep = ' '
    } else if (from == 'r_char') {
      from_sep = '" "'
    } else if (from == 'python') {
      from_sep = ','
    } else if (from == 'py_char') {
      from_sep = "', '"
    }
  }
  if (is.null(from_end)) {
    if (from %in% c('stata', 'r')) {
      from_end = ''
    } else if (from == 'r_char') {
      from_end = '"'
    } else if (from == 'python') {
      from_end = ']'
    } else if (from == 'py_char') {
      from_end = "']"
    }
  }

  # and "to" defaults
  if (is.null(to_start)) {
    if (string) {
      to_start = 'c("'
    } else {
      to_start = 'c('
    }
  }
  if (is.null(to_sep)) {
    if(string) {
      to_sep = '", "'
    } else {
      to_sep = ','
    }
  }
  if (is.null(to_end)) {
    if(string) {
      to_end = '")'
    } else {
      to_end = ')'
    }
  }

  # remove leading/following text
  if (is.character(from_start)) {
    from_start <- nchar(from_start)
  }
  if (is.character(from_end)) {
    from_end <- nchar(from_end)
  }
  s <- stringr::str_sub(s, from_start + 1, -(from_end + 1))

  # leading/following whitespace
  s <- trimws(s)

  # trim
  if (trim) {
    s <- stringr::str_replace_all(s, "\\s+", " ")
  }

  # break up the vector
  s <- s %>%
    stringr::str_split(from_sep) %>%
    `[[`(1) %>%
    trimws() %>%
    # then bind together
    paste(collapse = to_sep) %>%
    # and produce a full string
    paste0(to_start, ., to_end)

  # print to console
  if (!silent) {
    message(s)
  }

  return(s)
}
