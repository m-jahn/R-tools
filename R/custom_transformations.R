#' Conditional mutate(), mutating only selected observations (rows)
#' 
#' This function is a good answer from Stackoverflow. It is a 
#' conditional \code{mutate()} that mutates column x only at rows satisfying 
#' condition y == 'z'
#' 
#' @param .data (data.frame) data frame or tibble that is to be mutated
#' @param condition (logical) predicates defined in terms of the variables in 
#'   .data, the same way as \code{filter()} is used.
#' @param envir the environment. By default the environment of the function
#' @param ... other arguments passed to \code{mutate()}
#' 
#' @importFrom dplyr mutate
#' 
#' @examples
#' df <- data.frame(
#'   A = letters[1:10], 
#'   B = sample(1:3, 10, replace = TRUE),
#'   stringsAsFactors = FALSE
#' )
#' 
#' mutate_cond(df, B == 3, A = toupper(A))
#' 
#' @export
# ------------------------------------------------------------------------------
mutate_cond <- function(.data, condition, envir = parent.frame(), ...) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}
