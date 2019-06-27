#' Conditional mutate(), mutating only selected observations (rows)
#' 
#' This function is a good answer from Stack overflow. It is a 
#' conditional mutate() that mutates column x only at rows satisfying 
#' condition y == 'z'
#' 
#' @param .data (data.frame) data frame or tibble that is to be mutated
#' @param condition (logical) predicates defined in terms of the variables in .data, the same way as filter() is used.
#' @param envir the environment. By default the environment of the function
#' @param ... other arguments passed to mutate()
#' 
#' @importFrom dplyr mutate
#' @export
# ------------------------------------------------------------------------------
mutate_cond <- function(.data, condition, envir = parent.frame(), ...) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}
