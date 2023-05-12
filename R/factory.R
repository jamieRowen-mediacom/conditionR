#' Create a function to be used to create new condition signals
#'
#' Creates a function which can be used to create new condition signals of
#' a given type.
#'
#' @param type character(n) condition signal type of the resultant function
#' @include utils.R
#' @export
condition_factory = function(type) {
  force(type)
  f = function(message, call = sys.call(-1), ...) {
    x = structure(
      class = c(type, "condition"),
      list(
        message = message, call = call, ...
      )
    )
    signalCondition(x)
  }
  with_class(f, "conditionr")
}

#' As Handler
#'
#' Cast a function to be a condtionr handler object
#'
#' @param f a function to be a condtionr handler object
#' @oaram conditions character(n) the conditions that this handler should
#'  act upon
#' @export
as_handler = function(f, conditions) {
  attr(f, "conditionrs") = conditions
  with_class(f, "conditionr_handler")
}

#' @export
example_condition = condition_factory("conditionr_example")