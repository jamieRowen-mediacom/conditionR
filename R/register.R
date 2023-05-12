.shiny_handlers = new.env()
.shiny_error_handler = new.env()
.shiny_restart_handler = new.env()

#' Register a condition hander into the set of defaults
#' 
#' @param f a function of class conditionr_handler
#' @export
register_shiny_condition_handler = function(f) {
  stopifnot(inherits(f, "conditionr_handler"))
  for (attr in attr(f, "conditionrs")) {
    .shiny_handlers[[attr]] = f
  }
}

register_shiny_error_handler = function(f) {
  stopifnot(inherits(f, "conditionr_handler"))
  stopifnot("error" %in% attr(f, "conditionrs"))
  for (attr in attr(f, "conditionrs")) {
    .shiny_error_handler[[attr]] = f
  }
}