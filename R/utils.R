#' With class
#'
#' A convenience method for attaching new classes to a given object
#' @keywords internal
#' @param x an object
#' @param cls character(n) classes to attach to x
#' @return x with additional classes
with_class = function(x, cls) {
  class(x) = unique(c(cls, class(x)))
  x
}

#' expression to quosure
#' 
#' This is almost verbatim a copied implementation of this internal shiny function.
#' Prevents us from relying on an unexported function from that package.
#' 
#' @keywords internal
exprToQuo = function (expr, env = parent.frame(), quoted = FALSE){
  if (!quoted) {
      expr <- eval(substitute(substitute(expr)), parent.frame())
  }
  q <- if (rlang::is_quosure(expr)) {
      expr
  } else if (is.language(expr) || rlang::is_atomic(expr) || is.null(expr)) {
    rlang::new_quosure(expr, env = env)
  } else {
      stop("Don't know how to convert '", class(expr)[1], "' to a function; a quosure or quoted expression was expected")
  }
  q
}

#' quosure to label
#' 
#' This is almost verbatim a copied implementation of this internal shiny function.
#' Prevents us from relying on an unexported function from that package.
#' 
#' @keywords internal
quoToLabel = function (q, functionLabel, label = NULL) {
  if (!is.null(label)) 
    return(label)
  sprintf("%s(%s)", functionLabel, quoToLabelBody(q))
}

#' quosure to label body
#' 
#' This is almost verbatim a copied implementation of this internal shiny function.
#' Prevents us from relying on an unexported function from that package.
#' 
#' @keywords internal
quoToLabelBody = function (q) {
  paste(deparse(rlang::quo_get_expr(q)), collapse = "\n")
}