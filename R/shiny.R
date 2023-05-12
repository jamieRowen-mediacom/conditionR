#' ObserveEvent with handlers
#' 
#' An alternative implementation of [shiny::observeEvent()] which wraps the
#' handlerExpr with condition handling.
#' 
#' @inheritParams shiny::observeEvent
#' @param ... arguments passed on to [base::withCallingHandlers()]
#' @param use_default_shiny_handlers logical(1) if TRUE, the default,
#'  use the registered default condition handlers in addition to any specified
#'  handlers passed. See [conditionR::register_shiny_condition_handlers()]
wh_observeEvent = function(
  eventExpr, handlerExpr,
  ...,
  use_default_shiny_handlers = TRUE,
  event.env = parent.frame(),
  event.quoted = FALSE, handler.env = parent.frame(), handler.quoted = FALSE,
  label = NULL, suspended = FALSE, priority = 0, domain = shiny::getDefaultReactiveDomain(),
  autoDestroy = TRUE, ignoreNULL = TRUE, ignoreInit = FALSE,
  once = FALSE
) {
    dots = rlang::list2(...)
    if (use_default_shiny_handlers) {
      dots = c(dots, as.list(.shiny_handlers))
    }
      eventQ <- exprToQuo(eventExpr, event.env, event.quoted)
      handlerQ <- exprToQuo(handlerExpr, handler.env, handler.quoted)
      label <- quoToLabel(eventQ, "observeEvent", label)
      handler <- rlang::inject(
        shiny::observe(
          withCallingHandlers(!!handlerQ, !!!dots),
          label = label, suspended = suspended,
          priority = priority, domain = domain, autoDestroy = TRUE, 
          ..stacktraceon = FALSE
        )
      )
      o <- rlang::inject(
        shiny::bindEvent(
          ignoreNULL = ignoreNULL, ignoreInit = ignoreInit,
          once = once, label = label, !!eventQ, x = handler
        )
      )
      invisible(o)
}

skip = function() {
  r = findRestart("skip")
  if (is.null(r)) return()
  invokeRestart(r)
}

wh_reactive = function(x, ..., use_default_shiny_handlers = TRUE, env = parent.frame(), quoted = FALSE, label = NULL, domain = shiny::getDefaultReactiveDomain(), ..stacktraceon = TRUE) {
  expr = exprToQuo(x, env, quoted)
  dots = rlang::list2(...)
  if (use_default_shiny_handlers) {
    dots = c(dots, as.list(.shiny_handlers))
  }
  rlang::inject(
    shiny::reactive(
      withCallingHandlers(withRestarts(!!expr, skip = function(e) NULL), !!!dots),
      env = env, quoted = quoted, label = label, domain = domain, ..stacktraceon = ..stacktraceon
    )
  )
}

monkey_patch_shiny = function(functions = c("observeEvent", "reactive")) {
  if (length(functions) == 0) return(invisible(NULL))
  shiny = getNamespace("shiny")
  for (f in functions) {
    message("Patching ", f)
    unlockBinding(f, shiny)
    shiny[[f]] = get(paste0("wh_", f), envir = getNamespace("conditionR"))
    lockBinding(f, shiny)
  }
  # unlockBinding("observeEvent", shiny)
  # shiny$observeEvent = wh_observeEvent
  # lockBinding("observeEvent", shiny)
}