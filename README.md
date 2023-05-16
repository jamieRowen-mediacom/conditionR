
<!-- README.md is generated from README.Rmd. Please edit that file -->

# conditionR

<!-- badges: start -->

<!-- badges: end -->

Gracefully dealing with errors can be hard, particularly in shiny apps.
There are many operations that your shiny application might take that
could cause an issue that you want to recover from in some fashion.
Applying changes to a database, we might want to rollback if something
unexpected is found. If a user supplies data in a different format to
that expected we might want to indicate that to them, rather than have
the session crash. Dealing with different types of issues in shiny apps
can be difficult, this package facilitates easier processing of
different types of issues.

With {conditionR}, make your shiny application stand *head and
shoulders* above the rest by adding infrastructure for condition and
error handling to your reactives and observers.

There will be *no more tears* from your users getting disconnected from
your app as a result of a crash or unexpected operation if you nourish
your applications with {conditionR}. It’s *used by professionals*.

Each shiny reactive and observer function can be called with specified
condition handlers, restart procedures as well as default handlers that
should apply to all reactive and observer functions. {conditionR} also
provides a monkey patch for the shiny functions, so that you don’t have
to manually update all calls to these functions. Just add

``` r
conditionR::monkey_patch_shiny()
```

at the top of your app server to apply this infrastructure to the
existing calls to `shiny::reactive` etc.

*Because you’re worth it*

## Installation

You can install the development version of conditionR like so:

``` r
# install.package("remotes")
remotes::install_github("jamieRowen-mediacom/conditionR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(conditionR)
## basic example code

library(shiny)
library(magrittr)

# a basic ui for triggering conditions
ui = fluidPage(
  shiny::actionButton("go", "GO Specific handler!"),
  shiny::actionButton("go2", "Go default handler!"),
  shiny::actionButton("go3", "Go default and specific handler!"),
  shiny::textOutput("out")
)

# a new condition signal type
default_condition = condition_factory("default")

# a function to raise a default condition signal
throw_default = function() {
  default_condition("default")
}

# a handler function for default condition
handle_default = function(x) {
  shiny::showModal(
    shiny::modalDialog("Default Condition handled")
  )
}

# register this handler to apply to all reactives/observers
handle_default %>%
  as_handler("default") %>%
  register_shiny_condition_handler()

# an error handling function
handle_error = function(x) {
  shiny::showNotification("There was an error")
  skip() # with skip if an error is hit, restart execution by returning NULL instead of crashing
}


# register the error handler to apply by deafult
handle_error %>%
  as_handler("error") %>%
  register_shiny_condition_handler()

# a function to generate functions and sometimes return an "example condition" signal
rand = function() {
  x = runif(1)
  if (x < 0.5) {
    print("should raise condition")
    example_condition("bad value example")
  }
  x
}

# a function which induces an error
bad = function() {
  stop("error")
}

server = function(input, output, session) {
  monkey_patch_shiny() # apply condition handling and restarts to shiny reactives and observers
  x = shiny::reactiveVal(0)

  output$out = shiny::renderText(x())

  # observeEvent will now take parameters for condition handlers
  shiny::observeEvent(
    input$go, {
      print("go")
      val = rand()
      x(val)
      throw_default()
    },
    "conditionr_example" = function(x) { # handle the example condition signal
      print("help!!!")
      shiny::showNotification(x$message, session = session)
    },
    use_default_shiny_handlers = FALSE # this observer should ignore the default handlers
  )

  # this observer will respect the default handlers, clicking the button will throw a default condition
  shiny::observeEvent(
    input$go2, {
      print("go 2")
      val = rand()
      x(val)
      throw_default()
    }
   )
  
  # an observer which respects both specific handlers and the registered defaults
  shiny::observeEvent(
    input$go3, {
      print("go")
      val = rand()
      x(val)
      throw_default()
    },
    "conditionr_example" = function(x) {
      print("help!!!")
      shiny::showNotification(x$message, session = session)
    }
  )

  # the functions have a wh_[*] equivalent
  # the monkey patch binds these definitions to the shiny functions
  # for convenience
  # this reactive will respect the default conditions (including the error handler)
  # as well as the specific
  r = wh_reactive({
    shiny::invalidateLater(1000)
    x = rand()
    if (x > 0.8){
      bad() # throw an error
    }
    x
  }, "conditionr_example" = function(x) {
    shiny::showNotification(paste0("reactive condition handler ", x$message), session = session)
  })

  shiny::observe({
    x = r()
    print(x)
  })

}

shiny::shinyApp(ui, server)
```
