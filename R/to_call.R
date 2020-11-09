.to_call <- function(flow) {

  my_calls <- flow$calls

  call <- glue::glue("flow <- workflow('{flow$name}')")

  for (i in seq_along(my_calls)) {

    this_call <- my_calls[[i]]

    # Only inputs
    if (length(this_call$outputs) == 0) {

      added_inputs <- stringr::str_flatten(this_call$inputs, collapse = "', '")

      call <- c(call,
                glue::glue("inputs('{added_inputs}')"))

    }

    if (length(this_call$outputs) >= 1) {

      my_inputs <- stringr::str_flatten(this_call$inputs, collapse = "', '")

      my_function <- this_call$output_id

      my_outputs <- stringr::str_flatten(this_call$outputs, collapse = "', '")

      call <- c(call,
                glue::glue("step({my_function}, inputs = c( '{my_inputs}'), output = c('{my_outputs}'))"))

    }

  }

  call <- call %>%
    stringr::str_flatten(collapse = " %>% \n")

  if (requireNamespace("styler", quietly = TRUE)) {

    call <- call %>%
      styler::style_text()

  }


  return(call)

}
