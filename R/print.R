#' Print Flow Information
#'
#' @param x   (a \code{workflow} object) Flow to print
#' @param ... Not used
#'
#' @return Prints the name, inputs, outputs, package dependencies and memory used.
#'
#' @export
#'
print.workflow <- function(x, ...) {

  has_crayon <- requireNamespace("crayon", quietly = TRUE)

  if (!has_crayon) {

    bold <- red <- green <- underline <- blue <- function(s) s

  } else {

    bold <- crayon::bold
    red <- crayon::red
    green <- crayon::green
    underline <- crayon::underline
    blue <- crayon::blue

  }

  output_types <- c("Output", "HubOutput")

  # Name, inputs and outputs
  flow_name <- x$name %>% bold()
  flow_inputs <- stringr::str_flatten(x$inputs %>% green(),
                                      collapse = ", ")
  flow_outputs <- stringr::str_flatten((x$outputs[x$node_types %in% output_types]) %>% blue(),
                              collapse = ", ")

  cat(underline("workflow with:\n"),
      " name:", flow_name, "\n",
      " inputs:", flow_inputs, "\n",
      " outputs:", flow_outputs, "\n")

  # # Dependencies
  flow_deps <- x$pkgs %>%
    unlist() %>%
    unique()

  pkgs_id <- flow_deps %>%
    stringr::str_which(pattern = stringr::fixed("namespace:"))

  if (length(pkgs_id) > 0) {

    flow_deps <- flow_deps[pkgs_id] %>%
      stringr::str_remove_all(pattern = stringr::fixed("namespace:"))

    cat("It depends on functions of the following packages:",
        stringr::str_flatten(flow_deps %>% red(), collapse = ", "), "\n")

  }

  # Currently used memory
  cat("Currently using", x %>%
        pryr::object_size() %>%
        as.numeric() %>%
        prettyunits::pretty_bytes(),
      "of memory.\n")

  invisible(TRUE)

}

#' Summary of Flow
#'
#' @param object   (a \code{NIflow} object) Flow to print summary of.
#' @param ... Not used.
#'
#' @return Prints inputs, outputs and dependencies between them.
#' @export
#'
summary.workflow <- function(object, ...) {

  # If available, pretty-print summary
  has_crayon <- requireNamespace("crayon", quietly = TRUE)

  if (!has_crayon) {

    underline <- function(s) s

  } else {

    underline <- crayon::underline

  }

  print(object)

  # Dependencies between inputs and outputs
  output_types <- c("Output", "HubOutput")
  outputs <- object$outputs[object$node_types %in% output_types]

  relationships <- c()

  for (out in outputs) {

    inputs_needed <- .closest_inputs(object, out)

    relationships <- c(relationships,
                       paste0("  ", out, " needs ",
                              stringr::str_flatten(inputs_needed,
                                          collapse = ", "),
                              " as inputs."))

  }

  if (length(relationships) > 0) {

    cat(underline("Summary of processes:\n"))

    cat(relationships, sep = "\n")

  }

  invisible(TRUE)

}

.closest_inputs <- function(flow, output) {

  inputs <- flow$inmediate_inputs[[output]]

  id <- names(which(flow$node_types[inputs] == "Hub"))

  if (length(id) > 0) {

    hubs <- id
    inputs <- setdiff(inputs, hubs)

    for (h in hubs) {

      inputs <- c(inputs, .closest_inputs(flow, h))

    }

  }

  return(inputs)

}

.S3methods("print", "workflow")
.S3methods("summary", "workflow")
