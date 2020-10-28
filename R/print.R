#' Print Flow Information
#'
#' @param flow   (a \code{workflow} object) Flow to print
#'
#' @return Prints the name, inputs, outputs, package dependencies and memory used.
#'
#' @export
#'
print.workflow <- function(flow) {

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

  # Name, inputs and outputs
  flow_name <- flow$name %>% bold()
  flow_inputs <- stringr::str_flatten(flow$inputs %>% green(),
                                      collapse = ", ")
  flow_outputs <- stringr::str_flatten((flow$outputs[flow$node_types == "Output"]) %>% blue(),
                              collapse = ", ")

  cat(underline("workflow with:\n"),
      " name:", flow_name, "\n",
      " inputs:", flow_inputs, "\n",
      " outputs:", flow_outputs, "\n")

  # # Dependencies
  flow_deps <- flow$pkgs %>% unlist() %>% unique()

  if (length(flow_deps) > 0) {

    cat("It depends on functions of the following packages:",
        stringr::str_flatten(flow_deps %>% red(), collapse = ", "), "\n")

  }

  # Currently used memory
  cat("Currently using", (flow %>% pryr::object_size() %>% as.numeric()) %>% prettyunits::pretty_bytes(),
      "of memory.\n")

  invisible(TRUE)

}

#' Summary of Flow
#'
#' @param flow   (a \code{NIflow} object) Flow to print summary of.
#'
#' @return Prints inputs, outputs and dependencies between them.
#' @export
#'
summary.workflow <- function(flow) {

  # If available, pretty-print summary
  has_crayon <- requireNamespace("crayon", quietly = TRUE)

  if (!has_crayon) {

    underline <- function(s) s

  } else {

    underline <- crayon::underline

  }

  print(flow)

  # Dependencies between inputs and outputs
  outputs <- flow$outputs[flow$node_types == "Output"]
  # edges <- igraph::as_edgelist(flow$graph)
  # inputs_needed <- edges[, 1]
  # outputs <- edges[, 2]
  #
  # only_outputs <- setdiff(flow$outputs, flow$inputs)

  relationships <- c()

  for (out in outputs) {

    inputs_needed <- .closest_inputs(flow, out)

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
