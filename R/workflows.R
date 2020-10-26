##%######################################################%##
#                                                          #
####                     Workflows                      ####
#                                                          #
##%######################################################%##


# A workflow:
#  - Is represented by a graph
#  - Must save multiple temporary results to disk to save memory, if requested to do so.
#  - Must allow for a step to provide multiple outputs.
#  - Must check the input object type of a step in its creation. thus, must handle properly object types and their related functions.
#  - Must compute only the necessary steps to provide the requested outputs.
#  - May be exported to a package. Note its package dependencies.
#  - Can be saved to/loaded from disk in compressed format.
#  - Can be exported to disk and printed to console in a human-readable format.
#  - Can be nicely plotted (even using tikzDevice for Latex).
#  - Must log all the steps...
#  - Take care of dependencies between steps.

# The constructor function:
#  - Initializes the workflow name/description
#  - May initialize inputs.

#' @importFrom igraph make_empty_graph add_vertices
workflow <- function(name = "",
                     work_dir = tempdir(),
                     inputs = list()) {

  # A flow is an environment
  flow <- new.env()
  flow$name <- as.character(name)
  flow$work_dir <- work_dir

  # List of flow inputs and outputs
  flow$inputs <- list()
  flow$outputs <- list()
  flow$element_types <- list()
  flow$node_types <- list()

  # List of flow processes (both models and functions)
  flow$processes <- list()
  flow$pkgs <- list()

  # List of pipelines to execute for each process and of required inputs
  flow$pipeline <- list()
  flow$required_inputs <- list()
  flow$inmediate_inputs <- list()

  # Create graph of dependencies
  flow$graph <- make_empty_graph(directed = TRUE)

  # Add inputs to the graph
  if (length(inputs) > 0) {

    if (!is.null(names(inputs))) {

      input_names <- names(inputs)
      input_types <- unname(inputs)

    } else {

      input_names <- unlist(inputs)
      input_types <- rep(element_object(),
                         length(input_names))

    }

    flow$inputs <- input_names
    flow$element_types <- input_types
    flow$node_types <- rep("Input", length(input_names))
    flow$graph <- flow$graph %>% add_vertices(nv = length(inputs),
                                              name = unlist(inputs),
                                              type = rep("Input", length(inputs)))

  }

  # List of all possible outputs of the flow
  flow$outputs <- unlist(inputs)

  # Add ability to log:
  flow$log_lines <- c()

  with(flow, expr = {

    log <- function(level = c("DEBUG", "INFO", "WARNING", "ERROR"),
                    message = "...") {

      line_to_add <- paste0("(", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ") [",
                            level[1], "] ",
                            message)

      log_lines <<- c(log_lines, line_to_add)

    }

  })

  # Add function to execute processes (this way it can be extended to non-function objects)
  with(flow, expr = {

    execute_process <- function(what, args) {

      do.call(what = what, args = args)

    }

  })

  # Add a function to clone processes included in the flow
  with(flow, expr = {


    clone_process <- function(process) {

      return(process)

    }

  })

  # Add a function to save and load a process
  with(flow, expr = {

    export_process <- function(process, path, prefix) {

      saveRDS(object = process,
              file = file.path(path,
                               paste0(prefix, ".rds")))

      return(invisible(TRUE))

    }

    import_process <- function(filename) {

      output_name <- gsub(x = basename(filename), pattern = ".rds", replacement = "")

      output_process <- readRDS(filename)

      return(list(output_name, output_process))

    }

  })

  class(flow) <- "workflow"
  attr(flow, "package") <- "tidyneuro"
  return(flow)

}

#' @title Add an Input to a Flow
#'
#' @description This function adds a node of type input to a flow.
#'
#' @param flow      (a flow) The flow where to add the input
#' @param ...    (list or vector) List of the inputs to add, can be given with or without quotes, e.g. list(A, B) or list("A", "B"). Default: list()
#'
#' @return Returns (invisibly) the flow with added inputs.
#'
#' @seealso
#'  \code{\link[igraph]{add_vertices}}
#' @importFrom igraph add_vertices
#'
inputs <- function(flow, ...) {

  # Basic checks
  stopifnot(inherits(flow, "workflow"))

  inputs <- list(...)

  # Add inputs to the graph
  if (length(inputs) > 0) {

    if (!is.null(names(inputs))) {

      input_names <- names(inputs)
      input_types <- unname(inputs)

    } else {

      input_names <- unlist(inputs)
      input_types <- rep(element_object(),
                         length(input_names))

    }

    flow$log(level = "DEBUG",
             message = paste0("Adding inputs ",
                              str_flatten(unlist(input_names),
                                          collapse = ", ")))

    flow$inputs <- c(flow$inputs, input_names)
    flow$element_types <- c(flow$element_types, input_types)
    flow$graph <- flow$graph %>% add_vertices(nv = length(inputs),
                                              name = unlist(inputs),
                                              type = rep("Input", length(inputs)))

    # List of all possible outputs of the flow
    flow$outputs <- c(flow$outputs, unlist(input_names))
    flow$node_types <- c(flow$node_types, rep("Input", length(input_names)))

  }

  return(invisible(flow))

}
