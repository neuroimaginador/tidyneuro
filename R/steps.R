##%######################################################%##
#                                                          #
####                       Steps                        ####
#                                                          #
##%######################################################%##

# A step:
#  - Contains a function/method/procedure
#  - Must state which inputs are mandatory, and default values for optional inputs
#  - Must state a name for ALL of its inputs
#  - Must specify (...) arguments to be passed to other methods.
#  - Should rename the formals of the inner function to match those of the input.
#  - If more than one output is provided, the step should have a unique id.



#' @title Add a Function to a Flow
#'
#' @description This function adds another function to a given flow.
#'
#' @param flow       (a flow) The flow where to add a new function
#' @param f       (a function) The function itself to be added
#' @param inputs     (list) List of the inputs needed for the function to be executed, defaults to the formal arguments of the function,
#'    list())
#' @param output     (name) The name to assign to the output of the function.
#' @param ...        extra parameters for the function, allows to parameterize the flow.
#'
#' @return The flow with the function added
#'
#' @seealso
#'  \code{\link[igraph]{add_vertices}},\code{\link[igraph]{add_edges}}
#' @importFrom igraph add_vertices add_edges V
#' @import stringr
#' @importFrom pryr partial
#'
step <- function(flow,
                         f,
                         inputs = ifelse(inherits(proc, "function"), list(names(formals(proc))), list()),
                         output,
                         ...) {


  # Basic checks
  stopifnot(inherits(flow, "workflow"))
  stopifnot(inherits(f, "function"))
  output <- as.character(output)

  if (length(inputs) > 0) inputs <- unlist(inputs)

  if (inherits(f, "function")) type <- "function"

  flow$log(level = "DEBUG",
           message = paste0("Adding process with inputs: ",
                            str_flatten(unlist(inputs), collapse = ", "),
                            " and output(s): ",
                            str_flatten(unlist(output), collapse = ", ")))

  # Add a node to the graph, with edges from its inputs to it.
  flow$graph <- flow$graph %>% add_vertices(nv = 1, name = output, type = type)
  new_vertex_idx <- length(V(flow$graph))

  if (length(inputs) > 0) {

    input_ids <- match(inputs, flow$outputs)
    flow$inmediate_inputs[[output]] <- flow$outputs[input_ids]

    flow$graph <- flow$graph %>%
      add_edges(edges = as.vector(rbind(input_ids, new_vertex_idx)))

  }

  # Add the model to the list of flow models
  additional_params <- list(...)

  if (length(additional_params) > 0) {

    f <- f %>% partial(...)

  }

  flow$processes[[output]] <- f
  flow$outputs <- c(flow$outputs, output)
  flow$node_types <- c(flow$node_types, "Output")

  # Add package dependencies
  # flow$pkgs[[output]] <- .get_dependencies(f)

  # Add its pipeline (updating all previous pipelines)
  inputs <- which(flow$node_types == "Input")
    # which(V(flow$graph)$type == "Input")
  for (target_idx in setdiff(seq(new_vertex_idx), inputs)) {

    # Path from the current node to inputs
    paths <- flow$graph %>% all_simple_paths(from = target_idx, to = inputs, mode = "in")
    paths <- lapply(paths, unclass)
    paths <- lapply(paths, as.vector)
    nodes_for_target <- unique(unlist(paths))

    # Topological order of the graph
    pipeline <- topo_sort(flow$graph)

    # Restricted to nodes connected to the current node
    pipeline <- pipeline[pipeline %in% nodes_for_target]

    # Update the list of current required inputs and the pipeline for the current node
    flow$required_inputs[[V(flow$graph)$name[target_idx]]] <- intersect(pipeline, inputs)
    flow$pipeline[[V(flow$graph)$name[target_idx]]] <- setdiff(pipeline, inputs)

  }

  return(invisible(flow))

}
