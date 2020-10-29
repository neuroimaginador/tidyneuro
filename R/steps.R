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
#  - update(flow, output_name, new_function)



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
#' @export
#'
step <- function(flow,
                         f,
                         inputs = ifelse(inherits(f, "function"), list(names(formals(f))), list()),
                         output,
                         ...) {

  # browser()

  # Basic checks
  stopifnot(inherits(flow, "workflow"))
  stopifnot(inherits(f, "function"))
  output <- as.character(output)
  # Check that the output is not currently included.
  if (any(output %in% flow$outputs)) {

    stop("Output currently in the flow.", call. = FALSE)

  }


  if (length(inputs) > 0) inputs <- unlist(inputs)

  if (inherits(f, "function")) type <- "function"

  flow$log(level = "DEBUG",
           message = paste0("Adding process with inputs: ",
                            stringr::str_flatten(unlist(inputs), collapse = ", "),
                            " and output(s): ",
                            stringr::str_flatten(unlist(output), collapse = ", ")))

  # Add a node to the graph, with edges from its inputs to it.
  # Generate an ID for the name of the node in the graph
  id <- paste0(sample(c(letters, 0:9), size = 8L, replace = TRUE), collapse = "")

  if (length(output) == 1) id <- output

  flow$graph <- flow$graph %>%
    igraph::add_vertices(nv = 1, name = id, type = type)
  new_vertex_idx <- length(igraph::V(flow$graph))

  # browser()
  if (length(inputs) > 0) {

    input_ids <- match(inputs, flow$outputs)
    flow$inmediate_inputs[[id]] <- flow$outputs[input_ids]

    flow$graph <- flow$graph %>%
      igraph::add_edges(edges = as.vector(rbind(input_ids, new_vertex_idx)))

  }

  # Add the model to the list of flow models
  additional_params <- list(...)

  if (length(additional_params) > 0) {

    f <- f %>% pryr::partial(..., .lazy = FALSE)

  }

  # Add its pipeline (updating all previous pipelines)
  inputs <- which(flow$node_types == "Input")
    # which(V(flow$graph)$type == "Input")
  for (target_idx in setdiff(seq(new_vertex_idx), inputs)) {

    # Path from the current node to inputs
    paths <- flow$graph %>%
      igraph::all_simple_paths(from = target_idx,
                               to = inputs,
                               mode = "in")
    paths <- lapply(paths, unclass)
    paths <- lapply(paths, as.vector)
    nodes_for_target <- unique(unlist(paths))

    # Topological order of the graph
    pipeline <- igraph::topo_sort(flow$graph)

    # Restricted to nodes connected to the current node
    pipeline <- pipeline[pipeline %in% nodes_for_target]

    # Update the list of current required inputs and the pipeline for the current node
    flow$required_inputs[[igraph::V(flow$graph)$name[target_idx]]] <- intersect(pipeline, inputs)
    flow$pipeline[[igraph::V(flow$graph)$name[target_idx]]] <- setdiff(pipeline, inputs)

  }

  # Add the function to the list, depending if the number of
  # outputs is 1 or more than 1.
  if (length(output) == 1) {

    # V(flow$graph)$name[new_vertex_idx] <- output
    flow$processes[[output]] <- f
    flow$outputs <- c(flow$outputs, output)
    flow$node_types <- c(flow$node_types, "Output")
    names(flow$node_types)[length(flow$node_types)] <- output
    # Add package dependencies
    flow$pkgs[[output]] <- .get_dependencies(f)

  } else {

    flow$processes[[id]] <- f
    flow$outputs <- c(flow$outputs, id)
    flow$node_types <- c(flow$node_types, "Hub")
    names(flow$node_types)[length(flow$node_types)] <- id
    # Add package dependencies
    flow$pkgs[[id]] <- .get_dependencies(f)


    for (out_id in seq_along(output)) {

      out <- output[out_id]

      flow$outputs <- c(flow$outputs, out)
      flow$node_types <- c(flow$node_types, "Output")
      names(flow$node_types)[length(flow$node_types)] <- out

      flow$graph <- flow$graph %>%
        igraph::add_vertices(nv = 1, name = out, type = type)

      flow$inmediate_inputs[[out]] <- id

      flow$required_inputs[[out]] <- flow$required_inputs[[id]]
      flow$pipeline[[out]] <- c(flow$pipeline[[id]],
                                length(igraph::V(flow$graph)))

      flow$processes[[out]] <- pryr::partial(.extract_field, nm = out, i = out_id, .lazy = FALSE)

    }

    n <- length(igraph::V(flow$graph))

    flow$graph <- flow$graph %>%
      igraph::add_edges(edges = as.vector(rbind(new_vertex_idx,
                                        seq(new_vertex_idx + 1, n))))


  }


  return(invisible(flow))

}

.extract_field <- function(L, nm, i) {

  if (nm %in% names(L)) {

    return(L[[nm]])

  } else {

    return(L[[i]])

  }

}
