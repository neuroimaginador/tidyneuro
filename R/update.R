#' Update a Workflow Node
#'
#' @param flow      The \code{workflow} object to update
#' @param output    The name of the output to update
#' @param new_function A function that will substitute the one already stored in the \code{output}
#'
#' @return Invisibly, the updated flow
#' @export
#'
update <- function(flow, output, new_function) {

  if (flow$node_types[output] == "Output") {

    # should perform compatibility checks before...

    flow$processes[[output]] <- new_function
    flow$pkgs[[output]] <- get_deps(new_function)

  }

  if (flow$node_types[output] == "HubOutput") {

    # Get parent node:
    id <- flow$inmediate_inputs[[output]]

    flow$processes[[id]] <- new_function
    flow$pkgs[[id]] <- .get_dependencies(new_function)

  }

  return(invisible(flow))

}
