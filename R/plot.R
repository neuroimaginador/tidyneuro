#' @importFrom igraph as_adj
#' @importFrom hasseDiagram hasse
plot <- function(flow) {

  M <- as_adj(flow$graph) %>% as.logical()
  M <- matrix(M, nrow = length(V(flow$graph)))

  hasse(data = M,
        labels = flow$outputs,
        parameters = list(transitiveReduction = FALSE))

}
