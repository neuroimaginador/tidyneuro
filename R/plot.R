#' @title Plot a Flow
#'
#' @description This function plots the computation graph of the flow.
#'
#' @param flow The workflow to plot.
#'
#' @export
plot <- function(flow) {

  M <- igraph::as_adj(flow$graph) %>% as.logical()
  M <- matrix(M, nrow = length(igraph::V(flow$graph)))

  labels <- flow$outputs
  node_types <- unlist(flow$node_types)
  labels[node_types == "Hub"] <- "*"

  av_colors <- grDevices::palette.colors(n = length(unique(node_types)),
                              palette = "ggplot2")
  colors <- av_colors[as.numeric(as.factor(unlist(flow$node_types)))]

  attrs <- list(graph = list(bgcolor = "#FFFFFF"),
                edge = list(arrowsize = "0.5",
                            style = "lty",
                            minlen = "1"),
                node = list(color = "transparent",
                            fixedsize = FALSE,
                            fillcolor = "transparent"))
  # names(colors) <- labels
  nAttrs <- list(fontcolor = colors,
                 label = labels)

  nAttrs <- lapply(nAttrs, function(x) {
    names(x) <- flow$outputs
    x
  })

  colnames(M) <- rownames(M) <- flow$outputs

  graphics::plot.new()
  Rgraphviz::plot(methods::as(M, "graphNEL"),
                  attrs = attrs,
                  nodeAttrs = nAttrs)

}
