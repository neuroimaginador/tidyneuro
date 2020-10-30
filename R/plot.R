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

  color_ramp <- scales::colour_ramp(c("red", "black", "blue"))
  av_colors <- color_ramp(seq(0, 1, length = length(unique(node_types))))
  names(av_colors) <- c("Input", setdiff(unique(flow$node_types), c("Input", "Output")), "Output")
  colors <- av_colors[flow$node_types]

  attrs <- list(graph = list(bgcolor = "#FFFFFF",
                             splines = "spline",
                             ratio = "expand",
                             ranksep = "0.3",
                             center = TRUE),
                edge = list(arrowsize = "0.6",
                            arrowhead = "vee",
                            style = "lty",
                            minlen = "2"),
                node = list(color = "transparent",
                            fontsize = 16,
                            shape = "plaintext",
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
