correlation_network <- function(data, variables, cutoff = css_config$network_correlation_cutoff,
                                alpha = css_config$network_alpha,
                                adjustment = css_config$network_adjustment) {
  result <- psych::corr.test(data[, variables, drop = FALSE], use = "pairwise",
                             method = "spearman", adjust = adjustment, alpha = alpha)
  adjacency <- result$r
  adjacency[result$p > alpha | abs(adjacency) < cutoff] <- 0
  adjacency[is.na(adjacency)] <- 0
  diag(adjacency) <- 0
  graph <- igraph::graph_from_adjacency_matrix(adjacency, mode = "undirected", weighted = TRUE, diag = FALSE)
  igraph::E(graph)$correlation <- igraph::E(graph)$weight
  igraph::E(graph)$color <- ifelse(igraph::E(graph)$correlation > 0, "#d7301f", "#2c7fb8")
  igraph::E(graph)$width <- abs(igraph::E(graph)$correlation) * 5
  graph
}

plot_correlation_network <- function(graph, title, file) {
  grDevices::pdf(file, width = 9, height = 9)
  on.exit(grDevices::dev.off(), add = TRUE)
  plot(graph, layout = igraph::layout_in_circle(graph), edge.curved = 0.15,
       vertex.size = 11, vertex.label.cex = 0.65, main = title)
}
