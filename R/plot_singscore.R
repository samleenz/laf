#' Generate a singscore landscape plot
#'
#' @param se A summarised experiment object
#' @param gs_x The geneset to score for the x axis
#' @param gs_y The geneset to score for the y axis
#' @param name_x x-axis label
#' @param name_y y-axil label
#' @param colour A colData name (bare) to set point colour by
#' @param shape  A colData name (bare) to set point shape by
#'
#' @details
#' `gs_x` and `gs_y` are vectors of gene symbols (HGNC). This function assumes
#' that the gene symbols are in the gene_name slot of the SE object
#'
#'
#' @return a ggplot2 object
#' @export
#'
plot_singscore_landscape <- function(se, gs_x, gs_y, name_x, name_y, colour = NULL, shape = NULL){

  # check for NAs and drop
  if(any(is.na(gs_x))){
    message("gs_x contains NAs: dropping these")
    gs_x <- gs_x[! is.na(gs_x)]
  }
  if(any(is.na(gs_y))){
    message("gs_y contains NAs: dropping these")
    gs_y <- gs_y[! is.na(gs_y)]
  }

  # replace the ENSG labels with HGNC symbols and de-duplicate
  se2 <- se[!is.na(rowData(se)$gene_name), ]
  rownames(se2) <- rowData(se2)$gene_name
  se2 <- se2[! duplicated(rownames(se2)), ]

  # run the singscore procedure
  g_ranks <- singscore::rankGenes(se2)
  singscores <- singscore::multiScore(
    g_ranks,
    upSetColc = list(
      x = gs_x,
      y = gs_y
    ),
    knownDirection = FALSE,
    centerScore = FALSE
  )

  singscores$Scores |>
    as.data.frame() |>
    tibble::rownames_to_column(var = "geneset") |>
    tidyr::pivot_longer(-geneset, names_to = "Sample", values_to = "Score") |>
    tidyr::pivot_wider(names_from = geneset, values_from = Score) |>
    dplyr::left_join(
      SummarizedExperiment::colData(se) |>
        as.data.frame() |>
        tibble::rownames_to_column(var = "Sample"),
      by = "Sample"
    ) |>
    ggplot(aes(x, y, colour = {{ colour }}, shape = {{ shape }})) +
    geom_point(size = 4) +
    ggpubr::theme_pubr(border = TRUE) +
    labs(
      x = name_x,
      y = name_y
    )
}
