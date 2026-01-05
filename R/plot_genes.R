#' Plot Gene Expression
#'
#' Given a SummarizedExperiment object, plots gene expression for specified samples.
#' A simple wrapper for scater::plotExpression to work on SE objects
#'
#' @param se A SummarizedExperiment object.
#' @param samples Vector of sample names to include in the plot.
#' @param genes Vector of gene names to plot expression for.
#' @param assay The assay of expression to plot (default is "logcounts").
#' @param x Variable to use for the x-axis.
#' @param colour Variable to use for coloring points on the plot.
#'
#' @return A gene expression plot.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' plot_expression(
#'   se,
#'   samples = c("Sample1", "Sample2"),
#'   genes = c("Gene1", "Gene2"),
#'   x = "Condition",
#'   colour = "Group"
#' )
#' }
#'
#' @importFrom scater logNormCounts plotExpression
#' @importFrom SummarizedExperiment SummarizedExperiment
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import rlang
#'
#' @export
plot_expression <- function(se,
                       samples,
                       genes,
                       assay = "logcounts",
                       x,
                       colour){
  se[, samples] |>
    as("SingleCellExperiment") |>
    scater::plotExpression(
      genes,
      exprs_values = assay,
      colour_by = colour,
      x = x,
      swap_rownames = "gene_name"
    )
}
