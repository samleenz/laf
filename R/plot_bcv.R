#' Plot BCV versus average log counts per million (log-CPM) for genes in DGEList object
#'
#' @param y A DGEList object.
#' @param logCPM_cutoff A numeric value specifying the minimum average log-CPM required for a gene to be plotted.
#' @param BCV_cutoff A numeric value specifying the minimum biological coefficient of variation required for a gene to be plotted.
#' @param main A character string for the plot title. (defaults to BCV value)
#'
#' @importFrom edgeR plotBCV
#'
#' @examples
#' library(edgeR)
#' data(dgelist)
#' plot_genes(dgelist)
#'
#' @export
plot_bcv <- function(y, logCPM_cutoff = 2, BCV_cutoff = 0.7, main_title = NULL) {
  bcv_df <-  data.frame(
    'BCV' = sqrt(y$tagwise.dispersion),
    'AveLogCPM' = y$AveLogCPM,
    'gene_name' = y$genes$gene_name,
    'gene_id' = rownames(y)
  )

  high_bcv <- bcv_df$AveLogCPM > logCPM_cutoff & bcv_df$BCV > BCV_cutoff
  bcv_df <- bcv_df[high_bcv, ]

  if (is.null(main_title)) {
    main_title <- glue::glue("BCV: {round(sqrt(y$common.dispersion), 3)}")
  }

  plotBCV(y, main = main_title)
  points(bcv_df$AveLogCPM, bcv_df$BCV, col = "red")
  text(bcv_df$AveLogCPM, bcv_df$BCV, labels = bcv_df$gene_name, pos = 4)
}
