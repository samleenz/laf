#' Generate a heatmap based on dissimilarity values
#'
#' This function generates a heatmap based on the dissimilarity values
#' calculated using a specified distance function.
#'
#' @param se A SummarizedExperiment object containing the data.
#' @param features Character vector specifying the features to include in the heatmap.
#' @param samples Character vector specifying the samples to include in the heatmap.
#' @param assay The assay to be used for calculating dissimilarity (default is "logcounts").
#' @param annots Character vector, what sample annotations to include as strip charts
#' @param annots_col_list list, with colour palettes per annotation (see examples)
#' @param distfun The distance function to use (default is "pearson").
#' @param title The title of the heatmap (default is "A heatmap").
#'
#' @return A heatmap visualizing dissimilarity values.
#'
#' @importFrom ComplexHeatmap Heatmap HeatmapAnnotation draw
#' @importFrom viridis viridis
#' @importFrom scales rescale
#' @importFrom circlize colorRamp2
#' @importFrom ncla dissim
#' @importFrom SummarizedExperiment assay rownames colnames colData
#'
#' @examples
#' # Example usage of plot_distance_heatmap function
#' \dontrun{
#' se <- ... # Create or load your SummarizedExperiment object
#' plot_distance_heatmap(se)
#'
#' annot_cols_list = list(
#'   Tissue = pal_tissue[unique(se_f$Tissue)],
#'   Clone = pal_clone[unique(se_f$Clone)],
#'   Mouse_ID = pal_mouse[unique(se_f$Mouse_ID)],
#'   Batch = pal_batch[unique(se_f$Batch)]
#'   )
#' }
#'
#' @export
plot_distance_heatmap <- function(se,
                         features = rownames(se),
                         samples = colnames(se),
                         assay = "logcounts",
                         annots = NULL,
                         annots_col_list = NULL,
                         distfun = "pearson",
                         title = "A heatmap"){
  # require(ComplexHeatmap)

  stopifnot(all(features %in% rownames(se)))
  stopifnot(all(samples  %in% colnames(se)))
  stopifnot(all(annots   %in% colnames(SummarizedExperiment::colData(se))))
  stopifnot(all(names(annots_col_list) %in% annots))

  se_f <- se[features, samples]

  # generate sq dist matrix
  d <- ncla::dissim(
    t(SummarizedExperiment::assay(se_f, assay)),
    distfun
  )

  if(max(d) > 1){
    message("Note: dissimilarities > 1 detected, rescaling to [")
    d <- scales::rescale(d, 0, 1)
  }

  # make colour pal
  col_fun = circlize::colorRamp2(
    # seq(0,1, by = 0.25),
    seq(0, ceiling(max(d) * 10)/10, length.out = 6),
    # the ceiling func is just to round up to
    # the nearest 0.1/0.2 ... 0.9 etc
    # scico::scico(6, palette = "hawaii")
    viridis::viridis(6)
  )

  # each legend should be a single row
  annot_legends <- setNames(
    lapply(annots, function(x) list(nrow = 1)),
    annots
    )

  # make annotations for columns
  annot <- ComplexHeatmap::HeatmapAnnotation(
    df = as.data.frame(
      # SummarizedExperiment::colData(se_f)[, c("Tissue", "Clone", "Mouse_ID", "Batch")]
      SummarizedExperiment::colData(se_f)[, annots, drop = F]
    ),
    col = annots_col_list,
    annotation_legend_param = annot_legends,
    which = "column"
  )

  # make annotations for rows
  # don't edit, just copy/paste above and change which = ... ! and show_legend
  row_annot <- ComplexHeatmap::HeatmapAnnotation(
    df = as.data.frame(
      SummarizedExperiment::colData(se_f)[, annots, drop = F]
    ),
    col = annots_col_list,
    annotation_legend_param = annot_legends,
    which = "row",
    show_legend = FALSE,
    show_annotation_name = FALSE
  )



  hmap <- ComplexHeatmap::Heatmap(
    d,
    col = col_fun,
    top_annotation = annot,
    right_annotation = row_annot,
    show_row_names = FALSE,
    show_column_names = FALSE,
    name = title
  )


  ComplexHeatmap::draw(
    hmap,
    annotation_legend_side = "bottom"
  )
}
