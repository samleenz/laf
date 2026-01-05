#' Plot PCA Pairs
#'
#' Given a summarized experiment (SE) object, creates a series of PCA plots
#'
#' @param se A SummarizedExperiment object.
#' @param assay Name of the assay to use for PCA.
#' @param dimsList A list of length-two vectors specifying dimensions for each PCA plot.
#' @param colour Variable to use for coloring points on the PCA plots.
#' @param shape Variable to use for shaping points on the PCA plots.
#' @param scale_col Scale for the color variable (default is scale_color_discrete()).
#' @param ... aesthetic mappings to pass to 'ggplot2::aes_string()'.
#'
#' @return A patchwork object containing PCA plots.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' plot_pca_pairs(
#'   se,
#'   assay = "RNA",
#'   dimsList = list(c(1,2), c(3,2), c(1,3)),
#'   colour = "Group",
#'   shape = "Condition"
#' )
#' }
#'
#' @importFrom patchwork plot_spacer wrap_plots
#' @importFrom restools plotPCA
#'
#' @import ggplot2
#' @importFrom purrr map
#' @import scales
#' @import tidyr
#' @import dplyr
#' @import checkmate
#' @import rlang
#' @import BiocSingular
#'
#' @export
plot_pca_pairs <- function(
    se,
    assay,
    dimsList = list(c(1:2), c(3,2), c(1,3)),
    colour,
    shape,
    scale_col = ggplot2::scale_color_discrete(),
    ...
){
  # check what PCA backend is being used
  if(BiocSingular::bsparam()@class == "FastAutoParam"){
    message("Warning: Approximate PCA backend being used.\n For consistent results set to ExactParam()")
  }

  # check that dimsList is well formed
  stopifnot(
    "dimsList is not a list" = rlang::is_list(dimsList),
    "All items in dimsList must be length two" = all(purrr::map_lgl(dimsList, \(x) length(x) == 2))
  )

  # create a list of PCA plots
  plots <- purrr::map(
    dimsList,
    function(x){
      restools::plotPCA(
        se,
        dims = x,
        colour = !!rlang::enquo(colour),
        shape = !!rlang::enquo(shape),
        assay = assay,
        size = 3,
        ...
      ) +
        scale_col
    }
  )


  # if the number of plots is odd, add an empty padding area
  if(length(plots) %% 2 == 1){
    plots[[length(plots) + 1]] <- patchwork::plot_spacer()
  }

  patchwork::wrap_plots(plots, guides = "collect")
}
