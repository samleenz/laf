#' Plot library size with optional fill color
#'
#' Generate a bar plot of library sizes with an optional fill color.
#'
#' @param se SummarizedExperiment object.
#' @param fill Variable used for fill color in the plot. If NULL, no fill color is applied.
#' @param scale_fill A function to customize the fill color scale (optional).
#'
#' @return A ggplot object displaying library sizes.
#'
#' @export
plot_libsize <- function(se, fill = NULL, scale_fill = NULL){
  mean_libSize <- assay(se, "counts") |>
    colSums() |>
    mean()

  assay(se, "counts") |>
    colSums() |>
    cbind(
      SummarizedExperiment::colData(se) |>
        as.data.frame()
    ) |>
    tibble::rownames_to_column(var = "Sample_ID") |>
    dplyr::rename("LibrarySize" = 'colSums(assay(se, "counts"))') |>
    ggplot(
      aes(
        factor(Sample_ID),
        LibrarySize,
        fill = {{ fill }}
      )
    ) +
    geom_hline(yintercept = mean_libSize, linetype = "dashed", colour = "red") +
    geom_col() +
    ggpubr::theme_pubr(border = TRUE) +
    theme(
      axis.text.x = element_text(
        angle = 45,
        vjust = 1,
        hjust = 1
      )
    ) +
    scale_fill +
    labs(
      x = NULL,
      caption = "dashed line shows mean library size"
    )
}
