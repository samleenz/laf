#' Create an alluvial plot of colData characteristics for an SE object
#'
#' @param se SummarizedExperiment object
#' @param vars unquoted vector of columns from colData to plot
#'
#' @return ggplot plot object
#' @export
#'
plot_coldata_alluvial <- function(se, vars){

  # select the first variable and derive axis
  # breaks from this
  breaks <- se |>
    SummarizedExperiment::colData() |>
    as.data.frame() |>
    dplyr::select( {{ vars }}) |>
    dplyr::select(1) |>
    table() |>
    unname() |>
    rev() |>
    cumsum()

  se |>
    SummarizedExperiment::colData() |>
    as.data.frame() |>
    dplyr::summarise(samples = n(), .by = {{ vars }}) |>
    ggalluvial::to_lodes_form({{ vars }} ) |>
    ggplot(
      aes(x, samples, alluvium = alluvium, stratum = stratum, fill = stratum)
    ) +
    ggalluvial::geom_flow() +
    ggalluvial::geom_stratum() +
    geom_label(
      stat = ggalluvial::StatStratum,
      aes(label = after_stat(stratum)),
      fill = "white",
      alpha = 0.5
    ) +
    scale_x_discrete(expand = c(0.05, 0.05)) +
    scale_y_continuous(breaks = breaks) +
    ggpubr::theme_pubr(border = FALSE, legend = "bottom") +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid = element_blank(),
      axis.text = element_text(colour = "black"),
      legend.position = "bottom"
    ) +
    labs(x = NULL, y = NULL)
}
