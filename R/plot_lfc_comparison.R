#' Plot Log-Fold Change
#'
#' Given two DGE objects (\code{df1} and \code{df2}) and their corresponding labels (\code{lb1} and \code{lb2}),
#' plots log-fold change and correlation between the two datasets.
#'
#' @param df1 DGELRT object from edgeR
#' @param df2 DGELRT object from edgeR
#' @param lb1 Label for data frame 1.
#' @param lb2 Label for data frame 2.
#'
#' @return A ggplot object showing log-fold change and correlation between datasets.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' plot_lfc(df1, df2, lb1 = "Condition1", lb2 = "Condition2")
#' }
#'
#' @importFrom ggplot2 aes geom_abline geom_smooth geom_point labs scale_colour_manual scale_x_continuous scale_y_continuous coord_equal
#' @importFrom ggpubr theme_pubr
#' @importFrom dplyr bind_rows left_join mutate arrange
#' @importFrom tidyr pivot_wider
#' @importFrom stats cor round
#' @importFrom rlang sym
#' @importFrom forcats fct_relevel
#' @importFrom glue glue
#'
#' @export
plot_lfc <- function(df1,
                     df2,
                     lb1,
                     lb2) {


  dat1 <- dplyr::bind_rows(
    getLFC(df1, lb1),
    getLFC(df2, lb2)
  ) |>
    tidyr::pivot_wider(names_from = label, values_from = logFC)

  cor1 <- cor(
    dat1[[lb1]], dat1[[lb2]],
    method = "pearson",
    use = "complete.obs"
    ) |>
    round(2)


  dat1 |>
    dplyr::left_join(getSignif(df1), by = "ensg") |>
    dplyr::mutate(signif = forcats::fct_relevel(as.factor(signif), "0")) |>
    dplyr::arrange(signif) |>
    ggplot2::ggplot(ggplot2::aes(!!sym(lb1), !!sym(lb2))) +
    ggplot2::geom_abline(slope = 1, intercept = 1, colour = "red", linetype = "dashed") +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x) +
    ggplot2::geom_point(aes(colour = as.character(signif)), shape = 16, alpha = 0.8) +
    ggplot2::scale_colour_manual(name = glue::glue("{lb1} significance"), values = c(`1` = "#74C476", `0` = "black", `-1` = "#9E9AC8")) +
    ggpubr::theme_pubr(border = TRUE) +
    ggplot2::labs(
      x = paste0("LFC:", lb1),
      y = paste0("LFC:", lb2),
      caption = glue::glue("Pearson correlation: {cor1}")
    ) +
    ggplot2::scale_x_continuous(breaks = seq(-10,10,2)) +
    ggplot2::scale_y_continuous(breaks = seq(-10,10,2)) +
    ggplot2::coord_equal()
}

#' Get Log-Fold Change
#'
#' Given a differential expression result (\code{res}) and a label (\code{label}),
#' extracts the log-fold change values for each gene.
#'
#' @param res Differential expression result object.
#' @param label Label to assign to the log-fold change values.
#'
#' @return A data frame containing gene identifiers, log-fold change values, and the specified label.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' getLFC(my_results, label = "Condition1_vs_Condition2")
#' }
#'
#' @importFrom edgeR topTags
#' @importFrom dplyr select mutate
#' @importFrom tibble rownames_to_column
#'
#' @export
getLFC <- function(res, label){
  edgeR::topTags(res, n = Inf) |>
    as.data.frame() |>
    tibble::rownames_to_column("ensg") |>
    dplyr::select(ensg, logFC) |>
    dplyr::mutate(label = label)
}
