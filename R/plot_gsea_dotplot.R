#' Plot a GSEA dotplot for a set of different conditions
#'
#' @param lst a named list of results from `gsea_gene_score()`, with a little cleaning
#' @param column_order the x-axis order, should be names of
#' @param fdr FDR cutoff for significant pathways
#'
#' @return
#' @export
#'
plot_gsea_dotplot <- function(lst, column_order = names(lst), fdr = 0.1){
  if(! all(names(lst) %in% column_order)){
    message("FYI: not all conditions are in column_order")
    message(glue::glue("{setdiff(names(lst), column_order)} will not be plotted"))
  }

  lst <- lst[intersect(names(lst), column_order)]

  column_order <- str_replace_all(column_order, "_", " ")


  lst_tbl <- purrr::list_rbind(lst, names_to = "condition") |>
    dplyr::mutate(
      pathway = str_remove(pathway, "HALLMARK_") |>
        str_replace_all("_", " ") |>
        str_to_title()
    )

  # check what pathways have at least one significant condition
  sig_pathways <- lst_tbl |>
    filter(padj < fdr) |>
    pull(pathway) |>
    unique()

  if(length(sig_pathways) == 0){
    stop(glue::glue("No pathways are significant at an FDR < {fdr}"))
  }

  # generate an ordering of the pathways using hierarch clustering
  clusts <- lst_tbl |>
    dplyr::filter(pathway %in% sig_pathways) |>
    dplyr::select(pathway, condition, NES) |>
    tidyr::pivot_wider(names_from = condition, values_from = NES) |>
    tibble::column_to_rownames(var = "pathway") |>
    as.matrix() |>
    dist() |>
    hclust()

  plotOrder <- clusts$order

  lst_tbl |>
    dplyr::filter(padj < fdr) |>
    dplyr::mutate(
      pathway = forcats::fct_relevel(pathway, clusts$labels[clusts$order]),
      condition = str_replace_all(condition, "_", " ")
    ) |>
    ggplot(
      aes(condition, pathway, colour = NES, size = -log10(padj), label = round(NES, 2))
    ) +
    geom_point() +
    # geom_text(size = 2, colour = "black") +
    ggpubr::theme_pubr(border = TRUE) +
    theme(
      axis.text.x = element_text(
        angle = 45,
        vjust = 1,
        hjust = 1
      ),
      axis.text.y = element_text(
        size = rel(0.75)
      ),
      panel.grid.major.y = element_line(
        colour = "black", linewidth = 0.2, linetype = 3
      ) ,
      legend.position = "right"
    ) +
    scale_size(range = c(3,9)) +
    scale_x_discrete(limits = column_order) +
    scico::scale_color_scico(palette = "cork") +
    labs(
      x = NULL,
      y = NULL,
      # title = "GSEA enrichment of MSigDB Hallmark pathways",
      caption = glue::glue("Points with FDR < {fdr} shown")
    )
}
