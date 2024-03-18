#' Extract gene signatures with the treat test
#'
#' Create and save (to text file) gene signature of a specified contrast using
#' \code{glmTreat}.
#'
#' @param fit \code{DGEGLM} object
#' @param contrast the contrast of interest from the fit object
#' @param name prefix for the output file
#' @param lfc the logFC to test against for \code{glmTreat}
#' @param fdr hard FDR-pvalue cutoff for the gene sets
#' @param out_dir directory to save the gene signatures in
#'
#' @return
#' @export
#'
make_treat_signature <- function(fit, contrast, name, lfc = 1, fdr = 0.05, out_dir = "results/genesets"){


  if(! dir.exists(here::here(out_dir))){
    message(glue::glue("Making {out_dir}"))
    dir.create(here::here(out_dir), recursive = TRUE)
  }

  # do the glmTreat test and create formatted top table
  mydf <- fit |>
    edgeR::glmTreat(contrast = contrast, lfc = lfc) |>
    edgeR::topTags(n = Inf, p.value = 1) |>
    as.data.frame() |>
    tibble::rownames_to_column("ENSG") |>
    dplyr::filter(! is.na(ENSG)) |>
    dplyr::filter(FDR < fdr)


  # extract the up and down reg genes
  genes <- list(
    up = mydf |>
      dplyr::filter(logFC > 0) |>
      pull(ENSG),
    down = mydf |>
      dplyr::filter(logFC < 0) |>
      pull(ENSG)
  )

  # indexed map to access the names
  purrr::imap(
    genes,
    \(x, direc){
      readr::write_lines(
        x,
        glue::glue(
          "{out_dir}/{name}_{direc}_lfc{lfc}_fdr{fdr}_ensg_geneset.txt"
          ) |>
          here::here()
      )
    }
    )


}
