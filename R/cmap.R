#' Calculate Drug Correlations Using CMap Data
#'
#' This function calculates drug correlations using the Connectivity Map (CMap) dataset.
#' It performs empirical p-value testing through permutation and computes drug correlation scores.
#'
#' @param sig A named list of gene scores, where names correspond to HGNC symbols.
#' @param B Integer. The number of permutations for empirical p-value testing. Default is 10,000.
#' @param ngenes Integer. The number of genes to consider for correlation analysis. Default is 200.
#'
#' @return A data frame with drug correlation scores and empirical p-values. The columns include:
#'   \itemize{
#'     \item \code{drug}: The name of the drug.
#'     \item \code{score}: The correlation score for the drug.
#'     \item \code{pval}: The empirical p-value based on permutations. Minimum p-values is 1/B
#'   }
#'
#' @examples
#' \dontrun{
#'   # Example gene signature
#'   my_sig <- setNames(runif(1000, -1, 1), paste0("Gene", 1:1000))
#'
#'   # Run the function
#'   results <- get_cmap_correlations(sig = my_sig, B = 1000, ngenes = 200)
#'   head(results)
#' }
#'
#' @import ccmap
#' @import ccdata
#' @importFrom purrr map list_cbind
#' @importFrom tibble rownames_to_column
#' @export
get_cmap_correlations <- function(sig, B = 10000, ngenes = 200){

  if(! (rlang::is_installed("ccmap") | rlang::is_installed("ccdata"))){
    stop("Install ccdata and or cmap for this analysis")
  }

  # sig: named list of gene scores, where names are HGNC symbols
  # B: number of permutations for empirical p-value testing
  # data("cmap_es", package = "ccdata")
  require(ccdata)

  query <- ccmap::query_drugs(
    sig,
    cmap_es,
    sorted = FALSE,
    ngenes = ngenes
  )

  perms <- purrr::map(seq(B), \(b){
    ms_i <- setNames(
      sample(sig, replace = FALSE),
      names(sig)
    )

    query_i <- query_drugs(
      ms_i,
      cmap_es,
      sorted = FALSE,
      ngenes = ngenes
    )

    x <- data.frame(
      V1 = abs(query_i) > abs(query), # whether the |permuted score| is > the |score|
      row.names = names(query_i)
    )
    colnames(x) <- paste0("p", b)

    x
  },
  .progress = list(
    type = "iterator",
    format = "Calculating {cli::pb_bar} {cli::pb_percent}",
    clear = TRUE)
  ) |>
    purrr::list_cbind() |>
    apply(1, mean) |> # shorthand for sum(|permuted score| > |score|)/B
    pmax(1/B) # the min p-value is 1/B for an empirical test.

  drug_scores <- data.frame(
    score = query,
    pval = perms
  ) |>
    tibble::rownames_to_column("drug")

  return(drug_scores)
}
