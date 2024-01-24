#' Calculate TMM-normalized effective library sizes
#'
#' Given a summarized experiment object, this function generates TMM-normalized
#' size factors, also known as "effective library sizes".
#'
#' @param se SummarizedExperiment object.
#' @return A numeric vector of TMM-normalized effective library sizes.
#' @export
#'
#'
#' @import edgeR
#' @keywords internal
eff_libsize <- function(se){
  # given a summarized experiment object generate TMM-normalised size factors
  # e.g., "effective library sizes"
  dge <- edgeR::calcNormFactors(se, method = "TMM")

  dge$samples$lib.size * dge$samples$norm.factors
}
