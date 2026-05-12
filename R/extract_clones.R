#' Extract lego (feature barcode) counts from a SingleCellExperiment
#'
#' @param sce A SingleCellExperiment with legotek feature counts.
#' @param altexp The name of the altExp containing the lego counts. Default is "Antibody Capture".
#' @return Modified SingleCellExperiment with lego counts aggregated and stored as an altExp("lego").
#' @details Assumes "Antibody Capture" is the name of the lego altExp.
#' @export
extract_lego_counts <- function(sce, altexp = "Antibody Capture"){
  sce <- SingleCellExperiment::splitAltExps(
    sce,
    SummarizedExperiment::rowData(sce)$Type
  )

  # get the lego altExp
  if (!(altexp %in% SingleCellExperiment::altExpNames(sce))) {
    stop(paste("Alternative experiment", altexp, "not found in sce."))
  }

  lego_sce <- SingleCellExperiment::altExp(sce, altexp)


  colnames(lego_sce) <- lego_sce$Barcode


  # sum across the features per tag
  lego_sce <- scater::aggregateAcrossFeatures(
    lego_sce,
    ids = stringr::str_remove(
      rownames(lego_sce),
      "_.*$"
    ),
    use.assay.type = "counts"
  )
  SummarizedExperiment::colData(lego_sce) <- SummarizedExperiment::colData(sce)

  SingleCellExperiment::altExp(sce, altexp) <- lego_sce
  altexp_names <- SingleCellExperiment::altExpNames(sce)
  altexp_names[altexp_names == altexp] <- "lego"
  SingleCellExperiment::altExpNames(sce) <- altexp_names

  sce
}
