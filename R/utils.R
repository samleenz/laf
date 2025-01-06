#' colData to data frame
#'
#' @param se summarised object object
#'
#' @return dataframe
#' @export
#'
#' @examples
cd_2_df <- function(se){
  # take a summarised experiment object and return the colData as a data.frame
  # with the sample names in a column
  if("sample" %in% colnames(SummarizedExperiment::colData(se))){
    warning("sample already in colData, added rownames to `rowname` instead")
    nme <- "rowname"
  } else {
    nme <- "sample"
  }

  colData(se) |>
    as.data.frame() |>
    rownames_to_column(nme)
}


#' Convert SE object to gene-names from ENSG IDs
#'
#' @param se summarised object object
#'
#' @return se, with features in the hgnc gene_name space without duplicates
#' @export
#'
#' @examples
se_2_hgnc <- function(se){
  se <- se[!is.na(SummarizedExperiment::rowData(se)$gene_name), ]

  se <- se[! duplicated(SummarizedExperiment::rowData(se)$gene_name), ]

  SummarizedExperiment::rowData(se)$ensg <- rownames(se)
  rownames(se) <- SummarizedExperiment::rowData(se)$gene_name

  se
}
