#' colData to data frame
#'
#' @param se summarised object object
#'
#' @return dataframe
#' @export
#'
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
#' @param gene_name column in `rowData(se)` containing gene names to use as
#'   rownames
#'
#' @return se, with features in the hgnc gene_name space without duplicates
#' @export
#'
se_2_hgnc <- function(se, gene_name = "gene_name"){
  gene_names <- SummarizedExperiment::rowData(se)[[gene_name]]
  se <- se[!is.na(gene_names), ]

  gene_names <- SummarizedExperiment::rowData(se)[[gene_name]]
  se <- se[! duplicated(gene_names), ]

  SummarizedExperiment::rowData(se)$ensg <- rownames(se)
  rownames(se) <- SummarizedExperiment::rowData(se)[[gene_name]]

  se
}
