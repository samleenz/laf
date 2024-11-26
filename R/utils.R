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
