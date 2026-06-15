#' Split a SingleCellExperiment into human and mouse features. 
#' 
#' @details Assumes the ID symbols are the desired rownames
#'
#'
#' @param sce A SingleCellExperiment with `rowData` columns `ID` and `Symbol`
#'   prefixed by genome (e.g. "GRCh38_ENSG..." / "GRCm39_ENSMUSG...")
#'
#' @return A SingleCellExperiment with a `Species` rowData column, prefix-stripped
#'   `ID`/`Symbol`/rownames, human (main) and mouse (altExp) features split
#' @export
split_species <- function(sce){
  rowData(sce)$Species <- dplyr::case_when(
    str_starts(rowData(sce)$ID, "GRCh38") ~ "Human",
    str_starts(rowData(sce)$ID, "GRCm39") ~ "Mouse",
    .default = NA_character_
  )
  stopifnot(!anyNA(rowData(sce)$Species))


  reg <- "^GRC[^_]*_"
  rowData(sce)$ID <- stringr::str_remove(
    rowData(sce)$ID, 
    reg
  )
  rowData(sce)$Symbol <- stringr::str_remove(
    rowData(sce)$Symbol,
    reg
  )
  rownames(sce) <- stringr::str_remove(
    rownames(sce),
    reg
  )

  sce <- SingleCellExperiment::splitAltExps(
    sce,
    rowData(sce)$Species,
    ref = "Human"
  )

  sce
}