#' call_lego_clones
#'
#' Convert legotek sequencing counts into clone identities via thresholding
#'
#' @param sce SingleCellExperiment object
#' @param min_count min number of counts to be considered expressed
#' @param min_frac min fraction of total lego counts (relative to all in the cell) for a tag to be considered expressed
#' @param i the assay of the altExp to use
#' @param altexp the altExp the LeGOteknik data are store in
#'
#' @return a character vector of length `ncol(sce)` with clone assignments
#' @export
#'
call_lego_clones <- function(sce,
                             min_count = 1,
                             min_frac = 0.05,
                             i = "counts",
                             altexp = "Antibody Capture"
                             ){
  # check inputs
  if(min_frac > 1 | min_frac < 0){
    stop("min_frac must be (0,1)")
  }
  if(! altexp %in% SingleCellExperiment::altExpNames(sce)){
    stop("altexp missing")
  }
  if(! i %in% SummarizedExperiment::assayNames(
    SingleCellExperiment::altExp(sce, altexp)
    )){
    stop("assay missing from altExp")
  }

  lego_barcodes <- c("Katushka", "Tomato", "Venus", "Sapphire", "eBFP2")
  # get the matrix of LeGO counts
  lego_mat <- SingleCellExperiment::altExp(sce, altexp) |>
    SummarizedExperiment::assay(i = i) |>
    t()

  if(! all(lego_barcodes %in% colnames(lego_mat))){
    missing_lego <- setdiff(lego_barcodes, colnames(lego_mat))
    stop(
      glue::glue("The LeGO barcode matrix is missing: {missing_lego}")
    )
  }

  # set the tags in the correct order for binary encoding.
  #this is very important!
  lego_mat <- lego_mat[, lego_barcodes]

  # binarise matrix (at threshold)
  bin_mat <- matrix(1, nrow = nrow(lego_mat), ncol = ncol(lego_mat))

  # min count filter
  # blank out counts that are below the specified min_count
  bin_mat[lego_mat <= min_count] <- 0

  # min fraction filter
  # divides each row by it sum to get a fraction, and then blanks out
  # those that are below the min_frac
  bin_mat[(lego_mat / rowSums(lego_mat)) <= min_frac] <- 0

  empty_cells <- sum(rowSums(bin_mat) == 0)
  message(
    glue::glue(
      "{empty_cells} cells ({round(empty_cells/nrow(bin_mat)*100, 2)}%) with no detected LeGO tags above {min_count} count(s)."
    )
  )

  # encode the LeGO tags based on binarised detection
  lego_barcodes <- apply(
    bin_mat,
    1,
    function(x){
      paste0(x, collapse = "") |>
        strtoi(base = 2)
    }
  )

  # n_barcodes
  n_barcodes <- lego_barcodes[lego_barcodes != 0] |>
    unique() |>
    length()

  message(glue::glue("{n_barcodes} different clones detected"))

  return(as.character(lego_barcodes))
}
