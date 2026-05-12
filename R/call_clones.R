#' call_lego_clones
#'
#' Convert legotek BSVTK sequencing counts into clone identities via thresholding. 
#'
#' @param sce SingleCellExperiment object
#' @param min_count min number of counts to be considered expressed. Either a
#'   single value used for all barcodes, or a length five vector matching the
#'   LeGO barcode order. The order for this and the min_frac parameter is 
#'   Katushka, Tomato, Venus, Sapphire, eBFP2.
#' @param min_frac min fraction of total lego counts (relative to all in the
#'   cell) for a tag to be considered expressed. Either a single value used for
#'   all barcodes, or a length five vector matching the LeGO barcode order.
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
  lego_barcodes <- c("Katushka", "Tomato", "Venus", "Sapphire", "eBFP2")

  # check inputs
  if(! length(min_count) %in% c(1, length(lego_barcodes))){
    stop("min_count must be a single value or a length 5 vector")
  }
  if(! length(min_frac) %in% c(1, length(lego_barcodes))){
    stop("min_frac must be a single value or a length 5 vector")
  }
  if(any(min_frac > 1 | min_frac < 0)){
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

if(length(min_count) == 1){
  min_count <- rep(min_count, length(lego_barcodes))
}
if(length(min_frac) == 1){
  min_frac <- rep(min_frac, length(lego_barcodes))
}


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
  mask_min <- sweep(lego_mat, 2, min_count, `<=`)

  # min fraction filter
  # divides each row by it sum to get a fraction, and then blanks out
  # those that are below the min_frac
  mask_frac <- sweep(lego_mat / rowSums(lego_mat), 2, min_frac, `<=`)

  # if either filter fails (AKA the mask is TRUE), set value to 0
  bin_mat[mask_min | mask_frac] <- 0

  empty_cells <- sum(rowSums(bin_mat) == 0)
  message(
    glue::glue(
      "{empty_cells} cells ({round(empty_cells/nrow(bin_mat)*100, 2)}%) with no detected LeGO tags above the min count(s)."
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
