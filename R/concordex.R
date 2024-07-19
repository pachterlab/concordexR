#' Identify Spatially Homogeneous Regions with concordex
#'
#' @description Compute the raw and corrected concordex coefficient using a
#'   neighborhood graph and observation labels.
#'
#' @param x A \code{\link{SpatialExperiment}},
#'   \code{\link{SpatialFeatureExperiment}},
#'    \code{\link{SingleCellExperiment}}, or \code{\link{SummarizedExperiment}}
#'    object containing a count matrix.
#'
#'    Otherwise, a numeric matrix-like object containing counts for cells
#'    (or spots) in the columns and features in the rows.
#' @param labels Cell/spot labels used to compute the neighborhood consolidation
#'   matrix. Continuous or discrete labels are allowed, and typically, integer
#'   labels are assumed to be discrete.
#'
#'   Labels can be specified as follows:
#'
#'   * A vector or matrix-like object with one entry per cell. If a matrix, the
#'   cells should be on the rows and the label identifiers on the columns.
#'
#'   * If \code{x} inherits from \code{\link{SummarizedExperiment-class}}, a
#'   string or character vector specifying the names of columns in \code{colData(x)}
#'   or the name of a dimensionality reduction result (see \link{reducedDimNames})
#'
#' @param n_neighbors Number of neighbors to expect for each observation.
#'   Defaults to 30.
#' @param compute_similarity Logical. Whether to return the label similarity matrix.
#'   Only useful if discrete labels are provided.
#' @param BLUSPARAM A \code{\link{BlusterParam}} object specifying the clustering
#'   algorithm to use to identify spatial homogeneous regions. If this parameter
#'   is not specified, then regions are not returned. By default, this parameter
#'   is missing.
#' @param BNINDEX A \code{\link{BiocNeighborIndex}} object containing the precomputed
#'   index information.
#' @param BPPARAM A \code{\link{BiocParallelParam}} object specifying whether
#'   and how computing the metric for numerous observations shall be
#'   parallelized (see \code{\link{bpparam}}).
#' @returns A sparse matrix
#'
#' @export
#' @rdname calculateConcordex
#'
#' @importFrom cli cli_abort cli_warn
#' @importFrom methods setMethod setGeneric
#' @importFrom rlang check_dots_empty check_required
#' @importFrom BiocNeighbors findKNN KmknnParam
#' @importFrom BiocParallel SerialParam bplapply

setMethod("calculateConcordex", "ANY",
          function(x, labels,
            ...,
            n_neighbors=30,
            compute_similarity=FALSE,
            BLUSPARAM,
            BNINDEX,
            BNPARAM=KmknnParam(),
            BPPARAM=SerialParam()) {

              check_dots_empty()

              check_required(x)
              check_required(labels)

              check_all_numeric(x)
              check_labels(labels, expected=dim(x)[1])

              labels <- labels_make_friendly(labels, nm=dimnames(x)[[1]])

              req_args <- list(
                  x=x,
                  labels=labels,
                  n_neighbors=n_neighbors,
                  compute_similarity=compute_similarity,
                  BNPARAM=BNPARAM,
                  BPPARAM=BPPARAM)

              other_args <- list()

              if (!is_discrete_labels(labels) & compute_similarity) {
                  req_args['compute_similarity'] <- FALSE
                  warn_no_call("Discrete labels are required to compute the similarity matrix.")
              }

              # More argument checking
              if (!missing(BNINDEX)) {
                  other_args[["BNINDEX"]] <- BNINDEX
                  other_args[['precomputed']] <- TRUE
              }
              if (!missing(BLUSPARAM)) {
                  other_args[["BLUSPARAM"]] <- BLUSPARAM
                  other_args[['cluster_neighborhoods']] <- TRUE
              }

              all_args <- c(req_args, other_args)
              do.call(.calculate_concordex, all_args)
          })

#' @param ... Other parameters passed to methods
#' @param assay.type String or integer scalar indicating the assay of \code{x}
#'   containing the counts.
#'
#' @export
#' @docType methods
#' @rdname calculateConcordex
#'
#' @importFrom SummarizedExperiment assay
setMethod("calculateConcordex", "SummarizedExperiment",
          function(x, labels, ..., assay.type="logcounts") {
              labels <- labels_walk(x, labels)
              calculateConcordex(t(assay(x, i=assay.type)), labels, ...)
          })

#' @param ... Other parameters passed to methods
#' @param use.dimred Integer or string specifying the reduced dimensions to use
#'   for construction of the k-nearest neighbor graph. Note that if this is not
#'   \code{NULL}, reduced dimensions can not be used as labels to compute the
#'   neighborhood consolidation matrix.
#'
#' @export
#' @docType methods
#' @rdname calculateConcordex
#'
#' @importFrom SingleCellExperiment reducedDim
setMethod("calculateConcordex", "SingleCellExperiment",
          function(x, labels, ..., use.dimred=NULL) {
              if (!is.null(use.dimred)) {
                  labels <- labels_walk(x, labels, allow.dimred=FALSE)
                  calculateConcordex(reducedDim(x, use.dimred), labels, ...)

              } else {
                  callNextMethod(x=x, labels=labels, ...)
              }
          })


#' @param ... Other parameters passed to methods
#' @use.spatial Logical, should the spatial coordinates be used to compute the
#' k-nearest neighbor graph?
#'
#' @export
#' @docType methods
#' @rdname calculateConcordex
#'
#' @importFrom SpatialExperiment spatialCoords
setMethod("calculateConcordex", "SpatialExperiment",
          function(x, labels, ..., use.spatial=TRUE) {

              labels <- labels_walk(x, labels)

              if (use.spatial) {
                  calculateConcordex(spatialCoords(x), labels=labels, ...)
              } else {
                  callNextMethod(x=x, labels=labels, ...)
              }
          })
