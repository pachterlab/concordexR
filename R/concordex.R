#' Identify Spatially Homogeneous Regions with concordex
#'
#' @description Compute the neighborhood consolidation matrix and identify spatial
#' homogeneous regions.
#'
#' @param x A \code{\link{SpatialExperiment}},
#'   \code{SpatialFeatureExperiment},
#'    \code{\link{SingleCellExperiment}}, or \code{\link{SummarizedExperiment}}
#'    object containing a count matrix.
#'
#'    Otherwise, a numeric matrix-like object containing counts for observations
#'    (e.g. cells/spots) on the rows and features on the columns.
#'
#' @param labels Observation labels used to compute the neighborhood consolidation
#'   matrix. Continuous or discrete labels are allowed, and typically, integer
#'   labels are assumed to be discrete.
#'
#'   Labels can be specified as follows:
#'
#'   * A vector or matrix-like object with one entry per observation If a matrix, the
#'   observations should be on the rows and the label identifiers on the columns.
#'
#'   * If \code{x} inherits from \code{\link{SummarizedExperiment-class}}, a
#'   string or character vector specifying the names of columns in \code{colData(x)}
#'   or the name of a dimensionality reduction result (see \link{reducedDimNames})
#'
#' @param n_neighbors Number of neighbors to expect for each observation.
#'   Defaults to 30.
#'
#' @param compute_similarity Logical. Whether to return the label similarity matrix.
#'   Only useful if discrete labels are provided.
#'
#' @param BLUSPARAM A \code{\link{BlusterParam-class}} object specifying the clustering
#'   algorithm to use to identify spatial homogeneous regions. If this parameter
#'   is not specified, then regions are not returned. By default, this parameter
#'   is missing.
#'
#' @param BNPARAM A \code{\link{BiocNeighborParam}} object specifying the algorithm to use.
#' This can be missing if \code{BNINDEX} is supplied, see \link{findKNN}.
#'
#' @param BNINDEX A \code{\link{BiocNeighborIndex}} object containing the precomputed
#'   index information, see \link{findKNN}.
#'
#' @param BPPARAM A \code{\link{BiocParallelParam}} object specifying whether
#'   and how computing the metric for numerous observations shall be
#'   parallelized (see \code{\link{bpparam}}).
#'
#' @return
#' For \code{calculateConcordex}, A list containing a sparse, numeric matrix representing the
#' neighborhood consolidation for each cell (row) and SHR identities.
#'
#' For \code{runConcordex}, a SingleCellExperiment (or SpatialExperiment) object
#' is returned containing this matrix in \code{\link{reducedDims}(..., name)}.
#'
#' @examples
#' example(read10xVisium, "SpatialExperiment")
#' library(bluster)
#'
#' ## Setting BLUSPARAM clusters the consolidation
#' ## matrix into SHRs
#'
#'cdx <- calculateConcordex(
#'   spe, "in_tissue",
#'   n_neighbors=10,
#'   BLUSPARAM=KmeansParam(3)
#' )
#'
#' ## SHRs are an attribute of the result
#'shr <- attr(cdx, "shr")
#'
#' ## The label similarity matrix can be computed
#' ## with `compute_similarity=TRUE`
#'cdx <- calculateConcordex(
#'   spe, "in_tissue",
#'   n_neighbors=10,
#'   compute_similarity=TRUE,
#'   BLUSPARAM=KmeansParam(3)
#' )
#'
#'
#' @export
#' @rdname calculateConcordex
#'
#' @importFrom cli cli_abort cli_warn
#' @importFrom methods setMethod setGeneric callNextMethod as is
#' @importFrom rlang check_dots_empty check_required
#' @importFrom BiocNeighbors findKNN KmknnParam
#' @importFrom BiocParallel SerialParam bplapply

setMethod("calculateConcordex", "ANY",
          function(x, labels,
            ...,
            n_neighbors=30,
            compute_similarity=FALSE,
            BNINDEX,
            BLUSPARAM=NNGraphParam(cluster.fun="louvain"),
            BNPARAM=KmknnParam(),
            BPPARAM=SerialParam()) {

              check_dots_empty()

              check_required(x)
              check_required(labels)

              check_all_numeric(x)
              check_labels(labels, expected=dim(x)[1])

              labels <- labels_make_friendly(labels, nm=dimnames(x)[[1]])

              req_args <- list(
                  x=as.matrix(x),
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

#' @param ... Other parameters passed to default method
#' @param assay.type String or integer scalar indicating the assay of \code{x}
#'   containing the counts.
#'
#' @export
#' @docType methods
#' @rdname calculateConcordex
#'
#' @importFrom SummarizedExperiment assay colData colData<-
setMethod("calculateConcordex", "SummarizedExperiment",
          function(x, labels, ..., assay.type="logcounts") {
              labels <- labels_walk(x, labels)
              calculateConcordex(t(assay(x, i=assay.type)), labels, ...)
          })

#' @param ... Other parameters passed to default method
#' @param use.dimred Integer or string specifying the reduced dimensions to use
#'   for construction of the k-nearest neighbor graph. Note that if this is not
#'   \code{NULL}, reduced dimensions cannot be used as labels to compute the
#'   neighborhood consolidation matrix.
#'
#' @export
#' @docType methods
#' @rdname calculateConcordex
#'
#' @importFrom SingleCellExperiment reducedDim reducedDimNames
setMethod("calculateConcordex", "SingleCellExperiment",
          function(x, labels, ..., use.dimred=NULL) {
              if (!is.null(use.dimred)) {
                  labels <- labels_walk(x, labels, allow.dimred=FALSE)
                  calculateConcordex(reducedDim(x, use.dimred), labels, ...)

              } else {
                  callNextMethod(x=x, labels=labels, ...)
              }
          })

#' @param ... Other parameters passed to default method
#' @param use.spatial Logical, should the spatial coordinates be used to compute the
#' k-nearest neighbor graph?
#'
#' @export
#' @docType methods
#' @rdname calculateConcordex
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

#' @param name String specifying the name to be used to store the result in the
#' \code{\link{reducedDims}} of the output.
#'
#' @export
#' @docType methods
#' @rdname calculateConcordex
#' @importFrom SingleCellExperiment reducedDim<-
setMethod("runConcordex", "SpatialExperiment", function(x, labels, ..., name="NBC")
    {
    nbc <- calculateConcordex(x, labels, ...)

    if ("SHR" %in% names(nbc)) {
        colData["shr"] <- nbc[["SHR"]]
    }
    reducedDim(x, name) <- nbc[["NBC"]]

    x
    })

#' @export
#' @docType methods
#' @rdname calculateConcordex
setMethod("runConcordex", "SingleCellExperiment", function(x, labels, ..., name="NBC")
{
    nbc <- calculateConcordex(x, labels, ...)
    reducedDim(x, name) <- nbc[["NBC"]]
    x
})
