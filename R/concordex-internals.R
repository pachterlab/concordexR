#' @importFrom Matrix rowSums t
#' @importFrom DelayedArray rowsum
#' @importFrom BiocGenerics which
.concordex_map <- function(nbc, labels, n_neighbors) {
    # Temporarily collapse labels
    labels <- which(labels==1, arr.ind=TRUE)[,'col']

    nbc <- nbc*n_neighbors
    out <- rowsum(nbc, labels)

    # denominator is the number of observations for each label
    out <- out / rowSums(out)
    dimnames(out)[[1]] <- dimnames(out)[[2]]

    out
}

#' @importFrom Matrix colMeans
.concordex_nbhd_consolidation <- function(
        g, labels,
        ...,
        BPPARAM=SerialParam()) {

    rlang::check_dots_empty()
    dims <- dim(g)

    # Compute for each row(/spot/cell)
    nbc <- bplapply(seq_len(dims[1]), function(row) {
        nbx <- colMeans(labels[g[row,],])
        as(nbx,"sparseMatrix")}, BPPARAM=BPPARAM)

    nbc <- do.call(cbind, nbc)

    # Transpose so that labels are on columns
    t(nbc)

}

.concordex_stat <- function(nbc, labels, n_neighbors) {

    mapped <- .concordex_map(nbc, labels, n_neighbors=n_neighbors)
    cdx <- mean(diag(mapped))

    list(similarity=mapped, concordex=cdx)
}

.calculate_concordex <- function(
        x,
        labels,
        ...,
        n_neighbors=30,
        compute_similarity=FALSE,
        BLUSPARAM,
        BNINDEX,
        BNPARAM=KmknnParam(),
        BPPARAM=SerialParam(),
        precomputed=FALSE,
        cluster_neighborhoods=FALSE) {

    check_dots_empty()

    if (precomputed & !missing(BNINDEX)) {

        g <- findKNN(
            k=n_neighbors,
            warn.ties=FALSE,
            get.distance=FALSE,
            BNINDEX=BNINDEX,
            BPPARAM=BPPARAM
        )

    } else {
        g <- findKNN(
            x,
            k=n_neighbors,
            warn.ties=FALSE,
            get.distance=FALSE,
            BNPARAM=BNPARAM,
            BPPARAM=BPPARAM
        )
    }

    g <- g$index

    # Compute for each row(/cell/spot) in g
    nbc <- .concordex_nbhd_consolidation(g, labels,BPPARAM=BPPARAM)

    if (cluster_neighborhoods & !missing(BLUSPARAM)) {
        shr <- bluster::clusterRows(nbc, BLUSPARAM=BLUSPARAM)
        attr(nbc, "shrs") <- shr
    }

    if (compute_similarity) {
        # statistics
        cdx <- .concordex_stat(nbc, labels, n_neighbors=n_neighbors)
        attr(nbc, "similarity") <- cdx$similarity
        attr(nbc, "concordex") <- cdx$concordex
    }

    nbc
}
