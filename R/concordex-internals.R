#' @importFrom Matrix rowSums t
#' @importFrom DelayedArray rowsum
#' @importFrom BiocGenerics which
.concordex_map <- function(nbc, labels, n_neighbors) {
    # Temporarily collapse labels
    labels <- which(labels==1, arr.ind=TRUE)[,'col']
    tally_labs <- table(labels)

    nbc <- nbc
    out <- rowsum(nbc, labels)

    # denominator is the number of observations for each label
    out <- out / c(tally_labs[dimnames(out)[[1]]])

    dimnames(out)[[2]] <- dimnames(nbc)[[1]]

    out
}

#' @importFrom Matrix colMeans
#' @importFrom sparseMatrixStats colMeans2
.concordex_nbhd_consolidation <- function(
        g, labels,
        ...,
        BPPARAM=SerialParam()) {

    rlang::check_dots_empty()
    dims <- dim(g)


    # The graph is dense, so we can get some improvements on mapping
    # by converting to a list of rows
    g <- purrr::pmap(data.frame(g), c)

    nbc <- bplapply(g, function(rows) {
        nbx <- sparseMatrixStats::colMeans2(labels, rows=rows)
        as(nbx,"sparseMatrix")},
    BPPARAM=BPPARAM)

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
        if (class(BLUSPARAM) %in% "MbkmeansParam") {
            shr <- bluster::clusterRows(as.matrix(nbc), BLUSPARAM=BLUSPARAM)
        } else {
           shr <- bluster::clusterRows(nbc, BLUSPARAM=BLUSPARAM)
        }

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
