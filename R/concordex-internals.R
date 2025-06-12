#' @importFrom Matrix rowSums t
#' @importFrom DelayedArray rowsum
#' @importFrom BiocGenerics which
.concordex_map <- function(nbc, labels, n_neighbors) {
    # Temporarily collapse labels
    inds <- which(labels==1, arr.ind=TRUE)
    inds <- inds[order(inds[,1]), 2]

    labels <- dimnames(labels)[[2]][inds]

    tally_labs <- table(labels)
    tally_labs <- c(tally_labs)

    out <- rowsum(nbc, labels)

    # denominator is the number of observations for each label
    out <- out / tally_labs[dimnames(out)[[1]]]

    out
}

#' @importFrom purrr pmap
#' @importFrom Matrix colMeans
#' @importFrom sparseMatrixStats colMeans2
.concordex_nbhd_consolidation <- function(
        g, labels,
        ...,
        BPPARAM=SerialParam()) {

    check_dots_empty()
    dims <- dim(g)

    # The index is dense, so we can get some improvements
    # by converting to a list of rows
    g <- split(g, seq(dims[1]))

    nw <- BPPARAM$workers

    .consolidate <- function(rows) {
        mapply(FUN=function(r){colMeans2(labels, rows=r)}, rows, SIMPLIFY=FALSE)
    }

    if (nw <= 1L) {
        # Avoid `bplappy()` for serial
        nbc <- .consolidate(g)
        nbc <- do.call(rbind, nbc)

    } else {
        nbc <- bpvec(g, .consolidate,
                 AGGREGATE = function(...) do.call(rbind, c(...)),
                 BPPARAM=BPPARAM)
    }
    # Transpose so that labels are on columns
    # t(nbc)
    nbc
}

.concordex_stat <- function(nbc, labels, n_neighbors) {

    mapped <- .concordex_map(nbc, labels, n_neighbors=n_neighbors)
    cdx <- mean(diag(mapped))

    list(similarity=mapped, concordex=cdx)
}

#' @importFrom bluster clusterRows
.calculate_concordex <- function(
        x,
        labels,
        ...,
        BLUSPARAM,
        BNINDEX,
        n_neighbors=30,
        compute_similarity=FALSE,
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
    nbc <- .concordex_nbhd_consolidation(g, labels, BPPARAM=BPPARAM)
    out <- list("NBC"=nbc)

    if (cluster_neighborhoods & !missing(BLUSPARAM)) {
        if (class(BLUSPARAM) %in% "MbkmeansParam") {
            shr <- clusterRows(as.matrix(nbc), BLUSPARAM=BLUSPARAM)
        } else {
           shr <- clusterRows(nbc, BLUSPARAM=BLUSPARAM, full=FALSE)
        }

        out[['SHR']] <- shr
    }

    if (compute_similarity) {
        # statistics
        cdx <- .concordex_stat(nbc, labels, n_neighbors=n_neighbors)

        out[["SIMILARITY"]] <- cdx$similarity
        out[["CONCORDEX_SCORE"]] <- cdx$concordex

    }

    out
}
