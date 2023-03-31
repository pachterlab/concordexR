library(Matrix)

ncells <- 100
nlabels <- 20
knn <- 20

nn <- rep(c(1, 0), c(knn, ncells - knn))

labels <- paste0("label",seq_len(nlabels))
labels <- sample(labels, ncells, replace = TRUE)

mtx <- lapply(seq_len(ncells), function(x){
    nns <- sample(nn)
    if (nns[x] == 1){
        nns[x] <- 0
        inds <- which(nns == 0)
        ind <- inds[inds != x][1]
        nns[ind] <- 1
    }
    nns
})

mtx <- do.call(rbind, mtx)
sp_mtx <- as(mtx, "dgCMatrix")
nms <- c("concordex", "mean_random_concordex", "corrected_trace", "simulated")

test_that("calculateConcordex (matrix/array) returns correct output", {
    res <- calculateConcordex(mtx, labels, return.map = FALSE)
    expect_type(res, "list")
    expect_named(res, nms)

    res2 <- calculateConcordex(mtx, labels, return.map = TRUE)
    expect_type(res2, "list")
    expect_named(res2, c(nms, "map"))
    expect_true(all(vapply(res2, is.numeric, FUN.VALUE = logical(1))))
    expect_true(is.vector(res2[["simulated"]]))
    expect_true(is.matrix(res2[["map"]]))
    expect_equal(rownames(res2[["map"]]), sort(unique(labels)))
    expect_equal(colnames(res2[["map"]]), sort(unique(labels)))
    expect_true(!any(unlist(res2) < 0))
})

test_that("calculateConcordex (dgCMatrix) returns correct output", {
    res <- calculateConcordex(sp_mtx, labels, return.map = FALSE)
    expect_type(res, "list")
    expect_named(res, nms)

    res2 <- calculateConcordex(sp_mtx, labels, return.map = TRUE)
    expect_type(res2, "list")
    expect_named(res2, c(nms, "map"))
    expect_true(all(vapply(res2, is.numeric, FUN.VALUE = logical(1))))
    expect_true(is.vector(res2[["simulated"]]))
    expect_true(is.matrix(res2[["map"]]))
    expect_equal(rownames(res2[["map"]]), sort(unique(labels)))
    expect_equal(colnames(res2[["map"]]), sort(unique(labels)))
    expect_true(!any(unlist(res2) < 0))
})

# Make neighborhood list
nb <- apply(mtx, 1, function(x) which(x > 0))
# nb has 20 rows, 100 columns
nb_df <- as.data.frame(t(nb))
nb_list <- as.list(nb_df)
nb_vec <- as.vector(nb)

test_that("Only (sparse) matrix objects are compatible", {
    expect_error(calculateConcordex(nb_df, labels, k = knn), "must be a matrix")
    expect_error(calculateConcordex(nb_list, labels, k = knn), "must be a matrix")
    expect_error(calculateConcordex(nb_vec, labels, k = knn), "must be a matrix")
})

labels1 <- rep("label", 100)
test_that("Error and messages for wrong number of labels", {
    expect_error(calculateConcordex(sp_mtx, labels = labels1, k = knn),
                 "Must have at least 2 distinct labels")
    expect_error(calculateConcordex(sp_mtx, labels = labels[seq_len(99)], k = knn),
                 "Too few labels supplied")
    expect_error(calculateConcordex(sp_mtx, labels = c(labels, labels[1]), k = knn),
                 "Too many labels supplied")
})

test_that("Number of neighbors (k) is verified", {
    expect_error(calculateConcordex(mtx, labels, k = 2), "orient")
})

test_that("Matrix (re)-orientation is working properly", {
    res1 <- calculateConcordex(sp_mtx, labels, k = knn)
    res2 <- calculateConcordex(t(sp_mtx), labels, k = knn)
    res3 <- calculateConcordex(nb, labels, k = knn)
    res4 <- calculateConcordex(t(nb), labels, k = knn)
    v <- vapply(list(res1, res2, res3, res4), function(x) x[["concordex"]],
                FUN.VALUE = numeric(1))
    # Get the same results
    expect_equal(length(unique(v)), 1L)
    # can't be oriented
    expect_error(calculateConcordex(sp_mtx, labels, k = knn - 1),
                 "Cannot determine")
    expect_error(calculateConcordex(nb, labels, k = knn - 1),
                 "Cannot determine")
})

mtx_self <- mtx
mtx_self[1, which(mtx_self[1, ] > 0)[1]] <- 0
mtx_self[1, 1] <- 1
test_that("`calculateConcordex` identifies self-referential graphs", {
    expect_warning(
        calculateConcordex(mtx_self, labels, k = knn),
        "Some nodes in the graph are self-referential"
    )
})
