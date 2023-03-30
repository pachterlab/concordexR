library(Matrix)

# set.seed(526)

ncells <- 100
nlabels <- 20
knn <- 20

nn <- rep(c(1,0), c(knn,ncells-knn))

labels <- paste0("label",1:nlabels)
labels <- sample(labels, ncells, replace = TRUE)

# names(labels) <- paste0("cell_", 1:ncells)

mtx <- lapply(1:ncells, function(x){
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


test_that("calculateNomap (matrix/array) returns a list of length 3L", {
  expect_type(calculateNomap(mtx, labels), "list")
  expect_length(calculateNomap(mtx, labels), 3)
})

test_that("calculateNomap (dgCMatrix) returns a list of length 3L", {
  expect_type(calculateNomap(sp_mtx, labels), "list")
  expect_length(calculateNomap(sp_mtx, labels), 3)
})

# test_that("Matrix (re)-orientation is working properly",)
# test_that("Only (sparse) matrix objects are compatible", {expect_error()})
# test_that("`calculateNomap` identifies self-referential graphs")
# test_that("`n.iter` parameter is correctly verified")
