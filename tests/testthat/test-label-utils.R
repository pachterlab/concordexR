library(SFEData)
library(TENxPBMCData)

sce <- TENxPBMCData("pbmc3k")
sfe <- McKellarMuscleData("small")

# Add some fake reducedDims
reducedDim(sce, "pca") <- matrix(
    rnorm(ncol(sce)*50), ncol=50,
    dimnames=list(NULL,paste0("PC",1:50))
)

##### Tests for pulling

test_that("Error when labels are not found in the object", {
    expect_error(labels_walk(sfe, "False Labels"), "not found")
    expect_error(labels_walk(sce, "False Labels"), "not found")
})

test_that("All 'found' labels are pulled from colData SFE/SCE objects", {
    expect_named(
        labels_walk(sce, c("Barcode_type","Chemistry")),
        c("Barcode_type","Chemistry"))
    expect_named(labels_walk(sfe, c("sample_id","barcode")),
        c("sample_id","barcode"))
    expect_named(labels_walk(sfe, c("False Labels", "sample_id","barcode")),
        c("sample_id","barcode"))
})

test_that("reducedDim labels are pulled with an all-or-nothing logic", {
    expect_error(labels_walk(sce, "PC1"), "not found")
    expect_identical(labels_walk(sce,"pca"), reducedDim(sce,"pca"))
})

test_that("Labels can be provided as a vector of length == ncol(x)", {
    expect_vector(
        labels_walk(sce, sample(letters, ncol(sce), TRUE)),
        ptype=character() ,size=ncol(sce))
    expect_vector(
        labels_walk(sfe, sample(1:10, ncol(sfe), TRUE)),
        ptype=integer(), size=ncol(sfe))
    expect_vector(
        labels_walk(sce, sample(1:10, ncol(sce), TRUE)),
        ptype=integer(), size=ncol(sce))
})

test_that("Labels can be provided as an array-like with nrow(labels)==ncol(x)", {
    expect_identical(
        labels_walk(sce, test_labels<-matrix(3,nrow=ncol(sce),ncol=2)),
        test_labels)
    expect_identical(
        labels_walk(sfe, test_labels<-matrix(3,nrow=ncol(sfe),ncol=2)),
        test_labels)
    expect_identical(
        labels_walk(sce, test_labels<-data.frame(matrix(3,nrow=ncol(sce),ncol=2))),
        test_labels)
    expect_identical(
        labels_walk(sfe, test_labels<-data.frame(matrix(3,nrow=ncol(sfe),ncol=2))),
        test_labels)
})

#### Label validation

labels_incompatible <- data.frame(a=letters,b=rnorm(26))
labels_compatible <- data.frame(a=letters,b=1:26, c=logical(26))
labels_compatible2 <- data.frame(a=rnorm(2), b=rnorm(26))

test_that("Label types are identified correctly", {
    expect_match(labels_guess_type(c(1,3,4)), "discrete_vector")
    expect_match(labels_guess_type(c(1.5,3.5,4.5)), "continuous_vector")
    expect_match(labels_guess_type(labels_incompatible),'mixed_incompatible_multi')
    expect_match(labels_guess_type(labels_compatible),'mixed_compatible_multi')
    expect_match(labels_guess_type(labels_compatible2),'continuous_multi')
})

### Label rectangling
test_that("Discrete (but not continuous) labels are one-hot-encoded", {
    expect_s4_class(labels_make_friendly(labels_compatible),"dgCMatrix")
    expect_s4_class(labels_make_friendly(letters),"dgCMatrix")
    expect_s4_class(labels_make_friendly(1:10),"dgCMatrix")
    expect_equal(labels_make_friendly(labels_compatible2), labels_compatible2,
        ignore_attr=TRUE)
    expect_equal(labels_make_friendly(test_labels <- rnorm(30)), test_labels,
                 ignore_attr=TRUE)
})

