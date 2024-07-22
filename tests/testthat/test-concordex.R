library(Matrix)
library(SFEData)
library(TENxPBMCData)
library(scater)

sce <- TENxPBMCData("pbmc3k")

# Make smaller
sce <- sce[,seq(200)]
sce <- logNormCounts(sce)

sfe <- McKellarMuscleData("small")

# Add some fake reducedDims
reducedDim(sce, "pca") <- matrix(
    rnorm(ncol(sce)*50), ncol=50,
    dimnames=list(NULL,paste0("PC",1:50))
)

test_that("concordex uses spatialCoords() by default for SPE/SFE objects", {
    expect_equal(
        calculateConcordex(sfe, labels="in_tissue"),
        calculateConcordex(spatialCoords(sfe), labels = colData(sfe)[["in_tissue"]])
    )
})

test_that("concordex uses 'logcounts' by default for SCE objects", {
    expect_error(
        calculateConcordex(sce, labels=sample(letters,ncol(sce),TRUE)),
        "'logcounts' not in names"
    )
})

test_that("Similarity matrix is not computed with continuous labels",{
    expect_warning(
        calculateConcordex(sce, "pca", compute_similarity=TRUE),
        "Discrete labels are required")
})
