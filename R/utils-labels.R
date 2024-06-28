# This just does the pulling, checking happens elsewhere
# Need to determine how to handle the "where" problem,
# Pull labels from colData or from reducedDim?
# Solution: Do this upstream. Check if name of "labels"
# is in reducedDims, if so, x is the reducedDim
# otherwise, x is colData
labels_pull <- function(x, labels,...) {

    # Labels can be supplied as a character vector


    vars <- names((x))

    cd_labels <- intersect(labels,vars)

    # Since continuous labels are allowed, how will we handle this?
    # Should continous labels be allowed from colData?
    # Should a single continous label be allowed?
    if (length(labels) > 1L) stop("Only one column in colData please")


    # Return
}

# Search for labels in reducedDims?
labels_walk <- function(x, labels, ...) {

    if (length(labels) == dim(x)[2]) {
        return(identity)
    }
    opt <- if (labels %in% names(reducedDims(x))) 1 else 2

    switch(opt,
      reducedDim,
      colData,
    )
}
