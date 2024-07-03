if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("concordexR")

# If you don't have the `glue` package installed, you'll need to uncomment the
# line of code below and install the package. You only need to do this once

# install.packages("glue")

library(glue)
library(concordexR)
library(Matrix)

set.seed(30)

# Feel free to update these numbers as you explore the function
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
mtx2 <- t(mtx)
sp_mtx <- as(mtx, "dgCMatrix")

mtx3 <- lapply(seq_len(ncells), function(x){
    which(as.logical(mtx[x,]))
})
mtx3 <- do.call(rbind, mtx3)

# If you need help with the `calculateConcordex()` function uncomment the line
# of code below

# help("calculateConcordex")

# Please compute concordex for mtx, sp_mtx, mtx2, and mtx3. Print the concordex
# statistic and corrected concordex. Record any errors or points of confusion
# that you encounter.

# Here's a sketch of the code..
res <- calculateConcordex(...)

# Print concordex statistic
glue("The concordex statistic for mtx2 is {res$concordex}")
glue("The concordex ratio for mtx2 is {res$corrected_concordex}")


