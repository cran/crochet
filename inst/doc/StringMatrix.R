## -----------------------------------------------------------------------------
dim.StringMatrix <- function(x) {
    attr(x, "_dim") # store dimensions in `_dim` attribute
}

length.StringMatrix <- function(x) {
    prod(dim(x)) # rely on `dim()` method above
}

dimnames.StringMatrix <- function(x) {
    attr(x, "_dimnames") # store dim names in `_dimnames` attribute
}

## ---- eval = FALSE------------------------------------------------------------
#  X[seq(1, 25, by = 5)] # subsetting by positive integers
#  X[1, ] # simplifying subsetting by positive integers
#  X[-(2:5), ] # simplifying subsetting by negative integers
#  X[c(TRUE, FALSE, FALSE, FALSE, FALSE), ] # simplifying subsetting by booleans
#  X["row_1", ] # simplifying subsetting by row names (only if dimnames exist)

## -----------------------------------------------------------------------------
`[.StringMatrix` <- crochet::extract(
    extract_vector = function(x, i, ...) { # i are positive integers
        # Reserve output vector
        subset <- vector(mode = "character", length = length(i))
        # Populate output vector
        for (singleIdx in 1:length(i)) {
            subset[singleIdx] <- substr(attr(x, "_data"), i[singleIdx], i[singleIdx])
        }
        # Return output vector
        return(subset)
    },
    extract_matrix = function(x, i, j, ...) { # i and j are positive integers
        # Reserve output matrix
        subset <- matrix(
            data = vector(mode = "character", length = length(i) * length(j)),
            nrow = length(i),
            ncol = length(j)
        )
        # Populate output matrix
        for (colIdx in 1:length(j)) {
            for (rowIdx in 1:length(i)) {
                # two-dimensional index needs to be converted to one-dimensional index
                singleIdx <- crochet:::ijtok(x, i[rowIdx], j[colIdx])
                subset[rowIdx, colIdx] <- substr(attr(x, "_data"), singleIdx, singleIdx)
            }
        }
        # Return output matrix
        return(subset)
    }
)

## -----------------------------------------------------------------------------
# Generate data
n <- 5
p <- 5
alphabet <- c(0:9, letters)
data <- sample(alphabet, replace = TRUE, size = n * p)

# Create object
obj <- list()
class(obj) <- "StringMatrix"
attr(obj, "_dim") <- c(n, p)
attr(obj, "_dimnames") <- list(paste0("row_", 1:n), paste0("col_", 1:p))
attr(obj, "_data") <- paste(data, collapse = "")

# Call some methods
dim(obj)
nrow(obj) # you get this for free by implementing `dim()`
ncol(obj) # you get this for free by implementing `dim()`
length(obj)
dimnames(obj)
rownames(obj) # you get this for free by implementing `dimnames()`
colnames(obj) # you get this for free by implementing `dimnames()`

# Extract some data
obj[seq(1, length(obj), by = p)] # subsetting by positive integers
obj[1, ] # simplifying subsetting by positive integers
obj[-(2:length(obj)), ] # simplifying subsetting by negative integers
obj[c(TRUE, rep_len(FALSE, nrow(obj) - 1)), ] # simplifying subsetting by booleans
obj["row_1", ] # simplifying subsetting by row names (only if dimnames exist)

## -----------------------------------------------------------------------------
`[.StringMatrix` <- crochet::extract(
    extract_vector = function(x, i, ...) { # i are positive integers
        dotdotdot <- list(...)
        # Reserve output vector
        subset <- vector(mode = "character", length = length(i))
        # Populate output vector
        for (singleIdx in 1:length(i)) {
            subset[singleIdx] <- substr(attr(x, "_data"), i[singleIdx], i[singleIdx])
        }
        # Capitalize output
        if (!is.null(dotdotdot$capitalize) && dotdotdot$capitalize) {
            subset <- toupper(subset)
        }
        # Return output vector
        return(subset)
    },
    extract_matrix = function(x, i, j, ...) { # i and j are positive integers
        dotdotdot <- list(...)
        # Reserve output matrix
        subset <- matrix(
            data = vector(mode = "character", length = length(i) * length(j)),
            nrow = length(i),
            ncol = length(j)
        )
        # Populate output matrix
        for (colIdx in 1:length(j)) {
            for (rowIdx in 1:length(i)) {
                # two-dimensional index needs to be converted to one-dimensional index
                singleIdx <- crochet:::ijtok(x, i[rowIdx], j[colIdx])
                subset[rowIdx, colIdx] <- substr(attr(x, "_data"), singleIdx, singleIdx)
            }
        }
        # Capitalize output
        if (!is.null(dotdotdot$capitalize) && dotdotdot$capitalize) {
            subset <- toupper(subset)
        }
        # Return output matrix
        return(subset)
    }
)

## -----------------------------------------------------------------------------
obj[1, ]
obj[1, , capitalize = TRUE]
obj[1, , capitalize = FALSE]

## -----------------------------------------------------------------------------
`[<-.StringMatrix` <- crochet::replace(
    replace_vector = function(x, i, ..., value) { # i are positive integers
        # Perform replacement
        for (singleIdx in 1:length(i)) {
            substr(attr(x, "_data"), i[singleIdx], i[singleIdx]) <- value[singleIdx]
        }
        # Do not forget to return x
        return(x)
    },
    replace_matrix = function(x, i, j, ..., value) { # i and j are positive integers
        # Convert value to matrix for easier indexing
        dim(value) <- c(length(i), length(j))
        # Perform replacement
        for (colIdx in 1:length(j)) {
            for (rowIdx in 1:length(i)) { # two-dimensional index needs to be converted to one-dimensional index
                singleIdx <- crochet:::ijtok(x, i[rowIdx], j[colIdx])
                substr(attr(x, "_data"), singleIdx, singleIdx) <- value[rowIdx, colIdx]
            }
        }
        # Do not forget to return x
        return(x)
    }
)

## -----------------------------------------------------------------------------
obj[1:7] <- "z"
obj[]

