#' Iteratively make a matrix sparse
#' 
#' @param x a matrix 
#' 
#' @return a sparse matrix
#' @importFrom Matrix Matrix
#' 
#' @export
sparsify <- function(x) {
  if (prod(dim(x)) < (2^31 - 1)) {
    return(Matrix(data.matrix(x), sparse = TRUE))
  }
  
  halfway <- floor(nrow(x) / 2)
  
  x_s <- rbind(sparsify(x[1:halfway, ]),
               sparsify(x[(halfway+1):nrow(x), ]))
  
  return(x_s)
}