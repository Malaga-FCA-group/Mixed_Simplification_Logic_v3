#microbenchmark::microbenchmark(
#  union = .union(A, B),
#  otro = A[B > A] <- 1,
#  otro2 = .union2(A, B))
#
#microbenchmark::microbenchmark(
#  ext = .extract_column(LHS, 139),
#  otro = Matrix(LHS[, 139], sparse = TRUE),
#  otro2 = .ext_col(LHS, 139)
#)
#
#microbenchmark::microbenchmark(
#  suma = sum(A) > 0,
#  lengt = cardinal(A) > 0
#)
#
#microbenchmark::microbenchmark(
#  suma = sum(A) == 1,
#  lengt = cardinal(A) == 1
#)
#
library(Matrix)

cardinal <- function(A) {

  length(A@i)

}

#microbenchmark::microbenchmark(
#  uno = G <- A * 0,
#  dos = {G <- A; G[G == 1] <- 0}
#)

.ext_col <- function(M, j) {

  i <- M@i + 1
  ends <- c(M@p[j] + 1, M@p[j + 1])

  if (ends[1] > ends[2]) {

    return(Matrix::Matrix(0,
                          nrow = M@Dim[1],
                          ncol = 1,
                          sparse = TRUE))

  }

  idx <- seq(ends[1], ends[2])
  x <- M@x

  M2 <- M
  M2@i <- M2@i[idx]
  M2@x <- M2@x[idx]
  M2@p <- c(M@p[j], M@p[j + 1]) - M@p[j]
  M2@Dim[2] <- 1L
  M2@Dimnames[2] <- "1"

  return(M2)
}

empty_set <- function(rows) {

  Matrix::Matrix(0, nrow = rows, ncol = 1, sparse = TRUE)

}

.union2 <- function(A, B) {

  A2 <- A
  A2[B == 1] <- 1

  return(A2)

}

.union2 <- function(A, B) {

  Ai <- A@i
  Bi <- B@i

  ii <- unique(sort(c(Ai, Bi)))
  A2 <- A
  A2@i <- ii
  A2@p <- c(0L, as.integer(length(ii)))
  A2@x <- rep(1, length(ii))
  # Matrix::sparseVector(x = 1, i = ii, length = dim(A)[1])
  # build_sparse_matrix(ii, p = c(0, length(ii)), x = rep(1, length(ii)), dims = dim(A))

  return(A2)

}

microbenchmark::microbenchmark(.union2(A, B), .union22(A, B))

