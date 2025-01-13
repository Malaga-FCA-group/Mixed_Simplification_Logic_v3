simplify_key_opt <- function(A, B, C, D, attributes, pos_id, verbose = FALSE) {

  negB <- Matrix(negate(B), sparse = TRUE)

  AsmC <- .difference2(A, C)
  CsmA <- .difference2(C, A)

  A_orig <- A
  B_orig <- B
  C_orig <- C
  D_orig <- D

  if ((sum(AsmC) == 1) && .subset(AsmC, D)[1] &&
      (sum(CsmA) == 1) && .subset(CsmA, negB)[1]) {

    x <- which(as.matrix(AsmC) > 0)
    ybar <- which(as.matrix(CsmA) > 0)
    y <- negate_id(ybar, pos_id)
    B[y] <- 0
    C[ybar] <- 0
    D <- empty_set(nrow(D))
    # D[D == 1] <- 0
    D[y] <- 1

    if (verbose) {

      cat("^ Applied [Key']\n")
      cat("^ Changed A -> B:\n")
      cat("^ from:", .implication_to_string(A_orig, B_orig, attributes), "\n")
      cat("^ to: ", .implication_to_string(A, B, attributes), "\n")
      cat("^ Changed C -> D:\n")
      cat("^ from:", .implication_to_string(C_orig, D_orig, attributes), "\n")
      cat("^ to: ", .implication_to_string(C, D, attributes), "\n")

    }

    return(list(A, B, C, D, TRUE))

  }

  negD <- Matrix(negate(D), sparse = TRUE)

  if ((sum(AsmC) == 1) && .subset(AsmC, negD)[1] &&
      (sum(CsmA) == 1) && .subset(CsmA, B)[1]) {

    x <- which(as.matrix(AsmC) > 0)
    y <- which(as.matrix(CsmA) > 0)
    xbar <- negate_id(x, pos_id)

    D[xbar] <- 0
    A[x] <- 0
    B <- empty_set(nrow(B))
    B[xbar] <- 1

    if (verbose) {

      cat("^ Applied [Key']\n")
      cat("^ Changed A -> B:\n")
      cat("^ from:", .implication_to_string(A_orig, B_orig, attributes), "\n")
      cat("^ to: ", .implication_to_string(A, B, attributes), "\n")
      cat("^ Changed C -> D:\n")
      cat("^ from:", .implication_to_string(C_orig, D_orig, attributes), "\n")
      cat("^ to: ", .implication_to_string(C, D, attributes), "\n")

    }

    return(list(A, B, C, D, TRUE))

  }

  return(list(A, B, C, D, FALSE))

}
