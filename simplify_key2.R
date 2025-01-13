simplify_key2 <- function(A, B, C, D, attributes, pos_id, verbose = FALSE) {

  negC <- negate(C)
  negD <- negate(D)

  AcapD <- A * D
  BcapnegD <- B * negD

  A_orig <- A
  B_orig <- B
  C_orig <- C
  D_orig <- D

  if (sum(BcapnegD) > 0) {

    if ((.subset(C, A)[1]) && (sum(A) > 0)) {

      x <- which(as.matrix(A) > 0)[1]
      A[x] <- 0
      xbar <- negate_id(x, pos_id)
      B <- empty_set(nrow(B))
      # B[B == 1] <- 0
      B[xbar] <- 1
      change <- TRUE
      if (verbose) {

        cat("^ Applied [Key'']\n")
        cat("^ Changed A -> B:\n")
        cat("^  from:", .implication_to_string(A_orig, B_orig, attributes), "\n")
        cat("^  to:", .implication_to_string(A, B, attributes), "\n")
        cat("^ Changed C -> D:\n")
        cat("^  from:", .implication_to_string(C_orig, D_orig, attributes), "\n")
        cat("^  to:", .implication_to_string(C, D, attributes), "\n")

      }

      return(list(A, B, C, D, TRUE))

    } else {

      if ((.subset(A, C)[1]) && (sum(C) > 0)) {

        x <- which(as.matrix(C) > 0)[1]
        C[x] <- 0
        xbar <- negate_id(x, pos_id)
        D <- empty_set(nrow(D))
        # D[D == 1] <- 0
        D[xbar] <- 1
        change <- TRUE
        if (verbose) {

          cat("^ Applied [Key'']\n")
          cat("^ Changed A -> B:\n")
          cat("^  from:", .implication_to_string(A_orig, B_orig, attributes), "\n")
          cat("^  to:", .implication_to_string(A, B, attributes), "\n")
          cat("^ Changed C -> D:\n")
          cat("^  from:", .implication_to_string(C_orig, D_orig, attributes), "\n")
          cat("^  to:", .implication_to_string(C, D, attributes), "\n")

        }

        return(list(A, B, C, D, TRUE))

      }

    }

  }

  return(list(A, B, C, D, FALSE))

}
