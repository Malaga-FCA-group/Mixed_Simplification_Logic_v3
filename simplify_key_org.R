simplify_key_orig <- function(A, B, C, D, attributes, pos_id, verbose = FALSE) {

  negC <- negate(C)
  negD <- negate(D)

  BcapnegC <- B * negC

  A_orig <- A
  B_orig <- B
  C_orig <- C
  D_orig <- D

  if ((.subset(A, C)[1]) && (sum(BcapnegC) > 0)) {

    C <- empty_set(nrow(C))
    D <- empty_set(nrow(D))
    # C[C == 1] <- 0
    # D[D == 1] <- 0

    if (verbose) {

      cat("^ Applied [Key]\n")
      cat("^ Changed A -> B:\n")
      cat("^ from:", .implication_to_string(A_orig, B_orig, attributes), "\n")
      cat("^ to: ", .implication_to_string(A, B, attributes), "\n")
      cat("^ Changed C -> D:\n")
      cat("^ from:", .implication_to_string(C_orig, D_orig, attributes), "\n")
      cat("^ to: ", .implication_to_string(C, D, attributes), "\n")

    }

    return(list(A, B, C, D, TRUE))

  }

  AcapnegD <- A * negD

  if ((.subset(C, A)[1]) && (sum(AcapnegD) > 0)) {

    A <- empty_set(nrow(A))
    B <- empty_set(nrow(B))
    # A[A == 1] <- 0
    # B[B == 1] <- 0

    if (verbose) {

      cat("^ Applied [Key]\n")
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
