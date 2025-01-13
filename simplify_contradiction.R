simplify_contradiction <- function(A, B,
                                   attributes,
                                   pos_id,
                                   verbose = FALSE) {

  negB <- negate(B)

  AcapnegB <- A * negB
  BcapnegB <- B * negB
  change <- FALSE

  A_orig <- A
  B_orig <- B

  if (sum(AcapnegB) > 0) {

    x <- which(as.matrix(A) > 0)[1]
    A[x] <- 0
    xbar <- negate_id(x, pos_id)
    B <- empty_set(nrow(B))
    # B[B == 1] <- 0
    B[xbar] <- 1
    change <- TRUE
    if (verbose) {

      cat("^ Applied [Cont']\n")
      cat("^ Changed:\n")
      cat("^  from:", .implication_to_string(A_orig, B_orig, attributes), "\n")
      cat("^  to:", .implication_to_string(A, B, attributes), "\n")

    }

  }

  if (sum(BcapnegB) > 0) {

    if (sum(A) == 0) {

      return(list(A, B, FALSE))

    }

    x <- which(as.matrix(A) > 0)[1]
    A[x] <- 0
    xbar <- negate_id(x, pos_id)
    # B[B == 1] <- 0
    B <- empty_set(nrow(B))
    B[xbar] <- 1
    change <- TRUE
    if (verbose) {

      cat("^ Applied [Cont'']\n")
      cat("^ Changed:\n")
      cat("^  from:", .implication_to_string(A_orig, B_orig, attributes), "\n")
      cat("^  to:", .implication_to_string(A, B, attributes), "\n")

    }

  }
  return(list(A, B, change))

}
