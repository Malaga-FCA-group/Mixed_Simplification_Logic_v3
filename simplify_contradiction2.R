simplify_contradiction2 <- function(A, B, C, D,
                                    attributes,
                                    pos_id,
                                    verbose = FALSE) {

  change <- FALSE
  A_orig <- A
  B_orig <- B
  C_orig <- C
  D_orig <- D

  CD <- .union2(C, D)
  negCD <- negate(CD)

  BcapnegCD <- B * negCD

  if ((sum(C) > 0) && .subset(A, CD)[1] && (sum(BcapnegCD) > 0)) {

    x <- which(as.matrix(C) > 0)[1]
    C[x] <- 0
    xbar <- negate_id(x, pos_id)
    # D[D == 1] <- 0
    D <- empty_set(nrow(D))
    D[xbar] <- 1
    change <- TRUE
    if (verbose) {

      cat("^ Applied [Cont''']\n")
      cat("^ Changed C -> D:\n")
      cat("^  from:", .implication_to_string(C_orig, D_orig, attributes), "\n")
      cat("^  to:", .implication_to_string(C, D, attributes), "\n")

    }

    return(list(A, B, C, D, TRUE))

  }

  AB <- .union2(A, B)
  negAB <- negate(AB)
  DcapnegAB <- D * negAB

  if ((sum(A) > 0) && .subset(C, AB)[1] && (sum(DcapnegAB) > 0)) {

    x <- which(as.matrix(A) > 0)[1]
    A[x] <- 0
    xbar <- negate_id(x, pos_id)
    B <- empty_set(nrow(B))
    # B[B == 1] <- 0
    B[xbar] <- 1
    change <- TRUE
    if (verbose) {

      cat("^ Applied [Cont''']\n")
      cat("^ Changed A -> B:\n")
      cat("^  from:", .implication_to_string(A_orig, B_orig, attributes), "\n")
      cat("^  to:", .implication_to_string(A, B, attributes), "\n")

    }

    return(list(A, B, C, D, TRUE))

  }

  return(list(A, B, C, D, FALSE))

}
