simplify_gen2 <- function(A, B, C, D, attributes, pos_id, verbose = FALSE) {

  change <- FALSE
  A_orig <- A
  B_orig <- B
  C_orig <- C
  D_orig <- D

  AB <- .union2(A, B)
  diff <- .difference2(C, AB)
  negD <- Matrix::Matrix(negate(D), sparse = TRUE)
  neg_inter <- negD * AB
  inter <- negate(neg_inter)
  interset <- neg_inter

  if ((sum(diff) == 1) && (sum(inter) > 0)) {

    inter <- which(inter > 0)

    x <- which(diff > 0)[1]
    xbar <- negate_id(x, pos_id)

    change <- FALSE
    # if (B[xbar] > 0) change <- FALSE
    #
    P <- c()

    for (d in inter) {

      Cnotxdbar <- C
      Cnotxdbar[x] <- 0
      dbar <- negate_id(d, pos_id)
      Cnotxdbar[dbar] <- 1

      if (.subset(A, Cnotxdbar)[1]) {

        P <- c(P, d)

        # D[d] <- 0
        # change <- TRUE

      }

    }

    # if (length(P) > 0) {
    #
    #   change <- TRUE
    #   D[P] <- 0
    #
    # }

    if ((length(P) > 1) || (length(P) == sum(D))) {

      change <- TRUE
      D[P] <- 0

    }

    if (change && A[xbar] == 0 && B[xbar] == 0) {

      B[xbar] <- 1

    }

    if (change && verbose) {

      cat("^ Applied [MixGen']\n")
      cat("^ Changed A -> B:\n")
      cat("^  from:", .implication_to_string(A_orig, B_orig, attributes), "\n")
      cat("^  to:", .implication_to_string(A, B, attributes), "\n")
      cat("^ Changed C -> D:\n")
      cat("^  from:", .implication_to_string(C_orig, D_orig, attributes), "\n")
      cat("^  to:", .implication_to_string(C, D, attributes), "\n")
      cat("^  -> since x = ", attributes[x], "and P = {",
          stringr::str_flatten(attributes[P], ", "), "}\n")

    }

    return(list(A, B, C, D, change))

  }

  # Reverse
  CD <- .union2(C, D)
  diff <- .difference2(A, CD)
  negB <- Matrix::Matrix(negate(B), sparse = TRUE)
  neg_inter <- negB * CD
  inter <- negate(neg_inter)
  interset <- neg_inter

  if ((sum(diff) == 1) && (sum(inter) > 0)) {

    inter <- which(inter > 0)

    x <- which(diff > 0)[1]
    xbar <- negate_id(x, pos_id)

    change <- FALSE
    # if (B[xbar] > 0) change <- FALSE
    #
    P <- c()

    for (d in inter) {

      Anotxdbar <- A
      Anotxdbar[x] <- 0
      dbar <- negate_id(d, pos_id)
      Anotxdbar[dbar] <- 1

      if (.subset(C, Anotxdbar)[1]) {

        P <- c(P, d)

        # D[d] <- 0
        # change <- TRUE

      }

    }

    # if (length(P) > 0) {
    #
    #   change <- TRUE
    #   D[P] <- 0
    #
    # }

    if ((length(P) > 1) || (length(P) == sum(B))) {

      change <- TRUE
      B[P] <- 0

    }

    if (change && C[xbar] == 0 && D[xbar] == 0) {

      D[xbar] <- 1

    }

    if (change && verbose) {

      cat("^ Applied [MixGen']\n")
      cat("^ Changed A -> B:\n")
      cat("^  from:", .implication_to_string(A_orig, B_orig, attributes), "\n")
      cat("^  to:", .implication_to_string(A, B, attributes), "\n")
      cat("^ Changed C -> D:\n")
      cat("^  from:", .implication_to_string(C_orig, D_orig, attributes), "\n")
      cat("^  to:", .implication_to_string(C, D, attributes), "\n")
      cat("^  -> since x = ", attributes[x], "and P = {",
          stringr::str_flatten(attributes[P], ", "), "}\n")

    }

    return(list(A, B, C, D, change))

  }
  #


  return(list(A, B, C, D, FALSE))

}
