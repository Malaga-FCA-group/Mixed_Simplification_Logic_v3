simplify_simp <- function(A, B, C, D, attributes, pos_id, verbose = FALSE) {

  change <- FALSE
  A_orig <- A
  B_orig <- B
  C_orig <- C
  D_orig <- D

  negC <- negate(C)
  negA <- negate(A)
  AcapnegC <- A * negC
  BcapnegC <- B * negC
  CcapnegA <- C * negA
  DcapnegA <- D * negA

  if ((sum(AcapnegC) > 0) && (sum(BcapnegC) > 0)) {

    x_candidates <- which(as.matrix(AcapnegC) > 0)
    y_candidates <- which(as.matrix(BcapnegC) > 0)

    for (x in x_candidates) {

      for (y in y_candidates) {

        Anotx <- A
        Anotx[x] <- 0
        xbar <- negate_id(x, pos_id)
        ybar <- negate_id(y, pos_id)
        Cnotxbarybar <- C
        Cnotxbarybar[c(xbar, ybar)] <- 0

        if (.subset(Anotx, Cnotxbarybar)[1]) {

          C[xbar] <- 0
          change <- TRUE
          if (verbose) {

            cat("^ Applied [MixSimp]\n")
            cat("^ Changed C -> D:\n")
            cat("^  from:", .implication_to_string(C_orig, D_orig, attributes), "\n")
            cat("^  to:", .implication_to_string(C, D, attributes), "\n")
            cat("^  -> since x = ", attributes[x],
                " and y = ", attributes[y], "\n")

          }

          return(list(A, B, C, D, TRUE))

        }

      }

    }

  }

  if ((sum(CcapnegA) > 0) && (sum(DcapnegA) > 0)) {

    x_candidates <- which(as.matrix(CcapnegA) > 0)
    y_candidates <- which(as.matrix(DcapnegA) > 0)

    for (x in x_candidates) {

      for (y in y_candidates) {

        Cnotx <- C
        Cnotx[x] <- 0
        xbar <- negate_id(x, pos_id)
        ybar <- negate_id(y, pos_id)
        Anotxbarybar <- A
        Anotxbarybar[c(xbar, ybar)] <- 0

        if (.subset(Cnotx, Anotxbarybar)[1]) {

          A[xbar] <- 0
          change <- TRUE
          if (verbose) {

            cat("^ Applied [MixSimp]\n")
            cat("^ Changed A -> B:\n")
            cat("^  from:", .implication_to_string(A_orig, B_orig, attributes), "\n")
            cat("^  to:", .implication_to_string(A, B, attributes), "\n")
            cat("^  -> since x = ", attributes[x],
                " and y = ", attributes[y], "\n")


          }

          return(list(A, B, C, D, TRUE))

        }

      }

    }

  }

  return(list(A, B, C, D, FALSE))


}
