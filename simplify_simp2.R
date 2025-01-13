simplify_simp2 <- function(A, B, C, D, attributes, pos_id, verbose = FALSE) {

  change <- FALSE
  A_orig <- A
  B_orig <- B
  C_orig <- C
  D_orig <- D

  if (sum(A) * sum(B) > 0) {

    x_candidates <- which(as.matrix(A) > 0)
    y_candidates <- which(as.matrix(B) > 0)

    for (x in x_candidates) {

      for (y in y_candidates) {

        Anotxybar <- A
        xbar <- negate_id(x, pos_id)
        ybar <- negate_id(y, pos_id)
        Anotxybar[x] <- 0
        Anotxybar[ybar] <- 1

        if (.subset(Anotxybar, C)[1]) {

          if (D[xbar] > 0) {

            D[xbar] <- 0
            change <- TRUE

          }

          if (C[xbar] > 0) {

            C[xbar] <- 0
            change <- TRUE

          }

          if (change) {

            if (verbose) {

              cat("^ Applied [MixSimp2]\n")
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

  }

  if (sum(C) * sum(D) > 0) {

    x_candidates <- which(as.matrix(C) > 0)
    y_candidates <- which(as.matrix(D) > 0)

    for (x in x_candidates) {

      for (y in y_candidates) {

        Cnotxybar <- C
        xbar <- negate_id(x, pos_id)
        ybar <- negate_id(y, pos_id)
        Cnotxybar[x] <- 0
        Cnotxybar[ybar] <- 1

        if (.subset(Cnotxybar, A)[1]) {

          if (B[xbar] > 0) {

            B[xbar] <- 0
            change <- TRUE

          }

          if (A[xbar] > 0) {

            A[xbar] <- 0
            change <- TRUE

          }

          if (change) {

            if (verbose) {

              cat("^ Applied [MixSimp2]\n")
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

  }

  return(list(A, B, C, D, FALSE))

}
