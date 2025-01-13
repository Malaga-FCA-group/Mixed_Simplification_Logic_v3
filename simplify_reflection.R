simplify_reflection <- function(A, B, C, D, attributes, pos_id, verbose = FALSE) {

  negC <- negate(C)
  negD <- negate(D)

  change <- FALSE

  AcapnegD <- A * negD
  BcapnegC <- B * negC

  A_orig <- A
  B_orig <- B
  C_orig <- C
  D_orig <- D

  if ((sum(AcapnegD) > 0) && (sum(BcapnegC) > 0)) {

    x_candidates <- which(as.matrix(AcapnegD) > 0)
    y_candidates <- which(as.matrix(BcapnegC) > 0)

    for (x in x_candidates) {

      for (y in y_candidates) {

        Anotx <- A
        Anotx[x] <- 0
        Cnotybar <- C
        ybar <- negate_id(y, pos_id)
        Cnotybar[ybar] <- 0

        if (.subset(Anotx, Cnotybar)[1]) {

          xbar <- negate_id(x, pos_id)
          D[xbar] <- 0
          # if (sum(D) == 0) C <- 0 * C
          change <- TRUE

          if (verbose) {

            cat("^ Applied [Rft]\n")
            cat("^ Changed A -> B:\n")
            cat("^ from:", .implication_to_string(A_orig, B_orig, attributes), "\n")
            cat("^ to: ", .implication_to_string(A, B, attributes), "\n")
            cat("^ Changed C -> D:\n")
            cat("^ from:", .implication_to_string(C_orig, D_orig, attributes), "\n")
            cat("^ to: ", .implication_to_string(C, D, attributes), "\n")

          }

        } else {

          if (.subset(Cnotybar, Anotx)[1]) {

            B[y] <- 0
            # if (sum(B) == 0) A <- 0 * A
            change <- TRUE

            if (verbose) {

              cat("^ Applied [Rft]\n")
              cat("^ Changed A -> B:\n")
              cat("^ from:", .implication_to_string(A_orig, B_orig, attributes), "\n")
              cat("^ to: ", .implication_to_string(A, B, attributes), "\n")
              cat("^ Changed C -> D:\n")
              cat("^ from:", .implication_to_string(C_orig, D_orig, attributes), "\n")
              cat("^ to: ", .implication_to_string(C, D, attributes), "\n")

            }

          }

        }

      }

    }

  }

  return(list(A, B, C, D, change))

}
