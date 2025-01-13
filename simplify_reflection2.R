simplify_rft2 <- function(A, B, C, D, attributes, pos_id, verbose = FALSE) {

  negC <- negate(C)
  negD <- negate(D)

  change <- FALSE

  AcapnegD <- A * negD
  BcapnegC <- B * negC

  A_orig <- A
  B_orig <- B
  C_orig <- C
  D_orig <- D

  if (sum(B) == 1) {

    if ((sum(A) > 0) && (sum(BcapnegC) > 0)) {

      x_candidates <- which(as.matrix(A) > 0)
      y_candidates <- which(as.matrix(BcapnegC) > 0)

      for (x in x_candidates) {

        for (y in y_candidates) {

          Anotx <- A
          Anotx[x] <- 0
          Cnotybar <- C
          ybar <- negate_id(y, pos_id)
          Cnotybar[ybar] <- 0

          if (.equal_sets(Anotx, Cnotybar)[1]) {

            xbar <- negate_id(x, pos_id)
            D[xbar] <- 1
            B[y] <- 0
            A <- empty_set(nrow(A))
            # A[A == 1] <- 0
            change <- TRUE

            if (verbose) {

              cat("^ Applied [Rft2]\n")
              cat("^ Changed A -> B:\n")
              cat("^ from:", .implication_to_string(A_orig, B_orig, attributes), "\n")
              cat("^ to: ", .implication_to_string(A, B, attributes), "\n")

              cat("^ Changed C -> D:\n")
              cat("^ from:", .implication_to_string(C_orig, D_orig, attributes), "\n")
              cat("^ to: ", .implication_to_string(C, D, attributes), "\n")

            }

            return(list(A, B, C, D, change))

          }

        }

      }

    }

  }


  if (sum(D) == 1) {

    if ((sum(C) > 0) && (sum(AcapnegD) > 0)) {

      x_candidates <- which(as.matrix(C) > 0)
      y_candidates <- which(as.matrix(AcapnegD) > 0)

      for (x in x_candidates) {

        for (y in y_candidates) {

          Anoty <- A
          Anoty[y] <- 0
          Cnotx <- C
          Cnotx[x] <- 0
          xbar <- negate_id(x, pos_id)
          ybar <- negate_id(y, pos_id)

          if (.equal_sets(Anoty, Cnotx)[1]) {

            D[ybar] <- 0
            # C[C == 1] <- 0
            C <- empty_set(nrow(C))
            B[xbar] <- 1
            change <- TRUE

            if (verbose) {

              cat("^ Applied [Rft2]\n")
              cat("^ Changed A -> B:\n")
              cat("^ from:", .implication_to_string(A_orig, B_orig, attributes), "\n")
              cat("^ to: ", .implication_to_string(A, B, attributes), "\n")

              cat("^ Changed C -> D:\n")
              cat("^ from:", .implication_to_string(C_orig, D_orig, attributes), "\n")
              cat("^ to: ", .implication_to_string(C, D, attributes), "\n")

            }

            return(list(A, B, C, D, change))

          }

        }

      }

    }

  }

  return(list(A, B, C, D, FALSE))

}



# if ((sum(AcapnegD) > 0) && (sum(BcapnegC) > 0)) {
#
#   x_candidates <- which(as.matrix(AcapnegD) > 0)
#   y_candidates <- which(as.matrix(BcapnegC) > 0)
#
#   for (x in x_candidates) {
#
#     for (y in y_candidates) {
#
#       Anotx <- A
#       Anotx[x] <- 0
#       Cnotybar <- C
#       ybar <- negate_id(y, pos_id)
#       Cnotybar[ybar] <- 0
#
#       if (.equal_sets(Anotx, Cnotybar)[1]) {
#
#         xbar <- negate_id(x, pos_id)
#         D[xbar] <- 1
#         B[y] <- 0
#         change <- TRUE
#
#         if (verbose) {
#
#           cat("^ Applied [Rft2]\n")
#           cat("^ Changed A -> B:\n")
#           cat("^ from:", .implication_to_string(A_orig, B_orig, attributes), "\n")
#           cat("^ to: ", .implication_to_string(A, B, attributes), "\n")
#
#           cat("^ Changed C -> D:\n")
#           cat("^ from:", .implication_to_string(C_orig, D_orig, attributes), "\n")
#           cat("^ to: ", .implication_to_string(C, D, attributes), "\n")
#
#         }
#
#       }
#
#     }
#
#   }
#
# }

# return(list(A, B, C, D, FALSE))
#
# }
