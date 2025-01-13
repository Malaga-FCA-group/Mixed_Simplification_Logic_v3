simplify_key <- function(A, B, C, D, attributes, pos_id, verbose = FALSE) {

  negC <- negate(C)
  negD <- negate(D)

  AcapD <- A * D
  BcapnegC <- B * negC

  AcapnegD <- A * negD
  BcapC <- B * C

  A_orig <- A
  B_orig <- B
  C_orig <- C
  D_orig <- D

  if ((sum(AcapD) > 0) && (sum(BcapnegC) > 0)) {

    x_candidates <- which(as.matrix(AcapD) > 0)
    y_candidates <- which(as.matrix(BcapnegC) > 0)

    for (x in x_candidates) {

      for (y in y_candidates) {

        Anotx <- A
        Anotx[x] <- 0
        Cnotybar <- C
        ybar <- negate_id(y, pos_id)
        Cnotybar[ybar] <- 0

        if (.equal_sets(Anotx, Cnotybar)[1]) {

          B[y] <- 0
          C <- Cnotybar
          D <- 0 * D
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

      }

    }

  }

  if ((sum(AcapnegD) > 0) && (sum(BcapC) > 0)) {

    x_candidates <- which(as.matrix(AcapnegD) > 0)
    y_candidates <- which(as.matrix(BcapC) > 0)

    for (x in x_candidates) {

      for (y in y_candidates) {

        Anotx <- A
        Anotx[x] <- 0
        Cnoty <- C
        Cnoty[y] <- 0

        if (.equal_sets(Anotx, Cnoty)[1]) {

          ybar <- negate_id(y, pos_id)
          xbar <- negate_id(x, pos_id)
          D[xbar] <- 0
          A <- Anotx
          B <- 0 * B
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

      }

    }

  }

  return(list(A, B, C, D, FALSE))

}
