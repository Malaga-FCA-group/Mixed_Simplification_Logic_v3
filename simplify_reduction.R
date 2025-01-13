simplify_reduction <- function(A, B, C, D, attributes, pos_id, verbose = FALSE) {

  negC <- negate(C)

  AcapnegC <- A * negC

  A_orig <- A
  B_orig <- B
  C_orig <- C
  D_orig <- D

  if (sum(AcapnegC) > 0) {

    x_candidates <- which(as.matrix(AcapnegC) > 0)

    if (.subset(D, B)[1]) {

      for (x in x_candidates) {

        Anotx <- A
        Anotx[x] <- 0
        Cnotxbar <- C
        Cnotxbar[negate_id(x, pos_id)] <- 0

        if (.subset(Anotx, Cnotxbar)[1]) {

          C <- Cnotxbar

          if (.equal_sets(Anotx, C)[1]) {

            B <- .difference2(B, D)

          }

          if (verbose) {

            cat("^ Applied [Red]\n")
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

    } else {

      if (.subset(B, D)[1]) {

        for (x in x_candidates) {

          Anotx <- A
          Anotx[x] <- 0
          Cnotxbar <- C
          Cnotxbar[negate_id(x, pos_id)] <- 0

          if (.subset(Cnotxbar, Anotx)[1]) {

            A <- Anotx

            if (.equal_sets(A, Cnotxbar)[1]) {

              D <- .difference2(D, B)

            }

            if (verbose) {

              cat("^ Applied [Red]\n")
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

  }

  return(list(A, B, C, D, FALSE))

}
