simplify_rsimp <- function(A, B, C, D, attributes, pos_id, verbose = FALSE) {

  change <- FALSE
  A_orig <- A
  B_orig <- B
  C_orig <- C
  D_orig <- D

  CD <- .union2(C, D)

  possible1 <- sum(.difference2(A, CD)) <= 2

  if ((sum(A) * sum(B) > 0) && possible1) {

    negCD <- negate(CD)

    x_candidates <- which(as.matrix(A) > 0)
    y_candidates <- which(as.matrix(B * negCD) > 0)

    if ((length(x_candidates) > 0) && (length(y_candidates) > 0)) {

      for (x in x_candidates) {

        for (y in y_candidates) {

          Anotxybar <- A
          xbar <- negate_id(x, pos_id)
          ybar <- negate_id(y, pos_id)
          Anotxybar[x] <- 0
          Anotxybar[ybar] <- 1

          if (.subset(Anotxybar, CD)[1]) {

            if (D[xbar] > 0) {

              D[xbar] <- 0
              change <- TRUE

              # if (.subset(Anotxybar, C)[1]) {
              #
              #   C[xbar] <- 0
              #
              # }

              if (verbose) {

                cat("^ Applied [MixRSimp]\n")
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


  }

  AB <- .union2(A, B)

  possible2 <- sum(.difference2(C, AB)) <= 2

  if ((sum(C) * sum(D) > 0) && possible2) {

    negAB <- negate(AB)

    # x_candidates <- C@i + 1 #which(as.matrix(C) > 0)
    # y_candidates <- (D * negAB)@i + 1 #which(as.matrix(D * negAB) > 0)
    x_candidates <- which(as.matrix(C) > 0)
    y_candidates <- which(as.matrix(D * negAB) > 0)

    if ((length(x_candidates) > 0) && (length(y_candidates) > 0)) {

      for (x in x_candidates) {

        for (y in y_candidates) {

          Cnotxybar <- C
          xbar <- negate_id(x, pos_id)
          ybar <- negate_id(y, pos_id)
          Cnotxybar[x] <- 0
          Cnotxybar[ybar] <- 1

          if (.subset(Cnotxybar, AB)[1]) {

            if (B[xbar] > 0) {

              B[xbar] <- 0
              change <- TRUE

              # if (.subset(Cnotxybar, A)[1]) {
              #
              #   A[xbar] <- 0
              #
              # }

              if (verbose) {

                cat("^ Applied [MixRSimp]\n")
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


  }

  return(list(A, B, C, D, FALSE))

}
