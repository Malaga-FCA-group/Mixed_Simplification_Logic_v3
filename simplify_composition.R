simplify_composition <- function(A, B, C, D,
                                 attributes,
                                 pos_id,
                                 verbose = FALSE) {

  change <- FALSE
  A_orig <- A
  B_orig <- B
  C_orig <- C
  D_orig <- D

  if ((sum(B) == 1) && .subset(B, D)[1]) {

    x_candidates <- which(as.matrix(A) > 0)
    y_candidates <- which(as.matrix(C) > 0)

    for (x in x_candidates) {

      for (y in y_candidates) {

        Anotx <- A
        Anotx[x] <- 0

        Cnoty <- C
        Cnoty[y] <- 0

        if (.equal_sets(Anotx, Cnoty)[1]) {

          b <- which(as.matrix(B) > 0)
          bbar <- negate_id(b, pos_id)

          xbar <- negate_id(x, pos_id)
          ybar <- negate_id(y, pos_id)

          A <- Anotx
          A[bbar] <- 1
          B <- empty_set(nrow(B))
          # B[B == 1] <- 0
          B[c(xbar, ybar)] <- 1
          C <- C
          D[b] <- 0

          if (verbose) {

            cat("^ Applied [MixComp]\n")
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


  # if ((sum(D) == 1) && .subset(D, B)[1]) {
  #
  #   x_candidates <- which(as.matrix(C) > 0)
  #   y_candidates <- which(as.matrix(A) > 0)
  #
  #   for (x in x_candidates) {
  #
  #     for (y in y_candidates) {
  #
  #       Anotx <- C
  #       Anotx[x] <- 0
  #
  #       Cnoty <- A
  #       Cnoty[y] <- 0
  #
  #       if (.equal_sets(Anotx, Cnoty)[1]) {
  #
  #         b <- which(as.matrix(D) > 0)
  #         bbar <- negate_id(b, pos_id)
  #
  #         xbar <- negate_id(x, pos_id)
  #         ybar <- negate_id(y, pos_id)
  #
  #         C <- Anotx
  #         C[bbar] <- 1
  #         D <- 0 * D
  #         D[c(xbar, ybar)] <- 1
  #         A <- A
  #         B[b] <- 0
  #
  #         if (verbose) {
  #
  #           cat("^ Applied [MixComp]\n")
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
  #         return(list(A, B, C, D, TRUE))
  #
  #
  #       }
  #
  #     }
  #
  #   }
  #
  # }


  return(list(A, B, C, D, change))

}
