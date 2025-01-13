simplify_gen <- function(A, B, C, D, attributes, pos_id, verbose = FALSE) {

  change <- FALSE
  A_orig <- A
  B_orig <- B
  C_orig <- C
  D_orig <- D

  AcapC <- .intersection(A, C)

  if (sum(AcapC) > 0) {

    x_candidates <- which(as.matrix(AcapC) > 0)
    b_candidates <- which(as.matrix(B) > 0)
    d_candidates <- which(as.matrix(D) > 0)

    for (x in x_candidates) {

      for (b in b_candidates) {

        for (d in d_candidates) {

          bbar <- negate_id(b, pos_id)
          dbar <- negate_id(d, pos_id)

          Anotx_bbar <- A
          Anotx_bbar[x] <- 0
          Anotx_bbar[bbar] <- 1

          Cnotx_dbar <- C
          Cnotx_dbar[x] <- 0
          Cnotx_dbar[dbar] <- 1

          if (.subset(Anotx_bbar, Cnotx_dbar)[1]) {

            change <- TRUE
            D[d] <- 0

            if (verbose) {

              cat("^ Applied [MixGen]\n")
              cat("^ Changed C -> D:\n")
              cat("^  from:", .implication_to_string(C_orig, D_orig, attributes), "\n")
              cat("^  to:", .implication_to_string(C, D, attributes), "\n")
              cat("^  -> since x = ", attributes[x], ", b = ",
                  attributes[b], " and d = ", attributes[d], "\n")

            }

            return(list(A, B, C, D, TRUE))

          }

          if (.subset(Cnotx_dbar, Anotx_bbar)[1]) {

            change <- TRUE
            B[b] <- 0

            if (verbose) {

              cat("^ Applied [MixGen]\n")
              cat("^ Changed A -> B:\n")
              cat("^  from:", .implication_to_string(A_orig, B_orig, attributes), "\n")
              cat("^  to:", .implication_to_string(A, B, attributes), "\n")

            }

            return(list(A, B, C, D, TRUE))

          }

        }

      }

    }

  }

  return(list(A, B, C, D, FALSE))

}
