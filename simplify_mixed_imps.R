simplify_mixed_imps <- function(
    sigma,
    pos_id = length(sigma$get_attributes()) / 2,
    mixed_simplifications = NULL,
    simp_right = TRUE,
    apply_pos_logic = TRUE,
    verbose = FALSE
) {

  tictoc::tic()

  attr <- sigma$get_attributes()
  LHS <- sigma$get_LHS_matrix()
  RHS <- sigma$get_RHS_matrix()

  # Reduction
  RHS[LHS > 0] <- 0

  # Remove contradictions
  negLHS <- negate(LHS)
  id_cont <- which(colSums(LHS * negLHS) > 0)

  if (length(id_cont) > 0) {

    LHS <- LHS[, -id_cont]
    RHS <- RHS[, -id_cont]

  }

  iter <- 0
  oper <- length(id_cont)

  LHS_Sigma <- LHS
  RHS_Sigma <- RHS

  on.exit({

    cat("User interrupted.\n")

    timer <- tictoc::toc(quiet = TRUE)
    Sigma <- ImplicationSet$new(attributes = attr, lhs = LHS_Sigma, rhs = RHS_Sigma)

    return(list(Sigma,
                iter,
                oper,
                as.numeric(timer$toc - timer$tic)))

  })

  repeat {

    iter <- iter + 1

    if (verbose) {

      cat("** Iteration #", iter, "\n")

    }

    change <- FALSE

    LHS_Sigma_s <- LHS_Sigma
    RHS_Sigma_s <- RHS_Sigma

    LHS_Sigma <- NULL
    RHS_Sigma <- NULL

    if (is.null(LHS_Sigma_s) ||
        ncol(LHS_Sigma_s) == 0) break

    for (i in seq(ncol(LHS_Sigma_s))) {

      A <- .ext_col(LHS_Sigma_s, i)
      B <- .ext_col(RHS_Sigma_s, i)

      if (verbose) {

        cat("Studying A -> B:", .implication_to_string(A, B, attr), "\n")

      }

      L <- simplify_contradiction(A, B, attr, pos_id, verbose = verbose)
      A <- L[[1]]
      B <- L[[2]]
      foo <- L[[3]]
      change <- change | foo

      if (foo) oper <- oper + 1

      LHS_Gamma <- NULL
      RHS_Gamma <- NULL

      if (!is.null(LHS_Sigma) && (ncol(LHS_Sigma) > 0)) {

        for (j in seq(ncol(LHS_Sigma))) {

          C <- .ext_col(LHS_Sigma, j)
          D <- .ext_col(RHS_Sigma, j)

          if (verbose) {

            cat(" - Applying C -> D:", .implication_to_string(C, D, attr), "\n")

          }

          L <- simplify_contradiction(C, D, attr, pos_id, verbose = verbose)
          C <- L[[1]]
          D <- L[[2]]
          foo <- L[[3]]
          change <- change | foo
          if (foo) oper <- oper + 1

          CD <- .union2(C, D)
          AB <- .union2(A, B)

          if (apply_pos_logic && (sum(B) > 0)) {

            if ((.subset(C, A)[1] && .subset(A, CD)[1]) ||
                (.subset(A, C)[1] && .subset(C, AB)[1])) {

              A_orig <- A
              B_orig <- B

              A <- A * C
              B <- .union2(B, D)

              D <- empty_set(nrow(D))

              change <- TRUE
              oper <- oper + 1

              if (verbose) {

                cat("^ Applied [Comp]\n")
                cat("^ Changed A -> B:\n")
                cat("^  from:", .implication_to_string(A_orig, B_orig, attr), "\n")
                cat("^  to:", .implication_to_string(A, B, attr), "\n")

              }

            } else {

              if ((.subset(A, C)[1]) && !(.equal_sets(A, C)[1])) {

                if (!(.subset(D, B)[1])) {

                  C_orig <- C
                  D_orig <- D
                  C <- .difference2(C, B)
                  D <- .difference2(D, B)

                  if (any(C != C_orig) || any(D != D_orig)) {

                    change <- TRUE
                    oper <- oper + 1

                    if (verbose) {

                      cat("^ Applied [Simp]\n")
                      cat("^ Changed C -> D:\n")
                      cat("^  from:", .implication_to_string(C_orig, D_orig, attr), "\n")
                      cat("^  to:", .implication_to_string(C, D, attr), "\n")

                    }

                  }

                }

              } else {

                if ((.subset(C, A)[1]) && !(.equal_sets(C, A)[1])) {

                  A_orig <- A
                  B_orig <- B
                  A <- .difference2(A, D)
                  B <- .difference2(B, D)

                  if (any(A != A_orig) || any(B != B_orig)) {

                    change <- TRUE
                    oper <- oper + 1

                    if (verbose) {

                      cat("^ Applied [Simp]\n")
                      cat("^ Changed A -> B:\n")
                      cat("^  from:", .implication_to_string(A_orig, B_orig, attr), "\n")
                      cat("^  to:", .implication_to_string(A, B, attr), "\n")

                    }

                  }

                }

              }

            }

          }

          # Here they come! (the mixed simplifications)

          for (s in mixed_simplifications) {

            f <- simplificationsRegistry$get_entry(s)$fun

            L <- f(A, B, C, D, attr, pos_id, verbose = verbose)

            foo <- L[[5]]
            if (foo) {
              A_new <- L[[1]]
              B_new <- L[[2]]
              C_new <- L[[3]]
              D_new <- L[[4]]

              if (!simp_right) {

                if (.equal_sets(A, A_new)[1] &&
                    .equal_sets(C, C_new)[1]) {

                  foo <- FALSE

                }

              } else {

                B <- B_new
                D <- D_new

              }

              A <- A_new
              C <- C_new

            }

            change <- change | foo
            if (foo) oper <- oper + 1

          }

          if (cardinal(D) > 0) {

            if (verbose) {

              cat("-> Added C -> D:", .implication_to_string(C, D, attr), "\n")

            }

            LHS_Gamma <- cbind(LHS_Gamma, C)
            RHS_Gamma <- cbind(RHS_Gamma, D)

          }

        }

      }

      if (cardinal(B) == 0) {

        LHS_Sigma <- LHS_Gamma
        RHS_Sigma <- RHS_Gamma

      } else {

        L <- simplify_contradiction(A, B, attr, pos_id, verbose = verbose)
        A <- L[[1]]
        B <- L[[2]]
        foo <- L[[3]]
        change <- change | foo
        if (foo) oper <- oper + 1

        if (verbose) {

          cat("--> Added finally A -> B:",
              .implication_to_string(A, B, attr), "\n")

        }

        LHS_Sigma <- cbind(LHS_Gamma, A)
        RHS_Sigma <- cbind(RHS_Gamma, B)

      }

    }

    if (verbose) {

      cat("End of iteration #", iter, "with", oper, "operations\n")

    }

    if (!change) break

  }

  timer <- tictoc::toc(quiet = TRUE)
  Sigma <- ImplicationSet$new(attributes = attr, lhs = LHS_Sigma, rhs = RHS_Sigma)

  return(list(Sigma,
              iter,
              oper,
              as.numeric(timer$toc - timer$tic)))

}
