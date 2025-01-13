add_neg <- function(imps) {

  attr <- imps$get_attributes()
  neg_attr <- paste0("\\overline{", attr, "}")
  all_attrib <- c(attr, neg_attr)

  LHS <- imps$get_LHS_matrix()
  RHS <- imps$get_RHS_matrix()

  supp <- imps$support()
  ii <- which(supp == 0)

  negLHS <- Matrix(0,
                   ncol = ncol(LHS),
                   nrow = nrow(LHS))

  negRHS <- Matrix(0,
                   ncol = ncol(RHS),
                   nrow = nrow(RHS))
  if (length(ii) > 0) {

    negRHS[, ii] <- 1

  }


  imps2 <- ImplicationSet$new(
    attributes = all_attrib,
    lhs = rbind(LHS, negLHS),
    rhs = rbind(RHS, negRHS)
  )

  return(imps2)

}
