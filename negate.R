negate <- function(X, pos_id = nrow(X) / 2) {

  neg_id <- seq(pos_id + 1, nrow(X))
  X2 <- X[c(neg_id, seq(pos_id)), ]

  return(X2)

}

negate22 <- function(X, pos_id = as.integer(nrow(X) / 2)) {

  ii <- X@i
  newi <- ii
  newi[ii < pos_id] <- newi[ii < pos_id] + pos_id

  newi[ii >= pos_id] <- newi[ii >= pos_id] - pos_id
  X2 <- X
  X2@i <- newi
  # neg_id <- seq(pos_id + 1, nrow(X))
  # X2 <- X[c(neg_id, seq(pos_id)), ]

  return(X2)

}
