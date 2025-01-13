negate_id <- function(x, pos_id) {

  if (x + pos_id <= 2 * pos_id) return(x + pos_id)

  return(x - pos_id)

}
