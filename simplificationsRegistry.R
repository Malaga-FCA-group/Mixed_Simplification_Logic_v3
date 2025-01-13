## setup registry for equivalences rules
#' Simplification Rules Registry
#'
#' \code{simplificationsRegistry$get_entry_names()}
#'
#' @export
#' @import registry
simplificationsRegistry <- registry::registry(registry_class = "simplification_registry",
                                           entry_class = "simplification_rule")

simplificationsRegistry$set_field("method",
                               type = "character",
                               is_key = TRUE,
                               index_FUN = registry::match_partial_ignorecase)

simplificationsRegistry$set_field("fun",
                               type = "function",
                               is_key = FALSE)
simplificationsRegistry$set_field("description",
                               type = "character",
                               is_key = FALSE)
