#' Recreate a dataset from a survfit object
#'
#' @param fit A survfit object from the 'survival' package.
#' @return The data.frame with variables time, status, and strata (if applicable).
#' @export
#'
dataset_from_survfit <- function(fit){

  # Check fit is of class survfit
  if (!inherits(fit, "survfit")) {
    stop("Input must be a survfit object.")
  }

  # Handle unstratified vs stratified fits
  if (is.null(fit$strata)) {
    strata_labels <- rep("All", length(fit$time))
  } else {
    strata_labels <- rep(names(fit$strata), fit$strata)
  }

  # Expand events (status = 1)
  events_df <- data.frame(
    time = rep(fit$time, fit$n.event),
    status = 1,
    strata = rep(strata_labels, fit$n.event),
    stringsAsFactors = FALSE
  )

  # Expand censorings (status = 0)
  censored_df <- data.frame(
    time = rep(fit$time, fit$n.censor),
    status = 0,
    strata = rep(strata_labels, fit$n.censor),
    stringsAsFactors = FALSE
  )

  # Combine into single dataset
  res <- rbind(events_df, censored_df)

  # Remove strata column if unstratified
  if (is.null(fit$strata)) res$strata <- NULL

  return(res)
}
