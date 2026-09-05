#' Plot and adjusted Kaplan-Meier curve accounting for landmark analysis
#'
#' @param fit A survfit object from the 'survival' package.
#' @param landmark_time The time at which to perform the landmark analysis
#' @param landmark_label The label to use for the landmark time in the plot
#' @param ... Additional arguments passed to [survminer::ggsurvplot()].
#' @return The modified ggsurvplot object.
#' @export
#'
ggsurvplotlm <- function(fit,
                         landmark_time=6,
                         landmark_label = "Landmark Time",
                         ...){

  # reconstruct dataset from fit
  reconstructed_data <- dataset_from_survfit(fit) |>
    dplyr::arrange(strata,time) |>
    dplyr::mutate(
      strata2 = gsub("[A-Za-z]*=","",strata)
    )

  strata_name = gsub("=[0-9A-Za-z]*","",names(fit$strata))[1]

  # landmark time is less than or equal to 0, return the standard ggsurvplot
  if(landmark_time<=0){
    fit <- survival::survfit(Surv(time,status) ~ strata2, reconstructed_data)
    warning("Landmark time should be positive")
    return(survminer::ggsurvplot(fit,data=reconstructed_data,...))
  }

  # Create pooled KM estimates prior to landmark
  truncated = reconstructed_data |>
    dplyr::mutate(
      status = ifelse(time > landmark_time, 0, status),
      time  = ifelse(time > landmark_time, landmark_time, time)
    )

  fit_t <- survival::survfit(Surv(time,status) ~ TRUE, truncated)

  # Get the final survival probability at the landmark time
  pre_surv_final = fit_t$surv[length(fit_t$surv)]

  # Create KM estimates for post landmark
  offset = reconstructed_data |>
    dplyr::filter(time>=landmark_time) |>
    dplyr::bind_rows(
      truncated |>
        dplyr::mutate(
          strata2 = ""
        )
    ) |>
    dplyr::bind_rows(
      reconstructed_data |>
        dplyr::group_by(strata) |>
        dplyr::slice(1) |>
        dplyr::mutate(
          time=0,
          status=0,
        )
    )

  # create survfit objects for both conditional and combine objects
  fit_o2 <- survival::survfit(Surv(time,status) ~ strata2, data= offset |> dplyr::filter(strata2!=""))
  fit_o <- survival::survfit(Surv(time,status) ~ strata2, data= offset)

  # adjust the survival probabilities for the strata
  fit_o$surv =append(
    fit_o$surv[1:fit_o$strata["strata2="]],
    fit_o$surv[(fit_o$strata["strata2="]+1):length(fit_o$surv)]*pre_surv_final
  )

  # Update parameters passed to ggsurvplot to ensure colours set as desired
  dots <- list(...)
  if ("palette" %in% names(dots)) {
    dots$palette <- append("black",dots$palette)  # Override user-supplied palette
  }
  # remove median line if the pooled KM estimate at the landmark time is less than 0.5
  if ("surv.median.line" %in% names(dots)) {
    dots$surv.median.line <- dplyr::if_else(pre_surv_final<0.5, "none", dots$surv.median.line)
  }
  # add the fit and data arguments to the dots list
  dots$fit <- fit_o
  dots$data <- offset

  # create the ggsurvplot object for the combined KM estimates and the conditional KM estimates
  suppressWarnings({
    lmplot2 = survminer::ggsurvplot(fit_o2,data = offset |> dplyr::filter(strata2!=""))
    lmplot = do.call(survminer::ggsurvplot, dots)
  })

  # add the landmark time line and label to the plot
  lmplot$plot = lmplot$plot +
    ggplot2::geom_segment(x = landmark_time,y=0,yend =pre_surv_final,  linetype="solid", color = "black") +
    ggplot2::annotate("text", x = landmark_time, y = 0.2, label = landmark_label, angle=90, vjust=-0.5)

  # replace the ggplot$table with the table from the conditional KM estimates plot
  lmplot$table = lmplot2$table

  # return the final object
  lmplot
}
