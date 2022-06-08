
#' Prettify Discordant Kinship Regression Results
#'
#' @param regression_object The output of `discord_regression()`.
#'
#' @return A flextable object with model fit metrics.
#' @export
#'
#' @examples
prettify_regression_results <- function(regression_object) {
  regression_object %>%
    gtsummary::tbl_regression(intercept=TRUE) %>%
    gtsummary::add_glance_source_note(
      label = list(statistic ~ "F-statistic",
                   df  ~ "DF1",
                   df.residual  ~ "DF2"),
      include = c(r.squared, statistic, df, df.residual, p.value, nobs)
      ) %>%
    gtsummary::modify_header(
      statistic ~ "**t-statistic**", p.value ~ "**p-value**"
      )
}
