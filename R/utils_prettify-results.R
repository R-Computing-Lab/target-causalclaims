
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
    gtsummary::tbl_regression() %>%
    gtsummary::add_glance_source_note(
      label = list(df  ~ "Degrees of Freedom"),
      include = c(r.squared, statistic, df, p.value)
      ) %>%
    gtsummary::modify_header(
      statistic ~ "**T-Statistic**", p.value ~ "**P-Value**"
      )
}
