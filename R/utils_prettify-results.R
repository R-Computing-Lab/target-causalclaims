
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
  broom::tidy() %>%
    dplyr::mutate(p.value = scales::pvalue(p.value, add_p = TRUE),
           dplyr::across(.cols = where(is.numeric), ~round(.x, 3))) %>%
    dplyr::rename("Standard Error" = std.error,
           "T Statistic" = statistic) %>%
    dplyr::rename_with(~stringr::str_to_title(.x)) %>%
    flextable::flextable()
}
