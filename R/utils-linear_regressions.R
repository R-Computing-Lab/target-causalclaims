#' Perform a Linear Regression (non-Discordant) Using First-Born
#'
#' @param single_entered_data The output of [make_single_entered()]
#' @param outcome A character string containing the outcome variable of
#'   interest.
#' @param predictors A character string containing the outcome variable of
#'   interest.
#' @param demographic_variables A character vector (default `c("race", "sex")`)
#'   corresponding to additional demographic variables to include as covariates
#'   in the regression.
#'
#' @return The canonical linear regression outside of discordant framework.
#' @export
#'
linear_reg_oldest_sibling <- function(
    single_entered_data,
    outcome,
    predictors,
    demographic_variables = c("race", "sex")) {
  older_sibling_data <- single_entered_data %>%
    dplyr::select(
      contains("_s1")
    ) %>%
    dplyr::rename_with(
      ~ str_remove(.x, "_s1")
    )

  model_formula <- as.formula(
    paste(
      outcome,
      paste(
        predictors,
        demographic_variables,
        sep = " + ",
        collapse = " + "
      ),
      sep = " ~ "
    )
  )
  lm(model_formula, data = older_sibling_data)
}
