
#' Prettify Discordant Kinship Regression Results
#'
#' @param regression_object The output of `discord_regression()`.
#'
#' @return A flextable object with model fit metrics.
#' @export
#'
#' @examples
prettify_regression_results <- function(regression_object,
                                        intercept=TRUE,
                                        standardize=FALSE,
					digits=3) {

temp <-  regression_object %>%
            gtsummary::tbl_regression(intercept=intercept,
                                      pvalue_fun = ~ gtsummary::style_pvalue(.x, digits = digits),
                                      estimate_fun = ~ gtsummary::style_sigfig(.x, digits = digits)
            ) %>%
        gtsummary::modify_header(
            statistic ~ "**t-statistic**", p.value ~ "**p-value**"
        ) %>%
        gtsummary::modify_column_hide(column = ci) %>%
        gtsummary::modify_column_unhide(column = std.error) %>%
        gtsummary::add_glance_source_note(
            label = list(statistic ~ "F-statistic",
                         df  ~ "DF1",
                         df.residual  ~ "DF2"),
            include = c(r.squared, statistic, df, df.residual, p.value, nobs)
        )
if(standardize){
temp_stnd <-      regression_object %>%
        gtsummary::tbl_regression(intercept=intercept,
                                  estimate_fun = ~ gtsummary::style_sigfig(.x, digits = digits),
                                  pvalue_fun = ~ gtsummary::style_pvalue(.x, digits = digits),
                                  tidy_fun = gtsummary::tidy_standardize,
								  conf.int=FALSE) %>%
        gtsummary::modify_header(
            estimate ~ "**β**"
        )
temp <-
  tbl_merge(
    list(temp_stnd, temp),
    tab_spanner = c("**Standardized**", "**Unstandardized**")
  )
}
temp
}


prettify_jtools <- function(regression_object,
                                        scale=TRUE,
                                        transform.response=TRUE,
                                        digits=3){
  temp <-  jtools::summ(regression_object,
               scale = scale,
               transform.response = transform.response,
               digits= digits)
  temp
}
