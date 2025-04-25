#' Convert lm Object to summ Object
#'
#' This function takes a regression object and optionally applies scaling and transformation
#' before converting it into a summ object using the jtools::summ function.
#' It is designed to work with objects of class 'lm'. If the object is already a 'summ' object,
#' it returns the model component of the summ object.
#'
#' @param regression_object An object of class 'lm' containing the regression model to be converted.
#' @param confint Logical indicating whether to include confidence intervals in the summary output.
#' @param scale Logical indicating whether to scale the regression coefficients.
#' @param transform.response Logical indicating whether to apply a transformation to the response variable.
#' @return The model component of the converted summ object if `regression_object` is of class 'lm',
#'         or the model component directly if `regression_object` is already a 'summ' object.
#' @examples
#' lm_model <- lm(mpg ~ cyl + hp, data = mtcars)
#' summ_model <- convert_to_summ(lm_model)
#'
convert_to_summ <- function(regression_object, confint = TRUE, scale = TRUE, transform.response = TRUE) {
  if (inherits(regression_object, "lm")) {
    summ_object <- jtools::summ(regression_object,
                                confint = confint,
                                scale = scale,
                                transform.response = transform.response)
    return(summ_object$model)
  } else if (!inherits(regression_object, "summ")) {
    stop("Input object must be an 'lm' or 'summ' object.")
  } else {
    return(regression_object$model)
  }
}


#' Prettify Discordant Kinship Regression Results
#'
#' This function formats the output of a regression model, particularly from a discordant
#' kinship analysis, into a more readable and aesthetically pleasing table format. It leverages
#' the gtsummary package for formatting. The function allows for customization such as whether
#' to standardize coefficients, include the intercept, report confidence intervals, and specify
#' the number of significant digits. Additionally, it provides the option to display both
#' standardized and unstandardized results.
#'
#' @param regression_object The regression model output to be formatted. The output of `discord_regression()`.
#' @param intercept Logical indicating whether to include the intercept in the summary output.
#' @param standardize Logical indicating whether to standardize the regression coefficients.
#' @param report_unstandardized Logical indicating whether to include unstandardized coefficients
#'        when `standardize` is TRUE. Both standardized and unstandardized results will be displayed.
#' @param digits The number of significant digits to display in the summary output.
#' @param scale Logical indicating whether scaling is applied to the coefficients for the summary.
#'        Note: This parameter is overridden within the function and does not affect scaling.
#' @param transform.response Logical indicating whether a transformation is applied to the response variable.
#'        Note: This parameter is overridden within the function and does not affect the transformation.
#' @param confint Logical indicating whether to include confidence intervals in the summary.
#' @return A flextable object containing the formatted regression model summary.

#' @export
#'
#' @examples
prettify_regression_results <- function(regression_object,
                                        intercept=TRUE,
                                        standardize = FALSE,
                                        report_unstandardized = TRUE,
                                        digits = 3,
                                        scale = TRUE,
                                        transform.response = TRUE,
                                        confint = TRUE) {


  if (is.null(standardize)) {
    standardize <- (scale | transform.response)
  }

temp <-   convert_to_summ(regression_object,
                          confint, scale = FALSE,
                          transform.response=FALSE) %>%
            gtsummary::tbl_regression(intercept=intercept,
                                      pvalue_fun = ~ gtsummary::style_pvalue(.x, digits = digits),
                                      estimate_fun = ~ gtsummary::style_sigfig(.x, digits = digits)
            ) %>%
        gtsummary::modify_header(
            statistic ~ "**t-statistic**",
            conf.low ~ "**95% CI**", p.value ~ "**p-value**"
        )
if(!confint){temp %>%
        gtsummary::modify_column_hide(column = ci) %>%
        gtsummary::modify_column_unhide(column = std.error)}

if(report_unstandardized&standardize){
temp_stnd <- convert_to_summ(regression_object, confint, scale=scale,
                             transform.response=transform.response) %>%
        gtsummary::tbl_regression(intercept=intercept,
                                  estimate_fun = ~ gtsummary::style_sigfig(.x, digits = digits),
                                  pvalue_fun = ~ gtsummary::style_pvalue(.x, digits = digits),
                                  tidy_fun = gtsummary::tidy_standardize,
								  conf.int=confint) %>%
        gtsummary::modify_header(
            estimate ~ "**β**"
  )
if(!confint){temp_stnd %>%
        gtsummary::modify_column_hide(column = conf.low) %>%
        gtsummary::modify_column_unhide(column = std.error)
  }
temp <-
  gtsummary::tbl_merge(
    list(temp_stnd, temp),
    tab_spanner = c("**Standardized**", "**Raw**")
  )
} else if(standardize){
  temp <- convert_to_summ(regression_object, confint, scale=scale,
                          transform.response=transform.response) %>%
    gtsummary::tbl_regression(intercept=intercept,
                              estimate_fun = ~ gtsummary::style_sigfig(.x, digits = digits),
                              pvalue_fun = ~ gtsummary::style_pvalue(.x, digits = digits)) %>%
    gtsummary::modify_header(
      estimate ~ "**Standardized β**",
      statistic ~ "**t-statistic**", p.value ~ "**p-value**"
      ) %>% gtsummary::modify_table_body(~.x %>% dplyr::relocate(conf.low, .after = estimate))
  if(!confint){temp %>%
      gtsummary::modify_column_hide(column= conf.low) %>%
      gtsummary::modify_column_unhide(column = std.error)
  }
}

temp <- temp %>%
  gtsummary::add_glance_source_note(
    label = list(statistic ~ "F-statistic",
                 df  ~ "DF1",
                 df.residual  ~ "DF2"),
    include = c(r.squared, statistic, df, df.residual, p.value, nobs)
  )
  return(temp)
}

#' Prettify and Optionally Export Regression Results with jtools
#'
#' This function uses the jtools package to summarize or export regression analysis results.
#' It offers options to scale coefficients, apply transformations to the response variable,
#' adjust the number of significant digits, and include confidence intervals.
#' It can return the summary directly or export it in a specified format.
#'
#' @param regression_object An object containing regression analysis results, compatible with jtools summarization.
#' @param scale Logical indicating whether to scale the regression coefficients.
#' @param transform.response Logical indicating whether to apply a transformation to the response variable.
#' @param digits Integer specifying the number of significant digits to use in the summary.
#' @param confint Logical indicating whether to include confidence intervals.
#' @param export_tbl Logical
prettify_jtools <- function(regression_object,
                                        scale=TRUE,
                                        transform.response=TRUE,
                                        digits=3,
                            confint = FALSE,
                            export_tbl=FALSE
){
  if(!export_tbl){
    temp <-  jtools::summ(regression_object,
                          scale = scale,
                          transform.response = transform.response,
                          digits= digits,
                          confint = confint)
  }else{
    temp <-  jtools::export_summs(regression_object,
                                  scale = scale,
                                  transform.response = transform.response,
                                  digits= digits,
                                  confint = confint,
                                  error_format = "[{conf.low}, {conf.high}]")
  }
  temp
}
