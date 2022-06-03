
#' Prep Summary Stats Table Data
#'
#' @description This function preps the data for creating summary statistics
#'   tables on the variables used in the discordant regression. It accepts the
#'   raw data and processes them in a similar manner to the way used in the
#'   analysis with modifications to ensure an informative summary statistics
#'   table.
#'
#' @param consc_depression_data Raw data for asking whether Conscientiousness
#'   Causally Influence Mental Health (Depression)?
#' @param neuroticism_data Raw data for asking whether Neuroticism Causally
#'   Influence Mental Health (Depression)?
#' @param income_grade_data Raw income/education data used when asking whether
#'   Conscientiousness Causally Influence Mental Health (Depression) with Income
#'   and Highest Grade Level (At Age 50) as Covariates?
#' @param phys_consc_data Raw data for asking whether Conscientiousness Causally
#'   Influence Physical Health
#'
#' @return A data frame with summary statistics from [skimr::skim] on the
#'   different variables for placing in a summary statistics table.
#' @export
#'
#' @examples
prep_summary_stats_table_data <- function(
  consc_depression_data,
  neuroticism_data,
  income_grade_data,
  phys_consc_data
) {

  conscientuousness_table_var <- consc_depression_data %>%
    dplyr::transmute(
      depression = H0013301,
      dependable_disciplined = T4998602,
      disorganized_careless = T4998607
    ) %>%
    dplyr::mutate(
      across(
        .cols = everything(),
        ~ case_when(
          .x < 0 ~ NA_real_,
          TRUE ~ .x
        )
      ),
      # Reverse Coding disorganized/careless measurement from TIPI and
      # adding with dependable/discplined
      conscientiousness = (reverse_code(disorganized_careless) + dependable_disciplined) / 2
    ) %>%
    select(
      conscientiousness
    )

  neuroticism_table_var <- neuroticism_data %>%
    dplyr::transmute(
      depression = H0013301,
      anxious_upset = T4998603,
      calm_stable = T4998608
    ) %>%
    dplyr::mutate(
      across(
        .cols = everything(),
        ~ case_when(
          .x < 0 ~ NA_real_,
          TRUE ~ .x
        )
      ),
      # Define neuroticism on seven-point TIPI Scale
      neuroticism = (reverse_code(calm_stable) + anxious_upset) / 2
    ) %>%
    select(neuroticism)

  # Data from https://data.bls.gov/pdq/SurveyOutputServlet
  cpi_data <- data.frame(
    cpi_year = c(2006L,2007L,2008L,2009L,2010L,2011L,
                 2012L,2013L,2014L,2015L,2016L,2017L,2018L,2019L,2020L),
    cpi_cost = c(201.6,207.342,215.303,214.537,218.056,
                 224.939,229.594,232.957,236.736,237.017,240.007,245.12,
                 251.107,255.657,258.811)
  )

  cost_to_adjust <- cpi_data[which(cpi_data$cpi_year == 2014), "cpi_cost", drop = TRUE]

  income_grade_info <- left_join(
    income_grade_data %>%
      select(
        turns_50 = H0013201,
        highest_grade_2016 = T5771400,
        highest_grade_2014 = T5023500,
        highest_grade_2012 = T4113100,
        highest_grade_2010 = T3108600,
        highest_grade_2008 = T2210700,
        tnfi_2016 = T5770800,
        tnfi_2014 = T5022600,
        tnfi_2012 = T4112300,
        tnfi_2010 = T3107800,
        tnfi_2008 = T2210000
      ) %>%
      dplyr::mutate(
        highest_grade_at_age_50 = dplyr::case_when(
          turns_50 == 2008 ~ highest_grade_2008,
          turns_50 == 2010 ~ highest_grade_2010,
          turns_50 == 2012 ~ highest_grade_2012,
          turns_50 == 2014 ~ highest_grade_2014
        ),
        tnfi_at_age_50 = dplyr::case_when(
          turns_50 == 2008 ~ tnfi_2008,
          turns_50 == 2010 ~ tnfi_2010,
          turns_50 == 2012 ~ tnfi_2012,
          turns_50 == 2014 ~ tnfi_2014
        ),
        .after = turns_50
      ),
    cpi_data,
    by = c("turns_50" = "cpi_year")
  ) %>%
    transmute(
      highest_grade_at_age_50,
      tnfi_at_age_50 = (cost_to_adjust * tnfi_at_age_50) / cpi_cost
    )


  phys_consc_data %>%
    dplyr::transmute(
      age_40_physical_health = H0003200,
      age_50_physical_health = H0015801,
    ) %>%
    dplyr::bind_cols(
      neuroticism_data %>%
        dplyr::transmute(
          # H0013301 is depression at age 50
          # H001101 is depression at age 40
          depression_age_50 = H0013301
        )
    ) %>%
    mutate(
      across(
        .cols = everything(),
        ~ case_when(
          .x < 0 ~ NA_real_,
          TRUE ~ .x
        )
      ),
      across(
        .cols = starts_with("age"),
        # divide by 100 to convert to percent
        ~ .x / 100
      )
    ) %>%
    bind_cols(
      neuroticism_table_var,
      conscientuousness_table_var
    ) %>%
    bind_cols(
      income_grade_info
    ) %>%
    skimr::skim() %>%
    select(
      -c(
        skim_type,
        complete_rate,
        numeric.hist
      )
    ) %>%
    mutate(
      skim_variable = case_when(
        skim_variable == "age_40_physical_health" ~ "Physical Health",
        skim_variable == "age_50_physical_health" ~ "Physical Health",
        skim_variable == "depression_age_50" ~ "Depression",
        skim_variable == "highest_grade_at_age_50" ~ "Education",
        skim_variable == "tnfi_at_age_50" ~ "Income",
        TRUE ~ str_to_title(skim_variable)
      ),
      # How many observations are there for each variable?
      # Missing subtracted from total NLSY Data Set
      n_obs = 12686 - n_missing,
      across(
        .cols = where(is.numeric),
        ~ round(.x, 2)
      )
    ) %>%
    select(
      -n_missing
    )

}

# Copied from https://stackoverflow.com/questions/57175351/flextable-autofit-in-a-rmarkdown-to-word-doc-causes-table-to-go-outside-page-mar

FitFlextableToPage <- function(ft, pgwidth = 6){

  ft_out <- ft %>%
    flextable::autofit()

  ft_out <- flextable::width(
    ft_out,
    width = dim(ft_out)$widths*pgwidth / (flextable::flextable_dim(ft_out)$widths)
    )

  return(ft_out)
}

#' Make Summary Statistics Table
#'
#' @param summary_stats_data The output of [prep_summary_stats_table_data()].
#' @param include_caption Logical defaults to `TRUE`.
#' @param page_width Controls output width of returned flextable object when
#'   rendering in Rmd documents.
#'
#' @return A flextable object with the summary statistics for the different
#'   analyses performed.
#' @export
#'
#' @examples
make_summary_stats_table <- function(
  summary_stats_data,
  include_caption = TRUE,
  page_width = 6
  ) {

  out <- flextable::flextable(
    summary_stats_data
  ) %>%
    flextable::fontsize(size = 12) %>%
    flextable::bold(part = "header") %>%
    flextable::set_header_labels(
      skim_variable = "Variable",
      n_obs = "Number of Observations",
      numeric.mean = "Mean",
      numeric.sd = "Standard Deviation",
      numeric.p0 = "Minimum",
      numeric.p25 = "25th Percentile",
      numeric.p50 = "50th Percentile",
      numeric.p75 = "75th Percentile",
      numeric.p100 = "Maximum"
    ) %>%
    flextable::align_nottext_col("center") %>%
    flextable::autofit() %>%
    flextable::footnote(
      i = 1,
      j = 1,
      value = flextable::as_paragraph(
        "As measured at age 40"
      ),
      ref_symbols = c("i")
    ) %>%
    flextable::footnote(
      i = c(2,3,6,7),
      j = 1,
      value = flextable::as_paragraph(
        "As measured at age 50"
      ),
      ref_symbols = c("ii")
    )

  if (include_caption) {
    out <- out %>%
      flextable::set_caption(
        "**Summary Statistics for NLSY Variables**
    Physical Health scale ranges from 10 to 70.
    Depression measure from CES-D, ranging from 0 to 24;
    Neuroticism and Conscientiousness derived from TIPI, ranging from 1 to 7;
    Education represents highest grade completed, ranging from 0 (none) to 20 (eight years of college or more);
    Income represents total net family income in 2014 dollars",
      )
  }

  FitFlextableToPage(
    out,
    pgwidth = page_width
  )

}


