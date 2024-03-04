
#' Reverse Code Survey Items
#'
#' @description From https://github.com/svmiller/stevemisc/blob/master/R/revcode.R
#' @param x A vector of survey items
#'
#' @return
#' @export
#'
#' @examples
reverse_code <- function(x) {
  len <- length(na.omit(unique(x)))+1
  return((x*-1) + len)
}

#' Recode NLSY Demographics
#'
#' @description This function takes in raw data from the NLSY, read in using the
#'   `read_nlsy_data()` function and recodes the race and sex columns into
#'   indicator variables for easily performing regressions. Specifically, race
#'   is encoded as a 0 if the individual is a minority (black or hispanic) and a
#'   1 if the individual is non-black, non-hispanic. Sex is encoded as a 0 if
#'   the individual is female and 1 if the individual is male.
#' @param .data The output of `read_nlsy_data()`.
#'
#' @return A tibble with recoded race and sex columns.
#' @export
#'
#' @examples
recode_demographics <- function(.data) {

  .data %>%
    dplyr::mutate(
      # if race is 1 or 2, minority (hispanic or black)
      # if race is 3, non-black, non-hispanic
      race = dplyr::case_when(race == 1 | race == 2 ~ 0,
                       race == 3 ~ 1),
      # if sex is 2, then female
      # if sex is 1, then male
      sex = dplyr::if_else(sex == 2, 0, 1)
      )

}

#' Process neuroticism data
#'
#' @param .data The output of `recode_demographics()` on neuroticism
#'   data.
#'
#' @return A tibble with the case ID, sample ID, race, sex, depression and
#'   neuroticism scores (and the anxious/upset & calm_stable components of
#'   neuroticism) for all NLSY individuals. The 'depression' and 'neuroticism'
#'   columns have been Z-scored for easier interpretation with regression
#'   analysis.
#' @export
#'
#' @examples
process_neuroticism_data <- function(.data) {

  .data %>%
    dplyr::transmute(case_id,
                     sample_id,
                     race,
                     sex,
                     depression = H0013301,
                     anxious_upset = T4998603,
                     calm_stable = T4998608) %>%
    # # Filter the data frame so we only have values greater than zero in the columns 'depression_score', 'anxious_upset', and 'calm_stable'
    dplyr::filter(dplyr::across(.cols = c(depression, anxious_upset, calm_stable), function(value) value >= 0)) %>%
    dplyr::mutate(
      depression = scale(depression)[,1], # Depression column from NLSY
      neuroticism = scale(
        # Reverse Coding calm/stable measurement from TIPI and
        # adding with anxious/upset
        reverse_code(calm_stable) + anxious_upset
      )[,1])

}

#' Process Physical Health & Conscientiousness Data
#'
#' @param .data The output of `recode_demographics()` on conscientiousness &
#'   physical health data.
#'
#' @return A tibble with the case ID, sample ID, race, sex, physical health at
#'   age 40 and age 50 and conscientiousness scores (and the
#'   disorganized/careless & dependable/disciplined components of
#'   conscientiousness) for all NLSY individuals. The 'age_40_physical_health',
#'   'age_50_physical_health', and 'conscientiousness' columns have been
#'   Z-scored for easier interpretation with regression analysis.
#' @export
#'
#' @examples
process_phys_consc_data <- function(.data) {

  .data %>%
    dplyr::transmute(case_id,
                     sample_id,
                     race,
                     sex,
                     age_40_physical_health = H0003200,
                     age_50_physical_health = H0015801,
                     dependable_disciplined = T4998602,
                     disorganized_careless = T4998607) %>%
    # # Filter the data frame so we only have values greater than zero in the
    # columns 'age_40_physical_health', age_50_physical_health',
    # 'dependable_disciplined', and 'disorganized_careless'
    dplyr::filter(dplyr::across(.cols = c(age_40_physical_health, age_50_physical_health,
                                          dependable_disciplined, disorganized_careless),
                                function(value) value >= 0)) %>%
    dplyr::mutate(
      age_40_physical_health = scale(age_40_physical_health)[,1],
      age_50_physical_health = scale(age_50_physical_health)[,1],
      conscientiousness = scale(
        # Reverse Coding disorganized/careless measurement from TIPI and
        # adding with dependable/discplined
        reverse_code(disorganized_careless) + dependable_disciplined
      )[,1]
    )

}

#' Process Conscientipusness & Depression Data
#'
#' @param .data The output of `recode_demographics()` on conscientiousness and
#'   depression data.
#'
#' @return A tibble with the case ID, sample ID, race, sex, depression and
#'   conscientiousness scores (and the disorganized/careless &
#'   dependable/discplined components of conscientiousness) for all NLSY
#'   individuals. The 'depression' and 'conscientiousness' columns have been
#'   Z-scored for easier interpretation with regression analysis.
#' @export
#'
#' @examples
process_consc_depression_data <- function(.data) {

  .data %>%
    dplyr::transmute(case_id,
                     sample_id,
                     race,
                     sex,
                     depression = H0013301,
                     dependable_disciplined = T4998602,
                     disorganized_careless = T4998607) %>%
    # # Filter the data frame so we only have values greater than zero in the
    # columns 'depression', 'dependable_disciplined', and
    # 'disorganized_careless'
    dplyr::filter(dplyr::across(.cols = c(depression, dependable_disciplined, disorganized_careless),
                  function(value) value >= 0)) %>%
    dplyr::mutate(
      depression = scale(depression)[,1], # Depression column from NLSY,
      conscientiousness = scale(
        # Reverse Coding disorganized/careless measurement from TIPI and
        # adding with dependable/discplined
        reverse_code(disorganized_careless) + dependable_disciplined
      )[,1]
    )

}

#'Process Income & Grade Data
#'
#'@param .data The output of `recode_demographics()` on income and grade data.
#'@param adjust_inflation Logical: Should total net family income at age 50 be
#'  adjusted for inflation? `TRUE` by default.
#'@param inflation_year Year to adjust for inflation by.
#'
#' @return A tibble with the case ID, sample ID, race, sex, highest grade
#'   completed at age 50 (first grade (1) through 12th grade (12); 13 through 20
#'   corresponding to years of college), and the total net family income at age
#'   50 (in USD) for all NLSY individuals. If `adjust_inflation = TRUE`, then
#'   the 'tnfi_at_age_50' column will be in USD with purchasing power from
#'   `inflation_year`.
#'
#' @examples
#'
process_income_grade_data <- function(.data,
                                      adjust_inflation = TRUE, inflation_year = 2014) {

  # Data from https://data.bls.gov/pdq/SurveyOutputServlet
  cpi_data <- data.frame(
    cpi_year = c(2006L,2007L,2008L,2009L,2010L,2011L,
                 2012L,2013L,2014L,2015L,2016L,2017L,2018L,2019L,2020L),
    cpi_cost = c(201.6,207.342,215.303,214.537,218.056,
                 224.939,229.594,232.957,236.736,237.017,240.007,245.12,
                 251.107,255.657,258.811)
  )

  if (!inflation_year %in% cpi_data$cpi_year) stop("Sorry, you can only adjust dollars for years 2006 to 2020.")

  out <-  .data %>%
    dplyr::select(case_id,
                  sample_id,
                  race,
                  sex,
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
                  tnfi_2008 = T2210000) %>%
    dplyr::filter(dplyr::across(.cols = c(turns_50, contains("tnfi")),
                                function(value) value >= 0)) %>%
    dplyr::filter(dplyr::across(.cols = highest_grade_2016:highest_grade_2008,
                                function(value) dplyr::between(value, 1, 20))) %>%
    dplyr::mutate(highest_grade_at_age_50 = dplyr::case_when(turns_50 == 2008 ~ highest_grade_2008,
                                                             turns_50 == 2010 ~ highest_grade_2010,
                                                             turns_50 == 2012 ~ highest_grade_2012,
                                                             turns_50 == 2014 ~ highest_grade_2014),
                  tnfi_at_age_50 = dplyr::case_when(turns_50 == 2008 ~ tnfi_2008,
                                                    turns_50 == 2010 ~ tnfi_2010,
                                                    turns_50 == 2012 ~ tnfi_2012,
                                                    turns_50 == 2014 ~ tnfi_2014),
                  .after = turns_50)

  if (adjust_inflation) {

    cost_to_adjust <- cpi_data[which(cpi_data$cpi_year == inflation_year), "cpi_cost", drop = TRUE]

    out <- cpi_data %>%
      dplyr::left_join(out, by = c("cpi_year" = "turns_50")) %>%
      dplyr::mutate(tnfi_at_age_50 = (cost_to_adjust * tnfi_at_age_50) / cpi_cost)
  }

  out %>%
    dplyr::select(case_id, sample_id, race, sex,
                  highest_grade_at_age_50, tnfi_at_age_50
                  ) %>%
    tibble::as_tibble()
}


#' Process Conscientiousness & Depression Data with Income/Grade info as
#' Covariates
#'
#' @param raw_consc_depression_data The output of `recode_demographics()` on
#'   conscientiousness and depression data.
#' @param raw_income_grade_data The output of `recode_demographics()` on income
#'   and grade data.
#' @param adjust_inflation Logical: Should total net family income at age 50 be
#'   adjusted for inflation? `TRUE` by default.
#' @param inflation_year Year to adjust for inflation by.
#'
#'
#'@return A tibble with the case ID, sample ID, race, sex, highest grade
#'  completed at age 50 (first grade (1) through 12th grade (12); 13 through 20
#'  corresponding to years of college), the total net family income at age 50
#'  (in USD), depression and conscientiousness scores (and the
#'  disorganized/careless & dependable/discplined components of
#'  conscientiousness) for all NLSY individuals. The 'depression' and
#'  'conscientiousness' columns have been Z-scored for easier interpretation
#'  with regression analysis. If `adjust_inflation = TRUE`, then the
#'  'tnfi_at_age_50' column will be in USD with purchasing power from
#'  `inflation_year`.
#'
#' @export
#'
#' @examples
#'
process_consc_depression_income_data <- function(raw_consc_depression_data, raw_income_grade_data,
                                                 adjust_inflation = TRUE, inflation_year = 2014) {

  income_grade_data <- process_income_grade_data(raw_income_grade_data,
                                                 adjust_inflation = adjust_inflation,
                                                 inflation_year = inflation_year
                                                 )

  process_consc_depression_data(raw_consc_depression_data) %>%
    dplyr::left_join(income_grade_data,
                     by = c("case_id", "sample_id", "race", "sex"))

}

#' Process Neuroticism & Depression Data with Income/Grade info as
#' Covariates
#'
#' @param raw_neuroticism_data The output of `recode_demographics()` on
#'   neuroticism and depression data.
#' @param raw_income_grade_data The output of `recode_demographics()` on income
#'   and grade data.
#' @param adjust_inflation Logical: Should total net family income at age 50 be
#'   adjusted for inflation? `TRUE` by default.
#' @param inflation_year Year to adjust for inflation by.
#'
#'
#'@return A tibble with the case ID, sample ID, race, sex, highest grade
#'  completed at age 50 (first grade (1) through 12th grade (12); 13 through 20
#'  corresponding to years of college), the total net family income at age 50
#'  (in USD), depression and conscientiousness scores (and the
#'  disorganized/careless & dependable/discplined components of
#'  conscientiousness) for all NLSY individuals. The 'depression' and
#'  'conscientiousness' columns have been Z-scored for easier interpretation
#'  with regression analysis. If `adjust_inflation = TRUE`, then the
#'  'tnfi_at_age_50' column will be in USD with purchasing power from
#'  `inflation_year`.
#'
#' @export
#'
#' @examples
#'
process_neuroticism_income_data <- function(raw_neuroticism_data, raw_income_grade_data,
                                            adjust_inflation = TRUE, inflation_year = 2014) {

  income_grade_data <- process_income_grade_data(raw_income_grade_data,
                                                 adjust_inflation = adjust_inflation,
                                                 inflation_year = inflation_year
  )

  process_neuroticism_data(raw_neuroticism_data) %>%
    dplyr::left_join(income_grade_data,
                     by = c("case_id", "sample_id", "race", "sex"))

}

#' Transform NLSY Data into Single Entered Structure for Kin Comparison
#'
#' @param processed_data The processed data from NLSY (e.g. output of
#'   `process_neuroticism_data()`) or comparable data frame with case ID, sample
#'   ID, race, sex, and other variables of interest.
#' @param outcome A character string containing the outcome variable of
#'   interest.
#' @param predictors A character vector containing the column names for
#'   predicting the outcome.
#' @param link_pairs A data frame containing the SubjectTags of each subject in
#'   the pair and their R coefficient -- from [NlsyLinks] package (e.g.
#'   [NlsyLinks::Links79PairExpanded]).
#' @param sex A character string for the column name containing the sex
#'   variable.
#' @param race A character string for the column name containing the race
#'   variable.
#' @param pair_identifiers A character vector of length two that contains the
#'   variable identifier for each kinship pair.
#'
#' @return A tibble with paired, single-entered data for comparing kin using
#'   NLSY `link_pairs` relationship to connect kin.
#'
#' @export
#'
#' @examples
#'
make_single_entered <- function(processed_data,
                                outcome,
                                predictors,
                                link_pairs,
                                sex = "sex",
                                race = "race",
                                pair_identifiers = c("_s1", "_s2")
                                ) {

  processed_data %>%
    dplyr::mutate(SubjectTag = case_id * 100) %>%
    NlsyLinks::CreatePairLinksSingleEntered(linksPairDataset = link_pairs,
                                            linksNames = c("ExtendedID", "R", "RelationshipPath"),
                                            outcomeNames = c(outcome, predictors, sex, race),
                                            subject1Qualifier = pair_identifiers[1],
                                            subject2Qualifier = pair_identifiers[2]
                                            ) %>%
    # NAs in outcome variable cannot be present for discordant regression to work
    tidyr::drop_na( dplyr::contains( {{outcome}} ) ) %>%
    tibble::as_tibble() %>%
    dplyr::rename(
      subject_tag_s1 = SubjectTag_S1,
      subject_tag_s2 = SubjectTag_S2,
      extended_id = ExtendedID,
      relationship_path = RelationshipPath
    )  %>%
  mutate(id = row_number())

}
