
#' Read raw NLSY Data, renaming common columns
#'
#' @param .path The path to a csv file, downloaded from NLS database.
#'
#' @return A tibble with any number of topic-specific columns from the NLSY
#'   database *and* named columns for 'case_id', which is a number assigned
#'   according to the record number of each respondent; 'sample_id', subject
#'   population identification code; 'race', subject's race where 1 indicates
#'   "Hispanic", 2 indicates "Black", and 3 indicates "Non-Black, Non-Hispanic";
#'   'sex', subject's sex encoded as 1 for "Male", and 2 for "Female".
#' @export
#'
#' @examples
#'
read_nlsy_data <- function(.path) {

  data <- readr::read_csv(here::here(.path))

  if (!all(c("R0000100", "R0173600", "R0214700", "R0214800") %in% names(data))) {
    stop("Did you forget to download the case ID, sampel ID, race, or sex variables from NLSY?")
  }

  data %>%
    dplyr::rename(case_id = R0000100,
                  sample_id = R0173600,
                  race = R0214700,
                  sex = R0214800)

}
