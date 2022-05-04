# Load targets and tarchetypes
library(targets)
library(tarchetypes)

# Source R functions
source("R/utils_read-data.R")
source("R/utils_process-data.R")
source("R/utils_prettify-results.R")
source("R/utils_summary-stats-table.R")

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "here", "NlsyLinks",
                            "discord", "flextable"))

# Read and Process Initial Data
initial_read_process <- list(
  tar_target(
    link_pairs, {
      NlsyLinks::Links79PairExpanded %>%
        dplyr::filter(
          RelationshipPath == "Gen1Housemates" & RFull == 0.5
        )
      }
  ),
  tar_map(
    tar_target(
      raw,
      recode_demographics(
        read_nlsy_data(
          here(data_path)
        )
      )
    ),
    tar_target(
      processed,
      processing_function(raw)
    ),
    values = tibble::tibble(
      data_path = c(
        "data/neuroticism.csv", "data/phys_consc.csv",
        "data/consc_depression.csv", "data/income_grade_info.csv"
      ),
      model = c(
        "neuroticism", "phys_consc",
        "consc_depression", "income_grade"
      ),
      processing_function =  rlang::syms(
        glue::glue("process_{model}_data")
      )
    ),
    names = "model"
  ),
  # Manually process consc_depression_income
  # since the function takes a different format less conducive to mapping
  tar_target(
    processed_consc_depression_income,
    process_consc_depression_income_data(
      raw_consc_depression_data = raw_consc_depression,
      raw_income_grade_data = raw_income_grade
      )
  )
)

# Define a tibble for mapping through values to perform regressions and save results
modeling_values <- tibble::tibble(
  model = c(
    "neuroticism", "phys_consc",
    "consc_depression", "consc_depression_income"
    ),
  raw_data_name = rlang::syms(
    glue::glue("raw_{model}")
    ),
  processed_data_name = rlang::syms(
    glue::glue("processed_{model}")
    ),
  outcome = c(
    "depression", "age_50_physical_health",
    "depression", "depression"
    ),
  predictors = c(
    "neuroticism", "conscientousness",
    "conscientousness",
    list(
      c(
        "conscientousness",
        "highest_grade_at_age_50",
        "tnfi_at_age_50"
        )
      )
    ),
  results_save_path = glue::glue("output/{model}_results.png"),
)

# For each processed data, convert to single entered form then perform the
# regression, make the results into a pretty flextable and save them out to the
# path specified in the above tibble.
models <- list(
  tar_map(
    tar_target(
      single_entered,
      make_single_entered(
        processed_data = processed_data_name,
        outcome = outcome,
        predictors = predictors,
        link_pairs = link_pairs,
        sex = "sex",
        race = "race",
        pair_identifiers = c("_s1", "_s2")
        )
    ),
    tar_target(
      regression,
      discord_regression(
        data = single_entered,
        outcome = outcome,
        predictors = predictors,
        sex = "sex",
        race = "race",
        pair_identifiers = c("_s1", "_s2")
      )
    ),
    tar_target(
      results,
      prettify_regression_results(regression)
    ),
    values = modeling_values,
    names = "model"
    )
  )


tables <- list(
  tar_target(
    summary_stats_data,
    prep_summary_stats_table_data(
      consc_depression_data = raw_consc_depression,
      neuroticism_data = raw_neuroticism,
      income_grade_data = raw_income_grade,
      phys_consc_data = raw_phys_consc
    )
  ),
  tar_target(
    summary_stats_table,
    make_summary_stats_table(
      summary_stats_data = summary_stats_data,
      include_caption = TRUE,
      page_width = 7
    )
  ),
  tar_map(
    tar_render(
      regression_tables,
      path = "reports/tables.Rmd",
      output_format = format,
      output_dir = "output"
    ),
    tar_render(
      supplemental_materials,
      path = "reports/supplemental_materials.Rmd",
      output_format = format,
      output_dir = "output"
    ),
    values = tibble::tibble(
      format = c(
        "rmarkdown::pdf_document",
        "rmarkdown::html_document",
        "rmarkdown::word_document"
      ),
      suffix = c("pdf", "html", "word")
    ),
    names = "suffix"
  )
)


# End file with list specifying all targets.
list(
  initial_read_process,
  models,
  tables
)
