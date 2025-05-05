#' Create Concept-file
#'
#' This function creates a concepts-file in the DDF-format
#' @param dataset A data frame. The data set to be converted. Data has be in long-format.
#' @param variable_id A character variable. The ID connected to each variable. Should only include lower case letters and underscores.
#' @param variable_name A character variable. The Name of each variable.
#' @param entity_id A character variable. The ID connected to each entity. Should only include lower case letters and underscores.
#' @param description (Optional) A character variable. The definition of each variable. Should not include commas or semi-colons.
#' @param source (Optional) A character variable. The source of each variable. Preferably the URL of the source.
#' @param output_folder (Optional) Where the file should be saved. By default set to the working directory.
#' @returns Saves a CSV-file in the given output-folder named "ddf--concepts". This file includes one row for each named variable which always includes geo, year, name, domain, entity_id, all data-variables and the given, relevant variables.
#' @examples
#' \dontrun{
#' create_concepts(dataset = dataset, variable_id = "var_id", variable_name = "Variable",
#' entity_id = "reg_id", source = "source_url")
#'}
#' @import dplyr
#' @import tidyr
#' @import utils
#' @export

create_concepts <- function(dataset, variable_id, variable_name, entity_id, description = NULL, source = NULL, output_folder = getwd()) {

  #Möjliga tillägg
    #name_short
    #name_catalog
    #tags & tag

# Required packages
  library(dplyr)
  library(tidyr)

  # Possible errors
  if (!dir.exists(output_folder)) {
    stop("The output folder does not exist!")
  }

  required_columns <- c(variable_id, variable_name, entity_id)
  optional_columns <- c(description, source)

  if (!all(c(required_columns) %in% colnames(dataset))) {
    stop("One or more column names do not exist in the dataset.")
  }

  # all selected columns
  existing_optional <- optional_columns[optional_columns %in% colnames(dataset)]
  selected_columns <- c(variable_id, variable_name, existing_optional)


  # filter out the variables not used from the dataset
  concept_data <- dataset %>%
    select(all_of(selected_columns)) %>%
    distinct()


  # generate concept rows
  concept_data <- concept_data %>%
    mutate(
      concept = .[[variable_id]],
      concept_type = "measure",
      name = .[[variable_name]],
      domain = ""
    ) %>%
    select(concept, concept_type, name, domain, all_of(existing_optional))

# static concepts

  # ÄNDRA HÄR: OM DESCRIPTION/SOURCE_URL FINNS MED, MÅSTE DE FINNAS I LISTAN
    # Kan alltid finnas med även utan data (??)

  concept_list <- tibble::tibble(
    concept = c("geo", "year", "name", "domain", entity_id),
    concept_type = c("entity_domain", "time", "string", "string", "entity_set"),
    name = c("Geographic location", "Year", "Name", "Domain", "Entity name"),
    domain = c("", "", "", "", "geo")
  )


  if (!is.null(description) && description %in% colnames(dataset)) {
    concept_list <- bind_rows(
      concept_list,
      tibble::tibble(
        concept = description,
        concept_type = "string",
        name = "Description",
        domain = ""
      )
    )
  }

  if (!is.null(source) && source %in% colnames(dataset)) {
    concept_list <- bind_rows(
      concept_list,
      tibble::tibble(
        concept = source,
        concept_type = "string",
        name = "Source",
        domain = ""
      )
    )
  }

  # add columns if optional variables are included (filled with NA, will be removed in write.csv-function)
  for (col in existing_optional) {
    concept_list[[col]] <- NA
  }



  ddf_concepts <- rbind(concept_data, concept_list)


  # create a filename for the CSV
  output_filename <- "ddf--concepts.csv"
  # combine folder path and filename
  output_filepath <- file.path(output_folder, output_filename)
  # save as a CSV file
  write.csv(ddf_concepts, file = output_filepath, row.names = FALSE, quote = FALSE, na="", fileEncoding = "UTF-8")
}
