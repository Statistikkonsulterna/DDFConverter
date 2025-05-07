#' Create Concept-file
#'
#' This function creates a concepts-file in the DDF-format
#' @param dataset A data frame. The data set to be converted. Data has to be in wide-format.
#' @param variable_id A character variable. The ID connected to each variable. Should only include lower case letters and underscores.
#' @param variable_name A character variable. The Name of each variable.
#' @param entity_id A character variable. The ID connected to each entity. Should only include lower case letters and underscores.
#' @param description (Optional) A character variable. The definition of each variable. Should not include commas or semi-colons.
#' @param source (Optional) A character variable. The source of each variable. Preferably the URL of the source.
#' @param name_short (Optional) A character variable. A shortened version of the variable name.
#' @param name_catalog (Optional) A character variable. The variable name to be shown in the catalog / tree menu.
#' @param tag_id (Optional) A character variable. The ID specifying which folder the variable belongs in. Should only include lower case letters and underscores.
#' @param output_folder (Optional) Where the file should be saved. By default set to the working directory.
#' @returns Saves a CSV-file in the given output-folder named "ddf--concepts". This file includes one row for each named variable which always includes geo, year, name, domain, entity_id, all data-variables and the given, relevant variables.
#' @param return_output Returns the output. Default = FALSE
#' @examples
#' \dontrun{
#' create_concepts(dataset = dataset, variable_id = "var_id", variable_name = "Variable",
#' entity_id = "reg_id", source = "source_url")
#'}
#' @import dplyr
#' @import tidyr
#' @import utils
#' @export

create_concepts <- function(dataset, variable_id, variable_name, entity_id, description = NULL, source = NULL,
                            name_short = NULL, name_catalog = NULL, tag_id = NULL, output_folder = getwd(),
                            return_output = FALSE) {

# Required packages
  library(dplyr)
  library(tidyr)

  # Possible errors
  if (!dir.exists(output_folder)) {
    stop("The output folder does not exist!")
  }


  required_columns <- c(variable_id, variable_name, entity_id)

  optional_columns <- c(description, source, name_short, name_catalog, tag_id)

  # all selected columns
  existing_optional <- optional_columns[optional_columns %in% colnames(dataset)]


  # Columns to select
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


  #Error if an ID is duplicated
  if (any(duplicated(concept_data$concept))) {
    stop("The Variable ID contains duplicates.")
  }


  #Error if a Name is duplicated
  if (any(duplicated(concept_data$name))) {
    stop("The Variable Names contains duplicates.")
  }


# static concepts
if (tag_id %in% colnames(dataset)){
  concept_list <- tibble::tibble(
    concept = c("geo", "year", "name", "domain", entity_id, "tags", "parent"),
    concept_type = c("entity_domain", "time", "string", "string", "entity_set", "string", "string"),
    name = c("Geographic location", "Year", "Name", "Domain", "Entity name", "Tags", "Parent"),
    domain = c("", "", "", "", "geo", "", "")
  )} else {
    concept_list <- tibble::tibble(
      concept = c("geo", "year", "name", "domain", entity_id),
      concept_type = c("entity_domain", "time", "string", "string", "entity_set"),
      name = c("Geographic location", "Year", "Name", "Domain", "Entity name"),
      domain = c("", "", "", "", "geo")
  )}


  if (!is.null(description) && description %in% colnames(dataset)) {
    concept_list <- bind_rows(
      concept_list,
      tibble::tibble(
        concept = "description",
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
        concept = "source_URL",
        concept_type = "string",
        name = "Source URL",
        domain = ""
      )
    )
  }

  if (!is.null(name_short) && name_short %in% colnames(dataset)) {
    concept_list <- bind_rows(
      concept_list,
      tibble::tibble(
        concept = "name_short",
        concept_type = "string",
        name = "Short Name",
        domain = ""
      )
    )
  }

  if (!is.null(name_catalog) && name_catalog %in% colnames(dataset)) {
    concept_list <- bind_rows(
      concept_list,
      tibble::tibble(
        concept = "name_catalog",
        concept_type = "string",
        name = "Catalog Name",
        domain = ""
      )
    )
  }

  if (!is.null(tag_id) && tag_id %in% colnames(dataset)) {
    concept_list <- bind_rows(
      concept_list,
      tibble::tibble(
        concept = "tag",
        concept_type = "entity_domain",
        name = "Tag",
        domain = ""
      )
    )
  }

  # add columns if optional variables are included (filled with NA, will be removed in write.csv-function)
  for (col in existing_optional) {
    concept_list[[col]] <- NA
  }


  ddf_concepts <- rbind(concept_data, concept_list)

# rename optional variables
  if (!is.null(description) && description %in% colnames(ddf_concepts)) {
    ddf_concepts <- ddf_concepts %>%
      rename("description" = .data[[description]])
  }

  if (!is.null(source) && source %in% colnames(ddf_concepts)) {
    ddf_concepts <- ddf_concepts %>%
      rename("source_URL" = .data[[source]])
  }

  if (!is.null(name_short) && name_short %in% colnames(ddf_concepts)) {
    ddf_concepts <- ddf_concepts %>%
      rename("name_short" = .data[[name_short]])
  }

  if (!is.null(name_catalog) && name_catalog %in% colnames(ddf_concepts)) {
    ddf_concepts <- ddf_concepts %>%
      rename("name_catalog" = .data[[name_catalog]])
  }

  if (!is.null(tag_id) && tag_id %in% colnames(ddf_concepts)) {
    ddf_concepts <- ddf_concepts %>%
      rename("tags" = .data[[tag_id]])
  }

  # create a filename for the CSV
  output_filename <- "ddf--concepts.csv"
  # combine folder path and filename
  output_filepath <- file.path(output_folder, output_filename)
  # save as a CSV file
  write.csv(ddf_concepts, file = output_filepath, row.names = FALSE, quote = FALSE, na="", fileEncoding = "UTF-8")


  # return the output
  if (return_output) {
    return(ddf_concepts)
  }
}

