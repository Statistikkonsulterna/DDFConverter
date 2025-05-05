#' Create DDF-filesystem
#'
#' This function creates a filesystem in the DDF-format
#' @param dataset A data frame. The data set to be converted. Data has be in long-format.
#' @param variable_id A character variable. The ID connected to each variable. Should only include lower case letters and underscores.
#' @param variable_name A character variable. The Name of each variable.
#' @param entity_id A character variable. The ID connected to each entity. Should only include lower case letters and underscores.
#' @param entity_name A character variable. The Name of each entity.
#' @param datacolumns A range of numeric variables. The columns including data. Should be referenced in the format "1-3" or "1,2,3"
#' @param description (Optional) A character variable. The definition of each variable. Should not include commas or semi-colons.
#' @param source (Optional) A character variable. The source of each variable. Preferably the URL of the source.
#' @param output_folder (Optional) Where the file should be saved. By default set to the working directory.
#' @returns Saves a folder in the given output-folder named "ddf_filesystem_current_date". This folder includes a concepts-files, entities-file and a folder containing all datapoints.
#' @examples
#' \dontrun{
#' create_ddf(dataset = dataset, variable_id = "var_id", variable_name = "Variable",
#' entity_id = "reg_id", entity_name = "Region", datacolumns = "7-21", description = "description",
#' output_folder = get_wd())
#'}
#' @import dplyr
#' @import tidyr
#' @import utils
#' @export

create_ddf <- function(dataset, variable_id, variable_name, entity_id, entity_name, datacolumns, description = NULL, source = NULL, output_folder = getwd()) {

  # Required packages
  library(dplyr)
  library(tidyr)

  # Possible errors
  if (!dir.exists(output_folder)) {
    stop("The output folder does not exist!")
  }


  ######################### Code from concepts #################################
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


  folder_name <- paste0("ddf_filessystem_", Sys.Date())

  # Create the new subfolder path
  new_folder <- file.path(output_folder, folder_name)

  # Create the directory if it doesn't exist
  if (!dir.exists(new_folder)) {
    dir.create(new_folder, recursive = TRUE)
  }

  # create a filename for the CSV
  output_filename <- "ddf--concepts.csv"

  # Full output path
  output_filepath <- file.path(new_folder, output_filename)

  # Save the CSV
  write.csv(ddf_concepts, file = output_filepath, row.names = FALSE, quote = FALSE, na="", fileEncoding = "UTF-8")

  ##############################################################################


  ################################Entity code###################################

  entity <- dataset %>%
    select(all_of(entity_id), all_of(entity_name))

  entity <- unique(entity)

  entity <- cbind(entity, rep(TRUE))
  colnames(entity) <- c(entity_id, "name", paste0("is--", entity_id))


  # create a filename for the CSV
  output_filename <- paste0("ddf--entities--geo--", entity_id, ".csv")

  # combine folder path and filename
  output_filepath <- file.path(new_folder, output_filename)

  # save as a CSV file
  write.csv(entity, file = output_filepath, row.names = FALSE, quote = FALSE, na="", fileEncoding = "UTF-8")


  ##############################################################################


  ################################Datapoints####################################

  if (grepl("-", datacolumns)) {
    # if datacolumns is a range ("6-20")
    column_range <- as.integer(strsplit(datacolumns, "-")[[1]])
    datacolumns <- column_range[1]:column_range[2]  # Convert to a sequence of numbers
  } else {
    # if datacolumns is just a list of column numbers
    datacolumns <- as.integer(strsplit(datacolumns, ",")[[1]])
  }

  # filter out the variables not used from the dataset
  dataset <- dataset %>%
    select(all_of(entity_id), all_of(variable_id), all_of(datacolumns))

  # seperate based on variable id
  split_data <- split(dataset, dataset[[variable_id]])

  # separate data sets
  for (name in names(split_data)) {
    assign(paste0("ddf--datapoints--", name, "--by--", entity_id ,"--year"), split_data[[name]])
  }

  # list all datasets
  dataset_names <- ls(pattern = "^ddf--")


  # loop through each dataset
  for (name in dataset_names) {

    # get the dataset by its name
    dataset_format <- get(name)

    # identify the columns to be kept as identifiers
    identifier_cols <- c(entity_id, variable_id)

    long_format <- dataset_format %>%
      pivot_longer(
        cols = (-all_of(identifier_cols)),  # specify identifier columns
        names_to = "year",                # the name for the year column
        values_to = "value"            # the name for the value column
      )

    # rename the 'value' column with the new name to 'variable_id' and then drop 'variable_id'
    new_name <- unique(long_format[[variable_id]])

    long_format <- long_format %>%
      rename(!!new_name := "value") %>%
      select(all_of(entity_id), year, all_of(new_name))


    # Create the new subfolder path
    newer_folder <- file.path(new_folder, "ddf--datapoints")

    # Create the directory if it doesn't exist
    if (!dir.exists(newer_folder)) {
      dir.create(newer_folder, recursive = TRUE)
    }

    # Create the output filename
    output_filename <- paste0(name, ".csv")

    # Full output path
    output_filepath <- file.path(newer_folder, output_filename)

    # Save the CSV
    write.csv(long_format, file = output_filepath, row.names = FALSE, quote = FALSE, na = "", fileEncoding = "UTF-8")

  }



}
