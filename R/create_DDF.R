#' Create DDF-filesystem
#'
#' This function creates a filesystem in the DDF-format
#' @param dataset A data frame. The data set to be converted. Data has to be in wide-format.
#' @param variable_id A character variable. The ID connected to each variable. Should only include lower case letters and underscores.
#' @param variable_name A character variable. The Name of each variable.
#' @param entity_id A character variable. The ID connected to each entity. Should only include lower case letters and underscores.
#' @param entity_name A character variable. The Name of each entity.
#' @param datacolumns A range of numeric variables. The columns including data. Should be referenced in the format "1-3" or "1,2,3"
#' @param description (Optional) A character variable. The definition of each variable. Should not include commas or semi-colons.
#' @param source (Optional) A character variable. The source of each variable. Preferably the URL of the source.
#' @param name_short (Optional) A character variable. A shortened version of the variable name.
#' @param name_catalog (Optional) A character variable. The variable name to be shown in the catalog / tree menu.
#' @param tag_id A character variable. The ID connected to each variable. Should only include lower case letters and underscores.
#' @param tag_name A character variable. The Name of each variable.
#' @param parent (Optional) A character variable. The definition of each variable. Should not include commas or semi-colons.
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

create_ddf <- function(dataset, variable_id, variable_name, entity_id, entity_name,
                       datacolumns, tag_id, tag_name, name_short = NULL,
                       name_catalog = NULL, description = NULL, source = NULL,
                       parent = NULL, output_folder = getwd()) {

  # Required packages
  library(dplyr)
  library(tidyr)

  # Possible errors
  if (!dir.exists(output_folder)) {
    stop("The output folder does not exist!")
  }


  ##############################################################################


  ################################Tags##########################################


  if (is.null(parent)) {
    columns_to_select <- c(tag_id, tag_name)
    tag_data <- dataset %>%
      select(all_of(columns_to_select)) %>%
      distinct() %>%
      mutate(parent = NA)  # Add an empty 'parent' column
  } else {
    columns_to_select <- c(tag_id, tag_name, parent)
    tag_data <- dataset %>%
      select(all_of(columns_to_select)) %>%
      distinct()
  }

  #Error if an ID is duplicated
  if (any(duplicated(tag_data[[tag_id]]))) {
    stop("The Tag ID contains duplicates. Resolve before running the function again")
  }

  tag_data <- tag_data %>%
    rename("tag" = .data[[tag_id]],
           "name" = .data[[tag_name]])

  if (!is.null(parent)) {
    tag_data <- tag_data %>%
      rename("parent" = .data[[parent]])
  }

  folder_name <- paste0("ddf_filessystem_", Sys.Date())

  # Create the new subfolder path
  new_folder <- file.path(output_folder, folder_name)

  # Create the directory if it doesn't exist
  if (!dir.exists(new_folder)) {
    dir.create(new_folder, recursive = TRUE)
  }

  # create a filename for the CSV
  output_filename <- "ddf--entities--tag.csv"

  # combine folder path and filename
  output_filepath <- file.path(new_folder, output_filename)

  # save as a CSV file
  write.csv(tag_data, file = output_filepath, row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8", na="")


  ##############################################################################

  ######################### Code from concepts #################################
  required_columns <- c(variable_id, variable_name, entity_id)
  optional_columns <- c(description, source, name_short, name_catalog, tag_id)

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
        name = "Source",
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

  entity <- entity[, c(entity_id, paste0("is--", entity_id), "name")]

  #Error if an ID is duplicated
  if (any(duplicated(entity[[entity_id]]))) {
    stop("The Entity ID contains duplicates. Resolve before running the function again.")
  }

  # create a filename for the CSV
  output_filename <- paste0("ddf--entities--geo--", entity_id, ".csv")

  # combine folder path and filename
  output_filepath <- file.path(new_folder, output_filename)

  # save as a CSV file
  write.csv(entity, file = output_filepath, row.names = FALSE, quote = FALSE, na="", fileEncoding = "UTF-8")


  ##############################################################################


  ################################Datapoints####################################

  # check for duplicate Entity IDs per variable

  duplicates <- dataset %>%
    count(.[[entity_id]], .[[variable_id]]) %>%
    filter(n > 1)

  if (nrow(duplicates) > 0) {
    stop("Duplicate Entity ID(s) are associated with the same variable(s). Resolve before running the function again.")
  }

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
