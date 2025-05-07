<<<<<<< HEAD
#' Create datapoint-files
#'
#' This function creates one datapoint-file for each variable in the data set in the DDF-format
#' @param dataset A data frame. The data set to be converted. Data has to be in wide-format.
#' @param variable_id A character variable. The ID connected to each variable. Should only include lower case letters and underscores.
#' @param entity_id A character variable. The ID connected to each entity. Should only include lower case letters and underscores.
#' @param datacolumns A range of numeric variables. The columns including data. Should be referenced in the format "1-3" or "1,2,3"
#' @param output_folder (Optional) Where the file should be saved. By default set to the working directory.
#' @returns Saves a folder in the given output-folder named "ddf--datapoints". This folder includes one CSV-file for each unique variable in the data set.
#' @examples
#' \dontrun{
#' create_datapoints(dataset = dataset, variable_id = "var_id", entity_id = "reg_id",
#' datacolumns = "7-21", output_folder = get_wd())
#'}
#' @import dplyr
#' @import tidyr
#' @import utils
#' @export

  # Create a datapoint-file for each variable
create_datapoints <- function(dataset, entity_id, variable_id, datacolumns, output_folder = getwd()) {

  # Required packages
  library(dplyr)
  library(tidyr)

  # Possible errors
  if (!dir.exists(output_folder)) {
    stop("The output folder is not included!")
  }

  if (!all(c(entity_id, variable_id) %in% colnames(dataset))) {
    stop("One or more column names do not exist in the dataset.")
  }

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
    # if datacolumns is just a list of column numbers seperated by ','
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
    new_folder <- file.path(output_folder, "ddf--datapoints")

    # Create the directory if it doesn't exist
    if (!dir.exists(new_folder)) {
      dir.create(new_folder, recursive = TRUE)
    }

    # Create the output filename
    output_filename <- paste0(name, ".csv")

    # Full output path
    output_filepath <- file.path(new_folder, output_filename)

    # Save the CSV
    write.csv(long_format, file = output_filepath, row.names = FALSE, quote = FALSE, na = "", fileEncoding = "UTF-8")

  }

}


=======
#' Create datapoint-files
#'
#' This function creates one datapoint-file for each variable in the data set in the DDF-format
#' @param dataset A data frame. The data set to be converted. Data has to be in wide-format.
#' @param variable_id A character variable. The ID connected to each variable. Should only include lower case letters and underscores.
#' @param entity_id A character variable. The ID connected to each entity. Should only include lower case letters and underscores.
#' @param datacolumns A range of numeric variables. The columns including data. Should be referenced in the format "1-3" or "1,2,3"
#' @param output_folder (Optional) Where the file should be saved. By default set to the working directory.
#' @returns Saves a folder in the given output-folder named "ddf--datapoints". This folder includes one CSV-file for each unique variable in the data set.
#' @examples
#' \dontrun{
#' create_datapoints(dataset = dataset, variable_id = "var_id", entity_id = "reg_id",
#' datacolumns = "7-21", output_folder = get_wd())
#'}
#' @import dplyr
#' @import tidyr
#' @import utils
#' @export

  # Create a datapoint-file for each variable
create_datapoints <- function(dataset, entity_id, variable_id, datacolumns, output_folder = getwd()) {

  # Required packages
  library(dplyr)
  library(tidyr)

  # Possible errors
  if (!dir.exists(output_folder)) {
    stop("The output folder is not included!")
  }

  if (!all(c(entity_id, variable_id) %in% colnames(dataset))) {
    stop("One or more column names do not exist in the dataset.")
  }

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
    # if datacolumns is just a list of column numbers seperated by ','
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
    new_folder <- file.path(output_folder, "ddf--datapoints")

    # Create the directory if it doesn't exist
    if (!dir.exists(new_folder)) {
      dir.create(new_folder, recursive = TRUE)
    }

    # Create the output filename
    output_filename <- paste0(name, ".csv")

    # Full output path
    output_filepath <- file.path(new_folder, output_filename)

    # Save the CSV
    write.csv(long_format, file = output_filepath, row.names = FALSE, quote = FALSE, na = "", fileEncoding = "UTF-8")

  }

}


>>>>>>> 3d398c5faf9858dfda9da70da5d8b45335bd66de
