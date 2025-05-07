#' Create Tag-file
#'
#' This function creates a tag-file in the DDF-format
#' @param dataset A data frame. The data set to be converted. Data has to be in wide-format.
#' @param tag_id A character variable. The ID connected to each variable. Should only include lower case letters and underscores.
#' @param tag_name A character variable. The Name of each variable.
#' @param parent (Optional) A character variable. The definition of each variable. Should not include commas or semi-colons.
#' @param output_folder (Optional) Where the file should be saved. By default set to the working directory.
#' @returns Saves a CSV-file in the given output-folder named "ddf--entities--tag". This file includes one row for each named tag ID with name, and optionally parent tag.
#' @examples
#' \dontrun{
#' create_tags(dataset = dataset, tag_id = "tag_id", tag_name = "Name",
#'  output_folder = "path/to/folder")
#'}
#' @import dplyr
#' @import tidyr
#' @import utils
#' @export




create_tags <- function(dataset, tag_id, tag_name, parent = NULL, output_folder = getwd()){


  # Required packages
  library(dplyr)
  library(tidyr)

  # Possible errors
  if (!dir.exists(output_folder)) {
    stop("The output folder is not included!")
  }

  if (!all(c(tag_id, tag_name) %in% colnames(dataset))) {
    stop("One or more column names do not exist in the dataset.")
  }

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

  # create a filename for the CSV
  output_filename <- "ddf--entities--tag.csv"

  # combine folder path and filename
  output_filepath <- file.path(output_folder, output_filename)

  # save as a CSV file
  write.csv(tag_data, file = output_filepath, row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8", na="")
}
