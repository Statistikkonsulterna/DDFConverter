#' Create Entity-file
#'
#' This function creates a concepts-file in the DDF-format
#' @param dataset A data frame. The data set to be converted. Data has to be in long-format.
#' @param entity_id A character variable. The ID connected to each entity. Should only include lower case letters and underscores.
#' @param entity_name A character variable. The Name of each entity.
#' @param output_folder (Optional) Where the file should be saved. By default set to the working directory.
#' @returns Saves a CSV-file in the given output-folder named "ddf--entities--geo--entity_id". This file includes one row for each entity, connecting its ID to the name.
#' @examples
#' \dontrun{
#' create_entities(dataset = dataset, entity_id = "reg_id", entity_name = "Region",
#' output_folder = "path/to/folder")
#' }
#' @import dplyr
#' @import tidyr
#' @import utils
#' @export


create_entities <- function(dataset, entity_id, entity_name, output_folder = getwd()) {

  # Required packages
  library(dplyr)
  library(tidyr)

  # Possible errors
  if (!dir.exists(output_folder)) {
    stop("The output folder is not included!")
  }

  if (!all(c(entity_id, entity_name) %in% colnames(dataset))) {
    stop("One or more column names do not exist in the dataset.")
  }


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
  output_filepath <- file.path(output_folder, output_filename)

  # save as a CSV file
  write.csv(entity, file = output_filepath, row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8", na="")
}
