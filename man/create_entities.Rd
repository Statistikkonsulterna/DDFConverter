% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_entities.R
\name{create_entities}
\alias{create_entities}
\title{Create Entity-file}
\usage{
create_entities(dataset, entity_id, entity_name, output_folder = getwd())
}
\arguments{
\item{dataset}{A data frame. The data set to be converted. Data has to be in wide-format.}

\item{entity_id}{A character variable. The ID connected to each entity. Should only include lower case letters and underscores.}

\item{entity_name}{A character variable. The Name of each entity.}

\item{output_folder}{(Optional) Where the file should be saved. By default set to the working directory.}
}
\value{
Saves a CSV-file in the given output-folder named "ddf--entities--geo--entity_id". This file includes one row for each entity, connecting its ID to the name.
}
\description{
This function creates a concepts-file in the DDF-format
}
\examples{
\dontrun{
create_entities(dataset = dataset, entity_id = "reg_id", entity_name = "Region",
output_folder = "path/to/folder")
}
}
