This R-package uses an existing dataset and easily transforms into a DDF-filesystem, which can be used in the Vizabi tool. 
Five functions are included; 
create_concepts creates only the concepts-file. 
create_entities creates only the entities-file. 
create_datapoints creates only datapoint-files. 
create_tags creates only the tag-file. 
create_ddf creates an entire filesystem containing concepts, entities, tags and datapoints. 

NOTE: A datapackage-generator is not included. To validate your DDF-filesystem use the built-in validator in the Gapminder Offline-tool, or the terminal. More info on validating here: https://github.com/Gapminder/ddf-validation.
