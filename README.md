# refbank-import

## Structure

* import/ has a subfolder for each paper/project imported
	* project/
		* import.R a Rscript that takes raw_data and produces a wide_table schema-obeying object, and then uses validate.R to write the harmonized data
		* helper scripts are allowed
		* raw_data/ an *uncommitted* folder with raw_data
* harmonized_data/ the folder with the harmonized data -- this folder should only be written to by the validate function!
	* project/ the project in authoryear_firsttitleword format
		* choices.csv
		* conditions.csv
		* messages.csv
		* trials.csv
* validate.R has the validate and write function
* schema.txt has specification for the wide-table format schema

eventually we will also have to find a way to accommodate the target image files and metadata

## Importing
* start a new import subfolder for your project
* follow the wide-table schema in schema.txt
* use a github issue for that dataset to take note on any issues or questions 

## Merging and sharing

`merge_and_export.Rmd` creates both the `export` directory with CSVs and also pushes these to redivis for access by `refbankr`. 
