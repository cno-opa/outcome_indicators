library(tidyverse)
library(readxl)


# copy source files ------------------------------------------------------
source_folder <-
	'O:/projects/resultsnola outcome indicators/'

files_from_shared_drive <-
	dir(source_folder, 'outcome.*xls', full.names = TRUE)
file.copy(files_from_shared_drive, 'data/')


# calculation ------------------------------------------------------------

summarise_crime_rates <- function(source_data,
																	comparison_cities =
																		c(
																			'New Orleans',
																			'Baton Rouge',
																			'Miami',
																			'Tampa',
																			'Atlanta',
																			'Louisville Metro',
																			'Oklahoma City',
																			'Memphis',
																			'Nashville Metropolitan',
																			'Raleigh'
																		)) {
	stats_table <- source_data %>%
		filter(city %in% comparison_cities,
					 population > 100000) %>%
		mutate(
			benchmark = if_else(
				city == 'New Orleans',
				"New Orleans",
				"Benchmark (excl. New Orleans)"
			),
			city_count = 1
		) %>%
		group_by(benchmark) %>%
		summarise_at(
			vars(
				population,
				murder_and_nonnegligent_manslaughter,
				violent_crime,
				property_crime,
				city_count
			),
			~ sum(.x)
		) %>%
		ungroup()
	
	stats_table <- stats_table %>%
		summarise_at(
			vars(
				population,
				murder_and_nonnegligent_manslaughter,
				violent_crime,
				property_crime,
				city_count
			),
			~ sum(.x)
		) %>%
		mutate(benchmark = "Benchmark (incl. New Orleans)") %>%
		bind_rows(stats_table)
	
	percentages <- stats_table %>%
		mutate(
			murder_rate = murder_and_nonnegligent_manslaughter / city_count,
			property_crime_rate = property_crime / population * 100000,
			violent_crime_rate = violent_crime / population * 100000
		)
	
}

# confirm methodology from prior years -----------------------------------


## 2015 year source
prior_year_source <- read_excel(
	'data/source data/Table_8_Offenses_Known_to_Law_Enforcement_by_State_by_City_2015.xls',
	skip = 3
) %>%
	set_names(gsub(' ', '_', tolower(names(.)))) %>%
	set_names(gsub('[^a-z]', '_', names(.)))

comparison <- summarise_crime_rates(prior_year_source)




# calculate 2016 ---------------------------------------------------------

source_2016 <- read_excel(
	'data/source data/Table_6_Offenses_Known_to_Law_Enforcement_by_State_by_City_2016.xls',
	skip = 3
) %>%
	set_names(gsub(' ', '_', tolower(names(.)))) %>%
	set_names(gsub('[^a-z]', '_', names(.)))

comparison_2016 <- summarise_crime_rates(source_2016) 




# save output to csv -----------------------------------------------------
comparison_2016 %>%
	write_csv('data/ucr_summary_2016.csv')

