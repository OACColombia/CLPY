library(auk); #eBird data filters
library(tidyverse); #manipulate data

# Filter ebd and sed global! ####

f_ebd <- "tmp_data/ebd_clpy.txt" #Temporal file to save the filtering eBird data (records)
f_sed <- "tmp_data/sed_clpy.txt" #Temporal file to save the filtering sampling event data

#Filtering only presences to extract range extent in eBird (due to mistakes in some species)
ebd_clpy <- auk_ebd("ebd_clopyo1_smp_relJul-2025/ebd_clopyo1_smp_relJul-2025.txt",
                    file_sampling = "ebd_clopyo1_smp_relJul-2025/ebd_sampling_relJul-2025.txt") |>
  auk_bbox(bbox = c(-80,-1,-75,7)) |>
  auk_complete() |>
  auk_filter(f_ebd, f_sed, overwrite=T) 

saveRDS(ebd_clpy, file = "ebd_clopyo1_smp_relJul-2025/ebd_clpy.RDS")

sed_only <- read_sampling(f_sed) |>
  filter(number_observers < 10,
         effort_distance_km < 5,
         duration_minutes < 300)

ebd_only <- read_ebd(ebd_clpy) |>
  filter(number_observers < 10,
         effort_distance_km < 5,
         duration_minutes < 300)

clpy_zf <- auk_zerofill(ebd_only, sed_only, collapse = T)

saveRDS(clpy_zf, file = "clpy_zf.RDS")

### Occupancy model ####

clpy_zf |>
  filter(species_observed == TRUE) |>
  mutate(Month = month(observation_date)) |> 
  group_by(Month) |>
  summarise(Detections = n()) 

# January to March include the higher number of detections

clpy_zf_JFM <- clpy_zf |>
  mutate(Month = month(observation_date)) |>
  filter(Month %in% c(1:3))

# extract data suitable for occupancy modeling
visits <- filter_repeat_visits(clpy_zf_JFM, 
                               min_obs = 2, max_obs = 10,
                               date_var = "observation_date",
                               annual_closure = TRUE,
                               site_vars = c("latitude", "longitude", 
                                             "observer_id"))

# entire data set
nrow(clpy_zf)
#> [1] 261593
# only January-February-March
nrow(clpy_zf_JFM)
#> [1] 76398
# reduced data set
nrow(visits)
#> [1] 29175
# how many individual sites there are
n_distinct(visits$site)
#> [1] 9399

# Reformat to `unmarked`

visits_um <- format_unmarked_occu(visits, 
                                  site_covs = c("latitude", "longitude"),
                                  obs_covs = c("time_observations_started", 
                                               "duration_minutes", 
                                               "effort_distance_km", 
                                               "number_observers", 
                                               "protocol_name")) #previous version was "protocol_type"

# Elevation


