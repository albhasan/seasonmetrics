################################################################################
#                       INSTALLING AND LOADING REQUIRED PACKAGES               #
################################################################################
# Pacotes utilizados
pacotes <- c("dplyr", "sf", "tidyr", "purrr")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = TRUE)
    break()}
  sapply(pacotes, require, character.only = TRUE) 
} else {
  sapply(pacotes, require, character.only = TRUE) 
}


################################################################################
#                READING THE DATA AND CREATE MONTH_YEAR COLUMN                 #
################################################################################
#Yearly global VIIRS active fires were downloaded from NASA/FIRMS:
#https://firms.modaps.eosdis.nasa.gov/download/
# Set the working directory to the location of your files
setwd("E:/Bds_ArcGis/Fire_Calendar")

# List all CSV files in the directory
csv_files <- list.files(pattern = "*.csv")

# Initialize an empty dataframe to store combined results
combined_fire_data <- data.frame()


################################################################################
#                           CREATE EMPTY GRID                                  #
################################################################################
# Specify the dimensions based on your information
num_rows <- 720
num_cols <- 1440

# Generate latitude and longitude grids based on the dimensions
latitude <- seq(89.875, -89.875, length.out = num_rows)
longitude <- seq(-179.875, 179.875, length.out = num_cols)

# Create a dataframe with a single column containing integers from 1 to 1036800
fire_grid <- expand.grid(Longitude = longitude, Latitude = latitude)
fire_grid$Cell_Number <- 1:nrow(fire_grid)

# Reordering the columns
fire_grid <- fire_grid[, c("Cell_Number", "Latitude", "Longitude")]

# Function to create polygons for each cell
create_polygon <- function(cell) {
  lat <- cell$Latitude
  lon <- cell$Longitude
  
  # Define the four corners of the cell
  lon_min <- lon - (360 / num_cols) / 2
  lon_max <- lon + (360 / num_cols) / 2
  lat_min <- lat - (180 / num_rows) / 2
  lat_max <- lat + (180 / num_rows) / 2
  
  # Create a polygon for the cell
  polygon <- st_polygon(list(matrix(c(lon_min, lat_min,
                                      lon_min, lat_max,
                                      lon_max, lat_max,
                                      lon_max, lat_min,
                                      lon_min, lat_min), 
                                    ncol = 2, byrow = TRUE)))
  
  return(polygon)
}

# Apply the function to each cell in the fire_grid
fire_grid_polygons <- fire_grid %>%
  rowwise() %>%
  mutate(geometry = list(create_polygon(cur_data())))

# Convert to an sf object
fire_grid_sf <- st_as_sf(fire_grid_polygons, crs = 4326)

# Initialize an empty list to store intermediate results
results_list <- list()


################################################################################
#                               SPATIAL JOIN                                   #
################################################################################
# Loop through each CSV file and process
for (file in csv_files) {
  fire_data <- read.csv(file)
  
  # Select only the latitude, longitude, and acq_date columns
  fire_data <- fire_data %>% select(latitude, longitude, acq_date)
  
  # Convert acq_date to Date format
  fire_data$acq_date <- as.Date(fire_data$acq_date)
  
  # Create Month_Year column
  fire_data <- fire_data %>%
    mutate(Month_Year = format(acq_date, "%Y_%m"))
  
  # Select only the desired columns
  fire_data <- fire_data %>% select(latitude, longitude, Month_Year)
  
  # Convert fire data to sf object
  fire_data_sf <- st_as_sf(fire_data, coords = c("longitude", "latitude"), crs = 4326)
  
  # Perform spatial join to assign points to grid cells
  fire_data_joined <- st_join(fire_data_sf, fire_grid_sf, join = st_within)
  
  # Drop geometry and count number of observations within each grid cell per month
  fire_data_counts <- fire_data_joined %>%
    st_drop_geometry() %>%
    count(Cell_Number, Month_Year) %>%
    pivot_wider(names_from = Month_Year, values_from = n, values_fill = list(n = 0))
  
  # Add the result to the list
  results_list[[file]] <- fire_data_counts
  
  # Clear memory before processing the next file
  rm(fire_data, fire_data_sf, fire_data_joined, fire_data_counts)
  gc()
}

# Combine all results using purrr::reduce and full_join
combined_results <- purrr::reduce(results_list, full_join, by = "Cell_Number")

# Reorder columns sequentially by Month_Year
ordered_months <- sort(colnames(combined_results)[-1]) # Get sorted Month_Year columns
combined_results <- combined_results %>%
  select(Cell_Number, all_of(ordered_months))

# Sum the counts per month using an anonymous function
monthly_sums <- combined_results %>%
  summarise(across(matches("^201|^202"), ~sum(.x, na.rm = TRUE)))

# Join combined_results to fire_grid and replace NAs with zero
fire_grid_with_counts <- left_join(fire_grid, combined_results, by = "Cell_Number") %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

# Sum the counts per month using an anonymous function
monthly_sums <- fire_grid_with_counts %>%
  summarise(across(matches("^201|^202"), ~sum(.x, na.rm = TRUE)))

# Print the sum of counts per month
print(monthly_sums)


################################################################################
#                               EXPORT AS CSV                                  #
################################################################################
# Export the fire_grid_with_counts as a CSV file
output_csv <- "001_Monthly_Gridded_VIIRS_Active_Fires.csv"
write.csv(fire_grid_with_counts, output_csv, row.names = TRUE)

# Print confirmation
cat("CSV exported successfully:", output_csv, "\n")


################################################################################
#                               EXPORT AS SHAPEFILE                            #
################################################################################
# Convert fire_grid_with_counts to an sf object
fire_grid_with_counts_sf <- st_as_sf(fire_grid_with_counts, coords = c("Longitude", "Latitude"), crs = 4326)

# Export the merged_sf as a shapefile
output_shapefile <- "001_Monthly_Gridded_VIIRS_Active_Fires.shp"
st_write(fire_grid_with_counts_sf, output_shapefile, driver = "ESRI Shapefile", delete_layer = TRUE)

# Print confirmation
cat("Shapefile exported successfully:", output_shapefile, "\n")
