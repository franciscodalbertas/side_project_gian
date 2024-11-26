# Load necessary library
library(httr)
library(jsonlite)
library(terra)
library(stringr)

# Step 1: Inspect files on the Zenodo page
zenodo_url <- "https://zenodo.org/api/records/7428804"
response <- GET(zenodo_url)
# Parse response as JSON
content <- fromJSON(content(response, "text"))
# Check the structure of the response content
str(content$files)

# Extract filenames and download links from the `content$files` data frame
file_names <- content$files$key
download_links <- content$files$links$self

# Print to verify
# print(file_names)
# print(download_links)

# I can download each file, project to a more reasonable resolution then save to disk

# get probability rasters!

prob_rasters <- grep(pattern = "pnv_pct",x = download_links,value = T)

# Download each file
# for (i in seq_along(file_urls)) {
#   download.file(file_urls[i], destfile = file_names[i], mode = "wb")
# }

# Load the base world raster (replace by the reg cost in degrees)
#base_raster <- rast("rasters/world_base_moll_1km.tif")
base_raster <- rast("rasters/nat_regeneration_cost_latlong_1km.tif")
base_raster <- base_raster/base_raster

# Get the file into memory as raw data
  for(i in seq_along(prob_rasters)){
    
  response <- GET(prob_rasters[i])
  file_data <- content(response, "raw")
  
  # Save the content into a temporary file
  tmp_file <- tempfile(fileext = ".tif")
  writeBin(file_data, tmp_file)
  
  
  # ressample the file to 1km using the base r!
  raster_to_resample <- rast(tmp_file) 
  
    # Ensure this base raster is created earlier
    raster_to_resample_reprojected <- project(raster_to_resample,base_raster,"bilinear")
  
  # save with the name of the tile and then its only a matter of mosaicing!
  nm2save <- str_extract(prob_rasters[i], "pnv.*?\\.tif")
  # Save the raster with LZW compression
  writeRaster(raster_to_resample_reprojected, paste0("rasters/nat_reg_latlong/1km_latlong_",nm2save), 
              filetype = "GTiff", 
              datatype = "INT1U", # Suitable for integer data like IDs or binary values
              gdal = c("COMPRESS=LZW"),
              overwrite =T)
  
}
