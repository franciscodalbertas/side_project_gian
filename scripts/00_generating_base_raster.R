library(terra)
library(rnaturalearth)
library(sf)

# Load world shapefile from rnaturalearth
world <- ne_countries(scale = "medium", returnclass = "sf")

# Define the Mollweide projection (EPSG 54009)
mollweide_proj <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# Reproject world shapefile to Mollweide
world_moll <- st_transform(world, crs = mollweide_proj)

world_moll$ID <- 1


# Create an empty raster with the specified resolution and extent of the world in Mollweide
r <- rast(res = 1000, crs = mollweide_proj, extent = ext(world_moll),val)
# Assign a value of 1 to all pixels in the raster
values(r) <- 1
# Check and plot the raster to verify
plot(r)


# Convert to SpatVector
world_vect <- vect(world_moll)

country_raster <- rasterize(world_vect, r, field = "ID")
plot(country_raster)

# create dir to store rasters

dir.create("rasters/")

# Save the raster with LZW compression
writeRaster(country_raster, "rasters/world_base_moll_1km.tif", 
            filetype = "GTiff", 
            datatype = "INT1U", # Suitable for integer data like IDs or binary values
            gdal = c("COMPRESS=LZW"))
