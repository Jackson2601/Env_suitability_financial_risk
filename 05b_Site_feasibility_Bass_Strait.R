
#----Site feasibility - Bass Strait
#
#----Test framework using wind and aquaculture as examples
#
# 13/05/2024


#-------At start-up--------#

library(tidyverse)
library(here)
library(sf)
library(terra)
library(tmap)
library(raster)
library(biscale)
library(cowplot)

# Import projected rasters - NEEDED FROM LINE 164
wind_BS_prj <- rast(here('output/wind_BS_prj.tif'))
AC_fish_BS_prj <- rast(here('output/AC_fish_BS_prj.tif'))
AC_sw_BS_prj <- rast(here('output/AC_sw_BS_prj.tif'))

#--------------------------#

#----STUDY AREA----#

# Projected crs
prj <- 'EPSG:28355'

# Import Bass Strait shapefile
BS <- st_read(here('data/BS.gpkg'))

# Project
BS_prj <- st_transform(BS, prj)

# Add column to rasterise
BS_prj$value <- 99

# Rasterise Bass Strait shapefile
BS_r <- rasterize(BS_prj, rast(BS_prj, res = 2000), 
                  field = 'value', background = NA)


#----FINANCIAL FEASIBILITY----#

# Read in layer rasters
layers_FinRisk_files <- list.files(here('output/layers_risk'), full.names = T)
layers_FinRisk_r <- lapply(layers_FinRisk_files, rast)

# # Check
# lapply(layers_FinRisk_r, summary)

# Combine into SpatRaster
layers_FinRisk_sprc <- sprc(layers_FinRisk_r)

# Assign names
names(layers_FinRisk_sprc) <- tools::file_path_sans_ext(basename(layers_FinRisk_files))


#----APPLY FRAMEWORK----#

### Create risk map

# Combine using the sum of grid cell values
FinRisk_BS_r <- mosaic(layers_FinRisk_sprc, fun = 'sum')

# Resample to Bass Strait extent
FinRisk_BS_r_ext <- resample(FinRisk_BS_r, BS_r)


## Invert and rescale values

# Get minimum and maximum value
FinRisk_minmax <- minmax(FinRisk_BS_r_ext)

# # Invert values so that high values equal high feasibility
# FinRisk_BS_r_inv <- FinRisk_minmax[2,] - FinRisk_BS_r_ext

# Rescale values to 0-1
FinRisk_BS_01 <- (FinRisk_BS_r_inv - FinRisk_minmax[1,]) / 
  (FinRisk_minmax[2,] - FinRisk_minmax[1,])

# Rescale values to 0-1
FinRisk_BS_01 <- (FinRisk_BS_r_ext - FinRisk_minmax[1,]) / 
  (FinRisk_minmax[2,] - FinRisk_minmax[1,])


## Need to reduce the extent slightly

# # Get current extent
# ext(risk_BS_01)
# # 143548.774615057, 766548.774615057, 5348695.67164947, 5915695.67164947 (xmin, xmax, ymin, ymax)

# Crop to new extent
FinRisk_BS_01 <- crop(FinRisk_BS_01, c(160000, 756548.774615057, 5348695.67164947, 5905695.67164947))

# Plot
FinRisk_map <- 
  tm_shape(FinRisk_BS_01) +
  tm_raster(title = 'Financial risk',
            style = 'cont',
            palette = 'Reds', 
            textNA = 'Land',
            legend.reverse = T) +
  tm_shape(BS_prj) +
  tm_polygons(col = 'grey90',
              legend.show = F) +
  tm_layout(legend.outside = T,
            legend.title.size = 2,
            legend.text.size = 1)
FinRisk_map

# # Save
# writeRaster(FinRisk_BS_01, here('output/layers_combined/FinRisk_BS_01.tif'), overwrite = T)


#----ENVIRONMENTAL SUITABILITY----#

### Import environmental suitability data

## Wind

# Read in wind suitability rasters
layers_wind_files <- list.files(here('output/layers_wind'), full.names = T)
layers_wind_r <- lapply(layers_wind_files, rast)

# # Check
# lapply(layers_wind_r, summary)

# Combine into SpatRaster
layers_wind_sprc <- sprc(layers_wind_r)

# Assign names
names(layers_wind_sprc) <- tools::file_path_sans_ext(basename(layers_wind_files))

# Combine using the sum of grid cell values
wind_BS_r <- mosaic(layers_wind_sprc, fun = 'sum')

# Rename value name
names(wind_BS_r) <- 'Wind_suitability'

# # Save
# writeRaster(wind_BS_r, here('output/layers_combined/wind_BS_r.tif'), overwrite = T)


## Aquaculture

# Finfish aquaculture potential data
AC_fish_potential_BS <- rast(here('data/AC_fish_resample_BS.tif'))

# Seaweed aquaculture potential data
AC_sw_potential_BS <- rast(here('data/AC_seaweed.tif'))

# Project
wind_BS_prj <- project(wind_BS_r, prj)
AC_fish_BS_prj <- project(AC_fish_potential_BS, prj)
AC_sw_BS_prj <- project(AC_sw_potential_BS, prj)

# # Save projected data
# writeRaster(wind_BS_prj, here('output/wind_BS_prj.tif'), overwrite = T)
# writeRaster(AC_fish_BS_prj, here('output/AC_fish_BS_prj.tif'), overwrite = T)
# writeRaster(AC_sw_BS_prj, here('output/AC_sw_BS_prj.tif'), overwrite = T)


## Remove land areas

# Resample to Bass Strait extent
wind_BS_prj <- resample(wind_BS_prj, BS_r)
AC_fish_BS_prj <- resample(AC_fish_BS_prj, BS_r)
AC_sw_BS_prj <- resample(AC_sw_BS_prj, BS_r)

# # Convert NAs to 0
# wind_BS_prj[is.na(wind_BS_prj[])] <- 0
# AC_fish_BS_prj[is.na(AC_fish_BS_prj[])] <- 0
# AC_sw_BS_prj[is.na(AC_sw_BS_prj[])] <- 0

# # Mask with Bass Strait
# wind_BS_msk <- mask(wind_BS_prj, BS_r, maskvalue = 99, updatevalue = NA)
# AC_fish_BS_msk <- mask(AC_fish_BS_prj, BS_r, maskvalue = 99, updatevalue = NA)
# AC_sw_BS_msk <- mask(AC_sw_BS_prj, BS_r, maskvalue = 99, updatevalue = NA)

# Mask with Bass Strait
wind_BS_msk <- mask(wind_BS_prj, BS_r, updatevalue = NA, inverse = T)
AC_fish_BS_msk <- mask(AC_fish_BS_prj, BS_r, updatevalue = NA, inverse = T)
AC_sw_BS_msk <- mask(AC_sw_BS_prj, BS_r, updatevalue = NA, inverse = T)

# Rescale values to 0-1
wind_minmax <- minmax(wind_BS_msk)    
wind_BS_01 <- (wind_BS_msk - wind_minmax[1,]) / 
  (wind_minmax[2,] - wind_minmax[1,])
AC_fish_minmax <- minmax(AC_fish_BS_msk)    
AC_fish_BS_01 <- (AC_fish_BS_msk - AC_fish_minmax[1,]) / 
  (AC_fish_minmax[2,] - AC_fish_minmax[1,])
AC_sw_minmax <- minmax(AC_sw_BS_msk)    
AC_sw_BS_01 <- (AC_sw_BS_msk - AC_sw_minmax[1,]) / 
  (AC_sw_minmax[2,] - AC_sw_minmax[1,])

# # Check
# plot(wind_BS_01, colNA='black')
# plot(AC_fish_BS_01, colNA='black')
# plot(AC_sw_BS_01, colNA='black')


### Combine risk and environmental suitability

# Resample to same extent as risk layer
wind_BS_01_rsmp <- resample(wind_BS_01, FinRisk_BS_01)
AC_fish_BS_01_rsmp <- resample(AC_fish_BS_01, FinRisk_BS_01)
AC_sw_BS_01_rsmp <- resample(AC_sw_BS_01, FinRisk_BS_01)

# Remove land
FinRisk_wind_msk <- mask(FinRisk_BS_01, wind_BS_01_rsmp)
FinRisk_AC_fish_msk <- mask(FinRisk_BS_01, AC_fish_BS_01_rsmp)
FinRisk_AC_sw_msk <- mask(FinRisk_BS_01, AC_sw_BS_01_rsmp)

# Convert to points
FinRisk_wind_sf <- st_as_sf(as.points(FinRisk_wind_msk), crs = prj)
FinRisk_AC_fish_sf <- st_as_sf(as.points(FinRisk_AC_fish_msk), crs = prj)
FinRisk_AC_sw_sf <- st_as_sf(as.points(FinRisk_AC_sw_msk), crs = prj)
wind_sf <- st_as_sf(as.points(wind_BS_01), crs = prj)
AC_fish_sf <- st_as_sf(as.points(AC_fish_BS_01), crs = prj)
AC_sw_sf <- st_as_sf(as.points(AC_sw_BS_01), crs = prj)

# Spatial joins
wind_FinRisk_sf <- st_join(FinRisk_wind_sf, wind_sf)
AC_fish_FinRisk_sf <- st_join(FinRisk_AC_fish_sf, AC_fish_sf)
AC_sw_FinRisk_sf <- st_join(FinRisk_AC_sw_sf, AC_sw_sf)

# Rename columns
wind_FinRisk_sf <- rename(wind_FinRisk_sf, 'Financial_feasibility' = Aquaculture_ras)
AC_fish_FinRisk_sf <- rename(AC_fish_FinRisk_sf, 'Financial_feasibility' = Aquaculture_ras,
                          'AC_fish_suitability' = fish_phi_all_constraints)
AC_sw_FinRisk_sf <- rename(AC_sw_FinRisk_sf, 'Financial_feasibility' = Aquaculture_ras,
                        'AC_sw_suitability' = sum)

# Transform UTM to lat and long for table
wind_FinRisk_sf <- st_transform(wind_FinRisk_sf, 'EPSG:4383')
AC_fish_FinRisk_sf <- st_transform(AC_fish_FinRisk_sf, 'EPSG:4383')
AC_sw_FinRisk_sf <- st_transform(AC_sw_FinRisk_sf, 'EPSG:4383')

# # Save
# st_write(wind_FinRisk_sf, here('output/wind_FinRisk_sf.gpkg'), append = F)
# st_write(AC_fish_FinRisk_sf, here('output/AC_fish_FinRisk_sf.gpkg'), append = F)
# st_write(AC_sw_FinRisk_sf, here('output/AC_sw_FinRisk_sf.gpkg'), append = F)

# Get coordinates
coords_wind <- as.data.frame(st_coordinates(wind_FinRisk_sf))
coords_AC_fish <- as.data.frame(st_coordinates(AC_fish_FinRisk_sf))
coords_AC_sw <- as.data.frame(st_coordinates(AC_sw_FinRisk_sf))

# Convert to dataframes
wind_FinRisk_df <- as.data.frame(wind_FinRisk_sf, cells = T)
AC_fish_FinRisk_df <- as.data.frame(AC_fish_FinRisk_sf, cells = T)
AC_sw_FinRisk_df <- as.data.frame(AC_sw_FinRisk_sf, cells = T)

# Bind to coordinates
wind_FinRisk_df <- cbind(wind_FinRisk_df, coords_wind)
AC_fish_FinRisk_df <- cbind(AC_fish_FinRisk_df, coords_AC_fish)
AC_sw_FinRisk_df <- cbind(AC_sw_FinRisk_df, coords_AC_sw)

# # Save
# write_csv(wind_FinRisk_df, here('output/dataframes/wind_FinRisk_df.csv'))
# write_csv(AC_fish_FinRisk_df, here('output/dataframes/AC_fish_FinRisk_df.csv'))
# write_csv(AC_sw_FinRisk_df, here('output/dataframes/AC_sw_FinRisk_df.csv'))

# Preview
head(wind_FinRisk_df)
head(AC_fish_FinRisk_df)
head(AC_sw_FinRisk_df)


## XY plots

# Wind potential and risk
ggplot(filter(wind_FinRisk_df, Wind_suitability != 0), # Remove zeros
       aes(x = Financial_feasibility, y = Wind_suitability)) +
  geom_point() +
  labs(x = 'Financial risk', y = 'Environmental suitability - wind') +
  theme_classic() +
  theme(axis.title = element_text(size = 22),
        axis.text = element_text(size = 18))

# Finfish AC potential and risk
ggplot(filter(AC_fish_FinRisk_df, AC_fish_suitability != 0), # Remove zeros
       aes(x = Financial_feasibility, y = AC_fish_suitability)) +
  geom_point() +
  labs(x = 'Financial risk', y = 'Environmental suitability - Finfish aquaculture') +
  theme_classic() +
  theme(axis.title = element_text(size = 22),
        axis.text = element_text(size = 18))

# Seaweed AC potential and risk
ggplot(filter(AC_sw_FinRisk_df, AC_sw_suitability != 0), # Remove zeros
       aes(x = Financial_feasibility, y = AC_sw_suitability)) +
  geom_point() +
  labs(x = 'Financial risk', y = 'Environmental suitability - Seaweed aquaculture') +
  theme_classic() +
  theme(axis.title = element_text(size = 22),
        axis.text = element_text(size = 18))


# ## Bivariate maps
# 
# # Convert risk raster to polygons
# FinRisk_poly <- rasterToPolygons(raster(FinRisk_BS_01)) %>%
#   st_as_sf(crs = crs(FinRisk_BS_01))
# 
# # Remove zeros from environmental suitability rasters
# wind_BS_no_zeros <- wind_BS_01
# wind_BS_no_zeros[values(wind_BS_no_zeros == 0)] <- NA
# AC_fish_BS_no_zeros <- AC_fish_BS_01
# AC_fish_BS_no_zeros[values(AC_fish_BS_no_zeros == 0)] <- NA
# AC_sw_BS_no_zeros <- AC_sw_BS_01
# AC_sw_BS_no_zeros[values(AC_sw_BS_no_zeros == 0)] <- NA
# 
# # Convert environmental suitability rasters to polygons
# wind_poly <- rasterToPolygons(raster(wind_BS_no_zeros)) %>%
#   st_as_sf(crs = crs(wind_BS_01))
# AC_fish_poly <- rasterToPolygons(raster(AC_fish_BS_no_zeros)) %>%
#   st_as_sf(crs = crs(AC_fish_BS_01))
# AC_sw_poly <- rasterToPolygons(raster(AC_sw_BS_no_zeros)) %>%
#   st_as_sf(crs = crs(AC_sw_BS_01))
# 
# # Join dataframes
# wind_FinRisk_poly <- st_join(wind_poly, FinRisk_poly) %>%
#   drop_na()
# AC_fish_FinRisk_poly <- st_join(AC_fish_poly, FinRisk_poly) %>%
#   drop_na()
# AC_sw_FinRisk_poly <- st_join(AC_sw_poly, FinRisk_poly) %>%
#   drop_na()
# 
# # # Column names
# # colnames(wind_FinRisk_poly)
# # colnames(AC_fish_FinRisk_poly)
# # colnames(AC_sw_FinRisk_poly)
# 
# # Create bivariate classes
# wind_FinRisk_classes <- bi_class(wind_FinRisk_poly, 
#                               x = Aquaculture_ras, 
#                               y = Wind_suitability, 
#                               style = 'quantile', dim = 3)
# # Finfish AC potential and risk
# AC_fish_FinRisk_classes <- bi_class(AC_fish_FinRisk_poly, 
#                             x = Aquaculture_ras, 
#                             y = fish_phi_all_constraints, 
#                             style = 'quantile', dim = 3)
# # Seaweed AC potential and risk
# AC_sw_FinRisk_classes <- bi_class(AC_sw_FinRisk_poly, 
#                                x = Aquaculture_ras, 
#                                y = sum, 
#                                style = 'quantile', dim = 3)
# 
# # Save
# st_write(wind_FinRisk_classes, here('output/bivariate_data/wind_FinRisk_classes.gpkg'))
# st_write(AC_fish_FinRisk_classes, here('output/bivariate_data/AC_fish_FinRisk_classes.gpkg'))
# st_write(AC_sw_FinRisk_classes, here('output/bivariate_data/AC_sw_FinRisk_classes.gpkg'))
# 
# 
# ## Maps
# 
# # Project BS_prj
# BS_prj <- st_transform(BS_prj, crs = st_crs(wind_FinRisk_classes))
# 
# # Wind potential and risk
# wind_FinRisk_map <- ggplot(wind_FinRisk_classes, aes(col = bi_class)) +
#   #geom_sf(data = BS_prj, col = 'grey65') +
#   geom_sf(lwd = 0, show.legend = F) +
#   #bi_scale_fill(pal = 'PurpleGrn', dim = 3) +
#   bi_scale_color(pal = 'PurpleGrn', dim = 3) +
#   bi_theme()
# wind_FinRisk_map
# 
# # Finfish AC potential and risk
# AC_fish_FinRisk_map <- ggplot(AC_fish_FinRisk_classes, aes(col = bi_class)) +
#   geom_sf(data = BS_prj, col = 'grey65') +
#   geom_sf(lwd = 0, show.legend = F) +
#   #bi_scale_fill(pal = 'PurpleGrn', dim = 3) +
#   bi_scale_color(pal = 'PurpleGrn', dim = 3) +
#   bi_theme()
# AC_fish_FinRisk_map
# 
# # Seaweed AC potential and risk
# AC_sw_FinRisk_map <- ggplot(AC_sw_FinRisk_classes, aes(col = bi_class)) +
#   geom_sf(data = BS_prj, col = 'grey65') +
#   geom_sf(lwd = 0, show.legend = F) +
#   #bi_scale_fill(pal = 'PurpleGrn', dim = 3) +
#   bi_scale_color(pal = 'PurpleGrn', dim = 3) +
#   bi_theme()
# AC_sw_FinRisk_map
# 
# # Make legends
# legend <- bi_legend(pal = 'PurpleGrn',
#                     dim = 3,
#                     xlab = 'Financial risk',
#                     ylab = 'Env. suitability',
#                     size = 20)
# 
# # Save - weird output so need to manually export using plot window
# ggsave(here('output/figures/wind_FinRisk_map.png'), wind_FinRisk_map)
# ggsave(here('output/figures/AC_fish_FinRisk_map.png'), AC_fish_FinRisk_map)
# ggsave(here('output/figures/AC_sw_FinRisk_map.png'), AC_sw_FinRisk_map)
# ggsave(here('output/figures/legend.png'), legend)


# ### Bivariate map with wind budget constraint
# 
# ## We will use an equal weighting for env suitability and financial feasibility
# 
# # Combine suitability and feasibility using objective function
# wind_suit_feas_r <- 0.5*wind_BS_01_rsmp + (1 - 0.5)*FinRisk_wind_msk
# 
# # Convert to dataframe
# wind_suit_feas_poly <- rasterToPolygons(raster(wind_suit_feas_r)) %>%
#   st_as_sf(crs = crs(wind_suit_feas_r))

