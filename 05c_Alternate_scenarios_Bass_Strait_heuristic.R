
#----Site feasibility - Bass Strait
#
#----Alternate site selection scenarios using wind and aquaculture as examples
#
#----Heuristic rules of thumb
#
# 11/06/2024


#-------At start-up--------#

library(tidyverse)
library(here)
library(sf)
library(patchwork)
library(fields)
library(terra)
library(png)
library(grid)

sf_use_s2(F)

#--------------------------#

### We will generate curves representing different site selection scenarios
### by ordering the feasibility/suitability columns depending on management priorities

# Import environmental suitability and feasibility dataframes
wind_FinRisk_df <- read_csv(here('output/dataframes/wind_FinRisk_df.csv'))
AC_fish_FinRisk_df <- read_csv(here('output/dataframes/AC_fish_FinRisk_df.csv'))
AC_sw_FinRisk_df <- read_csv(here('output/dataframes/AC_sw_FinRisk_df.csv'))

# # Add column with combined risk and suitability
# wind_FinRisk_df$combined <- wind_FinRisk_df$Wind_suitability * 0.5 + wind_FinRisk_df$Financial_feasibility * (1-0.5)
# AC_fish_FinRisk_df$combined <- AC_fish_FinRisk_df$AC_fish_suitability * 0.5 + AC_fish_FinRisk_df$Financial_feasibility * (1-0.5)
# AC_sw_FinRisk_df$combined <- AC_sw_FinRisk_df$AC_sw_suitability * 0.5 + AC_sw_FinRisk_df$Financial_feasibility * (1-0.5)


#----Plotting financial feasibility----#

## Prioritising reducing financial risk

# Reorder financial feasibility column - lowest to highest (to represent risk)
wind_high_FinRisk <- wind_FinRisk_df %>%
  arrange(Financial_feasibility)
AC_fish_high_FinRisk <- AC_fish_FinRisk_df %>%
  arrange(Financial_feasibility)
AC_sw_high_FinRisk <- AC_sw_FinRisk_df %>%
  arrange(Financial_feasibility)

# Cumulative sum of financial feasibility
wind_high_FinRisk$FinRisk_sum <- cumsum(wind_high_FinRisk$Financial_feasibility)
AC_fish_high_FinRisk$FinRisk_sum <- cumsum(AC_fish_high_FinRisk$Financial_feasibility)
AC_sw_high_FinRisk$FinRisk_sum <- cumsum(AC_sw_high_FinRisk$Financial_feasibility)

# Cumulative sum of env suitability
wind_high_FinRisk$EnvSuit_sum <- cumsum(wind_high_FinRisk$Wind_suitability)
AC_fish_high_FinRisk$EnvSuit_sum <- cumsum(AC_fish_high_FinRisk$AC_fish_suitability)
AC_sw_high_FinRisk$EnvSuit_sum <- cumsum(AC_sw_high_FinRisk$AC_sw_suitability)

# Add cell column
wind_high_FinRisk$Cell <- 1:nrow(wind_high_FinRisk)
AC_fish_high_FinRisk$Cell <- 1:nrow(AC_fish_high_FinRisk)
AC_sw_high_FinRisk$Cell <- 1:nrow(AC_sw_high_FinRisk)

# Add column with priority
wind_high_FinRisk$Priority <- 'Reduce cumulative conflict risk'
AC_fish_high_FinRisk$Priority <- 'Reduce cumulative conflict risk'
AC_sw_high_FinRisk$Priority <- 'Reduce cumulative conflict risk'


## Prioritising environmental suitability

# Reorder environmental suitability column - highest to lowest
wind_high_suit <- wind_FinRisk_df %>%
  arrange(desc(Wind_suitability))
AC_fish_high_suit <- AC_fish_FinRisk_df %>%
  arrange(desc(AC_fish_suitability))
AC_sw_high_suit <- AC_sw_FinRisk_df %>%
  arrange(desc(AC_sw_suitability))

# Cumulative sum of financial feasibility
wind_high_suit$FinRisk_sum <- cumsum(wind_high_suit$Financial_feasibility)
AC_fish_high_suit$FinRisk_sum <- cumsum(AC_fish_high_suit$Financial_feasibility)
AC_sw_high_suit$FinRisk_sum <- cumsum(AC_sw_high_suit$Financial_feasibility)

# Cumulative sum of env suitability
wind_high_suit$EnvSuit_sum <- cumsum(wind_high_suit$Wind_suitability)
AC_fish_high_suit$EnvSuit_sum <- cumsum(AC_fish_high_suit$AC_fish_suitability)
AC_sw_high_suit$EnvSuit_sum <- cumsum(AC_sw_high_suit$AC_sw_suitability)

# Add cell column
wind_high_suit$Cell <- 1:nrow(wind_high_suit)
AC_fish_high_suit$Cell <- 1:nrow(AC_fish_high_suit)
AC_sw_high_suit$Cell <- 1:nrow(AC_sw_high_suit)

# Add column with priority
wind_high_suit$Priority <- 'Increase env. suitability'
AC_fish_high_suit$Priority <- 'Increase env. suitability'
AC_sw_high_suit$Priority <- 'Increase env. suitability'


# ## Equal weighting
# 
# # Reorder environmental suitability column - highest to lowest
# wind_combined <- wind_FinRisk_df %>%
#   arrange(desc(combined))
# AC_fish_combined <- AC_fish_FinRisk_df %>%
#   arrange(desc(combined))
# AC_sw_combined <- AC_sw_FinRisk_df %>%
#   arrange(desc(combined))
# 
# # Cumulative sum of financial feasibility
# wind_combined$FinRisk_sum <- cumsum(wind_combined$Financial_feasibility)
# AC_fish_combined$FinRisk_sum <- cumsum(AC_fish_combined$Financial_feasibility)
# AC_sw_combined$FinRisk_sum <- cumsum(AC_sw_combined$Financial_feasibility)
# 
# # Add cell column
# wind_combined$Cell <- 1:nrow(wind_combined)
# AC_fish_combined$Cell <- 1:nrow(AC_fish_combined)
# AC_sw_combined$Cell <- 1:nrow(AC_sw_combined)
# 
# # Add column with priority
# wind_combined$Priority <- 'Equal weighting'
# AC_fish_combined$Priority <- 'Equal weighting'
# AC_sw_combined$Priority <- 'Equal weighting'


## Random order

# Reorder environmental suitability column - highest to lowest
wind_random <- wind_FinRisk_df %>%
  arrange(sample(nrow(wind_FinRisk_df)))
AC_fish_random <- AC_fish_FinRisk_df %>%
  arrange(sample(nrow(AC_fish_FinRisk_df)))
AC_sw_random <- AC_sw_FinRisk_df %>%
  arrange(sample(nrow(AC_sw_FinRisk_df)))

# Cumulative sum of financial feasibility
wind_random$FinRisk_sum <- cumsum(wind_random$Financial_feasibility)
AC_fish_random$FinRisk_sum <- cumsum(AC_fish_random$Financial_feasibility)
AC_sw_random$FinRisk_sum <- cumsum(AC_sw_random$Financial_feasibility)

# Cumulative sum of env suitability
wind_random$EnvSuit_sum <- cumsum(wind_random$Wind_suitability)
AC_fish_random$EnvSuit_sum <- cumsum(AC_fish_random$AC_fish_suitability)
AC_sw_random$EnvSuit_sum <- cumsum(AC_sw_random$AC_sw_suitability)

# Add cell column
wind_random$Cell <- 1:nrow(wind_random)
AC_fish_random$Cell <- 1:nrow(AC_fish_random)
AC_sw_random$Cell <- 1:nrow(AC_sw_random)

# Add column with priority
wind_random$Priority <- 'Random'
AC_fish_random$Priority <- 'Random'
AC_sw_random$Priority <- 'Random'


## Join all

# Bind rows
wind_FinRisk_scenarios <- rbind(wind_high_FinRisk, wind_high_suit, wind_random)
AC_fish_FinRisk_scenarios <- rbind(AC_fish_high_FinRisk, AC_fish_high_suit, AC_fish_random)
AC_sw_FinRisk_scenarios <- rbind(AC_sw_high_FinRisk, AC_sw_high_suit, AC_sw_random)


## Convert response to percentage

# Wind
wind_FinRisk_scenarios$FinRisk_perc <- (wind_FinRisk_scenarios$FinRisk_sum/max(wind_FinRisk_scenarios$FinRisk_sum))*100
wind_FinRisk_scenarios$EnvSuit_perc <- (wind_FinRisk_scenarios$EnvSuit_sum/max(wind_FinRisk_scenarios$EnvSuit_sum))*100

# AC - finfish
AC_fish_FinRisk_scenarios$FinRisk_perc <- (AC_fish_FinRisk_scenarios$FinRisk_sum/max(AC_fish_FinRisk_scenarios$FinRisk_sum))*100
AC_fish_FinRisk_scenarios$EnvSuit_perc <- (AC_fish_FinRisk_scenarios$EnvSuit_sum/max(AC_fish_FinRisk_scenarios$EnvSuit_sum))*100

# AC - finfish
AC_sw_FinRisk_scenarios$FinRisk_perc <- (AC_sw_FinRisk_scenarios$FinRisk_sum/max(AC_sw_FinRisk_scenarios$FinRisk_sum))*100
AC_sw_FinRisk_scenarios$EnvSuit_perc <- (AC_sw_FinRisk_scenarios$EnvSuit_sum/max(AC_sw_FinRisk_scenarios$EnvSuit_sum))*100


### Plots

## Import symbols

# Wind
wind_img <- readPNG(here('data/wind-power.png'))
wind_grob <- rasterGrob(wind_img, interpolate = TRUE)

# Fish
fish_img <- readPNG(here('data/fish.png'))
fish_grob <- rasterGrob(fish_img, interpolate = TRUE)

# Seaweed
sw_img <- readPNG(here('data/seaweed.png'))
sw_grob <- rasterGrob(sw_img, interpolate = TRUE)

# Wind budget
wind_bud_img <- readPNG(here('data/dollar.png'))
wind_bud_grob <- rasterGrob(wind_bud_img, interpolate = TRUE)


### Plot

## Wind farming

# Financial feasibility on y axis
wind_scenarios_risk_plot <- 
  ggplot(wind_FinRisk_scenarios, aes(Cell, FinRisk_perc, col = Priority)) +
  geom_smooth(linewidth = 0.7) +
  scale_color_manual(values = c('blue', 'green4', 'red3')) +
  labs(x = '', 
       y = '',
       col = '') +
  guides(col='none') +
  theme_classic() +
  theme(axis.text = element_text(size = 15)) +
  annotation_custom(wind_grob, xmin = 0, xmax = 6000, ymin = 55, ymax = 115)
wind_scenarios_risk_plot

# Env suitability on y axis
wind_scenarios_env_plot <- 
  ggplot(wind_FinRisk_scenarios, aes(Cell, EnvSuit_perc, col = Priority)) +
  geom_smooth(linewidth = 0.7) +
  scale_color_manual(values = c('blue', 'green4', 'red3')) +
  labs(x = '', 
       y = '',
       col = '') +
  guides(col='none') +
  theme_classic() +
  theme(axis.text = element_text(size = 15)) +
  annotation_custom(wind_grob, xmin = 0, xmax = 6000, ymin = 55, ymax = 115)
wind_scenarios_env_plot


## Aquaculture - finfish

# Financial feasibility on y axis
AC_fish_scenarios_risk_plot <- 
  ggplot(AC_fish_FinRisk_scenarios, aes(Cell, FinRisk_perc, col = Priority)) +
  geom_smooth(linewidth = 0.7) +
  scale_color_manual(values = c('blue', 'green4', 'red3', 'black')) +
  labs(x = '', 
       y = '',
       col = '') +
  guides(col='none') +
  theme_classic()  +
  theme(axis.text = element_text(size = 15)) +
  annotation_custom(fish_grob, xmin = 0, xmax = 3700, ymin = 55, ymax = 115)
AC_fish_scenarios_risk_plot

# Env suitability on y axis
AC_fish_scenarios_env_plot <- 
  ggplot(AC_fish_FinRisk_scenarios, aes(Cell, EnvSuit_perc, col = Priority)) +
  geom_smooth(linewidth = 0.7) +
  scale_color_manual(values = c('blue', 'green4', 'red3')) +
  labs(x = '', 
       y = '',
       col = '') +
  guides(col='none') +
  theme_classic()  +
  theme(axis.text = element_text(size = 15)) +
  annotation_custom(fish_grob, xmin = 0, xmax = 3700, ymin = 55, ymax = 115)
AC_fish_scenarios_env_plot


## Aquaculture - seaweed

# Financial feasibility on y axis
AC_sw_scenarios_risk_plot <- 
  ggplot(AC_sw_FinRisk_scenarios, aes(Cell, FinRisk_perc, colour = Priority)) +
  geom_smooth(linewidth = 0.7) +
  scale_color_manual(values = c('blue', 'green4', 'red3')) +
  labs(x = '', 
       y = '',
       col = '') +
  guides(col = 'none') +
  theme_classic() +
  theme(axis.text = element_text(size = 15),
        plot.margin = margin(10, 100, 10, 10)) +
  annotation_custom(sw_grob, xmin = -1000, xmax = 2000, ymin = 55, ymax = 115)
AC_sw_scenarios_risk_plot

# Env suitability on y axis
AC_sw_scenarios_env_plot <- 
  ggplot(AC_sw_FinRisk_scenarios, aes(Cell, EnvSuit_perc, colour = Priority)) +
  geom_smooth(linewidth = 0.7) +
  scale_color_manual(values = c('blue', 'green4', 'red3')) +
  labs(x = '', 
       y = '',
       col = '') +
  guides(col = 'none') +
  theme_classic() +
  theme(axis.text = element_text(size = 15),
        plot.margin = margin(10, 100, 10, 10)) +
  annotation_custom(sw_grob, xmin = -1000, xmax = 2000, ymin = 55, ymax = 115)
AC_sw_scenarios_env_plot


### Find the maximum differences for finfish aquaculture

## Convert dataframes to wide format

# Wind
wind_wide <- wind_FinRisk_scenarios %>%
  pivot_wider(
    id_cols = Cell,
    names_from = Priority,
    values_from = c(FinRisk_perc, EnvSuit_perc)
  )

# Aquaculture - finfish
AC_fish_wide <- AC_fish_FinRisk_scenarios %>%
  pivot_wider(
    id_cols = Cell,
    names_from = Priority,
    values_from = c(FinRisk_perc, EnvSuit_perc)
  )

# Aquaculture - seaweed
AC_sw_wide <- AC_sw_FinRisk_scenarios %>%
  pivot_wider(
    id_cols = Cell,
    names_from = Priority,
    values_from = c(FinRisk_perc, EnvSuit_perc)
  )


## Calculate differences

# Wind
wind_risk_random <- wind_wide$FinRisk_perc_Random-wind_wide$`FinRisk_perc_Reduce cumulative conflict risk`
wind_risk_env <- wind_wide$`FinRisk_perc_Increase env. suitability`-wind_wide$`FinRisk_perc_Reduce cumulative conflict risk`
wind_env_random <- wind_wide$`EnvSuit_perc_Increase env. suitability`-wind_wide$EnvSuit_perc_Random
wind_env_risk <- wind_wide$`EnvSuit_perc_Increase env. suitability`-wind_wide$`EnvSuit_perc_Reduce cumulative conflict risk`

# Aquaculture - finfish
#AC_fish_risk_equal <- AC_fish_wide$`Equal weighting`-AC_fish_wide$`Reduce cumulative conflict risk`
AC_fish_risk_random <- AC_fish_wide$FinRisk_perc_Random-AC_fish_wide$`FinRisk_perc_Reduce cumulative conflict risk`
AC_fish_risk_env <- AC_fish_wide$`FinRisk_perc_Increase env. suitability`-AC_fish_wide$`FinRisk_perc_Reduce cumulative conflict risk`
AC_fish_env_random <- AC_fish_wide$`EnvSuit_perc_Increase env. suitability`-AC_fish_wide$EnvSuit_perc_Random
AC_fish_env_risk <- AC_fish_wide$`EnvSuit_perc_Increase env. suitability`-AC_fish_wide$`EnvSuit_perc_Reduce cumulative conflict risk`

# Aquaculture - seaweed
AC_sw_risk_random <- AC_sw_wide$FinRisk_perc_Random-AC_sw_wide$`FinRisk_perc_Reduce cumulative conflict risk`
AC_sw_risk_env <- AC_sw_wide$`FinRisk_perc_Increase env. suitability`-AC_sw_wide$`FinRisk_perc_Reduce cumulative conflict risk`
AC_sw_env_random <- AC_sw_wide$`EnvSuit_perc_Increase env. suitability`-AC_sw_wide$EnvSuit_perc_Random
AC_sw_env_risk <- AC_sw_wide$`EnvSuit_perc_Increase env. suitability`-AC_sw_wide$`EnvSuit_perc_Reduce cumulative conflict risk`


## Find maximum differences

# Wind
max(wind_risk_random) # 23.01529
max(wind_risk_env) # 22.33683
max(wind_env_random) # 12.08406
max(wind_env_risk) # 13.11409

# Aquaculture - finfish
#max(AC_fish_risk_equal) # 36.63188
max(AC_fish_risk_random) # 19.79057
max(AC_fish_risk_env) # 17.1769
max(AC_fish_env_random) # 4.793361
max(AC_fish_env_risk) # 4.876555

# Aquaculture - seaweed
max(AC_sw_risk_random) # 7.363815
max(AC_sw_risk_env) # 10.3972
max(AC_sw_env_random) # 9.927519
max(AC_sw_env_risk) # 13.92327


### Plot the top 6000 cells to represent wind farm budget area

# Filter to top 6000 cells
wind_FinRisk_budget <- wind_high_FinRisk[1:6000, ]
wind_suit_budget <- wind_high_suit[1:6000, ]
wind_random_budget <- wind_random[1:6000, ]
#wind_combined_budget <- wind_combined[1:6000, ]

# Join all
wind_budget_all <- rbind(wind_FinRisk_budget, wind_suit_budget, wind_random_budget)

# Convert response to percentage
wind_budget_all$FinRisk_perc <- (wind_budget_all$FinRisk_sum/max(wind_budget_all$FinRisk_sum))*100
wind_budget_all$EnvSuit_perc <- (wind_budget_all$EnvSuit_sum/max(wind_budget_all$EnvSuit_sum))*100

# Filter to find values at 6000 cells
filter(wind_budget_all, Cell == 6000)


## Plot wind farm budget

# Financial feasibility on y axis
wind_scenarios_budget_risk_plot <- 
  ggplot(wind_budget_all, aes(Cell, FinRisk_perc, col = Priority)) +
  geom_smooth(linewidth = 0.7, se = F) +
  scale_color_manual(values = c('blue', 'green4', 'red3')) +
  labs(x = '', 
       y = '',
       col = '') +
  theme_classic() +
  theme(axis.text = element_text(size = 15)) +
  guides(col = 'none') +
  annotation_custom(wind_grob, xmin = -1000, xmax = 2200, ymin = 50, ymax = 100) +
  annotation_custom(wind_bud_grob, xmin = -900, xmax = 1200, ymin = 75, ymax = 105)
wind_scenarios_budget_risk_plot

# Env suitability on y axis
wind_scenarios_budget_env_plot <- 
  ggplot(wind_budget_all, aes(Cell, EnvSuit_perc, col = Priority)) +
  geom_smooth(linewidth = 0.7, se = F) +
  scale_color_manual(values = c('blue', 'green4', 'red3')) +
  labs(x = '', 
       y = '',
       col = '') +
  theme_classic() +
  theme(axis.text = element_text(size = 15),
        legend.text = element_text(size = 20),
        legend.key.width = unit(2,'cm')) +
  guides(colour = guide_legend(override.aes = list(linewidth = 2))) +
  annotation_custom(wind_grob, xmin = -1000, xmax = 2200, ymin = 50, ymax = 100) +
  annotation_custom(wind_bud_grob, xmin = -900, xmax = 1200, ymin = 75, ymax = 105)
wind_scenarios_budget_env_plot

# Combine plots - risk
plots_risk_all <- 
  (wind_scenarios_risk_plot + AC_fish_scenarios_risk_plot) /
  (AC_sw_scenarios_risk_plot + wind_scenarios_budget_risk_plot)

# Combine plots - env
plots_env_all <- 
  (wind_scenarios_env_plot + AC_fish_scenarios_env_plot) /
  (AC_sw_scenarios_env_plot + wind_scenarios_budget_env_plot) + plot_layout(tag_level = 'new')

# Add labels - risk
plots_risk_all <- 
  plots_risk_all + plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(size = 25))

# Add labels - env
plots_env_all <- 
  plots_env_all + plot_annotation(tag_levels = list(c('e', 'f', 'g', 'h'))) &
  theme(plot.tag = element_text(size = 25))

# Shared x axis - risk
plots_risk_all_x <-
  wrap_elements(panel = plots_risk_all) +
  labs(tag = '') +
  theme(
    plot.tag = element_text(size = 30),
    plot.tag.position = c(0.4, 0.01)
  )
plots_risk_all_x

# Shared x axis - env
plots_env_all_x <-
  wrap_elements(panel = plots_env_all) +
  labs(tag = 'No.cells') +
  theme(
    plot.tag = element_text(size = 20),
    plot.tag.position = c(0.4, 0.01)
  )
plots_env_all_x

# Shared y label - risk
plots_risk_all_y <-
  wrap_elements(panel = plots_risk_all) +
  labs(tag = 'Cumulative conflict risk') +
  theme(
    plot.tag = element_text(size = 20, angle = 90),
    plot.tag.position = 'left'
  )
plots_risk_all_y

# Shared y label - env
plots_env_all_y <-
  wrap_elements(panel = plots_env_all) +
  labs(tag = 'Environmental suitability') +
  theme(
    plot.tag = element_text(size = 20, angle = 90),
    plot.tag.position = 'left'
  )
plots_env_all_y

# Save
ggsave(here('output/figures/scenarios_risk_all_x.png'), plots_risk_all_x, width = 12)
ggsave(here('output/figures/scenarios_risk_all_y.png'), plots_risk_all_y)
ggsave(here('output/figures/scenarios_env_all_x.png'), plots_env_all_x, width = 16)
ggsave(here('output/figures/scenarios_env_all_y.png'), plots_env_all_y)


#----MAPPING BEST OFFSHORE DEVELOPMENT AREAS----#

### We will use the objective function to combine suitability with feasibility

### Equation from Chris to calculate weighted sum:

### alpha*R  + (1-alpha)*S
### R = financial feasibility
### S = suitability
### alpha = a weighting ranging between 0 and 1


### WIND FARMING

# Invert risk values
wind_all_obj <- wind_FinRisk_df
wind_all_obj$Financial_feasibility_inv <- 1/wind_all_obj$Financial_feasibility

# Equal weighting
wind_all_obj$obj_fun_0.5 <- 
  0.5*wind_all_obj$Financial_feasibility_inv + (1-0.5)*wind_all_obj$Wind_suitability

# Priority - env. suitability
wind_all_obj$obj_fun_0.3 <- 
  0.3*wind_all_obj$Financial_feasibility_inv + (1-0.3)*wind_all_obj$Wind_suitability

# Priority - financial feasibility
wind_all_obj$obj_fun_0.7 <- 
  0.7*wind_all_obj$Financial_feasibility_inv + (1-0.7)*wind_all_obj$Wind_suitability

# Convert to sf
wind_all_obj_sf <- st_as_sf(wind_all_obj,
                            coords = c('X', 'Y'),
                            crs = 'EPSG:4559')


## Arrange by priority and filter to match wind farm budget

# Equal weighting
wind_obj_equal <- wind_all_obj_sf %>%
  arrange(desc(obj_fun_0.5)) %>%
  slice(1:6000)

# Priority - env. suitability
wind_obj_suit <- wind_all_obj_sf %>%
  arrange(desc(obj_fun_0.3)) %>%
  slice(1:6000)

# Priority - financial feasibility
wind_obj_FinRisk <- wind_all_obj_sf %>%
  arrange(desc(obj_fun_0.7)) %>%
  slice(1:6000)


## Maps

# Import Bass Strait shapefile
BS <- st_read(here('data/BS.gpkg'))

# Rasterise wind objects
wind_obj_equal_r <- rasterize(wind_obj_equal, rast(BS, res = 0.03),
                              field = 'obj_fun_0.5')
wind_obj_suit_r <- rasterize(wind_obj_suit, rast(BS, res = 0.03),
                              field = 'obj_fun_0.3')
wind_obj_FinRisk_r <- rasterize(wind_obj_FinRisk, rast(BS, res = 0.03),
                              field = 'obj_fun_0.7')


## Rescale all rasters to between 0 and 1

# Stack all rasters
wind_obj_stack <- c(wind_obj_equal_r, wind_obj_suit_r, wind_obj_FinRisk_r)

# Find global min and max
wind_global_min <- min(global(wind_obj_stack, 'min', na.rm = TRUE)[[1]])
wind_global_max <- max(global(wind_obj_stack, 'max', na.rm = TRUE)[[1]])

# Rescale
wind_equal_scaled <- (wind_obj_equal_r - wind_global_min) / (wind_global_max - wind_global_min)
wind_suit_scaled <- (wind_obj_suit_r - wind_global_min) / (wind_global_max - wind_global_min)
wind_FinRisk_scaled <- (wind_obj_FinRisk_r - wind_global_min) / (wind_global_max - wind_global_min)


## Add declared wind farm areas

# Import declared areas
wind_farms <- st_read(here('data/wind_farms/Offshore_Renewable_Energy_Infrastructure_Regions.shp'))

# Drop z dimension, transform crs, and crop
wind_farms_BS <- wind_farms %>%
  st_zm() %>%
  st_transform(., st_crs(BS)) %>%
  st_crop(., BS)

# Remove superceded areas
wind_farms_BS <- filter(wind_farms_BS, Status == 'Declared')

# Check
plot(st_geometry(wind_farms_BS))
plot(st_geometry(BS), add = T)

# Colour scheme
mycols <- RColorBrewer::brewer.pal(9, 'PuBuGn')

# Create list
wind_scaled_lst <- lst(wind_equal_scaled, wind_suit_scaled, wind_FinRisk_scaled)

# Get the minimum and maximum values for legend
wind_scaled_min <- min(sapply(wind_scaled_lst, function(r) min(values(r), na.rm = TRUE)))
wind_scaled_max <- max(sapply(wind_scaled_lst, function(r) max(values(r), na.rm = TRUE)))

# Set legend breaks
wind_breaks <- seq(wind_scaled_min, wind_scaled_max, 0.2)

# Equal weighting
png(file=here('output/figures/wind_obj_equal_r.png'),
    res=500, width=6, height=5, units = 'in')
plot(wind_equal_scaled, breaks = wind_breaks, col = mycols, legend = F, pax=list(cex.axis = 2))
plot(st_geometry(BS), col = 'grey90', add = T)
plot(st_geometry(wind_farms_BS), border = 'red3', col = NA, lwd = 1.3, add = T)
text(143.5,-37.2, '(a)', cex = 1.8)
north(xy=c(149.5,-41), type=1, cex=1.2, d = 0.25)
sbar(100, c(148.7, -41.75), cex=0.8, label='100 km')
dev.off()

# Priority - env. suitability
png(file=here('output/figures/wind_obj_suit_r.png'),
    res=500, width=6, height=5, units = 'in')
plot(wind_suit_scaled, breaks = wind_breaks, col = mycols, legend = F, pax=list(cex.axis = 2))
plot(st_geometry(BS), col = 'grey90', add = T)
plot(st_geometry(wind_farms_BS), border = 'red3', col = NA, lwd = 1.3, add = T)
text(143.5,-37.2, '(b)', cex = 1.8)
#north(xy=c(149.5,-41.5), type=1, cex=1.2, d = 0.25)
#sbar(100, c(143.4, -41.75), cex=0.8, label='100 km')
dev.off()

# Priority - financial feasibility
png(file=here('output/figures/wind_obj_FinRisk_r.png'),
    res=500, width=6, height=5, units = 'in')
plot(wind_FinRisk_scaled, breaks = wind_breaks, col = mycols, legend = F, pax=list(cex.axis = 2))
plot(st_geometry(BS), col = 'grey90', add = T)
plot(st_geometry(wind_farms_BS), border = 'red3', col = NA, lwd = 1.3, add = T)
text(143.5,-37.2, '(c)', cex = 1.8)
#north(xy=c(149.5,-41.5), type=1, cex=1.2, d = 0.25)
#sbar(100, c(143.4, -41.75), cex=0.8, label='100 km')
dev.off()


### AQUACULTURE

# Invert risk values
AC_fish_obj <- AC_fish_FinRisk_df
AC_sw_obj <- AC_sw_FinRisk_df
AC_fish_obj$Financial_feasibility_inv <- 1/AC_fish_obj$Financial_feasibility
AC_sw_obj$Financial_feasibility_inv <- 1/AC_sw_obj$Financial_feasibility

# Equal weighting
AC_fish_obj$obj_fun_0.5 <- 
  0.5*AC_fish_obj$Financial_feasibility_inv + (1-0.5)*AC_fish_obj$AC_fish_suitability
AC_sw_obj$obj_fun_0.5 <- 
  0.5*AC_sw_obj$Financial_feasibility_inv + (1-0.5)*AC_sw_obj$AC_sw_suitability

# Priority - env. suitability
AC_fish_obj$obj_fun_0.3 <- 
  0.3*AC_fish_obj$Financial_feasibility_inv + (1-0.3)*AC_fish_obj$AC_fish_suitability
AC_sw_obj$obj_fun_0.3 <- 
  0.3*AC_sw_obj$Financial_feasibility_inv + (1-0.3)*AC_sw_obj$AC_sw_suitability

# Priority - financial feasibility
AC_fish_obj$obj_fun_0.7 <- 
  0.7*AC_fish_obj$Financial_feasibility_inv + (1-0.7)*AC_fish_obj$AC_fish_suitability
AC_sw_obj$obj_fun_0.7 <- 
  0.7*AC_sw_obj$Financial_feasibility_inv + (1-0.7)*AC_sw_obj$AC_sw_suitability

# Convert to sf
AC_fish_obj_sf <- st_as_sf(AC_fish_obj,
                           coords = c('X', 'Y'),
                           crs = 'EPSG:4559')
AC_sw_obj_sf <- st_as_sf(AC_sw_obj,
                         coords = c('X', 'Y'),
                         crs = 'EPSG:4559')


## Arrange by priority

# Equal weighting
AC_fish_obj_equal <- AC_fish_obj_sf %>%
  arrange(desc(obj_fun_0.5))
AC_sw_obj_equal <- AC_sw_obj_sf %>%
  arrange(desc(obj_fun_0.5))

# Priority - env. suitability
AC_fish_obj_suit <- AC_fish_obj_sf %>%
  arrange(desc(obj_fun_0.3))
AC_sw_obj_suit <- AC_sw_obj_sf %>%
  arrange(desc(obj_fun_0.3))

# Priority - financial feasibility
AC_fish_obj_FinRisk <- AC_fish_obj_sf %>%
  arrange(desc(obj_fun_0.7))
AC_sw_obj_FinRisk <- AC_sw_obj_sf %>%
  arrange(desc(obj_fun_0.7))


## Rasterise

# # Import Bass Strait shapefile
# BS <- st_read(here('data/BS.gpkg'))

# Aquaculture - finfish
AC_fish_obj_equal_r <- rasterize(AC_fish_obj_equal, rast(BS, res = 0.03),
                                 field = 'obj_fun_0.5')
AC_fish_obj_suit_r <- rasterize(AC_fish_obj_suit, rast(BS, res = 0.03),
                                field = 'obj_fun_0.3')
AC_fish_obj_FinRisk_r <- rasterize(AC_fish_obj_FinRisk, rast(BS, res = 0.03),
                                   field = 'obj_fun_0.7')

# Aquaculture - seaweed
AC_sw_obj_equal_r <- rasterize(AC_sw_obj_equal, rast(BS, res = 0.03),
                               field = 'obj_fun_0.5')
AC_sw_obj_suit_r <- rasterize(AC_sw_obj_suit, rast(BS, res = 0.03),
                              field = 'obj_fun_0.3')
AC_sw_obj_FinRisk_r <- rasterize(AC_sw_obj_FinRisk, rast(BS, res = 0.03),
                                 field = 'obj_fun_0.7')


## Rescale all rasters to between 0 and 1 - AC fish

# Stack all rasters
AC_fish_obj_stack <- c(AC_fish_obj_equal_r, AC_fish_obj_suit_r, AC_fish_obj_FinRisk_r)

# Find global min and max
AC_fish_global_min <- min(global(AC_fish_obj_stack, 'min', na.rm = TRUE)[[1]])
AC_fish_global_max <- max(global(AC_fish_obj_stack, 'max', na.rm = TRUE)[[1]])

# Rescale
AC_fish_equal_scaled <- (AC_fish_obj_equal_r - AC_fish_global_min) / (AC_fish_global_max - AC_fish_global_min)
AC_fish_suit_scaled <- (AC_fish_obj_suit_r - AC_fish_global_min) / (AC_fish_global_max - AC_fish_global_min)
AC_fish_FinRisk_scaled <- (AC_fish_obj_FinRisk_r - AC_fish_global_min) / (AC_fish_global_max - AC_fish_global_min)


## Rescale all rasters to between 0 and 1 - AC seaweed

# Stack all rasters
AC_sw_obj_stack <- c(AC_sw_obj_equal_r, AC_sw_obj_suit_r, AC_sw_obj_FinRisk_r)

# Find global min and max
AC_sw_global_min <- min(global(AC_sw_obj_stack, 'min', na.rm = TRUE)[[1]])
AC_sw_global_max <- max(global(AC_sw_obj_stack, 'max', na.rm = TRUE)[[1]])

# Rescale
AC_sw_equal_scaled <- (AC_sw_obj_equal_r - AC_sw_global_min) / (AC_sw_global_max - AC_sw_global_min)
AC_sw_suit_scaled <- (AC_sw_obj_suit_r - AC_sw_global_min) / (AC_sw_global_max - AC_sw_global_min)
AC_sw_FinRisk_scaled <- (AC_sw_obj_FinRisk_r - AC_sw_global_min) / (AC_sw_global_max - AC_sw_global_min)


### Maps

# # Colour scheme
# mycols <- RColorBrewer::brewer.pal(9, 'PuBuGn')


## Aquaculture - finfish

# Create list
AC_fish_scaled_lst <- lst(AC_fish_equal_scaled, AC_fish_suit_scaled, AC_fish_FinRisk_scaled)

# Get the minimum and maximum values for legend
AC_fish_scaled_min <- min(sapply(AC_fish_scaled_lst, function(r) min(values(r), na.rm = TRUE)))
AC_fish_scaled_max <- max(sapply(AC_fish_scaled_lst, function(r) max(values(r), na.rm = TRUE)))

# Set legend breaks
AC_fish_breaks <- seq(AC_fish_scaled_min, AC_fish_scaled_max, length.out = 101)

# Equal weighting
png(file=here('output/figures/AC_fish_obj_equal_r.png'),
    res=500, width=6, height=5, units = 'in')
plot(AC_fish_equal_scaled, breaks = AC_fish_breaks, col = mycols, legend = F, pax=list(cex.axis = 2))
plot(st_geometry(BS), col = 'grey90', add = T)
text(143.5,-37.2, '(d)', cex = 1.8)
#north(xy=c(149.5,-41.5), type=1, cex=1.2, d = 0.25)
#sbar(100, c(143.4, -41.75), cex=0.8, label='100 km')
dev.off()

# Priority - env. suitability
png(file=here('output/figures/AC_fish_obj_suit_r.png'),
    res=500, width=6, height=5, units = 'in')
plot(AC_fish_suit_scaled, breaks = AC_fish_breaks, col = mycols, legend = F, pax=list(cex.axis = 2))
plot(st_geometry(BS), col = 'grey90', add = T)
text(143.5,-37.2, '(e)', cex = 1.8)
#north(xy=c(149.5,-41.5), type=1, cex=1.2, d = 0.25)
#sbar(100, c(143.4, -41.75), cex=0.8, label='100 km')
dev.off()

# Priority - financial feasibility
png(file=here('output/figures/AC_fish_obj_FinRisk_r.png'),
    res=500, width=6, height=5, units = 'in')
plot(AC_fish_FinRisk_scaled, breaks = AC_fish_breaks, col = mycols, legend = F, pax=list(cex.axis = 2))
plot(st_geometry(BS), col = 'grey90', add = T)
text(143.5,-37.2, '(f)', cex = 1.8)
#north(xy=c(149.5,-41.5), type=1, cex=1.2, d = 0.25)
#sbar(100, c(143.4, -41.75), cex=0.8, label='100 km')
dev.off()


## Aquaculture - seaweed

# Create list
AC_sw_scaled_lst <- lst(AC_sw_equal_scaled, AC_sw_suit_scaled, AC_sw_FinRisk_scaled)

# Get the minimum and maximum values for legend
AC_sw_scaled_min <- min(sapply(AC_sw_scaled_lst, function(r) min(values(r), na.rm = TRUE)))
AC_sw_scaled_max <- max(sapply(AC_sw_scaled_lst, function(r) max(values(r), na.rm = TRUE)))

# Set legend breaks
AC_sw_breaks <- seq(AC_sw_scaled_min, AC_sw_scaled_max, length.out = 101)

# Equal weighting
png(file=here('output/figures/AC_sw_obj_equal_r.png'),
    res=500, width=6, height=5, units = 'in')
plot(AC_sw_equal_scaled, breaks = AC_sw_breaks, col = mycols, legend = F, pax=list(cex.axis = 2))
plot(st_geometry(BS), col = 'grey90', add = T)
text(143.5,-37.2, '(g)', cex = 1.8)
#north(xy=c(149.5,-41.5), type=1, cex=1.2, d = 0.25)
#sbar(100, c(143.4, -41.75), cex=0.8, label='100 km')
dev.off()

# Priority - env. suitability
png(file=here('output/figures/AC_sw_obj_suit_r.png'),
    res=500, width=6, height=5, units = 'in')
plot(AC_sw_suit_scaled, breaks = AC_sw_breaks, col = mycols, legend = F, pax=list(cex.axis = 2))
plot(st_geometry(BS), col = 'grey90', add = T)
text(143.5,-37.2, '(h)', cex = 1.8)
#north(xy=c(149.5,-41.5), type=1, cex=1.2, d = 0.25)
#sbar(100, c(143.4, -41.75), cex=0.8, label='100 km')
dev.off()

# Priority - financial feasibility
png(file=here('output/figures/AC_sw_obj_FinRisk_r.png'),
    res=500, width=6, height=5, units = 'in')
plot(AC_sw_FinRisk_scaled, breaks = AC_sw_breaks, col = mycols, legend = F, pax=list(cex.axis = 2))
plot(st_geometry(BS), col = 'grey90', add = T)
text(143.5,-37.2, '(i)', cex = 1.8)
#north(xy=c(149.5,-41.5), type=1, cex=1.2, d = 0.25)
#sbar(100, c(143.4, -41.75), cex=0.8, label='100 km')
# image.plot(legend.only = TRUE, zlim = c(0, 1), col = mycols, 
#            legend.line = 2, legend.mar = 3)
dev.off()


# Create raster lists
wind_list <- lst(wind_equal_scaled, wind_suit_scaled, wind_FinRisk_scaled)
AC_fish_list <- lst(AC_fish_equal_scaled, AC_fish_suit_scaled, AC_fish_FinRisk_scaled)
AC_sw_list <- lst(AC_sw_equal_scaled, AC_sw_suit_scaled, AC_sw_FinRisk_scaled)


## Find summary values

# Wind
lapply(wind_list, summary)

# Aquaculture - finfish
lapply(AC_fish_list, summary)

# Aquaculture - seaweed
lapply(AC_sw_list, summary)


## Find standard deviation

# Wind
lapply(wind_list, global, sd, na.rm = TRUE)

# Aquaculture - fish
lapply(AC_fish_list, global, sd, na.rm = TRUE)

# Aquaculture - seaweed
lapply(AC_sw_list, global, sd, na.rm = TRUE)


## Count suitable cells (non-NA) across scenarios

# Wind
sapply(wind_list, function(r) sum(values(r) > 0.7, na.rm = TRUE))

# Aquaculture - fish
sapply(AC_fish_list, function(r) sum(values(r) > 0.7, na.rm = TRUE))

# Aquaculture - seaweed
sapply(AC_sw_list, function(r) sum(values(r) > 0.7, na.rm = TRUE))

# Import table of summary values
eco_via_df <- read_csv(here('data/eco_via_df.csv'))

# Convert factors
eco_via_df$`Management scenario` <- as.factor(eco_via_df$`Management scenario`)

# Boxplot
ggplot(eco_via_df, aes(x=`Management scenario`, y=Mean, color=Industry)) +
  geom_boxplot()
