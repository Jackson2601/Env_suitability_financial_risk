
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

#--------------------------#

### We will generate curves representing different site selection scenarios
### by ordering the feasibility/suitability columns depending on management priorities

# Import environmental suitability and feasibility dataframes
wind_FinRisk_df <- read_csv(here('output/dataframes/wind_FinRisk_df.csv'))
AC_fish_FinRisk_df <- read_csv(here('output/dataframes/AC_fish_FinRisk_df.csv'))
AC_sw_FinRisk_df <- read_csv(here('output/dataframes/AC_sw_FinRisk_df.csv'))

# Add column with combined risk and suitability
wind_FinRisk_df$combined <- wind_FinRisk_df$Wind_suitability * 0.5 + wind_FinRisk_df$Financial_feasibility * (1-0.5)
AC_fish_FinRisk_df$combined <- AC_fish_FinRisk_df$AC_fish_suitability * 0.5 + AC_fish_FinRisk_df$Financial_feasibility * (1-0.5)
AC_sw_FinRisk_df$combined <- AC_sw_FinRisk_df$AC_sw_suitability * 0.5 + AC_sw_FinRisk_df$Financial_feasibility * (1-0.5)


## Prioritising reducing financial risk

# Reorder financial feasibility column - highest to lowest
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

# Add cell column
wind_high_FinRisk$Cell <- 1:nrow(wind_high_FinRisk)
AC_fish_high_FinRisk$Cell <- 1:nrow(AC_fish_high_FinRisk)
AC_sw_high_FinRisk$Cell <- 1:nrow(AC_sw_high_FinRisk)

# Add column with priority
wind_high_FinRisk$Priority <- 'Reduce financial risk'
AC_fish_high_FinRisk$Priority <- 'Reduce financial risk'
AC_sw_high_FinRisk$Priority <- 'Reduce financial risk'


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

# Add cell column
wind_high_suit$Cell <- 1:nrow(wind_high_suit)
AC_fish_high_suit$Cell <- 1:nrow(AC_fish_high_suit)
AC_sw_high_suit$Cell <- 1:nrow(AC_sw_high_suit)

# Add column with priority
wind_high_suit$Priority <- 'Increase env. suitability'
AC_fish_high_suit$Priority <- 'Increase env. suitability'
AC_sw_high_suit$Priority <- 'Increase env. suitability'


## Equal weighting

# Reorder environmental suitability column - highest to lowest
wind_combined <- wind_FinRisk_df %>%
  arrange(desc(combined))
AC_fish_combined <- AC_fish_FinRisk_df %>%
  arrange(desc(combined))
AC_sw_combined <- AC_sw_FinRisk_df %>%
  arrange(desc(combined))

# Cumulative sum of financial feasibility
wind_combined$FinRisk_sum <- cumsum(wind_combined$Financial_feasibility)
AC_fish_combined$FinRisk_sum <- cumsum(AC_fish_combined$Financial_feasibility)
AC_sw_combined$FinRisk_sum <- cumsum(AC_sw_combined$Financial_feasibility)

# Add cell column
wind_combined$Cell <- 1:nrow(wind_combined)
AC_fish_combined$Cell <- 1:nrow(AC_fish_combined)
AC_sw_combined$Cell <- 1:nrow(AC_sw_combined)

# Add column with priority
wind_combined$Priority <- 'Equal weighting'
AC_fish_combined$Priority <- 'Equal weighting'
AC_sw_combined$Priority <- 'Equal weighting'


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
wind_FinRisk_scenarios <- rbind(wind_high_FinRisk, wind_high_suit, wind_random, wind_combined)
AC_fish_FinRisk_scenarios <- rbind(AC_fish_high_FinRisk, AC_fish_high_suit, AC_fish_random, AC_fish_combined)
AC_sw_FinRisk_scenarios <- rbind(AC_sw_high_FinRisk, AC_sw_high_suit, AC_sw_random, AC_sw_combined)


## Plots

# Wind farming
wind_scenarios_plot <- 
  ggplot(wind_FinRisk_scenarios, aes(Cell, FinRisk_sum, col = Priority)) +
  geom_smooth(linewidth = 0.25) +
  scale_color_manual(values = c('blue', 'green4', 'red3', 'black')) +
  labs(x = '', 
       y = '',
       col = '') +
  guides(col='none') +
  theme_classic() +
  theme(axis.text = element_text(size = 15))
wind_scenarios_plot

# Aquaculture - finfish
AC_fish_scenarios_plot <- 
  ggplot(AC_fish_FinRisk_scenarios, aes(Cell, FinRisk_sum, col = Priority)) +
  geom_smooth(linewidth = 0.25) +
  scale_color_manual(values = c('blue', 'green4', 'red3', 'black')) +
  labs(x = '', 
       y = '',
       col = '') +
  guides(col='none') +
  theme_classic()  +
  theme(axis.text = element_text(size = 15))
AC_fish_scenarios_plot

# Aquaculture - seaweed
AC_sw_scenarios_plot <- 
  ggplot(AC_sw_FinRisk_scenarios, aes(Cell, FinRisk_sum, colour = Priority)) +
  geom_smooth(linewidth = 0.25) +
  scale_color_manual(values = c('blue', 'green4', 'red3', 'black')) +
  labs(x = '', 
       y = '',
       col = '') +
  guides(col = 'none') +
  theme_classic() +
  theme(axis.text = element_text(size = 15),
        plot.margin = margin(10, 100, 10, 10))
AC_sw_scenarios_plot


### Plot the top 6000 cells to represent wind farm budget area

# Filter to top 6000 cells
wind_FinRisk_budget <- wind_high_FinRisk[1:6000, ]
wind_suit_budget <- wind_high_suit[1:6000, ]
wind_random_budget <- wind_random[1:6000, ]
wind_combined_budget <- wind_combined[1:6000, ]

# Join all
wind_budget_all <- rbind(wind_FinRisk_budget, wind_suit_budget, wind_random_budget, wind_combined_budget)

# Plot wind farm budget
wind_scenarios_budget_plot <- 
  ggplot(wind_budget_all, aes(Cell, FinRisk_sum, col = Priority)) +
  geom_smooth(linewidth = 0.25) +
  scale_color_manual(values = c('blue', 'green4', 'red3', 'black')) +
  labs(x = '', 
       y = '',
       col = '') +
  theme_classic() +
  theme(axis.text = element_text(size = 15),
        legend.text = element_text(size = 15))
wind_scenarios_budget_plot

# Combine plots
plots_all <- 
  (wind_scenarios_plot + AC_fish_scenarios_plot) /
  (AC_sw_scenarios_plot + wind_scenarios_budget_plot)

# Add labels
plots_all <- 
  plots_all + plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(size = 15))

# Shared x axis
plots_all_x <-
  wrap_elements(panel = plots_all) +
  labs(tag = 'No.cells') +
  theme(
    plot.tag = element_text(size = 20),
    plot.tag.position = c(0.46, 0.01)
  )
plots_all_x

# Shared y label
plots_all_y <-
  wrap_elements(panel = plots_all) +
  labs(tag = 'Financial risk') +
  theme(
    plot.tag = element_text(size = 20, angle = 90),
    plot.tag.position = 'left'
  )
plots_all_y

# Save
ggsave(here('output/figures/scenarios_all_x.png'), plots_all_x, width = 18)
ggsave(here('output/figures/scenarios_all_y.png'), plots_all_y)


#----MAPPING BEST OFFSHORE DEVELOPMENT AREAS----#

### We will use the objective function to combine suitability with feasibility

### Equation from Chris to calculate weighted sum:

### alpha*R  + (1-alpha)*S
### R = financial feasibility
### S = suitability
### alpha = a weighting ranging between 0 and 1


### WIND FARMING

# Invert risk values to equal high feasibility
wind_all_obj <- wind_FinRisk_df
wind_all_obj$Financial_feasibility <- wind_all_obj$Financial_feasibility

# Equal weighting
wind_all_obj$obj_fun_0.5 <- 
  0.5*wind_all_obj$Financial_feasibility + (1-0.5)*wind_all_obj$Wind_suitability

# Priority - env. suitability
wind_all_obj$obj_fun_0.3 <- 
  0.3*wind_all_obj$Financial_feasibility + (1-0.3)*wind_all_obj$Wind_suitability

# Priority - financial feasibility
wind_all_obj$obj_fun_0.7 <- 
  0.7*wind_all_obj$Financial_feasibility + (1-0.7)*wind_all_obj$Wind_suitability

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

# Colour scheme
mycols <- RColorBrewer::brewer.pal(9, 'PuBuGn')

# Equal weighting
png(file=here('output/figures/wind_obj_equal_r.png'),
    res=500, width=6, height=5, units = 'in')
plot(wind_obj_equal_r, col = mycols, legend = F, pax=list(cex.axis = 2))
plot(st_geometry(BS), col = 'grey90', add = T)
text(143.5,-37.2, '(a)', cex = 1.8)
north(xy=c(149.5,-41.5), type=1, cex=1.2, d = 0.25)
sbar(100, c(143.4, -41.75), cex=0.8, label='100 km')
dev.off()

# Priority - env. suitability
png(file=here('output/figures/wind_obj_suit_r.png'),
    res=500, width=6, height=5, units = 'in')
plot(wind_obj_suit_r, col = mycols, legend = F, pax=list(cex.axis = 2))
plot(st_geometry(BS), col = 'grey90', add = T)
text(143.5,-37.2, '(b)', cex = 1.8)
#north(xy=c(149.5,-41.5), type=1, cex=1.2, d = 0.25)
#sbar(100, c(143.4, -41.75), cex=0.8, label='100 km')
dev.off()

# Priority - financial feasibility
png(file=here('output/figures/wind_obj_FinRisk_r.png'),
    res=500, width=6, height=5, units = 'in')
plot(wind_obj_FinRisk_r, col = mycols, legend = F, pax=list(cex.axis = 2))
plot(st_geometry(BS), col = 'grey90', add = T)
text(143.5,-37.2, '(c)', cex = 1.8)
#north(xy=c(149.5,-41.5), type=1, cex=1.2, d = 0.25)
#sbar(100, c(143.4, -41.75), cex=0.8, label='100 km')
dev.off()


### AQUACULTURE

AC_fish_obj <- AC_fish_FinRisk_df
AC_sw_obj <- AC_sw_FinRisk_df

# Equal weighting
AC_fish_obj$obj_fun_0.5 <- 
  0.5*AC_fish_obj$Financial_feasibility + (1-0.5)*AC_fish_obj$AC_fish_suitability
AC_sw_obj$obj_fun_0.5 <- 
  0.5*AC_sw_obj$Financial_feasibility + (1-0.5)*AC_sw_obj$AC_sw_suitability

# Priority - env. suitability
AC_fish_obj$obj_fun_0.3 <- 
  0.3*AC_fish_obj$Financial_feasibility + (1-0.3)*AC_fish_obj$AC_fish_suitability
AC_sw_obj$obj_fun_0.3 <- 
  0.3*AC_sw_obj$Financial_feasibility + (1-0.3)*AC_sw_obj$AC_sw_suitability

# Priority - financial feasibility
AC_fish_obj$obj_fun_0.7 <- 
  0.7*AC_fish_obj$Financial_feasibility + (1-0.7)*AC_fish_obj$AC_fish_suitability
AC_sw_obj$obj_fun_0.7 <- 
  0.7*AC_sw_obj$Financial_feasibility + (1-0.7)*AC_sw_obj$AC_sw_suitability

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


### Maps

# # Colour scheme
# mycols <- RColorBrewer::brewer.pal(9, 'PuBuGn')


## Aquaculture - finfish

# Equal weighting
png(file=here('output/figures/AC_fish_obj_equal_r.png'),
    res=500, width=6, height=5, units = 'in')
plot(AC_fish_obj_equal_r, col = mycols, legend = F, pax=list(cex.axis = 2))
plot(st_geometry(BS), col = 'grey90', add = T)
text(143.5,-37.2, '(d)', cex = 1.8)
#north(xy=c(149.5,-41.5), type=1, cex=1.2, d = 0.25)
#sbar(100, c(143.4, -41.75), cex=0.8, label='100 km')
dev.off()

# Priority - env. suitability
png(file=here('output/figures/AC_fish_obj_suit_r.png'),
    res=500, width=6, height=5, units = 'in')
plot(AC_fish_obj_suit_r, col = mycols, legend = F, pax=list(cex.axis = 2))
plot(st_geometry(BS), col = 'grey90', add = T)
text(143.5,-37.2, '(e)', cex = 1.8)
#north(xy=c(149.5,-41.5), type=1, cex=1.2, d = 0.25)
#sbar(100, c(143.4, -41.75), cex=0.8, label='100 km')
dev.off()

# Priority - financial feasibility
png(file=here('output/figures/AC_fish_obj_FinRisk_r.png'),
    res=500, width=6, height=5, units = 'in')
plot(AC_fish_obj_FinRisk_r, col = mycols, legend = F, pax=list(cex.axis = 2))
plot(st_geometry(BS), col = 'grey90', add = T)
text(143.5,-37.2, '(f)', cex = 1.8)
#north(xy=c(149.5,-41.5), type=1, cex=1.2, d = 0.25)
#sbar(100, c(143.4, -41.75), cex=0.8, label='100 km')
dev.off()


## Aquaculture - seaweed

# Equal weighting
png(file=here('output/figures/AC_sw_obj_equal_r.png'),
    res=500, width=6, height=5, units = 'in')
plot(AC_sw_obj_equal_r, col = mycols, legend = F, pax=list(cex.axis = 2))
plot(st_geometry(BS), col = 'grey90', add = T)
text(143.5,-37.2, '(g)', cex = 1.8)
#north(xy=c(149.5,-41.5), type=1, cex=1.2, d = 0.25)
#sbar(100, c(143.4, -41.75), cex=0.8, label='100 km')
dev.off()

# Priority - env. suitability
png(file=here('output/figures/AC_sw_obj_suit_r.png'),
    res=500, width=6, height=5, units = 'in')
plot(AC_sw_obj_suit_r, col = mycols, legend = F, pax=list(cex.axis = 2))
plot(st_geometry(BS), col = 'grey90', add = T)
text(143.5,-37.2, '(h)', cex = 1.8)
#north(xy=c(149.5,-41.5), type=1, cex=1.2, d = 0.25)
#sbar(100, c(143.4, -41.75), cex=0.8, label='100 km')
dev.off()

# Priority - financial feasibility
png(file=here('output/figures/AC_sw_obj_FinRisk_r.png'),
    res=500, width=10, height=5, units = 'in')
plot(AC_sw_obj_FinRisk_r, col = mycols, legend = F, pax=list(cex.axis = 2))
plot(st_geometry(BS), col = 'grey90', add = T)
text(143.5,-37.2, '(i)', cex = 1.8)
#north(xy=c(149.5,-41.5), type=1, cex=1.2, d = 0.25)
#sbar(100, c(143.4, -41.75), cex=0.8, label='100 km')
image.plot(legend.only = TRUE, zlim = c(0, 1), col = mycols, 
           legend.line = 2, legend.mar = 3)
dev.off()
