
#----Site feasibility - Bass Strait
#
#----Trade-off curves to optimise site risk vs suitability for wind and aquaculture
#
#----Following meeting with Chris and Caitie
#
# 28/06/2024


#-------At start-up--------#

library(tidyverse)
library(here)
library(sf)
library(patchwork)

#--------------------------#


### Equation from Chris to calculate weighted sum:

### alpha*R  + (1-alpha)*S
### R = financial feasibility
### S = suitability
### alpha = a weighting ranging between 0 and 1

# Set alpha values
alphvals <- seq(0, 1, by = 0.1)
nalphvals <- length(alphvals)


#----WIND FARMING----#

# Import environmental suitability and feasibility dataframes
wind_FinFeas_df <- read_csv(here('wind_FinRisk_df.csv'))
# wind_risk_df <- read_csv(here('output/dataframes/wind_risk_df.csv'))

# Remove NAs
wind_FinFeas_df <- 
  wind_FinFeas_df %>% drop_na(Wind_suitability)

# # # Invert risk values to represent feasibility
# wind_FinFeas_df$risk_inv <- 1 - wind_FinFeas_df$Financial_feasibility

# Create iPU column
wind_FinFeas_df$iPU <- 1:nrow(wind_FinFeas_df)

### We will use a subset of cells to represent planning units
### For aquaculture, a single 1 km grid cell is likely to represent
### a single planning unit, due to the size of aquaculture farms

### For wind farms, this will not be the case, as they are much larger
### We could use the size of a declared area, or a subset of the declared area

# Number of cells/planning units
nPU <- nrow(wind_FinFeas_df)

# Set area constraint - this could be a declared area for wind farming
nPU_budget <- 6000

# Function to calculate objective function
obj <- function(dat, alpha){
  # calculate the objective function
  dat$Wind_suitability * alpha + dat$Financial_feasibility * (1-alpha)
}


# Objective fucntion:
# max(wind_potential[i]*alpha*x[i] + (1-alpha)*financial_risk[i]*x[i]) - penalty
# Subject to sum(x[i]) <= nPU_budget
# i is index for planning units 1:nPU
# x[i] is binary variable indicating if planning unit i is selected
# Later on you may want to add additional penalties as a negative on the first part
# of the above equation. e.g. a boundary length constraint 

# Initialize dat_opt
dat_opt <- data.frame(alpha = alphvals, 
                      windfarm_outcome = rep(NA, nalphvals), 
                      finances_outcome = rep(NA, nalphvals))

# Optimise using ranked values - we can do this for a simple obj function
for (ialpha in 1:nalphvals){
  # alpha <- 0.5 # can use this for a test run
  alpha <- alphvals[ialpha]
  dat_temp <- wind_FinFeas_df 
  # calculate the objective function
  dat_temp$obj_vals <- obj(dat_temp, alpha)
  dat_temp$ranked <- nPU - rank(dat_temp$obj_vals)+1
  
  # indicator of selected
  dat_temp$x <- ifelse(dat_temp$ranked <= nPU_budget, 1, 0) 
 
  # in the real one you want to store x and iPU, so you can map the optimal solution
  # for now we will just store the outcomes for windfarm and finances
  dat_opt$windfarm_outcome[ialpha] <- sum(dat_temp$Wind_suitability * dat_temp$x)
  dat_opt$finances_outcome[ialpha] <- sum(dat_temp$Financial_feasibility * dat_temp$x)
  dat_opt$indicator[ialpha] <- dat_temp$x
  dat_opt$ranked[ialpha] <- dat_temp$ranked
  dat_opt$iPU[ialpha] <- dat_temp$iPU
}

# # Save 'dat_temp'
# write_csv(dat_temp, here('output/dataframes/wind_obj_df.csv'))


# ### Run objective function on points within the wind farm declared areas
# 
# # Import environmental suitability and risk sf objects
# wind_FinFeas_sf <- st_read(here('output/wind_FinFeas_sf.gpkg'))
# 
# # Invert financial risk values, so 0 = high risk and 1 = low risk
# wind_FinFeas_sf$Financial_feasibility <- 1-wind_FinFeas_sf$Financial_risk
# 
# # Create iPU column
# wind_FinFeas_sf$iPU <- 1:nrow(wind_FinFeas_sf)
# 
# # Import wind farm declared areas for Bass Strait
# wind_decl <- st_read(here('data/industry/Wind_farming/Shapefile_of_the_Proposed_Area_Gippsland_Victoria.gpkg'))
# 
# # Transform crs
# wind_decl <- st_transform(wind_decl, 'EPSG:28355')
# wind_FinFeas_sf <- st_transform(wind_FinFeas_sf, 'EPSG:28355')
# 
# # Use a spatial join to identify points within the declared areas
# wind_decl_pts <- st_intersection(wind_FinFeas_sf, wind_decl)
# 
# # Run objective function
# wind_decl_suit <- sum(wind_decl_pts$Wind_suitability)
# wind_decl_FinFeas <- sum(wind_decl_pts$Financial_feasibility)

# Plot
pareto_wind <- 
  ggplot(dat_opt) +
  aes(x = windfarm_outcome, y = finances_outcome, color = alpha) +
  geom_point(size = 2) +
  geom_line(col = 'black') +
  # annotate(geom = 'point', x = wind_decl_suit,
  #          y = wind_decl_FinFeas, col = 'red', size = 2) +
  # annotate(geom = 'text', x = 4800, y = 5000,
  #          label = 'Declared areas', col = 'red', size = 4) +
  # annotate(geom = 'text', x = 5800, y = 5500,
  #          label = 'a', size = 8) +
  # labs(x = '\nEnv. Suitability', y = 'Financial feasibility\n') +
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 15)) +
  theme(legend.position = 'none')
pareto_wind

# Save
ggsave(here('figures/pareto_wind_plot.png'), pareto_wind)


#----AQUACULTURE----#

#---Finfish---#

# Import environmental suitability and feasibility dataframes
AC_fish_FinFeas_df <- read_csv(here('AC_fish_FinRisk_df.csv'))
# AC_fish_risk_df <- read_csv(here('output/dataframes/AC_fish_risk_df.csv'))

# # Invert risk values to represent feasibility
# AC_fish_risk_df$risk_inv <- 1 - AC_fish_risk_df$Financial_risk

# Remove NAs
AC_fish_FinFeas_df <- 
  AC_fish_FinFeas_df %>% drop_na(AC_fish_suitability)

# Create iPU column
AC_fish_FinFeas_df$iPU <- 1:nrow(AC_fish_FinFeas_df)

# Number of cells/planning units
nPU_AC_fish <- nrow(AC_fish_FinFeas_df)

# Set area constraint
nPU_budget_AC_fish <- 1000

# Function to calculate objective function
obj_AC_fish <- function(dat, alpha){
  # calculate the objective function
  dat$AC_fish_suitability * alpha + dat$Financial_feasibility * (1-alpha)
}

# Initialize dat_opt
dat_opt_AC_fish <- data.frame(alpha = alphvals, 
                              AC_fish_outcome = rep(NA, nalphvals), 
                              finances_outcome = rep(NA, nalphvals))

# Optimise using ranked values - we can do this for a simple obj function
for (ialpha in 1:nalphvals){
  alpha <- alphvals[ialpha]
  dat_temp_AC_fish <- AC_fish_FinFeas_df
  # calculate the objective function
  dat_temp_AC_fish$obj_vals <- obj_AC_fish(dat_temp_AC_fish, alpha)
  dat_temp_AC_fish$ranked <- nPU_AC_fish - rank(dat_temp_AC_fish$obj_vals)+1 
  
  # indicator of selected
  dat_temp_AC_fish$x <- ifelse(dat_temp_AC_fish$ranked <= nPU_budget_AC_fish, 1, 0) 
  
  # Store outcomes
  dat_opt_AC_fish$AC_fish_outcome[ialpha] <- sum(dat_temp_AC_fish$AC_fish_suitability * dat_temp_AC_fish$x)
  dat_opt_AC_fish$finances_outcome[ialpha] <- sum(dat_temp_AC_fish$Financial_feasibility * dat_temp_AC_fish$x)
  dat_opt_AC_fish$indicator[ialpha] <- dat_temp_AC_fish$x
  dat_opt_AC_fish$ranked[ialpha] <- dat_temp_AC_fish$ranked
  dat_opt_AC_fish$iPU[ialpha] <- dat_temp_AC_fish$iPU
}

# # Save 'dat_temp_AC_fish'
# write_csv(dat_temp_AC_fish, here('output/dataframes/AC_fish_obj_df.csv'))

# Plot
pareto_AC_fish <- 
  ggplot(dat_opt_AC_fish) +
  aes(x = AC_fish_outcome, y = finances_outcome, color = alpha) +
  geom_point(size = 2) +
  geom_line(col = 'black') +
  # annotate(geom = 'text', x = 800, y = 925,
  #          label = 'b', size = 8) +
  labs(x = '\nEnvironmental suitability') +
  theme_classic() +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 15)) +
  theme(legend.position = 'none')
pareto_AC_fish

# Save
ggsave(here('figures/pareto_AC_fish_plot.png'), pareto_AC_fish)


#---Seaweed---#

# Import environmental suitability and feasibility dataframes
AC_sw_FinFeas_df <- read_csv(here('AC_sw_FinRisk_df.csv'))
# AC_sw_risk_df <- read_csv(here('output/dataframes/AC_sw_risk_df.csv'))

# # Invert risk values to represent feasibility
# AC_sw_FinFeas_df$risk_inv <- 1 - AC_sw_FinFeas_df$Financial_risk

# Remove NAs
AC_sw_FinFeas_df <- 
  AC_sw_FinFeas_df %>% drop_na(AC_sw_suitability)

# Create iPU column
AC_sw_FinFeas_df$iPU <- 1:nrow(AC_sw_FinFeas_df)

# Number of cells/planning units
nPU_AC_sw <- nrow(AC_sw_FinFeas_df)

# Set area constraint
nPU_budget_AC_sw <- 1000

# Function to calculate objective function
obj_AC_sw <- function(dat, alpha){
  # calculate the objective function
  dat$AC_sw_suitability * alpha + dat$Financial_feasibility * (1-alpha)
}

# Initialize dat_opt
dat_opt_AC_sw <- data.frame(alpha = alphvals, 
                            AC_sw_outcome = rep(NA, nalphvals), 
                            finances_outcome = rep(NA, nalphvals))

# Optimise using ranked values - we can do this for a simple obj function
for (ialpha in 1:nalphvals){
  alpha <- alphvals[ialpha]
  dat_temp_AC_sw <- AC_sw_FinFeas_df
  # calculate the objective function
  dat_temp_AC_sw$obj_vals <- obj_AC_sw(dat_temp_AC_sw, alpha)
  dat_temp_AC_sw$ranked <- nPU_AC_sw - rank(dat_temp_AC_sw$obj_vals)+1
  
  # indicator of selected
  dat_temp_AC_sw$x <- ifelse(dat_temp_AC_sw$ranked <= nPU_budget_AC_sw, 1, 0) 
  
  # Store outcomes
  dat_opt_AC_sw$AC_sw_outcome[ialpha] <- sum(dat_temp_AC_sw$AC_sw_suitability * dat_temp_AC_sw$x, na.rm = T)
  dat_opt_AC_sw$finances_outcome[ialpha] <- sum(dat_temp_AC_sw$Financial_feasibility * dat_temp_AC_sw$x)
  dat_opt_AC_sw$indicator[ialpha] <- dat_temp_AC_sw$x
  dat_opt_AC_sw$ranked[ialpha] <- dat_temp_AC_sw$ranked
  dat_opt_AC_sw$iPU[ialpha] <- dat_temp_AC_sw$iPU
}

# # Save 'dat_temp_AC_fish'
# write_csv(dat_temp_AC_sw, here('output/dataframes/AC_sw_obj_df.csv'))

# Plot
pareto_AC_sw <- 
  ggplot(dat_opt_AC_sw) +
  aes(x = AC_sw_outcome, y = finances_outcome, color = alpha) +
  geom_point(size = 2) +
  geom_line(col = 'black') +
  # annotate(geom = 'text', x = 810, y = 800,
  #          label = 'c', size = 8) +
  labs(x = '\nEnvironmental suitability', color = 'Alpha\n') +
  theme_classic() +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 15),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15))
pareto_AC_sw

# Save
ggsave(here('figures/pareto_AC_sw_plot.png'), pareto_AC_sw)


### Combine plots

# Base plots
pareto_all <- pareto_wind + pareto_AC_fish + pareto_AC_sw + plot_layout(ncol = 2)

# Add labels
pareto_all <- 
  pareto_all + plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(size = 15))

# # Shared x axis
# pareto_all_x <-
#   wrap_elements(panel = pareto_all) +
#   labs(tag = 'Environmental suitability') +
#   theme(
#     plot.tag = element_text(size = 20),
#     plot.tag.position = c(0.5, 0.01)
#   )
# pareto_all_x

# Shared y label
pareto_all_y <-
  wrap_elements(panel = pareto_all) +
  labs(tag = 'Financial feasibility\n') +
  theme(
    plot.tag = element_text(size = 20, angle = 90),
    plot.tag.position = 'left'
  )
pareto_all_y

# Save
#ggsave(here('output/figures/pareto_all_x.png'), pareto_all_x)
ggsave(here('figures/pareto_all_y.png'), pareto_all_y, width = 16, height = 9)


#
# Algorithmic optimisation approach 
#
niter <- 1000

dat_temp_opt <- dat_temp
dat_temp_opt$x <- 0

# Randomly initialize the x values
dat_temp_opt$x[sample(dat_temp_opt$iPU, nPU_budget)] <- 1

# # Save outcomes
# outcome_vals <- data.frame(windfarm = rep(NA, niter), 
#                            finances = rep(NA, niter),
#                            obj_vals = rep(NA, niter))

# Save outcomes for each alpha
outcomes_all <- NULL

# Set alpha
# alpha <- 0.5 # will need to rerun for all alphas
for (ialpha in 1:nalphvals){
  alpha <- alphvals[ialpha]
  
  # Initialize a dataframe to store the outcome values for the current alpha
  outcome_vals <- data.frame(windfarm = numeric(niter), finances = numeric(niter), obj_vals = numeric(niter))
  
  for (iter in 1:niter){
  
  # calculate the objective function
  dat_temp_opt$obj_vals <- obj(dat_temp_opt, alpha)
  dat_temp_opt$ranked <- nPU - rank(dat_temp_opt$obj_vals)+1
  
  # calculate outcome for a given alpha 
  outcome_temp <- sum(dat_temp_opt$obj_vals * dat_temp_opt$x)  
  
  # now randomly turn off one of the x[i]=1 and turn on one of the x[i]=0
  # this is a simple algorithm, you could use a more sophisticated one
  # e.g. simulated annealing, genetic algorithm, etc.
  # for now we will just randomly select one to turn off and one to turn on
  ioff <- sample(which(dat_temp_opt$x == 1), 1)
  ion <- sample(which(dat_temp_opt$x == 0), 1)
  
  # calculate the new outcome
  dat_temp_opt$x[ioff] <- 0
  dat_temp_opt$x[ion] <- 1
  
  dat_temp_opt$obj_vals <- obj(dat_temp_opt, alpha)
  dat_temp_opt$ranked <- nPU - rank(dat_temp_opt$obj_vals)+1
  outcome_temp_new <- sum(dat_temp_opt$obj_vals * dat_temp_opt$x)
  
  # if the new outcome is better than the old outcome, keep the new solution
  if (outcome_temp_new > outcome_temp){
    dat_temp_opt$ranked <- nPU - rank(dat_temp_opt$obj_vals)+1
  } else {
    # if the new outcome is worse than the old outcome, keep the old solution
    dat_temp_opt$x[ioff] <- 1
    dat_temp_opt$x[ion] <- 0
  }
  
  outcome_vals$windfarm[iter] <- sum(dat_temp_opt$windfarm * dat_temp_opt$x)
  outcome_vals$finances[iter] <- sum(dat_temp_opt$finances * dat_temp_opt$x)
  outcome_vals$obj_vals[iter] <- sum(dat_temp_opt$obj_vals * dat_temp_opt$x)
  print(outcome_temp)
  }
  
  outcomes_all[[ialpha]] <- outcome_vals
  
}

# Get the optimal solution
windfarm_outcome_algorithmic <- sum(dat_temp_opt$Wind_suitability * dat_temp_opt$x)
finances_outcome_algorithmic <- sum(dat_temp_opt$Financial_feasibility * dat_temp_opt$x)


#
# Generate some random plans to compare to the pareto frontier
#
nrand <- 5000
dat_rand <- data.frame(windfarm_outcome = rep(NA, nrand), 
                       finances_outcome = rep(NA, nrand))

for (irand in 1:nrand){
  dat_temp <- dat
  
  iselected <- sample(dat_temp$iPU, nPU_budget, replace = TRUE)
  dat_temp$x <- 0
  dat_temp$x[iselected] <- 1
  dat_rand$windfarm_outcome[irand] <- sum(dat_temp$windfarm * dat_temp$x)
  dat_rand$finances_outcome[irand] <- sum(dat_temp$finances * dat_temp$x)
  rm(dat_temp)
}

# Dat policy decision default
dat_temp <- wind_risk_df
dat_temp$x <- 0
dat_temp$x[1:nPU_budget] <- 1
dat_default <- data.frame(windfarm_outcome = sum(dat_temp$Wind_suitability * dat_temp$x),
                          finances_outcome = sum(dat_temp$Financial_feasibility * dat_temp$x))

# Plot Pareto frontier
ggplot(dat_opt) +
  aes(x = windfarm_outcome, y = finances_outcome, color = alpha) +
  geom_point() +
  geom_line() + 
  # geom_point(data = dat_rand, 
  #            aes(x = windfarm_outcome, y = finances_outcome), 
  #            color = "grey") +
  # geom_point(data = dat_default,
  #            aes(x = windfarm_outcome, y = finances_outcome),
  #            color = "red") +
  annotate(geom = 'point', x = windfarm_outcome_algorithmic, 
           y = finances_outcome_algorithmic, color = "green") +
  xlab("Env. suitability") +
  ylab("Financial risk") +
  theme_classic()

# Plot convergence of algorithm
ggplot(outcome_vals) +
  aes(x = 1:niter, y = obj_vals) +
  geom_line() +
  xlab("Iteration") +
  ylab("Objective function value") +
  theme_classic()


#----AQUACULTURE - FINFISH----#

# Import environmental suitability and risk dataframes
AC_fish_risk_df <- read_csv(here('output/dataframes/AC_fish_risk_df.csv'))

# Invert financial risk values, so 0 = high risk and 1 = low risk
AC_fish_risk_df$Financial_risk_inv <- 1-AC_fish_risk_df$Financial_feasibility

# Create iPU column
AC_fish_risk_df$iPU <- 1:nrow(AC_fish_risk_df)

# Number of cells/planning units
nPU <- nrow(AC_fish_risk_df)

# Set alpha values
alphvals <- seq(0, 1, by = 0.1)
nalphvals <- length(alphvals)

# Set area constraint - this could be a declared area for wind farming
nPU_budget <- 16000

# Function to calculate objective function
obj <- function(dat, alpha){
  # calculate the objective function
  dat$AC_potential * alpha + dat$Financial_risk_inv * (1-alpha)
}

# Initialize dat_opt
dat_opt <- data.frame(alpha = alphvals, 
                      AC_outcome = rep(NA, nalphvals), 
                      finances_outcome = rep(NA, nalphvals))

# Optimise
for (ialpha in 1:nalphvals){
  alpha <- alphvals[ialpha]
  dat_temp <- AC_fish_risk_df
  # calculate the objective function
  dat_temp$obj_vals <- obj(dat_temp, alpha)
  dat_temp$ranked <- nPU - rank(dat_temp$obj_vals)+1
  
  # indicator of selected
  dat_temp$x <- ifelse(dat_temp$ranked <= nPU_budget, 1, 0)
  
  # store outcomes
  dat_opt$AC_outcome[ialpha] <- sum(dat_temp$AC_potential * dat_temp$x)
  dat_opt$finances_outcome[ialpha] <- sum(dat_temp$Financial_risk_inv * dat_temp$x)
  dat_opt$indicator[ialpha] <- dat_temp$x
  dat_opt$ranked[ialpha] <- dat_temp$ranked
  dat_opt$iPU[ialpha] <- dat_temp$iPU
}

# Plot
pareto_AC <- 
  ggplot(dat_opt) +
  aes(x = AC_outcome, y = finances_outcome, color = alpha) +
  geom_point() +
  geom_line() +
  labs(x = 'Env. Suitability', y = 'Financial feasibility') +
  theme_classic() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 15))
pareto_wind

# Save
ggsave(here('output/figures/pareto_AC_plot.png'), pareto_AC)


#
# Algorithmic optimisation approach 
#
niter <- 1000

dat_temp_opt <- dat_temp
dat_temp_opt$x <- 0

# Randomly initialize the x values
dat_temp_opt$x[sample(dat_temp_opt$iPU, nPU_budget)] <- 1

# Save outcomes for each alpha
outcomes_all <- NULL

# Set alpha
# alpha <- 0.5 # will need to rerun for all alphas
for (ialpha in 1:nalphvals){
  alpha <- alphvals[ialpha]
  
  # Initialize a dataframe to store the outcome values for the current alpha
  outcome_vals <- data.frame(AC_fish = numeric(niter), finances = numeric(niter), obj_vals = numeric(niter))
  
  for (iter in 1:niter){
    
    # calculate the objective function
    dat_temp_opt$obj_vals <- obj(dat_temp_opt, alpha)
    dat_temp_opt$ranked <- nPU - rank(dat_temp_opt$obj_vals)+1
    
    # calculate outcome for a given alpha 
    outcome_temp <- sum(dat_temp_opt$obj_vals * dat_temp_opt$x)  
    
    # simple algorithm
    ioff <- sample(which(dat_temp_opt$x == 1), 1)
    ion <- sample(which(dat_temp_opt$x == 0), 1)
    
    # calculate the new outcome
    dat_temp_opt$x[ioff] <- 0
    dat_temp_opt$x[ion] <- 1
    
    dat_temp_opt$obj_vals <- obj(dat_temp_opt, alpha)
    dat_temp_opt$ranked <- nPU - rank(dat_temp_opt$obj_vals)+1
    outcome_temp_new <- sum(dat_temp_opt$obj_vals * dat_temp_opt$x)
    
    # if the new outcome is better than the old outcome, keep the new solution
    if (outcome_temp_new > outcome_temp){
      dat_temp_opt$ranked <- nPU - rank(dat_temp_opt$obj_vals)+1
    } else {
      # if the new outcome is worse than the old outcome, keep the old solution
      dat_temp_opt$x[ioff] <- 1
      dat_temp_opt$x[ion] <- 0
    }
    
    outcome_vals$AC_fish[iter] <- sum(dat_temp_opt$AC_fish * dat_temp_opt$x)
    outcome_vals$finances[iter] <- sum(dat_temp_opt$finances * dat_temp_opt$x)
    outcome_vals$obj_vals[iter] <- sum(dat_temp_opt$obj_vals * dat_temp_opt$x)
    print(outcome_temp)
  }
  
  outcomes_all[[ialpha]] <- outcome_vals
  
}

# Get the optimal solution
AC_fish_outcome_algorithmic <- sum(dat_temp_opt$AC_potential * dat_temp_opt$x)
finances_outcome_algorithmic <- sum(dat_temp_opt$Financial_risk_inv * dat_temp_opt$x)


#
# Generate some random plans to compare to the pareto frontier
#
nrand <- 5000
dat_rand <- data.frame(AC_outcome = rep(NA, nrand), 
                       finances_outcome = rep(NA, nrand))

for (irand in 1:nrand){
  dat_temp <- dat
  
  iselected <- sample(dat_temp$iPU, nPU_budget, replace = TRUE)
  dat_temp$x <- 0
  dat_temp$x[iselected] <- 1
  dat_rand$AC_outcome[irand] <- sum(dat_temp$AC_fish * dat_temp$x)
  dat_rand$finances_outcome[irand] <- sum(dat_temp$finances * dat_temp$x)
  rm(dat_temp)
}

# Dat policy decision default
dat_temp <- AC_fish_risk_df
dat_temp$x <- 0
dat_temp$x[1:nPU_budget] <- 1
dat_default <- data.frame(AC_fish_outcome = sum(dat_temp$AC_potential * dat_temp$x),
                          finances_outcome = sum(dat_temp$Financial_risk_inv * dat_temp$x))

# Plot Pareto frontier
ggplot(dat_opt) +
  aes(x = AC_fish_outcome, y = finances_outcome, color = alpha) +
  geom_point() +
  geom_line() + 
  # geom_point(data = dat_rand, 
  #            aes(x = windfarm_outcome, y = finances_outcome), 
  #            color = "grey") +
  # geom_point(data = dat_default,
  #            aes(x = windfarm_outcome, y = finances_outcome),
  #            color = "red") +
  annotate(geom = 'point', x = AC_fish_outcome_algorithmic, 
           y = finances_outcome_algorithmic, color = "green") +
  xlab("Env. suitability") +
  ylab("Financial risk") +
  theme_classic()


# Plot convergence of algorithm
ggplot(outcome_vals) +
  aes(x = 1:niter, y = obj_vals) +
  geom_line() +
  xlab("Iteration") +
  ylab("Objective function value") +
  theme_classic()


#----AQUACULTURE - SEAWEED----#

# Import environmental suitability and risk dataframes
AC_sw_risk_df <- read_csv(here('output/dataframes/AC_sw_risk_df.csv'))

# Invert financial risk values, so 0 = high risk and 1 = low risk
AC_sw_risk_df$Financial_risk_inv <- 1-AC_sw_risk_df$Financial_risk

# Create iPU column
AC_sw_risk_df$iPU <- 1:nrow(AC_sw_risk_df)

# Number of cells/planning units
nPU <- nrow(AC_sw_risk_df)

# Set alpha values
alphvals <- seq(0, 1, by = 0.1)
nalphvals <- length(alphvals)

# Set area constraint - this could be a declared area for wind farming
nPU_budget <- 16000

# Function to calculate objective function
obj <- function(dat, alpha){
  # calculate the objective function
  dat$AC_potential * alpha + dat$Financial_risk_inv * (1-alpha)
}

# Initialize dat_opt
dat_opt <- data.frame(alpha = alphvals, 
                      AC_outcome = rep(NA, nalphvals), 
                      finances_outcome = rep(NA, nalphvals))

# Optimise
for (ialpha in 1:nalphvals){
  alpha <- alphvals[ialpha]
  dat_temp <- AC_sw_risk_df
  # calculate the objective function
  dat_temp$obj_vals <- obj(dat_temp, alpha)
  dat_temp$ranked <- nPU - rank(dat_temp$obj_vals)+1
  
  # indicator of selected
  dat_temp$x <- ifelse(dat_temp$ranked <= nPU_budget, 1, 0)
  
  # store outcomes
  dat_opt$AC_outcome[ialpha] <- sum(dat_temp$AC_potential * dat_temp$x)
  dat_opt$finances_outcome[ialpha] <- sum(dat_temp$Financial_risk_inv * dat_temp$x)
  dat_opt$indicator[ialpha] <- dat_temp$x
  dat_opt$ranked[ialpha] <- dat_temp$ranked
  dat_opt$iPU[ialpha] <- dat_temp$iPU
}

# Plot
pareto_AC <- 
  ggplot(dat_opt) +
  aes(x = AC_outcome, y = finances_outcome, color = alpha) +
  geom_point() +
  geom_line() +
  labs(x = 'Env. Suitability', y = 'Financial feasibility') +
  theme_classic() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 15))
pareto_wind

# Save
ggsave(here('output/figures/pareto_AC_plot.png'), pareto_AC)


#
# Algorithmic optimisation approach 
#
niter <- 1000

dat_temp_opt <- dat_temp
dat_temp_opt$x <- 0

# Randomly initialize the x values
dat_temp_opt$x[sample(dat_temp_opt$iPU, nPU_budget)] <- 1

# Save outcomes for each alpha
outcomes_all <- NULL

# Set alpha
# alpha <- 0.5 # will need to rerun for all alphas
for (ialpha in 1:nalphvals){
  alpha <- alphvals[ialpha]
  
  # Initialize a dataframe to store the outcome values for the current alpha
  outcome_vals <- data.frame(AC_sw = numeric(niter), finances = numeric(niter), obj_vals = numeric(niter))
  
  for (iter in 1:niter){
    
    # calculate the objective function
    dat_temp_opt$obj_vals <- obj(dat_temp_opt, alpha)
    dat_temp_opt$ranked <- nPU - rank(dat_temp_opt$obj_vals)+1
    
    # calculate outcome for a given alpha 
    outcome_temp <- sum(dat_temp_opt$obj_vals * dat_temp_opt$x)  
    
    # simple algorithm
    ioff <- sample(which(dat_temp_opt$x == 1), 1)
    ion <- sample(which(dat_temp_opt$x == 0), 1)
    
    # calculate the new outcome
    dat_temp_opt$x[ioff] <- 0
    dat_temp_opt$x[ion] <- 1
    
    dat_temp_opt$obj_vals <- obj(dat_temp_opt, alpha)
    dat_temp_opt$ranked <- nPU - rank(dat_temp_opt$obj_vals)+1
    outcome_temp_new <- sum(dat_temp_opt$obj_vals * dat_temp_opt$x)
    
    # if the new outcome is better than the old outcome, keep the new solution
    if (outcome_temp_new > outcome_temp){
      dat_temp_opt$ranked <- nPU - rank(dat_temp_opt$obj_vals)+1
    } else {
      # if the new outcome is worse than the old outcome, keep the old solution
      dat_temp_opt$x[ioff] <- 1
      dat_temp_opt$x[ion] <- 0
    }
    
    outcome_vals$AC_sw[iter] <- sum(dat_temp_opt$AC_sw * dat_temp_opt$x)
    outcome_vals$finances[iter] <- sum(dat_temp_opt$finances * dat_temp_opt$x)
    outcome_vals$obj_vals[iter] <- sum(dat_temp_opt$obj_vals * dat_temp_opt$x)
    print(outcome_temp)
  }
  
  outcomes_all[[ialpha]] <- outcome_vals
  
}

# Get the optimal solution
AC_sw_outcome_algorithmic <- sum(dat_temp_opt$AC_potential * dat_temp_opt$x)
finances_outcome_algorithmic <- sum(dat_temp_opt$Financial_risk_inv * dat_temp_opt$x)


#
# Generate some random plans to compare to the pareto frontier
#
nrand <- 5000
dat_rand <- data.frame(AC_outcome = rep(NA, nrand), 
                       finances_outcome = rep(NA, nrand))

for (irand in 1:nrand){
  dat_temp <- dat
  
  iselected <- sample(dat_temp$iPU, nPU_budget, replace = TRUE)
  dat_temp$x <- 0
  dat_temp$x[iselected] <- 1
  dat_rand$AC_outcome[irand] <- sum(dat_temp$AC_sw * dat_temp$x)
  dat_rand$finances_outcome[irand] <- sum(dat_temp$finances * dat_temp$x)
  rm(dat_temp)
}

# Dat policy decision default
dat_temp <- AC_sw_risk_df
dat_temp$x <- 0
dat_temp$x[1:nPU_budget] <- 1
dat_default <- data.frame(AC_sw_outcome = sum(dat_temp$AC_potential * dat_temp$x),
                          finances_outcome = sum(dat_temp$Financial_risk_inv * dat_temp$x))

# Plot Pareto frontier
ggplot(dat_opt) +
  aes(x = AC_sw_outcome, y = finances_outcome, color = alpha) +
  geom_point() +
  geom_line() + 
  # geom_point(data = dat_rand, 
  #            aes(x = windfarm_outcome, y = finances_outcome), 
  #            color = "grey") +
  # geom_point(data = dat_default,
  #            aes(x = windfarm_outcome, y = finances_outcome),
  #            color = "red") +
  annotate(geom = 'point', x = AC_sw_outcome_algorithmic, 
           y = finances_outcome_algorithmic, color = "green") +
  xlab("Env. suitability") +
  ylab("Financial risk") +
  theme_classic()


# Plot convergence of algorithm
ggplot(outcome_vals) +
  aes(x = 1:niter, y = obj_vals) +
  geom_line() +
  xlab("Iteration") +
  ylab("Objective function value") +
  theme_classic()
