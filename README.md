# Env_suitability_financial_risk

## 05c_Alternate_scenarios_Bass_Strait.R

## Inputs
wind_FinFeas_df.csv - offshore wind\
AC_fish_FinFeas_df.csv - finfish aquaculture\
AC_sw_FinFeas_df.csv - seaweed aquaculture

wind_fish_BS_prj - projected raster used to generate figure 1. Represents wind farm suitability
AC_fish_BS_prj - projected raster used to generate figure 1. Represents finfish aquaculture suitability
AC_sw_BS_prj - projected raster used to generate figure 1. Represents seaweed aquaculture suitability
FinRisk_BS_01 - raster representing financial risk for offshore industries within Bass Strait. Values scaled to between 0 and 1

#### Dataframe columns
Wind_suitability - environmental suitability for offshore wind farms at each grid cell
AC_fish_suitability - environmental suitability for offshore finfish aquaculture at each grid cell
AC_sw_suitability - environmental suitability for offshore seaweed aquaculture at each grid cell
Financial_feasibility - inverted financial risk values to represent financial feasibility for offshore industries at each grid cell

BS_gpkg - Bass Strait shapefile

### Outputs
+ Figure 1 - maps showing risk and suitability objective function when alpha is set to 0.3 (prioritise financial risk mitigation), 0.7 (env. suitability), or equal weighting
+ Figure 2 - XY plots of financial risk and environmental suitability under different management scenarios (different order of risk and suitability columns)
<br>

## 05d_Trade_off_curves_Bass_Strait.R

### Inputs
wind_FinFeas_df.csv - offshore wind\
AC_fish_FinFeas_df.csv - finfish aquaculture\
AC_sw_FinFeas_df.csv - seaweed aquaculture

### Outputs
+ Figure 3 - Pareto Front/trade-off curves to optimise site risk vs suitability for wind and aquaculture
