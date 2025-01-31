# Env_suitability_financial_risk

## 05c_Alternate_scenarios_Bass_Strait.R

### Inputs
wind_FinFeas_df.csv - offshore wind\
AC_fish_FinFeas_df.csv - finfish aquaculture\
AC_sw_FinFeas_df.csv - seaweed aquaculture

BS_gpkg - Bass Strait shapefile

### Outputs
+ Figure 1 - maps showing risk and suitability objective function when alpha is set to 0.3 (prioritise financial risk mitigation), 0.7 (env. suitability), or equal weighting
+ Figure 2 - XY plots of financial risk and environmental suitability under different management scenarios (different order of risk and suitability columns)
\

## 05d_Trade_off_curves_Bass_Strait.R

### Inputs
wind_FinFeas_df.csv - offshore wind\
AC_fish_FinFeas_df.csv - finfish aquaculture\
AC_sw_FinFeas_df.csv - seaweed aquaculture

### Outputs
+ Figure 3 - Pareto Front/trade-off curves to optimise site risk vs suitability for wind and aquaculture
