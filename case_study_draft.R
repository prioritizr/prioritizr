#### Case study ####

#### Prelims ####
# paths
input_dat <- "C:/Users/sandr/Documents/PhDQuex/MOO_chapter/case_study_data/featuredata"

# packages

# crs etc

#### Load data ####
# features
# crop
crop_dat <- arrow::read_feather(file.path(input_dat, "crop_features.feather"))

# 
land_avail <- terra::rast(file.path(input_dat, "s5_aggre_LandAvail_moll_10km.tif"))
pa <- terra::rast(file.path(input_dat, "s5_aggre_PA_moll_10km.tif"))

df.pu$pa.cover <- terra::extract(pa,df.pu[,c("x","y")], method="simple")/100
df.pu$pa.cover <- ifelse(is.na(df.pu$pa.cover), 0, df.pu$pa.cover)



# biodiversity
install.packages(c("DBI", "RSQLite"))
library(DBI)
library(RSQLite)
library(tidyverse)
conn <- dbConnect(RSQLite::SQLite(), dbname = file.path(input_dat,"rij.db" ))
rij.sp <- tbl(conn, "table_rij_update") %>% 
  collect()   
dbDisconnect(conn)

agr_path <- file.path(input_dat, "0_sourcedata_ProjectedCropDistribution", "Proactive")
rcp45_files <- list.files(
  agr_path,
  pattern = "rcp45",
  full.names = TRUE,
  ignore.case = TRUE
)

agr_feat <- terra::rast(rcp45_files)

crop_names <- sub(".*rcp45_([^.]*)\\.tif$", "\\1", basename(rcp45_files))
names(agr_feat) <- crop_names


# library(rnaturalearth)
# 
# aus <- ne_countries(
#   country = "United States of America",
#   scale = "medium",
#   returnclass = "sf"
# )
# 
# aus <- terra::vect(aus)  # convert to terra SpatVector
# aus_moll <- terra::project(aus, terra::crs(agr_feat))
# agr_feat_aus <- terra::mask(agr_feat, aus_moll)
# terra::plot(agr_feat_aus)

baseline <- terra::rast(file.path(input_dat, "0_sourcedata_ProjectedCropDistribution", "baseline2010_ESAcropland.tif"))


## targets
con_feat_targets <- read_csv(file.path(input_dat, "speciestarget.csv"))

# filter targets based on features we actually have in country we have selected

#### Problem formulation ####
con_zone_cost <- area_based_cost 
agr_zone_cost <- area_based_cost
con_zone_con_ft <- con_feat
con_zone_agr_ft <- agr_feat * 0 # vals 0
agr_zone_con_ft <- con_feat * 0 # vals 0
agr_zone_agr_ft <- agr_feat 

con_zone_con_targets <- con_feat_targets # reduce to cols we actually 
con_zone_agr_targets <- con_feat_targets * 0 # or just leave the vals, considering the feature vals are at 0 anyway?
con_zone_targets <- cbind(con_zone_con_targets, con_zone_agr_targets)
agr_zone_agr_targets <- agr_feat_targets 
agr_zone_con_targets <- agr_feat_targets  * 0 # or just leave the vals, considering the feature vals are at 0 anyway?
agr_zone_targets <- cbind(agr_zone_agr_targets, agr_zone_con_targets)

con_zone_con_budget <- terra::global(con_zone_cost, "sum", na.rm = TRUE)[[1]] * 0.5
con_zon_agr_budget <- 0 # or give same budget as for con? Again, feature vals are at 0 anyway
# agr_budget <- terra::global(agr_zone_cost, "sum", na.rm = TRUE)[[1]] * 0.2 # not needed bc min set if following Jinquao's approach


#### Hierarchical approach ####
rel_tol_mat <- 

mp_hierarchical <- multi_problem(
  con_obj = problem(
    c(con_zone_cost, agr_zone_cost),
    zones(
      con_zone = con_zone_con_ft,
      agr_zone = agr_zone_con_ft
    )
  ) %>%
    add_min_shortfall_objective(c(con_zone_con_budget, con_zon_agr_budget)) %>%
    add_absolute_targets(con_zone_targets) %>%
    add_binary_decisions(),
  agr_obj = problem(
    c(con_zone_cost, agr_zone_cost),
    zones(
      con_zone = con_zone_agr_ft,
      agr_zone = agr_zone_agr_ft
    )
  ) %>%
    add_min_set_objective(c(con_zone_cost, agr_zone_cost)) %>%
    add_absolute_targets(agr_zone_targets) %>%
    add_binary_decisions()
) %>%
  add_rel_constraint_approach(rel_tol = rel_tol_mat) %>%
  add_gurobi_solver()

ms_hierarchical <- solve(mp_hierarchical, run_checks = FALSE) 
#terra::plot(ms_hierarchical)


##### Weighted sum approach
weights_mat <- 

mp_weightedsum <- multi_problem(
  con_obj = problem(
    c(con_zone_cost, agr_zone_cost),
    zones(
      con_zone = con_zone_con_ft,
      agr_zone = agr_zone_con_ft
    )
  ) %>%
    add_min_shortfall_objective(c(con_zone_con_budget, con_zon_agr_budget)) %>%
    add_absolute_targets(con_zone_targets) %>%
    add_binary_decisions(),
  agr_obj = problem(
    c(con_zone_cost, agr_zone_cost),
    zones(
      con_zone = con_zone_agr_ft,
      agr_zone = agr_zone_agr_ft
    )
  ) %>%
    add_min_set_objective(c(con_zone_cost, agr_zone_cost)) %>%
    add_absolute_targets(agr_zone_targets) %>%
    add_binary_decisions()
) %>%
  add_weighted_sum_approach(weights = weights_mat) %>%
  add_gurobi_solver()

ms_weightedsum <- solve(mp_weightedsum, run_checks = FALSE) 