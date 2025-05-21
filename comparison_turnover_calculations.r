#calculate turnover as cpool/outer fluxes and as Cpool/GPP and check the differences
###################################################################################
library(raster)
library(ggplot2)
library(sf)
sf_use_s2(FALSE)
library(patchwork)
library(maps)
library(rnaturalearth)
library(viridis)
library(grid)
library(gridExtra)
library(devtools)
library(lemon)
library(ggpubr)
library(DGVMTools)
##############################################################
# upload cpool data and outer fluxes data (already processed and saved)
cpools_man_only <- read.table("C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\equilibrium\\data_pools_fluxes\\pools_eco_2220_2249", header = TRUE)
cfluxes_man_only <- read.table("C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\equilibrium\\data_pools_fluxes\\outer_fluxes_eco_2220_2249", header = TRUE)

########################################
# I need to get the avg gpp for each management option, to build a df with all the gpp values
#unmanaged
man_only_unm <- defineSource(id = "unm man_only",
                          dir = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\equilibrium\\unmanaged25p" ,
                          format = GUESS,
                          land.use.included = TRUE,
                          name = "unm man_only")

#for some input I need the active fraction (to know the fraction of forest in the grid cell)
active_fraction_man_only <- getField(source = man_only_unm, quant = "active_fraction_sts", first.year = 2220, last.year = 2249)
#to get the forest cover, I sum the 4 forest stands (NE, ND, BE, BD)
forest_area_man_only <- layerOp(x=active_fraction_man_only, operator = "+", layers=c("ForestNE", "ForestBD", "ForestBE", "ForestND"), new.layer = "forest_area")
#annual gpp is stored in "agpp_sts", with the values for each stand and a column for the forest_sum
gpp_unm_man_only <- getField(source = man_only_unm, quant= "agpp_sts", first.year=2220, last.year=2249)
#but I need to multiply the value of forest_sum by the forest_area
gpp_forest_unm_man_only <- calcNewField(x=gpp_unm_man_only, y=forest_area_man_only, x.col = "Forest_sum", y.col = "forest_area", op = "*" )
gpp_forest_unm_man_only_mean <- aggregateYears(gpp_forest_unm_man_only, method = "mean" )
gpp_forest_unm_man_only_df <- as.data.frame(gpp_forest_unm_man_only_mean)

#base
man_only_base <- defineSource(id = "base man_only",
                          dir = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\equilibrium\\BAU25p" ,
                          format = GUESS,
                          land.use.included = TRUE,
                          name = "base man_only")
gpp_base_man_only <- getField(source = man_only_base, quant= "agpp_sts", first.year=2220, last.year=2249)
gpp_forest_base_man_only <- calcNewField(x=gpp_base_man_only, y=forest_area_man_only, x.col = "Forest_sum", y.col = "forest_area", op = "*" )
gpp_forest_base_man_only_mean <- aggregateYears(gpp_forest_base_man_only, method = "mean" )
gpp_forest_base_man_only_df <- as.data.frame(gpp_forest_base_man_only_mean)

#tone
man_only_tone <- defineSource(id = "tone man_only",
                          dir = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\equilibrium\\tone25p" ,
                          format = GUESS,
                          land.use.included = TRUE,
                          name = "tone man_only")
gpp_tone_man_only <- getField(source = man_only_tone, quant= "agpp_sts", first.year=2220, last.year=2249)
gpp_forest_tone_man_only <- calcNewField(x=gpp_tone_man_only, y=forest_area_man_only, x.col = "Forest_sum", y.col = "forest_area", op = "*" )
gpp_forest_tone_man_only_mean <- aggregateYears(gpp_forest_tone_man_only, method = "mean" )
gpp_forest_tone_man_only_df <- as.data.frame(gpp_forest_tone_man_only_mean)

#tobd
man_only_tobd <- defineSource(id = "tobd man_only",
                          dir = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\equilibrium\\tobd25p" ,
                          format = GUESS,
                          land.use.included = TRUE,
                          name = "tobd man_only")
gpp_tobd_man_only <- getField(source = man_only_tobd, quant= "agpp_sts", first.year=2220, last.year=2249)
gpp_forest_tobd_man_only <- calcNewField(x=gpp_tobd_man_only, y=forest_area_man_only, x.col = "Forest_sum", y.col = "forest_area", op = "*" )
gpp_forest_tobd_man_only_mean <- aggregateYears(gpp_forest_tobd_man_only, method = "mean" )
gpp_forest_tobd_man_only_df <- as.data.frame(gpp_forest_tobd_man_only_mean)

#tobe
man_only_tobe <- defineSource(id = "tobe man_only",
                          dir = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\equilibrium\\tobe25p" ,
                          format = GUESS,
                          land.use.included = TRUE,
                          name = "tobe man_only")
gpp_tobe_man_only <- getField(source = man_only_tobe, quant= "agpp_sts", first.year=2220, last.year=2249)
gpp_forest_tobe_man_only <- calcNewField(x=gpp_tobe_man_only, y=forest_area_man_only, x.col = "Forest_sum", y.col = "forest_area", op = "*" )
gpp_forest_tobe_man_only_mean <- aggregateYears(gpp_forest_tobe_man_only, method = "mean" )
gpp_forest_tobe_man_only_df <- as.data.frame(gpp_forest_tobe_man_only_mean)

gpp_man_only_mean <- cbind(gpp_forest_base_man_only_df, gpp_forest_tobd_man_only_df$value, gpp_forest_tobe_man_only_df$value,
                        gpp_forest_tone_man_only_df$value, gpp_forest_unm_man_only_df$value)

colnames(gpp_man_only_mean) <- c("Lon", "Lat", "gpp_bau", "gpp_tobd", "gpp_tobe", "gpp_tone", "gpp_unm")

library(dplyr)
cpools_man_only_mean <- cpools_man_only %>% group_by(Lon, Lat) %>% summarise(across(C_eco_veg_bau:C_eco_tot_unm, mean))
cfluxes_man_only_mean <- cfluxes_man_only %>% group_by(Lon, Lat) %>% summarise(across(F_a_resp_bau:F_fire_unm, mean))

# Define a function to calculate out_fluxes for each management type
calculate_out_fluxes <- function(df) {
  df %>%
    mutate(out_fluxes_bau = F_a_resp_bau + F_h_resp_bau + F_harv_bau + F_fire_bau,
           out_fluxes_tobd = F_a_resp_tobd + F_h_resp_tobd + F_harv_tobd + F_fire_tobd,
           out_fluxes_tobe = F_a_resp_tobe + F_h_resp_tobe + F_harv_tobe + F_fire_tobe,
           out_fluxes_tone = F_a_resp_tone + F_h_resp_tone + F_harv_tone + F_fire_tone,
           out_fluxes_unm = F_a_resp_unm + F_h_resp_unm + F_harv_unm + F_fire_unm)
}

# Apply the function to create out_fluxes columns
cfluxes_tot_man_only_mean <- calculate_out_fluxes(cfluxes_man_only_mean)
cpool_fluxes_gpp_man_only <- cbind(cpools_man_only_mean[, c(1, 2, 6, 10, 14, 18, 22)], cfluxes_tot_man_only_mean[, c(23:27)],
                                    gpp_man_only_mean[, c(3:7)])

turn_base_man_only <- cpool_fluxes_gpp_man_only[, c(1, 2, 3, 8, 13)] %>%
                mutate(turn_bau_of = C_eco_tot_bau/out_fluxes_bau,                ,
                        turn_bau_gpp = C_eco_tot_bau/gpp_bau)
turn_tobd_man_only <- cpool_fluxes_gpp_man_only[, c(1, 2, 4, 9, 14)] %>%
                mutate(turn_tobd_of = C_eco_tot_tobd/out_fluxes_tobd,
                        turn_tobd_gpp = C_eco_tot_tobd/gpp_tobd)
turn_tobe_man_only <- cpool_fluxes_gpp_man_only[, c(1, 2, 5, 10, 15)] %>%
                mutate(turn_tobe_of = C_eco_tot_tobe/out_fluxes_tobe,
                        turn_tobe_gpp = C_eco_tot_tobe/gpp_tobe)                        

turn_tone_man_only <- cpool_fluxes_gpp_man_only[, c(1, 2, 6, 11, 16)] %>%
                mutate(turn_tone_of = C_eco_tot_tone/out_fluxes_tone,
                        turn_tone_gpp = C_eco_tot_tone/gpp_tone)                        

turn_unm_man_only <- cpool_fluxes_gpp_man_only[, c(1, 2, 7, 12, 17)] %>%
                mutate(turn_unm_of = C_eco_tot_unm/out_fluxes_unm,
                        turn_unm_gpp = C_eco_tot_unm/gpp_unm)

# I put a threshold (5m height) to remove areas where trees don't grow
setwd("C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\equilibrium\\BAU25p")
height_base_man_only <- read.table("height_sts.out.gz", header = TRUE)
setwd("C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\equilibrium\\unmanaged25p")
height_unm_man_only <- read.table("height_sts.out.gz", header = TRUE)
setwd("C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\equilibrium\\toNE25p")
height_NE_man_only <- read.table("height_sts.out.gz", header = TRUE)
setwd("C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\equilibrium\\toBD25p")
height_BD_man_only <- read.table("height_sts.out.gz", header = TRUE)
setwd("C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\equilibrium\\toBE25p")
height_BE_man_only <- read.table("height_sts.out.gz", header = TRUE)

height_threshold_man_only <- function(data) {
    data <- data %>%
                    filter(Year >= 2220 & Year <= 2249) %>%
                    group_by(Lon, Lat) %>%
                    summarise(mean_height = mean(Forest_sum)) %>%
                    filter(mean_height >= 5)
}

height_5mbase_man_only <- height_threshold_man_only(height_base_man_only)
height_5munm_man_only <- height_threshold_man_only(height_unm_man_only)
height_5mNE_man_only <- height_threshold_man_only(height_NE_man_only)
height_5mBD_man_only <- height_threshold_man_only(height_BD_man_only)
height_5mBE_man_only <- height_threshold_man_only(height_BE_man_only)

turn_base_man_only_thresold <- inner_join(height_5mbase_man_only, turn_base_man_only, by = c("Lon" = "Lon", "Lat" = "Lat"))
turn_unm_man_only_thresold <- inner_join(height_5munm_man_only, turn_unm_man_only, by = c("Lon" = "Lon", "Lat" = "Lat"))
turn_ne_man_only_thresold <- inner_join(height_5mNE_man_only, turn_tone_man_only, by = c("Lon" = "Lon", "Lat" = "Lat"))
turn_bd_man_only_thresold <- inner_join(height_5mBD_man_only, turn_tobd_man_only, by = c("Lon" = "Lon", "Lat" = "Lat"))
turn_be_man_only_thresold <- inner_join(height_5mBE_man_only, turn_tobe_man_only, by = c("Lon" = "Lon", "Lat" = "Lat"))

library(tidyverse)
merged_turn_eco_man_only <- list(turn_base_man_only_thresold, turn_unm_man_only_thresold, turn_ne_man_only_thresold, 
                    turn_bd_man_only_thresold, turn_be_man_only_thresold) %>%
               reduce(full_join, by = c("Lon", "Lat"))
merged_turn_eco_man_only <- merged_turn_eco_man_only %>% dplyr::select(-contains("height"))

#write.table(merged_turn_eco_man_only, file = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\internal_review\\types_of_turn_avg_man_only",
 #          row.names = FALSE, col.names = TRUE)

merged_turn_eco_man_only <- read.table("C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\internal_review\\types_of_turn_avg_2220_2249", header = TRUE)


##############################################################
##################### man_cc #################################
##############################################################
# I put a threshold (5m height) to remove areas where trees don't grow
setwd("C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\ssp370\\BAU")
height_base_man_cc <- read.table("height_sts.out.gz", header = TRUE)
setwd("C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\ssp370\\unmanaged")
height_unm_man_cc <- read.table("height_sts.out.gz", header = TRUE)
setwd("C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\ssp370\\toNE")
height_NE_man_cc <- read.table("height_sts.out.gz", header = TRUE)
setwd("C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\ssp370\\toBD")
height_BD_man_cc <- read.table("height_sts.out.gz", header = TRUE)
setwd("C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\ssp370\\toBE")
height_BE_man_cc <- read.table("height_sts.out.gz", header = TRUE)

height_threshold_man_cc <- function(data) {
    data <- data %>%
                    filter(Year >= 2060 & Year <= 2089) %>%
                    group_by(Lon, Lat) %>%
                    summarise(mean_height = mean(Forest_sum)) %>%
                    filter(mean_height >= 5)
}

height_5mbase_man_cc <- height_threshold_man_cc(height_base_man_cc)
height_5munm_man_cc <- height_threshold_man_cc(height_unm_man_cc)
height_5mNE_man_cc <- height_threshold_man_cc(height_NE_man_cc)
height_5mBD_man_cc <- height_threshold_man_cc(height_BD_man_cc)
height_5mBE_man_cc <- height_threshold_man_cc(height_BE_man_cc)
# upload cpool data and outer fluxes data (already processed and saved)
cpools_man_cc <- read.table("C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\ssp370\\data\\pools_eco_2060_2089", header = TRUE)
cfluxes_man_cc <- read.table("C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\ssp370\\data\\outer_fluxes_eco_2060_2089", header = TRUE)

########################################
# I need to get the avg gpp for each management option, to build a df with all the gpp values
#unmanaged
man_cc_unm <- defineSource(id = "unm man_cc",
                          dir = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\ssp370\\unmanaged" ,
                          format = GUESS,
                          land.use.included = TRUE,
                          name = "unm man_cc")

#for some input I need the active fraction (to know the fraction of forest in the grid cell)
active_fraction_man_cc <- getField(source = man_cc_unm, quant = "active_fraction_sts", first.year = 2060, last.year = 2089)
#to get the forest cover, I sum the 4 forest stands (NE, ND, BE, BD)
forest_area_man_cc <- layerOp(x=active_fraction_man_cc, operator = "+", layers=c("ForestNE", "ForestBD", "ForestBE", "ForestND"), new.layer = "forest_area")
#annual gpp is stored in "agpp_sts", with the values for each stand and a column for the forest_sum
gpp_unm_man_cc <- getField(source = man_cc_unm, quant= "agpp_sts", first.year=2060, last.year=2089)
#but I need to multiply the value of forest_sum by the forest_area
gpp_forest_unm_man_cc <- calcNewField(x=gpp_unm_man_cc, y=forest_area_man_cc, x.col = "Forest_sum", y.col = "forest_area", op = "*" )
gpp_forest_unm_man_cc_mean <- aggregateYears(gpp_forest_unm_man_cc, method = "mean" )
gpp_forest_unm_man_cc_df <- as.data.frame(gpp_forest_unm_man_cc_mean)

#base
man_cc_base <- defineSource(id = "base man_cc",
                          dir = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\ssp370\\BAU" ,
                          format = GUESS,
                          land.use.included = TRUE,
                          name = "base man_cc")
gpp_base_man_cc <- getField(source = man_cc_base, quant= "agpp_sts", first.year=2060, last.year=2089)
gpp_forest_base_man_cc <- calcNewField(x=gpp_base_man_cc, y=forest_area_man_cc, x.col = "Forest_sum", y.col = "forest_area", op = "*" )
gpp_forest_base_man_cc_mean <- aggregateYears(gpp_forest_base_man_cc, method = "mean" )
gpp_forest_base_man_cc_df <- as.data.frame(gpp_forest_base_man_cc_mean)

#tone
man_cc_tone <- defineSource(id = "tone man_cc",
                          dir = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\ssp370\\tone" ,
                          format = GUESS,
                          land.use.included = TRUE,
                          name = "tone man_cc")
gpp_tone_man_cc <- getField(source = man_cc_tone, quant= "agpp_sts", first.year=2060, last.year=2089)
gpp_forest_tone_man_cc <- calcNewField(x=gpp_tone_man_cc, y=forest_area_man_cc, x.col = "Forest_sum", y.col = "forest_area", op = "*" )
gpp_forest_tone_man_cc_mean <- aggregateYears(gpp_forest_tone_man_cc, method = "mean" )
gpp_forest_tone_man_cc_df <- as.data.frame(gpp_forest_tone_man_cc_mean)

#tobd
man_cc_tobd <- defineSource(id = "tobd man_cc",
                          dir = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\ssp370\\tobd",
                          format = GUESS,
                          land.use.included = TRUE,
                          name = "tobd man_cc")
gpp_tobd_man_cc <- getField(source = man_cc_tobd, quant= "agpp_sts", first.year=2060, last.year=2089)
gpp_forest_tobd_man_cc <- calcNewField(x=gpp_tobd_man_cc, y=forest_area_man_cc, x.col = "Forest_sum", y.col = "forest_area", op = "*" )
gpp_forest_tobd_man_cc_mean <- aggregateYears(gpp_forest_tobd_man_cc, method = "mean" )
gpp_forest_tobd_man_cc_df <- as.data.frame(gpp_forest_tobd_man_cc_mean)

#tobe
man_cc_tobe <- defineSource(id = "tobe man_cc",
                          dir = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\ssp370\\tobe",
                          format = GUESS,
                          land.use.included = TRUE,
                          name = "tobe man_cc")
gpp_tobe_man_cc <- getField(source = man_cc_tobe, quant= "agpp_sts", first.year=2060, last.year=2089)
gpp_forest_tobe_man_cc <- calcNewField(x=gpp_tobe_man_cc, y=forest_area_man_cc, x.col = "Forest_sum", y.col = "forest_area", op = "*" )
gpp_forest_tobe_man_cc_mean <- aggregateYears(gpp_forest_tobe_man_cc, method = "mean" )
gpp_forest_tobe_man_cc_df <- as.data.frame(gpp_forest_tobe_man_cc_mean)

gpp_man_cc_mean <- cbind(gpp_forest_base_man_cc_df, gpp_forest_tobd_man_cc_df$value, gpp_forest_tobe_man_cc_df$value,
                        gpp_forest_tone_man_cc_df$value, gpp_forest_unm_man_cc_df$value)

colnames(gpp_man_cc_mean) <- c("Lon", "Lat", "gpp_bau", "gpp_tobd", "gpp_tobe", "gpp_tone", "gpp_unm")

cpools_man_cc_mean <- cpools_man_cc %>% group_by(Lon, Lat) %>% summarise(across(C_eco_veg_bau:C_eco_tot_unm, mean))
cfluxes_man_cc_mean <- cfluxes_man_cc %>% group_by(Lon, Lat) %>% summarise(across(F_a_resp_bau:F_fire_unm, mean))

# Apply the function to create out_fluxes columns
cfluxes_tot_man_cc_mean <- calculate_out_fluxes(cfluxes_man_cc_mean)
cpool_fluxes_gpp_man_cc <- cbind(cpools_man_cc_mean[, c(1, 2, 6, 10, 14, 18, 22)], cfluxes_tot_man_cc_mean[, c(23:27)],
                                    gpp_man_cc_mean[, c(3:7)])

turn_base_man_cc <- cpool_fluxes_gpp_man_cc[, c(1, 2, 3, 8, 13)] %>%
                mutate(turn_bau_of = C_eco_tot_bau/out_fluxes_bau,                ,
                        turn_bau_gpp = C_eco_tot_bau/gpp_bau)
turn_tobd_man_cc <- cpool_fluxes_gpp_man_cc[, c(1, 2, 4, 9, 14)] %>%
                mutate(turn_tobd_of = C_eco_tot_tobd/out_fluxes_tobd,
                        turn_tobd_gpp = C_eco_tot_tobd/gpp_tobd)
turn_tobe_man_cc <- cpool_fluxes_gpp_man_cc[, c(1, 2, 5, 10, 15)] %>%
                mutate(turn_tobe_of = C_eco_tot_tobe/out_fluxes_tobe,
                        turn_tobe_gpp = C_eco_tot_tobe/gpp_tobe)                        

turn_tone_man_cc <- cpool_fluxes_gpp_man_cc[, c(1, 2, 6, 11, 16)] %>%
                mutate(turn_tone_of = C_eco_tot_tone/out_fluxes_tone,
                        turn_tone_gpp = C_eco_tot_tone/gpp_tone)                        

turn_unm_man_cc <- cpool_fluxes_gpp_man_cc[, c(1, 2, 7, 12, 17)] %>%
                mutate(turn_unm_of = C_eco_tot_unm/out_fluxes_unm,
                        turn_unm_gpp = C_eco_tot_unm/gpp_unm)

turn_base_man_cc_thresold <- inner_join(height_5mbase_man_cc, turn_base_man_cc, by = c("Lon" = "Lon", "Lat" = "Lat"))
turn_unm_man_cc_thresold <- inner_join(height_5munm_man_cc, turn_unm_man_cc, by = c("Lon" = "Lon", "Lat" = "Lat"))
turn_ne_man_cc_thresold <- inner_join(height_5mNE_man_cc, turn_tone_man_cc, by = c("Lon" = "Lon", "Lat" = "Lat"))
turn_bd_man_cc_thresold <- inner_join(height_5mBD_man_cc, turn_tobd_man_cc, by = c("Lon" = "Lon", "Lat" = "Lat"))
turn_be_man_cc_thresold <- inner_join(height_5mBE_man_cc, turn_tobe_man_cc, by = c("Lon" = "Lon", "Lat" = "Lat"))

merged_turn_eco_man_cc <- list(turn_base_man_cc_thresold, turn_unm_man_cc_thresold, turn_ne_man_cc_thresold, 
                    turn_bd_man_cc_thresold, turn_be_man_cc_thresold) %>%
               reduce(full_join, by = c("Lon", "Lat"))
merged_turn_eco_man_cc <- merged_turn_eco_man_cc %>% dplyr::select(-contains("height"))

# write.table(merged_turn_eco_man_cc, file = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\internal_review\\types_of_turn_avg_man_cc",
#            row.names = FALSE, col.names = TRUE)
