# code to plot the figures
# management scenarios:
# base = baseline
# unm = unmanaged
# NE = transition to needleleaf evergreen
# BD = transition to broadleaf deciduous
# BE = transition to broadleaf evergreen

# upload necessary libraries
library(raster)
library(ggplot2)
library(dplyr)
library(sf)
sf_use_s2(FALSE)
library(patchwork)
library(maps)
library(rnaturalearth)
library(grid)
library(gridExtra)
library(lemon)
library(ggpubr)
library(purrr)
library(DGVMTools)
library(tibble)
library(tidyverse)

# create palette 
pal_turn <- c("#00aba9","#7dc8b0","#c0e3c4","#f7ffe8",
    "#e7dc9f","#eaaf5d","#f37735",  na.value="#000000")

# set map extent and upload necessary borders
e <- as(extent(-9.75, 31.75, 35.25, 71.25), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
europe <- ne_countries(continent="europe", scale = "medium")
ocean <- ne_download(scale = 110, type = "ocean", category = "physical")

setwd("C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\LPJG_out")

#####################################################
#...................Fig2.............................
#####################################################
# upload tree height data to later set a height threshold
height_base_man_only <- read.table("man_only\\base\\height_sts.out.gz", header = TRUE) 
height_unm_man_only <- read.table("man_only\\unmanaged\\height_sts.out.gz", header = TRUE)
height_NE_man_only <- read.table("man_only\\to_ne\\height_sts.out.gz", header = TRUE)
height_BD_man_only <- read.table("man_only\\to_bd\\height_sts.out.gz", header = TRUE)
height_BE_man_only <- read.table("man_only\\to_be\\height_sts.out.gz", header = TRUE)

# function to set the 5m-height threshold
# mean height in the period 2220 - 2249
height_threshold_man_only <- function(data) {
    data <- data %>%
                    filter(Year >= 2220 & Year <= 2249) %>%
                    group_by(Lon, Lat) %>%
                    summarise(mean_height = mean(Forest_sum)) %>%
                    filter(mean_height >= 5)
}

# apply the height threshold function
height_5mbase_man_only <- height_threshold_man_only(height_base_man_only)
height_5munm_man_only <- height_threshold_man_only(height_unm_man_only)
height_5mNE_man_only <- height_threshold_man_only(height_NE_man_only)
height_5mBD_man_only <- height_threshold_man_only(height_BD_man_only)
height_5mBE_man_only <- height_threshold_man_only(height_BE_man_only)

#.......... ECOSYSTEM .............
# upload the ECOSYSTEM turnover data
turnover_eco_man_only_base_df <- as.data.frame(readRDS("man_only\\base\\turnover_eco_man_only_base.RData"))
turnover_eco_man_only_unm_df <- as.data.frame(readRDS("man_only\\unmanaged\\turnover_eco_man_only_unm.RData"))
turnover_eco_man_only_to_ne_df <- as.data.frame(readRDS("man_only\\to_ne\\turnover_eco_man_only_to_ne.RData")) 
turnover_eco_man_only_to_bd_df <- as.data.frame(readRDS("man_only\\to_bd\\turnover_eco_man_only_to_bd.RData")) 
turnover_eco_man_only_to_be_df <- as.data.frame(readRDS("man_only\\to_be\\turnover_eco_man_only_to_be.RData")) 

# put together the turnover and the height dataset,
# keeping only rows where height > 5m
turnover_eco_base_thresold_man_only <- inner_join(height_5mbase_man_only, 
    turnover_eco_man_only_base_df, by = c("Lon" = "Lon", "Lat" = "Lat"))
turnover_eco_unm_thresold_man_only <- inner_join(height_5munm_man_only,
    turnover_eco_man_only_unm_df, by = c("Lon" = "Lon", "Lat" = "Lat"))
turnover_eco_NE_thresold_man_only <- inner_join(height_5mNE_man_only,
    turnover_eco_man_only_to_ne_df, by = c("Lon" = "Lon", "Lat" = "Lat"))
turnover_eco_BD_thresold_man_only <- inner_join(height_5mBD_man_only, 
    turnover_eco_man_only_to_bd_df, by = c("Lon" = "Lon", "Lat" = "Lat"))
turnover_eco_BE_thresold_man_only <- inner_join(height_5mBE_man_only, 
    turnover_eco_man_only_to_be_df, by = c("Lon" = "Lon", "Lat" = "Lat"))

# put together all the management options in a single dataframe
# each option is a column
merged_turn_eco_man_only <- list(turnover_eco_base_thresold_man_only, turnover_eco_unm_thresold_man_only,
     turnover_eco_NE_thresold_man_only, turnover_eco_BD_thresold_man_only, turnover_eco_BE_thresold_man_only) %>%
               reduce(full_join, by = c("Lon", "Lat"))
# remove the "height" column                
merged_turn_eco_man_only <- merged_turn_eco_man_only %>% dplyr::select(-contains("height"))
#rename the other columns ("t_base" = turnover base, ...)
colnames(merged_turn_eco_man_only) <- c("Lon", "Lat", "t_base", "t_unm", "t_NE", "t_BD", "t_BE")

# create a function to plot the maps
create_plot <- function(data, title, tag, column1, column2) {
    ggplot(data = data) +
        geom_raster(aes(x = Lon, y = Lat, fill = .data[[column1]] - .data[[column2]])) +
        scale_fill_gradient2(low = "#00aba9", mid = "#f7ffe8", high = '#f37735', 
                             na.value = "#FFFFFF", limits = c(-40, 40), 
                             breaks = seq(from = 40, to = -40, by = -10),
                             labels = seq(from = 40, to = -40, by = -10)) +
        labs(tag = tag) +
        guides(fill = guide_legend(title = expression(Delta ~ years))) +
        ggtitle(title) +
        theme_void() +
        theme(plot.tag.position = c(0.03, 0.97),
              plot.title = element_text(hjust = 0.15, size = 12, margin = margin(t = 10, b = -20)),
              panel.background = element_rect(fill = "white", colour = "black")) +
               geom_sf(data = europe, fill = "transparent") +
                            geom_sf(data = ocean, fill = "light blue") +
                            coord_sf(xlim = c(-10.5, 32.5), ylim = c(34, 71.5),
                            expand = FALSE)
}

# plot each management option
plot_diff_turn_eco_unm_base_man_only <- create_plot(data = merged_turn_eco_man_only, 
            title = "Unmanaged - Baseline", 
            tag = "A", 
            column1 = "t_unm", 
            column2 = "t_base")

plot_diff_turn_eco_NE_base_man_only <- create_plot(data = merged_turn_eco_man_only, 
            title = "toNE - Baseline", 
            tag = "B", 
            column1 = "t_NE", 
            column2 = "t_base")

plot_diff_turn_eco_BD_base_man_only <- create_plot(data = merged_turn_eco_man_only, 
            title = "toBD - Baseline", 
            tag = "C", 
            column1 = "t_BD", 
            column2 = "t_base")

plot_diff_turn_eco_BE_base_man_only <- create_plot(data = merged_turn_eco_man_only, 
            title = "toBE - Baseline", 
            tag = "D", 
            column1 = "t_BE", 
            column2 = "t_base")

# put them together
diff_turn_eco_rest_vs_base_thresh_man_only <-grid_arrange_shared_legend(plot_diff_turn_eco_unm_base_man_only,
                                plot_diff_turn_eco_NE_base_man_only, plot_diff_turn_eco_BD_base_man_only,
                                plot_diff_turn_eco_BE_base_man_only,
                                nrow = 1, ncol = 4, position = "right")

#........... STEM ..........
# upload the STEM turnover data and repeat the previous 
# steps with this data (p.s. a new plot_stem function is created 
# (the only difference is a change in the range of values shown in the plot))
turnover_stem_man_only_base_df <- as.data.frame(readRDS("man_only\\base\\turnover_stem_man_only_base.RData"))
turnover_stem_man_only_unm_df <- as.data.frame(readRDS("man_only\\unmanaged\\turnover_stem_man_only_unm.RData"))
turnover_stem_man_only_to_ne_df <- as.data.frame(readRDS("man_only\\to_ne\\turnover_stem_man_only_to_ne.RData")) 
turnover_stem_man_only_to_bd_df <- as.data.frame(readRDS("man_only\\to_bd\\turnover_stem_man_only_to_bd.RData")) 
turnover_stem_man_only_to_be_df <- as.data.frame(readRDS("man_only\\to_be\\turnover_stem_man_only_to_be.RData")) 

turnover_stem_base_thresold_man_only <- inner_join(height_5mbase_man_only,
    turnover_stem_man_only_base_df, by = c("Lon" = "Lon", "Lat" = "Lat"))
turnover_stem_unm_thresold_man_only <- inner_join(height_5munm_man_only, 
    turnover_stem_man_only_unm_df, by = c("Lon" = "Lon", "Lat" = "Lat"))
turnover_stem_NE_thresold_man_only <- inner_join(height_5mNE_man_only, 
    turnover_stem_man_only_to_ne_df, by = c("Lon" = "Lon", "Lat" = "Lat"))
turnover_stem_BD_thresold_man_only <- inner_join(height_5mBD_man_only, 
    turnover_stem_man_only_to_bd_df, by = c("Lon" = "Lon", "Lat" = "Lat"))
turnover_stem_BE_thresold_man_only <- inner_join(height_5mBE_man_only, 
    turnover_stem_man_only_to_be_df, by = c("Lon" = "Lon", "Lat" = "Lat"))

merged_turn_stem_man_only <- list(turnover_stem_base_thresold_man_only, turnover_stem_unm_thresold_man_only,
    turnover_stem_NE_thresold_man_only, turnover_stem_BD_thresold_man_only, turnover_stem_BE_thresold_man_only) %>%
               reduce(full_join, by = c("Lon", "Lat"))
merged_turn_stem_man_only <- merged_turn_stem_man_only %>% dplyr::select(-contains("height"))
colnames(merged_turn_stem_man_only) <- c("Lon", "Lat", "t_base", "t_unm", "t_NE", "t_BD", "t_BE")

create_plot_stem <- function(data, title, tag, column1, column2) {
    ggplot(data = data) +
        geom_raster(aes(x = Lon, y = Lat, fill = .data[[column1]] - .data[[column2]])) +
        scale_fill_gradient2(low = "#00aba9", mid = "#f7ffe8", high = '#f37735', 
                             na.value = "#FFFFFF", limits = c(-75, 75), 
                             breaks = seq(from = 75, to = -75, by = -15),
                             labels = seq(from = 75, to = -75, by = -15)) +
        labs(tag = tag) +
        guides(fill = guide_legend(title = expression(Delta ~ years))) +
        ggtitle(title) +
        theme_void() +
        theme(plot.tag.position = c(0.03, 0.97),
              plot.title = element_text(hjust = 0.15, size = 12, margin = margin(t = 10, b = -20)),
              panel.background = element_rect(fill = "white", colour = "black")) +
         geom_sf(data = europe, fill = "transparent") +
                            geom_sf(data = ocean, fill = "light blue") +
                            coord_sf(xlim = c(-10.5, 32.5), ylim = c(34, 71.5),
                            expand = FALSE)
}

plot_diff_turn_stem_unm_base_man_only <- create_plot_stem(data = merged_turn_stem_man_only, 
            title = "Unmanaged - Baseline", 
            tag = "E", 
            column1 = "t_unm", 
            column2 = "t_base")
plot_diff_turn_stem_NE_base_man_only <- create_plot_stem(data = merged_turn_stem_man_only, 
            title = "toNE - Baseline", 
            tag = "F", 
            column1 = "t_NE", 
            column2 = "t_base")
plot_diff_turn_stem_BD_base_man_only <- create_plot_stem(data = merged_turn_stem_man_only, 
            title = "toBD - Baseline", 
            tag = "G", 
            column1 = "t_BD", 
            column2 = "t_base")
plot_diff_turn_stem_BE_base_man_only <- create_plot_stem(data = merged_turn_stem_man_only, 
            title = "toBE - Baseline", 
            tag = "H", 
            column1 = "t_BE", 
            column2 = "t_base")

diff_turn_stem_rest_vs_base_thresh_man_only <-grid_arrange_shared_legend(plot_diff_turn_stem_unm_base_man_only,
                                plot_diff_turn_stem_NE_base_man_only, plot_diff_turn_stem_BD_base_man_only,
                                plot_diff_turn_stem_BE_base_man_only,
                                nrow = 1, ncol = 4, position = "right")

#........... SOIL ...............
## upload the SOIL turnover data and repeat the previous steps
# here a new plot function is not needed (the range is the same as for the ecosystem)
turnover_tot_soil_man_only_base_df <- as.data.frame(readRDS("man_only\\base\\turnover_tot_soil_man_only_base.RData"))
turnover_tot_soil_man_only_unm_df <- as.data.frame(readRDS("man_only\\unmanaged\\turnover_tot_soil_man_only_unm.RData"))
turnover_tot_soil_man_only_to_ne_df <- as.data.frame(readRDS("man_only\\to_ne\\turnover_tot_soil_man_only_to_ne.RData")) 
turnover_tot_soil_man_only_to_bd_df <- as.data.frame(readRDS("man_only\\to_bd\\turnover_tot_soil_man_only_to_bd.RData")) 
turnover_tot_soil_man_only_to_be_df <- as.data.frame(readRDS("man_only\\to_be\\turnover_tot_soil_man_only_to_be.RData"))

turnover_tot_soil_base_thresold_man_only <- inner_join(height_5mbase_man_only,
    turnover_tot_soil_man_only_base_df, by = c("Lon" = "Lon", "Lat" = "Lat"))
turnover_tot_soil_unm_thresold_man_only <- inner_join(height_5munm_man_only, 
    turnover_tot_soil_man_only_unm_df, by = c("Lon" = "Lon", "Lat" = "Lat"))
turnover_tot_soil_NE_thresold_man_only <- inner_join(height_5mNE_man_only, 
    turnover_tot_soil_man_only_to_ne_df, by = c("Lon" = "Lon", "Lat" = "Lat"))
turnover_tot_soil_BD_thresold_man_only <- inner_join(height_5mBD_man_only, 
    turnover_tot_soil_man_only_to_bd_df, by = c("Lon" = "Lon", "Lat" = "Lat"))
turnover_tot_soil_BE_thresold_man_only <- inner_join(height_5mBE_man_only, 
    turnover_tot_soil_man_only_to_be_df, by = c("Lon" = "Lon", "Lat" = "Lat"))

merged_turn_tot_soil_man_only <- list(turnover_tot_soil_base_thresold_man_only, turnover_tot_soil_unm_thresold_man_only,
    turnover_tot_soil_NE_thresold_man_only, turnover_tot_soil_BD_thresold_man_only, turnover_tot_soil_BE_thresold_man_only) %>%
               reduce(full_join, by = c("Lon", "Lat"))
merged_turn_tot_soil_man_only <- merged_turn_tot_soil_man_only %>% dplyr::select(-contains("height"))
colnames(merged_turn_tot_soil_man_only) <- c("Lon", "Lat", "t_base", "t_unm", "t_NE", "t_BD", "t_BE")

plot_diff_turn_tot_soil_unm_base_man_only <- create_plot(data = merged_turn_tot_soil_man_only, 
            title = "Unmanaged - Baseline", 
            tag = "I", 
            column1 = "t_unm", 
            column2 = "t_base")
plot_diff_turn_tot_soil_NE_base_man_only <- create_plot(data = merged_turn_tot_soil_man_only, 
            title = "toNE - Baseline", 
            tag = "J", 
            column1 = "t_NE", 
            column2 = "t_base")
plot_diff_turn_tot_soil_BD_base_man_only <- create_plot(data = merged_turn_tot_soil_man_only, 
            title = "toBD - Baseline", 
            tag = "K", 
            column1 = "t_BD", 
            column2 = "t_base")
plot_diff_turn_tot_soil_BE_base_man_only <- create_plot(data = merged_turn_tot_soil_man_only, 
            title = "toBE - Baseline", 
            tag = "L", 
            column1 = "t_BE", 
            column2 = "t_base")
diff_turn_tot_soil_rest_vs_base_thresh_man_only <-grid_arrange_shared_legend(plot_diff_turn_tot_soil_unm_base_man_only,
                                plot_diff_turn_tot_soil_NE_base_man_only, plot_diff_turn_tot_soil_BD_base_man_only,
                                plot_diff_turn_tot_soil_BE_base_man_only,
                                nrow = 1, ncol = 4, position = "right")

# put together ecosystem, stem and soil
# Add a title for each group of plots
title_eco <- textGrob("ECOSYSTEM", gp = gpar(fontsize = 15, hjust = 0))
title_stem <- textGrob("STEM", gp = gpar(fontsize = 15, hjust = 0))
title_tot_soil <- textGrob("SOIL", gp = gpar(fontsize = 15, hjust = 0))

# Use grid.arrange to add the title
fig2 <- grid.arrange(title_eco, diff_turn_eco_rest_vs_base_thresh_man_only,
                                  title_stem, diff_turn_stem_rest_vs_base_thresh_man_only,
                                  title_tot_soil,  diff_turn_tot_soil_rest_vs_base_thresh_man_only, 
                                  nrow = 6, ncol= 1, heights = c(1, 10, 1, 10, 1, 10))

# ggsave(plot = fig2, path = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\internal_review",
#                filename = "fig2.jpeg",
#                dpi = 300, width = 27, height = 27, units = c("cm"), bg = "white")

#####################################################
#...................Fig3.............................
#####################################################
# repeat the steps above, but for the management and climate change data
height_base_man_cc <- read.table("man_cc\\base\\height_sts.out.gz", header = TRUE) 
height_unm_man_cc <- read.table("man_cc\\unmanaged\\height_sts.out.gz", header = TRUE)
height_NE_man_cc <- read.table("man_cc\\to_ne\\height_sts.out.gz", header = TRUE)
height_BD_man_cc <- read.table("man_cc\\to_bd\\height_sts.out.gz", header = TRUE)
height_BE_man_cc <- read.table("man_cc\\to_be\\height_sts.out.gz", header = TRUE)

# function to set the 5m-height threshold
# mean height in the period 2220 - 2249
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


#.......... ECOSYSTEM .............
# upload the ECOSYSTEM turnover data
# in this case, I already have all the management options in one dataset
turnover_eco_man_cc_base_df <- as.data.frame(readRDS("man_cc\\base\\turnover_eco_man_cc_base.RData"))
turnover_eco_man_cc_unm_df <- as.data.frame(readRDS("man_cc\\unmanaged\\turnover_eco_man_cc_unm.RData"))
turnover_eco_man_cc_to_ne_df <- as.data.frame(readRDS("man_cc\\to_ne\\turnover_eco_man_cc_to_ne.RData")) 
turnover_eco_man_cc_to_bd_df <- as.data.frame(readRDS("man_cc\\to_bd\\turnover_eco_man_cc_to_bd.RData")) 
turnover_eco_man_cc_to_be_df <- as.data.frame(readRDS("man_cc\\to_be\\turnover_eco_man_cc_to_be.RData")) 

# put together the turnover and the height dataset,
# keeping only rows where height > 5m
turnover_eco_base_thresold_man_cc <- inner_join(height_5mbase_man_cc, 
    turnover_eco_man_cc_base_df, by = c("Lon" = "Lon", "Lat" = "Lat"))
turnover_eco_unm_thresold_man_cc <- inner_join(height_5munm_man_cc,
    turnover_eco_man_cc_unm_df, by = c("Lon" = "Lon", "Lat" = "Lat"))
turnover_eco_NE_thresold_man_cc <- inner_join(height_5mNE_man_cc,
    turnover_eco_man_cc_to_ne_df, by = c("Lon" = "Lon", "Lat" = "Lat"))
turnover_eco_BD_thresold_man_cc <- inner_join(height_5mBD_man_cc, 
    turnover_eco_man_cc_to_bd_df, by = c("Lon" = "Lon", "Lat" = "Lat"))
turnover_eco_BE_thresold_man_cc <- inner_join(height_5mBE_man_cc, 
    turnover_eco_man_cc_to_be_df, by = c("Lon" = "Lon", "Lat" = "Lat"))

# put together all the management options in a single dataframe
# each option is a column
merged_turn_eco_man_cc <- list(turnover_eco_base_thresold_man_cc, turnover_eco_unm_thresold_man_cc,
     turnover_eco_NE_thresold_man_cc, turnover_eco_BD_thresold_man_cc, turnover_eco_BE_thresold_man_cc) %>%
               reduce(full_join, by = c("Lon", "Lat"))
# remove the "height" column                
merged_turn_eco_man_cc <- merged_turn_eco_man_cc %>% dplyr::select(-contains("height"))
#rename the other columns ("t_base" = turnover base, ...)
colnames(merged_turn_eco_man_cc) <- c("Lon", "Lat", "t_base", "t_unm", "t_NE", "t_BD", "t_BE")

plot_diff_turn_eco_unm_base_man_cc <- create_plot(data = merged_turn_eco_man_cc, 
            title = "Unmanaged - Baseline", 
            tag = "A", 
            column1 = "t_unm", 
            column2 = "t_base")

plot_diff_turn_eco_NE_base_man_cc <- create_plot(data = merged_turn_eco_man_cc, 
            title = "toNE - Baseline", 
            tag = "B", 
            column1 = "t_NE", 
            column2 = "t_base")

plot_diff_turn_eco_BD_base_man_cc <- create_plot(data = merged_turn_eco_man_cc, 
            title = "toBD - Baseline", 
            tag = "C", 
            column1 = "t_BD", 
            column2 = "t_base")

plot_diff_turn_eco_BE_base_man_cc <- create_plot(data = merged_turn_eco_man_cc, 
            title = "toBE - Baseline", 
            tag = "D", 
            column1 = "t_BE", 
            column2 = "t_base")

diff_turn_eco_rest_vs_base_thresh_man_cc <-grid_arrange_shared_legend(plot_diff_turn_eco_unm_base_man_cc,
                                plot_diff_turn_eco_NE_base_man_cc, plot_diff_turn_eco_BD_base_man_cc,
                                plot_diff_turn_eco_BE_base_man_cc,
                                nrow = 1, ncol = 4, position = "right")


#.......... STEM .............
# upload the STEM turnover data
turnover_stem_man_cc_base_df <- as.data.frame(readRDS("man_cc\\base\\turnover_stem_man_cc_base.RData"))
turnover_stem_man_cc_unm_df <- as.data.frame(readRDS("man_cc\\unmanaged\\turnover_stem_man_cc_unm.RData"))
turnover_stem_man_cc_to_ne_df <- as.data.frame(readRDS("man_cc\\to_ne\\turnover_stem_man_cc_to_ne.RData")) 
turnover_stem_man_cc_to_bd_df <- as.data.frame(readRDS("man_cc\\to_bd\\turnover_stem_man_cc_to_bd.RData")) 
turnover_stem_man_cc_to_be_df <- as.data.frame(readRDS("man_cc\\to_be\\turnover_stem_man_cc_to_be.RData")) 

# put together the turnover and the height dataset,
# keeping only rows where height > 5m
turnover_stem_base_thresold_man_cc <- inner_join(height_5mbase_man_cc, 
    turnover_stem_man_cc_base_df, by = c("Lon" = "Lon", "Lat" = "Lat"))
turnover_stem_unm_thresold_man_cc <- inner_join(height_5munm_man_cc,
    turnover_stem_man_cc_unm_df, by = c("Lon" = "Lon", "Lat" = "Lat"))
turnover_stem_NE_thresold_man_cc <- inner_join(height_5mNE_man_cc,
    turnover_stem_man_cc_to_ne_df, by = c("Lon" = "Lon", "Lat" = "Lat"))
turnover_stem_BD_thresold_man_cc <- inner_join(height_5mBD_man_cc, 
    turnover_stem_man_cc_to_bd_df, by = c("Lon" = "Lon", "Lat" = "Lat"))
turnover_stem_BE_thresold_man_cc <- inner_join(height_5mBE_man_cc, 
    turnover_stem_man_cc_to_be_df, by = c("Lon" = "Lon", "Lat" = "Lat"))

# put together all the management options in a single dataframe
# each option is a column
merged_turn_stem_man_cc <- list(turnover_stem_base_thresold_man_cc, turnover_stem_unm_thresold_man_cc,
     turnover_stem_NE_thresold_man_cc, turnover_stem_BD_thresold_man_cc, turnover_stem_BE_thresold_man_cc) %>%
               reduce(full_join, by = c("Lon", "Lat"))
# remove the "height" column                
merged_turn_stem_man_cc <- merged_turn_stem_man_cc %>% dplyr::select(-contains("height"))
#rename the other columns ("t_base" = turnover base, ...)
colnames(merged_turn_stem_man_cc) <- c("Lon", "Lat", "t_base", "t_unm", "t_NE", "t_BD", "t_BE")

# here I need another plot function, the range is again different
create_plot_stem_man_cc <- function(data, title, tag, column1, column2) {
    ggplot(data = data) +
        geom_raster(aes(x = Lon, y = Lat, fill = .data[[column1]] - .data[[column2]])) +
        scale_fill_gradient2(low = "#00aba9", mid = "#f7ffe8", high = '#f37735', 
                             na.value = "#FFFFFF", limits = c(-120, 120), 
                             breaks = seq(from = 100, to = -100, by = -20),
                             labels = seq(from = 100, to = -100, by = -20)) +
        labs(tag = tag) +
        guides(fill = guide_legend(title = expression(Delta ~ years))) +
        ggtitle(title) +
        theme_void() +
        theme(plot.tag.position = c(0.03, 0.97),
              plot.title = element_text(hjust = 0.15, size = 12, margin = margin(t = 10, b = -20)),
              panel.background = element_rect(fill = "white", colour = "black")) +
        geom_sf(data = europe, fill = "transparent") +
                geom_sf(data = ocean, fill = "light blue") +
                coord_sf(xlim = c(-10.5, 32.5), ylim = c(34, 71.5),
                expand = FALSE)
}

plot_diff_turn_stem_unm_base_man_cc <- create_plot_stem_man_cc(data = merged_turn_stem_man_cc, 
            title = "Unmanaged - Baseline", 
            tag = "E", 
            column1 = "t_unm", 
            column2 = "t_base")

plot_diff_turn_stem_NE_base_man_cc <- create_plot_stem_man_cc(data = merged_turn_stem_man_cc, 
            title = "toNE - Baseline", 
            tag = "F", 
            column1 = "t_NE", 
            column2 = "t_base")

plot_diff_turn_stem_BD_base_man_cc <- create_plot_stem_man_cc(data = merged_turn_stem_man_cc, 
            title = "toBD - Baseline", 
            tag = "G", 
            column1 = "t_BD", 
            column2 = "t_base")

plot_diff_turn_stem_BE_base_man_cc <- create_plot_stem_man_cc(data = merged_turn_stem_man_cc, 
            title = "toBE - Baseline", 
            tag = "H", 
            column1 = "t_BE", 
            column2 = "t_base")

diff_turn_stem_rest_vs_base_thresh_man_cc <-grid_arrange_shared_legend(plot_diff_turn_stem_unm_base_man_cc,
                                plot_diff_turn_stem_NE_base_man_cc, plot_diff_turn_stem_BD_base_man_cc,
                                plot_diff_turn_stem_BE_base_man_cc,
                                nrow = 1, ncol = 4, position = "right")

#.......... SOIL .............
# upload the SOIL turnover data
turnover_tot_soil_man_cc_base_df <- as.data.frame(readRDS("man_cc\\base\\turnover_tot_soil_man_cc_base.RData"))
turnover_tot_soil_man_cc_unm_df <- as.data.frame(readRDS("man_cc\\unmanaged\\turnover_tot_soil_man_cc_unm.RData"))
turnover_tot_soil_man_cc_to_ne_df <- as.data.frame(readRDS("man_cc\\to_ne\\turnover_tot_soil_man_cc_to_ne.RData")) 
turnover_tot_soil_man_cc_to_bd_df <- as.data.frame(readRDS("man_cc\\to_bd\\turnover_tot_soil_man_cc_to_bd.RData")) 
turnover_tot_soil_man_cc_to_be_df <- as.data.frame(readRDS("man_cc\\to_be\\turnover_tot_soil_man_cc_to_be.RData")) 

# put together the turnover and the height dataset,
# keeping only rows where height > 5m
turnover_tot_soil_base_thresold_man_cc <- inner_join(height_5mbase_man_cc, 
    turnover_tot_soil_man_cc_base_df, by = c("Lon" = "Lon", "Lat" = "Lat"))
turnover_tot_soil_unm_thresold_man_cc <- inner_join(height_5munm_man_cc,
    turnover_tot_soil_man_cc_unm_df, by = c("Lon" = "Lon", "Lat" = "Lat"))
turnover_tot_soil_NE_thresold_man_cc <- inner_join(height_5mNE_man_cc,
    turnover_tot_soil_man_cc_to_ne_df, by = c("Lon" = "Lon", "Lat" = "Lat"))
turnover_tot_soil_BD_thresold_man_cc <- inner_join(height_5mBD_man_cc, 
    turnover_tot_soil_man_cc_to_bd_df, by = c("Lon" = "Lon", "Lat" = "Lat"))
turnover_tot_soil_BE_thresold_man_cc <- inner_join(height_5mBE_man_cc, 
    turnover_tot_soil_man_cc_to_be_df, by = c("Lon" = "Lon", "Lat" = "Lat"))

# put together all the management options in a single dataframe
# each option is a column
merged_turn_tot_soil_man_cc <- list(turnover_tot_soil_base_thresold_man_cc, turnover_tot_soil_unm_thresold_man_cc,
     turnover_tot_soil_NE_thresold_man_cc, turnover_tot_soil_BD_thresold_man_cc, turnover_tot_soil_BE_thresold_man_cc) %>%
               reduce(full_join, by = c("Lon", "Lat"))
# remove the "height" column                
merged_turn_tot_soil_man_cc <- merged_turn_tot_soil_man_cc %>% dplyr::select(-contains("height"))
#rename the other columns ("t_base" = turnover base, ...)
colnames(merged_turn_tot_soil_man_cc) <- c("Lon", "Lat", "t_base", "t_unm", "t_NE", "t_BD", "t_BE")

plot_diff_turn_tot_soil_unm_base_man_cc <- create_plot(data = merged_turn_tot_soil_man_cc, 
            title = "Unmanaged - Baseline", 
            tag = "I", 
            column1 = "t_unm", 
            column2 = "t_base")

plot_diff_turn_tot_soil_NE_base_man_cc <- create_plot(data = merged_turn_tot_soil_man_cc, 
            title = "toNE - Baseline", 
            tag = "J", 
            column1 = "t_NE", 
            column2 = "t_base")

plot_diff_turn_tot_soil_BD_base_man_cc <- create_plot(data = merged_turn_tot_soil_man_cc, 
            title = "toBD - Baseline", 
            tag = "K", 
            column1 = "t_BD", 
            column2 = "t_base")

plot_diff_turn_tot_soil_BE_base_man_cc <- create_plot(data = merged_turn_tot_soil_man_cc, 
            title = "toBE - Baseline", 
            tag = "L", 
            column1 = "t_BE", 
            column2 = "t_base")

diff_turn_tot_soil_rest_vs_base_thresh_man_cc <-grid_arrange_shared_legend(plot_diff_turn_tot_soil_unm_base_man_cc,
                                plot_diff_turn_tot_soil_NE_base_man_cc, plot_diff_turn_tot_soil_BD_base_man_cc,
                                plot_diff_turn_tot_soil_BE_base_man_cc,
                                nrow = 1, ncol = 4, position = "right")

# Use grid.arrange to add the title
fig3 <- grid.arrange(title_eco, diff_turn_eco_rest_vs_base_thresh_man_cc,
                                  title_stem, diff_turn_stem_rest_vs_base_thresh_man_cc,
                                  title_tot_soil,  diff_turn_tot_soil_rest_vs_base_thresh_man_cc, 
                                  nrow = 6, ncol= 1, heights = c(1, 10, 1, 10, 1, 10))

#  ggsave(plot = fig3, path = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\internal_review",
#                filename = "fig3.jpeg",
#                dpi = 300, width = 27, height = 27, units = c("cm"), bg = "white")
#####################################################
#...................Fig4.............................
#####################################################
#add a column to the objects "merged_turn_eco", "merged_turn_stem", "merged_turn_tot_soil"
#with the management option with the longest turnover time
get_max_option <- function(data) {
                                data %>% 
                                rowwise() %>%
                                mutate(max_value = max(c(t_base, t_unm, t_NE, t_BD, t_BE), 
                                    na.rm = TRUE),
                                    type = if(all(is.na(c(t_base, t_unm, t_NE, t_BD, t_BE)))) {
                                    NA_character_
                                    } else {
                                    col_names <- c("Baseline", "Unmanaged", "ToNE", "ToBD", "ToBE")
                                    values <- c(t_base, t_unm, t_NE, t_BD, t_BE)
                                    col_names[which.max(replace(values, is.na(values), -Inf))]
                                    }
                                ) %>%
                                ungroup()
}

turnover_eco_man_only_thresh_longest <- get_max_option(merged_turn_eco_man_only)
turnover_stem_man_only_thresh_longest <- get_max_option(merged_turn_stem_man_only) 
turnover_tot_soil_man_only_thresh_longest <- get_max_option(merged_turn_tot_soil_man_only) 

ordered_management <- c("Unmanaged", "Baseline", "ToNE", "ToBD", "ToBE")
pal2 <- c( "#9164dc",
            "#5aca7e",
             "#F98400",
              "#5BBCD6",
              "#EBCC2A")
     
# create a function to plot the maps
create_plot_longest_tau <- function(data, title, tag, column1) {
    ggplot(data = data) +
        geom_raster(aes(x = Lon, y = Lat, fill = factor(type, ordered_management))) +
                                        coord_equal() + 
                                        scale_fill_manual(values = pal2) +
        labs(fill = "Management", tag = tag) +
        ggtitle(title) +
        theme_void() +
        theme(plot.title.position = "plot", # Title above the plot
              plot.title = element_text(hjust = 0.15, size = 12, margin = margin(t = 10, b = -20)),
              legend.position = "none",
              panel.background = element_rect(fill = "white", colour = "black")) +
        geom_sf(data = europe, fill = "transparent") +
        geom_sf(data = ocean, fill = "light blue") +
        coord_sf(xlim = c(-10.5, 32.5), ylim = c(34, 71.5),
                 expand = FALSE)
}

plot_longest_tau_eco_man_only <- create_plot_longest_tau(
            data = subset(turnover_eco_man_only_thresh_longest, !is.na(type)), 
            title = "ECOSYSTEM",
            tag = "A", 
            column1 = "max_value")

plot_longest_tau_stem_man_only <- create_plot_longest_tau(
            data = subset(turnover_stem_man_only_thresh_longest, !is.na(type)), 
            title = "STEM",
            tag = "C", 
            column1 = "max_value")

plot_longest_tau_tot_soil_man_only <- create_plot_longest_tau(
            data = subset(turnover_tot_soil_man_only_thresh_longest, !is.na(type)), 
            title = "SOIL",
            tag = "E", 
            column1 = "max_value")

# same as above, but exclude the unmanaged option, to see which of
# the other options has the longest tau
get_max_option_no_unm <- function(data) {
                                data %>%
                                rowwise() %>%
                                mutate(
                                    max_value = max(c(t_base, t_NE, t_BD, t_BE), na.rm = TRUE),
                                    type = if(all(is.na(c(t_base, t_NE, t_BD, t_BE)))) {
                                    NA_character_
                                    } else {
                                    col_names <- c("Baseline", "ToNE", "ToBD", "ToBE")
                                    values <- c(t_base, t_NE, t_BD, t_BE)
                                    col_names[which.max(replace(values, is.na(values), -Inf))]
                                    }
                                ) %>%
                                ungroup()
}


turnover_eco_man_only_thresh_longest_no_unm <-get_max_option_no_unm(merged_turn_eco_man_only)
turnover_stem_man_only_thresh_longest_no_unm <-get_max_option_no_unm(merged_turn_stem_man_only)
turnover_tot_soil_man_only_thresh_longest_no_unm <-get_max_option_no_unm(merged_turn_tot_soil_man_only)

pal3 <- c("#5aca7e",
             "#F98400",
              "#5BBCD6",
              "#EBCC2A")
ordered_management2 <- c("Baseline", "ToNE", "ToBD", "ToBE")

create_plot_longest_tau_no_unm <- function(data, title, tag, column1) {
    ggplot(data = data) +
        geom_raster(aes(x = Lon, y = Lat, fill = factor(type, ordered_management2))) +
                                        coord_equal() + 
                                        scale_fill_manual(values = pal3) +
        labs(fill = "Management", tag = tag) +
        ggtitle(title) +
        theme_void() +
        theme(plot.title.position = "plot", # Title above the plot
              plot.title = element_text(hjust = 0.15, size = 12, margin = margin(t = 10, b = -20)),
                legend.position = "none",
                panel.background = element_rect(fill = "white", colour = "black")) +
                geom_sf(data = europe, fill = "transparent") +
                            geom_sf(data = ocean, fill = "light blue") +
                            coord_sf(xlim = c(-10.5, 32.5), ylim = c(34, 71.5),
                            expand = FALSE)
}


plot_longest_tau_eco_man_only_no_unm <- create_plot_longest_tau_no_unm(
            data = subset(turnover_eco_man_only_thresh_longest_no_unm, !is.na(type)),
            title = "ECOSYSTEM - EXCLUDING UNMANAGED",
            tag = "B", 
            column1 = "max_value")

plot_longest_tau_stem_man_only_no_unm <- create_plot_longest_tau_no_unm(
            data = subset(turnover_stem_man_only_thresh_longest_no_unm, !is.na(type)), 
            title = "STEM - EXCLUDING UNMANAGED",
            tag = "D", 
            column1 = "max_value")

plot_longest_tau_tot_soil_man_only_no_unm <- create_plot_longest_tau_no_unm(
            data = subset(turnover_tot_soil_man_only_thresh_longest_no_unm, !is.na(type)), 
            title = "SOIL - EXCLUDING UNMANAGED",
            tag = "F", 
            column1 = "max_value")

#put all the plots together
#create a plot to extract the legend
plot_legend_fig4 <- ggplot(data = turnover_stem_man_only_thresh_longest)+
                    geom_raster(aes(x = Lon, y = Lat, fill = factor(type, ordered_management))) +
                                        coord_equal() + 
                                        scale_fill_manual(values = pal2) +
                    labs(fill = "Management") +
                    theme(
                      legend.text = element_text(size = 20),
                      legend.title = element_text(size = 20),
                      panel.background = element_rect(fill = "white", colour = "black"))

# Extract the legend as a grob
legend_grob_fig4 <- get_legend(plot_legend_fig4)

#put the figures and the legend together
fig4  <-grid.arrange(arrangeGrob(plot_longest_tau_eco_man_only, plot_longest_tau_eco_man_only_no_unm, ncol = 3, widths = c(4, 4, 2)),
  arrangeGrob(plot_longest_tau_stem_man_only, plot_longest_tau_stem_man_only_no_unm, legend_grob_fig4, ncol = 3, widths = c(4, 4, 2)),
  arrangeGrob(plot_longest_tau_tot_soil_man_only, plot_longest_tau_tot_soil_man_only_no_unm, ncol = 3, widths = c(4, 4, 2)),
  nrow = 3
)

# ggsave(plot = fig4, path = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\internal_review",
#                  filename = "fig4.jpeg",
#                   dpi = 300, width = 30, height = 40, units = c("cm"), bg = "white")

#####################################################
#...................Fig5.............................
#####################################################
# calculate ranges in the differences without outliers (IQR 1-99)
process_diff_turnover <- function(data) {
    data %>%
        mutate(diff_unm = (t_unm - t_base),
               diff_ne = (t_NE - t_base), 
               diff_bd = (t_BD - t_base), 
               diff_be = (t_BE - t_base))
}

# Apply the function to ecosystem, stem, and soil data
diff_turn_eco_man_only_df <- process_diff_turnover(merged_turn_eco_man_only)
diff_turn_stem_man_only_df <- process_diff_turnover(merged_turn_stem_man_only)
diff_turn_tot_soil_man_only_df <- process_diff_turnover(merged_turn_tot_soil_man_only)


diff_turn_eco_man_cc_df <- process_diff_turnover(merged_turn_eco_man_cc)
diff_turn_stem_man_cc_df <- process_diff_turnover(merged_turn_stem_man_cc)
diff_turn_tot_soil_man_cc_df <- process_diff_turnover(merged_turn_tot_soil_man_cc)

# calculate means by climatic zone and create histograms with differences to baseline
climatic_zones <- read.table("koppen_geiger_tif\\koppen_climate", header = TRUE)
climatic_zones <- subset(climatic_zones, climate != "tundra")

process_turnover_data <- function(diff_turnover_df, climatic_zones, management_columns) {
    # Merge climatic zones with turnover data
    turnover_and_climate <- list(climatic_zones, diff_turnover_df[, c(1:7)]) %>%
        reduce(full_join, by = c("Lon", "Lat"))
    
    colnames(turnover_and_climate) <- c("Lon", "Lat", "value", "climate", management_columns)
    
    # Reshape the data to long format
    long_data <- turnover_and_climate %>%
        pivot_longer(cols = management_columns,  # Select all management option columns
                                 names_to = "management", 
                                 values_to = "turnover")  # Rename value column appropriately
    
 # Calculate mean of the IQR (0.01 - 0.99) for each climatic zone and management option
 mean_iqr_values <- long_data %>%
     group_by(climate, management) %>%
     summarise(lower_bound = quantile(turnover, 0.01, na.rm = TRUE),
                         upper_bound = quantile(turnover, 0.99, na.rm = TRUE),
                         mean_iqr_value = mean(turnover[turnover >= lower_bound & turnover <= upper_bound], na.rm = TRUE),
                         n_iqr = sum(!is.na(turnover[turnover >= lower_bound & turnover <= upper_bound])),  # Count valid observations
                         std_error_iqr_value = sd(turnover[turnover >= lower_bound & turnover <= upper_bound], na.rm = TRUE) / sqrt(n_iqr),  # Standard error calculation
                         .groups = 'drop')
    
    # Reshape the mean IQR values to long format
    mean_iqr_values_long <- reshape2::melt(mean_iqr_values, measure.vars = c("mean_iqr_value"))
    mean_iqr_values_long <- mean_iqr_values_long %>%
        filter(!is.na(climate))
    
    return(mean_iqr_values_long)
}

# Apply the function to ecosystem, stem, and soil data
management_columns <- c("Baseline", "Unmanaged", "ToNE", "ToBD", "ToBE")
mean_iqr_values_eco_long_man_only <- process_turnover_data(diff_turn_eco_man_only_df, climatic_zones, management_columns)
mean_iqr_values_stem_long_man_only <- process_turnover_data(diff_turn_stem_man_only_df, climatic_zones, management_columns)
mean_iqr_values_tot_soil_long_man_only <- process_turnover_data(diff_turn_tot_soil_man_only_df, climatic_zones, management_columns)

mean_iqr_values_eco_long_man_cc <- process_turnover_data(diff_turn_eco_man_cc_df, climatic_zones, management_columns)
mean_iqr_values_stem_long_man_cc <- process_turnover_data(diff_turn_stem_man_cc_df, climatic_zones, management_columns)
mean_iqr_values_tot_soil_long_man_cc <- process_turnover_data(diff_turn_tot_soil_man_cc_df, climatic_zones, management_columns)

###########################################################################
# Define a generic function to create histograms for eco, stem, and soil
create_histogram <- function(data, title, tag, color_palette, ordered_management) {
    ggplot(data, 
                 aes(x = factor(management, levels = ordered_management), 
                         y = value,  
                         fill = factor(management, levels = ordered_management))) +  
        facet_grid(. ~ climate, labeller = label_wrap_gen(width = 15)) +
        geom_bar(stat = "identity", position = position_dodge(0.9)) +  
        geom_errorbar(aes(ymin = value - std_error_iqr_value, 
                                            ymax = value + std_error_iqr_value), 
                                    width = 0.2, 
                                    position = position_dodge(0.9),  
                                    color = "black") +  
        scale_fill_manual(values = color_palette) +
        labs(fill = "Management", tag = tag) +
        ggtitle(title) +
        theme(axis.title.x = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank(),
                    axis.title.y = element_blank(),
                    axis.text = element_text(size = 20),
                    strip.text.x = element_text(size = 20),
                    plot.title = element_text(size = 20),
                    plot.tag = element_text(size = 30, face = "bold"),
                    legend.position = "none")
}

ordered_management_tot <- c("Baseline", "Unmanaged", "ToNE", "ToBD", "ToBE")
palhist_tot <- c("#5aca7e",
               "#9164dc",
                "#F98400",
                 "#5BBCD6",
                 "#EBCC2A")
# Create histograms for eco, stem, and soil using the generic function
hist_eco_mean_man_only_tot <- create_histogram(mean_iqr_values_eco_long_man_only, 
                                            title = "ECOSYSTEM", 
                                            tag = "A", 
                                            color_palette = palhist_tot, 
                                            ordered_management = ordered_management_tot)

hist_stem_mean_man_only_tot <- create_histogram(mean_iqr_values_stem_long_man_only, 
                                            title = "STEM", 
                                            tag = "   ", 
                                            color_palette = palhist_tot, 
                                            ordered_management = ordered_management_tot)

hist_tot_soil_mean_man_only_tot <- create_histogram(mean_iqr_values_tot_soil_long_man_only, 
                                            title = "SOIL", 
                                            tag = "  ", 
                                            color_palette = palhist_tot, 
                                            ordered_management = ordered_management_tot)
hist_mean_man_only_tot <-grid_arrange_shared_legend(hist_eco_mean_man_only_tot, hist_stem_mean_man_only_tot, hist_tot_soil_mean_man_only_tot,
                                	nrow = 3, ncol = 1, position = "right")


hist_eco_mean_man_cc_tot <- create_histogram(mean_iqr_values_eco_long_man_cc, 
                                                title = "ECOSYSTEM", 
                                                tag = "B", 
                                                color_palette = palhist_tot, 
                                                ordered_management = ordered_management_tot)

hist_stem_mean_man_cc_tot <- create_histogram(mean_iqr_values_stem_long_man_cc, 
                                                title = "STEM", 
                                                tag = "   ", 
                                                color_palette = palhist_tot, 
                                                ordered_management = ordered_management_tot)

hist_tot_soil_mean_man_cc_tot <- create_histogram(mean_iqr_values_tot_soil_long_man_cc, 
                                                title = "SOIL", 
                                                tag = "  ", 
                                                color_palette = palhist_tot, 
                                                ordered_management = ordered_management_tot)
hist_mean_man_cc_tot <-grid_arrange_shared_legend(hist_eco_mean_man_cc_tot, hist_stem_mean_man_cc_tot, hist_tot_soil_mean_man_cc_tot,
                                	nrow = 3, ncol = 1, position = "right")


combine_data <- function(data_man_cc, data_man_only) {
    full_join(
        data_man_cc,
        data_man_only,
        by = c("climate", "management", "variable"),
        suffix = c("_man_cc", "_equil")
    )
}

combined_data_eco_clim <- combine_data(mean_iqr_values_eco_long_man_cc, mean_iqr_values_eco_long_man_only)
combined_data_stem_clim <- combine_data(mean_iqr_values_stem_long_man_cc, mean_iqr_values_stem_long_man_only)
combined_data_tot_soil_clim <- combine_data(mean_iqr_values_tot_soil_long_man_cc, mean_iqr_values_tot_soil_long_man_only)

# Calculate differences
process_difference_data <- function(combined_data) {
    combined_data %>%
        dplyr::mutate(value_diff = value_equil - value_man_cc,
               std_error_diff = sqrt(std_error_iqr_value_man_cc^2 + std_error_iqr_value_equil^2)) %>%
        dplyr::select(climate, management, value = value_diff, std_error_iqr_value = std_error_diff)
}


difference_data_eco_clim <- process_difference_data(combined_data_eco_clim)
difference_data_stem_clim <- process_difference_data(combined_data_stem_clim)
difference_data_tot_soil_clim <- process_difference_data(combined_data_tot_soil_clim)

pal_hist_diff_tot <- c("#0081a7", "#00afb9", "#fdfcdc", "#fed9b7", "#f07167")

hist_eco_diff_clim <- create_histogram(difference_data_eco_clim, 
                                        title = "ECOSYSTEM", 
                                        tag = "C", 
                                        color_palette = pal_hist_diff_tot, 
                                        ordered_management = ordered_management_tot)

hist_stem_diff_clim <- create_histogram(difference_data_stem_clim, 
                                        title = "STEM", 
                                        tag = "  ", 
                                        color_palette = pal_hist_diff_tot, 
                                        ordered_management = ordered_management_tot)

hist_tot_soil_diff_clim <- create_histogram(difference_data_tot_soil_clim, 
                                        title = "SOIL", 
                                        tag = "  ", 
                                        color_palette = pal_hist_diff_tot, 
                                        ordered_management = ordered_management_tot)

hist_mean_clim_diff_tot <-grid_arrange_shared_legend(hist_eco_diff_clim, hist_stem_diff_clim, hist_tot_soil_diff_clim,
                                	nrow = 3, ncol = 1, position = "right")

# Create a common legend 
plot_legend <- ggplot(mean_iqr_values_eco_long_man_cc, aes(x = factor(management, levels = ordered_management_tot), 
                                                 y = value,  
                                                 fill = factor(management, levels = ordered_management_tot))) +  
  geom_bar(stat = "identity", position = position_dodge(0.9)) +  
  scale_fill_manual(values = palhist_tot) +
  labs(fill = "Management") +
  theme(legend.text = element_text(size = 25),
        legend.title = element_text(size = 25),
        legend.key.size = unit(1.5, "lines"))

plot_legend2 <- ggplot(difference_data_tot_soil_clim, aes(x = factor(management, levels = ordered_management_tot), 
                                                 y = value,  
                                                 fill = factor(management, levels = ordered_management_tot))) +  
  geom_bar(stat = "identity", position = position_dodge(0.9)) +  
  scale_fill_manual(values = pal_hist_diff_tot) +
  labs(fill = "Management") +
  theme(legend.text = element_text(size = 25),
        legend.title = element_text(size = 25),
        legend.key.size = unit(1.5, "lines"))

# Extract the legend as a grob
legend_grob <- get_legend(plot_legend)
legend_grob2 <- get_legend(plot_legend2)

figure5  <-grid.arrange(arrangeGrob(hist_eco_mean_man_only_tot, ncol = 2, widths = c(6, 1.1)),
  arrangeGrob(hist_stem_mean_man_only_tot, ncol = 2, widths = c(6, 1.1)),
  arrangeGrob(hist_tot_soil_mean_man_only_tot, legend_grob, ncol = 2, widths = c(6, 1.1)),
  arrangeGrob(hist_eco_mean_man_cc_tot, ncol = 2, widths = c(6, 1.1)),
  arrangeGrob(hist_stem_mean_man_cc_tot, ncol = 2, widths = c(6, 1.1)),
  arrangeGrob(hist_tot_soil_mean_man_cc_tot, ncol = 2, widths = c(6, 1.1)),
  arrangeGrob(hist_eco_diff_clim, ncol = 2, widths = c(6, 1.1)),
  arrangeGrob(hist_stem_diff_clim, legend_grob2, ncol = 2, widths = c(6, 1.1)),
  arrangeGrob(hist_tot_soil_diff_clim, ncol = 2, widths = c(6, 1.1)),
  nrow = 9
)

#ggsave(plot=figure5, path ="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\Tom", 
 #      filename="fig5.jpeg",  dpi=300, width = 40, height =60 , units = c("cm"),bg = "white" )

#####################################################
#...................Fig6.............................
#####################################################
#upload forest types fractions
forest_types <- read.table("ST_1870_2010_changes_every10years.txt", header = TRUE)
forest_types2010 <- forest_types %>% filter(year == 2010)
dominant_forest_type <- forest_types2010 %>%
  mutate(
    dominant_type = case_when(
      ForestNE >= ForestBD & ForestNE >= ForestND & ForestNE >= ForestBE ~ "ForestNE",
      ForestBD >= ForestNE & ForestBD >= ForestND & ForestBD >= ForestBE ~ "ForestBD",
      ForestND >= ForestNE & ForestND >= ForestBD & ForestND >= ForestBE ~ "ForestND",
      ForestBE >= ForestNE & ForestBE >= ForestBD & ForestBE >= ForestND ~ "ForestBE",
      TRUE ~ NA_character_
    ),
    PFT = case_when(
      dominant_type %in% c("ForestBD", "ForestBE") ~ "Broadleaves",
      dominant_type %in% c("ForestNE", "ForestND") ~ "Needleleaves",
      TRUE ~ NA_character_
    )
  )
dominant_forest_type <- dominant_forest_type %>% dplyr::select(Lon, Lat, PFT)

process_turnover_data_forest_type <- function(diff_turnover_df, dominant_forest_type, management_columns) {
    # Merge climatic zones with turnover data
    turnover_eco_and_forest_type <- list(dominant_forest_type, diff_turnover_df[, c(1:7)]) %>%
        reduce(full_join, by = c("Lon", "Lat"))
    
    colnames(turnover_eco_and_forest_type) <- c("Lon", "Lat", "Dominant_type", management_columns)
    
    # Reshape the data to long format
    long_data <- turnover_eco_and_forest_type %>%
        pivot_longer(cols = management_columns,  # Select all management option columns
                                 names_to = "management", 
                                 values_to = "turnover")  # Rename value column appropriately
    
 # Calculate mean of the IQR (0.01 - 0.99) for each climatic zone and management option
 mean_iqr_values <- long_data %>%
  filter(management != "ToBE") %>%
     group_by(Dominant_type, management) %>%
     summarise(lower_bound = quantile(turnover, 0.01, na.rm = TRUE),
                         upper_bound = quantile(turnover, 0.99, na.rm = TRUE),
                         mean_iqr_value = mean(turnover[turnover >= lower_bound & turnover <= upper_bound], na.rm = TRUE),
                         n_iqr = sum(!is.na(turnover[turnover >= lower_bound & turnover <= upper_bound])),  # Count valid observations
                         std_error_iqr_value = sd(turnover[turnover >= lower_bound & turnover <= upper_bound], na.rm = TRUE) / sqrt(n_iqr),  # Standard error calculation
                         .groups = 'drop')
    
    mean_iqr_values_eco_vs_base_forest_type <- mean_iqr_values %>%
        group_by(Dominant_type) %>%
        mutate(
        baseline = mean_iqr_value[management == "Baseline"],
        difference = if_else(management == "Baseline", 0, mean_iqr_value - baseline)
        ) %>%
        ungroup()

        mean_iqr_values_eco_vs_base_forest_type <- mean_iqr_values_eco_vs_base_forest_type %>%
                filter(management != "Baseline")
    # Reshape the mean IQR values to long format
        mean_iqr_values_eco_vs_base_long_forest_type <- reshape2::melt(mean_iqr_values_eco_vs_base_forest_type, measure.vars = c("difference"))
        mean_iqr_values_eco_vs_base_long_forest_type <- mean_iqr_values_eco_vs_base_long_forest_type %>%
                                    filter(!is.na(Dominant_type))
    return(mean_iqr_values_eco_vs_base_long_forest_type)
}


mean_iqr_values_eco_forest_type_long_man_only <- process_turnover_data_forest_type(diff_turn_eco_man_only_df, dominant_forest_type, management_columns)
mean_iqr_values_stem_forest_type_long_man_only <- process_turnover_data_forest_type(diff_turn_stem_man_only_df, dominant_forest_type, management_columns)
mean_iqr_values_tot_soil_forest_type_long_man_only <- process_turnover_data_forest_type(diff_turn_tot_soil_man_only_df, dominant_forest_type, management_columns)

mean_iqr_values_eco_forest_type_long_man_cc <- process_turnover_data_forest_type(diff_turn_eco_man_cc_df, dominant_forest_type, management_columns)
mean_iqr_values_stem_forest_type_long_man_cc <- process_turnover_data_forest_type(diff_turn_stem_man_cc_df, dominant_forest_type, management_columns)
mean_iqr_values_tot_soil_forest_type_long_man_cc <- process_turnover_data_forest_type(diff_turn_tot_soil_man_cc_df, dominant_forest_type, management_columns)

###########################################################################
# Define a generic function to create histograms for eco, stem, and tot_soil
create_histogram_forest_type <- function(data, title, tag, color_palette, ordered_management) {
    ggplot(data, 
                 aes(x = factor(management, levels = ordered_management), 
                         y = value,  
                         fill = factor(management, levels = ordered_management))) +  
        facet_grid(. ~ Dominant_type, labeller = label_wrap_gen(width = 15)) +
        geom_bar(stat = "identity", position = position_dodge(0.9)) +  
        geom_errorbar(aes(ymin = value - std_error_iqr_value, 
                                            ymax = value + std_error_iqr_value), 
                                    width = 0.2, 
                                    position = position_dodge(0.9),  
                                    color = "black") +  
        scale_fill_manual(values = color_palette) +
        labs(fill = "Management", tag = tag) +
        ggtitle(title) +
        theme(axis.title.x = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank(),
                    axis.title.y = element_blank(),
                    axis.text = element_text(size = 20),
                    strip.text.x = element_text(size = 20),
                    plot.title = element_text(size = 20),
                    plot.tag = element_text(size = 30, face = "bold"),
                    legend.position = "none")
}

ordered_management <- c("Baseline", "Unmanaged", "ToNE", "ToBD", "ToBE")
palhist_diff <- c("#9164dc",
                "#F98400",
                 "#5BBCD6",
                 "#EBCC2A")
# Create histograms for eco, stem, and tot_soil using the generic function
hist_eco_mean_forest_type_man_only <- create_histogram_forest_type(mean_iqr_values_eco_forest_type_long_man_only, 
                                            title = "ECOSYSTEM", 
                                            tag = "A", 
                                            color_palette = palhist_diff, 
                                            ordered_management = ordered_management)

hist_stem_mean_forest_type_man_only <- create_histogram_forest_type(mean_iqr_values_stem_forest_type_long_man_only, 
                                            title = "STEM", 
                                            tag = "   ", 
                                            color_palette = palhist_diff, 
                                            ordered_management = ordered_management)

hist_tot_soil_mean_forest_type_man_only <- create_histogram_forest_type(mean_iqr_values_tot_soil_forest_type_long_man_only, 
                                            title = "SOIL", 
                                            tag = "  ", 
                                            color_palette = palhist_diff, 
                                            ordered_management = ordered_management)
hist_mean_forest_type_man_only <-grid_arrange_shared_legend(hist_eco_mean_forest_type_man_only, hist_stem_mean_forest_type_man_only, hist_tot_soil_mean_forest_type_man_only,
                                	nrow = 3, ncol = 1, position = "right")


hist_eco_mean_man_cc_forest_type <- create_histogram_forest_type(mean_iqr_values_eco_forest_type_long_man_cc, 
                                                title = "ECOSYSTEM", 
                                                tag = "B", 
                                                color_palette = palhist_diff, 
                                                ordered_management = ordered_management)


hist_stem_mean_man_cc_forest_type <- create_histogram_forest_type(mean_iqr_values_stem_forest_type_long_man_cc, 
                                                title = "STEM", 
                                                tag = "   ", 
                                                color_palette = palhist_diff, 
                                                ordered_management = ordered_management)

hist_tot_soil_mean_man_cc_forest_type <- create_histogram_forest_type(mean_iqr_values_tot_soil_forest_type_long_man_cc, 
                                                title = "SOIL", 
                                                tag = "  ", 
                                                color_palette = palhist_diff, 
                                                ordered_management = ordered_management)
hist_mean_forest_type_man_cc <-grid_arrange_shared_legend(hist_eco_mean_man_cc_forest_type, hist_stem_mean_man_cc_forest_type, hist_tot_soil_mean_man_cc_forest_type,
                                	nrow = 3, ncol = 1, position = "right")


combine_data_forest_type <- function(data_man_cc, data_man_only) {
    full_join(
        data_man_cc,
        data_man_only,
        by = c("Dominant_type", "management", "variable"),
        suffix = c("_man_cc", "_equil")
    )
}

combined_data_eco_forest_type <- combine_data_forest_type(mean_iqr_values_eco_forest_type_long_man_cc, mean_iqr_values_eco_forest_type_long_man_only)
combined_data_stem_forest_type <- combine_data_forest_type(mean_iqr_values_stem_forest_type_long_man_cc, mean_iqr_values_stem_forest_type_long_man_only)
combined_data_tot_soil_forest_type <- combine_data_forest_type(mean_iqr_values_tot_soil_forest_type_long_man_cc, mean_iqr_values_tot_soil_forest_type_long_man_only)

# Calculate differences
process_difference_data_forest_type <- function(combined_data) {
    combined_data %>%
        mutate(value_diff = value_equil - value_man_cc,
               std_error_diff = sqrt(std_error_iqr_value_man_cc^2 + std_error_iqr_value_equil^2)) %>%
        select(Dominant_type, management, value = value_diff, std_error_iqr_value = std_error_diff)
}

difference_data_eco_forest_type <- process_difference_data_forest_type(combined_data_eco_forest_type)
difference_data_stem_forest_type <- process_difference_data_forest_type(combined_data_stem_forest_type)
difference_data_tot_soil_forest_type <- process_difference_data_forest_type(combined_data_tot_soil_forest_type)

pal_hist_diff_forest_type <- c("#0081a7", "#00afb9", "#fed9b7", "#f07167")

hist_eco_diff_forest_type <- create_histogram_forest_type(difference_data_eco_forest_type, 
                                        title = "ECOSYSTEM", 
                                        tag = "C", 
                                        color_palette = pal_hist_diff_forest_type, 
                                        ordered_management = ordered_management)

hist_stem_diff_forest_type <- create_histogram_forest_type(difference_data_stem_forest_type, 
                                        title = "STEM", 
                                        tag = "  ", 
                                        color_palette = pal_hist_diff_forest_type, 
                                        ordered_management = ordered_management)

hist_tot_soil_diff_forest_type <- create_histogram_forest_type(difference_data_tot_soil_forest_type, 
                                        title = "SOIL", 
                                        tag = "  ", 
                                        color_palette = pal_hist_diff_forest_type, 
                                        ordered_management = ordered_management)

hist_mean_forest_type_diff <-grid_arrange_shared_legend(hist_eco_diff_forest_type, hist_stem_diff_forest_type, hist_tot_soil_diff_forest_type,
                                	nrow = 3, ncol = 1, position = "right")


# Create a common legend 
plot_legend_forest_type <- ggplot(mean_iqr_values_eco_forest_type_long_man_cc, aes(x = factor(management, levels = ordered_management), 
                                                 y = value,  
                                                 fill = factor(management, levels = ordered_management))) +  
  geom_bar(stat = "identity", position = position_dodge(0.9)) +  
  scale_fill_manual(values = palhist_diff) +
  labs(fill = "Management") +
  theme(legend.text = element_text(size = 25),
        legend.title = element_text(size = 25),
        legend.key.size = unit(1.5, "lines"))

plot_legend_forest_type2 <- ggplot(combined_data_eco_forest_type, aes(x = factor(management, levels = ordered_management), 
                                                 y = value_equil,  
                                                 fill = factor(management, levels = ordered_management))) +  
  geom_bar(stat = "identity", position = position_dodge(0.9)) +  
  scale_fill_manual(values = pal_hist_diff_forest_type) +
  labs(fill = "Management") +
  theme(legend.text = element_text(size = 25),
        legend.title = element_text(size = 25),
        legend.key.size = unit(1.5, "lines"))

# Extract the legend as a grob
legend_grob_forest_type <- get_legend(plot_legend_forest_type)
legend_grob_forest_type2 <- get_legend(plot_legend_forest_type2)

figure6  <-grid.arrange(arrangeGrob(hist_eco_mean_forest_type_man_only, ncol = 2, widths = c(6, 1.1)),
  arrangeGrob(hist_stem_mean_forest_type_man_only, ncol = 2, widths = c(6, 1.1)),
  arrangeGrob(hist_tot_soil_mean_forest_type_man_only, legend_grob_forest_type, ncol = 2, widths = c(6, 1.1)),
  arrangeGrob(hist_eco_mean_man_cc_forest_type, ncol = 2, widths = c(6, 1.1)),
  arrangeGrob(hist_stem_mean_man_cc_forest_type, ncol = 2, widths = c(6, 1.1)),
  arrangeGrob(hist_tot_soil_mean_man_cc_forest_type, ncol = 2, widths = c(6, 1.1)),
  arrangeGrob(hist_eco_diff_forest_type, ncol = 2, widths = c(6, 1.1)),
  arrangeGrob(hist_stem_diff_forest_type, legend_grob_forest_type2, ncol = 2, widths = c(6, 1.1)),
  arrangeGrob(hist_tot_soil_diff_forest_type, ncol = 2, widths = c(6, 1.1)),
  nrow = 9
)

#ggsave(plot=figure6, path ="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\internal_review", 
 #       filename="fig6.jpeg",  dpi=300, width = 40, height =60 , units = c("cm"),bg = "white" )
##############################################
########## SUPPLEMENTARY MATERIAL ############
#####################################################
#...................FigS1.............................
#####################################################
#Koppen climatic zones
#data from Beck et al. 2022, downloaded from https://www.gloh2o.org/koppen/
koppen <- raster("koppen_geiger_tif\\1991_2020\\koppen_geiger_0p5.tif")
#set the right extension and crop the data
koppen_EU <- crop(koppen, e)
#transform it into a dataframe
test_spdf <- as(koppen_EU, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "Lon", "Lat")

#merge the dataframe with one of my plots, to have the right grid cells
#I used turnover_eco_man_only_base but I could have used any of my plots
turnover_eco_man_only_base <- readRDS("man_only\\base\\turnover_eco_man_only_base.RData")
turnover_eco_man_only_base_df <- as.data.frame(turnover_eco_man_only_base)
colnames(turnover_eco_man_only_base_df) <- c("Lon", "Lat", "turnover")
koppen_crop <- merge(turnover_eco_man_only_base_df, test_df, by= c("Lon", "Lat"))
koppen_crop$value <- as.factor(koppen_crop$value)
koppen_crop <- koppen_crop[,c(1,2,4)]

# I aggregate some of the climatic zones because they are too many for the analysis
koppen_EU_aggregated <- mutate(koppen_crop, climate=
                        ifelse(grepl(27,value),"cold with cold summer",
                        ifelse(grepl(29,value),"tundra",
                        ifelse(grepl(26,value),"cold with warm summer",
                        ifelse(grepl(25,value),"cold with warm summer",
                        ifelse(grepl(18,value),"cold with warm summer",
                        ifelse(grepl(15,value),"temperate - no dry season",
                        ifelse(grepl(14,value), "temperate - no dry season",
                        ifelse(grepl(4, value), "arid",
                        ifelse(grepl(6,value), "arid",
                        ifelse(grepl(7,value), "arid", 
                        ifelse(grepl(8,value),"temperate dry summer",
                        "temperate dry summer"))))))))))))

koppen_crop$value <- as.factor(koppen_crop$value)
pal_arid <- c("#ff0000", "#f5a700", "#fedc63")
pal_cold_cold <- c("#007e7e")
pal_cold_warm <- c("#c900c9", "#00fefe", "#34c9ff")
pal_temp_dry <- c("#ffff00", "#c9c900")
pal_temp_no_dry <- c("#c9ff4f", "#64ff4f")
pal_tundra <- c("#b3b4b3")

arid_koppen <- koppen_crop %>%
              filter(value %in% c("4", "6", "7"))
cold_cold_koppen <- koppen_crop %>%
              filter(value %in% "27")
cold_warm_koppen <- koppen_crop %>%
              filter(value %in% c("18", "25", "26"))
temp_no_dry_koppen <- koppen_crop %>%
              filter(value %in% c("14", "15"))
temp_dry_koppen <- koppen_crop %>%
              filter(value %in% c("8", "9"))
tundra_koppen <- koppen_crop %>%
              filter(value %in% c("29"))

plot_koppen <- function(data, title, label, color_palette) {
 ggplot() + geom_tile(data=data, aes(x=Lon, y=Lat, fill= value), alpha=0.8) + 
  coord_equal() +
  scale_fill_manual(values = color_palette, labels = label) +
  labs(fill = "") +
  ggtitle(title) +
  theme_void()+
  theme(plot.title = element_text(size = 15)) +
  theme(strip.text.x = element_text(size = 12)) +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 15))+
  theme(plot.tag.position = c(0.04, 0.978)) +
  theme(plot.title = element_text(
         hjust = 0.15, size = 10, margin = margin(t = 5, b = -20))) +
           theme(plot.margin = margin(0, 0, 0, 0)) +
                            geom_sf(data = europe, fill = "transparent") +
                            geom_sf(data = ocean, fill = "light blue") +
                            coord_sf(xlim = c(-10.5, 32.5), ylim = c(34, 71.5),
                            expand = FALSE)
}
plot_arid <- plot_koppen(arid_koppen,
                title = "ARID",
                label = c("BWh", "BSh", "BSK"),
                color_palette = pal_arid)

plot_cold_cold <- plot_koppen(cold_cold_koppen,
                      title = "COLD WITH COLD SUMMER",
                      label = "Dfc",
                      color_palette = pal_cold_cold)

plot_cold_warm <- plot_koppen(cold_warm_koppen,
                                 title = "COLD WITH WARM SUMMER",
                                 label = c("Dsb", "Dfa", "Dfb"),
                                 color_palette = pal_cold_warm) 

plot_temp_no_dry <- plot_koppen(temp_no_dry_koppen, 
                                      title = "TEMPERATE - NO DRY SEASON",
                                      label = c("Cfa", "Cfb"),
                                      color_palette = pal_cold_warm) 

plot_temp_dry <- plot_koppen(temp_dry_koppen, 
                                  title = "TEMPERATE WITH DRY SUMMER",
                                  label = c("Csa", "Csb"),
                                  color_palette = pal_temp_dry)

plot_tundra <- plot_koppen(tundra_koppen, 
                                  title = "TUNDRA",
                                  label = c("ET"), 
                                  color_palette = pal_tundra) 

plot_koppen_combined <-ggarrange(plot_arid, plot_cold_cold, plot_cold_warm,
                                    plot_temp_no_dry, plot_temp_dry, plot_tundra, labels = "A", nrow=2, ncol=3, align = "v")


#########################
#for 2071-2099 man_cc
#########################
#data from Beck et al. 2022, downloaded from https://www.gloh2o.org/koppen/
koppen_fut <- raster("koppen_geiger_tif\\2071_2099\\ssp370\\koppen_geiger_0p5.tif")
koppen_fut_EU <- crop(koppen_fut, e)
#transform it into a dataframe
test_spdf_fut <- as(koppen_fut_EU, "SpatialPixelsDataFrame")
test_fut_df <- as.data.frame(test_spdf_fut)
colnames(test_fut_df) <- c("value", "Lon", "Lat")

#merge the dataframe with one of my plots, to have the right grid cells
koppen_crop_fut <- merge(turnover_eco_man_only_base_df, test_fut_df, by= c("Lon", "Lat"))
koppen_crop_fut$value <- as.factor(koppen_crop_fut$value)
koppen_crop_fut <- koppen_crop_fut[,c(1,2,4)]

# I aggregate some of the climatic zones because they are too many for the analysis
koppen_EU_fut_aggregated <- mutate(koppen_crop_fut, climate=
                        ifelse(grepl(27,value),"cold with cold summer",
                        ifelse(grepl(26,value),"cold with warm summer",
                        ifelse(grepl(25,value),"cold with warm summer",
                        ifelse(grepl(15,value),"temperate - no dry season",
                        ifelse(grepl(14,value), "temperate - no dry season",
                        ifelse(grepl(5,value),"arid",
                        ifelse(grepl(4, value), "arid",
                        ifelse(grepl(6,value), "arid",
                        ifelse(grepl(7,value), "arid", 
                        ifelse(grepl(8,value),"temperate dry summer",
                        "temperate dry summer")))))))))))

koppen_crop_fut$value <- as.factor(koppen_crop_fut$value)
pal_arid_fut <- c("#ff0000", "#f5a700", "#fedc63")
pal_cold_cold_fut <- c("#007e7e")
pal_cold_warm_fut <- c("#00fefe", "#34c9ff")
pal_temp_dry_fut <- c("#ffff00", "#c9c900")
pal_temp_no_dry_fut <- c("#c9ff4f", "#64ff4f")

arid_koppen_fut <- koppen_crop_fut %>%
              filter(value %in% c("4", "6", "7"))
cold_cold_koppen_fut <- koppen_crop_fut %>%
              filter(value %in% "27")
cold_warm_koppen_fut <- koppen_crop_fut %>%
              filter(value %in% c("25", "26"))
temp_no_dry_koppen_fut <- koppen_crop_fut %>%
              filter(value %in% c("14", "15"))
temp_dry_koppen_fut <- koppen_crop_fut %>%
              filter(value %in% c("8", "9"))

plot_arid_fut <- plot_koppen(arid_koppen_fut,
                                title = "ARID",
                                label = c("BWh", "BSh", "BSK"),
                                color_palette = pal_arid_fut)

plot_cold_cold_fut <- plot_koppen(cold_cold_koppen_fut, 
                                    title = "COLD WITH COLD SUMMER",
                                    label = c("Dfc"),
                                    color_palette = pal_cold_cold_fut) 

plot_cold_warm_fut <- plot_koppen(cold_warm_koppen_fut, 
                                     title = "COLD WITH WARM SUMMER",
                                     label = c("Dfa", "Dfb"),
                                     color_palette = pal_cold_warm_fut) 
plot_temp_no_dry_fut <- plot_koppen(temp_no_dry_koppen_fut, 
                                    title = "TEMPERATE - NO DRY SEASON",
                                    label = c("Cfa", "Cfb"),
                                    color_palette = pal_temp_no_dry_fut)

plot_temp_dry_fut <- plot_koppen(temp_dry_koppen_fut, 
                                 title = "TEMPERATE WITH DRY SUMMER",
                                 label = c("Csa", "Csb"),
                                 color_palette = pal_temp_dry_fut)
plot_koppen_combined_fut <-ggarrange(plot_arid_fut, plot_cold_cold_fut, plot_cold_warm_fut,
                                    plot_temp_no_dry_fut, plot_temp_dry_fut, labels = "B", nrow=2, ncol=3, align = "v")

#####################################################
#...................FigS2............................
#####################################################
#upload table with the turnover data with GPP and outfluxes for the management-only simulations
merged_type_turn_eco_man_only <- read.table("man_only\\types_of_turn_avg_man_only", header = TRUE)

#create relative plots
#relative differences in the calculation of C turnover (with GPP method vs fluxes method)
create_relative_raster_plot <- function(data, Lon, Lat, value1_col, value2_col, 
                                        plot_tag, plot_title) {
  # Calculate relative difference
  data$rel_diff <- (data[[value2_col]] - data[[value1_col]]) / 
                   pmax(abs(data[[value1_col]]), abs(data[[value2_col]]))
  
  # Generate the plot
  plot <- ggplot(data = data) +
    geom_raster(aes(x = Lon, y = Lat, fill = rel_diff)) +
    scale_fill_gradientn(colors = c("#1e6091", "#1a759f", "#168aad", "#34a0a4", "#52b69a", "#76c893","#99d98c", "#b5e48c", "#d9ed92"),
                         na.value = "#FFFFFF", limits = c(-0.5, 0), 
                          breaks = seq(from = -0.5, to = 0, by = 0.10),
                          labels = scales::percent(seq(from = -0.5, to = 0, by = 0.10))) +
    labs(tag = plot_tag) +
    guides(fill = guide_colorbar(title = "Relative
Difference")) +
    ggtitle(plot_title) +
    theme_void() +
    theme(plot.tag.position = c(0.03, 0.97),
          plot.title = element_text(hjust = 0.15, size = 12, margin = margin(t = 10, b = -20)),
          panel.background = element_rect(fill = "white", colour = "black")) +
    geom_sf(data = europe, fill = "transparent") +
    geom_sf(data = ocean, fill = "light blue") +
    coord_sf(xlim = c(-10.5, 32.5), ylim = c(34, 71.5), expand = FALSE)
  
  return(plot)
}


relative_plot_turn_base_diff_fluxes_gpp_man_only <- create_relative_raster_plot(data = merged_type_turn_eco_man_only,
  value1_col = "turn_bau_of", value2_col = "turn_bau_gpp",
  plot_title = "Baseline", plot_tag = "A"
)
relative_plot_turn_unm_diff_fluxes_gpp_man_only <- create_relative_raster_plot(data = merged_type_turn_eco_man_only,
  value1_col = "turn_unm_of", value2_col = "turn_unm_gpp",
  plot_title = "Unmanaged", plot_tag = "B"
)
relative_plot_turn_tone_diff_fluxes_gpp_man_only <- create_relative_raster_plot(data = merged_type_turn_eco_man_only,
  value1_col = "turn_tone_of", value2_col = "turn_tone_gpp",
  plot_title = "to NE", plot_tag = "C"
)
relative_plot_turn_tobd_diff_fluxes_gpp_man_only <- create_relative_raster_plot(data = merged_type_turn_eco_man_only,
  value1_col = "turn_tobd_of", value2_col = "turn_tobd_gpp",
  plot_title = "to BD", plot_tag = "D"
)
relative_plot_turn_tobe_diff_fluxes_gpp_man_only <- create_relative_raster_plot(data = merged_type_turn_eco_man_only,
  value1_col = "turn_tobe_of", value2_col = "turn_tobe_gpp",
  plot_title = "to BE", plot_tag = "E"
)

figS2 <-grid_arrange_shared_legend(relative_plot_turn_base_diff_fluxes_gpp_man_only,
                                relative_plot_turn_unm_diff_fluxes_gpp_man_only, relative_plot_turn_tone_diff_fluxes_gpp_man_only,
                                relative_plot_turn_tobd_diff_fluxes_gpp_man_only, relative_plot_turn_tobe_diff_fluxes_gpp_man_only,
                                nrow = 1, ncol = 5, position = "right")

#  ggsave(plot = figS2, path = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\internal_review",
#                   filename = "figS2.jpeg",
#                   dpi = 300, width = 40, height = 13, units = c("cm"), bg = "white")

#####################################################
#...................FigS3............................
#####################################################
###########################
#upload table with the turnover data with GPP and outfluxes for the management and climate change simulations
merged_turn_eco_man_cc <- read.table("man_cc\\types_of_turn_avg_man_cc", header = TRUE)

relative_plot_turn_base_diff_fluxes_gpp_man_cc <- create_relative_raster_plot(data = merged_turn_eco_man_cc,
  value1_col = "turn_bau_of", value2_col = "turn_bau_gpp",
  plot_title = "Baseline", plot_tag = "A"
)
relative_plot_turn_unm_diff_fluxes_gpp_man_cc <- create_relative_raster_plot(data = merged_turn_eco_man_cc,
  value1_col = "turn_unm_of", value2_col = "turn_unm_gpp",
  plot_title = "Unmanaged", plot_tag = "B"
)
relative_plot_turn_tone_diff_fluxes_gpp_man_cc <- create_relative_raster_plot(data = merged_turn_eco_man_cc,
  value1_col = "turn_tone_of", value2_col = "turn_tone_gpp",
  plot_title = "to NE", plot_tag = "C"
)
relative_plot_turn_tobd_diff_fluxes_gpp_man_cc <- create_relative_raster_plot(data = merged_turn_eco_man_cc,
  value1_col = "turn_tobd_of", value2_col = "turn_tobd_gpp",
  plot_title = "to BD", plot_tag = "D"
)
relative_plot_turn_tobe_diff_fluxes_gpp_man_cc <- create_relative_raster_plot(data = merged_turn_eco_man_cc,
  value1_col = "turn_tobe_of", value2_col = "turn_tobe_gpp",
  plot_title = "to BE", plot_tag = "E"
)


figS3 <-grid_arrange_shared_legend(relative_plot_turn_base_diff_fluxes_gpp_man_cc,
                                relative_plot_turn_unm_diff_fluxes_gpp_man_cc, relative_plot_turn_tone_diff_fluxes_gpp_man_cc,
                                relative_plot_turn_tobd_diff_fluxes_gpp_man_cc, relative_plot_turn_tobe_diff_fluxes_gpp_man_cc,
                                nrow = 1, ncol = 5, position = "right")

#gsave(plot = figS3, path = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\internal_review",
#                   filename = "figS3.jpeg",
#                   dpi = 300, width = 40, height = 13, units = c("cm"), bg = "white")
#####################################################
#...................FigS4............................
#####################################################
climatic_zones <- read.table("koppen_geiger_tif\\koppen_climate", header = TRUE)
climatic_zones <- subset(climatic_zones, climate != "tundra")

# create pairs of boxplots for each management in each climatic zone, where I compare
# turnover calculated with GPP and turnover calculated with outer fluxes

##############
# man_only
##############
turnover_eco_and_climate_man_only <- list(climatic_zones, merged_type_turn_eco_man_only[, c(1, 2, 6, 7, 11, 12, 16, 17, 21, 22,  26, 27)]) %>%
               reduce(full_join, by = c("Lon", "Lat"))
long_data_eco_man_only <- turnover_eco_and_climate_man_only %>%
  pivot_longer(
    cols = starts_with("turn_"),
    names_to = c("management", "calc"),
    names_pattern = "turn_(.*)_(.*)",
    values_to = "turnover"
  ) %>%
  mutate(
    management = case_when(
      management == "bau" ~ "Baseline",
      management == "unm" ~ "Unmanaged",
      management == "tone" ~ "ToNE",
      management == "tobd" ~ "ToBD",
      management == "tobe" ~ "ToBE"
    ),
    calc = case_when(
      calc == "of" ~ "Cpool/Outflows",
      calc == "gpp" ~ "Cpool/GPP"
    )
  )


# Prepare the data
long_data_eco_man_only_filtered <- long_data_eco_man_only %>%
  filter(!is.na(climate)) %>%
  mutate(management = factor(management, levels = c("Baseline", "Unmanaged", "ToNE", "ToBD", "ToBE")))
long_data_eco_man_only_filtered$calc <- factor(long_data_eco_man_only_filtered$calc, levels = c("Cpool/Outflows", "Cpool/GPP"))

# Create the plot
figS4 <- ggplot(long_data_eco_man_only_filtered, aes(x = management, y = turnover, fill = calc)) +
                                      geom_boxplot(position = position_dodge(width = 0.8)) +
                                      scale_fill_manual(values = c("Cpool/Outflows" = "#FF006E", "Cpool/GPP" = "#8338ec")) +
                                      labs(y = "Turnover", x = "Management", fill = "Turnover calculation") +
                                      theme_minimal() +
                                      facet_wrap(~ climate, scales = "free_y", labeller = as_labeller(c("arid" = "ARID", 
                                      "cold with cold summer" = "COLD WITH COLD SUMMER",
                                      "cold with warm summer" = "COLD WITH WARM SUMMER",
                                      "temperate - no dry season" = "TEMPERATE - NO DRY SEASON",
                                      "temperate dry summer" = "TEMPERATE DRY SUMMER")), nrow = 1) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 10, face = "bold"),
    strip.background = element_rect(fill = "white", color = "black")
  )

# ggsave(plot = figS4, path = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\internal_review",
#             filename = "figS4.jpeg",
#                dpi = 300, width = 40, height = 10, units = c("cm"), bg = "white")

#####################################################
#...................FigS5............................
#####################################################
turnover_eco_and_climate_man_cc <- list(climatic_zones, merged_turn_eco_man_cc[, c(1, 2, 6, 7, 11, 12, 16, 17, 21, 22,  26, 27)]) %>%
               reduce(full_join, by = c("Lon", "Lat"))

long_data_eco_man_cc <- turnover_eco_and_climate_man_cc %>%
  pivot_longer(
    cols = starts_with("turn_"),
    names_to = c("management", "calc"),
    names_pattern = "turn_(.*)_(.*)",
    values_to = "turnover"
  ) %>%
  mutate(
    management = case_when(
      management == "bau" ~ "Baseline",
      management == "unm" ~ "Unmanaged",
      management == "tone" ~ "ToNE",
      management == "tobd" ~ "ToBD",
      management == "tobe" ~ "ToBE"
    ),
    calc = case_when(
       calc == "of" ~ "Cpool/Outflows",
      calc == "gpp" ~ "Cpool/GPP"
    )
  )

# Prepare the data
long_data_eco_man_cc_filtered <- long_data_eco_man_cc %>%
  filter(!is.na(climate)) %>%
  mutate(management = factor(management, levels = c("Baseline", "Unmanaged", "ToNE", "ToBD", "ToBE")))
long_data_eco_man_cc_filtered$calc <- factor(long_data_eco_man_cc_filtered$calc, levels = c("Cpool/Outflows", "Cpool/GPP"))

# Create the plot
figS5 <- ggplot(long_data_eco_man_cc_filtered, aes(x = management, y = turnover, fill = calc)) +
                                      geom_boxplot(position = position_dodge(width = 0.8)) +
                                      scale_fill_manual(values = c("Cpool/Outflows" = "#FF006E", "Cpool/GPP" = "#8338ec")) +
                                      labs(y = "Turnover", x = "Management", fill = "Turnover calculation") +
                                      theme_minimal() +
                                      facet_wrap(~ climate, scales = "free_y", labeller = as_labeller(c("arid" = "ARID", 
                                      "cold with cold summer" = "COLD WITH COLD SUMMER",
                                      "cold with warm summer" = "COLD WITH WARM SUMMER",
                                      "temperate - no dry season" = "TEMPERATE - NO DRY SEASON",
                                      "temperate dry summer" = "TEMPERATE DRY SUMMER")), nrow = 1) +
                                      theme(
                                        legend.position = "bottom",
                                        axis.text.x = element_text(angle = 45, hjust = 1),
                                        strip.text = element_text(size = 10, face = "bold"),
                                        strip.background = element_rect(fill = "white", color = "black")
                                      )

# ggsave(plot = figS5, path = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\internal_review",
#             filename = "figS5.jpeg",
#                dpi = 300, width = 40, height = 10, units = c("cm"), bg = "white")
#####################################################
#...................FigS6............................
#####################################################
###############
# management-only simulations
###############
# Pivot the data longer
long_data_for_scatterplot_man_only <- turnover_eco_and_climate_man_only %>%
  pivot_longer(
    cols = starts_with("turn_"),
    names_to = c("management", "calc"),
    names_pattern = "turn_(.*)_(.*)",
    values_to = "turnover"
  ) %>%
  mutate(
    management = case_when(
      management == "bau" ~ "Baseline",
      management == "unm" ~ "Unmanaged",
      management == "tone" ~ "ToNE",
      management == "tobd" ~ "ToBD",
      management == "tobe" ~ "ToBE"
    )
  ) %>%
  filter(!is.na(climate))

# Pivot wider to create scatter data
scatter_data_man_only <- long_data_for_scatterplot_man_only %>%
  pivot_wider(
    names_from = calc, 
    values_from = turnover
  )

ordered_management_scatter <- c("Baseline", "Unmanaged", "ToNE", "ToBD", "ToBE")

figS6 <- ggplot(scatter_data_man_only, aes(x = of, y = gpp)) +
                            geom_point(alpha = 0.7) +
                            geom_smooth(method = "lm", se = FALSE, color = "#70e000") +
                            stat_cor(method = "spearman", 
                                    label.x.npc = 0.05,  # Adjust horizontal position (0-1 scale)
                                    label.y.npc = 0.95,  # Adjust vertical position (0-1 scale)
                                    size = 5,            # Adjust text size
                                    cor.coef.name = "rho",  # Use rho for Spearman correlation
                                    r.accuracy = 0.001,
                                    label.sep = "\n") +
                            facet_grid(climate ~ factor(management, levels = ordered_management_scatter), #scales = "free_y",
                            labeller = label_wrap_gen(width = 15)) +
                            labs(x = "Turnover calculated as Cpool/Outflows", y = "Turnover calculated as Cpool/GPP") +
                            theme_minimal() +
                            theme(
                              strip.text = element_text(size = 15),
                              strip.background = element_blank(),
                              axis.title = element_text(size = 15),
                              axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
                              axis.text.y = element_text(size = 15),
                              aspect.ratio = 1,
                              panel.border = element_rect(color = "black", fill = NA)
                            ) +
                            scale_x_continuous(breaks = c(0, 50, 100, 150)) + # This adds "0" to x-axis
                              scale_y_continuous(breaks = c(0, 50, 100, 150))  # This adds "0" to x-axis

# ggsave(plot = figS6, path = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\internal_review",
#            filename = "figS6.jpeg",
#                 dpi = 300, width = 40, height = 40, units = c("cm"), bg = "white")

#####################################################
#...................FigS7............................
#####################################################
###############
# management and climate change simulations
###############
long_data_for_scatterplot_man_cc <- turnover_eco_and_climate_man_cc %>%
  pivot_longer(
    cols = starts_with("turn_"),
    names_to = c("management", "calc"),
    names_pattern = "turn_(.*)_(.*)",
    values_to = "turnover"
  ) %>%
  mutate(
    management = case_when(
      management == "bau" ~ "Baseline",
      management == "unm" ~ "Unmanaged",
      management == "tone" ~ "ToNE",
      management == "tobd" ~ "ToBD",
      management == "tobe" ~ "ToBE"
    )
  ) %>%
  filter(!is.na(climate))

# Pivot wider to create scatter data
scatter_data_man_cc <- long_data_for_scatterplot_man_cc %>%
  pivot_wider(
    names_from = calc, 
    values_from = turnover
  )
figS7 <- ggplot(scatter_data_man_cc, aes(x = of, y = gpp)) +
                            geom_point(alpha = 0.7) +
                            geom_smooth(method = "lm", se = FALSE, color = "#70e000") +
                            stat_cor(method = "spearman", 
                                    label.x.npc = 0.05,  # Adjust horizontal position (0-1 scale)
                                    label.y.npc = 0.95,  # Adjust vertical position (0-1 scale)
                                    size = 5,            # Adjust text size
                                    cor.coef.name = "rho",  # Use rho for Spearman correlation
                                    r.accuracy = 0.001,
                                    label.sep = "\n") +
                            facet_grid(climate ~ factor(management, levels = ordered_management_scatter), #scales = "free_y",
                            labeller = label_wrap_gen(width = 15)) +
                            labs(x = "Turnover calculated as Cpool/Outflows", y = "Turnover calculated as Cpool/GPP") +
                            theme_minimal() +
                            theme(
                              strip.text = element_text(size = 15),
                              strip.background = element_blank(),
                              axis.title = element_text(size = 15),
                              axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
                              axis.text.y = element_text(size = 15),
                              aspect.ratio = 1,
                              panel.border = element_rect(color = "black", fill = NA)
                            ) +
                            scale_x_continuous(breaks = c(0, 50, 100, 150)) + # This adds "0" to x-axis
                              scale_y_continuous(breaks = c(0, 50, 100, 150))  # This adds "0" to x-axis

# ggsave(plot = figS7, path = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\internal_review",
#             filename = "figS7.jpeg",
#                  dpi = 300, width = 40, height = 40, units = c("cm"), bg = "white")


#####################################################
#...................FigS8............................
#####################################################
# SURF HUMUS management-only simulations
turnover_surfhum_man_only_base_df <- as.data.frame(readRDS("man_only\\base\\turnover_surfhum_man_only_base.RData"))
turnover_surfhum_man_only_unm_df <- as.data.frame(readRDS("man_only\\unmanaged\\turnover_surfhum_man_only_unm.RData"))
turnover_surfhum_man_only_to_ne_df <- as.data.frame(readRDS("man_only\\to_ne\\turnover_surfhum_man_only_to_ne.RData")) 
turnover_surfhum_man_only_to_bd_df <- as.data.frame(readRDS("man_only\\to_bd\\turnover_surfhum_man_only_to_bd.RData")) 
turnover_surfhum_man_only_to_be_df <- as.data.frame(readRDS("man_only\\to_be\\turnover_surfhum_man_only_to_be.RData")) 

turnover_surfhum_base_thresold_man_only <- inner_join(height_5mbase_man_only, turnover_surfhum_man_only_base_df, by = c("Lon" = "Lon", "Lat" = "Lat"))
turnover_surfhum_unm_thresold_man_only <- inner_join(height_5munm_man_only, turnover_surfhum_man_only_unm_df, by = c("Lon" = "Lon", "Lat" = "Lat"))
turnover_surfhum_NE_thresold_man_only <- inner_join(height_5mNE_man_only, turnover_surfhum_man_only_to_ne_df, by = c("Lon" = "Lon", "Lat" = "Lat"))
turnover_surfhum_BD_thresold_man_only <- inner_join(height_5mBD_man_only, turnover_surfhum_man_only_to_bd_df, by = c("Lon" = "Lon", "Lat" = "Lat"))
turnover_surfhum_BE_thresold_man_only <- inner_join(height_5mBE_man_only, turnover_surfhum_man_only_to_be_df, by = c("Lon" = "Lon", "Lat" = "Lat"))

merged_turn_surfhum_man_only <- list(turnover_surfhum_base_thresold_man_only, turnover_surfhum_unm_thresold_man_only, turnover_surfhum_NE_thresold_man_only, 
                    turnover_surfhum_BD_thresold_man_only, turnover_surfhum_BE_thresold_man_only) %>%
                    reduce(full_join, by = c("Lon", "Lat"))
merged_turn_surfhum_man_only <- merged_turn_surfhum_man_only %>% dplyr::select(-contains("height"))
colnames(merged_turn_surfhum_man_only) <- c("Lon", "Lat", "t_base", "t_unm", "t_NE", "t_BD", "t_BE")

plot_diff_turn_surfhum_unm_base_man_only <- create_plot(data = merged_turn_surfhum_man_only, 
            title = "Unmanaged - Baseline", 
            tag = "A", 
            column1 = "t_unm", 
            column2 = "t_base")
plot_diff_turn_surfhum_NE_base_man_only <- create_plot(data = merged_turn_surfhum_man_only, 
            title = "toNE - Baseline", 
            tag = "B", 
            column1 = "t_NE", 
            column2 = "t_base")
plot_diff_turn_surfhum_BD_base_man_only <- create_plot(data = merged_turn_surfhum_man_only, 
            title = "toBD - Baseline", 
            tag = "C", 
            column1 = "t_BD", 
            column2 = "t_base")
plot_diff_turn_surfhum_BE_base_man_only <- create_plot(data = merged_turn_surfhum_man_only, 
            title = "toBE - Baseline", 
            tag = "D", 
            column1 = "t_BE", 
            column2 = "t_base")
figS8 <-grid_arrange_shared_legend(plot_diff_turn_surfhum_unm_base_man_only,
                                plot_diff_turn_surfhum_NE_base_man_only, plot_diff_turn_surfhum_BD_base_man_only,
                                plot_diff_turn_surfhum_BE_base_man_only,
                                nrow = 2, ncol = 2, position = "right")

# ggsave(plot = figS8, path = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\internal_review",
#                filename = "figS8.jpeg",
#                dpi = 300, width = 20, height = 22, units = c("cm"), bg = "white")

#####################################################
#...................FigS9............................
#####################################################
# SURF HUMUS management and climate simulations
turnover_surfhum_man_cc_base_df <- as.data.frame(readRDS("man_cc\\base\\turnover_surfhum_man_cc_base.RData"))
turnover_surfhum_man_cc_unm_df <- as.data.frame(readRDS("man_cc\\unmanaged\\turnover_surfhum_man_cc_unm.RData"))
turnover_surfhum_man_cc_to_ne_df <- as.data.frame(readRDS("man_cc\\to_ne\\turnover_surfhum_man_cc_to_ne.RData")) 
turnover_surfhum_man_cc_to_bd_df <- as.data.frame(readRDS("man_cc\\to_bd\\turnover_surfhum_man_cc_to_bd.RData")) 
turnover_surfhum_man_cc_to_be_df <- as.data.frame(readRDS("man_cc\\to_be\\turnover_surfhum_man_cc_to_be.RData")) 

turnover_surfhum_base_thresold_man_cc <- inner_join(height_5mbase_man_cc, turnover_surfhum_man_cc_base_df, by = c("Lon" = "Lon", "Lat" = "Lat"))
turnover_surfhum_unm_thresold_man_cc <- inner_join(height_5munm_man_cc, turnover_surfhum_man_cc_unm_df, by = c("Lon" = "Lon", "Lat" = "Lat"))
turnover_surfhum_NE_thresold_man_cc <- inner_join(height_5mNE_man_cc, turnover_surfhum_man_cc_to_ne_df, by = c("Lon" = "Lon", "Lat" = "Lat"))
turnover_surfhum_BD_thresold_man_cc <- inner_join(height_5mBD_man_cc, turnover_surfhum_man_cc_to_bd_df, by = c("Lon" = "Lon", "Lat" = "Lat"))
turnover_surfhum_BE_thresold_man_cc <- inner_join(height_5mBE_man_cc, turnover_surfhum_man_cc_to_be_df, by = c("Lon" = "Lon", "Lat" = "Lat"))


merged_turn_surfhum_man_cc <- list(turnover_surfhum_base_thresold_man_cc, turnover_surfhum_unm_thresold_man_cc, turnover_surfhum_NE_thresold_man_cc, 
                    turnover_surfhum_BD_thresold_man_cc, turnover_surfhum_BE_thresold_man_cc) %>%
               reduce(full_join, by = c("Lon", "Lat"))
merged_turn_surfhum_man_cc <- merged_turn_surfhum_man_cc %>% dplyr::select(-contains("height"))
colnames(merged_turn_surfhum_man_cc) <- c("Lon", "Lat", "t_base", "t_unm", "t_NE", "t_BD", "t_BE")

plot_diff_turn_surfhum_unm_base_man_cc <- create_plot(data = merged_turn_surfhum_man_cc, 
            title = "Unmanaged - Baseline", 
            tag = "A", 
            column1 = "t_unm", 
            column2 = "t_base")

plot_diff_turn_surfhum_NE_base_man_cc <- create_plot(data = merged_turn_surfhum_man_cc, 
            title = "toNE - Baseline", 
            tag = "B", 
            column1 = "t_NE", 
            column2 = "t_base")

plot_diff_turn_surfhum_BD_base_man_cc <- create_plot(data = merged_turn_surfhum_man_cc, 
            title = "toBD - Baseline", 
            tag = "C", 
            column1 = "t_BD", 
            column2 = "t_base")

plot_diff_turn_surfhum_BE_base_man_cc <- create_plot(data = merged_turn_surfhum_man_cc, 
            title = "toBE - Baseline", 
            tag = "D", 
            column1 = "t_BE", 
            column2 = "t_base")

figS9 <-grid_arrange_shared_legend(plot_diff_turn_surfhum_unm_base_man_cc,
                                plot_diff_turn_surfhum_NE_base_man_cc, plot_diff_turn_surfhum_BD_base_man_cc,
                                plot_diff_turn_surfhum_BE_base_man_cc,
                                nrow = 2, ncol = 2, position = "right")

#  ggsave(plot = figS9, path = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\internal_review",
#                filename = "figS9.jpeg",
#                dpi = 300, width = 20, height = 22, units = c("cm"), bg = "white")
#####################################################
#...................FigS10...........................
#####################################################
#management-only simulations
create_relative_plot <- function(data, Lon, Lat, value1_col, value2_col, 
                                        plot_tag, plot_title) {
  # Calculate relative difference
  data$rel_diff <- (data[[value2_col]] - data[[value1_col]]) / 
                   pmax(abs(data[[value1_col]]), abs(data[[value2_col]]))
  
  # Generate the plot
  plot <- ggplot(data = data) +
    geom_raster(aes(x = Lon, y = Lat, fill = rel_diff)) +
    scale_fill_gradientn(colors = c("#fb8500", "#ffb703", "#fdf0d5", "#219ebc", "#023047"),
               na.value = "#FFFFFF", limits = c(-1, 1), 
               breaks = seq(from = -1, to = 1, by = 0.2),
               labels = scales::percent(seq(from = -1, to = 1, by = 0.2))) +
    labs(tag = plot_tag) +
    guides(fill = guide_colorbar(title = "Relative
Difference")) +
    ggtitle(plot_title) +
    theme_void() +
    theme(plot.tag.position = c(0.03, 0.97),
          plot.title = element_text(hjust = 0.15, size = 12, margin = margin(t = 10, b = -20)),
          panel.background = element_rect(fill = "white", colour = "black")) +
    geom_sf(data = europe, fill = "transparent") +
    geom_sf(data = ocean, fill = "light blue") +
    coord_sf(xlim = c(-10.5, 32.5), ylim = c(34, 71.5), expand = FALSE)
  
  return(plot)
}


relative_plot_diff_turn_eco_unm_base_man_only <- create_relative_plot(data = merged_turn_eco_man_only,
  value1_col = "t_unm", value2_col = "t_base",
  plot_title = "Unmanaged - Baseline", plot_tag = "A"
)

relative_plot_diff_turn_eco_NE_base_man_only <- create_relative_plot(data = merged_turn_eco_man_only,
  value1_col = "t_NE", value2_col = "t_base",
  plot_title = "toNE - Baseline", plot_tag = "B"
)

relative_plot_diff_turn_eco_BE_base_man_only <- create_relative_plot(data = merged_turn_eco_man_only,
  value1_col = "t_BE", value2_col = "t_base",
  plot_title = "toBE - Baseline", plot_tag = "D"
)

relative_plot_diff_turn_eco_BD_base_man_only <- create_relative_plot(data = merged_turn_eco_man_only,
  value1_col = "t_BD", value2_col = "t_base",
  plot_title = "toBD - Baseline", plot_tag = "C"
)


relative_plot_diff_turn_eco_base_vs_rest_man_only <-grid_arrange_shared_legend(relative_plot_diff_turn_eco_unm_base_man_only,
                                relative_plot_diff_turn_eco_NE_base_man_only, relative_plot_diff_turn_eco_BD_base_man_only,
                                relative_plot_diff_turn_eco_BE_base_man_only,
                                nrow = 1, ncol = 4, position = "right")


relative_plot_diff_turn_stem_unm_base_man_only <- create_relative_plot(data = merged_turn_stem_man_only,
  value1_col = "t_unm", value2_col = "t_base",
  plot_title = "Unmanaged - Baseline", plot_tag = "E"
)

relative_plot_diff_turn_stem_NE_base_man_only <- create_relative_plot(data = merged_turn_stem_man_only,
  value1_col = "t_NE", value2_col = "t_base",
  plot_title = "toNE - Baseline", plot_tag = "F"
)

relative_plot_diff_turn_stem_BE_base_man_only <- create_relative_plot(data = merged_turn_stem_man_only,
  value1_col = "t_BE", value2_col = "t_base",
  plot_title = "toBE - Baseline", plot_tag = "G"
)

relative_plot_diff_turn_stem_BD_base_man_only <- create_relative_plot(data = merged_turn_stem_man_only,
  value1_col = "t_BD", value2_col = "t_base",
  plot_title = "toBD - Baseline", plot_tag = "H"
)


relative_plot_diff_turn_stem_base_vs_rest_man_only <-grid_arrange_shared_legend(relative_plot_diff_turn_stem_unm_base_man_only,
                                relative_plot_diff_turn_stem_NE_base_man_only, relative_plot_diff_turn_stem_BD_base_man_only,
                                relative_plot_diff_turn_stem_BE_base_man_only,
                                nrow = 1, ncol = 4, position = "right")


relative_plot_diff_turn_tot_soil_unm_base_man_only <- create_relative_plot(data = merged_turn_tot_soil_man_only,
  value1_col = "t_unm", value2_col = "t_base",
  plot_title = "Unmanaged - Baseline", plot_tag = "I"
)

relative_plot_diff_turn_tot_soil_NE_base_man_only <- create_relative_plot(data = merged_turn_tot_soil_man_only,
  value1_col = "t_NE", value2_col = "t_base",
  plot_title = "toNE - Baseline", plot_tag = "J"
)

relative_plot_diff_turn_tot_soil_BE_base_man_only <- create_relative_plot(data = merged_turn_tot_soil_man_only,
  value1_col = "t_BE", value2_col = "t_base",
  plot_title = "toBE - Baseline", plot_tag = "K"
)

relative_plot_diff_turn_tot_soil_BD_base_man_only <- create_relative_plot(data = merged_turn_tot_soil_man_only,
  value1_col = "t_BD", value2_col = "t_base",
  plot_title = "toBD - Baseline", plot_tag = "L"
)



relative_plot_diff_turn_tot_soil_base_vs_rest_man_only <-grid_arrange_shared_legend(relative_plot_diff_turn_tot_soil_unm_base_man_only,
                                relative_plot_diff_turn_tot_soil_NE_base_man_only, relative_plot_diff_turn_tot_soil_BD_base_man_only,
                                relative_plot_diff_turn_tot_soil_BE_base_man_only,
                                nrow = 1, ncol = 4, position = "right")


# Use grid.arrange to add the title
figS10 <- grid.arrange(title_eco, relative_plot_diff_turn_eco_base_vs_rest_man_only,
                                  title_stem, relative_plot_diff_turn_stem_base_vs_rest_man_only,
                                  title_tot_soil,  relative_plot_diff_turn_tot_soil_base_vs_rest_man_only, 
                                  nrow = 6, ncol= 1, heights = c(1, 10, 1, 10, 1, 10))
# ggsave(plot = figS10, path = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\internal_review",
#                 filename = "figS10.jpeg",
#                 dpi = 300, width = 27, height = 27, units = c("cm"), bg = "white")

#####################################################
#...................FigS11...........................
#####################################################
#management and climate change simulations
#################################################################################
relative_plot_diff_turn_eco_unm_base_man_cc <- create_relative_plot(data = merged_turn_eco_man_cc,
  value1_col = "t_unm", value2_col = "t_base",
  plot_title = "Unmanaged - Baseline", plot_tag = "A"
)

relative_plot_diff_turn_eco_NE_base_man_cc <- create_relative_plot(data = merged_turn_eco_man_cc,
  value1_col = "t_NE", value2_col = "t_base",
  plot_title = "toNE - Baseline", plot_tag = "B"
)

relative_plot_diff_turn_eco_BE_base_man_cc <- create_relative_plot(data = merged_turn_eco_man_cc,
  value1_col = "t_BE", value2_col = "t_base",
  plot_title = "toBE - Baseline", plot_tag = "D"
)

relative_plot_diff_turn_eco_BD_base_man_cc <- create_relative_plot(data = merged_turn_eco_man_cc,
  value1_col = "t_BD", value2_col = "t_base",
  plot_title = "toBD - Baseline", plot_tag = "C"
)


relative_plot_diff_turn_eco_base_vs_rest_man_cc <-grid_arrange_shared_legend(relative_plot_diff_turn_eco_unm_base_man_cc,
                                relative_plot_diff_turn_eco_NE_base_man_cc, relative_plot_diff_turn_eco_BD_base_man_cc,
                                relative_plot_diff_turn_eco_BE_base_man_cc,
                                nrow = 1, ncol = 4, position = "right")


relative_plot_diff_turn_stem_unm_base_man_cc <- create_relative_plot(data = merged_turn_stem_man_cc,
  value1_col = "t_unm", value2_col = "t_base",
  plot_title = "Unmanaged - Baseline", plot_tag = "E"
)

relative_plot_diff_turn_stem_NE_base_man_cc <- create_relative_plot(data = merged_turn_stem_man_cc,
  value1_col = "t_NE", value2_col = "t_base",
  plot_title = "toNE - Baseline", plot_tag = "F"
)

relative_plot_diff_turn_stem_BE_base_man_cc <- create_relative_plot(data = merged_turn_stem_man_cc,
  value1_col = "t_BE", value2_col = "t_base",
  plot_title = "toBE - Baseline", plot_tag = "G"
)

relative_plot_diff_turn_stem_BD_base_man_cc <- create_relative_plot(data = merged_turn_stem_man_cc,
  value1_col = "t_BD", value2_col = "t_base",
  plot_title = "toBD - Baseline", plot_tag = "H"
)


relative_plot_diff_turn_stem_base_vs_rest_man_cc <-grid_arrange_shared_legend(relative_plot_diff_turn_stem_unm_base_man_cc,
                                relative_plot_diff_turn_stem_NE_base_man_cc, relative_plot_diff_turn_stem_BD_base_man_cc,
                                relative_plot_diff_turn_stem_BE_base_man_cc,
                                nrow = 1, ncol = 4, position = "right")


relative_plot_diff_turn_tot_soil_unm_base_man_cc <- create_relative_plot(data = merged_turn_tot_soil_man_cc,
  value1_col = "t_unm", value2_col = "t_base",
  plot_title = "Unmanaged - Baseline", plot_tag = "I"
)

relative_plot_diff_turn_tot_soil_NE_base_man_cc <- create_relative_plot(data = merged_turn_tot_soil_man_cc,
  value1_col = "t_NE", value2_col = "t_base",
  plot_title = "toNE - Baseline", plot_tag = "J"
)

relative_plot_diff_turn_tot_soil_BE_base_man_cc <- create_relative_plot(data = merged_turn_tot_soil_man_cc,
  value1_col = "t_BE", value2_col = "t_base",
  plot_title = "toBE - Baseline", plot_tag = "K"
)

relative_plot_diff_turn_tot_soil_BD_base_man_cc <- create_relative_plot(data = merged_turn_tot_soil_man_cc,
  value1_col = "t_BD", value2_col = "t_base",
  plot_title = "toBD - Baseline", plot_tag = "L"
)


relative_plot_diff_turn_tot_soil_base_vs_rest_man_cc <-grid_arrange_shared_legend(relative_plot_diff_turn_tot_soil_unm_base_man_cc,
                                relative_plot_diff_turn_tot_soil_NE_base_man_cc, relative_plot_diff_turn_tot_soil_BD_base_man_cc,
                                relative_plot_diff_turn_tot_soil_BE_base_man_cc,
                                nrow = 1, ncol = 4, position = "right")

# Use grid.arrange to add the title
figS11 <- grid.arrange(title_eco, relative_plot_diff_turn_eco_base_vs_rest_man_cc,
                                  title_stem, relative_plot_diff_turn_stem_base_vs_rest_man_cc,
                                  title_tot_soil,  relative_plot_diff_turn_tot_soil_base_vs_rest_man_cc, 
                                  nrow = 6, ncol= 1, heights = c(1, 10, 1, 10, 1, 10))


# ggsave(plot = figS11, path = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\internal_review",
#                  filename = "figS11.jpeg",
#                  dpi = 300, width = 27, height = 27, units = c("cm"), bg = "white")
#####################################################
#...................FigS12...........................
#####################################################
#upload the stand type data
st_2010 <- read.table("ST_2010.txt", header = TRUE) # nolint

st_only2010 <- st_2010 %>%  #keep only 2010 (last year of land use)
filter(year == 2010)

#select the ST with the greatest cover in each grid cell
dominant_st <- st_only2010 %>%
mutate(dominant_st = apply(.[, 4:7], 1, function(st_only2010)
        names(st_only2010)[which.max(st_only2010)]))

dominant_st$dominant_st <- as.factor(dominant_st$dominant_st)
plot_dominant_st <- ggplot(data = dominant_st)

plot_raster_dominant_st <- plot_dominant_st +
                           geom_raster(data = dominant_st, aes(x = Lon, y = Lat,
                                       fill = dominant_st)) +
                            scale_fill_manual(values =  c("#ffc425", "#f37735",
                            "#d11141", "#00b159"),
                            labels = c("BD", "BE", "ND", "NE")) + labs(fill = "") + # nolint
                            theme_void() +
                            guides(fill = guide_legend(title = "")) +
                            theme(panel.background = element_rect(
                                  fill = "#eaf0a1fe", colour = "black")) +
                            geom_sf(data = europe, fill = "transparent") +
                            geom_sf(data = ocean, fill = "light blue") +
                            coord_sf(xlim = c(-10.5, 40.5), ylim = c(34, 71.5),
                            expand = FALSE)

#####################################################
#...................FigS13...........................
#####################################################
#upload age structure data
#unmanaged
man_only_unm <- defineSource(id = "unm man_only",
                          dir = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\LPJG_out\\man_only\\unmanaged" ,
                          format = GUESS,
                          land.use.included = TRUE,
                          name = "unm man_only")

#baseline
man_only_base <- defineSource(id = "base man_only",
                          dir = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\LPJG_out\\man_only\\base" ,
                          format = GUESS,
                          land.use.included = TRUE,
                          name = "base man_only")

man_forest_age2220_2249_unm <- getField(source = man_only_unm, quant = "agestruct_forest", first.year = 2220, last.year = 2249)
man_forest_age2220_2249_unm_mean <- aggregateYears(man_forest_age2220_2249_unm, method ="mean")
man_forest_age2220_2249_unm_mean_df <- as.data.frame(man_forest_age2220_2249_unm_mean)

# bind the forest age df with the columns of the diff turn in which turn base is greater than turn unm 
man_forest_age2220_2249_df_AND_turn_unm <- inner_join(man_forest_age2220_2249_unm_mean_df, merged_turn_stem_man_only[, c(1:4)], by = c("Lon" = "Lon", "Lat" = "Lat"))
df_where_tbase_greater_than_tunm <- man_forest_age2220_2249_df_AND_turn_unm %>%
                                    dplyr::filter(t_base > t_unm)

man_forest_age2220_2249_df_AND_turn_unm_filtered <- df_where_tbase_greater_than_tunm %>% rowwise() %>%
                                               mutate(mean_young = mean(c_across(3:7))) %>%
                                                mutate(mean_med = mean(c_across(8:16))) %>%
                                                mutate(mean_old = mean(c_across(17:33)))


man_forest_age2220_2249_base <- getField(source = man_only_base, quant = "agestruct_forest", first.year = 2220, last.year = 2249)
man_forest_age2220_2249_base_mean <- aggregateYears(man_forest_age2220_2249_base, method ="mean")
man_forest_age2220_2249_base_mean_df <- as.data.frame(man_forest_age2220_2249_base_mean)

man_forest_age2220_2249_df_AND_turn_base <- inner_join(man_forest_age2220_2249_base_mean_df, merged_turn_stem_man_only[, c(1:4)], by = c("Lon" = "Lon", "Lat" = "Lat"))
man_forest_age2220_2249_df_AND_turn_base_filtered <-  man_forest_age2220_2249_df_AND_turn_base %>%
                                    dplyr::filter(t_base > t_unm)
man_forest_age2220_2249_df_AND_turn_base_filtered <- man_forest_age2220_2249_df_AND_turn_base_filtered %>% rowwise() %>%
                                                mutate(mean_young = mean(c_across(3:7))) %>%
                                                mutate(mean_med = mean(c_across(8:16))) %>%
                                                mutate(mean_old = mean(c_across(17:33)))

df_histogram <- man_forest_age2220_2249_df_AND_turn_base_filtered[, c(1,2,36,37,38)]
df_histogram <- cbind(df_histogram, man_forest_age2220_2249_df_AND_turn_filtered$mean_young,
                      man_forest_age2220_2249_df_AND_turn_filtered$mean_med,
                      man_forest_age2220_2249_df_AND_turn_filtered$mean_old)
colnames(df_histogram) <- c("Lon", "Lat", "baseline<50y", "baseline<140y", "baseline>140y", "unm<50y", "unm<140y", "unm>140y")


df_histogram_means <- colMeans(df_histogram[,c(3:8)])
df_histogram_means_long <- reshape2::melt(df_histogram_means)
df_histogram_means_long <- tibble::rownames_to_column(df_histogram_means_long, "variable")

df_histogram_means_long2 <-  mutate(df_histogram_means_long, management=ifelse(grepl("baseline", variable), "baseline", "unmanaged"))
df_histogram_means_long2 <- mutate(df_histogram_means_long2, forest_age=ifelse(grepl("<50", variable), "<50y",
                                     ifelse(grepl("<140", variable), "<140y", ">140y")))

ordered_ages <- c("<50y", "<140y", ">140y")
pal_age <- c("#F98400","#5BBCD6", "#EBCC2A")

hist_mean_age <- ggplot(data = df_histogram_means_long2) + 
                  geom_bar(aes( x= management, y=value, fill = factor(forest_age, ordered_ages)), stat = "identity", position = "dodge") + #facet_grid(.~management)+
                  scale_fill_manual(values = pal_age)+
                  theme(legend.position = "right")+
                  labs(fill= "Forest age")+
                  scale_y_continuous(limits = c(0,160))+
                  ggtitle("Forest age classes in areas where stem \u03C4 is longer in the
                  baseline than in unmanaged scenario (average 2220 - 2249)")+
                  ylab("N trees/ hectare") + xlab("")+
                  theme(axis.text=element_text(size=20),
                        legend.text = element_text(size = 20),
                        legend.title = element_text(size = 20, face = "bold"),
                        axis.title=element_text(size=12), plot.title = element_text(size=22))

#####################################################
#...................FigS14...........................
#####################################################

ST_2010 <- read.table("ST_2010.txt", header = TRUE)

BD_distrib <- ggplot() +
                geom_raster(data = ST_2010, aes(x = Lon, y = Lat, fill= ForestBD)) +
                scale_fill_gradient2(low = "#f7ffe8",  high = '#de425b', limits = c(0,1)) +
             guides(fill = guide_legend(title = "Forest fraction")) +
                ggtitle("Broadleaf deciduous") +
                theme_void() +
                theme(plot.title = element_text(
                        hjust = 0.15, size = 12, margin = margin(t = 10, b = -20))) +
                theme(panel.background = element_rect(
                        fill = "white", colour = "black")) +
                geom_sf(data = europe, fill = "transparent") +
                geom_sf(data = ocean, fill = "light blue") +
                coord_sf(xlim = c(-10.5, 32.5), ylim = c(34, 71.5),
                         expand = FALSE)


BE_distrib <- ggplot() +
              geom_raster(data = ST_2010, aes(x = Lon, y = Lat, fill= ForestBE)) +
                scale_fill_gradient2(low = "#f7ffe8",  high = '#de425b', limits = c(0,1)) +
              guides(fill = guide_legend(title = "Forest fraction")) +
              ggtitle("Broadleaf evergreen") +
              theme_void() +
              theme(plot.title = element_text(
                        hjust = 0.15, size = 12, margin = margin(t = 10, b = -20))) +
              theme(panel.background = element_rect(
                        fill = "white", colour = "black")) +
              geom_sf(data = europe, fill = "transparent") +
              geom_sf(data = ocean, fill = "light blue") +
              coord_sf(xlim = c(-10.5, 32.5), ylim = c(34, 71.5),
                            expand = FALSE)

NE_distrib <- ggplot() +
              geom_raster(data = ST_2010, aes(x = Lon, y = Lat, fill= ForestNE)) +
               scale_fill_gradient2(low = "#f7ffe8",  high = '#de425b', limits = c(0,1)) +
              guides(fill = guide_legend(title = "Forest fraction")) +
              ggtitle("Needleleaf evergreen") +
              theme_void() +
              theme(plot.title = element_text(
                        hjust = 0.15, size = 12, margin = margin(t = 10, b = -20))) +
              theme(panel.background = element_rect(
                        fill = "white", colour = "black")) +
              geom_sf(data = europe, fill = "transparent") +
              geom_sf(data = ocean, fill = "light blue") +
              coord_sf(xlim = c(-10.5, 32.5), ylim = c(34, 71.5),
                            expand = FALSE)

ND_distrib <- ggplot() +
              geom_raster(data = ST_2010, aes(x = Lon, y = Lat, fill= ForestND)) +
              scale_fill_gradient2(low = "#f7ffe8",  high = '#de425b', limits = c(0,1)) +
              guides(fill = guide_legend(title = "Forest fraction")) +
              ggtitle("Needleleaf deciduous") +
              theme_void() +
              theme(plot.title = element_text(
                        hjust = 0.15, size = 12, margin = margin(t = 10, b = -20))) +
              theme(panel.background = element_rect(
                        fill = "white", colour = "black")) +   
              geom_sf(data = europe, fill = "transparent") +
              geom_sf(data = ocean, fill = "light blue") +
              coord_sf(xlim = c(-10.5, 32.5), ylim = c(34, 71.5),
                            expand = FALSE)

#save all the plot in a single plot unmanaged vs other types of management
figS14 <-grid_arrange_shared_legend(NE_distrib, BD_distrib,
                                    ND_distrib, BE_distrib,
                                nrow = 2, ncol = 2, position = "right")

# ggsave(plot = figS14, path = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\internal_review",
#             filename = "figS14.jpeg",
#             dpi = 300, width = 20, height = 22, units = c("cm"), bg = "white")

#####################################################
#...................FigS15...........................
#####################################################
# upload NPP data
npp_forest_unm_man_only_mean <- read.table("aNPP_forest_unm_man_only_mean", header = TRUE)
colnames(npp_forest_unm_man_only_mean) <- c("Lon", "Lat", "aNPP_unm")

npp_forest_base_man_only_mean <- read.table("aNPP_forest_base_man_only_mean", header = TRUE)
colnames(npp_forest_base_man_only_mean) <- c("Lon", "Lat", "aNPP_base")

npp_base_unm <- cbind(npp_forest_base_man_only_mean, npp_forest_unm_man_only_mean$aNPP_unm)
colnames(npp_base_unm) <- c("Lon", "Lat", "aNPP_base", "aNPP_unm")

figS15 <- ggplot() +
                   geom_raster(data= npp_base_unm,
                        aes(x= Lon, y=Lat, fill= aNPP_base - aNPP_unm))+
                   scale_fill_gradient2(low = "#00aba9", mid = "#f7ffe8", high = '#f37735') +
                               guides(fill = guide_legend(title = "gC/m2/year")) +
                   theme_void() +
                   theme(legend.text=element_text(size=12), legend.title = element_text(size = 15)) +
                   theme(panel.background = element_rect(
                                        fill = "white", colour = "black")) +
               geom_sf(data = europe, fill = "transparent") +
                            geom_sf(data = ocean, fill = "light blue") +
                            coord_sf(xlim = c(-10.5, 32.5), ylim = c(34, 71.5),
                            expand = FALSE)

# ggsave(plot=figS15, path ="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\internal_review",
#                                  filename = "figS15.jpeg",
#                                  dpi = 300, width = 32, height = 30, units = c("cm"), bg = "white")

#####################################################
#####################################################