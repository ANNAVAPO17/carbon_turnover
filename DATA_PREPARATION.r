#####################################################
# Carbon turnover calculation from LPJ-GUESS output #
#####################################################

            #****************
            # ** ECOSYSTEM **
            #****************
#.................................................................
#ecosystem C turnover time calculated with the following equation:
# (total C - product C) / ((GPP - NPP) + soil fluxes + DOC + fire + harvest to product + harvest to flux)
#input data taken from:
#total C --> cpool_forest (total column)
#produtC --> forest_vegc (tot_prod column)
#GPP --> agpp_sts (Forest_sum column multiplied by the forest fraction)
#NPP --> anpp_sts (Forest_sum column multiplied by the forest fraction)
#soil_fluxes --> cflux_all_ForestNE,cflux_all_ForestND, cflux_all_ForestBE, cflux_all_ForestBD  (all the column to ATM are summed in each stand, and
#then the total of the stands are summed to get the forest total)
#DOC --> doc (forest column)
#fire --> forest_cflux_veg (for_fireC)
#harv_to_prod --> forest_harvest (for_harv_to_prod)
#harv_to_flux --> forest_harvest (for_harv_to_flux)
#.................................................................
library(DGVMTools)

##############################
# MANAGEMENT ONLY SIMULATIONS#
##############################
#unmanaged
man_only_unm <- defineSource(id = "unm man_only",
                          dir = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\equilibrium\\unmanaged25p" ,
                          format = GUESS,
                          land.use.included = TRUE,
                          name = "unm man_only")

#for some input I need the active fraction (to know the fraction of forest in the grid cell)
active_fraction <- getField(source = man_only_unm, quant = "active_fraction_sts", first.year = 2220, last.year = 2249)
#to get the forest cover, I sum the 4 forest stands (NE, ND, BE, BD)
forest_area <- layerOp(x=active_fraction, operator = "+", layers=c("ForestNE", "ForestBD", "ForestBE", "ForestND"), new.layer = "forest_area")

#cpool of the forest is stored in cpool_forest 
tot_cpool_man_only_unm <- getField(source = man_only_unm, quant = "cpool_forest", first.year = 2220, last.year = 2249)
#cpool of the product is stored in forest_vegc
tot_prod_man_only_unm <- getField(source= man_only_unm, quant = "forest_vegc", first.year = 2220, last.year = 2249)
#annual gpp is stored in "agpp_sts", with the values for each stand and a column for the forest_sum
gpp_man_only_unm <- getField(source = man_only_unm, quant= "agpp_sts", first.year=2220, last.year=2249)
#but I need to multiply the value of forest_sum by the forest_area
gpp_forest_man_only_unm <- calcNewField(x=gpp_man_only_unm, y=forest_area, x.col = "Forest_sum", y.col = "forest_area", op = "*" )
#same for annual npp
npp_man_only_unm <- getField(source = man_only_unm, quant="anpp_sts",first.year=2220, last.year=2249)
npp_forest_man_only_unm <- calcNewField(x=npp_man_only_unm, y=forest_area, x.col = "Forest_sum", y.col = "forest_area", op = "*" )

#the fire flux is stored in forest_cflux_veg, and I have a column for the forest
fire_man_only_unm <- getField(source = man_only_unm, quant = "forest_cflux_veg", first.year = 2220, last.year = 2249)
#also harvest (stored in "forest_harvest" has a specific column for the forest)
harvest_man_only_unm <- getField(source = man_only_unm, quant="forest_harvest", first.year = 2220, last.year = 2249)

#first I subtract the c product pool to the total cpool
cpool_no_prod_man_only_unm <- calcNewField(x= tot_cpool_man_only_unm, y= tot_prod_man_only_unm, x.col="Total", y.col="for_prod", op="-") 
#then I calculate the autotrophic respiration (GPP-NPP)
aresp_man_only_unm <- calcNewField( x=gpp_forest_man_only_unm, y=npp_forest_man_only_unm, x.col="value", y.col="value", op = "-") 
#then I sum the harvest to products and the harvest to flux, to have the sum of the harvest that leaves the ecosystem (to the products and to the atmosphere)
harvest_sum_man_only_unm <- layerOp( x= harvest_man_only_unm, operator = "+", layers = c("forharv_toprod", "forharv_toflux"), new.layer = "for_harv_out")
#then I sum the fire and the harvest fluxes
fire_and_harv_man_only_unm <- calcNewField(x=harvest_sum_man_only_unm, y=fire_man_only_unm, x.col="for_harv_out", y.col = "for_fireC", op= "+")

#hresp_man_only_unm_forest corresponds to cflux_forest.out, column soil (which also contains DOC values, so in this case we don't need to add them later)
forest_cflux_man_only_unm <- getField(source = man_only_unm, quant ="cflux_forest", first.year = 2220, last.year = 2249)
resp_and_soil_fluxes_man_only_unm <- calcNewField(x=aresp_man_only_unm, y=forest_cflux_man_only_unm, x.col = "value", y.col = "Soil", op = "+")
out_fluxes_sum_man_only_unm <- calcNewField(x=resp_and_soil_fluxes_man_only_unm, y=fire_and_harv_man_only_unm, x.col = "value", y.col = "value", op = "+" )

#I calculate the mean of the outer fluxes for the period 2220-2249 (end of the 3rd (last) forest cycle)
out_fluxes_sum_man_only_unm_mean <- aggregateYears(out_fluxes_sum_man_only_unm, method = "mean" )
#and I do the same for the c pool
cpool_no_prod_man_only_unm_mean <- aggregateYears(cpool_no_prod_man_only_unm, method = "mean")

turnover_eco_man_only_unm <- calcNewField(x=cpool_no_prod_man_only_unm_mean, y=out_fluxes_sum_man_only_unm_mean,
                            x.col= "value", y.col = "value", op= "/")

#saveRDS(turnover_eco_man_only_unm, file= "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_eco_man_only_unm.RData",ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)

#toNe
#repeat all the steps above, for the to_Ne simulation
man_only_to_ne <- defineSource(id = "to_ne man_only",
                          dir = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\equilibrium\\toNe25p" ,
                          format = GUESS,
                          land.use.included = TRUE,
                          name = "to_ne man_only")
active_fraction <- getField(source = man_only_to_ne, quant = "active_fraction_sts", first.year = 2220, last.year = 2249)
forest_area <- layerOp(x=active_fraction, operator = "+", layers=c("ForestNE", "ForestBD", "ForestBE", "ForestND"), new.layer = "forest_area")

tot_cpool_man_only_to_ne <- getField(source = man_only_to_ne, quant = "cpool_forest", first.year = 2220, last.year = 2249)
tot_prod_man_only_to_ne <- getField(source= man_only_to_ne, quant = "forest_vegc", first.year = 2220, last.year = 2249)
gpp_man_only_to_ne <- getField(source = man_only_to_ne, quant= "agpp_sts", first.year=2220, last.year=2249)
gpp_forest_man_only_to_ne <- calcNewField(x=gpp_man_only_to_ne, y=forest_area, x.col = "Forest_sum", y.col = "forest_area", op = "*" )
npp_man_only_to_ne <- getField(source = man_only_to_ne, quant="anpp_sts",first.year=2220, last.year=2249)
npp_forest_man_only_to_ne <- calcNewField(x=npp_man_only_to_ne, y=forest_area, x.col = "Forest_sum", y.col = "forest_area", op = "*" )

fire_man_only_to_ne <- getField(source = man_only_to_ne, quant = "forest_cflux_veg", first.year = 2220, last.year = 2249)
harvest_man_only_to_ne <- getField(source = man_only_to_ne, quant="forest_harvest", first.year = 2220, last.year = 2249)

cpool_no_prod_man_only_to_ne <- calcNewField(x= tot_cpool_man_only_to_ne, y= tot_prod_man_only_to_ne, x.col="Total", y.col="for_prod", op="-") 
aresp_man_only_to_ne <- calcNewField( x=gpp_forest_man_only_to_ne, y=npp_forest_man_only_to_ne, x.col="value", y.col="value", op = "-") 
harvest_sum_man_only_to_ne <- layerOp( x= harvest_man_only_to_ne, operator = "+", layers = c("forharv_toprod", "forharv_toflux"), new.layer = "for_harv_out")
fire_and_harv_man_only_to_ne <- calcNewField(x=harvest_sum_man_only_to_ne, y=fire_man_only_to_ne, x.col="for_harv_out", y.col = "for_fireC", op= "+")

forest_cflux_man_only_to_ne <- getField(source = man_only_to_ne, quant ="cflux_forest", first.year = 2220, last.year = 2249)
resp_and_soil_fluxes_man_only_to_ne <- calcNewField(x=aresp_man_only_to_ne, y=forest_cflux_man_only_to_ne, x.col = "value", y.col = "Soil", op = "+")
out_fluxes_sum_man_only_to_ne <- calcNewField(x=resp_and_soil_fluxes_man_only_to_ne, y=fire_and_harv_man_only_to_ne, x.col = "value", y.col = "value", op = "+" )

out_fluxes_sum_man_only_to_ne_mean <- aggregateYears(out_fluxes_sum_man_only_to_ne, method = "mean" )
cpool_no_prod_man_only_to_ne_mean <- aggregateYears(cpool_no_prod_man_only_to_ne, method = "mean")

turnover_eco_man_only_to_ne <- calcNewField(x=cpool_no_prod_man_only_to_ne_mean, y=out_fluxes_sum_man_only_to_ne_mean,
                            x.col= "value", y.col = "value", op= "/")
#saveRDS(turnover_eco_man_only_to_ne, file= "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_eco_man_only_to_ne.RData",ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)

#toBd
#repeat all the steps above, for the to_Bd simulation
man_only_to_bd <- defineSource(id = "to_bd man_only",
                          dir = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\equilibrium\\toBd25p" ,
                          format = GUESS,
                          land.use.included = TRUE,
                          name = "to_bd man_only")
active_fraction <- getField(source = man_only_to_bd, quant = "active_fraction_sts", first.year = 2220, last.year = 2249)
forest_area <- layerOp(x=active_fraction, operator = "+", layers=c("ForestNE", "ForestBD", "ForestBE", "ForestND"), new.layer = "forest_area")

tot_cpool_man_only_to_bd <- getField(source = man_only_to_bd, quant = "cpool_forest", first.year = 2220, last.year = 2249)
tot_prod_man_only_to_bd <- getField(source= man_only_to_bd, quant = "forest_vegc", first.year = 2220, last.year = 2249)
gpp_man_only_to_bd <- getField(source = man_only_to_bd, quant= "agpp_sts", first.year=2220, last.year=2249)
gpp_forest_man_only_to_bd <- calcNewField(x=gpp_man_only_to_bd, y=forest_area, x.col = "Forest_sum", y.col = "forest_area", op = "*" )
npp_man_only_to_bd <- getField(source = man_only_to_bd, quant="anpp_sts",first.year=2220, last.year=2249)
npp_forest_man_only_to_bd <- calcNewField(x=npp_man_only_to_bd, y=forest_area, x.col = "Forest_sum", y.col = "forest_area", op = "*" )

fire_man_only_to_bd <- getField(source = man_only_to_bd, quant = "forest_cflux_veg", first.year = 2220, last.year = 2249)
harvest_man_only_to_bd <- getField(source = man_only_to_bd, quant="forest_harvest", first.year = 2220, last.year = 2249)

cpool_no_prod_man_only_to_bd <- calcNewField(x= tot_cpool_man_only_to_bd, y= tot_prod_man_only_to_bd, x.col="Total", y.col="for_prod", op="-") 
aresp_man_only_to_bd <- calcNewField( x=gpp_forest_man_only_to_bd, y=npp_forest_man_only_to_bd, x.col="value", y.col="value", op = "-") 
harvest_sum_man_only_to_bd <- layerOp( x= harvest_man_only_to_bd, operator = "+", layers = c("forharv_toprod", "forharv_toflux"), new.layer = "for_harv_out")
fire_and_harv_man_only_to_bd <- calcNewField(x=harvest_sum_man_only_to_bd, y=fire_man_only_to_bd, x.col="for_harv_out", y.col = "for_fireC", op= "+")

forest_cflux_man_only_to_bd <- getField(source = man_only_to_bd, quant ="cflux_forest", first.year = 2220, last.year = 2249)
resp_and_soil_fluxes_man_only_to_bd <- calcNewField(x=aresp_man_only_to_bd, y=forest_cflux_man_only_to_bd, x.col = "value", y.col = "Soil", op = "+")
out_fluxes_sum_man_only_to_bd <- calcNewField(x=resp_and_soil_fluxes_man_only_to_bd, y=fire_and_harv_man_only_to_bd, x.col = "value", y.col = "value", op = "+" )

out_fluxes_sum_man_only_to_bd_mean <- aggregateYears(out_fluxes_sum_man_only_to_bd, method = "mean" )
cpool_no_prod_man_only_to_bd_mean <- aggregateYears(cpool_no_prod_man_only_to_bd, method = "mean")

turnover_eco_man_only_to_bd <- calcNewField(x=cpool_no_prod_man_only_to_bd_mean, y=out_fluxes_sum_man_only_to_bd_mean,
                            x.col= "value", y.col = "value", op= "/")
#saveRDS(turnover_eco_man_only_to_bd, file= "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_eco_man_only_to_bd.RData",ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)


#toBe
#repeat all the steps above, for the to_Be simulation
man_only_to_be <- defineSource(id = "to_be man_only",
                          dir = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\equilibrium\\toBe25p" ,
                          format = GUESS,
                          land.use.included = TRUE,
                          name = "to_be man_only")
active_fraction <- getField(source = man_only_to_be, quant = "active_fraction_sts", first.year = 2220, last.year = 2249)
forest_area <- layerOp(x=active_fraction, operator = "+", layers=c("ForestNE", "ForestBD", "ForestBE", "ForestND"), new.layer = "forest_area")

tot_cpool_man_only_to_be <- getField(source = man_only_to_be, quant = "cpool_forest", first.year = 2220, last.year = 2249)
tot_prod_man_only_to_be <- getField(source= man_only_to_be, quant = "forest_vegc", first.year = 2220, last.year = 2249)
gpp_man_only_to_be <- getField(source = man_only_to_be, quant= "agpp_sts", first.year=2220, last.year=2249)
gpp_forest_man_only_to_be <- calcNewField(x=gpp_man_only_to_be, y=forest_area, x.col = "Forest_sum", y.col = "forest_area", op = "*" )
npp_man_only_to_be <- getField(source = man_only_to_be, quant="anpp_sts",first.year=2220, last.year=2249)
npp_forest_man_only_to_be <- calcNewField(x=npp_man_only_to_be, y=forest_area, x.col = "Forest_sum", y.col = "forest_area", op = "*" )

fire_man_only_to_be <- getField(source = man_only_to_be, quant = "forest_cflux_veg", first.year = 2220, last.year = 2249)
harvest_man_only_to_be <- getField(source = man_only_to_be, quant="forest_harvest", first.year = 2220, last.year = 2249)

cpool_no_prod_man_only_to_be <- calcNewField(x= tot_cpool_man_only_to_be, y= tot_prod_man_only_to_be, x.col="Total", y.col="for_prod", op="-") 
aresp_man_only_to_be <- calcNewField( x=gpp_forest_man_only_to_be, y=npp_forest_man_only_to_be, x.col="value", y.col="value", op = "-") 
harvest_sum_man_only_to_be <- layerOp( x= harvest_man_only_to_be, operator = "+", layers = c("forharv_toprod", "forharv_toflux"), new.layer = "for_harv_out")
fire_and_harv_man_only_to_be <- calcNewField(x=harvest_sum_man_only_to_be, y=fire_man_only_to_be, x.col="for_harv_out", y.col = "for_fireC", op= "+")

forest_cflux_man_only_to_be <- getField(source = man_only_to_be, quant ="cflux_forest", first.year = 2220, last.year = 2249)
resp_and_soil_fluxes_man_only_to_be <- calcNewField(x=aresp_man_only_to_be, y=forest_cflux_man_only_to_be, x.col = "value", y.col = "Soil", op = "+")
out_fluxes_sum_man_only_to_be <- calcNewField(x=resp_and_soil_fluxes_man_only_to_be, y=fire_and_harv_man_only_to_be, x.col = "value", y.col = "value", op = "+" )

out_fluxes_sum_man_only_to_be_mean <- aggregateYears(out_fluxes_sum_man_only_to_be, method = "mean" )
cpool_no_prod_man_only_to_be_mean <- aggregateYears(cpool_no_prod_man_only_to_be, method = "mean")

turnover_eco_man_only_to_be <- calcNewField(x=cpool_no_prod_man_only_to_be_mean, y=out_fluxes_sum_man_only_to_be_mean,
                            x.col= "value", y.col = "value", op= "/")
#saveRDS(turnover_eco_man_only_to_be, file= "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_eco_man_only_to_be.RData",ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)


#baseline
#repeat all the steps above, for the baseline simulation
man_only_base <- defineSource(id = "base man_only",
                          dir = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\equilibrium\\BAU25p" ,
                          format = GUESS,
                          land.use.included = TRUE,
                          name = "base man_only")
active_fraction <- getField(source = man_only_base, quant = "active_fraction_sts", first.year = 2220, last.year = 2249)
forest_area <- layerOp(x=active_fraction, operator = "+", layers=c("ForestNE", "ForestBD", "ForestBE", "ForestND"), new.layer = "forest_area")

tot_cpool_man_only_base <- getField(source = man_only_base, quant = "cpool_forest", first.year = 2220, last.year = 2249)
tot_prod_man_only_base <- getField(source= man_only_base, quant = "forest_vegc", first.year = 2220, last.year = 2249)
gpp_man_only_base <- getField(source = man_only_base, quant= "agpp_sts", first.year=2220, last.year=2249)
gpp_forest_man_only_base <- calcNewField(x=gpp_man_only_base, y=forest_area, x.col = "Forest_sum", y.col = "forest_area", op = "*" )
npp_man_only_base <- getField(source = man_only_base, quant="anpp_sts",first.year=2220, last.year=2249)
npp_forest_man_only_base <- calcNewField(x=npp_man_only_base, y=forest_area, x.col = "Forest_sum", y.col = "forest_area", op = "*" )

fire_man_only_base <- getField(source = man_only_base, quant = "forest_cflux_veg", first.year = 2220, last.year = 2249)
harvest_man_only_base <- getField(source = man_only_base, quant="forest_harvest", first.year = 2220, last.year = 2249)

cpool_no_prod_man_only_base <- calcNewField(x= tot_cpool_man_only_base, y= tot_prod_man_only_base, x.col="Total", y.col="for_prod", op="-") 
aresp_man_only_base <- calcNewField( x=gpp_forest_man_only_base, y=npp_forest_man_only_base, x.col="value", y.col="value", op = "-") 
harvest_sum_man_only_base <- layerOp( x= harvest_man_only_base, operator = "+", layers = c("forharv_toprod", "forharv_toflux"), new.layer = "for_harv_out")
fire_and_harv_man_only_base <- calcNewField(x=harvest_sum_man_only_base, y=fire_man_only_base, x.col="for_harv_out", y.col = "for_fireC", op= "+")

forest_cflux_man_only_base <- getField(source = man_only_base, quant ="cflux_forest", first.year = 2220, last.year = 2249)
resp_and_soil_fluxes_man_only_base <- calcNewField(x=aresp_man_only_base, y=forest_cflux_man_only_base, x.col = "value", y.col = "Soil", op = "+")
out_fluxes_sum_man_only_base <- calcNewField(x=resp_and_soil_fluxes_man_only_base, y=fire_and_harv_man_only_base, x.col = "value", y.col = "value", op = "+" )

out_fluxes_sum_man_only_base_mean <- aggregateYears(out_fluxes_sum_man_only_base, method = "mean" )
cpool_no_prod_man_only_base_mean <- aggregateYears(cpool_no_prod_man_only_base, method = "mean")

turnover_eco_man_only_base <- calcNewField(x=cpool_no_prod_man_only_base_mean, y=out_fluxes_sum_man_only_base_mean,
                            x.col= "value", y.col = "value", op= "/")
#saveRDS(turnover_eco_man_only_base, file= "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_eco_man_only_base.RData",ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)

#################################################################
# save fluxes and pools from all FM in a single dataframe
#................. ECO FLUXES ..........................
aresp_man_only_unm <- as.data.frame(aresp_man_only_unm)
fire_man_only_unm <- as.data.frame(fire_man_only_unm)
harvest_man_only_unm <- as.data.frame(harvest_sum_man_only_unm)
hresp_man_only_unm <- as.data.frame(forest_cflux_man_only_unm)
fluxes_eco_man_only_unm <- cbind(aresp_man_only_unm, hresp_man_only_unm$Soil, harvest_man_only_unm$for_harv_out, fire_man_only_unm$for_fireC)
colnames(fluxes_eco_man_only_unm) <- c("Lon", "Lat", "Year", "a_resp", "h_resp", "harvest", "fire")

aresp_man_only_to_ne <- as.data.frame(aresp_man_only_to_ne)
fire_man_only_to_ne <- as.data.frame(fire_man_only_to_ne)
harvest_man_only_to_ne <- as.data.frame(harvest_sum_man_only_to_ne)
hresp_man_only_to_ne <- as.data.frame(forest_cflux_man_only_to_ne)
fluxes_eco_man_only_to_ne <- cbind(aresp_man_only_to_ne, hresp_man_only_to_ne$Soil, harvest_man_only_to_ne$for_harv_out, fire_man_only_to_ne$for_fireC)
colnames(fluxes_eco_man_only_to_ne) <- c("Lon", "Lat", "Year", "a_resp", "h_resp", "harvest", "fire")

aresp_man_only_to_bd <- as.data.frame(aresp_man_only_to_bd)
fire_man_only_to_bd <- as.data.frame(fire_man_only_to_bd)
harvest_man_only_to_bd <- as.data.frame(harvest_sum_man_only_to_bd)
hresp_man_only_to_bd <- as.data.frame(forest_cflux_man_only_to_bd)
fluxes_eco_man_only_to_bd <- cbind(aresp_man_only_to_bd, hresp_man_only_to_bd$Soil, harvest_man_only_to_bd$for_harv_out, fire_man_only_to_bd$for_fireC)
colnames(fluxes_eco_man_only_to_bd) <- c("Lon", "Lat", "Year", "a_resp", "h_resp", "harvest", "fire")

aresp_man_only_to_be <- as.data.frame(aresp_man_only_to_be)
fire_man_only_to_be <- as.data.frame(fire_man_only_to_be)
harvest_man_only_to_be <- as.data.frame(harvest_sum_man_only_to_be)
hresp_man_only_to_be <- as.data.frame(forest_cflux_man_only_to_be)
fluxes_eco_man_only_to_be <- cbind(aresp_man_only_to_be, hresp_man_only_to_be$Soil, harvest_man_only_to_be$for_harv_out, fire_man_only_to_be$for_fireC)
colnames(fluxes_eco_man_only_to_be) <- c("Lon", "Lat", "Year", "a_resp", "h_resp", "harvest", "fire")

aresp_man_only_base <- as.data.frame(aresp_man_only_base)
fire_man_only_base <- as.data.frame(fire_man_only_base)
harvest_man_only_base <- as.data.frame(harvest_sum_man_only_base)
hresp_man_only_base <- as.data.frame(forest_cflux_man_only_base)
fluxes_eco_man_only_base <- cbind(aresp_man_only_base, hresp_man_only_base$Soil, harvest_man_only_base$for_harv_out, fire_man_only_base$for_fireC)
colnames(fluxes_eco_man_only_base) <- c("Lon", "Lat", "Year", "a_resp", "h_resp", "harvest", "fire")


#combine the fluxes from each management option in a df
fluxes_eco_man_only_all <- cbind(fluxes_eco_man_only_base, fluxes_eco_man_only_to_bd[,c(4:7)], 
                                  fluxes_eco_man_only_to_be[,c(4:7)],fluxes_eco_man_only_to_ne[,c(4:7)],
                                  fluxes_eco_man_only_unm[,c(4:7)])
colnames(fluxes_eco_man_only_all) <- c("Lon", "Lat","Year",
                            "F_a_resp_base", "F_h_resp_base","F_harv_base", "F_fire_base",
                            "F_a_resp_tobd", "F_h_resp_tobd","F_harv_tobd", "F_fire_tobd",
                            "F_a_resp_tobe", "F_h_resp_tobe","F_harv_tobe", "F_fire_tobe",
                            "F_a_resp_tone", "F_h_resp_tone","F_harv_tone", "F_fire_tone",
                            "F_a_resp_unm", "F_h_resp_unm","F_harv_unm", "F_fire_unm")

#write.table(fluxes_eco_man_only_all, file = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\LPJG_out\\man_only\\fluxes_eco_man_only_all",
#               row.names = FALSE, col.names = TRUE)

#................. ECO POOLS ..........................
cpool_man_only_unm <- as.data.frame(tot_cpool_man_only_unm)
cpool_prod_man_only_unm <- as.data.frame(tot_prod_man_only_unm)
cpool_and_prod_man_only_unm <- cbind(cpool_man_only_unm, cpool_prod_man_only_unm$for_prod)
colnames(cpool_and_prod_man_only_unm)[9] <- "for_prod" 
cpool_and_tot_no_prod_man_only_unm <- cpool_and_prod_man_only_unm %>%
                        mutate(tot_no_prod = Total - for_prod )

cpool_man_only_to_ne <- as.data.frame(tot_cpool_man_only_to_ne)
cpool_prod_man_only_to_ne <- as.data.frame(tot_prod_man_only_to_ne)
cpool_and_prod_man_only_to_ne <- cbind(cpool_man_only_to_ne, cpool_prod_man_only_to_ne$for_prod)
colnames(cpool_and_prod_man_only_to_ne)[9] <- "for_prod" 
cpool_and_tot_no_prod_man_only_to_ne <- cpool_and_prod_man_only_to_ne %>%
                        mutate(tot_no_prod = Total - for_prod )

cpool_man_only_to_bd <- as.data.frame(tot_cpool_man_only_to_bd)
cpool_prod_man_only_to_bd <- as.data.frame(tot_prod_man_only_to_bd)
cpool_and_prod_man_only_to_bd <- cbind(cpool_man_only_to_bd, cpool_prod_man_only_to_bd$for_prod)
colnames(cpool_and_prod_man_only_to_bd)[9] <- "for_prod" 
cpool_and_tot_no_prod_man_only_to_bd <- cpool_and_prod_man_only_to_bd %>%
                        mutate(tot_no_prod = Total - for_prod )

cpool_man_only_to_be <- as.data.frame(tot_cpool_man_only_to_be)
cpool_prod_man_only_to_be <- as.data.frame(tot_prod_man_only_to_be)
cpool_and_prod_man_only_to_be <- cbind(cpool_man_only_to_be, cpool_prod_man_only_to_be$for_prod)
colnames(cpool_and_prod_man_only_to_be)[9] <- "for_prod" 
cpool_and_tot_no_prod_man_only_to_be <- cpool_and_prod_man_only_to_be %>%
                        mutate(tot_no_prod = Total - for_prod )

cpool_man_only_base <- as.data.frame(tot_cpool_man_only_base)
cpool_prod_man_only_base <- as.data.frame(tot_prod_man_only_base)
cpool_and_prod_man_only_base <- cbind(cpool_man_only_base, cpool_prod_man_only_base$for_prod)
colnames(cpool_and_prod_man_only_base)[9] <- "for_prod" 
cpool_and_tot_no_prod_man_only_base <- cpool_and_prod_man_only_base %>%
                        mutate(tot_no_prod = Total - for_prod )


pools_eco_man_only_all <- cbind(cpool_and_tot_no_prod_man_only_base[, c(1, 2, 3, 4, 5, 6, 10)], 
                                cpool_and_tot_no_prod_man_only_to_bd[, c(4, 5, 6, 10)], 
                                cpool_and_tot_no_prod_man_only_to_be[, c(4, 5, 6, 10)],
                                cpool_and_tot_no_prod_man_only_to_ne[, c(4, 5, 6, 10)], 
                                cpool_and_tot_no_prod_man_only_unm[, c(4, 5, 6, 10)])
colnames(pools_eco_man_only_all) <- c("Lon", "Lat","Year", "C_eco_veg_base", "C_eco_litter_base", "C_eco_soil_base", "C_eco_tot_base", 
                            "C_eco_veg_tobd", "C_eco_litter_tobd", "C_eco_soil_tobd", "C_eco_tot_tobd",
                            "C_eco_veg_tobe", "C_eco_litter_tobe", "C_eco_soil_tobe", "C_eco_tot_tobe",
                            "C_eco_veg_tone", "C_eco_litter_tone", "C_eco_soil_tone", "C_eco_tot_tone",
                            "C_eco_veg_unm", "C_eco_litter_unm", "C_eco_soil_unm", "C_eco_tot_unm")      

#write.table(pools_eco_man_only_all, file = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\LPJG_out\\man_only\\pools_eco_man_only_all",
 #           row.names = FALSE, col.names = TRUE)

#############################################################
# calculate differences between unmanaged and different FM options
#############################################################
Cturnover.Quantity <- defineQuantity(id = "Cturnover",
                                     name = "Carbon Turnover Time",
                                     colours = viridis::magma,
                                     units = "y") # note no CF standard name is defined
# and add it to the GUESS format and check it is there
GUESS.updated <- addTo(Cturnover.Quantity, GUESS)
## Also add it *directly* to the GUESS Source by using the "add.to" argument of the defineQuantity
GUESS.updated2 <- defineQuantity(id = "Cturnover2",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = GUESS) 

turnover_eco_man_only_unm <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_eco_man_only_unm)
turnover_eco_man_only_to_ne <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_eco_man_only_to_ne)
turnover_eco_man_only_to_bd <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_eco_man_only_to_bd)
turnover_eco_man_only_to_be <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_eco_man_only_to_be)
turnover_eco_man_only_base <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_eco_man_only_base)


turnover_eco_man_only_unm_df <- as.data.frame(turnover_eco_man_only_unm)
colnames(turnover_eco_man_only_unm_df) <- c("Lon", "Lat", "turn_unm") 
turnover_eco_man_only_to_ne_df <- as.data.frame(turnover_eco_man_only_to_ne)
colnames(turnover_eco_man_only_to_ne_df) <- c("Lon", "Lat", "turn_tone") #change col names not to have spaces
turnover_eco_man_only_to_bd_df <- as.data.frame(turnover_eco_man_only_to_bd)
colnames(turnover_eco_man_only_to_bd_df) <- c("Lon", "Lat", "turn_tobd") #change col names not to have spaces
turnover_eco_man_only_to_be_df <- as.data.frame(turnover_eco_man_only_to_be)
colnames(turnover_eco_man_only_to_be_df) <- c("Lon", "Lat", "turn_tobe") #change col names not to have spaces
turnover_eco_man_only_base_df <- as.data.frame(turnover_eco_man_only_base)
colnames(turnover_eco_man_only_base_df) <- c("Lon", "Lat", "turn_base") #change col names not to have spaces


library(tidyverse)
turnover_eco_man_only_list <- list(turnover_eco_man_only_unm_df, 
                                            turnover_eco_man_only_to_ne_df,
                                            turnover_eco_man_only_to_bd_df,
                                            turnover_eco_man_only_to_be_df,
                                            turnover_eco_man_only_base_df
                                            )
turnover_eco_man_only_all <- reduce(turnover_eco_man_only_list, full_join, by = c("Lon", "Lat"))

library(dplyr)
turnover_eco_man_only_all <- turnover_eco_man_only_all %>%
  mutate(
    diff_unm_base = turn_unm - turn_base,
    diff_tone_base = turn_tone - turn_base,
    diff_tobd_base = turn_tobd - turn_base,
    diff_tobe_base = turn_tobe - turn_base
  )

#saveRDS(turnover_eco_man_only_all, file= "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_eco_man_only_all_values_and_diff_with_base.RData",ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#write.table(turnover_eco_man_only_all, file = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_eco_man_only_all_values_and_diff_with_base")
############################################################################################


############################################
# MANAGEMENT AND CLIMATE CHANGE SIMULATIONS#
############################################
#unmanaged
man_cc_unm <- defineSource(id = "unm man_cc",
                          dir = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\ssp370\\unmanaged" ,
                          format = GUESS,
                          land.use.included = TRUE,
                          name = "unm man_cc")

active_fraction <- getField(source = man_cc_unm, quant = "active_fraction_sts", first.year = 2060, last.year = 2089)
forest_area <- layerOp(x=active_fraction, operator = "+", layers=c("ForestNE", "ForestBD", "ForestBE", "ForestND"), new.layer = "forest_area")

tot_cpool_man_cc_unm <- getField(source = man_cc_unm, quant = "cpool_forest", first.year = 2060, last.year = 2089)
tot_prod_man_cc_unm <- getField(source= man_cc_unm, quant = "forest_vegc", first.year = 2060, last.year = 2089)
gpp_man_cc_unm <- getField(source = man_cc_unm, quant= "agpp_sts", first.year = 2060, last.year = 2089)
gpp_forest_man_cc_unm <- calcNewField(x=gpp_man_cc_unm, y=forest_area, x.col = "Forest_sum", y.col = "forest_area", op = "*" )
npp_man_cc_unm <- getField(source = man_cc_unm, quant="anpp_sts",first.year = 2060, last.year = 2089)
npp_forest_man_cc_unm <- calcNewField(x=npp_man_cc_unm, y=forest_area, x.col = "Forest_sum", y.col = "forest_area", op = "*" )

fire_man_cc_unm <- getField(source = man_cc_unm, quant = "forest_cflux_veg", first.year = 2060, last.year = 2089)
harvest_man_cc_unm <- getField(source = man_cc_unm, quant="forest_harvest", first.year = 2060, last.year = 2089)

cpool_no_prod_man_cc_unm <- calcNewField(x= tot_cpool_man_cc_unm, y= tot_prod_man_cc_unm, x.col="Total", y.col="for_prod", op="-") 
aresp_man_cc_unm <- calcNewField( x=gpp_forest_man_cc_unm, y=npp_forest_man_cc_unm, x.col="value", y.col="value", op = "-") 
harvest_sum_man_cc_unm <- layerOp( x= harvest_man_cc_unm, operator = "+", layers = c("forharv_toprod", "forharv_toflux"), new.layer = "for_harv_out")
fire_and_harv_man_cc_unm <- calcNewField(x=harvest_sum_man_cc_unm, y=fire_man_cc_unm, x.col="for_harv_out", y.col = "for_fireC", op= "+")

forest_cflux_man_cc_unm <- getField(source = man_cc_unm, quant ="cflux_forest", first.year = 2060, last.year = 2089)
resp_and_soil_fluxes_man_cc_unm <- calcNewField(x=aresp_man_cc_unm, y=forest_cflux_man_cc_unm, x.col = "value", y.col = "Soil", op = "+")
out_fluxes_sum_man_cc_unm <- calcNewField(x=resp_and_soil_fluxes_man_cc_unm, y=fire_and_harv_man_cc_unm, x.col = "value", y.col = "value", op = "+" )

out_fluxes_sum_man_cc_unm_mean <- aggregateYears(out_fluxes_sum_man_cc_unm, method = "mean" )
cpool_no_prod_man_cc_unm_mean <- aggregateYears(cpool_no_prod_man_cc_unm, method = "mean")

turnover_eco_man_cc_unm <- calcNewField(x=cpool_no_prod_man_cc_unm_mean, y=out_fluxes_sum_man_cc_unm_mean,
                            x.col= "value", y.col = "value", op= "/")
#saveRDS(turnover_eco_man_cc_unm, file= "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_eco_man_cc_unm.RData",ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)

#toNe
#repeat all the steps above, for the to_Ne simulation
man_cc_to_ne <- defineSource(id = "to_ne man_cc",
                          dir = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\ssp370\\toNe" ,
                          format = GUESS,
                          land.use.included = TRUE,
                          name = "to_ne man_cc")
active_fraction <- getField(source = man_cc_to_ne, quant = "active_fraction_sts", first.year = 2060, last.year = 2089)
forest_area <- layerOp(x=active_fraction, operator = "+", layers=c("ForestNE", "ForestBD", "ForestBE", "ForestND"), new.layer = "forest_area")

tot_cpool_man_cc_to_ne <- getField(source = man_cc_to_ne, quant = "cpool_forest", first.year = 2060, last.year = 2089)
tot_prod_man_cc_to_ne <- getField(source= man_cc_to_ne, quant = "forest_vegc", first.year = 2060, last.year = 2089)
gpp_man_cc_to_ne <- getField(source = man_cc_to_ne, quant= "agpp_sts", first.year = 2060, last.year = 2089)
gpp_forest_man_cc_to_ne <- calcNewField(x=gpp_man_cc_to_ne, y=forest_area, x.col = "Forest_sum", y.col = "forest_area", op = "*" )
npp_man_cc_to_ne <- getField(source = man_cc_to_ne, quant="anpp_sts",first.year = 2060, last.year = 2089)
npp_forest_man_cc_to_ne <- calcNewField(x=npp_man_cc_to_ne, y=forest_area, x.col = "Forest_sum", y.col = "forest_area", op = "*" )

fire_man_cc_to_ne <- getField(source = man_cc_to_ne, quant = "forest_cflux_veg", first.year = 2060, last.year = 2089)
harvest_man_cc_to_ne <- getField(source = man_cc_to_ne, quant="forest_harvest", first.year = 2060, last.year = 2089)

cpool_no_prod_man_cc_to_ne <- calcNewField(x= tot_cpool_man_cc_to_ne, y= tot_prod_man_cc_to_ne, x.col="Total", y.col="for_prod", op="-") 
aresp_man_cc_to_ne <- calcNewField( x=gpp_forest_man_cc_to_ne, y=npp_forest_man_cc_to_ne, x.col="value", y.col="value", op = "-") 
harvest_sum_man_cc_to_ne <- layerOp( x= harvest_man_cc_to_ne, operator = "+", layers = c("forharv_toprod", "forharv_toflux"), new.layer = "for_harv_out")
fire_and_harv_man_cc_to_ne <- calcNewField(x=harvest_sum_man_cc_to_ne, y=fire_man_cc_to_ne, x.col="for_harv_out", y.col = "for_fireC", op= "+")

forest_cflux_man_cc_to_ne <- getField(source = man_cc_to_ne, quant ="cflux_forest", first.year = 2060, last.year = 2089)
resp_and_soil_fluxes_man_cc_to_ne <- calcNewField(x=aresp_man_cc_to_ne, y=forest_cflux_man_cc_to_ne, x.col = "value", y.col = "Soil", op = "+")
out_fluxes_sum_man_cc_to_ne <- calcNewField(x=resp_and_soil_fluxes_man_cc_to_ne, y=fire_and_harv_man_cc_to_ne, x.col = "value", y.col = "value", op = "+" )

out_fluxes_sum_man_cc_to_ne_mean <- aggregateYears(out_fluxes_sum_man_cc_to_ne, method = "mean" )
cpool_no_prod_man_cc_to_ne_mean <- aggregateYears(cpool_no_prod_man_cc_to_ne, method = "mean")

turnover_eco_man_cc_to_ne <- calcNewField(x=cpool_no_prod_man_cc_to_ne_mean, y=out_fluxes_sum_man_cc_to_ne_mean,
                            x.col= "value", y.col = "value", op= "/")
#saveRDS(turnover_eco_man_cc_to_ne, file= "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_eco_man_cc_to_ne.RData",ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)

#toBd
#repeat all the steps above, for the to_Bd simulation
man_cc_to_bd <- defineSource(id = "to_bd man_cc",
                          dir = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\ssp370\\toBd" ,
                          format = GUESS,
                          land.use.included = TRUE,
                          name = "to_bd man_cc")
active_fraction <- getField(source = man_cc_to_bd, quant = "active_fraction_sts", first.year = 2060, last.year = 2089)
forest_area <- layerOp(x=active_fraction, operator = "+", layers=c("ForestNE", "ForestBD", "ForestBE", "ForestND"), new.layer = "forest_area")

tot_cpool_man_cc_to_bd <- getField(source = man_cc_to_bd, quant = "cpool_forest", first.year = 2060, last.year = 2089)
tot_prod_man_cc_to_bd <- getField(source= man_cc_to_bd, quant = "forest_vegc", first.year = 2060, last.year = 2089)
gpp_man_cc_to_bd <- getField(source = man_cc_to_bd, quant= "agpp_sts", first.year = 2060, last.year = 2089)
gpp_forest_man_cc_to_bd <- calcNewField(x=gpp_man_cc_to_bd, y=forest_area, x.col = "Forest_sum", y.col = "forest_area", op = "*" )
npp_man_cc_to_bd <- getField(source = man_cc_to_bd, quant="anpp_sts",first.year = 2060, last.year = 2089)
npp_forest_man_cc_to_bd <- calcNewField(x=npp_man_cc_to_bd, y=forest_area, x.col = "Forest_sum", y.col = "forest_area", op = "*" )

fire_man_cc_to_bd <- getField(source = man_cc_to_bd, quant = "forest_cflux_veg", first.year = 2060, last.year = 2089)
harvest_man_cc_to_bd <- getField(source = man_cc_to_bd, quant="forest_harvest", first.year = 2060, last.year = 2089)

cpool_no_prod_man_cc_to_bd <- calcNewField(x= tot_cpool_man_cc_to_bd, y= tot_prod_man_cc_to_bd, x.col="Total", y.col="for_prod", op="-") 
aresp_man_cc_to_bd <- calcNewField( x=gpp_forest_man_cc_to_bd, y=npp_forest_man_cc_to_bd, x.col="value", y.col="value", op = "-") 
harvest_sum_man_cc_to_bd <- layerOp( x= harvest_man_cc_to_bd, operator = "+", layers = c("forharv_toprod", "forharv_toflux"), new.layer = "for_harv_out")
fire_and_harv_man_cc_to_bd <- calcNewField(x=harvest_sum_man_cc_to_bd, y=fire_man_cc_to_bd, x.col="for_harv_out", y.col = "for_fireC", op= "+")

forest_cflux_man_cc_to_bd <- getField(source = man_cc_to_bd, quant ="cflux_forest", first.year = 2060, last.year = 2089)
resp_and_soil_fluxes_man_cc_to_bd <- calcNewField(x=aresp_man_cc_to_bd, y=forest_cflux_man_cc_to_bd, x.col = "value", y.col = "Soil", op = "+")
out_fluxes_sum_man_cc_to_bd <- calcNewField(x=resp_and_soil_fluxes_man_cc_to_bd, y=fire_and_harv_man_cc_to_bd, x.col = "value", y.col = "value", op = "+" )

out_fluxes_sum_man_cc_to_bd_mean <- aggregateYears(out_fluxes_sum_man_cc_to_bd, method = "mean" )
cpool_no_prod_man_cc_to_bd_mean <- aggregateYears(cpool_no_prod_man_cc_to_bd, method = "mean")

turnover_eco_man_cc_to_bd <- calcNewField(x=cpool_no_prod_man_cc_to_bd_mean, y=out_fluxes_sum_man_cc_to_bd_mean,
                            x.col= "value", y.col = "value", op= "/")
#saveRDS(turnover_eco_man_cc_to_bd, file= "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_eco_man_cc_to_bd.RData",ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)

#toBe
#repeat all the steps above, for the to_Be simulation
man_cc_to_be <- defineSource(id = "to_be man_cc",
                          dir = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\ssp370\\toBe" ,
                          format = GUESS,
                          land.use.included = TRUE,
                          name = "to_be man_cc")
active_fraction <- getField(source = man_cc_to_be, quant = "active_fraction_sts", first.year = 2060, last.year = 2089)
forest_area <- layerOp(x=active_fraction, operator = "+", layers=c("ForestNE", "ForestBD", "ForestBE", "ForestND"), new.layer = "forest_area")

tot_cpool_man_cc_to_be <- getField(source = man_cc_to_be, quant = "cpool_forest", first.year = 2060, last.year = 2089)
tot_prod_man_cc_to_be <- getField(source= man_cc_to_be, quant = "forest_vegc", first.year = 2060, last.year = 2089)
gpp_man_cc_to_be <- getField(source = man_cc_to_be, quant= "agpp_sts", first.year = 2060, last.year = 2089)
gpp_forest_man_cc_to_be <- calcNewField(x=gpp_man_cc_to_be, y=forest_area, x.col = "Forest_sum", y.col = "forest_area", op = "*" )
npp_man_cc_to_be <- getField(source = man_cc_to_be, quant="anpp_sts",first.year = 2060, last.year = 2089)
npp_forest_man_cc_to_be <- calcNewField(x=npp_man_cc_to_be, y=forest_area, x.col = "Forest_sum", y.col = "forest_area", op = "*" )

fire_man_cc_to_be <- getField(source = man_cc_to_be, quant = "forest_cflux_veg", first.year = 2060, last.year = 2089)
harvest_man_cc_to_be <- getField(source = man_cc_to_be, quant="forest_harvest", first.year = 2060, last.year = 2089)

cpool_no_prod_man_cc_to_be <- calcNewField(x= tot_cpool_man_cc_to_be, y= tot_prod_man_cc_to_be, x.col="Total", y.col="for_prod", op="-") 
aresp_man_cc_to_be <- calcNewField( x=gpp_forest_man_cc_to_be, y=npp_forest_man_cc_to_be, x.col="value", y.col="value", op = "-") 
harvest_sum_man_cc_to_be <- layerOp( x= harvest_man_cc_to_be, operator = "+", layers = c("forharv_toprod", "forharv_toflux"), new.layer = "for_harv_out")
fire_and_harv_man_cc_to_be <- calcNewField(x=harvest_sum_man_cc_to_be, y=fire_man_cc_to_be, x.col="for_harv_out", y.col = "for_fireC", op= "+")

forest_cflux_man_cc_to_be <- getField(source = man_cc_to_be, quant ="cflux_forest", first.year = 2060, last.year = 2089)
resp_and_soil_fluxes_man_cc_to_be <- calcNewField(x=aresp_man_cc_to_be, y=forest_cflux_man_cc_to_be, x.col = "value", y.col = "Soil", op = "+")
out_fluxes_sum_man_cc_to_be <- calcNewField(x=resp_and_soil_fluxes_man_cc_to_be, y=fire_and_harv_man_cc_to_be, x.col = "value", y.col = "value", op = "+" )

out_fluxes_sum_man_cc_to_be_mean <- aggregateYears(out_fluxes_sum_man_cc_to_be, method = "mean" )
cpool_no_prod_man_cc_to_be_mean <- aggregateYears(cpool_no_prod_man_cc_to_be, method = "mean")

turnover_eco_man_cc_to_be <- calcNewField(x=cpool_no_prod_man_cc_to_be_mean, y=out_fluxes_sum_man_cc_to_be_mean,
                            x.col= "value", y.col = "value", op= "/")
#saveRDS(turnover_eco_man_cc_to_be, file= "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_eco_man_cc_to_be.RData",ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)

#baseline
#repeat all the steps above, for the baseline simulation
man_cc_base <- defineSource(id = "base man_cc",
                          dir = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\ssp370\\BAU" ,
                          format = GUESS,
                          land.use.included = TRUE,
                          name = "base man_cc")
active_fraction <- getField(source = man_cc_base, quant = "active_fraction_sts", first.year = 2060, last.year = 2089)
forest_area <- layerOp(x=active_fraction, operator = "+", layers=c("ForestNE", "ForestBD", "ForestBE", "ForestND"), new.layer = "forest_area")

tot_cpool_man_cc_base <- getField(source = man_cc_base, quant = "cpool_forest", first.year = 2060, last.year = 2089)
tot_prod_man_cc_base <- getField(source= man_cc_base, quant = "forest_vegc", first.year = 2060, last.year = 2089)
gpp_man_cc_base <- getField(source = man_cc_base, quant= "agpp_sts", first.year = 2060, last.year = 2089)
gpp_forest_man_cc_base <- calcNewField(x=gpp_man_cc_base, y=forest_area, x.col = "Forest_sum", y.col = "forest_area", op = "*" )
npp_man_cc_base <- getField(source = man_cc_base, quant="anpp_sts",first.year = 2060, last.year = 2089)
npp_forest_man_cc_base <- calcNewField(x=npp_man_cc_base, y=forest_area, x.col = "Forest_sum", y.col = "forest_area", op = "*" )

fire_man_cc_base <- getField(source = man_cc_base, quant = "forest_cflux_veg", first.year = 2060, last.year = 2089)
harvest_man_cc_base <- getField(source = man_cc_base, quant="forest_harvest", first.year = 2060, last.year = 2089)

cpool_no_prod_man_cc_base <- calcNewField(x= tot_cpool_man_cc_base, y= tot_prod_man_cc_base, x.col="Total", y.col="for_prod", op="-") 
aresp_man_cc_base <- calcNewField( x=gpp_forest_man_cc_base, y=npp_forest_man_cc_base, x.col="value", y.col="value", op = "-") 
harvest_sum_man_cc_base <- layerOp( x= harvest_man_cc_base, operator = "+", layers = c("forharv_toprod", "forharv_toflux"), new.layer = "for_harv_out")
fire_and_harv_man_cc_base <- calcNewField(x=harvest_sum_man_cc_base, y=fire_man_cc_base, x.col="for_harv_out", y.col = "for_fireC", op= "+")

forest_cflux_man_cc_base <- getField(source = man_cc_base, quant ="cflux_forest", first.year = 2060, last.year = 2089)
resp_and_soil_fluxes_man_cc_base <- calcNewField(x=aresp_man_cc_base, y=forest_cflux_man_cc_base, x.col = "value", y.col = "Soil", op = "+")
out_fluxes_sum_man_cc_base <- calcNewField(x=resp_and_soil_fluxes_man_cc_base, y=fire_and_harv_man_cc_base, x.col = "value", y.col = "value", op = "+" )

out_fluxes_sum_man_cc_base_mean <- aggregateYears(out_fluxes_sum_man_cc_base, method = "mean" )
cpool_no_prod_man_cc_base_mean <- aggregateYears(cpool_no_prod_man_cc_base, method = "mean")

turnover_eco_man_cc_base <- calcNewField(x=cpool_no_prod_man_cc_base_mean, y=out_fluxes_sum_man_cc_base_mean,
                            x.col= "value", y.col = "value", op= "/")
#saveRDS(turnover_eco_man_cc_base, file= "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_eco_man_cc_base.RData",ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)

#################################################################
# save fluxes and pools from all FM in a single dataframe
#................. ECO FLUXES ..........................
aresp_man_cc_unm <- as.data.frame(aresp_man_cc_unm)
fire_man_cc_unm <- as.data.frame(fire_man_cc_unm)
harvest_man_cc_unm <- as.data.frame(harvest_sum_man_cc_unm)
hresp_man_cc_unm <- as.data.frame(forest_cflux_man_cc_unm)
fluxes_eco_man_cc_unm <- cbind(aresp_man_cc_unm, hresp_man_cc_unm$Soil, harvest_man_cc_unm$for_harv_out, fire_man_cc_unm$for_fireC)
colnames(fluxes_eco_man_cc_unm) <- c("Lon", "Lat", "Year", "a_resp", "h_resp", "harvest", "fire")

aresp_man_cc_to_ne <- as.data.frame(aresp_man_cc_to_ne)
fire_man_cc_to_ne <- as.data.frame(fire_man_cc_to_ne)
harvest_man_cc_to_ne <- as.data.frame(harvest_sum_man_cc_to_ne)
hresp_man_cc_to_ne <- as.data.frame(forest_cflux_man_cc_to_ne)
fluxes_eco_man_cc_to_ne <- cbind(aresp_man_cc_to_ne, hresp_man_cc_to_ne$Soil, harvest_man_cc_to_ne$for_harv_out, fire_man_cc_to_ne$for_fireC)
colnames(fluxes_eco_man_cc_to_ne) <- c("Lon", "Lat", "Year", "a_resp", "h_resp", "harvest", "fire")

aresp_man_cc_to_bd <- as.data.frame(aresp_man_cc_to_bd)
fire_man_cc_to_bd <- as.data.frame(fire_man_cc_to_bd)
harvest_man_cc_to_bd <- as.data.frame(harvest_sum_man_cc_to_bd)
hresp_man_cc_to_bd <- as.data.frame(forest_cflux_man_cc_to_bd)
fluxes_eco_man_cc_to_bd <- cbind(aresp_man_cc_to_bd, hresp_man_cc_to_bd$Soil, harvest_man_cc_to_bd$for_harv_out, fire_man_cc_to_bd$for_fireC)
colnames(fluxes_eco_man_cc_to_bd) <- c("Lon", "Lat", "Year", "a_resp", "h_resp", "harvest", "fire")

aresp_man_cc_to_be <- as.data.frame(aresp_man_cc_to_be)
fire_man_cc_to_be <- as.data.frame(fire_man_cc_to_be)
harvest_man_cc_to_be <- as.data.frame(harvest_sum_man_cc_to_be)
hresp_man_cc_to_be <- as.data.frame(forest_cflux_man_cc_to_be)
fluxes_eco_man_cc_to_be <- cbind(aresp_man_cc_to_be, hresp_man_cc_to_be$Soil, harvest_man_cc_to_be$for_harv_out, fire_man_cc_to_be$for_fireC)
colnames(fluxes_eco_man_cc_to_be) <- c("Lon", "Lat", "Year", "a_resp", "h_resp", "harvest", "fire")

aresp_man_cc_base <- as.data.frame(aresp_man_cc_base)
fire_man_cc_base <- as.data.frame(fire_man_cc_base)
harvest_man_cc_base <- as.data.frame(harvest_sum_man_cc_base)
hresp_man_cc_base <- as.data.frame(forest_cflux_man_cc_base)
fluxes_eco_man_cc_base <- cbind(aresp_man_cc_base, hresp_man_cc_base$Soil, harvest_man_cc_base$for_harv_out, fire_man_cc_base$for_fireC)
colnames(fluxes_eco_man_cc_base) <- c("Lon", "Lat", "Year", "a_resp", "h_resp", "harvest", "fire")


#combine the fluxes from each management option in a df
fluxes_eco_man_cc_all <- cbind(fluxes_eco_man_cc_base, fluxes_eco_man_cc_to_bd[,c(4:7)], 
                                  fluxes_eco_man_cc_to_be[,c(4:7)],fluxes_eco_man_cc_to_ne[,c(4:7)],
                                  fluxes_eco_man_cc_unm[,c(4:7)])
colnames(fluxes_eco_man_cc_all) <- c("Lon", "Lat","Year",
                            "F_a_resp_base", "F_h_resp_base","F_harv_base", "F_fire_base",
                            "F_a_resp_tobd", "F_h_resp_tobd","F_harv_tobd", "F_fire_tobd",
                            "F_a_resp_tobe", "F_h_resp_tobe","F_harv_tobe", "F_fire_tobe",
                            "F_a_resp_tone", "F_h_resp_tone","F_harv_tone", "F_fire_tone",
                            "F_a_resp_unm", "F_h_resp_unm","F_harv_unm", "F_fire_unm")

#write.table(fluxes_eco_man_cc_all, file = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\LPJG_out\\man_cc\\fluxes_eco_man_cc_all",
 #              row.names = FALSE, col.names = TRUE)

#................. ECO POOLS ..........................
cpool_man_cc_unm <- as.data.frame(tot_cpool_man_cc_unm)
cpool_prod_man_cc_unm <- as.data.frame(tot_prod_man_cc_unm)
cpool_and_prod_man_cc_unm <- cbind(cpool_man_cc_unm, cpool_prod_man_cc_unm$for_prod)
colnames(cpool_and_prod_man_cc_unm)[9] <- "for_prod" 
cpool_and_tot_no_prod_man_cc_unm <- cpool_and_prod_man_cc_unm %>%
                        mutate(tot_no_prod = Total - for_prod )

cpool_man_cc_to_ne <- as.data.frame(tot_cpool_man_cc_to_ne)
cpool_prod_man_cc_to_ne <- as.data.frame(tot_prod_man_cc_to_ne)
cpool_and_prod_man_cc_to_ne <- cbind(cpool_man_cc_to_ne, cpool_prod_man_cc_to_ne$for_prod)
colnames(cpool_and_prod_man_cc_to_ne)[9] <- "for_prod" 
cpool_and_tot_no_prod_man_cc_to_ne <- cpool_and_prod_man_cc_to_ne %>%
                        mutate(tot_no_prod = Total - for_prod )

cpool_man_cc_to_bd <- as.data.frame(tot_cpool_man_cc_to_bd)
cpool_prod_man_cc_to_bd <- as.data.frame(tot_prod_man_cc_to_bd)
cpool_and_prod_man_cc_to_bd <- cbind(cpool_man_cc_to_bd, cpool_prod_man_cc_to_bd$for_prod)
colnames(cpool_and_prod_man_cc_to_bd)[9] <- "for_prod" 
cpool_and_tot_no_prod_man_cc_to_bd <- cpool_and_prod_man_cc_to_bd %>%
                        mutate(tot_no_prod = Total - for_prod )

cpool_man_cc_to_be <- as.data.frame(tot_cpool_man_cc_to_be)
cpool_prod_man_cc_to_be <- as.data.frame(tot_prod_man_cc_to_be)
cpool_and_prod_man_cc_to_be <- cbind(cpool_man_cc_to_be, cpool_prod_man_cc_to_be$for_prod)
colnames(cpool_and_prod_man_cc_to_be)[9] <- "for_prod" 
cpool_and_tot_no_prod_man_cc_to_be <- cpool_and_prod_man_cc_to_be %>%
                        mutate(tot_no_prod = Total - for_prod )

cpool_man_cc_base <- as.data.frame(tot_cpool_man_cc_base)
cpool_prod_man_cc_base <- as.data.frame(tot_prod_man_cc_base)
cpool_and_prod_man_cc_base <- cbind(cpool_man_cc_base, cpool_prod_man_cc_base$for_prod)
colnames(cpool_and_prod_man_cc_base)[9] <- "for_prod" 
cpool_and_tot_no_prod_man_cc_base <- cpool_and_prod_man_cc_base %>%
                        mutate(tot_no_prod = Total - for_prod )


pools_eco_man_cc_all <- cbind(cpool_and_tot_no_prod_man_cc_base[, c(1, 2, 3, 4, 5, 6, 10)], 
                                cpool_and_tot_no_prod_man_cc_to_bd[, c(4, 5, 6, 10)], 
                                cpool_and_tot_no_prod_man_cc_to_be[, c(4, 5, 6, 10)],
                                cpool_and_tot_no_prod_man_cc_to_ne[, c(4, 5, 6, 10)], 
                                cpool_and_tot_no_prod_man_cc_unm[, c(4, 5, 6, 10)])
colnames(pools_eco_man_cc_all) <- c("Lon", "Lat","Year", "C_eco_veg_base", "C_eco_litter_base", "C_eco_soil_base", "C_eco_tot_base", 
                            "C_eco_veg_tobd", "C_eco_litter_tobd", "C_eco_soil_tobd", "C_eco_tot_tobd",
                            "C_eco_veg_tobe", "C_eco_litter_tobe", "C_eco_soil_tobe", "C_eco_tot_tobe",
                            "C_eco_veg_tone", "C_eco_litter_tone", "C_eco_soil_tone", "C_eco_tot_tone",
                            "C_eco_veg_unm", "C_eco_litter_unm", "C_eco_soil_unm", "C_eco_tot_unm")      

#write.table(pools_eco_man_cc_all, file = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\LPJG_out\\man_cc\\pools_eco_man_cc_all",
 #           row.names = FALSE, col.names = TRUE)


#############################################################
# calculate differences between unmanaged and different FM options
#############################################################
turnover_eco_man_cc_unm <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_eco_man_cc_unm)
turnover_eco_man_cc_to_ne <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_eco_man_cc_to_ne)
turnover_eco_man_cc_to_bd <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_eco_man_cc_to_bd)
turnover_eco_man_cc_to_be <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_eco_man_cc_to_be)
turnover_eco_man_cc_base <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_eco_man_cc_base)

turnover_eco_man_cc_unm_df <- as.data.frame(turnover_eco_man_cc_unm)
colnames(turnover_eco_man_cc_unm_df) <- c("Lon", "Lat", "turn_unm") 
turnover_eco_man_cc_to_ne_df <- as.data.frame(turnover_eco_man_cc_to_ne)
colnames(turnover_eco_man_cc_to_ne_df) <- c("Lon", "Lat", "turn_tone") #change col names not to have spaces
turnover_eco_man_cc_to_bd_df <- as.data.frame(turnover_eco_man_cc_to_bd)
colnames(turnover_eco_man_cc_to_bd_df) <- c("Lon", "Lat", "turn_tobd") #change col names not to have spaces
turnover_eco_man_cc_to_be_df <- as.data.frame(turnover_eco_man_cc_to_be)
colnames(turnover_eco_man_cc_to_be_df) <- c("Lon", "Lat", "turn_tobe") #change col names not to have spaces
turnover_eco_man_cc_base_df <- as.data.frame(turnover_eco_man_cc_base)
colnames(turnover_eco_man_cc_base_df) <- c("Lon", "Lat", "turn_base") #change col names not to have spaces

turnover_eco_man_cc_list <- list(turnover_eco_man_cc_unm_df, 
                                            turnover_eco_man_cc_to_ne_df,
                                            turnover_eco_man_cc_to_bd_df,
                                            turnover_eco_man_cc_to_be_df,
                                            turnover_eco_man_cc_base_df
                                            )
turnover_eco_man_cc_all <- reduce(turnover_eco_man_cc_list, full_join, by = c("Lon", "Lat"))
turnover_eco_man_cc_all <- turnover_eco_man_cc_all %>%
  mutate(
    diff_unm_base = turn_unm - turn_base,
    diff_tone_base = turn_tone - turn_base,
    diff_tobd_base = turn_tobd - turn_base,
    diff_tobe_base = turn_tobe - turn_base
  )

#saveRDS(turnover_eco_man_cc_all, file= "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_eco_man_cc_all_values_and_diff_with_base.RData",ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#write.table(turnover_eco_man_cc_all, file = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_eco_man_cc_all_values_and_diff_with_base")
############################################################################################



            #****************
            # **** STEM *****
            #****************


#######################################################################################
# calculate carbon turnover STEM 
# variation of the formula of Pugh et al. (2019)  Fturn = Fmort + Fdist + Ffire + Fharv
# we calculate it only for stem wood (not including Fleaves, Ffineroots and Frepro)

setwd("C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\equilibrium")

####################################
##calculate turnover in 2220-2249 for the different management options

#unmanaged
cpool_man_only_unm <- getField(source = man_only_unm, quant= "forest_vegc", first.year=2220, last.year=2249)
cflux_man_only_unm <- getField(source = man_only_unm, quant="forest_cflux_veg",first.year=2220, last.year=2249)

cfluxes_man_only_unm <- layerOp( x=cflux_man_only_unm,operator = "+", layers = c("for_harvC",             #sum outer fluxes and store the in a new layer
                                "for_mortC", "for_fireC", "for_distC"),
                                  new.layer = "tot_out_flux")
cpool_man_only_unm_mean <- aggregateYears(cpool_man_only_unm, method ="mean")
cfluxes_man_only_unm_mean <- aggregateYears(cfluxes_man_only_unm, method ="mean")

turnover_stem_man_only_unm <- calcNewField( x=cpool_man_only_unm_mean, y=cfluxes_man_only_unm_mean,                   #calculate turnover as VegC/outer fluxes
                            x.col="for_stemC", y.col="tot_out_flux", op = "/")

#saveRDS(turnover_stem_man_only_unm, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_stem_man_only_unm.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)

#to needleleaf evergreen
cpool_man_only_to_ne <- getField(source = man_only_to_ne, quant= "forest_vegc", first.year=2220, last.year=2249)
cflux_man_only_to_ne <- getField(source = man_only_to_ne, quant="forest_cflux_veg",first.year=2220, last.year=2249)
cfluxes_man_only_to_ne <- layerOp( x=cflux_man_only_to_ne,operator = "+", layers = c("for_harvC",             #sum outer fluxes and store the in a new layer
                                "for_mortC", "for_fireC", "for_distC"),
                                new.layer = "tot_out_flux")
cpool_man_only_to_ne_mean <- aggregateYears(cpool_man_only_to_ne, method ="mean")
cfluxes_man_only_to_ne_mean <- aggregateYears(cfluxes_man_only_to_ne, method ="mean")

turnover_stem_man_only_to_ne<- calcNewField( x=cpool_man_only_to_ne_mean, y=cfluxes_man_only_to_ne_mean,                   #calculate turnover as VegC/outer fluxes
                            x.col="for_stemC", y.col="tot_out_flux", op = "/")

#saveRDS(turnover_stem_man_only_to_ne, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_stem_man_only_to_ne.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)

#to broadleaf deciduous
cpool_man_only_to_bd <- getField(source = man_only_to_bd, quant= "forest_vegc", first.year=2220, last.year=2249)
cflux_man_only_to_bd <- getField(source = man_only_to_bd, quant="forest_cflux_veg",first.year=2220, last.year=2249)
cfluxes_man_only_to_bd <- layerOp( x=cflux_man_only_to_bd,operator = "+", layers = c("for_harvC",             #sum outer fluxes and store the in a new layer
                                "for_mortC", "for_fireC", "for_distC"),
                                  new.layer = "tot_out_flux")
cpool_man_only_to_bd_mean <- aggregateYears(cpool_man_only_to_bd, method ="mean")
cfluxes_man_only_to_bd_mean <- aggregateYears(cfluxes_man_only_to_bd, method ="mean")

turnover_stem_man_only_to_bd<- calcNewField( x=cpool_man_only_to_bd_mean, y=cfluxes_man_only_to_bd_mean,                   #calculate turnover as VegC/outer fluxes
                            x.col="for_stemC", y.col="tot_out_flux", op = "/")

#saveRDS(turnover_stem_man_only_to_bd, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_stem_man_only_to_bd.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)


#to broadleaf evergreen
cpool_man_only_to_be <- getField(source = man_only_to_be, quant= "forest_vegc", first.year=2220, last.year=2249)
cflux_man_only_to_be <- getField(source = man_only_to_be, quant="forest_cflux_veg",first.year=2220, last.year=2249)
cfluxes_man_only_to_be <- layerOp( x=cflux_man_only_to_be,operator = "+", layers = c("for_harvC",             #sum outer fluxes and store the in a new layer
                                "for_mortC", "for_fireC", "for_distC"),
                                  new.layer = "tot_out_flux")
cpool_man_only_to_be_mean <- aggregateYears(cpool_man_only_to_be, method ="mean")
cfluxes_man_only_to_be_mean <- aggregateYears(cfluxes_man_only_to_be, method ="mean")

turnover_stem_man_only_to_be<- calcNewField( x=cpool_man_only_to_be_mean, y=cfluxes_man_only_to_be_mean,                   #calculate turnover as VegC/outer fluxes
                            x.col="for_stemC", y.col="tot_out_flux", op = "/")

#saveRDS(turnover_stem_man_only_to_be, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_stem_man_only_to_be.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)


#base
cpool_man_only_base <- getField(source = man_only_base, quant= "forest_vegc", first.year=2220, last.year=2249)
cflux_man_only_base <- getField(source = man_only_base, quant="forest_cflux_veg",first.year=2220, last.year=2249)
cfluxes_man_only_base <- layerOp( x=cflux_man_only_base,operator = "+", layers = c("for_harvC",             #sum outer fluxes and store the in a new layer
                                "for_mortC", "for_fireC", "for_distC"),
                                  new.layer = "tot_out_flux")
cpool_man_only_base_mean <- aggregateYears(cpool_man_only_base, method ="mean")
cfluxes_man_only_base_mean <- aggregateYears(cfluxes_man_only_base, method ="mean")

turnover_stem_man_only_base<- calcNewField( x=cpool_man_only_base_mean, y=cfluxes_man_only_base_mean,                   #calculate turnover as VegC/outer fluxes
                            x.col="for_stemC", y.col="tot_out_flux", op = "/")

#saveRDS(turnover_stem_man_only_base, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_stem_man_only_base.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)



################################################################################################
# save fluxes and pools from all FM in a single dataframe
#................. STEM FLUXES ..........................

#outer fluxes:
# - harvest
# - mortality
# - fire
# - disturbance 

#all the output are in the forest_cfluxes file, previously uploaded and called "fire_unm"
# because I needed the fire fluxes from it

fluxes_stem_man_only_unm <- fire_man_only_unm[, c(1, 2, 3, 16, 17, 18, 20)]
fluxes_stem_man_only_base <- fire_man_only_base[, c(1, 2, 3, 16, 17, 18, 20)]
fluxes_stem_man_only_to_be <- fire_man_only_to_be[, c(1, 2, 3, 16, 17, 18, 20)]
fluxes_stem_man_only_to_ne <- fire_man_only_to_ne[, c(1, 2, 3, 16, 17, 18, 20)]
fluxes_stem_man_only_to_bd <- fire_man_only_to_bd[, c(1, 2, 3, 16, 17, 18, 20)]

#combine the fluxes from each management option in a df
fluxes_stem_man_only_all <- cbind(fluxes_stem_man_only_base, fluxes_stem_man_only_to_bd[,c(4:7)],
                                   fluxes_stem_man_only_to_be[,c(4:7)], fluxes_stem_man_only_to_ne[,c(4:7)], fluxes_stem_man_only_unm[,c(4:7)])
colnames(fluxes_stem_man_only_all) <- c("Lon", "Lat","Year",
                            "F_harv_base", "F_mort_base","F_fire_base", "F_dist_base",
                            "F_harv_tobd", "F_mort_tobd","F_fire_tobd", "F_dist_tobd",
                            "F_harv_tobe", "F_mort_tobe","F_fire_tobe", "F_dist_tobe",
                            "F_harv_tone", "F_mort_tone","F_fire_tone", "F_dist_tone",
                            "F_harv_unm", "F_mort_unm","F_fire_unm", "F_dist_unm")

#write.table(fluxes_stem_man_only_all, file = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\LPJG_out\\man_only\\fluxes_stem_man_only_all",
 #              row.names = FALSE, col.names = TRUE)

#................. STEM POOLS ..........................

stem_pool_man_only_unm <- as.data.frame(tot_prod_man_only_unm)
stem_pool_man_only_unm <- stem_pool_man_only_unm[, c(1:3, 9)]

stem_pool_man_only_base <- as.data.frame(tot_prod_man_only_base)
stem_pool_man_only_base <- stem_pool_man_only_base[, c(1:3, 9)]

stem_pool_man_only_to_ne <- as.data.frame(tot_prod_man_only_to_ne)
stem_pool_man_only_to_ne <- stem_pool_man_only_to_ne[, c(1:3, 9)]

stem_pool_man_only_to_bd <- as.data.frame(tot_prod_man_only_to_bd)
stem_pool_man_only_to_bd <- stem_pool_man_only_to_bd[, c(1:3, 9)]

stem_pool_man_only_to_be <- as.data.frame(tot_prod_man_only_to_be)
stem_pool_man_only_to_be <- stem_pool_man_only_to_be[, c(1:3, 9)]

pools_stem_man_only_all <- cbind(stem_pool_man_only_base, stem_pool_man_only_to_bd[, 4], stem_pool_man_only_to_be[, 4],
                                 stem_pool_man_only_to_ne[, 4], stem_pool_man_only_unm[, 4])
colnames(pools_stem_man_only_all) <- c("Lon", "Lat","Year",
                            "C_stem_base", "C_stem_tobd", "C_stem_tobe", "C_stem_tone", "C_stem_unm")

#write.table(pools_stem_man_only_all, file = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\LPJG_out\\man_only\\pools_stem_man_only_all",
 #              row.names = FALSE, col.names = TRUE)

#############################################################
# calculate differences between unmanaged and different FM options
#############################################################
turnover_stem_man_only_unm <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_stem_man_only_unm)
turnover_stem_man_only_to_ne <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_stem_man_only_to_ne)
turnover_stem_man_only_to_bd <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_stem_man_only_to_bd)
turnover_stem_man_only_to_be <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_stem_man_only_to_be)
turnover_stem_man_only_base <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_stem_man_only_base)


turnover_stem_man_only_unm_df <- as.data.frame(turnover_stem_man_only_unm)
colnames(turnover_stem_man_only_unm_df) <- c("Lon", "Lat", "turn_unm") 
turnover_stem_man_only_to_ne_df <- as.data.frame(turnover_stem_man_only_to_ne)
colnames(turnover_stem_man_only_to_ne_df) <- c("Lon", "Lat", "turn_tone") #change col names not to have spaces
turnover_stem_man_only_to_bd_df <- as.data.frame(turnover_stem_man_only_to_bd)
colnames(turnover_stem_man_only_to_bd_df) <- c("Lon", "Lat", "turn_tobd") #change col names not to have spaces
turnover_stem_man_only_to_be_df <- as.data.frame(turnover_stem_man_only_to_be)
colnames(turnover_stem_man_only_to_be_df) <- c("Lon", "Lat", "turn_tobe") #change col names not to have spaces
turnover_stem_man_only_base_df <- as.data.frame(turnover_stem_man_only_base)
colnames(turnover_stem_man_only_base_df) <- c("Lon", "Lat", "turn_base") #change col names not to have spaces

turnover_stem_man_only_list <- list(turnover_stem_man_only_unm_df, 
                                            turnover_stem_man_only_to_ne_df,
                                            turnover_stem_man_only_to_bd_df,
                                            turnover_stem_man_only_to_be_df,
                                            turnover_stem_man_only_base_df
                                            )
turnover_stem_man_only_all <- reduce(turnover_stem_man_only_list, full_join, by = c("Lon", "Lat"))

turnover_stem_man_only_all <- turnover_stem_man_only_all %>%
  mutate(
    diff_unm_base = turn_unm - turn_base,
    diff_tone_base = turn_tone - turn_base,
    diff_tobd_base = turn_tobd - turn_base,
    diff_tobe_base = turn_tobe - turn_base
  )

#saveRDS(turnover_stem_man_only_all, file= "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_stem_man_only_all_values_and_diff_with_base.RData",ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#write.table(turnover_stem_man_only_all, file = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_stem_man_only_all_values_and_diff_with_base")
############################################################################################

####################################
##calculate turnover in 2060-2089 for the different management options

#unmanaged
cpool_man_cc_unm <- getField(source = man_cc_unm, quant= "forest_vegc", first.year = 2060, last.year = 2089)
cflux_man_cc_unm <- getField(source = man_cc_unm, quant="forest_cflux_veg",first.year = 2060, last.year = 2089)

cfluxes_man_cc_unm <- layerOp( x=cflux_man_cc_unm,operator = "+", layers = c("for_harvC",             #sum outer fluxes and store the in a new layer
                                "for_mortC", "for_fireC", "for_distC"),
                                  new.layer = "tot_out_flux")
cpool_man_cc_unm_mean <- aggregateYears(cpool_man_cc_unm, method ="mean")
cfluxes_man_cc_unm_mean <- aggregateYears(cfluxes_man_cc_unm, method ="mean")

turnover_stem_man_cc_unm <- calcNewField( x=cpool_man_cc_unm_mean, y=cfluxes_man_cc_unm_mean,                   #calculate turnover as VegC/outer fluxes
                            x.col="for_stemC", y.col="tot_out_flux", op = "/")

#saveRDS(turnover_stem_man_cc_unm, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_stem_man_cc_unm.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)

#to needleleaf evergreen
cpool_man_cc_to_ne <- getField(source = man_cc_to_ne, quant= "forest_vegc", first.year = 2060, last.year = 2089)
cflux_man_cc_to_ne <- getField(source = man_cc_to_ne, quant="forest_cflux_veg",first.year = 2060, last.year = 2089)
cfluxes_man_cc_to_ne <- layerOp( x=cflux_man_cc_to_ne,operator = "+", layers = c("for_harvC",             #sum outer fluxes and store the in a new layer
                                "for_mortC", "for_fireC", "for_distC"),
                                new.layer = "tot_out_flux")
cpool_man_cc_to_ne_mean <- aggregateYears(cpool_man_cc_to_ne, method ="mean")
cfluxes_man_cc_to_ne_mean <- aggregateYears(cfluxes_man_cc_to_ne, method ="mean")

turnover_stem_man_cc_to_ne<- calcNewField( x=cpool_man_cc_to_ne_mean, y=cfluxes_man_cc_to_ne_mean,                   #calculate turnover as VegC/outer fluxes
                            x.col="for_stemC", y.col="tot_out_flux", op = "/")

#saveRDS(turnover_stem_man_cc_to_ne, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_stem_man_cc_to_ne.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)

#to broadleaf deciduous
cpool_man_cc_to_bd <- getField(source = man_cc_to_bd, quant= "forest_vegc", first.year = 2060, last.year = 2089)
cflux_man_cc_to_bd <- getField(source = man_cc_to_bd, quant="forest_cflux_veg",first.year = 2060, last.year = 2089)
cfluxes_man_cc_to_bd <- layerOp( x=cflux_man_cc_to_bd,operator = "+", layers = c("for_harvC",             #sum outer fluxes and store the in a new layer
                                "for_mortC", "for_fireC", "for_distC"),
                                  new.layer = "tot_out_flux")
cpool_man_cc_to_bd_mean <- aggregateYears(cpool_man_cc_to_bd, method ="mean")
cfluxes_man_cc_to_bd_mean <- aggregateYears(cfluxes_man_cc_to_bd, method ="mean")

turnover_stem_man_cc_to_bd<- calcNewField( x=cpool_man_cc_to_bd_mean, y=cfluxes_man_cc_to_bd_mean,                   #calculate turnover as VegC/outer fluxes
                            x.col="for_stemC", y.col="tot_out_flux", op = "/")

#saveRDS(turnover_stem_man_cc_to_bd, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_stem_man_cc_to_bd.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)

#to broadleaf evergreen
cpool_man_cc_to_be <- getField(source = man_cc_to_be, quant= "forest_vegc", first.year = 2060, last.year = 2089)
cflux_man_cc_to_be <- getField(source = man_cc_to_be, quant="forest_cflux_veg",first.year = 2060, last.year = 2089)
cfluxes_man_cc_to_be <- layerOp( x=cflux_man_cc_to_be,operator = "+", layers = c("for_harvC",             #sum outer fluxes and store the in a new layer
                                "for_mortC", "for_fireC", "for_distC"),
                                  new.layer = "tot_out_flux")
cpool_man_cc_to_be_mean <- aggregateYears(cpool_man_cc_to_be, method ="mean")
cfluxes_man_cc_to_be_mean <- aggregateYears(cfluxes_man_cc_to_be, method ="mean")

turnover_stem_man_cc_to_be<- calcNewField( x=cpool_man_cc_to_be_mean, y=cfluxes_man_cc_to_be_mean,                   #calculate turnover as VegC/outer fluxes
                            x.col="for_stemC", y.col="tot_out_flux", op = "/")

#saveRDS(turnover_stem_man_cc_to_be, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_stem_man_cc_to_be.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)

#business as usual
cpool_man_cc_base <- getField(source = man_cc_base, quant= "forest_vegc", first.year = 2060, last.year = 2089)
cflux_man_cc_base <- getField(source = man_cc_base, quant="forest_cflux_veg",first.year = 2060, last.year = 2089)
cfluxes_man_cc_base <- layerOp( x=cflux_man_cc_base,operator = "+", layers = c("for_harvC",             #sum outer fluxes and store the in a new layer
                                "for_mortC", "for_fireC", "for_distC"),
                                  new.layer = "tot_out_flux")
cpool_man_cc_base_mean <- aggregateYears(cpool_man_cc_base, method ="mean")
cfluxes_man_cc_base_mean <- aggregateYears(cfluxes_man_cc_base, method ="mean")

turnover_stem_man_cc_base<- calcNewField( x=cpool_man_cc_base_mean, y=cfluxes_man_cc_base_mean,                   #calculate turnover as VegC/outer fluxes
                            x.col="for_stemC", y.col="tot_out_flux", op = "/")

#saveRDS(turnover_stem_man_cc_base, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_stem_man_cc_base.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)


################################################################################################
# save fluxes and pools from all FM in a single dataframe
#................. STEM FLUXES ..........................

#outer fluxes:
# - harvest
# - mortality
# - fire
# - disturbance 

#all the output are in the forest_cfluxes file, previously uploaded and called "fire_unm"
# because I needed the fire fluxes from it

fluxes_stem_man_cc_unm <- fire_man_cc_unm[, c(1, 2, 3, 16, 17, 18, 20)]
fluxes_stem_man_cc_base <- fire_man_cc_base[, c(1, 2, 3, 16, 17, 18, 20)]
fluxes_stem_man_cc_to_be <- fire_man_cc_to_be[, c(1, 2, 3, 16, 17, 18, 20)]
fluxes_stem_man_cc_to_ne <- fire_man_cc_to_ne[, c(1, 2, 3, 16, 17, 18, 20)]
fluxes_stem_man_cc_to_bd <- fire_man_cc_to_bd[, c(1, 2, 3, 16, 17, 18, 20)]

#combine the fluxes from each management option in a df
fluxes_stem_man_cc_all <- cbind(fluxes_stem_man_cc_base, fluxes_stem_man_cc_to_bd[,c(4:7)],
                                   fluxes_stem_man_cc_to_be[,c(4:7)], fluxes_stem_man_cc_to_ne[,c(4:7)], fluxes_stem_man_cc_unm[,c(4:7)])
colnames(fluxes_stem_man_cc_all) <- c("Lon", "Lat","Year",
                            "F_harv_base", "F_mort_base","F_fire_base", "F_dist_base",
                            "F_harv_tobd", "F_mort_tobd","F_fire_tobd", "F_dist_tobd",
                            "F_harv_tobe", "F_mort_tobe","F_fire_tobe", "F_dist_tobe",
                            "F_harv_tone", "F_mort_tone","F_fire_tone", "F_dist_tone",
                            "F_harv_unm", "F_mort_unm","F_fire_unm", "F_dist_unm")

#write.table(fluxes_stem_man_cc_all, file = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\LPJG_out\\man_cc\\fluxes_stem_man_cc_all",
 #              row.names = FALSE, col.names = TRUE)

#................. STEM POOLS ..........................

stem_pool_man_cc_unm <- as.data.frame(tot_prod_man_cc_unm)
stem_pool_man_cc_unm <- stem_pool_man_cc_unm[, c(1:3, 9)]

stem_pool_man_cc_base <- as.data.frame(tot_prod_man_cc_base)
stem_pool_man_cc_base <- stem_pool_man_cc_base[, c(1:3, 9)]

stem_pool_man_cc_to_ne <- as.data.frame(tot_prod_man_cc_to_ne)
stem_pool_man_cc_to_ne <- stem_pool_man_cc_to_ne[, c(1:3, 9)]

stem_pool_man_cc_to_bd <- as.data.frame(tot_prod_man_cc_to_bd)
stem_pool_man_cc_to_bd <- stem_pool_man_cc_to_bd[, c(1:3, 9)]

stem_pool_man_cc_to_be <- as.data.frame(tot_prod_man_cc_to_be)
stem_pool_man_cc_to_be <- stem_pool_man_cc_to_be[, c(1:3, 9)]

pools_stem_man_cc_all <- cbind(stem_pool_man_cc_base, stem_pool_man_cc_to_bd[, 4], stem_pool_man_cc_to_be[, 4],
                                 stem_pool_man_cc_to_ne[, 4], stem_pool_man_cc_unm[, 4])
colnames(pools_stem_man_cc_all) <- c("Lon", "Lat","Year",
                            "C_stem_base", "C_stem_tobd", "C_stem_tobe", "C_stem_tone", "C_stem_unm")

#write.table(pools_stem_man_cc_all, file = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\LPJG_out\\man_cc\\pools_stem_man_cc_all",
 #              row.names = FALSE, col.names = TRUE)

#############################################################
# calculate differences between baseline and different FM options
#############################################################
turnover_stem_man_cc_unm <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_stem_man_cc_unm)
turnover_stem_man_cc_to_ne <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_stem_man_cc_to_ne)
turnover_stem_man_cc_to_bd <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_stem_man_cc_to_bd)
turnover_stem_man_cc_to_be <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_stem_man_cc_to_be)
turnover_stem_man_cc_base <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_stem_man_cc_base)

turnover_stem_man_cc_unm_df <- as.data.frame(turnover_stem_man_cc_unm)
colnames(turnover_stem_man_cc_unm_df) <- c("Lon", "Lat", "turn_unm") 
turnover_stem_man_cc_to_ne_df <- as.data.frame(turnover_stem_man_cc_to_ne)
colnames(turnover_stem_man_cc_to_ne_df) <- c("Lon", "Lat", "turn_tone") #change col names not to have spaces
turnover_stem_man_cc_to_bd_df <- as.data.frame(turnover_stem_man_cc_to_bd)
colnames(turnover_stem_man_cc_to_bd_df) <- c("Lon", "Lat", "turn_tobd") #change col names not to have spaces
turnover_stem_man_cc_to_be_df <- as.data.frame(turnover_stem_man_cc_to_be)
colnames(turnover_stem_man_cc_to_be_df) <- c("Lon", "Lat", "turn_tobe") #change col names not to have spaces
turnover_stem_man_cc_base_df <- as.data.frame(turnover_stem_man_cc_base)
colnames(turnover_stem_man_cc_base_df) <- c("Lon", "Lat", "turn_base") #change col names not to have spaces

turnover_stem_man_cc_list <- list(turnover_stem_man_cc_unm_df, 
                                            turnover_stem_man_cc_to_ne_df,
                                            turnover_stem_man_cc_to_bd_df,
                                            turnover_stem_man_cc_to_be_df,
                                            turnover_stem_man_cc_base_df
                                            )
turnover_stem_man_cc_all <- reduce(turnover_stem_man_cc_list, full_join, by = c("Lon", "Lat"))
turnover_stem_man_cc_all <- turnover_stem_man_cc_all %>%
  mutate(
    diff_unm_base = turn_unm - turn_base,
    diff_tone_base = turn_tone - turn_base,
    diff_tobd_base = turn_tobd - turn_base,
    diff_tobe_base = turn_tobe - turn_base
  )

#saveRDS(turnover_stem_man_cc_all, file= "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_stem_man_cc_all_values_and_diff_with_base.RData",ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#write.table(turnover_stem_man_cc_all, file = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_stem_man_cc_all_values_and_diff_with_base")
############################################################################################

            #**************
            # ** SOIL **
            #**************
#################################################
#.............. MANAGEMENT ONLY .................
#################################################
###################
#....unmanaged ....
###################
tot_cpool_man_only_unm <- getField(source = man_only_unm, quant = "cpool_forest", first.year = 2220, last.year = 2249)

soil_pools_man_only_unm_NE <- getField(source = man_only_unm, quant = "cpool_all_ForestNE", first.year = 2220, last.year = 2249)
soil_pools_man_only_unm_BE <- getField(source = man_only_unm, quant = "cpool_all_ForestBE", first.year = 2220, last.year = 2249)
soil_pools_man_only_unm_BD <- getField(source = man_only_unm, quant = "cpool_all_ForestBD", first.year = 2220, last.year = 2249)
soil_pools_man_only_unm_ND <- getField(source = man_only_unm, quant = "cpool_all_ForestND", first.year = 2220, last.year = 2249)

soil_fluxes_man_only_unm_NE <- getField(source = man_only_unm, quant = "cflux_all_ForestNE", first.year = 2220, last.year = 2249)
soil_fluxes_man_only_unm_BE <- getField(source = man_only_unm, quant = "cflux_all_ForestBE", first.year = 2220, last.year = 2249)
soil_fluxes_man_only_unm_BD <- getField(source = man_only_unm, quant = "cflux_all_ForestBD", first.year = 2220, last.year = 2249)
soil_fluxes_man_only_unm_ND <- getField(source = man_only_unm, quant = "cflux_all_ForestND", first.year = 2220, last.year = 2249)
#................................
#...... TOTAL SOIL POOL .........
#................................
tot_soil_fluxes_man_only_unm_NE <- layerOp( x=soil_fluxes_man_only_unm_NE, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")
tot_soil_fluxes_man_only_unm_ND <- layerOp( x=soil_fluxes_man_only_unm_ND, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")
tot_soil_fluxes_man_only_unm_BD <- layerOp( x=soil_fluxes_man_only_unm_BD, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")
tot_soil_fluxes_man_only_unm_BE <- layerOp( x=soil_fluxes_man_only_unm_BE, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")
tot_soil_fluxes_man_only_unm_NE_ND <- calcNewField(x=tot_soil_fluxes_man_only_unm_NE, y=tot_soil_fluxes_man_only_unm_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
tot_soil_fluxes_man_only_unm_BE_BD <- calcNewField(x=tot_soil_fluxes_man_only_unm_BE, y=tot_soil_fluxes_man_only_unm_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
tot_soil_fluxes_man_only_unm_forest <- calcNewField(x=tot_soil_fluxes_man_only_unm_NE_ND, y=tot_soil_fluxes_man_only_unm_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
tot_soil_fluxes_man_only_unm_forest_mean <- aggregateYears(tot_soil_fluxes_man_only_unm_forest, method = "mean" )

tot_pools_man_only_unm_forest_mean <- aggregateYears(tot_cpool_man_only_unm, method = "mean" )

turnover_tot_soil_man_only_unm <- calcNewField( x=tot_pools_man_only_unm_forest_mean, y=tot_soil_fluxes_man_only_unm_forest_mean,
                            x.col="SoilC", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_tot_soil_man_only_unm, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_tot_soil_man_only_unm.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#.....................
#... SURF FWD POOL ...
#.....................
surffwd_fluxes_man_only_unm_NE <- layerOp( x=soil_fluxes_man_only_unm_NE, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")
surffwd_fluxes_man_only_unm_ND <- layerOp( x=soil_fluxes_man_only_unm_ND, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")
surffwd_fluxes_man_only_unm_BD <- layerOp( x=soil_fluxes_man_only_unm_BD, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")
surffwd_fluxes_man_only_unm_BE <- layerOp( x=soil_fluxes_man_only_unm_BE, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")
surffwd_fluxes_man_only_unm_NE_ND <- calcNewField(x=surffwd_fluxes_man_only_unm_NE, y=surffwd_fluxes_man_only_unm_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surffwd_fluxes_man_only_unm_BE_BD <- calcNewField(x=surffwd_fluxes_man_only_unm_BE, y=surffwd_fluxes_man_only_unm_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surffwd_fluxes_man_only_unm_forest <- calcNewField(x=surffwd_fluxes_man_only_unm_NE_ND, y=surffwd_fluxes_man_only_unm_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surffwd_fluxes_man_only_unm_forest_mean <- aggregateYears(surffwd_fluxes_man_only_unm_forest, method = "mean" )

surffwd_pools_man_only_unm_NE_ND <- calcNewField(x=soil_pools_man_only_unm_NE, y=soil_pools_man_only_unm_ND, x.col = "SURFFWD", y.col = "SURFFWD", op="+" )
surffwd_pools_man_only_unm_BE_BD <- calcNewField(x=soil_pools_man_only_unm_BE, y=soil_pools_man_only_unm_BD, x.col = "SURFFWD", y.col = "SURFFWD", op="+" )
surffwd_pools_man_only_unm_forest <- calcNewField(x=surffwd_pools_man_only_unm_NE_ND, y=surffwd_pools_man_only_unm_BE_BD, x.col = "SURFFWD", y.col = "SURFFWD", op="+" )
surffwd_pools_man_only_unm_forest_mean <- aggregateYears(surffwd_pools_man_only_unm_forest, method = "mean" )

turnover_surffwd_man_only_unm <- calcNewField( x=surffwd_pools_man_only_unm_forest_mean, y=surffwd_fluxes_man_only_unm_forest_mean,
                            x.col="SURFFWD", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_surffwd_man_only_unm, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_surffwd_man_only_unm.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#.....................
#... SURF CWD POOL ...
#.....................
surfcwd_fluxes_man_only_unm_NE <- layerOp( x=soil_fluxes_man_only_unm_NE, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")
surfcwd_fluxes_man_only_unm_ND <- layerOp( x=soil_fluxes_man_only_unm_ND, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")
surfcwd_fluxes_man_only_unm_BD <- layerOp( x=soil_fluxes_man_only_unm_BD, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")
surfcwd_fluxes_man_only_unm_BE <- layerOp( x=soil_fluxes_man_only_unm_BE, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")

surfcwd_fluxes_man_only_unm_NE_ND <- calcNewField(x=surfcwd_fluxes_man_only_unm_NE, y=surfcwd_fluxes_man_only_unm_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfcwd_fluxes_man_only_unm_BE_BD <- calcNewField(x=surfcwd_fluxes_man_only_unm_BE, y=surfcwd_fluxes_man_only_unm_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfcwd_fluxes_man_only_unm_forest <- calcNewField(x=surfcwd_fluxes_man_only_unm_NE_ND, y=surfcwd_fluxes_man_only_unm_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfcwd_fluxes_man_only_unm_forest_mean <- aggregateYears(surfcwd_fluxes_man_only_unm_forest, method = "mean" )

surfcwd_pools_man_only_unm_NE_ND <- calcNewField(x=soil_pools_man_only_unm_NE, y=soil_pools_man_only_unm_ND, x.col = "SURFCWD", y.col = "SURFCWD", op="+" )
surfcwd_pools_man_only_unm_BE_BD <- calcNewField(x=soil_pools_man_only_unm_BE, y=soil_pools_man_only_unm_BD, x.col = "SURFCWD", y.col = "SURFCWD", op="+" )
surfcwd_pools_man_only_unm_forest <- calcNewField(x=surfcwd_pools_man_only_unm_NE_ND, y=surfcwd_pools_man_only_unm_BE_BD, x.col = "SURFCWD", y.col = "SURFCWD", op="+" )
surfcwd_pools_man_only_unm_forest_mean <- aggregateYears(surfcwd_pools_man_only_unm_forest, method = "mean" )

turnover_surfcwd_man_only_unm <- calcNewField( x=surfcwd_pools_man_only_unm_forest_mean, y=surfcwd_fluxes_man_only_unm_forest_mean,
                            x.col="SURFCWD", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_surfcwd_man_only_unm, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_surfcwd_man_only_unm.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#....................
#.... SURF HUMUS ....
#....................
surfhum_fluxes_man_only_unm_NE <- layerOp( x=soil_fluxes_man_only_unm_NE, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")
surfhum_fluxes_man_only_unm_ND <- layerOp( x=soil_fluxes_man_only_unm_ND, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")
surfhum_fluxes_man_only_unm_BD <- layerOp( x=soil_fluxes_man_only_unm_BD, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")
surfhum_fluxes_man_only_unm_BE <- layerOp( x=soil_fluxes_man_only_unm_BE, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")

surfhum_fluxes_man_only_unm_NE_ND <- calcNewField(x=surfhum_fluxes_man_only_unm_NE, y=surfhum_fluxes_man_only_unm_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfhum_fluxes_man_only_unm_BE_BD <- calcNewField(x=surfhum_fluxes_man_only_unm_BE, y=surfhum_fluxes_man_only_unm_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfhum_fluxes_man_only_unm_forest <- calcNewField(x=surfhum_fluxes_man_only_unm_NE_ND, y=surfhum_fluxes_man_only_unm_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfhum_fluxes_man_only_unm_forest_mean <- aggregateYears(surfhum_fluxes_man_only_unm_forest, method = "mean" )

surfhum_pools_man_only_unm_NE_ND <- calcNewField(x=soil_pools_man_only_unm_NE, y=soil_pools_man_only_unm_ND, x.col = "SURFHUMUS", y.col = "SURFHUMUS", op="+" )
surfhum_pools_man_only_unm_BE_BD <- calcNewField(x=soil_pools_man_only_unm_BE, y=soil_pools_man_only_unm_BD, x.col = "SURFHUMUS", y.col = "SURFHUMUS", op="+" )
surfhum_pools_man_only_unm_forest <- calcNewField(x=surfhum_pools_man_only_unm_NE_ND, y=surfhum_pools_man_only_unm_BE_BD, x.col = "SURFHUMUS", y.col = "SURFHUMUS", op="+" )
surfhum_pools_man_only_unm_forest_mean <- aggregateYears(surfhum_pools_man_only_unm_forest, method = "mean" )

turnover_surfhum_man_only_unm <- calcNewField( x=surfhum_pools_man_only_unm_forest_mean, y=surfhum_fluxes_man_only_unm_forest_mean,
                            x.col="SURFHUMUS", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_surfhum_man_only_unm, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_surfhum_man_only_unm.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#.........................
#..... SLOW SOM POOL .....
#.........................
slowsom_fluxes_man_only_unm_NE <- layerOp( x=soil_fluxes_man_only_unm_NE, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")
slowsom_fluxes_man_only_unm_ND <- layerOp( x=soil_fluxes_man_only_unm_ND, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")
slowsom_fluxes_man_only_unm_BD <- layerOp( x=soil_fluxes_man_only_unm_BD, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")
slowsom_fluxes_man_only_unm_BE <- layerOp( x=soil_fluxes_man_only_unm_BE, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")

slowsom_fluxes_man_only_unm_NE_ND <- calcNewField(x=slowsom_fluxes_man_only_unm_NE, y=slowsom_fluxes_man_only_unm_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
slowsom_fluxes_man_only_unm_BE_BD <- calcNewField(x=slowsom_fluxes_man_only_unm_BE, y=slowsom_fluxes_man_only_unm_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
slowsom_fluxes_man_only_unm_forest <- calcNewField(x=slowsom_fluxes_man_only_unm_NE_ND, y=slowsom_fluxes_man_only_unm_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
slowsom_fluxes_man_only_unm_forest_mean <- aggregateYears(slowsom_fluxes_man_only_unm_forest, method = "mean" )

slowsom_pools_man_only_unm_NE_ND <- calcNewField(x=soil_pools_man_only_unm_NE, y=soil_pools_man_only_unm_ND, x.col = "SLOWSOM", y.col = "SLOWSOM", op="+" )
slowsom_pools_man_only_unm_BE_BD <- calcNewField(x=soil_pools_man_only_unm_BE, y=soil_pools_man_only_unm_BD, x.col = "SLOWSOM", y.col = "SLOWSOM", op="+" )
slowsom_pools_man_only_unm_forest <- calcNewField(x=slowsom_pools_man_only_unm_NE_ND, y=slowsom_pools_man_only_unm_BE_BD, x.col = "SLOWSOM", y.col = "SLOWSOM", op="+" )
slowsom_pools_man_only_unm_forest_mean <- aggregateYears(slowsom_pools_man_only_unm_forest, method = "mean" )

turnover_slowsom_man_only_unm <- calcNewField( x=slowsom_pools_man_only_unm_forest_mean, y=slowsom_fluxes_man_only_unm_forest_mean,
                            x.col="SLOWSOM", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_slowsom_man_only_unm, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_slowsom_man_only_unm.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)

########################
#....... to NE .........
#########################
tot_cpool_man_only_to_ne <- getField(source = man_only_to_ne, quant = "cpool_forest", first.year = 2220, last.year = 2249)

soil_pools_man_only_to_ne_NE <- getField(source = man_only_to_ne, quant = "cpool_all_ForestNE", first.year = 2220, last.year = 2249)
soil_pools_man_only_to_ne_BE <- getField(source = man_only_to_ne, quant = "cpool_all_ForestBE", first.year = 2220, last.year = 2249)
soil_pools_man_only_to_ne_BD <- getField(source = man_only_to_ne, quant = "cpool_all_ForestBD", first.year = 2220, last.year = 2249)
soil_pools_man_only_to_ne_ND <- getField(source = man_only_to_ne, quant = "cpool_all_ForestND", first.year = 2220, last.year = 2249)

soil_fluxes_man_only_to_ne_NE <- getField(source = man_only_to_ne, quant = "cflux_all_ForestNE", first.year = 2220, last.year = 2249)
soil_fluxes_man_only_to_ne_BE <- getField(source = man_only_to_ne, quant = "cflux_all_ForestBE", first.year = 2220, last.year = 2249)
soil_fluxes_man_only_to_ne_BD <- getField(source = man_only_to_ne, quant = "cflux_all_ForestBD", first.year = 2220, last.year = 2249)
soil_fluxes_man_only_to_ne_ND <- getField(source = man_only_to_ne, quant = "cflux_all_ForestND", first.year = 2220, last.year = 2249)
#.......................
#... TOTAL SOIL POOL ...
#.......................
tot_soil_fluxes_man_only_to_ne_NE <- layerOp( x=soil_fluxes_man_only_to_ne_NE, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")
tot_soil_fluxes_man_only_to_ne_ND <- layerOp( x=soil_fluxes_man_only_to_ne_ND, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")
tot_soil_fluxes_man_only_to_ne_BD <- layerOp( x=soil_fluxes_man_only_to_ne_BD, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")
tot_soil_fluxes_man_only_to_ne_BE <- layerOp( x=soil_fluxes_man_only_to_ne_BE, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")

tot_soil_fluxes_man_only_to_ne_NE_ND <- calcNewField(x=tot_soil_fluxes_man_only_to_ne_NE, y=tot_soil_fluxes_man_only_to_ne_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
tot_soil_fluxes_man_only_to_ne_BE_BD <- calcNewField(x=tot_soil_fluxes_man_only_to_ne_BE, y=tot_soil_fluxes_man_only_to_ne_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
tot_soil_fluxes_man_only_to_ne_forest <- calcNewField(x=tot_soil_fluxes_man_only_to_ne_NE_ND, y=tot_soil_fluxes_man_only_to_ne_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
tot_soil_fluxes_man_only_to_ne_forest_mean <- aggregateYears(tot_soil_fluxes_man_only_to_ne_forest, method = "mean" )

tot_pools_man_only_to_ne_forest_mean <- aggregateYears(tot_cpool_man_only_to_ne, method = "mean" )

turnover_tot_soil_man_only_to_ne <- calcNewField( x=tot_pools_man_only_to_ne_forest_mean, y=tot_soil_fluxes_man_only_to_ne_forest_mean,
                            x.col="SoilC", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_tot_soil_man_only_to_ne, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_tot_soil_man_only_to_ne.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#.....................
#... SURF FWD POOL ...
#.....................
surffwd_fluxes_man_only_to_ne_NE <- layerOp( x=soil_fluxes_man_only_to_ne_NE, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")
surffwd_fluxes_man_only_to_ne_ND <- layerOp( x=soil_fluxes_man_only_to_ne_ND, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")
surffwd_fluxes_man_only_to_ne_BD <- layerOp( x=soil_fluxes_man_only_to_ne_BD, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")
surffwd_fluxes_man_only_to_ne_BE <- layerOp( x=soil_fluxes_man_only_to_ne_BE, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")

surffwd_fluxes_man_only_to_ne_NE_ND <- calcNewField(x=surffwd_fluxes_man_only_to_ne_NE, y=surffwd_fluxes_man_only_to_ne_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surffwd_fluxes_man_only_to_ne_BE_BD <- calcNewField(x=surffwd_fluxes_man_only_to_ne_BE, y=surffwd_fluxes_man_only_to_ne_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surffwd_fluxes_man_only_to_ne_forest <- calcNewField(x=surffwd_fluxes_man_only_to_ne_NE_ND, y=surffwd_fluxes_man_only_to_ne_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surffwd_fluxes_man_only_to_ne_forest_mean <- aggregateYears(surffwd_fluxes_man_only_to_ne_forest, method = "mean" )

surffwd_pools_man_only_to_ne_NE_ND <- calcNewField(x=soil_pools_man_only_to_ne_NE, y=soil_pools_man_only_to_ne_ND, x.col = "SURFFWD", y.col = "SURFFWD", op="+" )
surffwd_pools_man_only_to_ne_BE_BD <- calcNewField(x=soil_pools_man_only_to_ne_BE, y=soil_pools_man_only_to_ne_BD, x.col = "SURFFWD", y.col = "SURFFWD", op="+" )
surffwd_pools_man_only_to_ne_forest <- calcNewField(x=surffwd_pools_man_only_to_ne_NE_ND, y=surffwd_pools_man_only_to_ne_BE_BD, x.col = "SURFFWD", y.col = "SURFFWD", op="+" )
surffwd_pools_man_only_to_ne_forest_mean <- aggregateYears(surffwd_pools_man_only_to_ne_forest, method = "mean" )

turnover_surffwd_man_only_to_ne <- calcNewField( x=surffwd_pools_man_only_to_ne_forest_mean, y=surffwd_fluxes_man_only_to_ne_forest_mean,
                            x.col="SURFFWD", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_surffwd_man_only_to_ne, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_surffwd_man_only_to_ne.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#.....................
#... SURF CWD POOL ...
#.....................
surfcwd_fluxes_man_only_to_ne_NE <- layerOp( x=soil_fluxes_man_only_to_ne_NE, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")
surfcwd_fluxes_man_only_to_ne_ND <- layerOp( x=soil_fluxes_man_only_to_ne_ND, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")
surfcwd_fluxes_man_only_to_ne_BD <- layerOp( x=soil_fluxes_man_only_to_ne_BD, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")
surfcwd_fluxes_man_only_to_ne_BE <- layerOp( x=soil_fluxes_man_only_to_ne_BE, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")

surfcwd_fluxes_man_only_to_ne_NE_ND <- calcNewField(x=surfcwd_fluxes_man_only_to_ne_NE, y=surfcwd_fluxes_man_only_to_ne_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfcwd_fluxes_man_only_to_ne_BE_BD <- calcNewField(x=surfcwd_fluxes_man_only_to_ne_BE, y=surfcwd_fluxes_man_only_to_ne_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfcwd_fluxes_man_only_to_ne_forest <- calcNewField(x=surfcwd_fluxes_man_only_to_ne_NE_ND, y=surfcwd_fluxes_man_only_to_ne_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfcwd_fluxes_man_only_to_ne_forest_mean <- aggregateYears(surfcwd_fluxes_man_only_to_ne_forest, method = "mean" )

surfcwd_pools_man_only_to_ne_NE_ND <- calcNewField(x=soil_pools_man_only_to_ne_NE, y=soil_pools_man_only_to_ne_ND, x.col = "SURFCWD", y.col = "SURFCWD", op="+" )
surfcwd_pools_man_only_to_ne_BE_BD <- calcNewField(x=soil_pools_man_only_to_ne_BE, y=soil_pools_man_only_to_ne_BD, x.col = "SURFCWD", y.col = "SURFCWD", op="+" )
surfcwd_pools_man_only_to_ne_forest <- calcNewField(x=surfcwd_pools_man_only_to_ne_NE_ND, y=surfcwd_pools_man_only_to_ne_BE_BD, x.col = "SURFCWD", y.col = "SURFCWD", op="+" )
surfcwd_pools_man_only_to_ne_forest_mean <- aggregateYears(surfcwd_pools_man_only_to_ne_forest, method = "mean" )

turnover_surfcwd_man_only_to_ne <- calcNewField( x=surfcwd_pools_man_only_to_ne_forest_mean, y=surfcwd_fluxes_man_only_to_ne_forest_mean,
                            x.col="SURFCWD", y.col="tot_out_flux", op = "/")

#saveRDS(turnover_surfcwd_man_only_to_ne, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_surfcwd_man_only_to_ne.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)

#..................
#... SURF HUMUS ...
#..................
surfhum_fluxes_man_only_to_ne_NE <- layerOp( x=soil_fluxes_man_only_to_ne_NE, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")
surfhum_fluxes_man_only_to_ne_ND <- layerOp( x=soil_fluxes_man_only_to_ne_ND, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")
surfhum_fluxes_man_only_to_ne_BD <- layerOp( x=soil_fluxes_man_only_to_ne_BD, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")
surfhum_fluxes_man_only_to_ne_BE <- layerOp( x=soil_fluxes_man_only_to_ne_BE, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")

surfhum_fluxes_man_only_to_ne_NE_ND <- calcNewField(x=surfhum_fluxes_man_only_to_ne_NE, y=surfhum_fluxes_man_only_to_ne_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfhum_fluxes_man_only_to_ne_BE_BD <- calcNewField(x=surfhum_fluxes_man_only_to_ne_BE, y=surfhum_fluxes_man_only_to_ne_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfhum_fluxes_man_only_to_ne_forest <- calcNewField(x=surfhum_fluxes_man_only_to_ne_NE_ND, y=surfhum_fluxes_man_only_to_ne_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfhum_fluxes_man_only_to_ne_forest_mean <- aggregateYears(surfhum_fluxes_man_only_to_ne_forest, method = "mean" )

surfhum_pools_man_only_to_ne_NE_ND <- calcNewField(x=soil_pools_man_only_to_ne_NE, y=soil_pools_man_only_to_ne_ND, x.col = "SURFHUMUS", y.col = "SURFHUMUS", op="+" )
surfhum_pools_man_only_to_ne_BE_BD <- calcNewField(x=soil_pools_man_only_to_ne_BE, y=soil_pools_man_only_to_ne_BD, x.col = "SURFHUMUS", y.col = "SURFHUMUS", op="+" )
surfhum_pools_man_only_to_ne_forest <- calcNewField(x=surfhum_pools_man_only_to_ne_NE_ND, y=surfhum_pools_man_only_to_ne_BE_BD, x.col = "SURFHUMUS", y.col = "SURFHUMUS", op="+" )
surfhum_pools_man_only_to_ne_forest_mean <- aggregateYears(surfhum_pools_man_only_to_ne_forest, method = "mean" )

turnover_surfhum_man_only_to_ne <- calcNewField( x=surfhum_pools_man_only_to_ne_forest_mean, y=surfhum_fluxes_man_only_to_ne_forest_mean,
                            x.col="SURFHUMUS", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_surfhum_man_only_to_ne, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_surfhum_man_only_to_ne.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)

#.....................
#... SLOW SOM POOL ...
#.....................
slowsom_fluxes_man_only_to_ne_NE <- layerOp( x=soil_fluxes_man_only_to_ne_NE, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")
slowsom_fluxes_man_only_to_ne_ND <- layerOp( x=soil_fluxes_man_only_to_ne_ND, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")
slowsom_fluxes_man_only_to_ne_BD <- layerOp( x=soil_fluxes_man_only_to_ne_BD, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")
slowsom_fluxes_man_only_to_ne_BE <- layerOp( x=soil_fluxes_man_only_to_ne_BE, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")
slowsom_fluxes_man_only_to_ne_NE_ND <- calcNewField(x=slowsom_fluxes_man_only_to_ne_NE, y=slowsom_fluxes_man_only_to_ne_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
slowsom_fluxes_man_only_to_ne_BE_BD <- calcNewField(x=slowsom_fluxes_man_only_to_ne_BE, y=slowsom_fluxes_man_only_to_ne_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
slowsom_fluxes_man_only_to_ne_forest <- calcNewField(x=slowsom_fluxes_man_only_to_ne_NE_ND, y=slowsom_fluxes_man_only_to_ne_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
slowsom_fluxes_man_only_to_ne_forest_mean <- aggregateYears(slowsom_fluxes_man_only_to_ne_forest, method = "mean" )

slowsom_pools_man_only_to_ne_NE_ND <- calcNewField(x=soil_pools_man_only_to_ne_NE, y=soil_pools_man_only_to_ne_ND, x.col = "SLOWSOM", y.col = "SLOWSOM", op="+" )
slowsom_pools_man_only_to_ne_BE_BD <- calcNewField(x=soil_pools_man_only_to_ne_BE, y=soil_pools_man_only_to_ne_BD, x.col = "SLOWSOM", y.col = "SLOWSOM", op="+" )
slowsom_pools_man_only_to_ne_forest <- calcNewField(x=slowsom_pools_man_only_to_ne_NE_ND, y=slowsom_pools_man_only_to_ne_BE_BD, x.col = "SLOWSOM", y.col = "SLOWSOM", op="+" )
slowsom_pools_man_only_to_ne_forest_mean <- aggregateYears(slowsom_pools_man_only_to_ne_forest, method = "mean" )

turnover_slowsom_man_only_to_ne <- calcNewField( x=slowsom_pools_man_only_to_ne_forest_mean, y=slowsom_fluxes_man_only_to_ne_forest_mean,
                            x.col="SLOWSOM", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_slowsom_man_only_to_ne, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_slowsom_man_only_to_ne.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)

##################
#..... to BD .....
##################
tot_cpool_man_only_to_bd <- getField(source = man_only_to_bd, quant = "cpool_forest", first.year = 2220, last.year = 2249)

soil_pools_man_only_to_bd_NE <- getField(source = man_only_to_bd, quant = "cpool_all_ForestNE", first.year = 2220, last.year = 2249)
soil_pools_man_only_to_bd_BE <- getField(source = man_only_to_bd, quant = "cpool_all_ForestBE", first.year = 2220, last.year = 2249)
soil_pools_man_only_to_bd_BD <- getField(source = man_only_to_bd, quant = "cpool_all_ForestBD", first.year = 2220, last.year = 2249)
soil_pools_man_only_to_bd_ND <- getField(source = man_only_to_bd, quant = "cpool_all_ForestND", first.year = 2220, last.year = 2249)
soil_fluxes_man_only_to_bd_NE <- getField(source = man_only_to_bd, quant = "cflux_all_ForestNE", first.year = 2220, last.year = 2249)
soil_fluxes_man_only_to_bd_BE <- getField(source = man_only_to_bd, quant = "cflux_all_ForestBE", first.year = 2220, last.year = 2249)
soil_fluxes_man_only_to_bd_BD <- getField(source = man_only_to_bd, quant = "cflux_all_ForestBD", first.year = 2220, last.year = 2249)
soil_fluxes_man_only_to_bd_ND <- getField(source = man_only_to_bd, quant = "cflux_all_ForestND", first.year = 2220, last.year = 2249)

#........................
#....TOTAL SOIL POOL ....
#........................
tot_soil_fluxes_man_only_to_bd_NE <- layerOp( x=soil_fluxes_man_only_to_bd_NE, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")
tot_soil_fluxes_man_only_to_bd_ND <- layerOp( x=soil_fluxes_man_only_to_bd_ND, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")
tot_soil_fluxes_man_only_to_bd_BD <- layerOp( x=soil_fluxes_man_only_to_bd_BD, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")
tot_soil_fluxes_man_only_to_bd_BE <- layerOp( x=soil_fluxes_man_only_to_bd_BE, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")

tot_soil_fluxes_man_only_to_bd_NE_ND <- calcNewField(x=tot_soil_fluxes_man_only_to_bd_NE, y=tot_soil_fluxes_man_only_to_bd_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
tot_soil_fluxes_man_only_to_bd_BE_BD <- calcNewField(x=tot_soil_fluxes_man_only_to_bd_BE, y=tot_soil_fluxes_man_only_to_bd_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
tot_soil_fluxes_man_only_to_bd_forest <- calcNewField(x=tot_soil_fluxes_man_only_to_bd_NE_ND, y=tot_soil_fluxes_man_only_to_bd_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
tot_soil_fluxes_man_only_to_bd_forest_mean <- aggregateYears(tot_soil_fluxes_man_only_to_bd_forest, method = "mean" )

tot_pools_man_only_to_bd_forest_mean <- aggregateYears(tot_cpool_man_only_to_bd, method = "mean" )

turnover_tot_soil_man_only_to_bd <- calcNewField( x=tot_pools_man_only_to_bd_forest_mean, y=tot_soil_fluxes_man_only_to_bd_forest_mean,
                            x.col="SoilC", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_tot_soil_man_only_to_bd, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_tot_soil_man_only_to_bd.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#.....................
#... SURF FWD POOL ...
#.....................
surffwd_fluxes_man_only_to_bd_NE <- layerOp( x=soil_fluxes_man_only_to_bd_NE, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")
surffwd_fluxes_man_only_to_bd_ND <- layerOp( x=soil_fluxes_man_only_to_bd_ND, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")
surffwd_fluxes_man_only_to_bd_BD <- layerOp( x=soil_fluxes_man_only_to_bd_BD, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")
surffwd_fluxes_man_only_to_bd_BE <- layerOp( x=soil_fluxes_man_only_to_bd_BE, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")

surffwd_fluxes_man_only_to_bd_NE_ND <- calcNewField(x=surffwd_fluxes_man_only_to_bd_NE, y=surffwd_fluxes_man_only_to_bd_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surffwd_fluxes_man_only_to_bd_BE_BD <- calcNewField(x=surffwd_fluxes_man_only_to_bd_BE, y=surffwd_fluxes_man_only_to_bd_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surffwd_fluxes_man_only_to_bd_forest <- calcNewField(x=surffwd_fluxes_man_only_to_bd_NE_ND, y=surffwd_fluxes_man_only_to_bd_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surffwd_fluxes_man_only_to_bd_forest_mean <- aggregateYears(surffwd_fluxes_man_only_to_bd_forest, method = "mean" )

surffwd_pools_man_only_to_bd_NE_ND <- calcNewField(x=soil_pools_man_only_to_bd_NE, y=soil_pools_man_only_to_bd_ND, x.col = "SURFFWD", y.col = "SURFFWD", op="+" )
surffwd_pools_man_only_to_bd_BE_BD <- calcNewField(x=soil_pools_man_only_to_bd_BE, y=soil_pools_man_only_to_bd_BD, x.col = "SURFFWD", y.col = "SURFFWD", op="+" )
surffwd_pools_man_only_to_bd_forest <- calcNewField(x=surffwd_pools_man_only_to_bd_NE_ND, y=surffwd_pools_man_only_to_bd_BE_BD, x.col = "SURFFWD", y.col = "SURFFWD", op="+" )
surffwd_pools_man_only_to_bd_forest_mean <- aggregateYears(surffwd_pools_man_only_to_bd_forest, method = "mean" )

turnover_surffwd_man_only_to_bd <- calcNewField( x=surffwd_pools_man_only_to_bd_forest_mean, y=surffwd_fluxes_man_only_to_bd_forest_mean,
                            x.col="SURFFWD", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_surffwd_man_only_to_bd, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_surffwd_man_only_to_bd.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#.....................
#... SURF CWD POOL ...
#.....................
surfcwd_fluxes_man_only_to_bd_NE <- layerOp( x=soil_fluxes_man_only_to_bd_NE, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")
surfcwd_fluxes_man_only_to_bd_ND <- layerOp( x=soil_fluxes_man_only_to_bd_ND, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")
surfcwd_fluxes_man_only_to_bd_BD <- layerOp( x=soil_fluxes_man_only_to_bd_BD, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")
surfcwd_fluxes_man_only_to_bd_BE <- layerOp( x=soil_fluxes_man_only_to_bd_BE, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")

surfcwd_fluxes_man_only_to_bd_NE_ND <- calcNewField(x=surfcwd_fluxes_man_only_to_bd_NE, y=surfcwd_fluxes_man_only_to_bd_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfcwd_fluxes_man_only_to_bd_BE_BD <- calcNewField(x=surfcwd_fluxes_man_only_to_bd_BE, y=surfcwd_fluxes_man_only_to_bd_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfcwd_fluxes_man_only_to_bd_forest <- calcNewField(x=surfcwd_fluxes_man_only_to_bd_NE_ND, y=surfcwd_fluxes_man_only_to_bd_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfcwd_fluxes_man_only_to_bd_forest_mean <- aggregateYears(surfcwd_fluxes_man_only_to_bd_forest, method = "mean" )

surfcwd_pools_man_only_to_bd_NE_ND <- calcNewField(x=soil_pools_man_only_to_bd_NE, y=soil_pools_man_only_to_bd_ND, x.col = "SURFCWD", y.col = "SURFCWD", op="+" )
surfcwd_pools_man_only_to_bd_BE_BD <- calcNewField(x=soil_pools_man_only_to_bd_BE, y=soil_pools_man_only_to_bd_BD, x.col = "SURFCWD", y.col = "SURFCWD", op="+" )
surfcwd_pools_man_only_to_bd_forest <- calcNewField(x=surfcwd_pools_man_only_to_bd_NE_ND, y=surfcwd_pools_man_only_to_bd_BE_BD, x.col = "SURFCWD", y.col = "SURFCWD", op="+" )
surfcwd_pools_man_only_to_bd_forest_mean <- aggregateYears(surfcwd_pools_man_only_to_bd_forest, method = "mean" )

turnover_surfcwd_man_only_to_bd <- calcNewField( x=surfcwd_pools_man_only_to_bd_forest_mean, y=surfcwd_fluxes_man_only_to_bd_forest_mean,
                            x.col="SURFCWD", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_surfcwd_man_only_to_bd, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_surfcwd_man_only_to_bd.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#..................
#... SURF HUMUS ...
#..................
surfhum_fluxes_man_only_to_bd_NE <- layerOp( x=soil_fluxes_man_only_to_bd_NE, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")
surfhum_fluxes_man_only_to_bd_ND <- layerOp( x=soil_fluxes_man_only_to_bd_ND, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")
surfhum_fluxes_man_only_to_bd_BD <- layerOp( x=soil_fluxes_man_only_to_bd_BD, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")
surfhum_fluxes_man_only_to_bd_BE <- layerOp( x=soil_fluxes_man_only_to_bd_BE, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")

surfhum_fluxes_man_only_to_bd_NE_ND <- calcNewField(x=surfhum_fluxes_man_only_to_bd_NE, y=surfhum_fluxes_man_only_to_bd_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfhum_fluxes_man_only_to_bd_BE_BD <- calcNewField(x=surfhum_fluxes_man_only_to_bd_BE, y=surfhum_fluxes_man_only_to_bd_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfhum_fluxes_man_only_to_bd_forest <- calcNewField(x=surfhum_fluxes_man_only_to_bd_NE_ND, y=surfhum_fluxes_man_only_to_bd_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfhum_fluxes_man_only_to_bd_forest_mean <- aggregateYears(surfhum_fluxes_man_only_to_bd_forest, method = "mean" )

surfhum_pools_man_only_to_bd_NE_ND <- calcNewField(x=soil_pools_man_only_to_bd_NE, y=soil_pools_man_only_to_bd_ND, x.col = "SURFHUMUS", y.col = "SURFHUMUS", op="+" )
surfhum_pools_man_only_to_bd_BE_BD <- calcNewField(x=soil_pools_man_only_to_bd_BE, y=soil_pools_man_only_to_bd_BD, x.col = "SURFHUMUS", y.col = "SURFHUMUS", op="+" )
surfhum_pools_man_only_to_bd_forest <- calcNewField(x=surfhum_pools_man_only_to_bd_NE_ND, y=surfhum_pools_man_only_to_bd_BE_BD, x.col = "SURFHUMUS", y.col = "SURFHUMUS", op="+" )
surfhum_pools_man_only_to_bd_forest_mean <- aggregateYears(surfhum_pools_man_only_to_bd_forest, method = "mean" )

turnover_surfhum_man_only_to_bd <- calcNewField( x=surfhum_pools_man_only_to_bd_forest_mean, y=surfhum_fluxes_man_only_to_bd_forest_mean,
                            x.col="SURFHUMUS", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_surfhum_man_only_to_bd, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_surfhum_man_only_to_bd.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#.....................
#... SLOW SOM POOL ...
#.....................
slowsom_fluxes_man_only_to_bd_NE <- layerOp( x=soil_fluxes_man_only_to_bd_NE, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")
slowsom_fluxes_man_only_to_bd_ND <- layerOp( x=soil_fluxes_man_only_to_bd_ND, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")
slowsom_fluxes_man_only_to_bd_BD <- layerOp( x=soil_fluxes_man_only_to_bd_BD, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")
slowsom_fluxes_man_only_to_bd_BE <- layerOp( x=soil_fluxes_man_only_to_bd_BE, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")

slowsom_fluxes_man_only_to_bd_NE_ND <- calcNewField(x=slowsom_fluxes_man_only_to_bd_NE, y=slowsom_fluxes_man_only_to_bd_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
slowsom_fluxes_man_only_to_bd_BE_BD <- calcNewField(x=slowsom_fluxes_man_only_to_bd_BE, y=slowsom_fluxes_man_only_to_bd_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
slowsom_fluxes_man_only_to_bd_forest <- calcNewField(x=slowsom_fluxes_man_only_to_bd_NE_ND, y=slowsom_fluxes_man_only_to_bd_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
slowsom_fluxes_man_only_to_bd_forest_mean <- aggregateYears(slowsom_fluxes_man_only_to_bd_forest, method = "mean" )

slowsom_pools_man_only_to_bd_NE_ND <- calcNewField(x=soil_pools_man_only_to_bd_NE, y=soil_pools_man_only_to_bd_ND, x.col = "SLOWSOM", y.col = "SLOWSOM", op="+" )
slowsom_pools_man_only_to_bd_BE_BD <- calcNewField(x=soil_pools_man_only_to_bd_BE, y=soil_pools_man_only_to_bd_BD, x.col = "SLOWSOM", y.col = "SLOWSOM", op="+" )
slowsom_pools_man_only_to_bd_forest <- calcNewField(x=slowsom_pools_man_only_to_bd_NE_ND, y=slowsom_pools_man_only_to_bd_BE_BD, x.col = "SLOWSOM", y.col = "SLOWSOM", op="+" )
slowsom_pools_man_only_to_bd_forest_mean <- aggregateYears(slowsom_pools_man_only_to_bd_forest, method = "mean" )

turnover_slowsom_man_only_to_bd <- calcNewField( x=slowsom_pools_man_only_to_bd_forest_mean, y=slowsom_fluxes_man_only_to_bd_forest_mean,
                            x.col="SLOWSOM", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_slowsom_man_only_to_bd, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_slowsom_man_only_to_bd.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)

################
#.... to BE ....
################
tot_cpool_man_only_to_be <- getField(source = man_only_to_be, quant = "cpool_forest", first.year = 2220, last.year = 2249)

soil_pools_man_only_to_be_NE <- getField(source = man_only_to_be, quant = "cpool_all_ForestNE", first.year = 2220, last.year = 2249)
soil_pools_man_only_to_be_BE <- getField(source = man_only_to_be, quant = "cpool_all_ForestBE", first.year = 2220, last.year = 2249)
soil_pools_man_only_to_be_BD <- getField(source = man_only_to_be, quant = "cpool_all_ForestBD", first.year = 2220, last.year = 2249)
soil_pools_man_only_to_be_ND <- getField(source = man_only_to_be, quant = "cpool_all_ForestND", first.year = 2220, last.year = 2249)

soil_fluxes_man_only_to_be_NE <- getField(source = man_only_to_be, quant = "cflux_all_ForestNE", first.year = 2220, last.year = 2249)
soil_fluxes_man_only_to_be_BE <- getField(source = man_only_to_be, quant = "cflux_all_ForestBE", first.year = 2220, last.year = 2249)
soil_fluxes_man_only_to_be_BD <- getField(source = man_only_to_be, quant = "cflux_all_ForestBD", first.year = 2220, last.year = 2249)
soil_fluxes_man_only_to_be_ND <- getField(source = man_only_to_be, quant = "cflux_all_ForestND", first.year = 2220, last.year = 2249)

#.......................
#... TOTAL SOIL POOL ...
#.......................
tot_soil_fluxes_man_only_to_be_NE <- layerOp( x=soil_fluxes_man_only_to_be_NE, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")
tot_soil_fluxes_man_only_to_be_ND <- layerOp( x=soil_fluxes_man_only_to_be_ND, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")
tot_soil_fluxes_man_only_to_be_BD <- layerOp( x=soil_fluxes_man_only_to_be_BD, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")
tot_soil_fluxes_man_only_to_be_BE <- layerOp( x=soil_fluxes_man_only_to_be_BE, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")

tot_soil_fluxes_man_only_to_be_NE_ND <- calcNewField(x=tot_soil_fluxes_man_only_to_be_NE, y=tot_soil_fluxes_man_only_to_be_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
tot_soil_fluxes_man_only_to_be_BE_BD <- calcNewField(x=tot_soil_fluxes_man_only_to_be_BE, y=tot_soil_fluxes_man_only_to_be_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
tot_soil_fluxes_man_only_to_be_forest <- calcNewField(x=tot_soil_fluxes_man_only_to_be_NE_ND, y=tot_soil_fluxes_man_only_to_be_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
tot_soil_fluxes_man_only_to_be_forest_mean <- aggregateYears(tot_soil_fluxes_man_only_to_be_forest, method = "mean" )

tot_pools_man_only_to_be_forest_mean <- aggregateYears(tot_cpool_man_only_to_be, method = "mean" )

turnover_tot_soil_man_only_to_be <- calcNewField( x=tot_pools_man_only_to_be_forest_mean, y=tot_soil_fluxes_man_only_to_be_forest_mean,
                            x.col="SoilC", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_tot_soil_man_only_to_be, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_tot_soil_man_only_to_be.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#.....................
#... SURF FWD POOL ...
#.....................
surffwd_fluxes_man_only_to_be_NE <- layerOp( x=soil_fluxes_man_only_to_be_NE, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")
surffwd_fluxes_man_only_to_be_ND <- layerOp( x=soil_fluxes_man_only_to_be_ND, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")
surffwd_fluxes_man_only_to_be_BD <- layerOp( x=soil_fluxes_man_only_to_be_BD, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")
surffwd_fluxes_man_only_to_be_BE <- layerOp( x=soil_fluxes_man_only_to_be_BE, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")

surffwd_fluxes_man_only_to_be_NE_ND <- calcNewField(x=surffwd_fluxes_man_only_to_be_NE, y=surffwd_fluxes_man_only_to_be_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surffwd_fluxes_man_only_to_be_BE_BD <- calcNewField(x=surffwd_fluxes_man_only_to_be_BE, y=surffwd_fluxes_man_only_to_be_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surffwd_fluxes_man_only_to_be_forest <- calcNewField(x=surffwd_fluxes_man_only_to_be_NE_ND, y=surffwd_fluxes_man_only_to_be_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surffwd_fluxes_man_only_to_be_forest_mean <- aggregateYears(surffwd_fluxes_man_only_to_be_forest, method = "mean" )

surffwd_pools_man_only_to_be_NE_ND <- calcNewField(x=soil_pools_man_only_to_be_NE, y=soil_pools_man_only_to_be_ND, x.col = "SURFFWD", y.col = "SURFFWD", op="+" )
surffwd_pools_man_only_to_be_BE_BD <- calcNewField(x=soil_pools_man_only_to_be_BE, y=soil_pools_man_only_to_be_BD, x.col = "SURFFWD", y.col = "SURFFWD", op="+" )
surffwd_pools_man_only_to_be_forest <- calcNewField(x=surffwd_pools_man_only_to_be_NE_ND, y=surffwd_pools_man_only_to_be_BE_BD, x.col = "SURFFWD", y.col = "SURFFWD", op="+" )
surffwd_pools_man_only_to_be_forest_mean <- aggregateYears(surffwd_pools_man_only_to_be_forest, method = "mean" )

turnover_surffwd_man_only_to_be <- calcNewField( x=surffwd_pools_man_only_to_be_forest_mean, y=surffwd_fluxes_man_only_to_be_forest_mean,
                            x.col="SURFFWD", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_surffwd_man_only_to_be, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_surffwd_man_only_to_be.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#.....................
#... SURF CWD POOL ...
#.....................
surfcwd_fluxes_man_only_to_be_NE <- layerOp( x=soil_fluxes_man_only_to_be_NE, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")
surfcwd_fluxes_man_only_to_be_ND <- layerOp( x=soil_fluxes_man_only_to_be_ND, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")
surfcwd_fluxes_man_only_to_be_BD <- layerOp( x=soil_fluxes_man_only_to_be_BD, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")
surfcwd_fluxes_man_only_to_be_BE <- layerOp( x=soil_fluxes_man_only_to_be_BE, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")

surfcwd_fluxes_man_only_to_be_NE_ND <- calcNewField(x=surfcwd_fluxes_man_only_to_be_NE, y=surfcwd_fluxes_man_only_to_be_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfcwd_fluxes_man_only_to_be_BE_BD <- calcNewField(x=surfcwd_fluxes_man_only_to_be_BE, y=surfcwd_fluxes_man_only_to_be_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfcwd_fluxes_man_only_to_be_forest <- calcNewField(x=surfcwd_fluxes_man_only_to_be_NE_ND, y=surfcwd_fluxes_man_only_to_be_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfcwd_fluxes_man_only_to_be_forest_mean <- aggregateYears(surfcwd_fluxes_man_only_to_be_forest, method = "mean" )

surfcwd_pools_man_only_to_be_NE_ND <- calcNewField(x=soil_pools_man_only_to_be_NE, y=soil_pools_man_only_to_be_ND, x.col = "SURFCWD", y.col = "SURFCWD", op="+" )
surfcwd_pools_man_only_to_be_BE_BD <- calcNewField(x=soil_pools_man_only_to_be_BE, y=soil_pools_man_only_to_be_BD, x.col = "SURFCWD", y.col = "SURFCWD", op="+" )
surfcwd_pools_man_only_to_be_forest <- calcNewField(x=surfcwd_pools_man_only_to_be_NE_ND, y=surfcwd_pools_man_only_to_be_BE_BD, x.col = "SURFCWD", y.col = "SURFCWD", op="+" )
surfcwd_pools_man_only_to_be_forest_mean <- aggregateYears(surfcwd_pools_man_only_to_be_forest, method = "mean" )

turnover_surfcwd_man_only_to_be <- calcNewField( x=surfcwd_pools_man_only_to_be_forest_mean, y=surfcwd_fluxes_man_only_to_be_forest_mean,
                            x.col="SURFCWD", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_surfcwd_man_only_to_be, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_surfcwd_man_only_to_be.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#..................
#... SURF HUMUS ...
#..................
surfhum_fluxes_man_only_to_be_NE <- layerOp( x=soil_fluxes_man_only_to_be_NE, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")
surfhum_fluxes_man_only_to_be_ND <- layerOp( x=soil_fluxes_man_only_to_be_ND, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")
surfhum_fluxes_man_only_to_be_BD <- layerOp( x=soil_fluxes_man_only_to_be_BD, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")
surfhum_fluxes_man_only_to_be_BE <- layerOp( x=soil_fluxes_man_only_to_be_BE, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")

surfhum_fluxes_man_only_to_be_NE_ND <- calcNewField(x=surfhum_fluxes_man_only_to_be_NE, y=surfhum_fluxes_man_only_to_be_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfhum_fluxes_man_only_to_be_BE_BD <- calcNewField(x=surfhum_fluxes_man_only_to_be_BE, y=surfhum_fluxes_man_only_to_be_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfhum_fluxes_man_only_to_be_forest <- calcNewField(x=surfhum_fluxes_man_only_to_be_NE_ND, y=surfhum_fluxes_man_only_to_be_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfhum_fluxes_man_only_to_be_forest_mean <- aggregateYears(surfhum_fluxes_man_only_to_be_forest, method = "mean" )

surfhum_pools_man_only_to_be_NE_ND <- calcNewField(x=soil_pools_man_only_to_be_NE, y=soil_pools_man_only_to_be_ND, x.col = "SURFHUMUS", y.col = "SURFHUMUS", op="+" )
surfhum_pools_man_only_to_be_BE_BD <- calcNewField(x=soil_pools_man_only_to_be_BE, y=soil_pools_man_only_to_be_BD, x.col = "SURFHUMUS", y.col = "SURFHUMUS", op="+" )
surfhum_pools_man_only_to_be_forest <- calcNewField(x=surfhum_pools_man_only_to_be_NE_ND, y=surfhum_pools_man_only_to_be_BE_BD, x.col = "SURFHUMUS", y.col = "SURFHUMUS", op="+" )
surfhum_pools_man_only_to_be_forest_mean <- aggregateYears(surfhum_pools_man_only_to_be_forest, method = "mean" )

turnover_surfhum_man_only_to_be <- calcNewField( x=surfhum_pools_man_only_to_be_forest_mean, y=surfhum_fluxes_man_only_to_be_forest_mean,
                            x.col="SURFHUMUS", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_surfhum_man_only_to_be, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_surfhum_man_only_to_be.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#.....................
#... SLOW SOM POOL ...
#.....................
slowsom_fluxes_man_only_to_be_NE <- layerOp( x=soil_fluxes_man_only_to_be_NE, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")
slowsom_fluxes_man_only_to_be_ND <- layerOp( x=soil_fluxes_man_only_to_be_ND, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")
slowsom_fluxes_man_only_to_be_BD <- layerOp( x=soil_fluxes_man_only_to_be_BD, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")
slowsom_fluxes_man_only_to_be_BE <- layerOp( x=soil_fluxes_man_only_to_be_BE, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")

slowsom_fluxes_man_only_to_be_NE_ND <- calcNewField(x=slowsom_fluxes_man_only_to_be_NE, y=slowsom_fluxes_man_only_to_be_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
slowsom_fluxes_man_only_to_be_BE_BD <- calcNewField(x=slowsom_fluxes_man_only_to_be_BE, y=slowsom_fluxes_man_only_to_be_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
slowsom_fluxes_man_only_to_be_forest <- calcNewField(x=slowsom_fluxes_man_only_to_be_NE_ND, y=slowsom_fluxes_man_only_to_be_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
slowsom_fluxes_man_only_to_be_forest_mean <- aggregateYears(slowsom_fluxes_man_only_to_be_forest, method = "mean" )

slowsom_pools_man_only_to_be_NE_ND <- calcNewField(x=soil_pools_man_only_to_be_NE, y=soil_pools_man_only_to_be_ND, x.col = "SLOWSOM", y.col = "SLOWSOM", op="+" )
slowsom_pools_man_only_to_be_BE_BD <- calcNewField(x=soil_pools_man_only_to_be_BE, y=soil_pools_man_only_to_be_BD, x.col = "SLOWSOM", y.col = "SLOWSOM", op="+" )
slowsom_pools_man_only_to_be_forest <- calcNewField(x=slowsom_pools_man_only_to_be_NE_ND, y=slowsom_pools_man_only_to_be_BE_BD, x.col = "SLOWSOM", y.col = "SLOWSOM", op="+" )
slowsom_pools_man_only_to_be_forest_mean <- aggregateYears(slowsom_pools_man_only_to_be_forest, method = "mean" )

turnover_slowsom_man_only_to_be <- calcNewField( x=slowsom_pools_man_only_to_be_forest_mean, y=slowsom_fluxes_man_only_to_be_forest_mean,
                            x.col="SLOWSOM", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_slowsom_man_only_to_be, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_slowsom_man_only_to_be.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)

#####################
#..... baseline .....
#####################
tot_cpool_man_only_base <- getField(source = man_only_base, quant = "cpool_forest", first.year = 2220, last.year = 2249)

soil_pools_man_only_base_NE <- getField(source = man_only_base, quant = "cpool_all_ForestNE", first.year = 2220, last.year = 2249)
soil_pools_man_only_base_BE <- getField(source = man_only_base, quant = "cpool_all_ForestBE", first.year = 2220, last.year = 2249)
soil_pools_man_only_base_BD <- getField(source = man_only_base, quant = "cpool_all_ForestBD", first.year = 2220, last.year = 2249)
soil_pools_man_only_base_ND <- getField(source = man_only_base, quant = "cpool_all_ForestND", first.year = 2220, last.year = 2249)

soil_fluxes_man_only_base_NE <- getField(source = man_only_base, quant = "cflux_all_ForestNE", first.year = 2220, last.year = 2249)
soil_fluxes_man_only_base_BE <- getField(source = man_only_base, quant = "cflux_all_ForestBE", first.year = 2220, last.year = 2249)
soil_fluxes_man_only_base_BD <- getField(source = man_only_base, quant = "cflux_all_ForestBD", first.year = 2220, last.year = 2249)
soil_fluxes_man_only_base_ND <- getField(source = man_only_base, quant = "cflux_all_ForestND", first.year = 2220, last.year = 2249)
#.......................
#... TOTAL SOIL POOL ...
#.......................
tot_soil_fluxes_man_only_base_NE <- layerOp( x=soil_fluxes_man_only_base_NE, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")
tot_soil_fluxes_man_only_base_ND <- layerOp( x=soil_fluxes_man_only_base_ND, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")
tot_soil_fluxes_man_only_base_BD <- layerOp( x=soil_fluxes_man_only_base_BD, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")
tot_soil_fluxes_man_only_base_BE <- layerOp( x=soil_fluxes_man_only_base_BE, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")

tot_soil_fluxes_man_only_base_NE_ND <- calcNewField(x=tot_soil_fluxes_man_only_base_NE, y=tot_soil_fluxes_man_only_base_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
tot_soil_fluxes_man_only_base_BE_BD <- calcNewField(x=tot_soil_fluxes_man_only_base_BE, y=tot_soil_fluxes_man_only_base_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
tot_soil_fluxes_man_only_base_forest <- calcNewField(x=tot_soil_fluxes_man_only_base_NE_ND, y=tot_soil_fluxes_man_only_base_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
tot_soil_fluxes_man_only_base_forest_mean <- aggregateYears(tot_soil_fluxes_man_only_base_forest, method = "mean" )

tot_pools_man_only_base_forest_mean <- aggregateYears(tot_cpool_man_only_base, method = "mean" )

turnover_tot_soil_man_only_base <- calcNewField( x=tot_pools_man_only_base_forest_mean, y=tot_soil_fluxes_man_only_base_forest_mean,
                            x.col="SoilC", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_tot_soil_man_only_base, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_tot_soil_man_only_base.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#.....................
#... SURF FWD POOL ...
#.....................
surffwd_fluxes_man_only_base_NE <- layerOp( x=soil_fluxes_man_only_base_NE, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")
surffwd_fluxes_man_only_base_ND <- layerOp( x=soil_fluxes_man_only_base_ND, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")
surffwd_fluxes_man_only_base_BD <- layerOp( x=soil_fluxes_man_only_base_BD, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")
surffwd_fluxes_man_only_base_BE <- layerOp( x=soil_fluxes_man_only_base_BE, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")

surffwd_fluxes_man_only_base_NE_ND <- calcNewField(x=surffwd_fluxes_man_only_base_NE, y=surffwd_fluxes_man_only_base_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surffwd_fluxes_man_only_base_BE_BD <- calcNewField(x=surffwd_fluxes_man_only_base_BE, y=surffwd_fluxes_man_only_base_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surffwd_fluxes_man_only_base_forest <- calcNewField(x=surffwd_fluxes_man_only_base_NE_ND, y=surffwd_fluxes_man_only_base_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surffwd_fluxes_man_only_base_forest_mean <- aggregateYears(surffwd_fluxes_man_only_base_forest, method = "mean" )

surffwd_pools_man_only_base_NE_ND <- calcNewField(x=soil_pools_man_only_base_NE, y=soil_pools_man_only_base_ND, x.col = "SURFFWD", y.col = "SURFFWD", op="+" )
surffwd_pools_man_only_base_BE_BD <- calcNewField(x=soil_pools_man_only_base_BE, y=soil_pools_man_only_base_BD, x.col = "SURFFWD", y.col = "SURFFWD", op="+" )
surffwd_pools_man_only_base_forest <- calcNewField(x=surffwd_pools_man_only_base_NE_ND, y=surffwd_pools_man_only_base_BE_BD, x.col = "SURFFWD", y.col = "SURFFWD", op="+" )
surffwd_pools_man_only_base_forest_mean <- aggregateYears(surffwd_pools_man_only_base_forest, method = "mean" )

turnover_surffwd_man_only_base <- calcNewField( x=surffwd_pools_man_only_base_forest_mean, y=surffwd_fluxes_man_only_base_forest_mean,
                            x.col="SURFFWD", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_surffwd_man_only_base, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_surffwd_man_only_base.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#.....................
#... SURF CWD POOL ...
#.....................
surfcwd_fluxes_man_only_base_NE <- layerOp( x=soil_fluxes_man_only_base_NE, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")
surfcwd_fluxes_man_only_base_ND <- layerOp( x=soil_fluxes_man_only_base_ND, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")
surfcwd_fluxes_man_only_base_BD <- layerOp( x=soil_fluxes_man_only_base_BD, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")
surfcwd_fluxes_man_only_base_BE <- layerOp( x=soil_fluxes_man_only_base_BE, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")

surfcwd_fluxes_man_only_base_NE_ND <- calcNewField(x=surfcwd_fluxes_man_only_base_NE, y=surfcwd_fluxes_man_only_base_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfcwd_fluxes_man_only_base_BE_BD <- calcNewField(x=surfcwd_fluxes_man_only_base_BE, y=surfcwd_fluxes_man_only_base_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfcwd_fluxes_man_only_base_forest <- calcNewField(x=surfcwd_fluxes_man_only_base_NE_ND, y=surfcwd_fluxes_man_only_base_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfcwd_fluxes_man_only_base_forest_mean <- aggregateYears(surfcwd_fluxes_man_only_base_forest, method = "mean" )

surfcwd_pools_man_only_base_NE_ND <- calcNewField(x=soil_pools_man_only_base_NE, y=soil_pools_man_only_base_ND, x.col = "SURFCWD", y.col = "SURFCWD", op="+" )
surfcwd_pools_man_only_base_BE_BD <- calcNewField(x=soil_pools_man_only_base_BE, y=soil_pools_man_only_base_BD, x.col = "SURFCWD", y.col = "SURFCWD", op="+" )
surfcwd_pools_man_only_base_forest <- calcNewField(x=surfcwd_pools_man_only_base_NE_ND, y=surfcwd_pools_man_only_base_BE_BD, x.col = "SURFCWD", y.col = "SURFCWD", op="+" )
surfcwd_pools_man_only_base_forest_mean <- aggregateYears(surfcwd_pools_man_only_base_forest, method = "mean" )

turnover_surfcwd_man_only_base <- calcNewField( x=surfcwd_pools_man_only_base_forest_mean, y=surfcwd_fluxes_man_only_base_forest_mean,
                            x.col="SURFCWD", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_surfcwd_man_only_base, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_surfcwd_man_only_base.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#..................
#... SURF HUMUS ...
#..................
surfhum_fluxes_man_only_base_NE <- layerOp( x=soil_fluxes_man_only_base_NE, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")
surfhum_fluxes_man_only_base_ND <- layerOp( x=soil_fluxes_man_only_base_ND, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")
surfhum_fluxes_man_only_base_BD <- layerOp( x=soil_fluxes_man_only_base_BD, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")
surfhum_fluxes_man_only_base_BE <- layerOp( x=soil_fluxes_man_only_base_BE, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")

surfhum_fluxes_man_only_base_NE_ND <- calcNewField(x=surfhum_fluxes_man_only_base_NE, y=surfhum_fluxes_man_only_base_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfhum_fluxes_man_only_base_BE_BD <- calcNewField(x=surfhum_fluxes_man_only_base_BE, y=surfhum_fluxes_man_only_base_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfhum_fluxes_man_only_base_forest <- calcNewField(x=surfhum_fluxes_man_only_base_NE_ND, y=surfhum_fluxes_man_only_base_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfhum_fluxes_man_only_base_forest_mean <- aggregateYears(surfhum_fluxes_man_only_base_forest, method = "mean" )

surfhum_pools_man_only_base_NE_ND <- calcNewField(x=soil_pools_man_only_base_NE, y=soil_pools_man_only_base_ND, x.col = "SURFHUMUS", y.col = "SURFHUMUS", op="+" )
surfhum_pools_man_only_base_BE_BD <- calcNewField(x=soil_pools_man_only_base_BE, y=soil_pools_man_only_base_BD, x.col = "SURFHUMUS", y.col = "SURFHUMUS", op="+" )
surfhum_pools_man_only_base_forest <- calcNewField(x=surfhum_pools_man_only_base_NE_ND, y=surfhum_pools_man_only_base_BE_BD, x.col = "SURFHUMUS", y.col = "SURFHUMUS", op="+" )
surfhum_pools_man_only_base_forest_mean <- aggregateYears(surfhum_pools_man_only_base_forest, method = "mean" )

turnover_surfhum_man_only_base <- calcNewField( x=surfhum_pools_man_only_base_forest_mean, y=surfhum_fluxes_man_only_base_forest_mean,
                            x.col="SURFHUMUS", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_surfhum_man_only_base, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_surfhum_man_only_base.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)

#.....................
#... SLOW SOM POOL ...
#.....................
slowsom_fluxes_man_only_base_NE <- layerOp( x=soil_fluxes_man_only_base_NE, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")
slowsom_fluxes_man_only_base_ND <- layerOp( x=soil_fluxes_man_only_base_ND, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")
slowsom_fluxes_man_only_base_BD <- layerOp( x=soil_fluxes_man_only_base_BD, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")
slowsom_fluxes_man_only_base_BE <- layerOp( x=soil_fluxes_man_only_base_BE, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")

slowsom_fluxes_man_only_base_NE_ND <- calcNewField(x=slowsom_fluxes_man_only_base_NE, y=slowsom_fluxes_man_only_base_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
slowsom_fluxes_man_only_base_BE_BD <- calcNewField(x=slowsom_fluxes_man_only_base_BE, y=slowsom_fluxes_man_only_base_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
slowsom_fluxes_man_only_base_forest <- calcNewField(x=slowsom_fluxes_man_only_base_NE_ND, y=slowsom_fluxes_man_only_base_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
slowsom_fluxes_man_only_base_forest_mean <- aggregateYears(slowsom_fluxes_man_only_base_forest, method = "mean" )

slowsom_pools_man_only_base_NE_ND <- calcNewField(x=soil_pools_man_only_base_NE, y=soil_pools_man_only_base_ND, x.col = "SLOWSOM", y.col = "SLOWSOM", op="+" )
slowsom_pools_man_only_base_BE_BD <- calcNewField(x=soil_pools_man_only_base_BE, y=soil_pools_man_only_base_BD, x.col = "SLOWSOM", y.col = "SLOWSOM", op="+" )
slowsom_pools_man_only_base_forest <- calcNewField(x=slowsom_pools_man_only_base_NE_ND, y=slowsom_pools_man_only_base_BE_BD, x.col = "SLOWSOM", y.col = "SLOWSOM", op="+" )
slowsom_pools_man_only_base_forest_mean <- aggregateYears(slowsom_pools_man_only_base_forest, method = "mean" )

turnover_slowsom_man_only_base <- calcNewField( x=slowsom_pools_man_only_base_forest_mean, y=slowsom_fluxes_man_only_base_forest_mean,
                            x.col="SLOWSOM", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_slowsom_man_only_base, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_slowsom_man_only_base.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)

########################################################################################
########################################################################################

# save all the total soil fluxes
#combine the fluxes from each management option in a df
tot_soil_fluxes_man_only_base_forest <- as.data.frame(tot_soil_fluxes_man_only_base_forest)
fluxes_soil_unm <- fluxes_soil_unm_df
fluxes_soil_tobd <- as.data.frame(fluxes_soil_tobd)
fluxes_soil_tobe <- as.data.frame(fluxes_soil_tobe)
fluxes_soil_tone <- as.data.frame(fluxes_soil_tone)

fluxes_soil_all <- cbind(fluxes_soil_base, fluxes_soil_tobd[, 4], fluxes_soil_tobe[, 4], fluxes_soil_tone[, 4], fluxes_soil_unm[, 4])
colnames(fluxes_soil_all) <- c("Lon", "Lat","Year",
                            "F_h_resp_base", "F_h_resp_tobd", "F_h_resp_tobe", "F_h_resp_tone", "F_soil_fire_unm", "F_h_resp_unm")

# write.table(fluxes_soil_all, file = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\equilibrium\\data_pools_fluxes\\outer_fluxes_soil_2220_2249",
#            row.names = FALSE, col.names = TRUE)

#fluxes_soil_all <- read.table("C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\equilibrium\\data_pools_fluxes\\outer_fluxes_soil_2220_2249", header = TRUE)


colnames(all_fluxes) <- c("Lon", "Lat", "Year", "F_eco_a_resp_base", "F_eco_h_resp_base", "F_eco_harv_base", "F_eco_fire_base",
                            "F_eco_a_resp_tobd", "F_eco_h_resp_tobd", "F_eco_harv_tobd", "F_eco_fire_tobd",
                            "F_eco_a_resp_tobe", "F_eco_h_resp_tobe", "F_eco_harv_tobe", "F_eco_fire_tobe",
                            "F_eco_a_resp_tone", "F_eco_h_resp_tone", "F_eco_harv_tone", "F_eco_fire_tone", 
                            "F_eco_a_resp_unm", "F_eco_h_resp_unm", "F_eco_harv_unm", "F_eco_fire_unm",
                            "F_stem_harv_base", "F_stem_mort_base", "F_stem_fire_base", "F_stem_dist_base",
                            "F_stem_harv_tobd", "F_stem_mort_tobd", "F_stem_fire_tobd", "F_stem_dist_tobd",
                            "F_stem_harv_tobe", "F_stem_mort_tobe", "F_stem_fire_tobe", "F_stem_dist_tobe",
                            "F_stem_harv_tone", "F_stem_mort_tone", "F_stem_fire_tone", "F_stem_dist_tone",
                            "F_stem_harv_unm", "F_stem_mort_unm", "F_stem_fire_unm", "F_stem_dist_unm",
                            "F_soil_base", "F_soil_tobd", "F_soil_tobe", "F_soil_tone", "F_soil_burn", "F_soil_unm")

mean_all_fluxes <- all_fluxes %>%
                group_by(Lon,Lat)%>%  #I group the operation that I want to do, by Lon and Lat, so that I calculate the value for each grid cell
                summarise(across(F_eco_a_resp_base:F_soil_unm, mean))  #mean of each column between F_harv_base and F_repr_unm



#############################################################
# calculate differences between baseline and different FM options
#############################################################
turnover_tot_soil_man_only_unm <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_tot_soil_man_only_unm)
                        
turnover_tot_soil_man_only_to_ne <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_tot_soil_man_only_to_ne)
turnover_tot_soil_man_only_to_bd <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_tot_soil_man_only_to_bd)
turnover_tot_soil_man_only_to_be <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_tot_soil_man_only_to_be)
turnover_tot_soil_man_only_base <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_tot_soil_man_only_base)

turnover_tot_soil_man_only_unm_df <- as.data.frame(turnover_tot_soil_man_only_unm)
colnames(turnover_tot_soil_man_only_unm_df) <- c("Lon", "Lat", "turn_unm") 
turnover_tot_soil_man_only_to_ne_df <- as.data.frame(turnover_tot_soil_man_only_to_ne)
colnames(turnover_tot_soil_man_only_to_ne_df) <- c("Lon", "Lat", "turn_tone") #change col names not to have spaces
turnover_tot_soil_man_only_to_bd_df <- as.data.frame(turnover_tot_soil_man_only_to_bd)
colnames(turnover_tot_soil_man_only_to_bd_df) <- c("Lon", "Lat", "turn_tobd") #change col names not to have spaces
turnover_tot_soil_man_only_to_be_df <- as.data.frame(turnover_tot_soil_man_only_to_be)
colnames(turnover_tot_soil_man_only_to_be_df) <- c("Lon", "Lat", "turn_tobe") #change col names not to have spaces
turnover_tot_soil_man_only_base_df <- as.data.frame(turnover_tot_soil_man_only_base)
colnames(turnover_tot_soil_man_only_base_df) <- c("Lon", "Lat", "turn_base") #change col names not to have spaces

turnover_tot_soil_man_only_list <- list(turnover_tot_soil_man_only_unm_df, 
                                            turnover_tot_soil_man_only_to_ne_df,
                                            turnover_tot_soil_man_only_to_bd_df,
                                            turnover_tot_soil_man_only_to_be_df,
                                            turnover_tot_soil_man_only_base_df
                                            )
turnover_tot_soil_man_only_all <- reduce(turnover_tot_soil_man_only_list, full_join, by = c("Lon", "Lat"))
turnover_tot_soil_man_only_all <- turnover_tot_soil_man_only_all %>%
  mutate(
    diff_unm_base = turn_unm - turn_base,
    diff_tone_base = turn_tone - turn_base,
    diff_tobd_base = turn_tobd - turn_base,
    diff_tobe_base = turn_tobe - turn_base
  )

#saveRDS(turnover_tot_soil_man_only_all, file= "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_tot_soil_man_only_all_values_and_diff_with_base.RData",ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#write.table(turnover_tot_soil_man_only_all, file = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_tot_soil_man_only_all_values_and_diff_with_base")
############################################################################################

turnover_surfhum_man_only_unm <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_surfhum_man_only_unm)
                        
turnover_surfhum_man_only_to_ne <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_surfhum_man_only_to_ne)
turnover_surfhum_man_only_to_bd <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_surfhum_man_only_to_bd)
turnover_surfhum_man_only_to_be <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_surfhum_man_only_to_be)
turnover_surfhum_man_only_base <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_surfhum_man_only_base)

turnover_surfhum_man_only_unm_df <- as.data.frame(turnover_surfhum_man_only_unm)
colnames(turnover_surfhum_man_only_unm_df) <- c("Lon", "Lat", "turn_unm") 
turnover_surfhum_man_only_to_ne_df <- as.data.frame(turnover_surfhum_man_only_to_ne)
colnames(turnover_surfhum_man_only_to_ne_df) <- c("Lon", "Lat", "turn_tone") #change col names not to have spaces
turnover_surfhum_man_only_to_bd_df <- as.data.frame(turnover_surfhum_man_only_to_bd)
colnames(turnover_surfhum_man_only_to_bd_df) <- c("Lon", "Lat", "turn_tobd") #change col names not to have spaces
turnover_surfhum_man_only_to_be_df <- as.data.frame(turnover_surfhum_man_only_to_be)
colnames(turnover_surfhum_man_only_to_be_df) <- c("Lon", "Lat", "turn_tobe") #change col names not to have spaces
turnover_surfhum_man_only_base_df <- as.data.frame(turnover_surfhum_man_only_base)
colnames(turnover_surfhum_man_only_base_df) <- c("Lon", "Lat", "turn_base") #change col names not to have spaces

turnover_surfhum_man_only_list <- list(turnover_surfhum_man_only_unm_df, 
                                            turnover_surfhum_man_only_to_ne_df,
                                            turnover_surfhum_man_only_to_bd_df,
                                            turnover_surfhum_man_only_to_be_df,
                                            turnover_surfhum_man_only_base_df
                                            )
turnover_surfhum_man_only_all <- reduce(turnover_surfhum_man_only_list, full_join, by = c("Lon", "Lat"))
turnover_surfhum_man_only_all <- turnover_surfhum_man_only_all %>%
  mutate(
    diff_unm_base = turn_unm - turn_base,
    diff_tone_base = turn_tone - turn_base,
    diff_tobd_base = turn_tobd - turn_base,
    diff_tobe_base = turn_tobe - turn_base
  )

#saveRDS(turnover_surfhum_man_only_all, file= "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_surfhum_man_only_all_values_and_diff_with_base.RData",ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#write.table(turnover_surfhum_man_only_all, file = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_surf_hum_man_only_all_values_and_diff_with_base")
############################################################################################

######################################################################
# the same steps can be done for surf cwd, surf fwd, and slowsom pools
######################################################################


############################################################################
#...................MANAGEMENT AND CLIMATE CHANGE ..........................
############################################################################
###################
#....unmanaged ....
###################
tot_cpool_man_cc_unm <- getField(source = man_cc_unm, quant = "cpool_forest", first.year = 2060, last.year = 2089)

soil_pools_man_cc_unm_NE <- getField(source = man_cc_unm, quant = "cpool_all_ForestNE", first.year = 2060, last.year = 2089)
soil_pools_man_cc_unm_BE <- getField(source = man_cc_unm, quant = "cpool_all_ForestBE", first.year = 2060, last.year = 2089)
soil_pools_man_cc_unm_BD <- getField(source = man_cc_unm, quant = "cpool_all_ForestBD", first.year = 2060, last.year = 2089)
soil_pools_man_cc_unm_ND <- getField(source = man_cc_unm, quant = "cpool_all_ForestND", first.year = 2060, last.year = 2089)

soil_fluxes_man_cc_unm_NE <- getField(source = man_cc_unm, quant = "cflux_all_ForestNE", first.year = 2060, last.year = 2089)
soil_fluxes_man_cc_unm_BE <- getField(source = man_cc_unm, quant = "cflux_all_ForestBE", first.year = 2060, last.year = 2089)
soil_fluxes_man_cc_unm_BD <- getField(source = man_cc_unm, quant = "cflux_all_ForestBD", first.year = 2060, last.year = 2089)
soil_fluxes_man_cc_unm_ND <- getField(source = man_cc_unm, quant = "cflux_all_ForestND", first.year = 2060, last.year = 2089)
#................................
#...... TOTAL SOIL POOL .........
#................................
tot_soil_fluxes_man_cc_unm_NE <- layerOp( x=soil_fluxes_man_cc_unm_NE, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")
tot_soil_fluxes_man_cc_unm_ND <- layerOp( x=soil_fluxes_man_cc_unm_ND, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")
tot_soil_fluxes_man_cc_unm_BD <- layerOp( x=soil_fluxes_man_cc_unm_BD, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")
tot_soil_fluxes_man_cc_unm_BE <- layerOp( x=soil_fluxes_man_cc_unm_BE, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")
tot_soil_fluxes_man_cc_unm_NE_ND <- calcNewField(x=tot_soil_fluxes_man_cc_unm_NE, y=tot_soil_fluxes_man_cc_unm_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
tot_soil_fluxes_man_cc_unm_BE_BD <- calcNewField(x=tot_soil_fluxes_man_cc_unm_BE, y=tot_soil_fluxes_man_cc_unm_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
tot_soil_fluxes_man_cc_unm_forest <- calcNewField(x=tot_soil_fluxes_man_cc_unm_NE_ND, y=tot_soil_fluxes_man_cc_unm_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
tot_soil_fluxes_man_cc_unm_forest_mean <- aggregateYears(tot_soil_fluxes_man_cc_unm_forest, method = "mean" )

tot_pools_man_cc_unm_forest_mean <- aggregateYears(tot_cpool_man_cc_unm, method = "mean" )

turnover_tot_soil_man_cc_unm <- calcNewField( x=tot_pools_man_cc_unm_forest_mean, y=tot_soil_fluxes_man_cc_unm_forest_mean,
                            x.col="SoilC", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_tot_soil_man_cc_unm, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_tot_soil_man_cc_unm.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#.....................
#... SURF FWD POOL ...
#.....................
surffwd_fluxes_man_cc_unm_NE <- layerOp( x=soil_fluxes_man_cc_unm_NE, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")
surffwd_fluxes_man_cc_unm_ND <- layerOp( x=soil_fluxes_man_cc_unm_ND, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")
surffwd_fluxes_man_cc_unm_BD <- layerOp( x=soil_fluxes_man_cc_unm_BD, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")
surffwd_fluxes_man_cc_unm_BE <- layerOp( x=soil_fluxes_man_cc_unm_BE, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")
surffwd_fluxes_man_cc_unm_NE_ND <- calcNewField(x=surffwd_fluxes_man_cc_unm_NE, y=surffwd_fluxes_man_cc_unm_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surffwd_fluxes_man_cc_unm_BE_BD <- calcNewField(x=surffwd_fluxes_man_cc_unm_BE, y=surffwd_fluxes_man_cc_unm_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surffwd_fluxes_man_cc_unm_forest <- calcNewField(x=surffwd_fluxes_man_cc_unm_NE_ND, y=surffwd_fluxes_man_cc_unm_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surffwd_fluxes_man_cc_unm_forest_mean <- aggregateYears(surffwd_fluxes_man_cc_unm_forest, method = "mean" )

surffwd_pools_man_cc_unm_NE_ND <- calcNewField(x=soil_pools_man_cc_unm_NE, y=soil_pools_man_cc_unm_ND, x.col = "SURFFWD", y.col = "SURFFWD", op="+" )
surffwd_pools_man_cc_unm_BE_BD <- calcNewField(x=soil_pools_man_cc_unm_BE, y=soil_pools_man_cc_unm_BD, x.col = "SURFFWD", y.col = "SURFFWD", op="+" )
surffwd_pools_man_cc_unm_forest <- calcNewField(x=surffwd_pools_man_cc_unm_NE_ND, y=surffwd_pools_man_cc_unm_BE_BD, x.col = "SURFFWD", y.col = "SURFFWD", op="+" )
surffwd_pools_man_cc_unm_forest_mean <- aggregateYears(surffwd_pools_man_cc_unm_forest, method = "mean" )

turnover_surffwd_man_cc_unm <- calcNewField( x=surffwd_pools_man_cc_unm_forest_mean, y=surffwd_fluxes_man_cc_unm_forest_mean,
                            x.col="SURFFWD", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_surffwd_man_cc_unm, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_surffwd_man_cc_unm.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#.....................
#... SURF CWD POOL ...
#.....................
surfcwd_fluxes_man_cc_unm_NE <- layerOp( x=soil_fluxes_man_cc_unm_NE, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")
surfcwd_fluxes_man_cc_unm_ND <- layerOp( x=soil_fluxes_man_cc_unm_ND, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")
surfcwd_fluxes_man_cc_unm_BD <- layerOp( x=soil_fluxes_man_cc_unm_BD, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")
surfcwd_fluxes_man_cc_unm_BE <- layerOp( x=soil_fluxes_man_cc_unm_BE, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")

surfcwd_fluxes_man_cc_unm_NE_ND <- calcNewField(x=surfcwd_fluxes_man_cc_unm_NE, y=surfcwd_fluxes_man_cc_unm_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfcwd_fluxes_man_cc_unm_BE_BD <- calcNewField(x=surfcwd_fluxes_man_cc_unm_BE, y=surfcwd_fluxes_man_cc_unm_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfcwd_fluxes_man_cc_unm_forest <- calcNewField(x=surfcwd_fluxes_man_cc_unm_NE_ND, y=surfcwd_fluxes_man_cc_unm_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfcwd_fluxes_man_cc_unm_forest_mean <- aggregateYears(surfcwd_fluxes_man_cc_unm_forest, method = "mean" )

surfcwd_pools_man_cc_unm_NE_ND <- calcNewField(x=soil_pools_man_cc_unm_NE, y=soil_pools_man_cc_unm_ND, x.col = "SURFCWD", y.col = "SURFCWD", op="+" )
surfcwd_pools_man_cc_unm_BE_BD <- calcNewField(x=soil_pools_man_cc_unm_BE, y=soil_pools_man_cc_unm_BD, x.col = "SURFCWD", y.col = "SURFCWD", op="+" )
surfcwd_pools_man_cc_unm_forest <- calcNewField(x=surfcwd_pools_man_cc_unm_NE_ND, y=surfcwd_pools_man_cc_unm_BE_BD, x.col = "SURFCWD", y.col = "SURFCWD", op="+" )
surfcwd_pools_man_cc_unm_forest_mean <- aggregateYears(surfcwd_pools_man_cc_unm_forest, method = "mean" )

turnover_surfcwd_man_cc_unm <- calcNewField( x=surfcwd_pools_man_cc_unm_forest_mean, y=surfcwd_fluxes_man_cc_unm_forest_mean,
                            x.col="SURFCWD", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_surfcwd_man_cc_unm, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_surfcwd_man_cc_unm.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#....................
#.... SURF HUMUS ....
#....................
surfhum_fluxes_man_cc_unm_NE <- layerOp( x=soil_fluxes_man_cc_unm_NE, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")
surfhum_fluxes_man_cc_unm_ND <- layerOp( x=soil_fluxes_man_cc_unm_ND, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")
surfhum_fluxes_man_cc_unm_BD <- layerOp( x=soil_fluxes_man_cc_unm_BD, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")
surfhum_fluxes_man_cc_unm_BE <- layerOp( x=soil_fluxes_man_cc_unm_BE, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")

surfhum_fluxes_man_cc_unm_NE_ND <- calcNewField(x=surfhum_fluxes_man_cc_unm_NE, y=surfhum_fluxes_man_cc_unm_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfhum_fluxes_man_cc_unm_BE_BD <- calcNewField(x=surfhum_fluxes_man_cc_unm_BE, y=surfhum_fluxes_man_cc_unm_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfhum_fluxes_man_cc_unm_forest <- calcNewField(x=surfhum_fluxes_man_cc_unm_NE_ND, y=surfhum_fluxes_man_cc_unm_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfhum_fluxes_man_cc_unm_forest_mean <- aggregateYears(surfhum_fluxes_man_cc_unm_forest, method = "mean" )

surfhum_pools_man_cc_unm_NE_ND <- calcNewField(x=soil_pools_man_cc_unm_NE, y=soil_pools_man_cc_unm_ND, x.col = "SURFHUMUS", y.col = "SURFHUMUS", op="+" )
surfhum_pools_man_cc_unm_BE_BD <- calcNewField(x=soil_pools_man_cc_unm_BE, y=soil_pools_man_cc_unm_BD, x.col = "SURFHUMUS", y.col = "SURFHUMUS", op="+" )
surfhum_pools_man_cc_unm_forest <- calcNewField(x=surfhum_pools_man_cc_unm_NE_ND, y=surfhum_pools_man_cc_unm_BE_BD, x.col = "SURFHUMUS", y.col = "SURFHUMUS", op="+" )
surfhum_pools_man_cc_unm_forest_mean <- aggregateYears(surfhum_pools_man_cc_unm_forest, method = "mean" )

turnover_surfhum_man_cc_unm <- calcNewField( x=surfhum_pools_man_cc_unm_forest_mean, y=surfhum_fluxes_man_cc_unm_forest_mean,
                            x.col="SURFHUMUS", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_surfhum_man_cc_unm, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_surfhum_man_cc_unm.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#.........................
#..... SLOW SOM POOL .....
#.........................
slowsom_fluxes_man_cc_unm_NE <- layerOp( x=soil_fluxes_man_cc_unm_NE, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")
slowsom_fluxes_man_cc_unm_ND <- layerOp( x=soil_fluxes_man_cc_unm_ND, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")
slowsom_fluxes_man_cc_unm_BD <- layerOp( x=soil_fluxes_man_cc_unm_BD, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")
slowsom_fluxes_man_cc_unm_BE <- layerOp( x=soil_fluxes_man_cc_unm_BE, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")

slowsom_fluxes_man_cc_unm_NE_ND <- calcNewField(x=slowsom_fluxes_man_cc_unm_NE, y=slowsom_fluxes_man_cc_unm_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
slowsom_fluxes_man_cc_unm_BE_BD <- calcNewField(x=slowsom_fluxes_man_cc_unm_BE, y=slowsom_fluxes_man_cc_unm_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
slowsom_fluxes_man_cc_unm_forest <- calcNewField(x=slowsom_fluxes_man_cc_unm_NE_ND, y=slowsom_fluxes_man_cc_unm_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
slowsom_fluxes_man_cc_unm_forest_mean <- aggregateYears(slowsom_fluxes_man_cc_unm_forest, method = "mean" )

slowsom_pools_man_cc_unm_NE_ND <- calcNewField(x=soil_pools_man_cc_unm_NE, y=soil_pools_man_cc_unm_ND, x.col = "SLOWSOM", y.col = "SLOWSOM", op="+" )
slowsom_pools_man_cc_unm_BE_BD <- calcNewField(x=soil_pools_man_cc_unm_BE, y=soil_pools_man_cc_unm_BD, x.col = "SLOWSOM", y.col = "SLOWSOM", op="+" )
slowsom_pools_man_cc_unm_forest <- calcNewField(x=slowsom_pools_man_cc_unm_NE_ND, y=slowsom_pools_man_cc_unm_BE_BD, x.col = "SLOWSOM", y.col = "SLOWSOM", op="+" )
slowsom_pools_man_cc_unm_forest_mean <- aggregateYears(slowsom_pools_man_cc_unm_forest, method = "mean" )

turnover_slowsom_man_cc_unm <- calcNewField( x=slowsom_pools_man_cc_unm_forest_mean, y=slowsom_fluxes_man_cc_unm_forest_mean,
                            x.col="SLOWSOM", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_slowsom_man_cc_unm, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_slowsom_man_cc_unm.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)

########################
#....... to NE .........
#########################
tot_cpool_man_cc_to_ne <- getField(source = man_cc_to_ne, quant = "cpool_forest", first.year = 2060, last.year = 2089)

soil_pools_man_cc_to_ne_NE <- getField(source = man_cc_to_ne, quant = "cpool_all_ForestNE", first.year = 2060, last.year = 2089)
soil_pools_man_cc_to_ne_BE <- getField(source = man_cc_to_ne, quant = "cpool_all_ForestBE", first.year = 2060, last.year = 2089)
soil_pools_man_cc_to_ne_BD <- getField(source = man_cc_to_ne, quant = "cpool_all_ForestBD", first.year = 2060, last.year = 2089)
soil_pools_man_cc_to_ne_ND <- getField(source = man_cc_to_ne, quant = "cpool_all_ForestND", first.year = 2060, last.year = 2089)

soil_fluxes_man_cc_to_ne_NE <- getField(source = man_cc_to_ne, quant = "cflux_all_ForestNE", first.year = 2060, last.year = 2089)
soil_fluxes_man_cc_to_ne_BE <- getField(source = man_cc_to_ne, quant = "cflux_all_ForestBE", first.year = 2060, last.year = 2089)
soil_fluxes_man_cc_to_ne_BD <- getField(source = man_cc_to_ne, quant = "cflux_all_ForestBD", first.year = 2060, last.year = 2089)
soil_fluxes_man_cc_to_ne_ND <- getField(source = man_cc_to_ne, quant = "cflux_all_ForestND", first.year = 2060, last.year = 2089)
#.......................
#... TOTAL SOIL POOL ...
#.......................
tot_soil_fluxes_man_cc_to_ne_NE <- layerOp( x=soil_fluxes_man_cc_to_ne_NE, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")
tot_soil_fluxes_man_cc_to_ne_ND <- layerOp( x=soil_fluxes_man_cc_to_ne_ND, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")
tot_soil_fluxes_man_cc_to_ne_BD <- layerOp( x=soil_fluxes_man_cc_to_ne_BD, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")
tot_soil_fluxes_man_cc_to_ne_BE <- layerOp( x=soil_fluxes_man_cc_to_ne_BE, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")

tot_soil_fluxes_man_cc_to_ne_NE_ND <- calcNewField(x=tot_soil_fluxes_man_cc_to_ne_NE, y=tot_soil_fluxes_man_cc_to_ne_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
tot_soil_fluxes_man_cc_to_ne_BE_BD <- calcNewField(x=tot_soil_fluxes_man_cc_to_ne_BE, y=tot_soil_fluxes_man_cc_to_ne_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
tot_soil_fluxes_man_cc_to_ne_forest <- calcNewField(x=tot_soil_fluxes_man_cc_to_ne_NE_ND, y=tot_soil_fluxes_man_cc_to_ne_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
tot_soil_fluxes_man_cc_to_ne_forest_mean <- aggregateYears(tot_soil_fluxes_man_cc_to_ne_forest, method = "mean" )

tot_pools_man_cc_to_ne_forest_mean <- aggregateYears(tot_cpool_man_cc_to_ne, method = "mean" )

turnover_tot_soil_man_cc_to_ne <- calcNewField( x=tot_pools_man_cc_to_ne_forest_mean, y=tot_soil_fluxes_man_cc_to_ne_forest_mean,
                            x.col="SoilC", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_tot_soil_man_cc_to_ne, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_tot_soil_man_cc_to_ne.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#.....................
#... SURF FWD POOL ...
#.....................
surffwd_fluxes_man_cc_to_ne_NE <- layerOp( x=soil_fluxes_man_cc_to_ne_NE, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")
surffwd_fluxes_man_cc_to_ne_ND <- layerOp( x=soil_fluxes_man_cc_to_ne_ND, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")
surffwd_fluxes_man_cc_to_ne_BD <- layerOp( x=soil_fluxes_man_cc_to_ne_BD, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")
surffwd_fluxes_man_cc_to_ne_BE <- layerOp( x=soil_fluxes_man_cc_to_ne_BE, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")

surffwd_fluxes_man_cc_to_ne_NE_ND <- calcNewField(x=surffwd_fluxes_man_cc_to_ne_NE, y=surffwd_fluxes_man_cc_to_ne_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surffwd_fluxes_man_cc_to_ne_BE_BD <- calcNewField(x=surffwd_fluxes_man_cc_to_ne_BE, y=surffwd_fluxes_man_cc_to_ne_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surffwd_fluxes_man_cc_to_ne_forest <- calcNewField(x=surffwd_fluxes_man_cc_to_ne_NE_ND, y=surffwd_fluxes_man_cc_to_ne_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surffwd_fluxes_man_cc_to_ne_forest_mean <- aggregateYears(surffwd_fluxes_man_cc_to_ne_forest, method = "mean" )

surffwd_pools_man_cc_to_ne_NE_ND <- calcNewField(x=soil_pools_man_cc_to_ne_NE, y=soil_pools_man_cc_to_ne_ND, x.col = "SURFFWD", y.col = "SURFFWD", op="+" )
surffwd_pools_man_cc_to_ne_BE_BD <- calcNewField(x=soil_pools_man_cc_to_ne_BE, y=soil_pools_man_cc_to_ne_BD, x.col = "SURFFWD", y.col = "SURFFWD", op="+" )
surffwd_pools_man_cc_to_ne_forest <- calcNewField(x=surffwd_pools_man_cc_to_ne_NE_ND, y=surffwd_pools_man_cc_to_ne_BE_BD, x.col = "SURFFWD", y.col = "SURFFWD", op="+" )
surffwd_pools_man_cc_to_ne_forest_mean <- aggregateYears(surffwd_pools_man_cc_to_ne_forest, method = "mean" )

turnover_surffwd_man_cc_to_ne <- calcNewField( x=surffwd_pools_man_cc_to_ne_forest_mean, y=surffwd_fluxes_man_cc_to_ne_forest_mean,
                            x.col="SURFFWD", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_surffwd_man_cc_to_ne, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_surffwd_man_cc_to_ne.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#.....................
#... SURF CWD POOL ...
#.....................
surfcwd_fluxes_man_cc_to_ne_NE <- layerOp( x=soil_fluxes_man_cc_to_ne_NE, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")
surfcwd_fluxes_man_cc_to_ne_ND <- layerOp( x=soil_fluxes_man_cc_to_ne_ND, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")
surfcwd_fluxes_man_cc_to_ne_BD <- layerOp( x=soil_fluxes_man_cc_to_ne_BD, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")
surfcwd_fluxes_man_cc_to_ne_BE <- layerOp( x=soil_fluxes_man_cc_to_ne_BE, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")

surfcwd_fluxes_man_cc_to_ne_NE_ND <- calcNewField(x=surfcwd_fluxes_man_cc_to_ne_NE, y=surfcwd_fluxes_man_cc_to_ne_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfcwd_fluxes_man_cc_to_ne_BE_BD <- calcNewField(x=surfcwd_fluxes_man_cc_to_ne_BE, y=surfcwd_fluxes_man_cc_to_ne_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfcwd_fluxes_man_cc_to_ne_forest <- calcNewField(x=surfcwd_fluxes_man_cc_to_ne_NE_ND, y=surfcwd_fluxes_man_cc_to_ne_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfcwd_fluxes_man_cc_to_ne_forest_mean <- aggregateYears(surfcwd_fluxes_man_cc_to_ne_forest, method = "mean" )

surfcwd_pools_man_cc_to_ne_NE_ND <- calcNewField(x=soil_pools_man_cc_to_ne_NE, y=soil_pools_man_cc_to_ne_ND, x.col = "SURFCWD", y.col = "SURFCWD", op="+" )
surfcwd_pools_man_cc_to_ne_BE_BD <- calcNewField(x=soil_pools_man_cc_to_ne_BE, y=soil_pools_man_cc_to_ne_BD, x.col = "SURFCWD", y.col = "SURFCWD", op="+" )
surfcwd_pools_man_cc_to_ne_forest <- calcNewField(x=surfcwd_pools_man_cc_to_ne_NE_ND, y=surfcwd_pools_man_cc_to_ne_BE_BD, x.col = "SURFCWD", y.col = "SURFCWD", op="+" )
surfcwd_pools_man_cc_to_ne_forest_mean <- aggregateYears(surfcwd_pools_man_cc_to_ne_forest, method = "mean" )

turnover_surfcwd_man_cc_to_ne <- calcNewField( x=surfcwd_pools_man_cc_to_ne_forest_mean, y=surfcwd_fluxes_man_cc_to_ne_forest_mean,
                            x.col="SURFCWD", y.col="tot_out_flux", op = "/")

#saveRDS(turnover_surfcwd_man_cc_to_ne, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_surfcwd_man_cc_to_ne.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)

#..................
#... SURF HUMUS ...
#..................
surfhum_fluxes_man_cc_to_ne_NE <- layerOp( x=soil_fluxes_man_cc_to_ne_NE, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")
surfhum_fluxes_man_cc_to_ne_ND <- layerOp( x=soil_fluxes_man_cc_to_ne_ND, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")
surfhum_fluxes_man_cc_to_ne_BD <- layerOp( x=soil_fluxes_man_cc_to_ne_BD, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")
surfhum_fluxes_man_cc_to_ne_BE <- layerOp( x=soil_fluxes_man_cc_to_ne_BE, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")

surfhum_fluxes_man_cc_to_ne_NE_ND <- calcNewField(x=surfhum_fluxes_man_cc_to_ne_NE, y=surfhum_fluxes_man_cc_to_ne_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfhum_fluxes_man_cc_to_ne_BE_BD <- calcNewField(x=surfhum_fluxes_man_cc_to_ne_BE, y=surfhum_fluxes_man_cc_to_ne_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfhum_fluxes_man_cc_to_ne_forest <- calcNewField(x=surfhum_fluxes_man_cc_to_ne_NE_ND, y=surfhum_fluxes_man_cc_to_ne_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfhum_fluxes_man_cc_to_ne_forest_mean <- aggregateYears(surfhum_fluxes_man_cc_to_ne_forest, method = "mean" )

surfhum_pools_man_cc_to_ne_NE_ND <- calcNewField(x=soil_pools_man_cc_to_ne_NE, y=soil_pools_man_cc_to_ne_ND, x.col = "SURFHUMUS", y.col = "SURFHUMUS", op="+" )
surfhum_pools_man_cc_to_ne_BE_BD <- calcNewField(x=soil_pools_man_cc_to_ne_BE, y=soil_pools_man_cc_to_ne_BD, x.col = "SURFHUMUS", y.col = "SURFHUMUS", op="+" )
surfhum_pools_man_cc_to_ne_forest <- calcNewField(x=surfhum_pools_man_cc_to_ne_NE_ND, y=surfhum_pools_man_cc_to_ne_BE_BD, x.col = "SURFHUMUS", y.col = "SURFHUMUS", op="+" )
surfhum_pools_man_cc_to_ne_forest_mean <- aggregateYears(surfhum_pools_man_cc_to_ne_forest, method = "mean" )

turnover_surfhum_man_cc_to_ne <- calcNewField( x=surfhum_pools_man_cc_to_ne_forest_mean, y=surfhum_fluxes_man_cc_to_ne_forest_mean,
                            x.col="SURFHUMUS", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_surfhum_man_cc_to_ne, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_surfhum_man_cc_to_ne.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#.....................
#... SLOW SOM POOL ...
#.....................
slowsom_fluxes_man_cc_to_ne_NE <- layerOp( x=soil_fluxes_man_cc_to_ne_NE, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")
slowsom_fluxes_man_cc_to_ne_ND <- layerOp( x=soil_fluxes_man_cc_to_ne_ND, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")
slowsom_fluxes_man_cc_to_ne_BD <- layerOp( x=soil_fluxes_man_cc_to_ne_BD, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")
slowsom_fluxes_man_cc_to_ne_BE <- layerOp( x=soil_fluxes_man_cc_to_ne_BE, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")
slowsom_fluxes_man_cc_to_ne_NE_ND <- calcNewField(x=slowsom_fluxes_man_cc_to_ne_NE, y=slowsom_fluxes_man_cc_to_ne_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
slowsom_fluxes_man_cc_to_ne_BE_BD <- calcNewField(x=slowsom_fluxes_man_cc_to_ne_BE, y=slowsom_fluxes_man_cc_to_ne_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
slowsom_fluxes_man_cc_to_ne_forest <- calcNewField(x=slowsom_fluxes_man_cc_to_ne_NE_ND, y=slowsom_fluxes_man_cc_to_ne_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
slowsom_fluxes_man_cc_to_ne_forest_mean <- aggregateYears(slowsom_fluxes_man_cc_to_ne_forest, method = "mean" )

slowsom_pools_man_cc_to_ne_NE_ND <- calcNewField(x=soil_pools_man_cc_to_ne_NE, y=soil_pools_man_cc_to_ne_ND, x.col = "SLOWSOM", y.col = "SLOWSOM", op="+" )
slowsom_pools_man_cc_to_ne_BE_BD <- calcNewField(x=soil_pools_man_cc_to_ne_BE, y=soil_pools_man_cc_to_ne_BD, x.col = "SLOWSOM", y.col = "SLOWSOM", op="+" )
slowsom_pools_man_cc_to_ne_forest <- calcNewField(x=slowsom_pools_man_cc_to_ne_NE_ND, y=slowsom_pools_man_cc_to_ne_BE_BD, x.col = "SLOWSOM", y.col = "SLOWSOM", op="+" )
slowsom_pools_man_cc_to_ne_forest_mean <- aggregateYears(slowsom_pools_man_cc_to_ne_forest, method = "mean" )

turnover_slowsom_man_cc_to_ne <- calcNewField( x=slowsom_pools_man_cc_to_ne_forest_mean, y=slowsom_fluxes_man_cc_to_ne_forest_mean,
                            x.col="SLOWSOM", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_slowsom_man_cc_to_ne, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_slowsom_man_cc_to_ne.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)

##################
#..... to BD .....
##################
tot_cpool_man_cc_to_bd <- getField(source = man_cc_to_bd, quant = "cpool_forest", first.year = 2060, last.year = 2089)

soil_pools_man_cc_to_bd_NE <- getField(source = man_cc_to_bd, quant = "cpool_all_ForestNE", first.year = 2060, last.year = 2089)
soil_pools_man_cc_to_bd_BE <- getField(source = man_cc_to_bd, quant = "cpool_all_ForestBE", first.year = 2060, last.year = 2089)
soil_pools_man_cc_to_bd_BD <- getField(source = man_cc_to_bd, quant = "cpool_all_ForestBD", first.year = 2060, last.year = 2089)
soil_pools_man_cc_to_bd_ND <- getField(source = man_cc_to_bd, quant = "cpool_all_ForestND", first.year = 2060, last.year = 2089)

soil_fluxes_man_cc_to_bd_NE <- getField(source = man_cc_to_bd, quant = "cflux_all_ForestNE", first.year = 2060, last.year = 2089)
soil_fluxes_man_cc_to_bd_BE <- getField(source = man_cc_to_bd, quant = "cflux_all_ForestBE", first.year = 2060, last.year = 2089)
soil_fluxes_man_cc_to_bd_BD <- getField(source = man_cc_to_bd, quant = "cflux_all_ForestBD", first.year = 2060, last.year = 2089)
soil_fluxes_man_cc_to_bd_ND <- getField(source = man_cc_to_bd, quant = "cflux_all_ForestND", first.year = 2060, last.year = 2089)

#........................
#....TOTAL SOIL POOL ....
#........................
tot_soil_fluxes_man_cc_to_bd_NE <- layerOp( x=soil_fluxes_man_cc_to_bd_NE, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")
tot_soil_fluxes_man_cc_to_bd_ND <- layerOp( x=soil_fluxes_man_cc_to_bd_ND, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")
tot_soil_fluxes_man_cc_to_bd_BD <- layerOp( x=soil_fluxes_man_cc_to_bd_BD, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")
tot_soil_fluxes_man_cc_to_bd_BE <- layerOp( x=soil_fluxes_man_cc_to_bd_BE, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")

tot_soil_fluxes_man_cc_to_bd_NE_ND <- calcNewField(x=tot_soil_fluxes_man_cc_to_bd_NE, y=tot_soil_fluxes_man_cc_to_bd_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
tot_soil_fluxes_man_cc_to_bd_BE_BD <- calcNewField(x=tot_soil_fluxes_man_cc_to_bd_BE, y=tot_soil_fluxes_man_cc_to_bd_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
tot_soil_fluxes_man_cc_to_bd_forest <- calcNewField(x=tot_soil_fluxes_man_cc_to_bd_NE_ND, y=tot_soil_fluxes_man_cc_to_bd_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
tot_soil_fluxes_man_cc_to_bd_forest_mean <- aggregateYears(tot_soil_fluxes_man_cc_to_bd_forest, method = "mean" )

tot_pools_man_cc_to_bd_forest_mean <- aggregateYears(tot_cpool_man_cc_to_bd, method = "mean" )

turnover_tot_soil_man_cc_to_bd <- calcNewField( x=tot_pools_man_cc_to_bd_forest_mean, y=tot_soil_fluxes_man_cc_to_bd_forest_mean,
                            x.col="SoilC", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_tot_soil_man_cc_to_bd, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_tot_soil_man_cc_to_bd.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#.....................
#... SURF FWD POOL ...
#.....................
surffwd_fluxes_man_cc_to_bd_NE <- layerOp( x=soil_fluxes_man_cc_to_bd_NE, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")
surffwd_fluxes_man_cc_to_bd_ND <- layerOp( x=soil_fluxes_man_cc_to_bd_ND, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")
surffwd_fluxes_man_cc_to_bd_BD <- layerOp( x=soil_fluxes_man_cc_to_bd_BD, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")
surffwd_fluxes_man_cc_to_bd_BE <- layerOp( x=soil_fluxes_man_cc_to_bd_BE, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")

surffwd_fluxes_man_cc_to_bd_NE_ND <- calcNewField(x=surffwd_fluxes_man_cc_to_bd_NE, y=surffwd_fluxes_man_cc_to_bd_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surffwd_fluxes_man_cc_to_bd_BE_BD <- calcNewField(x=surffwd_fluxes_man_cc_to_bd_BE, y=surffwd_fluxes_man_cc_to_bd_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surffwd_fluxes_man_cc_to_bd_forest <- calcNewField(x=surffwd_fluxes_man_cc_to_bd_NE_ND, y=surffwd_fluxes_man_cc_to_bd_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surffwd_fluxes_man_cc_to_bd_forest_mean <- aggregateYears(surffwd_fluxes_man_cc_to_bd_forest, method = "mean" )

surffwd_pools_man_cc_to_bd_NE_ND <- calcNewField(x=soil_pools_man_cc_to_bd_NE, y=soil_pools_man_cc_to_bd_ND, x.col = "SURFFWD", y.col = "SURFFWD", op="+" )
surffwd_pools_man_cc_to_bd_BE_BD <- calcNewField(x=soil_pools_man_cc_to_bd_BE, y=soil_pools_man_cc_to_bd_BD, x.col = "SURFFWD", y.col = "SURFFWD", op="+" )
surffwd_pools_man_cc_to_bd_forest <- calcNewField(x=surffwd_pools_man_cc_to_bd_NE_ND, y=surffwd_pools_man_cc_to_bd_BE_BD, x.col = "SURFFWD", y.col = "SURFFWD", op="+" )
surffwd_pools_man_cc_to_bd_forest_mean <- aggregateYears(surffwd_pools_man_cc_to_bd_forest, method = "mean" )

turnover_surffwd_man_cc_to_bd <- calcNewField( x=surffwd_pools_man_cc_to_bd_forest_mean, y=surffwd_fluxes_man_cc_to_bd_forest_mean,
                            x.col="SURFFWD", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_surffwd_man_cc_to_bd, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_surffwd_man_cc_to_bd.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#.....................
#... SURF CWD POOL ...
#.....................
surfcwd_fluxes_man_cc_to_bd_NE <- layerOp( x=soil_fluxes_man_cc_to_bd_NE, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")
surfcwd_fluxes_man_cc_to_bd_ND <- layerOp( x=soil_fluxes_man_cc_to_bd_ND, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")
surfcwd_fluxes_man_cc_to_bd_BD <- layerOp( x=soil_fluxes_man_cc_to_bd_BD, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")
surfcwd_fluxes_man_cc_to_bd_BE <- layerOp( x=soil_fluxes_man_cc_to_bd_BE, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")

surfcwd_fluxes_man_cc_to_bd_NE_ND <- calcNewField(x=surfcwd_fluxes_man_cc_to_bd_NE, y=surfcwd_fluxes_man_cc_to_bd_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfcwd_fluxes_man_cc_to_bd_BE_BD <- calcNewField(x=surfcwd_fluxes_man_cc_to_bd_BE, y=surfcwd_fluxes_man_cc_to_bd_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfcwd_fluxes_man_cc_to_bd_forest <- calcNewField(x=surfcwd_fluxes_man_cc_to_bd_NE_ND, y=surfcwd_fluxes_man_cc_to_bd_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfcwd_fluxes_man_cc_to_bd_forest_mean <- aggregateYears(surfcwd_fluxes_man_cc_to_bd_forest, method = "mean" )

surfcwd_pools_man_cc_to_bd_NE_ND <- calcNewField(x=soil_pools_man_cc_to_bd_NE, y=soil_pools_man_cc_to_bd_ND, x.col = "SURFCWD", y.col = "SURFCWD", op="+" )
surfcwd_pools_man_cc_to_bd_BE_BD <- calcNewField(x=soil_pools_man_cc_to_bd_BE, y=soil_pools_man_cc_to_bd_BD, x.col = "SURFCWD", y.col = "SURFCWD", op="+" )
surfcwd_pools_man_cc_to_bd_forest <- calcNewField(x=surfcwd_pools_man_cc_to_bd_NE_ND, y=surfcwd_pools_man_cc_to_bd_BE_BD, x.col = "SURFCWD", y.col = "SURFCWD", op="+" )
surfcwd_pools_man_cc_to_bd_forest_mean <- aggregateYears(surfcwd_pools_man_cc_to_bd_forest, method = "mean" )

turnover_surfcwd_man_cc_to_bd <- calcNewField( x=surfcwd_pools_man_cc_to_bd_forest_mean, y=surfcwd_fluxes_man_cc_to_bd_forest_mean,
                            x.col="SURFCWD", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_surfcwd_man_cc_to_bd, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_surfcwd_man_cc_to_bd.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#..................
#... SURF HUMUS ...
#..................
surfhum_fluxes_man_cc_to_bd_NE <- layerOp( x=soil_fluxes_man_cc_to_bd_NE, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")
surfhum_fluxes_man_cc_to_bd_ND <- layerOp( x=soil_fluxes_man_cc_to_bd_ND, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")
surfhum_fluxes_man_cc_to_bd_BD <- layerOp( x=soil_fluxes_man_cc_to_bd_BD, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")
surfhum_fluxes_man_cc_to_bd_BE <- layerOp( x=soil_fluxes_man_cc_to_bd_BE, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")

surfhum_fluxes_man_cc_to_bd_NE_ND <- calcNewField(x=surfhum_fluxes_man_cc_to_bd_NE, y=surfhum_fluxes_man_cc_to_bd_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfhum_fluxes_man_cc_to_bd_BE_BD <- calcNewField(x=surfhum_fluxes_man_cc_to_bd_BE, y=surfhum_fluxes_man_cc_to_bd_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfhum_fluxes_man_cc_to_bd_forest <- calcNewField(x=surfhum_fluxes_man_cc_to_bd_NE_ND, y=surfhum_fluxes_man_cc_to_bd_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfhum_fluxes_man_cc_to_bd_forest_mean <- aggregateYears(surfhum_fluxes_man_cc_to_bd_forest, method = "mean" )

surfhum_pools_man_cc_to_bd_NE_ND <- calcNewField(x=soil_pools_man_cc_to_bd_NE, y=soil_pools_man_cc_to_bd_ND, x.col = "SURFHUMUS", y.col = "SURFHUMUS", op="+" )
surfhum_pools_man_cc_to_bd_BE_BD <- calcNewField(x=soil_pools_man_cc_to_bd_BE, y=soil_pools_man_cc_to_bd_BD, x.col = "SURFHUMUS", y.col = "SURFHUMUS", op="+" )
surfhum_pools_man_cc_to_bd_forest <- calcNewField(x=surfhum_pools_man_cc_to_bd_NE_ND, y=surfhum_pools_man_cc_to_bd_BE_BD, x.col = "SURFHUMUS", y.col = "SURFHUMUS", op="+" )
surfhum_pools_man_cc_to_bd_forest_mean <- aggregateYears(surfhum_pools_man_cc_to_bd_forest, method = "mean" )

turnover_surfhum_man_cc_to_bd <- calcNewField( x=surfhum_pools_man_cc_to_bd_forest_mean, y=surfhum_fluxes_man_cc_to_bd_forest_mean,
                            x.col="SURFHUMUS", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_surfhum_man_cc_to_bd, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_surfhum_man_cc_to_bd.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#.....................
#... SLOW SOM POOL ...
#.....................
slowsom_fluxes_man_cc_to_bd_NE <- layerOp( x=soil_fluxes_man_cc_to_bd_NE, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")
slowsom_fluxes_man_cc_to_bd_ND <- layerOp( x=soil_fluxes_man_cc_to_bd_ND, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")
slowsom_fluxes_man_cc_to_bd_BD <- layerOp( x=soil_fluxes_man_cc_to_bd_BD, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")
slowsom_fluxes_man_cc_to_bd_BE <- layerOp( x=soil_fluxes_man_cc_to_bd_BE, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")

slowsom_fluxes_man_cc_to_bd_NE_ND <- calcNewField(x=slowsom_fluxes_man_cc_to_bd_NE, y=slowsom_fluxes_man_cc_to_bd_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
slowsom_fluxes_man_cc_to_bd_BE_BD <- calcNewField(x=slowsom_fluxes_man_cc_to_bd_BE, y=slowsom_fluxes_man_cc_to_bd_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
slowsom_fluxes_man_cc_to_bd_forest <- calcNewField(x=slowsom_fluxes_man_cc_to_bd_NE_ND, y=slowsom_fluxes_man_cc_to_bd_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
slowsom_fluxes_man_cc_to_bd_forest_mean <- aggregateYears(slowsom_fluxes_man_cc_to_bd_forest, method = "mean" )

slowsom_pools_man_cc_to_bd_NE_ND <- calcNewField(x=soil_pools_man_cc_to_bd_NE, y=soil_pools_man_cc_to_bd_ND, x.col = "SLOWSOM", y.col = "SLOWSOM", op="+" )
slowsom_pools_man_cc_to_bd_BE_BD <- calcNewField(x=soil_pools_man_cc_to_bd_BE, y=soil_pools_man_cc_to_bd_BD, x.col = "SLOWSOM", y.col = "SLOWSOM", op="+" )
slowsom_pools_man_cc_to_bd_forest <- calcNewField(x=slowsom_pools_man_cc_to_bd_NE_ND, y=slowsom_pools_man_cc_to_bd_BE_BD, x.col = "SLOWSOM", y.col = "SLOWSOM", op="+" )
slowsom_pools_man_cc_to_bd_forest_mean <- aggregateYears(slowsom_pools_man_cc_to_bd_forest, method = "mean" )

turnover_slowsom_man_cc_to_bd <- calcNewField( x=slowsom_pools_man_cc_to_bd_forest_mean, y=slowsom_fluxes_man_cc_to_bd_forest_mean,
                            x.col="SLOWSOM", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_slowsom_man_cc_to_bd, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_slowsom_man_cc_to_bd.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)

################
#.... to BE ....
################
tot_cpool_man_cc_to_be <- getField(source = man_cc_to_be, quant = "cpool_forest", first.year = 2060, last.year = 2089)

soil_pools_man_cc_to_be_NE <- getField(source = man_cc_to_be, quant = "cpool_all_ForestNE", first.year = 2060, last.year = 2089)
soil_pools_man_cc_to_be_BE <- getField(source = man_cc_to_be, quant = "cpool_all_ForestBE", first.year = 2060, last.year = 2089)
soil_pools_man_cc_to_be_BD <- getField(source = man_cc_to_be, quant = "cpool_all_ForestBD", first.year = 2060, last.year = 2089)
soil_pools_man_cc_to_be_ND <- getField(source = man_cc_to_be, quant = "cpool_all_ForestND", first.year = 2060, last.year = 2089)

soil_fluxes_man_cc_to_be_NE <- getField(source = man_cc_to_be, quant = "cflux_all_ForestNE", first.year = 2060, last.year = 2089)
soil_fluxes_man_cc_to_be_BE <- getField(source = man_cc_to_be, quant = "cflux_all_ForestBE", first.year = 2060, last.year = 2089)
soil_fluxes_man_cc_to_be_BD <- getField(source = man_cc_to_be, quant = "cflux_all_ForestBD", first.year = 2060, last.year = 2089)
soil_fluxes_man_cc_to_be_ND <- getField(source = man_cc_to_be, quant = "cflux_all_ForestND", first.year = 2060, last.year = 2089)

#.......................
#... TOTAL SOIL POOL ...
#.......................
tot_soil_fluxes_man_cc_to_be_NE <- layerOp( x=soil_fluxes_man_cc_to_be_NE, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")
tot_soil_fluxes_man_cc_to_be_ND <- layerOp( x=soil_fluxes_man_cc_to_be_ND, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")
tot_soil_fluxes_man_cc_to_be_BD <- layerOp( x=soil_fluxes_man_cc_to_be_BD, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")
tot_soil_fluxes_man_cc_to_be_BE <- layerOp( x=soil_fluxes_man_cc_to_be_BE, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")

tot_soil_fluxes_man_cc_to_be_NE_ND <- calcNewField(x=tot_soil_fluxes_man_cc_to_be_NE, y=tot_soil_fluxes_man_cc_to_be_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
tot_soil_fluxes_man_cc_to_be_BE_BD <- calcNewField(x=tot_soil_fluxes_man_cc_to_be_BE, y=tot_soil_fluxes_man_cc_to_be_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
tot_soil_fluxes_man_cc_to_be_forest <- calcNewField(x=tot_soil_fluxes_man_cc_to_be_NE_ND, y=tot_soil_fluxes_man_cc_to_be_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
tot_soil_fluxes_man_cc_to_be_forest_mean <- aggregateYears(tot_soil_fluxes_man_cc_to_be_forest, method = "mean" )

tot_pools_man_cc_to_be_forest_mean <- aggregateYears(tot_cpool_man_cc_to_be, method = "mean" )

turnover_tot_soil_man_cc_to_be <- calcNewField( x=tot_pools_man_cc_to_be_forest_mean, y=tot_soil_fluxes_man_cc_to_be_forest_mean,
                            x.col="SoilC", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_tot_soil_man_cc_to_be, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_tot_soil_man_cc_to_be.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#.....................
#... SURF FWD POOL ...
#.....................
surffwd_fluxes_man_cc_to_be_NE <- layerOp( x=soil_fluxes_man_cc_to_be_NE, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")
surffwd_fluxes_man_cc_to_be_ND <- layerOp( x=soil_fluxes_man_cc_to_be_ND, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")
surffwd_fluxes_man_cc_to_be_BD <- layerOp( x=soil_fluxes_man_cc_to_be_BD, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")
surffwd_fluxes_man_cc_to_be_BE <- layerOp( x=soil_fluxes_man_cc_to_be_BE, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")

surffwd_fluxes_man_cc_to_be_NE_ND <- calcNewField(x=surffwd_fluxes_man_cc_to_be_NE, y=surffwd_fluxes_man_cc_to_be_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surffwd_fluxes_man_cc_to_be_BE_BD <- calcNewField(x=surffwd_fluxes_man_cc_to_be_BE, y=surffwd_fluxes_man_cc_to_be_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surffwd_fluxes_man_cc_to_be_forest <- calcNewField(x=surffwd_fluxes_man_cc_to_be_NE_ND, y=surffwd_fluxes_man_cc_to_be_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surffwd_fluxes_man_cc_to_be_forest_mean <- aggregateYears(surffwd_fluxes_man_cc_to_be_forest, method = "mean" )

surffwd_pools_man_cc_to_be_NE_ND <- calcNewField(x=soil_pools_man_cc_to_be_NE, y=soil_pools_man_cc_to_be_ND, x.col = "SURFFWD", y.col = "SURFFWD", op="+" )
surffwd_pools_man_cc_to_be_BE_BD <- calcNewField(x=soil_pools_man_cc_to_be_BE, y=soil_pools_man_cc_to_be_BD, x.col = "SURFFWD", y.col = "SURFFWD", op="+" )
surffwd_pools_man_cc_to_be_forest <- calcNewField(x=surffwd_pools_man_cc_to_be_NE_ND, y=surffwd_pools_man_cc_to_be_BE_BD, x.col = "SURFFWD", y.col = "SURFFWD", op="+" )
surffwd_pools_man_cc_to_be_forest_mean <- aggregateYears(surffwd_pools_man_cc_to_be_forest, method = "mean" )

turnover_surffwd_man_cc_to_be <- calcNewField( x=surffwd_pools_man_cc_to_be_forest_mean, y=surffwd_fluxes_man_cc_to_be_forest_mean,
                            x.col="SURFFWD", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_surffwd_man_cc_to_be, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_surffwd_man_cc_to_be.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#.....................
#... SURF CWD POOL ...
#.....................
surfcwd_fluxes_man_cc_to_be_NE <- layerOp( x=soil_fluxes_man_cc_to_be_NE, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")
surfcwd_fluxes_man_cc_to_be_ND <- layerOp( x=soil_fluxes_man_cc_to_be_ND, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")
surfcwd_fluxes_man_cc_to_be_BD <- layerOp( x=soil_fluxes_man_cc_to_be_BD, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")
surfcwd_fluxes_man_cc_to_be_BE <- layerOp( x=soil_fluxes_man_cc_to_be_BE, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")

surfcwd_fluxes_man_cc_to_be_NE_ND <- calcNewField(x=surfcwd_fluxes_man_cc_to_be_NE, y=surfcwd_fluxes_man_cc_to_be_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfcwd_fluxes_man_cc_to_be_BE_BD <- calcNewField(x=surfcwd_fluxes_man_cc_to_be_BE, y=surfcwd_fluxes_man_cc_to_be_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfcwd_fluxes_man_cc_to_be_forest <- calcNewField(x=surfcwd_fluxes_man_cc_to_be_NE_ND, y=surfcwd_fluxes_man_cc_to_be_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfcwd_fluxes_man_cc_to_be_forest_mean <- aggregateYears(surfcwd_fluxes_man_cc_to_be_forest, method = "mean" )

surfcwd_pools_man_cc_to_be_NE_ND <- calcNewField(x=soil_pools_man_cc_to_be_NE, y=soil_pools_man_cc_to_be_ND, x.col = "SURFCWD", y.col = "SURFCWD", op="+" )
surfcwd_pools_man_cc_to_be_BE_BD <- calcNewField(x=soil_pools_man_cc_to_be_BE, y=soil_pools_man_cc_to_be_BD, x.col = "SURFCWD", y.col = "SURFCWD", op="+" )
surfcwd_pools_man_cc_to_be_forest <- calcNewField(x=surfcwd_pools_man_cc_to_be_NE_ND, y=surfcwd_pools_man_cc_to_be_BE_BD, x.col = "SURFCWD", y.col = "SURFCWD", op="+" )
surfcwd_pools_man_cc_to_be_forest_mean <- aggregateYears(surfcwd_pools_man_cc_to_be_forest, method = "mean" )

turnover_surfcwd_man_cc_to_be <- calcNewField( x=surfcwd_pools_man_cc_to_be_forest_mean, y=surfcwd_fluxes_man_cc_to_be_forest_mean,
                            x.col="SURFCWD", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_surfcwd_man_cc_to_be, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_surfcwd_man_cc_to_be.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#..................
#... SURF HUMUS ...
#..................
surfhum_fluxes_man_cc_to_be_NE <- layerOp( x=soil_fluxes_man_cc_to_be_NE, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")
surfhum_fluxes_man_cc_to_be_ND <- layerOp( x=soil_fluxes_man_cc_to_be_ND, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")
surfhum_fluxes_man_cc_to_be_BD <- layerOp( x=soil_fluxes_man_cc_to_be_BD, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")
surfhum_fluxes_man_cc_to_be_BE <- layerOp( x=soil_fluxes_man_cc_to_be_BE, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")

surfhum_fluxes_man_cc_to_be_NE_ND <- calcNewField(x=surfhum_fluxes_man_cc_to_be_NE, y=surfhum_fluxes_man_cc_to_be_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfhum_fluxes_man_cc_to_be_BE_BD <- calcNewField(x=surfhum_fluxes_man_cc_to_be_BE, y=surfhum_fluxes_man_cc_to_be_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfhum_fluxes_man_cc_to_be_forest <- calcNewField(x=surfhum_fluxes_man_cc_to_be_NE_ND, y=surfhum_fluxes_man_cc_to_be_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfhum_fluxes_man_cc_to_be_forest_mean <- aggregateYears(surfhum_fluxes_man_cc_to_be_forest, method = "mean" )

surfhum_pools_man_cc_to_be_NE_ND <- calcNewField(x=soil_pools_man_cc_to_be_NE, y=soil_pools_man_cc_to_be_ND, x.col = "SURFHUMUS", y.col = "SURFHUMUS", op="+" )
surfhum_pools_man_cc_to_be_BE_BD <- calcNewField(x=soil_pools_man_cc_to_be_BE, y=soil_pools_man_cc_to_be_BD, x.col = "SURFHUMUS", y.col = "SURFHUMUS", op="+" )
surfhum_pools_man_cc_to_be_forest <- calcNewField(x=surfhum_pools_man_cc_to_be_NE_ND, y=surfhum_pools_man_cc_to_be_BE_BD, x.col = "SURFHUMUS", y.col = "SURFHUMUS", op="+" )
surfhum_pools_man_cc_to_be_forest_mean <- aggregateYears(surfhum_pools_man_cc_to_be_forest, method = "mean" )

turnover_surfhum_man_cc_to_be <- calcNewField( x=surfhum_pools_man_cc_to_be_forest_mean, y=surfhum_fluxes_man_cc_to_be_forest_mean,
                            x.col="SURFHUMUS", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_surfhum_man_cc_to_be, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_surfhum_man_cc_to_be.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#.....................
#... SLOW SOM POOL ...
#.....................
slowsom_fluxes_man_cc_to_be_NE <- layerOp( x=soil_fluxes_man_cc_to_be_NE, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")
slowsom_fluxes_man_cc_to_be_ND <- layerOp( x=soil_fluxes_man_cc_to_be_ND, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")
slowsom_fluxes_man_cc_to_be_BD <- layerOp( x=soil_fluxes_man_cc_to_be_BD, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")
slowsom_fluxes_man_cc_to_be_BE <- layerOp( x=soil_fluxes_man_cc_to_be_BE, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")

slowsom_fluxes_man_cc_to_be_NE_ND <- calcNewField(x=slowsom_fluxes_man_cc_to_be_NE, y=slowsom_fluxes_man_cc_to_be_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
slowsom_fluxes_man_cc_to_be_BE_BD <- calcNewField(x=slowsom_fluxes_man_cc_to_be_BE, y=slowsom_fluxes_man_cc_to_be_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
slowsom_fluxes_man_cc_to_be_forest <- calcNewField(x=slowsom_fluxes_man_cc_to_be_NE_ND, y=slowsom_fluxes_man_cc_to_be_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
slowsom_fluxes_man_cc_to_be_forest_mean <- aggregateYears(slowsom_fluxes_man_cc_to_be_forest, method = "mean" )

slowsom_pools_man_cc_to_be_NE_ND <- calcNewField(x=soil_pools_man_cc_to_be_NE, y=soil_pools_man_cc_to_be_ND, x.col = "SLOWSOM", y.col = "SLOWSOM", op="+" )
slowsom_pools_man_cc_to_be_BE_BD <- calcNewField(x=soil_pools_man_cc_to_be_BE, y=soil_pools_man_cc_to_be_BD, x.col = "SLOWSOM", y.col = "SLOWSOM", op="+" )
slowsom_pools_man_cc_to_be_forest <- calcNewField(x=slowsom_pools_man_cc_to_be_NE_ND, y=slowsom_pools_man_cc_to_be_BE_BD, x.col = "SLOWSOM", y.col = "SLOWSOM", op="+" )
slowsom_pools_man_cc_to_be_forest_mean <- aggregateYears(slowsom_pools_man_cc_to_be_forest, method = "mean" )

turnover_slowsom_man_cc_to_be <- calcNewField( x=slowsom_pools_man_cc_to_be_forest_mean, y=slowsom_fluxes_man_cc_to_be_forest_mean,
                            x.col="SLOWSOM", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_slowsom_man_cc_to_be, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_slowsom_man_cc_to_be.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)

#####################
#..... baseline .....
#####################
tot_cpool_man_cc_base <- getField(source = man_cc_base, quant = "cpool_forest", first.year = 2060, last.year = 2089)

soil_pools_man_cc_base_NE <- getField(source = man_cc_base, quant = "cpool_all_ForestNE", first.year = 2060, last.year = 2089)
soil_pools_man_cc_base_BE <- getField(source = man_cc_base, quant = "cpool_all_ForestBE", first.year = 2060, last.year = 2089)
soil_pools_man_cc_base_BD <- getField(source = man_cc_base, quant = "cpool_all_ForestBD", first.year = 2060, last.year = 2089)
soil_pools_man_cc_base_ND <- getField(source = man_cc_base, quant = "cpool_all_ForestND", first.year = 2060, last.year = 2089)

soil_fluxes_man_cc_base_NE <- getField(source = man_cc_base, quant = "cflux_all_ForestNE", first.year = 2060, last.year = 2089)
soil_fluxes_man_cc_base_BE <- getField(source = man_cc_base, quant = "cflux_all_ForestBE", first.year = 2060, last.year = 2089)
soil_fluxes_man_cc_base_BD <- getField(source = man_cc_base, quant = "cflux_all_ForestBD", first.year = 2060, last.year = 2089)
soil_fluxes_man_cc_base_ND <- getField(source = man_cc_base, quant = "cflux_all_ForestND", first.year = 2060, last.year = 2089)
#.......................
#... TOTAL SOIL POOL ...
#.......................
tot_soil_fluxes_man_cc_base_NE <- layerOp( x=soil_fluxes_man_cc_base_NE, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")
tot_soil_fluxes_man_cc_base_ND <- layerOp( x=soil_fluxes_man_cc_base_ND, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")
tot_soil_fluxes_man_cc_base_BD <- layerOp( x=soil_fluxes_man_cc_base_BD, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")
tot_soil_fluxes_man_cc_base_BE <- layerOp( x=soil_fluxes_man_cc_base_BE, operator = "+", 
                            layers = c("SURFSTRUCTATMO", "SURFMETAATMO","SOILSTRUCTATMO", "SOILMETAATMO", "SURFFWDATMO", "SURFCWDATMO", "SURFMICROATMO", "SURFHUMUSATMO", "SLOWSOMATMO", "SOILMICROATMO", "PASSIVESOMATMO", "FIREATMO"), new.layer = "tot_out_flux")

tot_soil_fluxes_man_cc_base_NE_ND <- calcNewField(x=tot_soil_fluxes_man_cc_base_NE, y=tot_soil_fluxes_man_cc_base_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
tot_soil_fluxes_man_cc_base_BE_BD <- calcNewField(x=tot_soil_fluxes_man_cc_base_BE, y=tot_soil_fluxes_man_cc_base_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
tot_soil_fluxes_man_cc_base_forest <- calcNewField(x=tot_soil_fluxes_man_cc_base_NE_ND, y=tot_soil_fluxes_man_cc_base_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
tot_soil_fluxes_man_cc_base_forest_mean <- aggregateYears(tot_soil_fluxes_man_cc_base_forest, method = "mean" )

tot_pools_man_cc_base_forest_mean <- aggregateYears(tot_cpool_man_cc_base, method = "mean" )

turnover_tot_soil_man_cc_base <- calcNewField( x=tot_pools_man_cc_base_forest_mean, y=tot_soil_fluxes_man_cc_base_forest_mean,
                            x.col="SoilC", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_tot_soil_man_cc_base, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_tot_soil_man_cc_base.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#.....................
#... SURF FWD POOL ...
#.....................
surffwd_fluxes_man_cc_base_NE <- layerOp( x=soil_fluxes_man_cc_base_NE, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")
surffwd_fluxes_man_cc_base_ND <- layerOp( x=soil_fluxes_man_cc_base_ND, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")
surffwd_fluxes_man_cc_base_BD <- layerOp( x=soil_fluxes_man_cc_base_BD, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")
surffwd_fluxes_man_cc_base_BE <- layerOp( x=soil_fluxes_man_cc_base_BE, operator = "+", 
                            layers = c("SURFFWDATMO", "SURFFWDSURFMICRO", "SURFFWDSURFHUMUS"), new.layer = "tot_out_flux")

surffwd_fluxes_man_cc_base_NE_ND <- calcNewField(x=surffwd_fluxes_man_cc_base_NE, y=surffwd_fluxes_man_cc_base_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surffwd_fluxes_man_cc_base_BE_BD <- calcNewField(x=surffwd_fluxes_man_cc_base_BE, y=surffwd_fluxes_man_cc_base_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surffwd_fluxes_man_cc_base_forest <- calcNewField(x=surffwd_fluxes_man_cc_base_NE_ND, y=surffwd_fluxes_man_cc_base_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surffwd_fluxes_man_cc_base_forest_mean <- aggregateYears(surffwd_fluxes_man_cc_base_forest, method = "mean" )

surffwd_pools_man_cc_base_NE_ND <- calcNewField(x=soil_pools_man_cc_base_NE, y=soil_pools_man_cc_base_ND, x.col = "SURFFWD", y.col = "SURFFWD", op="+" )
surffwd_pools_man_cc_base_BE_BD <- calcNewField(x=soil_pools_man_cc_base_BE, y=soil_pools_man_cc_base_BD, x.col = "SURFFWD", y.col = "SURFFWD", op="+" )
surffwd_pools_man_cc_base_forest <- calcNewField(x=surffwd_pools_man_cc_base_NE_ND, y=surffwd_pools_man_cc_base_BE_BD, x.col = "SURFFWD", y.col = "SURFFWD", op="+" )
surffwd_pools_man_cc_base_forest_mean <- aggregateYears(surffwd_pools_man_cc_base_forest, method = "mean" )

turnover_surffwd_man_cc_base <- calcNewField( x=surffwd_pools_man_cc_base_forest_mean, y=surffwd_fluxes_man_cc_base_forest_mean,
                            x.col="SURFFWD", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_surffwd_man_cc_base, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_surffwd_man_cc_base.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#.....................
#... SURF CWD POOL ...
#.....................
surfcwd_fluxes_man_cc_base_NE <- layerOp( x=soil_fluxes_man_cc_base_NE, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")
surfcwd_fluxes_man_cc_base_ND <- layerOp( x=soil_fluxes_man_cc_base_ND, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")
surfcwd_fluxes_man_cc_base_BD <- layerOp( x=soil_fluxes_man_cc_base_BD, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")
surfcwd_fluxes_man_cc_base_BE <- layerOp( x=soil_fluxes_man_cc_base_BE, operator = "+", 
                            layers = c("SURFCWDATMO", "SURFCWDSURFMICRO", "SURFCWDSURFHUMUS"), new.layer = "tot_out_flux")

surfcwd_fluxes_man_cc_base_NE_ND <- calcNewField(x=surfcwd_fluxes_man_cc_base_NE, y=surfcwd_fluxes_man_cc_base_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfcwd_fluxes_man_cc_base_BE_BD <- calcNewField(x=surfcwd_fluxes_man_cc_base_BE, y=surfcwd_fluxes_man_cc_base_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfcwd_fluxes_man_cc_base_forest <- calcNewField(x=surfcwd_fluxes_man_cc_base_NE_ND, y=surfcwd_fluxes_man_cc_base_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfcwd_fluxes_man_cc_base_forest_mean <- aggregateYears(surfcwd_fluxes_man_cc_base_forest, method = "mean" )

surfcwd_pools_man_cc_base_NE_ND <- calcNewField(x=soil_pools_man_cc_base_NE, y=soil_pools_man_cc_base_ND, x.col = "SURFCWD", y.col = "SURFCWD", op="+" )
surfcwd_pools_man_cc_base_BE_BD <- calcNewField(x=soil_pools_man_cc_base_BE, y=soil_pools_man_cc_base_BD, x.col = "SURFCWD", y.col = "SURFCWD", op="+" )
surfcwd_pools_man_cc_base_forest <- calcNewField(x=surfcwd_pools_man_cc_base_NE_ND, y=surfcwd_pools_man_cc_base_BE_BD, x.col = "SURFCWD", y.col = "SURFCWD", op="+" )
surfcwd_pools_man_cc_base_forest_mean <- aggregateYears(surfcwd_pools_man_cc_base_forest, method = "mean" )

turnover_surfcwd_man_cc_base <- calcNewField( x=surfcwd_pools_man_cc_base_forest_mean, y=surfcwd_fluxes_man_cc_base_forest_mean,
                            x.col="SURFCWD", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_surfcwd_man_cc_base, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_surfcwd_man_cc_base.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#..................
#... SURF HUMUS ...
#..................
surfhum_fluxes_man_cc_base_NE <- layerOp( x=soil_fluxes_man_cc_base_NE, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")
surfhum_fluxes_man_cc_base_ND <- layerOp( x=soil_fluxes_man_cc_base_ND, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")
surfhum_fluxes_man_cc_base_BD <- layerOp( x=soil_fluxes_man_cc_base_BD, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")
surfhum_fluxes_man_cc_base_BE <- layerOp( x=soil_fluxes_man_cc_base_BE, operator = "+", 
                            layers = c("SURFHUMUSATMO", "SURFHUMUSSLOWSOM"), new.layer = "tot_out_flux")

surfhum_fluxes_man_cc_base_NE_ND <- calcNewField(x=surfhum_fluxes_man_cc_base_NE, y=surfhum_fluxes_man_cc_base_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfhum_fluxes_man_cc_base_BE_BD <- calcNewField(x=surfhum_fluxes_man_cc_base_BE, y=surfhum_fluxes_man_cc_base_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfhum_fluxes_man_cc_base_forest <- calcNewField(x=surfhum_fluxes_man_cc_base_NE_ND, y=surfhum_fluxes_man_cc_base_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
surfhum_fluxes_man_cc_base_forest_mean <- aggregateYears(surfhum_fluxes_man_cc_base_forest, method = "mean" )

surfhum_pools_man_cc_base_NE_ND <- calcNewField(x=soil_pools_man_cc_base_NE, y=soil_pools_man_cc_base_ND, x.col = "SURFHUMUS", y.col = "SURFHUMUS", op="+" )
surfhum_pools_man_cc_base_BE_BD <- calcNewField(x=soil_pools_man_cc_base_BE, y=soil_pools_man_cc_base_BD, x.col = "SURFHUMUS", y.col = "SURFHUMUS", op="+" )
surfhum_pools_man_cc_base_forest <- calcNewField(x=surfhum_pools_man_cc_base_NE_ND, y=surfhum_pools_man_cc_base_BE_BD, x.col = "SURFHUMUS", y.col = "SURFHUMUS", op="+" )
surfhum_pools_man_cc_base_forest_mean <- aggregateYears(surfhum_pools_man_cc_base_forest, method = "mean" )

turnover_surfhum_man_cc_base <- calcNewField( x=surfhum_pools_man_cc_base_forest_mean, y=surfhum_fluxes_man_cc_base_forest_mean,
                            x.col="SURFHUMUS", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_surfhum_man_cc_base, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_surfhum_man_cc_base.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)

#.....................
#... SLOW SOM POOL ...
#.....................
slowsom_fluxes_man_cc_base_NE <- layerOp( x=soil_fluxes_man_cc_base_NE, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")
slowsom_fluxes_man_cc_base_ND <- layerOp( x=soil_fluxes_man_cc_base_ND, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")
slowsom_fluxes_man_cc_base_BD <- layerOp( x=soil_fluxes_man_cc_base_BD, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")
slowsom_fluxes_man_cc_base_BE <- layerOp( x=soil_fluxes_man_cc_base_BE, operator = "+", 
                            layers = c("SLOWSOMATMO", "SLOWSOMSOILMICRO", "SLOWSOMPASSIVESOM"), new.layer = "tot_out_flux")

slowsom_fluxes_man_cc_base_NE_ND <- calcNewField(x=slowsom_fluxes_man_cc_base_NE, y=slowsom_fluxes_man_cc_base_ND, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
slowsom_fluxes_man_cc_base_BE_BD <- calcNewField(x=slowsom_fluxes_man_cc_base_BE, y=slowsom_fluxes_man_cc_base_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
slowsom_fluxes_man_cc_base_forest <- calcNewField(x=slowsom_fluxes_man_cc_base_NE_ND, y=slowsom_fluxes_man_cc_base_BE_BD, x.col = "tot_out_flux", y.col = "tot_out_flux", op="+" )
slowsom_fluxes_man_cc_base_forest_mean <- aggregateYears(slowsom_fluxes_man_cc_base_forest, method = "mean" )

slowsom_pools_man_cc_base_NE_ND <- calcNewField(x=soil_pools_man_cc_base_NE, y=soil_pools_man_cc_base_ND, x.col = "SLOWSOM", y.col = "SLOWSOM", op="+" )
slowsom_pools_man_cc_base_BE_BD <- calcNewField(x=soil_pools_man_cc_base_BE, y=soil_pools_man_cc_base_BD, x.col = "SLOWSOM", y.col = "SLOWSOM", op="+" )
slowsom_pools_man_cc_base_forest <- calcNewField(x=slowsom_pools_man_cc_base_NE_ND, y=slowsom_pools_man_cc_base_BE_BD, x.col = "SLOWSOM", y.col = "SLOWSOM", op="+" )
slowsom_pools_man_cc_base_forest_mean <- aggregateYears(slowsom_pools_man_cc_base_forest, method = "mean" )

turnover_slowsom_man_cc_base <- calcNewField( x=slowsom_pools_man_cc_base_forest_mean, y=slowsom_fluxes_man_cc_base_forest_mean,
                            x.col="SLOWSOM", y.col="tot_out_flux", op = "/")
#saveRDS(turnover_slowsom_man_cc_base, file="C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_slowsom_man_cc_base.RData", ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)

########################################################################################
########################################################################################

#############################################################
# calculate differences between baseline and different FM options
#############################################################
turnover_tot_soil_man_cc_unm <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_tot_soil_man_cc_unm)
                        
turnover_tot_soil_man_cc_to_ne <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_tot_soil_man_cc_to_ne)
turnover_tot_soil_man_cc_to_bd <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_tot_soil_man_cc_to_bd)
turnover_tot_soil_man_cc_to_be <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_tot_soil_man_cc_to_be)
turnover_tot_soil_man_cc_base <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_tot_soil_man_cc_base)

turnover_tot_soil_man_cc_unm_df <- as.data.frame(turnover_tot_soil_man_cc_unm)
colnames(turnover_tot_soil_man_cc_unm_df) <- c("Lon", "Lat", "turn_unm") 
turnover_tot_soil_man_cc_to_ne_df <- as.data.frame(turnover_tot_soil_man_cc_to_ne)
colnames(turnover_tot_soil_man_cc_to_ne_df) <- c("Lon", "Lat", "turn_tone") #change col names not to have spaces
turnover_tot_soil_man_cc_to_bd_df <- as.data.frame(turnover_tot_soil_man_cc_to_bd)
colnames(turnover_tot_soil_man_cc_to_bd_df) <- c("Lon", "Lat", "turn_tobd") #change col names not to have spaces
turnover_tot_soil_man_cc_to_be_df <- as.data.frame(turnover_tot_soil_man_cc_to_be)
colnames(turnover_tot_soil_man_cc_to_be_df) <- c("Lon", "Lat", "turn_tobe") #change col names not to have spaces
turnover_tot_soil_man_cc_base_df <- as.data.frame(turnover_tot_soil_man_cc_base)
colnames(turnover_tot_soil_man_cc_base_df) <- c("Lon", "Lat", "turn_base") #change col names not to have spaces

turnover_tot_soil_man_cc_list <- list(turnover_tot_soil_man_cc_unm_df, 
                                            turnover_tot_soil_man_cc_to_ne_df,
                                            turnover_tot_soil_man_cc_to_bd_df,
                                            turnover_tot_soil_man_cc_to_be_df,
                                            turnover_tot_soil_man_cc_base_df
                                            )
turnover_tot_soil_man_cc_all <- reduce(turnover_tot_soil_man_cc_list, full_join, by = c("Lon", "Lat"))
turnover_tot_soil_man_cc_all <- turnover_tot_soil_man_cc_all %>%
  mutate(
    diff_unm_base = turn_unm - turn_base,
    diff_tone_base = turn_tone - turn_base,
    diff_tobd_base = turn_tobd - turn_base,
    diff_tobe_base = turn_tobe - turn_base
  )

#saveRDS(turnover_tot_soil_man_cc_all, file= "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_tot_soil_man_cc_all_values_and_diff_with_base.RData",ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#write.table(turnover_tot_soil_man_cc_all, file = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_tot_soil_man_cc_all_values_and_diff_with_base")
############################################################################################

turnover_surfhum_man_cc_unm <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_surfhum_man_cc_unm)
                        
turnover_surfhum_man_cc_to_ne <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_surfhum_man_cc_to_ne)
turnover_surfhum_man_cc_to_bd <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_surfhum_man_cc_to_bd)
turnover_surfhum_man_cc_to_be <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_surfhum_man_cc_to_be)
turnover_surfhum_man_cc_base <- defineQuantity(id = "Cturnover",
                        name = "Carbon Turnover Time",
                        colours = viridis::magma,
                        units = "y",
                        add.to = turnover_surfhum_man_cc_base)

turnover_surfhum_man_cc_unm_df <- as.data.frame(turnover_surfhum_man_cc_unm)
colnames(turnover_surfhum_man_cc_unm_df) <- c("Lon", "Lat", "turn_unm") 
turnover_surfhum_man_cc_to_ne_df <- as.data.frame(turnover_surfhum_man_cc_to_ne)
colnames(turnover_surfhum_man_cc_to_ne_df) <- c("Lon", "Lat", "turn_tone") #change col names not to have spaces
turnover_surfhum_man_cc_to_bd_df <- as.data.frame(turnover_surfhum_man_cc_to_bd)
colnames(turnover_surfhum_man_cc_to_bd_df) <- c("Lon", "Lat", "turn_tobd") #change col names not to have spaces
turnover_surfhum_man_cc_to_be_df <- as.data.frame(turnover_surfhum_man_cc_to_be)
colnames(turnover_surfhum_man_cc_to_be_df) <- c("Lon", "Lat", "turn_tobe") #change col names not to have spaces
turnover_surfhum_man_cc_base_df <- as.data.frame(turnover_surfhum_man_cc_base)
colnames(turnover_surfhum_man_cc_base_df) <- c("Lon", "Lat", "turn_base") #change col names not to have spaces

turnover_surfhum_man_cc_list <- list(turnover_surfhum_man_cc_unm_df, 
                                            turnover_surfhum_man_cc_to_ne_df,
                                            turnover_surfhum_man_cc_to_bd_df,
                                            turnover_surfhum_man_cc_to_be_df,
                                            turnover_surfhum_man_cc_base_df
                                            )
turnover_surfhum_man_cc_all <- reduce(turnover_surfhum_man_cc_list, full_join, by = c("Lon", "Lat"))
turnover_surfhum_man_cc_all <- turnover_surfhum_man_cc_all %>%
  mutate(
    diff_unm_base = turn_unm - turn_base,
    diff_tone_base = turn_tone - turn_base,
    diff_tobd_base = turn_tobd - turn_base,
    diff_tobe_base = turn_tobe - turn_base
  )

#saveRDS(turnover_surfhum_man_cc_all, file= "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_surfhum_man_cc_all_values_and_diff_with_base.RData",ascii = FALSE, version = NULL, compress=TRUE, refhook = NULL)
#write.table(turnover_surfhum_man_cc_all, file = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\turnover_surf_hum_man_cc_all_values_and_diff_with_base")
############################################################################################

######################################################################
# the same steps can be done for surf cwd, surf fwd, and slowsom pools
######################################################################



#### anpp data for figure s14
#aNPP
npp_man_only_unm <- getField(source = man_only_unm, quant="anpp_sts",first.year = 2220, last.year = 2249)
npp_forest_man_only_unm <- calcNewField(x = npp_man_only_unm, y = forest_area, x.col = "Forest_sum", y.col = "forest_area", op = "*" )

npp_forest_man_only_unm_mean <- aggregateYears(npp_forest_man_only_unm, method ="mean")
npp_forest_man_only_unm_mean_df <- as.data.frame(npp_forest_man_only_unm_mean)
# write.table(npp_forest_man_only_unm_mean_df, file = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\LPJG_out\\aNPP_forest_unm_man_only_mean",
#               row.names = FALSE, col.names = TRUE)


npp_man_only_base <- getField(source = man_only_base, quant="anpp_sts",first.year = 2220, last.year = 2249)
npp_forest_man_only_base <- calcNewField(x = npp_man_only_base, y = forest_area, x.col = "Forest_sum", y.col = "forest_area", op = "*" )

npp_forest_man_only_base_mean <- aggregateYears(npp_forest_man_only_base, method ="mean")
npp_forest_man_only_base_mean_df <- as.data.frame(npp_forest_man_only_base_mean)
# write.table(npp_forest_man_only_base_mean_df, file = "C:\\Users\\ferretto-a.IMK-IFU\\Documents\\LPJ-GUESS\\carbon_turnover\\paper_runs\\data_to_upload\\LPJG_out\\aNPP_forest_base_man_only_mean",
#               row.names = FALSE, col.names = TRUE)

