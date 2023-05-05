 
### This script assgns addresses to SMP in smp population app-extent layer shapefile already exported to server
#Farshad Ebrahimi-4/10/2023

  library(DBI)
  library(RPostgreSQL)
  library(RPostgres)
  library(odbc)
  library(dplyr)
  library(sf)
  library(tidyr)
  
#pg14 con
  con <- dbConnect(odbc::odbc(), dsn = "mars14_data", uid = Sys.getenv("shiny_uid"), pwd = Sys.getenv("shiny_pwd"), MaxLongVarcharSize = 8190  )
  
#get the smps
  gis_apps <- paste0("MSSQL:server=PWDGISSQL;",
                          "database=GSO_APPS;",
                          "UID=gisread;",
                          "PWD=gisread;")
  
  
  basin <- suppressWarnings(st_read(gis_apps, "gisad.GSWIBASIN", quiet = TRUE)) 
  
  blueroof <- suppressWarnings(st_read(gis_apps, "gisad.GSWIBLUEROOF", quiet = TRUE)) 
  
  bumpout <- suppressWarnings(st_read(gis_apps, "gisad.GSWIBUMPOUT", quiet = TRUE)) 
  
  cistern <- suppressWarnings(st_read(gis_apps, "gisad.GSWICISTERN", quiet = TRUE)) 
  
  # something wrong with drainagewell 
  
  #drainagewell <- suppressWarnings(st_read(gis_apps, "GISAD.gswiDrainageWell", quiet = TRUE))
  
  greenroof <- suppressWarnings(st_read(gis_apps, "gisad.GSWIGREENROOF", quiet = TRUE))
  
  permeablepavement <- suppressWarnings(st_read(gis_apps, "gisad.GSWIPERMEABLEPAVEMENT", quiet = TRUE))
  
  planter <- suppressWarnings(st_read(gis_apps, "gisad.GSWIPLANTER", quiet = TRUE))
  
  raingarden <- suppressWarnings(st_read(gis_apps, "gisad.GSWIRAINGARDEN", quiet = TRUE))
  
  swale <- suppressWarnings(st_read(gis_apps, "gisad.GSWISWALE", quiet = TRUE))
  
  treetrench <- suppressWarnings(st_read(gis_apps, "gisad.GSWITREETRENCH", quiet = TRUE))
  
  tree <- suppressWarnings(st_read(gis_apps, "gisad.GSWITree", quiet = TRUE))
  
  trench <- suppressWarnings(st_read(gis_apps, "gisad.GSWITRENCH", quiet = TRUE)) 
  
  wetland <- suppressWarnings(st_read(gis_apps, "gisad.GSWIWETLAND", quiet = TRUE))


#gso address layer
  gso_address<- st_read(dsn = "\\\\pwdoows\\oows\\Watershed Sciences\\GSI Monitoring\\09 GIS Data\\GSO SMP Addresses ", layer = "Extent_layer")

#bind all smps
  # Drop columns except the SMP_ID and XY (just in case for joining) Merge all SMPs
  
  basin <- basin %>% select(SMP_ID, X_STATEPLANE, Y_STATEPLANE)
  
  blueroof<- blueroof %>% select(SMP_ID, X_STATEPLANE, Y_STATEPLANE)
  
  bumpout <- bumpout %>% select(SMP_ID, X_STATEPLANE, Y_STATEPLANE)
  
  cistern <- cistern %>% select(SMP_ID, X_STATEPLANE, Y_STATEPLANE)
  
  #drainagewell <- drainagewell %>% select(SMP_ID)
  
  greenroof<- greenroof %>% select(SMP_ID, X_STATEPLANE, Y_STATEPLANE)
  
  permeablepavement <- permeablepavement %>%  select(SMP_ID, X_STATEPLANE, Y_STATEPLANE)
  
  planter<- planter %>% select(SMP_ID, X_STATEPLANE, Y_STATEPLANE)
  
  raingarden <- raingarden %>% select(SMP_ID, X_STATEPLANE, Y_STATEPLANE)
  
  swale <- swale %>% select(SMP_ID, X_STATEPLANE, Y_STATEPLANE)
  
  treetrench <- treetrench %>% select(SMP_ID, X_STATEPLANE, Y_STATEPLANE)
  
  tree <- tree %>% select(SMP_ID, X_STATEPLANE, Y_STATEPLANE)
  
  trench<- trench %>% select(SMP_ID, X_STATEPLANE, Y_STATEPLANE)
  
  wetland <- wetland %>% select(SMP_ID, X_STATEPLANE, Y_STATEPLANE)
  
  # something wrong with drainagewell 
  
  # SMP <- bind_rows(basin, blueroof, bumpout, cistern, drainagewell, greenroof, permeablepavement, planter, raingarden, swale, treetrench, trench, wetland)
  
  SMP <- bind_rows(basin,tree, blueroof, bumpout, cistern, greenroof, permeablepavement, planter, raingarden, swale, treetrench, trench, wetland) 
    
  st_crs(SMP) <- 2272
  st_crs(gso_address) <- 2272
  
  #Intersecting SMPs with address layer
  intersect_smp_address <- st_intersects(SMP, gso_address)
  
  #deciphering the intersection data structure
  
  Inters_Obj <- intersect_smp_address
  
  GSI <- SMP
  
  output <- NULL
  
  df <- NULL
  
  for(i in 1:length(Inters_Obj)) {
    
    temp <- Inters_Obj[[i]]
    
    if (length(temp) > 0) {
      
      
      Address <- gso_address[temp, "Address"] 
      District <- gso_address[temp, "District"] 
      Neighborhood <- gso_address[temp, "Neighborho"] 
      
      SMPID <- GSI [i,"SMP_ID"]
      SMPID <- SMPID %>% st_set_geometry(NULL)

      df <- data.frame(SMPID, Address, District,Neighborhood)
    
      output <- rbind(output, df ) 
      
    }
    
  } 
  
  #trimming, name changing, switching to df frin sf
  output['system_id'] <- gsub('-\\d+$','',output$SMP_ID ) 
  output_final <- output %>%
    as.data.frame() %>% 
    select(system_id, address=Address, district=District, neighborhood=Neighborho )
  
  #joining and dropping uids-this is a one-time step to replace the existing data in the db
  clustered_systems_table_ad <- clustered_systems_table %>%
    left_join(output_final, by="system_id") %>%
    select(-clustered_systems_uid) %>%
    distinct() 
  
  #keep only one address per system if multiple exists
  db_table <- clustered_systems_table_ad %>%
    group_by(system_id) %>%
    mutate(address = paste(address, collapse = ', ')) %>%
    distinct()
  
  
  dbWriteTable (con, SQL("fieldwork.tbl_clustered_systems"),  db_table,append= TRUE, row.names = FALSE)
  
  dbDisconnect(con)
  