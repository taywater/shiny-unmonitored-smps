
#Script to manipulate the unmonitoed query, add con-phase to cwl and filter out post con data
#Author: Farshad Ebrahimi, Last modified: 8/18/2022

#load libraries
#shiny
library(shiny)
#shiny themes for color 
library(shinythemes)
#tidyverse for data manipulation 
library(tidyverse)
#pool for database connections
library(pool)
#odbc for database connection
library(odbc)
#easy javascript commands
library(shinyjs)
#datatables
library(DT)
#shinyjs
library(shinyjs)
#dplyr
library(dplyr)
#lubridate
library(lubridate)
#create negate of %in%
`%!in%` = Negate(`%in%`)

#connection
con <- dbConnect(odbc(), dsn = "mars_testing")
con_pg12 <- dbConnect(odbc(), dsn = "mars_data")


#Write queries to populate the data tables
cwl_smp <- dbGetQuery(con,"SELECT DISTINCT *
           FROM fieldwork.deployment_full_cwl")

# Construction phase dates for smps
external.cipit_project <- dbGetQuery(con_pg12, "SELECT * FROM external.cipit_project")

external.smpbdv <- dbGetQuery(con_pg12, "SELECT * FROM  external.smpbdv")

smp_milestones <- inner_join(external.cipit_project, external.smpbdv, by = c("work_number" = "worknumber"  )) 

smp_milestones <- smp_milestones %>% 
  inner_join(cwl_smp, by="smp_id")

conphase <- data.frame(phase_uid = 1:4, phase = c("Pre-Construction", "Construction", "Post-Construction", "Unknown"))


#A loop to populate the con-phase for each CWL deployment
#setting the lookup_id's default in smpmilestone to 4
smp_milestones['phase_lookup_uid'] <- 4
  
  for(i in 1:nrow(smp_milestones)) {
    
    if (!is.na(smp_milestones[i, "construction_start_date"]) && !is.na(smp_milestones[i, "construction_complete_date"]) ) {
      
      if (smp_milestones[i, "deployment_dtime_est"] >= smp_milestones[i, "construction_start_date"] && smp_milestones[i, "deployment_dtime_est"] <= smp_milestones[i, "construction_complete_date"]  ) {
        
        smp_milestones[i, "phase_lookup_uid"] <- 2
        
      } else if (smp_milestones[i, "deployment_dtime_est"] < smp_milestones[i, "construction_start_date"]) {
        
        smp_milestones[i, "phase_lookup_uid"] <- 1
        
      } else {
        
        smp_milestones[i, "phase_lookup_uid"] <- 3
        
      }
      
      
    } else if (!is.na(smp_milestones[i, "pc_ntp_date"]) && !is.na(smp_milestones[i, "construction_complete_date"] )) {
      
      if (smp_milestones[i, "deployment_dtime_est"] >= smp_milestones[i, "pc_ntp_date"] && smp_milestones[i, "deployment_dtime_est"] <= smp_milestones[i, "construction_complete_date"]  ) {
        
        smp_milestones[i, "phase_lookup_uid"] <- 2
        
      } else if (smp_milestones[i, "deployment_dtime_est"] < smp_milestones[i, "pc_ntp_date"]) {
        
        smp_milestones[i, "phase_lookup_uid"] <- 1
        
      } else {
        
        smp_milestones[i, "phase_lookup_uid"] <- 3
        
      }
      
    } else if (!is.na(smp_milestones[i, "construction_start_date"]) && !is.na(smp_milestones[i, "contract_closed_date"])) {
      
      if (smp_milestones[i, "deployment_dtime_est"] >= smp_milestones[i, "construction_start_date"] && smp_milestones[i, "deployment_dtime_est"] <= smp_milestones[i, "contract_closed_date"]  ) {
        
        smp_milestones[i, "phase_lookup_uid"] <- 2
        
      } else if (smp_milestones[i, "deployment_dtime_est"] < smp_milestones[i, "construction_start_date"]) {
        
        smp_milestones[i, "phase_lookup_uid"] <- 1
        
      } else {
        
        smp_milestones[i, "phase_lookup_uid"] <- 3
        
      }
      
      
      
    } else if (!is.na(smp_milestones[i, "pc_ntp_date"]) && !is.na(smp_milestones[i, "contract_closed_date"])) {
      
      if (smp_milestones[i, "deployment_dtime_est"] >= smp_milestones[i, "pc_ntp_date"] && smp_milestones[i, "deployment_dtime_est"] <= smp_milestones[i, "contract_closed_date"]  ) {
        
        smp_milestones[i, "phase_lookup_uid"] <- 2
        
      } else if (smp_milestones[i, "deployment_dtime_est"] < smp_milestones[i, "pc_ntp_date"]) {
        
        smp_milestones[i, "phase_lookup_uid"] <- 1
        
      } else {
        
        smp_milestones[i, "phase_lookup_uid"] <- 3
        
      }
      
      
    } else { 
      
      smp_milestones[i, "phase_lookup_uid"] <- 4
      
      
    }
    
    
  }
  
cwl_smp <- smp_milestones %>%
  inner_join(conphase, by=c("phase_lookup_uid"="phase_uid")) %>%
  select(smp_id,system_id,phase) %>%
  distinct()


# run the old query to populate unmonitored sites, separately get the srt, inavalid-inlet and cwl data and change the queries to avoid filtering based on these (reserved for R for further manipulation at the end)
cwl_system <- dbGetQuery(con,"SELECT DISTINCT smp_to_system(deployment_full_cwl.smp_id::character varying) AS system_id
           FROM fieldwork.deployment_full_cwl")

srt_systems <-dbGetQuery(con,"SELECT DISTINCT *
           FROM fieldwork.srt_full")

inactive_inlets <- dbGetQuery(con,"SELECT dataconv_gswiinlet.lifecycle_status,
            dataconv_gswiinlet.facility_id,
            component_to_smp(dataconv_gswiinlet.component_id::character varying) AS smp_id,
            smp_to_system(component_to_smp(dataconv_gswiinlet.component_id::character varying)) AS system_id,
            dataconv_gswiinlet.component_id
           FROM dataconv_gswiinlet
          WHERE (dataconv_gswiinlet.lifecycle_status <> 'ACT'::text OR dataconv_gswiinlet.plug_status <> 'ONLINE'::text) AND dataconv_gswiinlet.component_id IS NOT NULL
        UNION
         SELECT c.lifecycle_status,
            c.facility_id,
            component_to_smp(btrim(c.component_id, ' '::text)::character varying) AS smp_id,
            smp_to_system(component_to_smp(btrim(c.component_id, ' '::text)::character varying)) AS system_id,
            c.component_id
           FROM dataconv_gswiconveyance c
             LEFT JOIN gswi_conveyance_subtype_lookup lo ON c.subtype = lo.code
          WHERE c.component_id IS NOT NULL AND c.lifecycle_status <> 'ACT'::text
        UNION
         SELECT s.lifecycle_status,
            s.facility_id,
            component_to_smp(btrim(s.component_id, ' '::text)::character varying) AS smp_id,
            smp_to_system(component_to_smp(btrim(s.component_id, ' '::text)::character varying)) AS system_id,
            s.component_id
           FROM dataconv_gswistructure s
          WHERE s.component_id IS NOT NULL AND s.lifecycle_status <> 'ACT'::text")


output <- dbGetQuery(con,"WITH inactive_inlets AS (
         SELECT dataconv_gswiinlet.lifecycle_status,
            dataconv_gswiinlet.facility_id,
            component_to_smp(dataconv_gswiinlet.component_id::character varying) AS smp_id,
            smp_to_system(component_to_smp(dataconv_gswiinlet.component_id::character varying)) AS system_id,
            dataconv_gswiinlet.component_id
           FROM dataconv_gswiinlet
          WHERE (dataconv_gswiinlet.lifecycle_status <> 'ACT'::text OR dataconv_gswiinlet.plug_status <> 'ONLINE'::text) AND dataconv_gswiinlet.component_id IS NOT NULL
        UNION
         SELECT c.lifecycle_status,
            c.facility_id,
            component_to_smp(btrim(c.component_id, ' '::text)::character varying) AS smp_id,
            smp_to_system(component_to_smp(btrim(c.component_id, ' '::text)::character varying)) AS system_id,
            c.component_id
           FROM dataconv_gswiconveyance c
             LEFT JOIN gswi_conveyance_subtype_lookup lo ON c.subtype = lo.code
          WHERE c.component_id IS NOT NULL AND c.lifecycle_status <> 'ACT'::text
        UNION
         SELECT s.lifecycle_status,
            s.facility_id,
            component_to_smp(btrim(s.component_id, ' '::text)::character varying) AS smp_id,
            smp_to_system(component_to_smp(btrim(s.component_id, ' '::text)::character varying)) AS system_id,
            s.component_id
           FROM dataconv_gswistructure s
          WHERE s.component_id IS NOT NULL AND s.lifecycle_status <> 'ACT'::text
        ), greenit_built_info AS (
         SELECT greenit_smpbestdata.smp_id,
            greenit_smpbestdata.system_id,
            greenit_smpbestdata.smp_notbuiltretired,
            greenit_smpbestdata.smp_smptype,
            greenit_smpbestdata.capit_status
           FROM greenit_smpbestdata
          WHERE greenit_smpbestdata.smp_notbuiltretired IS NULL AND greenit_smpbestdata.smp_smptype <> 'Depaving'::text
        ), cwl_smp AS (
         SELECT DISTINCT deployment_full_cwl.smp_id
           FROM fieldwork.deployment_full_cwl
        ), cwl_system AS (
         SELECT DISTINCT smp_to_system(deployment_full_cwl.smp_id::character varying) AS system_id
           FROM fieldwork.deployment_full_cwl
        ), srt_systems AS (
         SELECT DISTINCT srt_full.system_id
           FROM fieldwork.srt_full
        ), gso_info AS (
         SELECT dataconv_gswibasin.smp_id,
                CASE
                    WHEN dataconv_gswibasin.surface_maintenance = 'PWD'::text OR dataconv_gswibasin.subsurface_maintenance = 'PWD'::text THEN true
                    ELSE false
                END AS maintained
           FROM dataconv_gswibasin
        UNION
         SELECT dataconv_gswiblueroof.smp_id,
                CASE
                    WHEN dataconv_gswiblueroof.surface_maintenance = 'PWD'::text OR dataconv_gswiblueroof.subsurface__maintenance = 'PWD'::text THEN true
                    ELSE false
                END AS maintained
           FROM dataconv_gswiblueroof
        UNION
         SELECT dataconv_gswibumpout.smp_id,
                CASE
                    WHEN dataconv_gswibumpout.surface_maintenance = 'PWD'::text OR dataconv_gswibumpout.subsurface_maintenance = 'PWD'::text THEN true
                    ELSE false
                END AS maintained
           FROM dataconv_gswibumpout
        UNION
         SELECT dataconv_gswicistern.smp_id,
                CASE
                    WHEN dataconv_gswicistern.surface_maintenance = 'PWD'::text OR dataconv_gswicistern.subsurface_maintenance = 'PWD'::text THEN true
                    ELSE false
                END AS maintained
           FROM dataconv_gswicistern
        UNION
         SELECT dataconv_gswidrainagewell.smp_id,
                CASE
                    WHEN dataconv_gswidrainagewell.surface_maintenance = 'PWD'::text OR dataconv_gswidrainagewell.subsurface_maintenance = 'PWD'::text THEN true
                    ELSE false
                END AS maintained
           FROM dataconv_gswidrainagewell
        UNION
         SELECT dataconv_gswigreenroof.smp_id,
                CASE
                    WHEN dataconv_gswigreenroof.surface_maintenance = 'PWD'::text OR dataconv_gswigreenroof.subsurface_maintenance = 'PWD'::text THEN true
                    ELSE false
                END AS maintained
           FROM dataconv_gswigreenroof
        UNION
         SELECT dataconv_gswipermeablepavement.smp_id,
                CASE
                    WHEN dataconv_gswipermeablepavement.surface_maintenance = 'PWD'::text OR dataconv_gswipermeablepavement.subsurface_maintenance = 'PWD'::text OR dataconv_gswipermeablepavement.porous_maintenance = 'PWD'::text THEN true
                    ELSE false
                END AS maintained
           FROM dataconv_gswipermeablepavement
        UNION
         SELECT dataconv_gswiplanter.smp_id,
                CASE
                    WHEN dataconv_gswiplanter.surface_maintenance = 'PWD'::text OR dataconv_gswiplanter.subsurface_maintenance = 'PWD'::text THEN true
                    ELSE false
                END AS maintained
           FROM dataconv_gswiplanter
        UNION
         SELECT dataconv_gswiraingarden.smp_id,
                CASE
                    WHEN dataconv_gswiraingarden.surface_maintenance = 'PWD'::text OR dataconv_gswiraingarden.subsurface_maintenance = 'PWD'::text THEN true
                    ELSE false
                END AS maintained
           FROM dataconv_gswiraingarden
        UNION
         SELECT dataconv_gswiswale.smp_id,
                CASE
                    WHEN dataconv_gswiswale.surface_maintenance = 'PWD'::text OR dataconv_gswiswale.subsurface_maintenance = 'PWD'::text THEN true
                    ELSE false
                END AS maintained
           FROM dataconv_gswiswale
        UNION
         SELECT dataconv_gswitree.smp_id,
                CASE
                    WHEN dataconv_gswitree.surface_maintenance = 'PWD'::text OR dataconv_gswitree.subsurface_maintenance = 'PWD'::text THEN true
                    ELSE false
                END AS maintained
           FROM dataconv_gswitree
        UNION
         SELECT dataconv_gswitreetrench.smp_id,
                CASE
                    WHEN dataconv_gswitreetrench.surface_maintenance = 'PWD'::text OR dataconv_gswitreetrench.subsurface_maintenance = 'PWD'::text THEN true
                    ELSE false
                END AS maintained
           FROM dataconv_gswitreetrench
        UNION
         SELECT dataconv_gswitrench.smp_id,
                CASE
                    WHEN dataconv_gswitrench.surface_maintenance = 'PWD'::text OR dataconv_gswitrench.subsurface_maintenance = 'PWD'::text THEN true
                    ELSE false
                END AS maintained
           FROM dataconv_gswitrench
        UNION
         SELECT dataconv_gswiwetland.smp_id,
                CASE
                    WHEN dataconv_gswiwetland.surface_maintenance = 'PWD'::text OR dataconv_gswiwetland.subsurface_maintenance = 'PWD'::text THEN true
                    ELSE false
                END AS maintained
           FROM dataconv_gswiwetland
        )
 SELECT DISTINCT gbi.smp_id, gbi.system_id,
    gbi.smp_smptype AS smp_type,  
    gbi.capit_status,
        CASE
            WHEN sys.system_id IS NULL THEN false
            WHEN sys.system_id IS NOT NULL THEN true
            ELSE NULL::boolean
        END AS other_cwl_at_this_system
   FROM greenit_built_info gbi
     LEFT JOIN cwl_system sys ON sys.system_id::text = gbi.system_id
  WHERE (gbi.smp_id IN ( SELECT gso_info.smp_id
           FROM gso_info
          WHERE gso_info.maintained = true)) AND NOT (EXISTS ( SELECT deny.smp_id
           FROM fieldwork.monitoring_deny_list deny
          WHERE deny.smp_id::text = gbi.smp_id)) AND NOT (EXISTS ( SELECT cet.system_id
           FROM fieldwork.capture_efficiency cet
          WHERE cet.system_id = gbi.system_id AND gbi.smp_smptype = 'Stormwater Tree'::text)) AND NOT (EXISTS ( SELECT ppt.smp_id
           FROM fieldwork.porous_pavement ppt
          WHERE ppt.smp_id = gbi.smp_id AND ppt.test_date > (now() - '2 years'::interval)))
  ORDER BY gbi.smp_id;" )


##script to create the same table as the shiny app, now we can play around with the CWL and SRT, they both have con-phase
  output_filtered <- output %>% 
    filter(smp_id %!in% cwl_smp$smp_id &
             system_id %!in% srt_systems$system_id &
             system_id %!in% inactive_inlets$system_id) %>%
    select(-system_id) %>%
    distinct()
  
dbDisconnect(con)
dbDisconnect(con_pg12)










