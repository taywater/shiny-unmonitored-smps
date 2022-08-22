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

#connections
con <- dbConnect(odbc(), dsn = "mars_testing")
con_pg12 <- dbConnect(odbc(), dsn = "mars_data")


#Write queries to populate cwl data tables
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

#get the maintenance status and all online inlets from Taylor's new data structure, along with cwl and srt systems
gso_info <- dbGetQuery(con,"select * from fieldwork.gso_maintenance")


cwl_system <- dbGetQuery(con,"SELECT DISTINCT smp_to_system(deployment_full_cwl.smp_id::character varying) AS system_id
           FROM fieldwork.deployment_full_cwl")

srt_systems <-dbGetQuery(con,"SELECT DISTINCT *
           FROM fieldwork.srt_full")

all_inlets <- dbGetQuery(con,"SELECT * from fieldwork.all_inlets")

#query to get un-filtered output to later filter with the new criteria and tables

output <- dbGetQuery(con,"WITH greenit_built_info AS (
         SELECT greenit_smpbestdata.smp_id,
            greenit_smpbestdata.system_id,
            greenit_smpbestdata.smp_notbuiltretired,
            greenit_smpbestdata.smp_smptype,
            greenit_smpbestdata.capit_status
           FROM greenit_smpbestdata
          WHERE greenit_smpbestdata.smp_notbuiltretired IS NULL AND greenit_smpbestdata.smp_smptype <> 'Depaving'::text
        ),  cwl_system AS (
         SELECT DISTINCT smp_to_system(deployment_full_cwl.smp_id::character varying) AS system_id
           FROM fieldwork.deployment_full_cwl
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
  WHERE NOT (EXISTS ( SELECT deny.smp_id
           FROM fieldwork.monitoring_deny_list deny
          WHERE deny.smp_id::text = gbi.smp_id)) AND NOT (EXISTS ( SELECT cet.system_id
           FROM fieldwork.capture_efficiency cet
          WHERE cet.system_id = gbi.system_id AND gbi.smp_smptype = 'Stormwater Tree'::text)) AND NOT (EXISTS ( SELECT ppt.smp_id
           FROM fieldwork.porous_pavement ppt
          WHERE ppt.smp_id = gbi.smp_id AND ppt.test_date > (now() - '2 years'::interval)))
  ORDER BY gbi.smp_id;" )


##script to create similar table as the shiny app, now we can play around with filtering criteria 

#only post-con
 cwl_smp <- cwl_smp %>% 
   filter(phase == "Post-Construction")
 #only post-con
 srt_systems <- srt_systems %>% 
   filter(phase == "Post-Construction")
 #only maintained by PWD
 gso_info <- gso_info %>% 
   filter(maintained ==1)
 #only online inlets based on all_inlet table
 online_inlets <- all_inlets %>% 
   filter(plug_status == "ONLINE") %>%
   select(smp_id)

 #smps that do not show up in all_inlet will be here; Taylor can you confirm this?
 no_inlets <- dbGetQuery(con, "with cwl_smp AS (
             SELECT DISTINCT deployment_full_cwl.smp_id
               FROM fieldwork.deployment_full_cwl
    	)
    select distinct sbd.smp_id from greenit_smpbestdata sbd  where NOT (EXISTS ( SELECT cs.smp_id
               FROM cwl_smp cs
              WHERE cs.smp_id = sbd.smp_id)) 
    		  order by sbd.smp_id") %>% 
   anti_join(all_inlets, by="smp_id")

 #combine online + no-inlet for final filtering
 allowed_smp_inletbased <- union_all(online_inlets, no_inlets) %>%
   distinct()

 #do the final filtering
 output_filtered <- output %>% 
    filter(smp_id %!in% cwl_smp$smp_id &
             system_id %!in% srt_systems$system_id &
             smp_id %in% gso_info$smp_id &
             smp_id %in% allowed_smp_inletbased$smp_id) %>%
    select(-system_id) %>%
    distinct()
#disconnect from pg9 and 12
dbDisconnect(con)
dbDisconnect(con_pg12)










