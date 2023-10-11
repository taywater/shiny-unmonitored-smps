#Unmonitored SMPs
#Show all unmonitored active SMPs, along with a "Deny List"
# Revised by Farshad Ebrahimi on 4/21/2023


#0.0 load libraries ----
    
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
    #download as excel
    library(openxlsx)
    #reactable
    library(reactable)
    #Write to DB
    library(DBI)
    #SF
    library(sf)
    library(ggplot2)
    library(pool)


    #create negate of %in%
    `%!in%` = Negate(`%in%`)
  

#0.1 set up----
    options(stringsAsFactors=FALSE)
    
    #set default page length for datatables
    options(DT.options = list(pageLength = 15))
    
    #set db connection
    #gets environmental variables saved in local or pwdrstudio environment
    poolConn <- dbPool(odbc(), dsn = "mars14_datav2", uid = Sys.getenv("shiny_uid"), pwd = Sys.getenv("shiny_pwd"))
    
    #GSO DB
    # gso_db <- paste0("MSSQL:server=PWDGISSQL;",
    #                  "database=GSODB;",
    #                  "UID=gisread;",
    #                  "PWD=gisread;")
    
    
    
    #disconnect from db on stop 
    onStop(function(){
      poolClose(poolConn)
    })
    #js warning about leaving page
    jscode <- 'window.onbeforeunload = function() { return "Please use the button on the webpage"; };'

#0.2 global variables and functions plus exclusion/incluion lists for unmonitored sites ----
 
    #query SMPs that have not been monitored 
    # Sample for the clustering section
    
    smp <- dbGetQuery(poolConn, "with cwl_smp AS (
             SELECT DISTINCT fieldwork.viw_deployment_full_cwl.smp_id
               FROM fieldwork.viw_deployment_full_cwl
    	)
    select distinct sbd.smp_id from external.tbl_smpbdv sbd  where NOT (EXISTS ( SELECT cs.smp_id
               FROM cwl_smp cs
              WHERE cs.smp_id = sbd.smp_id)) 
    		  order by sbd.smp_id")  %>% 
      dplyr::arrange(smp_id) %>% 
      dplyr::pull()
    
### There are two check boxes in the app, resulting on 4 different tables as output
    
    #A view for each combo of checkboxes
    unmonitored_smp_view_postcon_on <- dbGetQuery(poolConn, "SELECT * FROM fieldwork.viw_unmonitored_postcon_on")
    unmonitored_smp_view_both_on <- dbGetQuery(poolConn, "SELECT * FROM fieldwork.viw_unmonitored_both_on")
    unmonitored_smp_view_both_off <- dbGetQuery(poolConn, "SELECT * FROM fieldwork.viw_unmonitored_both_off")
    unmonitored_smp_view_future_on <- dbGetQuery(poolConn, "SELECT * FROM fieldwork.viw_unmonitored_future_on")
    
    cwl_system <- dbGetQuery(poolConn,"SELECT DISTINCT admin.fun_smp_to_system(fieldwork.viw_deployment_full_cwl.smp_id::text) AS system_id FROM fieldwork.viw_deployment_full_cwl")
  
    
####output tables for the unmonitored smp tab are generated here
    #both check box off-means that SMPs with precon- and construction SRT and CWL records are considered as "Monitored SMP" and will not show in the app
    #Also mean that SMPs with future deployment are included on the output, hence considered unmonitored
    output_table_1 <- unmonitored_smp_view_both_off %>%
      select(smp_id, system_id, smp_type, capit_status) %>%
      mutate("other_cwl_at_this_system" =  case_when(system_id %in% cwl_system$system_id ~ TRUE,
                                                     system_id %!in% cwl_system$system_id ~ FALSE)) %>%
      select(-system_id) %>%
      distinct()
    
    #just future deployment on
    #same as the previous output, only excluding future deployment SMPs from the app    
    output_table_2 <- unmonitored_smp_view_future_on %>%
      select(smp_id, system_id, smp_type, capit_status) %>%
      mutate("other_cwl_at_this_system" =  case_when(system_id %in% cwl_system$system_id ~ TRUE,
                                                     system_id %!in% cwl_system$system_id ~ FALSE)) %>%
      select(-system_id) %>%
      distinct()
    
    #both check boxes on
    #SMPs with precon- and construction SRT and CWL records are considered as "Unmonitored SMP" and willshow in the app
    #future deployments are excluded
    output_table_3 <- unmonitored_smp_view_both_on %>%
      select(smp_id, system_id, smp_type, capit_status) %>%
      mutate("other_cwl_at_this_system" =  case_when(system_id %in% cwl_system$system_id ~ TRUE,
                                                     system_id %!in% cwl_system$system_id ~ FALSE)) %>%
      select(-system_id) %>%
      distinct()
    
    #just postcon check box on
    #SMPs with precon- and construction SRT and CWL records are considered as "Unmonitored SMP" and willshow in the app
    #future deployments are not excluded
    output_table_4 <- unmonitored_smp_view_postcon_on %>%
      select(smp_id, system_id, smp_type, capit_status) %>%
      mutate("other_cwl_at_this_system" =  case_when(system_id %in% cwl_system$system_id ~ TRUE,
                                                     system_id %!in% cwl_system$system_id ~ FALSE)) %>%
      select(-system_id) %>%
      distinct()
    

    #replace special characters with friendlier characters
    special_char_replace <- function(note){
        
        note_fix <- note %>% 
            str_replace_all(c("•" = "-", "ï‚§" = "-", "“" = '"', '”' = '"'))
        
        return(note_fix)
        
    }
    
### Clustering app data processing 
  
    # clustering tables-the sample id will be used to get the current set of systems
    clustered_samples_db <- dbGetQuery(poolConn,"SELECT * from  fieldwork.tbl_clustered_samples WHERE desktop_analysis != 'Failed' AND pre_inspection != 'Failed' ORDER BY district, neighborhood")
    
    # population to gennerate alternatives for failed system in desktop or pre-inspection
    clustered_population_db <- dbGetQuery(poolConn,"SELECT * from  fieldwork.tbl_clustered_systems")
    
    # Monitoring the deployment records to generate sensor_deployed column
    deployed_systems <- dbGetQuery(poolConn,"select distinct admin.fun_smp_to_system(smp_id) as system_id, deployment_dtime_est from fieldwork.viw_deployment_full_cwl")
    
    # date of sample generation for the UI of clustering
    sample_generated_date <- clustered_population_db %>%
      select(date_generated) %>%
      pull %>%
      max()
    
    # This date will be used for looking at recent deployments, there may be old deployments with no QAed data that we want to avoid
    deployment_date_cutoff <- clustered_population_db %>%
      select(date_generated) %>%
      pull %>%
      min()
    
    # only deployments after the sample generation date-this is to avoid counting the past deployments with no data
    deployed_systems <- deployed_systems %>%
      filter(deployment_dtime_est > deployment_date_cutoff)
    
    # mutate sensors deployed based on deployment records
    clustered_samples_db <- clustered_samples_db %>% 
      mutate(sensor_deployed = case_when(system_id %in% deployed_systems$system_id ~ "Yes",
                                         system_id %!in% deployed_systems$system_id ~ "No")) 
    
    
    
    # greenit to get the smp-type
    smpbdv_df <- dbGetQuery(poolConn,"SELECT * FROM external.tbl_smpbdv")
    
  # Get the maintenance district map- disabled since SF doesn't work on RStudio Connect Server 
    # Maint_Dist_db <- suppressWarnings(st_read(gso_db, "GSOADMIN.GSWI_MAINTENANCE_DISTRICTS", quiet = TRUE))
    
   
    
    
    
# 1.0 UI --------
ui <-  navbarPage("MARS Unmonitored Active SMPs", theme = shinytheme("cerulean"),
                  #1.1 Unmonitored Active SMPs -------
                  tabPanel("Unmonitored Active SMPs", value = "main_tab", 
                           titlePanel("Unmonitored Active SMPs"),
                           #1.1.1 sidebarPanel ------
                           sidebarPanel(
                             tags$figure(
                               class = "centerFigure",
                               tags$img(
                                 src = "pwd.png",
                                 width = 350,
                                 alt = "logo"
                               ),
                               tags$figcaption("")
                             ),
                                        checkboxInput("exclude_future", "Exclude SMPs with Future Deployments?", value = TRUE), 
                                        checkboxInput("exclude_postcon", "Only Exclude Post-Construction Monitoring or Testing?", value = TRUE),
                           downloadButton("download", label = "Download"), 
                           h5("This query looks for SMPs that meeting the following criteria:
                              PWD is responsible for surface, subsurface, and/or porous maintenance;
                              SMP is not \"not built\" or \"retired\";
                              no inactive or plugged inlets, structures, or conveyance;
                              no continuous water level monitoring or SRTs performed; 
                              if stormwater tree, no CET; 
                              if porous pavement, no test within the past two years.
                              "),
                           width = 3
                           ),
                           #1.1.2 table ---------
                           mainPanel(
        
                                DTOutput("unmonitored_table")
                                ) 
                  ), 
                  #1.2 Deny List ----------
                  tabPanel("Deny List", value = "deny_tab", 
                           titlePanel("SMPs Denied Monitoring (a.k.a. SMPs Excluded from the Main Tab)"),
                           #1.2.1 sidebarPanel ---------
                           sidebarPanel(selectizeInput("smp_id", "SMP ID", choices = NULL, 
                                                       options = list(
                                                           placeholder = 'Select an Option', 
                                                            onInitialize = I('function() { this.setValue(""); }')
                                                       )), 
                                        textAreaInput("reason", "Reason", height = '85px'), 
                                        actionButton("add_smp", "Add SMP to Deny List"), 
                                        disabled(actionButton("remove_smp", "Remove SMP from Deny List"))), 
                           #1.2.2 tables ------
                           mainPanel(
                           DTOutput("deny_table")
                           ), 
                           #must call useShinyjs() for shinyjs() functionality to work in app
                           useShinyjs()
                  ),
                  
                  #1.3 Clustered systems app
                  tabPanel("Clustered Systems",
                           titlePanel(paste0("List of Semi-Random Systems for Monitoring"," (Most Recent Batch Generated: ", sample_generated_date,")" )),
                           sidebarLayout(
                             sidebarPanel(
                               # Input fields for updating values in selected row
                               selectInput("desktop_analysis", "Desktop Analysis Passed?", c("","Not Determined", "Passed", "Failed"), selected = NULL),
                               selectInput("pre_inspection", "Pre-Monitoring Inspection Passed?", c("","Not Determined", "Passed", "Failed"), selected = NULL),
                               #selectInput("sensor_deployed", "Sensor Deployed?", c("","Yes", "No"), selected = NULL),
                               actionButton("update_button", "Update"),
                               downloadButton("table_dl", "Download"),
                               conditionalPanel("input.desktop_analysis == 'Failed' | input.pre_inspection == 'Failed'",
                                                br(),
                                                textAreaInput("reason_clustering", "Reason for Adding to Deny List:", height = '85px')),
                               h5(strong("If either desktop analysis or Pre-Monitoring Inspection  fails, clicking update removes the system, adds relevant SMPs to deny list, 
                                  and replaces the system with an alternative. You can add a reason for adding this system to the deny list in the text box. Please note that this list only contains systems where all SMPs are on the Unmonitored Active SMPs list")),
                               tags$figure(
                                 class = "centerFigure",
                                 tags$img(
                                   src = "DM.png",
                                   width = 580,
                                   alt = "District Map"
                                 ),
                                 tags$figcaption("")
                               ),
                               #plotOutput("maint_dc_plot"),
                               width = 4
                             ),
                             mainPanel(
                               DTOutput("clustered_table")
                             )
                           )
                  )
)

#2.0 server --------
# Define server logic
server <- function(input, output, session) {

    #2.0 set up -----
    rv <- reactiveValues()
    
    #2.1 unmonitored tab -----
    #2.1.1 reactive output tables -------
    unmonitored_sites_db <- reactive(
      
      if(input$exclude_future == FALSE & input$exclude_postcon == FALSE){
        output_table_1 
      } else if (input$exclude_future == TRUE & input$exclude_postcon == FALSE){
        output_table_2
      } else if (input$exclude_future == TRUE & input$exclude_postcon == TRUE){
        output_table_3
      } else{
        output_table_4
      }
  
    )
    
    
    rv$unmonitored_sites <- reactive(unmonitored_sites_db() %>%  
                                         mutate(across(other_cwl_at_this_system,  
                                                       ~ case_when(. == TRUE ~ "Yes", 
                                                                   . == FALSE ~ "No"))) %>% 
                                         dplyr::rename("SMP ID" = "smp_id", "SMP Type" = "smp_type", "Capit Status" = "capit_status", "CWL at Other SMPs in this System?" = "other_cwl_at_this_system"))
    
    #Output DT
    output$unmonitored_table <- renderDT(
        rv$unmonitored_sites(),
        selection = 'single', 
        style = 'bootstrap',
        class = 'table-responsive, table-hover', 
        rownames = FALSE
    )
    
    #2.1.2 download --------
    output$download <- downloadHandler(
        filename = function(){
                paste("unmonitored_sites_", Sys.Date(), ".csv", sep = "") 
        }, 
        content = function(file){
            write.csv(rv$unmonitored_sites(), file, row.names = FALSE)
        }
    )
    
    
    #2.2 deny tab----
    
    #2.2.1 updates system ids ----
    updateSelectizeInput(session, "smp_id", choices = smp, selected = character(0), server = TRUE)
    
    #2.2.2 query and show table -------
    rv$deny_query <- reactive("select * from fieldwork.tbl_monitoring_deny_list order by smp_id")
    
    rv$deny_db <- reactive(dbGetQuery(poolConn, rv$deny_query()))
    
    rv$deny <- reactive(rv$deny_db()%>% 
                            dplyr::select(-1) %>% 
                            dplyr::rename("Reason" = "reason", "SMP ID" = "smp_id"))
    
    output$deny_table <- renderDT(
        rv$deny(), 
        selection = 'single', 
        style = 'bootstrap',
        class = 'table-responsive, table-hover', 
        rownames = FALSE
    )
    
    #2.2.3 prepare inputs ------
    #process text field to prevent sql injection
    rv$reason_step <- reactive(gsub('\'', '\'\'', input$reason))
    rv$reason_step_two <- reactive(special_char_replace(rv$reason_step()))
    rv$reason <- reactive(if(nchar(rv$reason_step_two()) == 0) "NULL" else paste0("'", rv$reason_step_two(), "'"))
    
    #2.2.4 toggle states ----
    #toggle state of add/edit based on whether SMP ID and Reason fields are selected
    observe(toggleState(id = "add_smp", condition = nchar(input$smp_id) > 0 & nchar(input$reason) > 0))
    
    #toggle text on add/edit
    rv$label <- reactive(if(!(input$smp_id %in% rv$deny_db()$smp_id)) "Add SMP" else "Edit SMP")
    observe(updateActionButton(session, "add_smp", label = rv$label()))
    
    #toggle state of remove based on whether row is selected
    observe(toggleState(id = "remove_smp", condition = input$smp_id %in% rv$deny_db()$smp_id))
    
    #2.2.4 editing ------
    #update values based on selected row 
    observeEvent(input$deny_table_rows_selected, {
        updateSelectizeInput(session, "smp_id", selected = rv$deny_db()$smp_id[input$deny_table_rows_selected])
        #updateTextAreaInput(session, "reason", value = rv$deny_db()$reason[input$deny_table_rows_selected])
    })
    
    observeEvent(input$smp_id, {
        updateTextAreaInput(session, "reason", value = character(0))
        if(input$smp_id %in% rv$deny_db()$smp_id){
            rv$update_reason_index <- which(rv$deny_db()$smp_id == input$smp_id)
            print(rv$update_reason_index)
            rv$update_reason <- rv$deny_db()$reason[rv$update_reason_index]
            updateTextAreaInput(session, "reason", value = rv$update_reason)
        }
    })
    
    #2.2.5 click button/add -----------
    #add to deny list
    observeEvent(input$add_smp, {
        if(!(input$smp_id %in% rv$deny_db()$smp_id)){
            add_smp_query <- paste0("INSERT INTO fieldwork.tbl_monitoring_deny_list (smp_id, reason)
                                    VALUES('", input$smp_id, "', ", rv$reason(), ")")
            
            dbGetQuery(poolConn, add_smp_query)
        }else{
            edit_smp_query <- paste0(
                "UPDATE fieldwork.tbl_monitoring_deny_list SET 
                reason = ", rv$reason(), "
                WHERE smp_id = '", input$smp_id, "'")
            
            dbGetQuery(poolConn, edit_smp_query)
        }
        
        #update deny table
        rv$deny_db <- reactive(dbGetQuery(poolConn, rv$deny_query()))
        #update main table with new/removed smp 
        rv$unmonitored_sites_db <- reactive(dbGetQuery(poolConn, rv$unmonitored_query()))
        
        #clear fields
        reset("smp_id")
        reset("reason")
    })
    
    #2.2.6 delete------
    #remove from deny list
    observeEvent(input$remove_smp, {
        showModal(modalDialog(title = "Remove from Deny List", 
                              paste("Are you sure you want to remove SMP", 
                                    input$smp_id, "from the deny list?"), 
                              modalButton("No"), 
                              actionButton("confirm_removal", "Yes")))
    })
    
    #confirm removal
    observeEvent(input$confirm_removal, {
        dbGetQuery(poolConn, 
                   paste0("DELETE FROM fieldwork.tbl_monitoring_deny_list WHERE smp_id = '",
                          input$smp_id, "'"))
        
        #update deny table
        rv$deny_db <- reactive(dbGetQuery(poolConn, rv$deny_query()))
        #update main table with new/removed smp 
        rv$unmonitored_sites_db <- reactive(dbGetQuery(poolConn, rv$unmonitored_query()))
        
        #clear fields
        reset("smp_id")
        reset("reason")
        
        #remove pop up
        removeModal()
    })
   
  ### Clustered app
    # Get the sample, add the smp type and trim it for the final table
    
    #toggle state for the update button
    observe(toggleState(id = "update_button", condition = !is.null(input$clustered_table_rows_selected)))
   
    clustered_samples_db <- clustered_samples_db %>%
      inner_join(smpbdv_df, by = join_by(system_id),multiple = "all") %>%
      group_by(system_id) %>%
      mutate(types = paste(smp_smptype, collapse = ', ')) %>%
      arrange(sensor_deployed) %>% 
      distinct() 
    #keep unique systems
    clustered_samples_db <- clustered_samples_db[!duplicated(clustered_samples_db$system_id), ] 
    
    selected_row <- reactiveVal()
    clustered_samples <- reactiveVal(clustered_samples_db)
    

    output$clustered_table <- renderDT(
      datatable(
        clustered_samples() %>%
          select('System ID'= system_id,'SMP Type' = types,
                 Address = address, District = district, Neighborhood = neighborhood,
                 'Desktop Analysis' = desktop_analysis,
                 'Pre-Monitoring Inspection' = pre_inspection,
                 'Sensor Deployment' = sensor_deployed, 'Replaced System' = alternative) %>%
          distinct(),
        selection = 'single', 
        style = 'bootstrap',
        class = 'table-responsive, table-hover', 
        rownames = FALSE
      )%>% 
        formatStyle(
          'Desktop Analysis',
          valueColumns = 'Desktop Analysis',
          backgroundColor = styleEqual('Passed', 'lightgreen'),
          fontWeight = styleEqual('Passed', 'bold')
        ) %>% 
        formatStyle(
          'Pre-Monitoring Inspection',
          valueColumns = 'Pre-Monitoring Inspection',
          backgroundColor = styleEqual('Passed', 'lightgreen'),
          fontWeight = styleEqual('Passed', 'bold')
        ) %>% 
        formatStyle(
          'Sensor Deployment',
          valueColumns = 'Sensor Deployment',
          backgroundColor = styleEqual('Yes', 'lightgreen'),
          fontWeight = styleEqual('Yes', 'bold')
        ) 
    )
    
    #Downloading the QA query list of systems-3 sheets of systems/smps
    output$table_dl <- downloadHandler(
      filename = function() {
        paste("Clustered_Systems_",Sys.Date(),".xlsx", sep = "")
      },
      content = function(filename){
        
        df_list <- list(unmonitored_systems= clustered_samples())
        write.xlsx(x = df_list , file = filename, rowNames = TRUE)
      }
    )
    
    # Update the values in the drop-down menus when a row is selected
    observeEvent(input$clustered_table_rows_selected, {
      # Get the selected row
      row <- input$clustered_table_rows_selected
      
      # Update the reactive value
      if (!is.null(row) && length(row) > 0) {
        selected_row(row)
        updateSelectInput(session, "desktop_analysis", selected = clustered_samples()$desktop_analysis[row])
        updateSelectInput(session, "pre_inspection", selected = clustered_samples()$pre_inspection[row])
        #updateSelectInput(session, "sensor_deployed", selected = clustered_samples()$sensor_deployed[row])
      }
    })
    
    # Update the selected row with the values from the drop-down menus when the action button is clicked
    observeEvent(input$update_button, {
      # Get the selected row
      row <- selected_row()
      
      # Update the attributes of the selected row with the values from the drop-down menus
      if (!is.null(row) && length(row) > 0) {
        
      # Update the DB 
        edit_cluster_query <- paste0("UPDATE fieldwork.tbl_clustered_samples SET desktop_analysis = '", input$desktop_analysis, "',",
                                     "pre_inspection = '", input$pre_inspection,"'",
            "WHERE system_id = '", clustered_samples()$system_id[row], "'")
        
        odbc::dbGetQuery(poolConn, edit_cluster_query)
        
        
      # Add the failed cases to the deny list and add an alternative 
        if (input$pre_inspection == "Failed" | input$desktop_analysis == "Failed") {
          

          #process text field to prevent sql injection
          rv$reason_step_clust <- reactive(gsub('\'', '\'\'', input$reason_clustering))
          rv$reason_step_clust_two <- reactive(special_char_replace(rv$reason_step_clust()))
          rv$reason_clust <- reactive(if(nchar(rv$reason_step_clust_two()) == 0) "NULL" else rv$reason_step_clust_two())

          
          #generate the deny tab data frame 
          deny_smps_df <- unmonitored_smp_view_postcon_on %>%
            filter(system_id == clustered_samples()$system_id[row]) %>%
            select(smp_id)
          
          deny_df <- data.frame(smp_id = deny_smps_df,
                                reason = rv$reason_clust()
                                )
          
          dbWriteTable(poolConn, SQL("fieldwork.tbl_monitoring_deny_list"), deny_df, append = TRUE, row.names = FALSE)
          
          #generate the alternative row
          alternative_system_df <- clustered_population_db %>%
            filter(batch_id == max(batch_id)) %>%
            filter(sys_modelinputcategory == clustered_samples()$sys_modelinputcategory[row] & cluster == clustered_samples()$cluster[row] & system_id != clustered_samples()$system_id[row]) 
          
          if (nrow(alternative_system_df) != 0) {
            
            random_alternative_system <- alternative_system_df[sample(nrow(alternative_system_df), 1), ]
            random_alternative_system_df <- data.frame(system_id = random_alternative_system$system_id,
                                                       cluster = random_alternative_system$cluster,
                                                       sys_modelinputcategory = random_alternative_system$sys_modelinputcategory,
                                                       sample_id = clustered_samples()$sample_id[row],
                                                       address = random_alternative_system$address,
                                                       district = random_alternative_system$district,
                                                       neighborhood = random_alternative_system$neighborhood,
                                                       desktop_analysis = "Not Determined",
                                                       pre_inspection = "Not Determined",
                                                       alternative = clustered_samples()$system_id[row])
            
            dbWriteTable(poolConn, SQL("fieldwork.tbl_clustered_samples"), random_alternative_system_df, append = TRUE, row.names = FALSE)
            
          }  
          
          
        }
        
        
        
        # Clear the selection in the table
        selected_row(NULL)
        
        #Update the output
        clustered_samples_db <- dbGetQuery(poolConn,"SELECT * from  fieldwork.tbl_clustered_samples WHERE desktop_analysis != 'Failed' AND pre_inspection != 'Failed' ORDER BY district, neighborhood")
        
        #mutate sensors deployed based on deployment records
        clustered_samples_db <- clustered_samples_db %>% 
          mutate(sensor_deployed = case_when(system_id %in% deployed_systems$system_id ~ "Yes",
                                             system_id %!in% deployed_systems$system_id ~ "No")) 
        
        clustered_samples_db <- clustered_samples_db %>%
          inner_join(smpbdv_df, by = join_by(system_id),multiple = "all") %>%
          group_by(system_id) %>%
          mutate(types = paste(smp_smptype, collapse = ', ')) %>%
          arrange(sensor_deployed) %>% 
          distinct() 
        
        #keep unique systems
        clustered_samples_db <- clustered_samples_db[!duplicated(clustered_samples_db$system_id), ]
        # make it reactive 
        clustered_samples(clustered_samples_db)
        
        
        output$clustered_table <- renderDT(
          datatable(
            clustered_samples() %>%
              select('System ID'= system_id,'SMP Type' = types,
                     Address = address, District = district, Neighborhood = neighborhood,
                     'Desktop Analysis' = desktop_analysis,
                     'Pre-Monitoring Inspection' = pre_inspection,
                     'Sensor Deployment' = sensor_deployed, 'Replaced System' = alternative) %>%
              distinct(),
            selection = 'single', 
            style = 'bootstrap',
            class = 'table-responsive, table-hover', 
            rownames = FALSE
          ) %>% 
            formatStyle(
              'Desktop Analysis',
              valueColumns = 'Desktop Analysis',
              backgroundColor = styleEqual('Passed', 'lightgreen'),
              fontWeight = styleEqual('Passed', 'bold')
            ) %>% 
            formatStyle(
              'Pre-Monitoring Inspection',
              valueColumns = 'Pre-Monitoring Inspection',
              backgroundColor = styleEqual('Passed', 'lightgreen'),
              fontWeight = styleEqual('Passed', 'bold')
            ) %>% 
            formatStyle(
              'Sensor Deployment',
              valueColumns = 'Sensor Deployment',
              backgroundColor = styleEqual('Yes', 'lightgreen'),
              fontWeight = styleEqual('Yes', 'bold')
            ) 
        )
        
        #Downloading the QA query list of systems-3 sheets of systems/smps
        output$table_dl <- downloadHandler(
          filename = function() {
            paste("Clustered_Systems_",Sys.Date(),".xlsx", sep = "")
          },
          content = function(filename){
            
            df_list <- list(unmonitored_systems= clustered_samples())
            write.xlsx(x = df_list , file = filename, rowNames = TRUE)
          }
        )
        
        #Update Deny table -------
        rv$deny_query <- reactive("select * from fieldwork.tbl_monitoring_deny_list order by smp_id")
        
        rv$deny_db <- reactive(dbGetQuery(poolConn, rv$deny_query()))
        
        rv$deny <- reactive(rv$deny_db()%>% 
                              dplyr::select(-1) %>% 
                              dplyr::rename("Reason" = "reason", "SMP ID" = "smp_id"))
        
        output$deny_table <- renderDT(
          rv$deny(), 
          selection = 'single', 
          style = 'bootstrap',
          class = 'table-responsive, table-hover', 
          rownames = FALSE
        )
        
        
        
        
        # Update the values in the drop-down menus when a row is selected
        observeEvent(input$clustered_table_rows_selected, {
          # Get the selected row
          row <- input$clustered_table_rows_selected
          
          # Update the reactive value
          if (!is.null(row) && length(row) > 0) {
            selected_row(row)
            updateSelectInput(session, "desktop_analysis", selected = clustered_samples()$desktop_analysis[row])
            updateSelectInput(session, "pre_inspection", selected = clustered_samples()$pre_inspection[row])
            #updateSelectInput(session, "sensor_deployed", selected = clustered_samples()$sensor_deployed[row])
          }
        })
        
        
      }
    })
    
    #map
    # output$maint_dc_plot <- renderPlot({
    #   ggplot() +
    #     geom_sf(data = Maint_Dist_db, aes(fill = DISTRICT)) +
    #     geom_sf_text(data = Maint_Dist_db, aes(label = DISTRICT), size = 4, fontface = "bold") +
    #     labs(title = "Maintenance Districts") +
    #     theme_void() +
    #     guides(fill = FALSE) +
    #     theme(plot.title = element_text(size = 20, hjust = 0.5))
    # 
    # 
    # })
    

    
}
    
#3.0 run App --------
# Run the application 
shinyApp(ui = ui, server = server)
