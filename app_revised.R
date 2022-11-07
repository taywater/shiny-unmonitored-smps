#Unmonitored SMPs
#Show all unmonitored active SMPs, along with a "Deny List"
# Revised by Farshad Ebrahimi on 8/24/2022


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
    #create negate of %in%
    `%!in%` = Negate(`%in%`)
  

#0.1 set up----
    options(stringsAsFactors=FALSE)
    
    #set default page length for datatables
    options(DT.options = list(pageLength = 15))
    
    #set db connection
    #gets environmental variables saved in local or pwdrstudio environment
    con <- dbConnect(odbc::odbc(), dsn = "mars14_data", uid = Sys.getenv("shiny_uid"), pwd = Sys.getenv("shiny_pwd"), MaxLongVarcharSize = 8190  )
    #js warning about leaving page
    jscode <- 'window.onbeforeunload = function() { return "Please use the button on the webpage"; };'

#0.2 global variables and functions plus exclusion/incluion lists for unmonitored sites ----
    #query SMPs that have not been monitored 
    smp <- dbGetQuery(con, "with cwl_smp AS (
             SELECT DISTINCT fieldwork.viw_deployment_full_cwl.smp_id
               FROM fieldwork.viw_deployment_full_cwl
    	)
    select distinct sbd.smp_id from external.tbl_smpbdv sbd  where NOT (EXISTS ( SELECT cs.smp_id
               FROM cwl_smp cs
              WHERE cs.smp_id = sbd.smp_id)) 
    		  order by sbd.smp_id")  %>% 
      dplyr::arrange(smp_id) %>% 
      dplyr::pull()
    
    fieldwork.tbl_monitoring_deny_list <- dbGetQuery(con,"SELECT * FROM fieldwork.tbl_monitoring_deny_list")
    fieldwork.tbl_capture_efficiency <- dbGetQuery(con,"SELECT system_id FROM fieldwork.tbl_capture_efficiency")
    fieldwork.tbl_porous_pavement <- dbGetQuery(con,"select * from fieldwork.tbl_porous_pavement") %>%
      filter(test_date > now()-years(2))
    online_inlets <- dbGetQuery(con,"SELECT * from fieldwork.viw_all_inlets") %>%
      filter(plug_status == "ONLINE") %>%
      select(smp_id)
    greenit_built_info <- dbGetQuery(con,"SELECT external.tbl_smpbdv.smp_id,
            external.tbl_smpbdv.system_id,
            external.tbl_smpbdv.smp_notbuiltretired,
            external.tbl_smpbdv.smp_smptype,
            external.tbl_smpbdv.capit_status
           FROM external.tbl_smpbdv
          WHERE external.tbl_smpbdv.smp_notbuiltretired IS NULL AND external.tbl_smpbdv.smp_smptype <> 'Depaving'::text")
    greenit_built_info_stromwatertree <- greenit_built_info %>%
      filter(smp_smptype == "Stormwater Tree")
    srt_systems <-dbGetQuery(con,"SELECT DISTINCT *
           FROM fieldwork.viw_srt_full")
    cwl_smp <- dbGetQuery(con,"SELECT DISTINCT *
           FROM fieldwork.viw_deployment_full_cwl")
    fieldwork.deployment_full <- dbGetQuery(con,"SELECT *
           FROM fieldwork.viw_deployment_full")
    cwl_system <- dbGetQuery(con,"SELECT DISTINCT admin.fun_smp_to_system(fieldwork.viw_deployment_full_cwl.smp_id::text) AS system_id
           FROM fieldwork.viw_deployment_full_cwl")
    gso_info <- dbGetQuery(con,"select * from external.viw_gso_maintenance")%>% 
      filter(maintained == 1) %>%
      select(smp_id)
    fieldwork.viw_future_deployments_full <- dbGetQuery(con, "select * from fieldwork.viw_future_deployments_full")
    #Maintained and online smp list
    maintained_online_smp <- union_all(online_inlets, gso_info) %>%
      distinct()
    
    #output tables generated here
    #both check box off
    output_table_1 <- greenit_built_info %>%
      select(smp_id, system_id, smp_type = smp_smptype, capit_status ) %>%
      mutate("other_cwl_at_this_system" =  case_when(system_id %in% cwl_system$system_id ~ TRUE,
                                                     system_id %!in% cwl_system$system_id ~ FALSE)) %>%
      filter(smp_id %in% maintained_online_smp$smp_id &
               smp_id %!in%  cwl_smp$smp_id &
               system_id %!in% srt_systems$system_id &
               smp_id %!in% fieldwork.tbl_monitoring_deny_list$smp_id &
               greenit_built_info_stromwatertree$system_id %!in% fieldwork.tbl_capture_efficiency$system_id &
               smp_id %!in% fieldwork.tbl_porous_pavement$smp_id) %>%
      select(-system_id) %>%
      distinct()
    
    #just future deployment on
    output_table_2 <- output_table_1 %>%
      filter(smp_id %!in% fieldwork.viw_future_deployments_full$smp_id)
    
    #both check boxes on
    non_post_con_list <- srt_systems %>%
      filter(phase !="Post-Construction") 
    fieldwork.deployment_full["system_id"] <- gsub('-\\d+$','',fieldwork.deployment_full$smp_id)
    special_cases <- fieldwork.deployment_full %>%
      inner_join(non_post_con_list, by=c("system_id"="system_id","deployment_dtime_est"="test_date"))
    srt_systems <- srt_systems %>%
      filter(phase =="Post-Construction")
    cwl_smp <- cwl_smp %>%
      anti_join(special_cases, by="deployment_uid")
    
    output_table_3 <- greenit_built_info %>%
      select(smp_id, system_id, smp_type = smp_smptype, capit_status ) %>%
      mutate("other_cwl_at_this_system" =  case_when(system_id %in% cwl_system$system_id ~ TRUE,
                                                     system_id %!in% cwl_system$system_id ~ FALSE)) %>%
      filter(smp_id %in% maintained_online_smp$smp_id &
               smp_id %!in%  cwl_smp$smp_id &
               system_id %!in% srt_systems$system_id &
               smp_id %!in% fieldwork.tbl_monitoring_deny_list$smp_id &
               greenit_built_info_stromwatertree$system_id %!in% fieldwork.tbl_capture_efficiency$system_id &
               smp_id %!in% fieldwork.tbl_porous_pavement$smp_id &
               smp_id %!in% fieldwork.viw_future_deployments_full$smp_id) %>%
      select(-system_id) %>%
      distinct()
    
    #just postcon check box on
    output_table_4 <- greenit_built_info %>%
      select(smp_id, system_id, smp_type = smp_smptype, capit_status ) %>%
      mutate("other_cwl_at_this_system" =  case_when(system_id %in% cwl_system$system_id ~ TRUE,
                                                     system_id %!in% cwl_system$system_id ~ FALSE)) %>%
      filter(smp_id %in% maintained_online_smp$smp_id &
               smp_id %!in%  cwl_smp$smp_id &
               system_id %!in% srt_systems$system_id &
               smp_id %!in% fieldwork.tbl_monitoring_deny_list$smp_id &
               greenit_built_info_stromwatertree$system_id %!in% fieldwork.tbl_capture_efficiency$system_id &
               smp_id %!in% fieldwork.tbl_porous_pavement$smp_id) %>%
      select(-system_id) %>%
      distinct()

    #replace special characters with friendlier characters
    special_char_replace <- function(note){
        
        note_fix <- note %>% 
            str_replace_all(c("•" = "-", "ï‚§" = "-", "“" = '"', '”' = '"'))
        
        return(note_fix)
        
    }

# 1.0 UI --------
ui <-  navbarPage("MARS Unmonitored Active SMPs", theme = shinytheme("cerulean"),
                  #1.1 Unmonitored Active SMPs -------
                  tabPanel("Unmonitored Active SMPs", value = "main_tab", 
                           titlePanel("Unmonitored Active SMPs"),
                           #1.1.1 sidebarPanel ------
                           sidebarPanel(checkboxInput("exclude_future", "Exclude SMPs with Future Deployments?", value = TRUE), 
                                        checkboxInput("exclude_postcon", "Only Exclude Post-Construction Monitoring or Testing?", value = TRUE),
                           downloadButton("download", label = "Download"), 
                           h5("This query looks for SMPs that meeting the following criteria:
                              PWD is responsible for surface, subsurface, and/or porous maintenance;
                              SMP is not \"not built\" or \"retired\";
                              no inactive or plugged inlets, structures, or conveyance;
                              no continuous water level monitoring or SRTs performed; 
                              if stormwater tree, no CET; 
                              if porous pavement, no test within the past two years.
                              ")
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
    
    rv$deny_db <- reactive(dbGetQuery(con, rv$deny_query()))
    
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
            
            dbGetQuery(con, add_smp_query)
        }else{
            edit_smp_query <- paste0(
                "UPDATE fieldwork.tbl_monitoring_deny_list SET 
                reason = ", rv$reason(), "
                WHERE smp_id = '", input$smp_id, "'")
            
            dbGetQuery(con, edit_smp_query)
        }
        
        #update deny table
        rv$deny_db <- reactive(dbGetQuery(con, rv$deny_query()))
        #update main table with new/removed smp 
        rv$unmonitored_sites_db <- reactive(dbGetQuery(con, rv$unmonitored_query()))
        
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
        dbGetQuery(con, 
                   paste0("DELETE FROM fieldwork.tbl_monitoring_deny_list WHERE smp_id = '",
                          input$smp_id, "'"))
        
        #update deny table
        rv$deny_db <- reactive(dbGetQuery(con, rv$deny_query()))
        #update main table with new/removed smp 
        rv$unmonitored_sites_db <- reactive(dbGetQuery(con, rv$unmonitored_query()))
        
        #clear fields
        reset("smp_id")
        reset("reason")
        
        #remove pop up
        removeModal()
    })
    
}
    
#3.0 run App --------
# Run the application 
shinyApp(ui = ui, server = server)
