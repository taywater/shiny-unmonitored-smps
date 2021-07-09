#Unmonitored SMPs
#Show all unmonitored active SMPs, along with a "Deny List"


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


#0.1 set up----
    options(stringsAsFactors=FALSE)
    
    #set default page length for datatables
    options(DT.options = list(pageLength = 15))
    
    #set db connection
    #using a pool connection so separate connections are unified
    #gets environmental variables saved in local or pwdrstudio environment
    poolConn <- dbPool(odbc(), dsn = "mars_testing", uid = Sys.getenv("shiny_uid"), pwd = Sys.getenv("shiny_pwd"))
    
    #disconnect from db on stop 
    onStop(function(){
        poolClose(poolConn)
    })
    
    #js warning about leaving page
    jscode <- 'window.onbeforeunload = function() { return "Please use the button on the webpage"; };'

#0.2 global variables and functions ----
    #query SMPs that have not been monitored 
    smp <- dbGetQuery(poolConn, "with cwl_smp AS (
             SELECT DISTINCT deployment_full_cwl.smp_id
               FROM fieldwork.deployment_full_cwl
    	)
    select distinct sbd.smp_id from greenit_smpbestdata sbd  where NOT (EXISTS ( SELECT cs.smp_id
               FROM cwl_smp cs
              WHERE cs.smp_id = sbd.smp_id)) 
    		  order by sbd.smp_id")  %>% 
        dplyr::arrange(smp_id) %>% 
        dplyr::pull()
    
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
                           sidebarPanel(checkboxInput("exclude_future", "Exclude SMPs with Future Deployments?"), 
                           downloadButton("download", label = "Download"), 
                           h5("This query looks for SMPs that meeting the following criteria:
                              a CAPIT Status of Construction-Substantially Complete or Closed;
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
    #2.1.1 query and show table -------
    rv$unmonitored_query <- reactive(if(input$exclude_future == FALSE){
        "select * from fieldwork.unmonitored_active_smps"
    }else{
        "select * from fieldwork.unmonitored_active_smps uas
        where not exists (select fdf.smp_id 
                          from fieldwork.future_deployments_full fdf
                          where fdf.smp_id = uas.smp_id)"
    })
    
    rv$unmonitored_sites_db <- reactive(dbGetQuery(poolConn, rv$unmonitored_query()))
    
    rv$unmonitored_sites <- reactive(rv$unmonitored_sites_db() %>%  
                                         mutate(across(other_cwl_at_this_system,  
                                                       ~ case_when(. == 1 ~ "Yes", 
                                                                   . == 0 ~ "No"))) %>% 
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
            if(input$exclude_future == TRUE){
                paste("unmonitored_sites_with_future_deployments_excluded_", Sys.Date(), ".csv", sep = "")
            }else{
                paste("unmonitored_sites_", Sys.Date(), ".csv", sep = "") 
            }
        }, 
        content = function(file){
            write.csv(rv$unmonitored_sites(), file, row.names = FALSE)
        }
    )
    
    
    #2.2 deny tab----
    
    #2.2.1 updates system ids ----
    updateSelectizeInput(session, "smp_id", choices = smp, selected = character(0), server = TRUE)
    
    #2.2.2 query and show table -------
    rv$deny_query <- reactive("select * from fieldwork.monitoring_deny_list order by smp_id")
    
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
            add_smp_query <- paste0("INSERT INTO fieldwork.monitoring_deny_list (smp_id, reason)
                                    VALUES('", input$smp_id, "', ", rv$reason(), ")")
            
            dbGetQuery(poolConn, add_smp_query)
        }else{
            edit_smp_query <- paste0(
                "UPDATE fieldwork.monitoring_deny_list SET 
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
                   paste0("DELETE FROM fieldwork.monitoring_deny_list WHERE smp_id = '",
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
    
}
    
#3.0 run App --------
# Run the application 
shinyApp(ui = ui, server = server)
