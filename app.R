#Unmonitored SMPs
#Show all unmonitored active SMPs, along with a "Deny List"

#load libraries ----
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


#set up----
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

#global variables and functions ----
    #query SMPs that have not been monitored 
    smp <- dbGetQuery(poolConn, "with cwl_smp AS (
             SELECT DISTINCT deployment_full_cwl.smp_id
               FROM fieldwork.deployment_full_cwl
    	)
    select distinct sfc.smp_id from smpid_facilityid_componentid sfc  where NOT (EXISTS ( SELECT cs.smp_id
               FROM cwl_smp cs
              WHERE cs.smp_id = sfc.smp_id)) 
    		  order by sfc.smp_id")  %>% 
        dplyr::arrange(smp_id) %>% 
        dplyr::pull()
    
    #replace special characters with friendlier characters
    special_char_replace <- function(note){
        
        note_fix <- note %>% 
            str_replace_all(c("•" = "-", "ï‚§" = "-", "“" = '"', '”' = '"'))
        
        return(note_fix)
        
    }

# Define UI
ui <-  navbarPage("MARS Unmonitored Active SMPs", theme = shinytheme("cerulean"),
                  tabPanel("Unmonitored Active SMPs", value = "main_tab", 
                           titlePanel("Unmonitored Active SMPs"),
                           sidebarPanel(checkboxInput("exclude_future", "Exclude SMPs with Future Deployments?"), 
                           downloadButton("download", label = "Download")
                           ),
                           mainPanel(
                                DTOutput("unmonitored_table")
                                ) 
                  ), 
                  tabPanel("Deny List", value = "deny_tab", 
                           titlePanel("SMPs Denied Monitoring (to be excluded from main tab)"),
                           sidebarPanel(selectizeInput("smp_id", "SMP ID", choices = NULL, 
                                                       options = list(
                                                           placeholder = 'Select an Option', 
                                                            onInitialize = I('function() { this.setValue(""); }')
                                                       )), 
                                        textAreaInput("reason", "Reason", height = '85px'), 
                                        actionButton("add_smp", "Add SMP to Deny List"), 
                                        disabled(actionButton("remove_smp", "Remove SMP from Deny List"))), 
                           mainPanel(
                           DTOutput("deny_table")
                           ), 
                           #must call useShinyjs() for shinyjs() functionality to work in app
                           useShinyjs()
                  )
)

# Define server logic
server <- function(input, output, session) {

    rv <- reactiveValues()
    
    #unmonitored tab -----
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
    
    
    output$download <- downloadHandler(
        filename = function(){
            if(input$exclude_future == TRUE){
                paste("unmonitored_sites_with_future_deployments_excluded_", Sys.Date(), ".csv", sep = "")
            }else{
                paste("future_deployments_", Sys.Date(), ".csv", sep = "") 
            }
        }, 
        content = function(file){
            write.csv(rv$unmonitored_sites(), file, row.names = FALSE)
        }
    )
    
    
    #deny tab----
    
    #updates system ids
    updateSelectizeInput(session, "smp_id", choices = smp, selected = character(0), server = TRUE)
    
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
    
    #process text field to prevent sql injection
    rv$reason_step <- reactive(gsub('\'', '\'\'', input$reason))
    rv$reason_step_two <- reactive(special_char_replace(rv$reason_step()))
    rv$reason <- reactive(if(nchar(rv$reason_step_two()) == 0) "NULL" else paste0("'", rv$reason_step_two(), "'"))
    
    #toggle state of add/edit based on whether SMP ID and Reason fields are selected
    observe(toggleState(id = "add_smp", condition = nchar(input$smp_id) > 0 & nchar(input$reason) > 0))
    
    #toggle text on add/edit
    rv$label <- reactive(if(!(input$smp_id %in% rv$deny_db()$smp_id)) "Add SMP" else "Edit SMP")
    observe(updateActionButton(session, "add_smp", label = rv$label()))
    
    #toggle state of remove based on whether row is selected
    observe(toggleState(id = "remove_smp", condition = length(input$deny_table_rows_selected) != 0))
    
    #update values based on selected row 
    observeEvent(input$deny_table_rows_selected, {
        updateSelectizeInput(session, "smp_id", selected = rv$deny_db()$smp_id[input$deny_table_rows_selected])
        updateTextAreaInput(session, "reason", value = rv$deny_db()$reason[input$deny_table_rows_selected])
    })
    
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
    
    #remove from deny list
    observeEvent(input$remove_smp, {
        showModal(modalDialog(title = "Remove from Deny List", 
                              "Are you sure you want to remove this SMP from the deny list?", 
                              modalButton("No"), 
                              actionButton("confirm_removal", "Yes")))
    })
    
    #confirm removal
    observeEvent(input$confirm_removal, {
        dbGetQuery(poolConn, 
                   paste0("DELETE FROM fieldwork.monitoring_deny_list WHERE monitoring_deny_list_uid = '",
                          rv$deny_db()[input$deny_table_rows_selected, 1], "'"))
        
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

# Run the application 
shinyApp(ui = ui, server = server)
