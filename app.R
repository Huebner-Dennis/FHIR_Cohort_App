httr::set_config(httr::config(ssl_verifypeer = FALSE))

library(shiny)
library(bslib)
library(fhircrackr)
library(jsonlite)
library(DBI)
library(echarts4r)
library(tidyverse)
library(httr)
library(plotly)


# Define UI for application that draws a histogram
ui <- page_navbar(
  
  title="FDPG-Cohort-Explorer",
  bg = "lightblue",
  sidebar = sidebar(
    fileInput("json", "Choose .json File", accept = ".json"),
    actionButton("process_button", "Process Data")
  ),
  
  tags$style(HTML("
    body {
      background-color: #e6ebef;
    }
  ")),
  
  # Add custom CSS for the card
  tags$style(HTML("
    .custom-card {
      background-color: #e6ebef;
      color: #206fa8; /* Change text color if needed */
    }
    
  ")),
  
  tags$div(style = "position: fixed; top: 0; right: 0; padding: 5px; z-index: 30000; display: flex; gap: 5px;",
           
           # First Logo
           tags$a(
                  tags$img(src = 'mul-ct_logo_weiss_rgb.png',
                           title = "MUL-CT", height = "42px")),
           
           # Second Logo
           tags$a(
                  tags$img(src = 'DIZ_deutsch_RGB_transparent.png',
                           title = "DIZ", height = "42px"))
  ),
  
  fluidRow(
    column(width=4,
           card(
            class = "custom-card",
            full_screen = TRUE,
            card_header("Verteilung nach Geschlecht"),
            echarts4rOutput("gender")
           ),
           
           card(
             class = "custom-card",
             full_screen = TRUE,
             card_header("Diagnose"),
             plotlyOutput("diagnose")
           )
           
           
           
    ),
    
    column(width=4,
           
           
           card(
             class = "custom-card",
             full_screen = TRUE,
             card_header("Laboruntersuchungen"),
             plotlyOutput("laboruntersuchungen")
           ),
           
           card(
             class = "custom-card",
             full_screen = TRUE,
             card_header("Primärdiagnose"),
             plotlyOutput("primärdiagnosen")
           ),
          
    ),
    
    column(width=4,
           
           card(
             class = "custom-card",
             full_screen = TRUE,
             card_header("Prozeduren"),
             plotlyOutput("prozeduren")
           ),
           
           card(
             class = "custom-card",
             full_screen = TRUE,
             card_header("Sekundärdiagnosen"),
             plotlyOutput("sekundärdiagnosen")
           ),
    ),
    
    card(
      class = "custom-card",
      full_screen = TRUE,
      card_header("Medikation"),
      plotlyOutput("medication")
    )
  )
)


server <- function(input, output, session) {
  # Reactive value to store the patient data
  patient_data <- reactiveVal(NULL)
  
  
  observeEvent(input$json, {
    req(input$json)  # Ensure the file is uploaded
    
    # Read the JSON file into a reactive value
    json_data <-  fromJSON(input$json$datapath) 
 
    patient_data(json_data)  
    
    # Update status or perform any other UI-related actions
    shinyjs::enable("process_button")  # Enable the process button once file is uploaded
  })
  
  observeEvent(input$process_button, {
    req(patient_data())  # Ensure data is available
    
    withProgress(message = "Processing...", value = 0, {
      incProgress(0.1, detail = "Processing data...")
      
      # Extract gender data
      json_string <- toJSON(patient_data(), auto_unbox = TRUE)
   
      post <- POST(
        url = "https://fdpg-torch.ctk/api/data/",
        body = json_string,
        encode = "json",
        authenticate("torch", "j*cVR8:F479^")
      )
      
      uuids <- NULL
      while (is.null(uuids)) {
        Sys.sleep(5)  # Wait for 5 seconds before checking again
        get <- GET(
          url = paste0("https://fdpg-torch.ctk/api/data/", content(post)),
          authenticate("torch", "j*cVR8:F479^")
        )
        uuids <- content(get)  
        }
      
      if(length(uuids) > 10) {

      
   #Patient --------------
      print("Patient start")
      incProgress(0.2, detail = "Rendering Patients ...")
      
      patient = data.frame()
      
        for (i in uuids) {
        
        access_token = fhir_authenticate(
          secret = Sys.getenv("secret"),
          key = Sys.getenv("key"),
          base_url = Sys.getenv("base_url"), 
          access = Sys.getenv("access"), 
          authorize = Sys.getenv("authorize"),
          query_authorize_extra = list(scope = "openid", grant_type = "client_credentials")
        )
        
        table_description_patient <- fhir_table_description(
          resource = "Patient",
          cols = c("gender"),
          sep = " | ",
          format = "compact",
          keep_attr = FALSE
        )
        
        # Retrieve patient data
        patient_bundles <- fhir_get_resources_by_ids(
          base_url = Sys.getenv("base_url"),
          resource = "Patient",
          ids = i,
          token = access_token,  
          verbose = 0,
          parameters = list("_elements" = "gender"),
          id_param = "_id"
        )
        
        patient_data <- fhir_crack(patient_bundles, design = table_description_patient, ncores = 5)
        
       patient = rbind(patient,patient_data)
        
      }
        
       # Render gender plot
      output$gender <- renderEcharts4r({
              patient %>%
            dplyr::filter(!is.na(gender) & gender != "") %>%
            dplyr::group_by(gender) %>%
            dplyr::summarise(Anzahl = n(), .groups = "drop") %>%
            e_charts(gender) |> 
            e_pie(Anzahl, radius = c("40%", "70%")) %>%
            e_tooltip(trigger = "item") %>%
            e_legend(top = "5%") %>%
            e_text_g(
              elements = list(
                list(
                  type = "text",
                  left = "center",
                  top = "center",
                  style = list(
                    text = paste("Patienten\n", nrow(patient)),
                    textAlign = "center",
                    fill = "#000",  
                    fontSize = 20,
                    fontWeight = "bold"
                  )
                )
              )
            )
        })
      
    # Condition ------------

        incProgress(0.4, detail = "Rendering Conditions ...")  
        
        diagnose = data.frame()
        primärdiagnose = data.frame()
        sekundärdiagnose = data.frame()
        
     for (i in uuids) {
       access_token = fhir_authenticate(
         secret = Sys.getenv("secret"),
         key = Sys.getenv("key"),
         base_url = Sys.getenv("base_url"), 
         access = Sys.getenv("access"), 
         authorize = Sys.getenv("authorize"),
         query_authorize_extra = list(scope = "openid", grant_type = "client_credentials")
       )
       
       table_description_condition <- fhir_table_description(
         resource = "Condition",
         cols = c(
           Hauptdiagnose_ICD10 = "code/coding/code",
           Hauptdiagnose_display = "code/coding/display",
           Kennzeichen = 
             "code/coding/extension/valueCoding[system[@value='http://fhir.de/CodeSystem/icd-10-gm-mehrfachcodierungs-kennzeichen']]/code"
         ),
         sep = " | ",
         format = "compact",
         keep_attr = FALSE
       )
       
       condition_bundles <- fhir_get_resources_by_ids(
         base_url = Sys.getenv("base_url"),
         resource = "Condition",
         ids = i,
         token = access_token,
         verbose = 0,
         parameters = list("_elements" = "code"),
         id_param = "patient"
       )
       
       diagnose_data <- fhir_crack(condition_bundles, design = table_description_condition, ncores = 5) %>% 
         dplyr::filter(is.na(Kennzeichen))
       
       diagnose=rbind(diagnose,diagnose_data)
       
       primärdiagnose_data <- fhir_crack(condition_bundles, design = table_description_condition, ncores = 5) %>% 
         dplyr::filter(Kennzeichen == "†")
       
       primärdiagnose = rbind(primärdiagnose, primärdiagnose_data)
       
       sekundärdiagnose_data <- fhir_crack(condition_bundles, design = table_description_condition, ncores = 5) %>% 
         dplyr::filter(Kennzeichen == "*") 
       
       sekundärdiagnose = rbind(sekundärdiagnose, sekundärdiagnose_data)
       
     }
        
        
        output$diagnose <- renderPlotly({
          diagnose %>% 
            dplyr::group_by(Hauptdiagnose_ICD10, Hauptdiagnose_display) %>% 
            dplyr::summarise(Anzahl = n()) %>% 
            plot_ly(x = ~reorder(Hauptdiagnose_ICD10,Anzahl), 
                    y = ~Anzahl, 
                    type = 'bar', 
                    color = ~Hauptdiagnose_display,
                    colors = "Purples",
                    hoverinfo = "text",
                    hovertemplate = ~paste('ICD10: %{x}',
                                           '<br>Anzahl:%{y} <br>',
                                           Hauptdiagnose_display)) %>% 
            layout(barmode = 'stack',
                   yaxis = list(title = "Anzahl"),
                   xaxis = list(title = "ICD10",
                                rangeslider = list(visible = TRUE)
                   ),
                   paper_bgcolor = 'rgba(0,0,0,0)',
                   plot_bgcolor = 'rgba(0,0,0,0)',
                   showlegend = FALSE) 
        })
        
        
        output$primärdiagnosen <- renderPlotly({
          primärdiagnose %>% 
            dplyr::group_by(Hauptdiagnose_ICD10, Hauptdiagnose_display) %>% 
            dplyr::summarise(Anzahl = n()) %>% 
            plot_ly(x = ~reorder(Hauptdiagnose_ICD10,Anzahl), 
                    y = ~Anzahl, 
                    type = 'bar', 
                    color = ~Hauptdiagnose_display,
                    colors = "Blues",
                    hoverinfo = "text",
                    hovertemplate = ~paste('ICD10: %{x}',
                                           '<br>Anzahl:%{y} <br>',
                                           Hauptdiagnose_display)) %>% 
            layout(barmode = 'stack',
                   yaxis = list(title = "Anzahl"),
                   xaxis = list(title = "ICD10",
                                rangeslider = list(visible = TRUE)
                   ),
                   paper_bgcolor = 'rgba(0,0,0,0)',
                   plot_bgcolor = 'rgba(0,0,0,0)',
                   showlegend = FALSE) 
        })
        
        
        output$sekundärdiagnosen <- renderPlotly({
          sekundärdiagnose %>% 
            dplyr::group_by(Hauptdiagnose_ICD10, Hauptdiagnose_display) %>% 
            dplyr::summarise(Anzahl = n()) %>% 
            plot_ly(x = ~reorder(Hauptdiagnose_ICD10,Anzahl), 
                    y = ~Anzahl, 
                    type = 'bar', 
                    color = ~Hauptdiagnose_display,
                    colors = "Oranges",
                    hoverinfo = "text",
                    hovertemplate = ~paste('ICD10: %{x}',
                                           '<br>Anzahl:%{y} <br>',
                                           Hauptdiagnose_display)) %>% 
            layout(barmode = 'stack',
                   yaxis = list(title = "Anzahl"),
                   xaxis = list(title = "ICD10",
                                rangeslider = list(visible = TRUE)
                   ),
                   paper_bgcolor = 'rgba(0,0,0,0)',
                   plot_bgcolor = 'rgba(0,0,0,0)',
                   showlegend = FALSE) 
        })
     
    # Prozeduren---------------
        incProgress(0.4, detail = "Rendering Procedures ...")   
        
        procedure = data.frame()

        for (i in uuids) {
          
          access_token = fhir_authenticate(
            secret = Sys.getenv("secret"),
            key = Sys.getenv("key"),
            base_url = Sys.getenv("base_url"), 
            access = Sys.getenv("access"), 
            authorize = Sys.getenv("authorize"),
            query_authorize_extra = list(scope = "openid", grant_type = "client_credentials")
          )
          
          table_description_procedure <- fhir_table_description(
            resource = "Procedure",
            cols = c(ops.code = "code/coding/code",
                     ops.code.display = "code/coding/display"),
            sep = " | ",
            format = "compact",
            keep_attr = FALSE
          )
          
          procedure_bundles <- fhir_get_resources_by_ids(
            base_url = Sys.getenv("base_url"), 
            resource = "Procedure",
            ids = i,
            token = access_token,
            verbose = 0,
            parameters = list("_elements" = "code"),
            id_param = "patient"
          )
          
          procedure_data <- fhir_crack(procedure_bundles, design = table_description_procedure, ncores = 5)
          
          procedure = rbind(procedure, procedure_data)
          
        }
      
        
      output$prozeduren <- renderPlotly({
          
          procedure %>% 
            dplyr::group_by(ops.code,ops.code.display) %>% 
            dplyr::summarise(Anzahl = n()) %>% 
            plot_ly(x = ~reorder(ops.code,Anzahl), 
                    y = ~Anzahl, 
                    type = 'bar', 
                    color = ~ops.code.display, 
                    colors = "Greens",
                    hovertemplate = ~paste('OPS: %{x}',
                                           '<br>Anzahl: %{y} <br>',
                                           ops.code.display)) %>% 
            layout(barmode = 'stack',
                   xaxis = list(title = "OPS", rangeslider = list(visible = TRUE)),
                   yaxis = list(title = "Anzahl"),
                   paper_bgcolor = 'rgba(0,0,0,0)',
                   plot_bgcolor = 'rgba(0,0,0,0)',
                   showlegend = FALSE)
        })
        

    # Labor --------

        incProgress(0.4, detail = "Rendering Observations ...")  
    
        lab = data.frame()
        
        for (i in uuids) {
          access_token = fhir_authenticate(
            secret = Sys.getenv("secret"),
            key = Sys.getenv("key"),
            base_url = Sys.getenv("base_url"), 
            access = Sys.getenv("access"), 
            authorize = Sys.getenv("authorize"),
            query_authorize_extra = list(scope = "openid", grant_type = "client_credentials")
          )
          
          table_description_lab <- fhir_table_description(
            resource = "Observation",
            cols = c(LOINC_Laborparameter = "code/coding/code"),
            sep = " | ",
            format = "compact",
            keep_attr = FALSE
          )
          
          lab_bundles <- fhir_get_resources_by_ids(
            base_url = Sys.getenv("base_url"),
            resource = "Observation",
            ids = i,
            token =  access_token,
            verbose = 0,
            parameters = list(
              "_profile"="https://www.medizininformatik-initiative.de/fhir/core/modul-labor/StructureDefinition/ObservationLab",
              "_elements" = "code"),
            id_param = "patient"
          )
          
          lab_data <- fhir_crack(lab_bundles, design = table_description_lab, ncores = 5)
          
          lab = rbind(lab, lab_data)
          
        }
        
        output$laboruntersuchungen <- renderPlotly({
          
          lab %>% 
            dplyr::group_by(LOINC_Laborparameter) %>% 
            dplyr::summarise(Anzahl = n()) %>% 
            drop_na() %>% 
            plot_ly(x = ~reorder(LOINC_Laborparameter, Anzahl),
                    y = ~Anzahl,
                    type = 'bar', 
                    colors = "Reds",
                    hovertemplate = ~paste('LOINC: %{x}',
                                           '<br>Anzahl: %{y} <br>'
                                           )) %>%  
            layout(barmode = 'stack',
                   yaxis = list(title = "Anzahl"),
                   xaxis = list(title = "LOINC", rangeslider=list(visible = TRUE)),
                   paper_bgcolor = 'rgba(0,0,0,0)',
                   plot_bgcolor = 'rgba(0,0,0,0)',
                   showlegend = FALSE)
          
          
        })

 #       #Medication----------------

        incProgress(0.6, detail = "Rendering Medications ...")  
     
      
      
        table_description_medication <- fhir_table_description(
          resource = "Medication",
          cols = c(
            ATC = "code/coding[system[@value='http://fhir.de/CodeSystem/bfarm/atc']]/code",
            ATC.display = "code/coding[system[@value='http://fhir.de/CodeSystem/bfarm/atc']]/display"
          ),
          sep = " | ",
          format = "compact",
          keep_attr = FALSE
        )
        
        request = fhir_url(url = Sys.getenv("base_url"), 
                           resource = "Medication"
        )
        
        # Initialize an empty data frame
        medication <- data.frame()
        
        for (i in uuids) {
          
        access_token <- fhir_authenticate(
          secret = Sys.getenv("secret"),
          key = Sys.getenv("key"),
          base_url = Sys.getenv("base_url"), 
          access = Sys.getenv("access"), 
          authorize = Sys.getenv("authorize"),
          query_authorize_extra = list(scope = "openid", grant_type = "client_credentials")
          )
          
          body = fhir_body(content = paste0("_has:MedicationAdministration:medication:patient=",i), type="application/x-www-form-urlencoded")
          
          medication_bundles <- fhir_search(request = request, 
                                            body=body, 
                                            verbose = 0, 
                                            token = access_token) 
          
          
          medication_data <- fhir_crack(medication_bundles, design = table_description_medication, ncores = 5)
          
     
          medication <- rbind(medication, medication_data)
        }
        
        output$medication <- renderPlotly({
          medication %>% 
            dplyr::group_by(ATC, ATC.display) %>% 
            dplyr::summarise(Anzahl = n()) %>% 
            drop_na() %>% 
            plot_ly(x = ~reorder(ATC, Anzahl),
                    y = ~Anzahl,
                    type = 'bar', 
                    color = ~ATC.display,
                    colors = "Reds",
                    hovertemplate = ~paste('ATC: %{x}',
                                           '<br>Anzahl: %{y} <br>',
                                           ATC.display)) %>%  
            layout(barmode = 'stack',
                   yaxis = list(title = "Anzahl"),
                   xaxis = list(title = "ATC", rangeslider=list(visible = TRUE)),
                   paper_bgcolor = 'rgba(0,0,0,0)',
                   plot_bgcolor = 'rgba(0,0,0,0)',
                   showlegend = FALSE)
        })
        

    } else{
      showModal(modalDialog(
        title = "Error",
        "Es sind weniger als 10 Patienten in Ihrer Anfrage erhalten. 
        Bitte stellen Sie eine neue Anfrage im FDPG, damit eine Kernanonymität von mehr als 10 Patienten gewährleistet wird.",
        size = "xl",
        fade = TRUE,
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }
        
      })
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
