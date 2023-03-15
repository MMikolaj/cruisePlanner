
library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(tidyr)
library(googlesheets4)

source("R/funs.R")

#no need to authorize
gs4_deauth()

address <-  "https://docs.google.com/spreadsheets/d/1Sqxe8bkgopHXzCKf3Dcl1hZ7V1-DEHxBjd66velCbqY/edit?usp=sharing"


data <- read_sheet(address, sheet=1, skip = 0, na = c("NA", "missing", ""),
                   col_types = "ccccccccccc-c") 



data <- data %>% 
  filter(!is.na(`Latitude [decimal]`)) %>% 
  mutate(lng=clean_data(`Longitude [decimal]`),
         lat=clean_data(`Latitude [decimal]`),
         `Time on station [h]`=as.numeric(`Time on station [h]`),
         `Time after station [h]`=as.numeric(`Time after station [h]`),
         Description = paste(`station name`, `Department/Lab`, `person responsible`,paste("Time on station:", `Time on station [h]`), paste("Time after staton:", `Time after station [h]`), sep = "<br>")
         )

people <- extract_names(data$`person responsible`)



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#          UI                                                  #### 

### zakładki

## raw data

## lista sprzetów


ui <- fluidPage(
  
tabsetPanel(type="tabs",
            
            tabPanel("Cruise data", sidebarLayout(
              
              sidebarPanel(
                actionButton(inputId = "select_all_departments", label = "Select/deselect all"),
                checkboxGroupInput(inputId = "ID_Department", label = "Department", choices = unique(data$`Department/Lab`), selected =unique(data$`Department/Lab`) ),
                actionButton(inputId = "select_all_personel", label = "Select/deselect all"),
                checkboxGroupInput(inputId = "ID_Person", label = "Person responsible", choices = people, selected = people)
              ),
              mainPanel(
                leafletOutput("cruise_map", width = "100%", height = "600px"),
                h2("Time requirements"),
                checkboxGroupInput(inputId = "ID_sampling_time_table_gouping_variables", label="Group time requirements by:", choices = c("fjord/area", "Department/Lab", "person responsible"), selected="fjord/area", inline=T), 
                dataTableOutput("sampling_time_table"),
                radioButtons(inputId = "sampling_time_plot_grouping", label="summarise time by:", choices = c("person responsible", "Department/Lab"), selected = "person responsible", inline = T),
                plotOutput(outputId = "sampling_time_plot", width = "100%")
              )
            )),
            
            
            
            tabPanel("Raw Data", fluidRow(column(12, h1("Cruise raw data"), tableOutput("raw_data_table")))),
            
            tabPanel("Lists", fluidRow(column(6, h1("List of all gears"), tableOutput("gear_table")),
                                       column(6, h1("Crew list"), tableOutput("crew_table"))),
                     
                     )
            
            
)

)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#          server                                                  #### 


server <- function(input, output, session) {
  
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #          departments                                                  #### 
  
  
  observe({

    if (input$select_all_departments >0) {
      if(input$select_all_departments %% 2 == 0){

        updateCheckboxGroupInput(session = session,
                                 inputId = "ID_Department",
                                 choices = unique(data$`Department/Lab`),
                                 selected = unique(data$`Department/Lab`))



      } else {


        updateCheckboxGroupInput(session = session,
                                 inputId = "ID_Department",
                                 choices = unique(data$`Department/Lab`),
                                 selected = "")

        }
      }
    }
  )
  
  
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #          personel                                                  #### 
  
  observe({
    
    if(input$select_all_personel >0) {
      if(input$select_all_personel %% 2 == 0){
        
        updateCheckboxGroupInput(
                                 inputId = "ID_Person",
                                 choices = people,
                                 selected = people)
        
        
        
      } else {
        
        
        updateCheckboxGroupInput(
                                 inputId = "ID_Person",
                                 choices = people,
                                 selected = "")
        
      } 
    }
  }
  )
  
  
  
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #          map                                                  #### 
  
  
  
  
  map_data <- reactive({
    ## subset based on department
    data_s <- subset(data, `Department/Lab` %in% input$ID_Department)
    
    ## subset based on names
    # data_s <- subset(data_s, )
    
    if(is.null(input$ID_Person)){
      
      data_s <- data_s[0,]
      
    }  
    
    data_s <- data_s[grepl(paste(input$ID_Person, collapse = "|"), data_s$`person responsible`), ]
    
    return(data_s)
    
  })
  
  color_pal <- reactive({
    
    
    colorFactor(palette = "Spectral", domain=map_data()$`Department/Lab`)
    
  })
  
  

  output$cruise_map <- renderLeaflet({


      leaflet(data) %>%
      addTiles() %>%
      fitBounds(~min(lng), ~min(lat), ~max(lng), ~max(lat)) 
      # addCircleMarkers(lng=~lng, lat=~lat, stroke = F, opacity = 0.8, color = ~pal(`Department/Lab`),
      #                  popup = ~`Description (filled automatically)`,
      #                  label = ~`Description (filled automatically)`) %>%
      # addLegend("bottomright", pal = pal, values = ~`Department/Lab`, opacity = 1
      # )

  })
  
  observe({

    pal <- color_pal()


   proxy <-  leafletProxy("cruise_map")

   proxy %>%
      clearMarkers() %>%
      clearControls()

    if (nrow(map_data())>0){

     proxy %>%
     addCircleMarkers(data=map_data(), lng=~lng, lat=~lat, stroke = T, fillColor = ~pal(`Department/Lab`), fillOpacity = 0.95, color = "grey30",
                       popup =~Description,
                       label = ~`station name`) %>% 

      addLegend(data=map_data(), "bottomright", pal = pal, values = ~`Department/Lab`, opacity = 1)
    }




  })
  
  
  
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #          sampling time                                                  #### 
  
  time_requirements_grouping <- reactive({
    input$ID_sampling_time_table_gouping_variables
  })
   
  
  sampling <- reactive({

    

  sampling_time <- data %>% 
       group_by_at(time_requirements_grouping()) %>% 
      summarise(`Time on station [h]`=sum(`Time on station [h]`, na.rm = T),
                `Time after station [h]`=sum(`Time after station [h]`, na.rm = T))

    return(sampling_time)

  })
  
  output$sampling_time_table <- renderDataTable(sampling())
  
  
  
  ### plot #######
  
  plot_grouping <- reactive({
    input$sampling_time_plot_grouping
    
  })
  
  plot_data <- reactive({
    
    data %>% 
      group_by(across(input$sampling_time_plot_grouping), `fjord/area`) %>% 
      summarise(`Time on station [h]`=sum(`Time on station [h]`, na.rm = T),
                `Time after station [h]`=sum(`Time after station [h]`, na.rm = T)) %>% 
      
       pivot_longer(cols = `Time on station [h]`:`Time after station [h]`, names_to = "Time_variable", values_to = "Time") 
    
  })
  
  
  output$sampling_time_plot <- renderPlot(
    
   
plot_data() %>% 
      
      ggplot(aes(x=!!sym(plot_grouping()), y=Time_variable, size=Time, label=Time)) +
      geom_point(color="tomato")+
      geom_text(data=. %>% filter(Time>0), size=4)+
      coord_flip()+
      facet_wrap(~`fjord/area`)+
      theme_minimal()+
      labs(x="", y="")
  )
  
  
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #          raw data tab                                                  #### 
  
  
  output$raw_data_table <- renderTable({
    
    select(data, -c(`Description (filled automatically)`, lng, lat))
  })
  
  
  
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #          lists tab                                                  #### 
  
  
  #### gear ####
  
  output$gear_table <- renderTable({
   
    data %>% 
      distinct(Gear) %>% 
      mutate(Gear=strsplit(Gear, split=";")) %>% 
      unnest(Gear) %>% 
      mutate(Gear=gsub("^\\s|\\s$", "", Gear)) %>% 
      distinct() %>% 
      arrange(Gear) 
    
  }
  )
  
  #### crew ####
  
  output$crew_table <- renderTable({
    
   people <- extract_names(data$`person responsible`)
    
   data.frame(crew=people)
    
    
  }
  )
  

  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #          testing                                                  #### 
  
  
  observe({

    print(plot_data())
    # print(map_data())
    
    
    
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)
