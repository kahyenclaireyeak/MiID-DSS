#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)
library(readxl)
library(dplyr)
library(tidyverse)
library(shinyWidgets)
library(writexl)
library(DT)
library(data.table)
library(markdown)
library(shinyjs)
library(bslib)
library(shinyFeedback)
### 

## get the system date

sysdate <- Sys.Date()

##load background excel sheet
## load the excel sheet for food categories
food_categories <- read_excel("HI_Microbial_hazards_identification_processed_database.xlsx",sheet = "Step_1b_HFP_subcategory")


## load food products
food_products <- HI_Microbial_hazards_identification_processed_database <- read_excel("HI_Microbial_hazards_identification_processed_database.xlsx", sheet = "Step_1a_FPS")

## load the inactivation sheet (+wet/dry processing)
inactivationsheet <- read_excel("HI_Microbial_hazards_identification_processed_database.xlsx", sheet = "Step_2_PE")

## input options
inputoptions <- str_to_sentence(food_products$Food_item)
inputoptionssaffi <- str_to_sentence(na.omit(unique(food_products$Food_subcategory_1)))
inputprocessing <- unique(inactivationsheet$Processing_condition)


## recontamination risk
recontamination <- read_excel("HI_Microbial_hazards_identification_processed_database.xlsx", sheet = "Step_3_RP")

recontamination$Hazard_origin = "Recontamination"


### Product properties characteristics
PPC <- read_excel("HI_Microbial_hazards_identification_processed_database.xlsx", sheet = "Step_4_GO")

## define table for Processing thermal
PTE_DES <- read_excel("HI_Microbial_hazards_identification_processed_database.xlsx", sheet = "Step_2_PEdes")

#define pictogram inputs
logo <- div(tags$img(src="https://github.com/alexdank/pics/blob/main/MIDI%20logo.jpg?raw=true", alt="Decision support system graphical overview", deleteFile=FALSE, width='300px',height='100px'))
DSS_scheme <- div(tags$img(src="https://github.com/alexdank/pics/blob/main/Microbial%20Hazards%20Identification%20DSS%20procedures.jpg?raw=true", alt="Decision support system graphical overview", deleteFile=FALSE, width='600px',height='500px'))

### function to color based on "Original, in tab 3 and 4, 
render <- c(
  "function(data, type, row){",
  "  if(type === 'display'){",
  "    var color = /Original$/.test(data) ? 'green' : (/Recontamination$/.test(data) ? 'red' : 'black');",
  "    data = '<span style=\"color: ' + color + ';\">' + data + '</span>';",
  "  }",
  "  return data;",
  "}"
)

## function to change barcolor of slider 1
barcol <- tags$head(tags$style(HTML('.js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
                                                  background: #18BC9C;
                                                  border-top: 1px solid #18BC9C ;
                                                  border-bottom: 1px solid #18BC9C ;}

                            /* changes the colour of the number tags */
                           .irs-from, .irs-to, .irs-single { background: #18BC9C }')))

## function to change barcolor of slider 2
barcol1 <- tags$head(tags$style(HTML('.js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {
                                                  background: #18BC9C;
                                                  border-top: 1px solid #18BC9C ;
                                                  border-bottom: 1px solid #18BC9C ;}

                            /* changes the colour of the number tags */
                           .irs-from, .irs-to, .irs-single { background: #18BC9C }')))

## Function for displaying empty rows
rowCallback <- c(
  "function(row, data){",
  "  for(var i=0; i<data.length; i++){",
  "    if(data[i] === null){",
  "      $('td:eq('+i+')', row).html('NA')",
  "        .css({'color': 'rgb(151,151,151)', 'font-style': 'italic'});",
  "    }",
  "  }",
  "}"  
)


## Information text for information button
information <- "Only selected information is presented in the active list of identified microbial hazards, details are available in the downloaded file upon downloading of all results."
generalinformation <- "In this step, specific hazard identification knowledge rules (abbreviated as Hinorus) are applied. Please refer to the user manual for details information"
# Define UI for application that draws a histogram
ui <- dashboardPage(
  ## header
  dashboardHeader(title = "Microbial Hazards Identification Decision Support System (MIDI)",
                  titleWidth = 700),
  
  ## Sidebar
  dashboardSidebar(
    tags$hr(
      tags$style(HTML("hr {border-top: 1px solid #000000;}"))
    ),
    sidebarMenu(id = "sidebar", 
                ## define welcome page
                menuItem("Welcome page", tabName = "welcome", icon = icon("gulp")),
                
                ## set horizontal line
                hr(),
                
                ## define food selection tab
                menuItem("Step 1: Food selection", tabName = "Food_selec", icon = icon("gulp")),
                
                ## define food processing tab
                menuItem("Step 2: Processing variables", tabName = "Processing_variables", icon = icon("gulp")),
                
                ## define Recontamination tab
                menuItem("Step 3: Recontamination", tabName ="Recontamination", icon=icon("gulp")),
                
                menuItem("Step 4: Product characteristics", tabName ="Product_char", icon=icon("gulp")),
                
                
                menuItem("Step 5: Assocciation selection", tabName ="Product_ass", icon=icon("gulp")),
                
                ## set horizontal line
                hr(),
                
                ## define the download tab
                menuItem("Download", tabName="Download", icon=icon("gulp")),
                
                ## set a horizontal line
                hr(),
                
                menuItem("User manual", icon = icon("audible"),
                         href = "https://docs.google.com/document/d/1T1gOCQ0vZeU3C6PaE2yJJIu1hv-8k152qjBhyeThzRY/edit?usp=sharing"),
                
                
                menuItem("Github page", icon = icon("github"),
                         href = "https://github.com/alexdank")
                
    )),
  
  
  ## Body
  dashboardBody(
    
    shinyDashboardThemes(
      theme = "poor_mans_flatly"
    ),
    tabItems(
      
      ## Welcome tab
      tabItem(tabName = "welcome",
              h2("Welcome!"),
              logo,
              fluidRow(column(width=6, DSS_scheme),
                       column(width=6,
              #boxPlus(width = 12,
              withMathJax(includeMarkdown("welcome_page.md")),
              actionButton(inputId="start", label="Start"))
              )
      ),
      
      ### Food selection tab
      
      tabItem(tabName = "Food_selec",
              h2("The active list of identified microbial hazards after step 1: Food products selection & Hazards pairing"),
              
              ## Set a row with the MIDI logo
              fluidRow(
                logo),
              
              ## Set a  row with the input, helptext and DSS scheme
              fluidRow(
                column(width=6, div(tags$img(src="https://github.com/alexdank/pics/blob/main/Microbial%20Hazards%20Identification%20DSS%20procedures%20step%201.jpg?raw=true", alt="Decision support system graphical overview", deleteFile=FALSE, width='600px',height='500px')),
                ),
                column(width=6, 
                       box(width=12, h2("Food Selection"),
                           
                           radioButtons("radio", label = "Do you want to use FoodEx2 selection or SAFFI food categories", 
                                              choices = list("FoodEx2" = 1, "SAFFI food categories" = 2),
                                              selected = 1),  
                           
                  
                    ### Code for when radiobutton == "SAFFI"
                    conditionalPanel(
                      condition = "input.radio == 1",       
                    helpText("Select the food product(s) for which you would like to identify microbial hazards. The hazards will be displayed in the Hazard overview table."),
                    
                    
                    pickerInput(
                      inputId = "food_selection",
                      label = "Select food item", 
                      choices = inputoptions,
                      multiple = FALSE)),
                    
                    conditionalPanel(
                      condition = "input.radio == 2",       
                      helpText("Select the food categories for which you would like to identify microbial hazards. The hazards will be displayed in the Hazard overview table."),
                      
                      
                      pickerInput(
                        inputId = "category_selection",
                        label = "Select food category", 
                        choices = inputoptionssaffi,
                        multiple = FALSE))
                ))),
              
              
              ### Set  row with the output
              fluidRow(
                actionButton(inputId="next1", label="NEXT"),
                useShinyjs(),
                actionButton("refresh", "Refresh session"),
                box(dropdownButton(status = 'success', icon = icon('info'), inline=TRUE,
                                   h2("Information"),
                                   h4(information), width = "300px"),
                    title = "Hazard count overview",
                     DT::DTOutput("tbl1"), width = 12)
              )
      ),
      
      
      ### Food Processing tab
      tabItem(tabName = "Processing_variables",
              h2("The active list of identified microbial hazards after step 2: Processing variables"),
              
              ## Set a row with the MIDI logo
              fluidRow(logo),
              ## Set a row with the input and DSS Scheme
              fluidRow(
                column(width=6, div(tags$img(src="https://github.com/alexdank/pics/blob/main/Microbial%20Hazards%20Identification%20DSS%20procedures%20step%202.jpg?raw=true", alt="Decision support system graphical overview", deleteFile=FALSE, width='600px',height='500px'))
                ),
                column(width=6,
                box(width=12,
                    hr(),
                  helpText(h3("Important"), "Select the correct processing technique based on the description in the processing conditions described in the table."),
                  hr(),
                  pickerInput(
                    inputId = "processingvar",
                    label = "Select processing variable",
                    choices = inputprocessing,
                    selected = "None"),
                  
                  prettySwitch(
                    inputId = "wetdry",
                    label = "Does it concern a dry food"),
                  
                  helpText("Use the table below as a reference for which processing condition to choose."),
                  
                  DT::DTOutput("PTE_DES", width=5)))
               ),
              
              ### SEt a row with the output
              fluidRow(
                actionButton(inputId="next2", label="NEXT"),
                useShinyjs(),
                actionButton("refresh2", "Refresh session"),
                box(dropdownButton(status = 'success', icon = icon('info'), inline=TRUE,
                                   h2("Information"),
                                   h4(information), width = "300px"),
                    title = "Hazards after processing", 
                     DT::DTOutput("tbl2"), width = 12)
              )
              
      ),
      
      ### Recontamination tab
      tabItem(tabName = "Recontamination",
              h2("The active list of identified microbial hazards after step 3: Recontamination"),
              
              ## Set a row with the MIDI logo
              fluidRow(logo),
              
              ## Set a row with input and DSS Scheme
              
              fluidRow(
                column(width=6, div(tags$img(src="https://github.com/alexdank/pics/blob/main/Microbial%20Hazards%20Identification%20DSS%20procedures%20step%203.jpg?raw=true", alt="Decision support system graphical overview", deleteFile=FALSE, width='600px',height='500px'))
                ),
                column(width=6,
                box(width=12,
                  helpText(strong("Identify possible hazards due to recontamination")),
                  prettySwitch(
                    inputId = "outside_bag",
                    label = "Was there any processing performed outside of a closed container?"),
                  helpText("If this button is not selected, it is assumed no container opening has been performed since initial processing
                  and potential hazards due to recontamination are excluded."),
                  
                  ## Define a conditional panel
                  conditionalPanel(
                    condition = "input.outside_bag == true",
    
                  prettySwitch(inputId = "include_drywet",
                               label = "Do you want to include hazards from dry and wet environments in table?"),
                  
                  helpText(strong("Please indicate whether the processing environment was dry or wet.")),
                  ## processingwet
                  tags$div(
                    materialSwitch(inputId = "env_wet", label = "Dry processing environment", inline = TRUE, status = "primary"),
                    tags$span("wet processing environment")
                  ),
                  #prettySwitch(
                  #inputId = "env_wet",
                  #label = "Was the processing environment wet?"),
                  
                  ## Was there any human contact?
                  prettySwitch(
                    inputId = "human_contact",
                    label = "was there any human contact to foods or human contact while adding ingredients"),
                  
                  prettySwitch(
                    inputId = "dry_spices",
                    label = "Were any unprocessed dry spices added?"),
                  
                  prettySwitch(
                    inputId = "dry_vitamins",
                    label = "Were any unprocessed dry vitamins and/or other dry ingredients added?”")
                )))
              ),
              
              ### Set a row with the output
              fluidRow(
                actionButton(inputId="next3", label="NEXT"),
                useShinyjs(),
                actionButton("refresh3", "Refresh session"),
                box(dropdownButton(status = 'success', icon = icon('info'), inline=TRUE,
                                   h2("Information"),
                                   h4(information), width = "300px"),
                    title = "Hazards after recontamination",
                     helpText("This table displays the hazards after potential recontamination due to additions after processing.
                              Hazards added to this list due to potential recontamination are displayed in red. Hazards originating from
                             due to initial food product characteristics are displayed in green.
                              It is possible some hazards occur multiple times during recontamination. (Example).
                              This is information is displayed by the amount of times Recontamination is displayed in the table"),
                     DT::DTOutput("tblrecon"), width = 12)
              )
              
              
              
      ),
      
      ### Set product characteristics tab
      tabItem(tabName="Product_char",
              h2("The active list of identified microbial hazards after step 4: Product characteristic"),
              
              ## set the logo
              fluidRow(logo),
              
              ## set the input and logo tab
              fluidRow(
                column(width=6, div(tags$img(src="https://github.com/alexdank/pics/blob/main/Microbial%20Hazards%20Identification%20DSS%20procedures%20step%204.jpg?raw=true", alt="Decision support system graphical overview", deleteFile=FALSE, width='600px',height='500px'))
                ),
                useShinyjs(), 
                column(width=6,
                box(width = 12,
                  tags$div(
                  barcol,
                  sliderTextInput(
                    inputId = "pH",
                    label = "Indicate the pH of your product:", 
                    choices = c(seq(from = 1, to = 14, by = 0.1)),
                    grid = TRUE,
                    selected = 7
                  )),
                  tags$div(
                  barcol1,
                  sliderTextInput(
                    inputId = "aw",
                    label = "Indicate the Aw of your product:", 
                    choices = c(seq(from = 0, to = 1, by = 0.1)),
                    grid = TRUE,
                    selected = 1
                  )),
                  materialSwitch(
                    inputId = "tempabuse",
                    label = "Was there any temperature abuse (uncontrolled temperature) during the production and transportation chain?",
                    value = FALSE, 
                    status = "danger"
                  ),
                  helpText("When there was any uncontrolled temperature during the chain it is assumed temperatures have potentially risen to room temperature."),
                  pickerInput(
                    inputId = "chaintemp",
                    label = "What is the used temperature during the production and transportation chain", 
                    choices = c("Room temperature", "1-4 degrees Celcius", "Below 0 degrees Celcius")
                  )
                ))
              ),
              
              ## set the output row
              fluidRow(
                actionButton(inputId="next4", label="NEXT"),
                useShinyjs(),
                actionButton("refresh4", "Refresh session"),
                box(dropdownButton(status = 'success', icon = icon('info'), inline=TRUE,
                                   h2("Information"),
                                   h4(information), width = "300px"),
                    title = "Hazards after product characteristics",
                     DT::DTOutput("tbl_proces"), width = 12), width=12)
              
      ),
      tabItem(tabName = "Product_ass",
              h2("The active list of identified microbial hazards step 5: Microbial hazard association level selection"),
              
              fluidRow(logo),
              
              fluidRow(
                column(width=6,div(tags$img(src="https://github.com/alexdank/pics/blob/main/Microbial%20Hazards%20Identification%20DSS%20procedures%20step%205.jpg?raw=true", alt="Decision support system graphical overview", deleteFile=FALSE, width='600px',height='500px')
              )),
              column(width = 6,
                     box(width = 12,
                materialSwitch(
                  inputId = "filtcount",
                  label = "Filter MHs with low assocation?",
                  value = FALSE, 
                  status = "danger"
              )))),
              
              #set the output row
              fluidRow(
                actionButton(inputId="next5", label="NEXT"),
                useShinyjs(),
                actionButton("refresh5", "Refresh session"),
                box(dropdownButton(status = 'success', icon = icon('info'), inline=TRUE,
                                   h2("Information"),
                                   h4(information), width = "300px"),
                    title = "Selected relevant hazards", 
                    DT::DTOutput("tbl_filt"), width = 12))
              
              ),
      
      tabItem(tabName = "Download",
              h2("Download results"),
              
              
              ## set the logo
              fluidRow(logo),
              
              ## set the downloadbutton
              fluidRow(
                box(
                  helpText("Thank you for using the Microbial Hazards Identification Decision Support System (MIDI)!
                           Click the downloadbutton below to download the results of all tabs.
                           The results of each tab are separated in unique excel sheets.
                           (ref to publication?)"),
                  downloadButton("dl", "Download"),
                  useShinyjs(),
                  actionButton("refresh6", "Refresh session")))
              
      )
      
      
    ))
)








# Define server logic required to draw a histogram

server <- function(input, output, session) {
  
  ### display a table for selecting and showing the relevant food categories and risks to the user
  tbl_1 <- reactive({
    
    if(input$radio == 1) {
    ## make tbl for showing hazards
    selectedfood <- input$food_selection
    
    selected_category <- food_products %>% filter(str_to_sentence(Food_item) %in% selectedfood)
    food_sub_categoryvec <- selected_category %>% pull(Food_subcategory_1)
    
    
    ## extract relevant hazards from categorie list
    Hazard_counts <- food_categories %>% filter(Food_subcategory_1 %in% food_sub_categoryvec)
    
    ## print table of releant counts and sort based on highest hazard
    Hazard_counts <- Hazard_counts %>% select(1:14) %>% filter(Count > 0) %>% arrange(desc(Count), Food_subcategory_1) %>% select(-Food_subcategory_1)
    print(Hazard_counts)
    
    ## save as table output 1
    tbl_1 <- Hazard_counts
    } else {
      food_sub_categoryvec <- input$category_selection
      
      
      ## extract relevant hazards from categorie list
      Hazard_counts <- food_categories %>% filter(str_to_sentence(Food_subcategory_1) %in% food_sub_categoryvec)
      
      ## print table of releant counts and sort based on highest hazard
      Hazard_counts <- Hazard_counts %>% select(1:14) %>% filter(Count > 0) %>% arrange(desc(Count), Food_subcategory_1) %>% select(-Food_subcategory_1)
      print(Hazard_counts)
      
      ## save as table output 1
      tbl_1 <- Hazard_counts
    }
  })
  
  output$tbl1 <- DT::renderDT({
    to_display <- tbl_1()
    to_display <- to_display %>% select(Food_main_category, Type, Genus, Species, Count)
    datatable(to_display %>% arrange(desc(Count)) %>% mutate(Count = case_when(
      Count == 0 ~ "Low Risk (0)",
      Count == 1 ~ "Low Risk (1)",
      Count == 2 ~ "Low Risk (2)",
      Count == 3 ~ "Medium Risk (3)",
      Count == 4 ~ "High Risk (4)",
      Count == 5 ~ "High Risk (5)"),
      Food_main_category = str_to_sentence(Food_main_category)) %>%
        relocate(Count, .after = Species) %>%
        dplyr::rename("Risk Association" = Count,
                      "Food main category" = Food_main_category),
      options = list(language = list(
        zeroRecords = "There are no identified microbial hazards under the selected scenarios"),
        rowCallback = JS(rowCallback))
       )
                      #"Food subcategory" = Food_sub_to_print))
  })
  
  
  ### Display a table with selection of thermal processing and wet or dry food
  tbl_2 <- reactive({
    tbl_1 <- tbl_1()
    ##extract data which bacteria to consider from tbl_1
    selechazard <- left_join(tbl_1, inactivationsheet, by =c("Type", "Genus", "Species"))
    
    ##Filter based on which thermal processing user entered
    processhazard <- selechazard %>% filter(Processing_condition %in% input$processingvar)
    
    ## select to show either wet or dry column based on user input
    
    if(input$wetdry == TRUE) {
      processhazdry <- processhazard %>% filter(Hazard_inactivation5D_dryfood == "no")
      tbl_hazarddry <- processhazdry %>% select(Food_main_category, Type, Genus, Species, Hazard_inactivation5D_dryfood)
    } else{
      processhazdry <- processhazard %>% filter(Hazard_inactivation5D == "no")
      tbl_hazarddry <- processhazdry %>% select(Food_main_category, Type, Genus, Species, Hazard_inactivation5D)
    }
    
    ## save as table output 1
    tbl_2 <- tbl_hazarddry
    
  })
  
  output$tbl2 <- DT::renderDT({
    to_display2 <- tbl_2()
    to_display2 <- as.data.frame(to_display2 %>% mutate(Food_main_category = str_to_sentence(Food_main_category)) %>%
                                   dplyr::rename("Food main category" = Food_main_category))
    if(input$wetdry == TRUE) {
      to_display2 <- datatable(to_display2 %>% dplyr::rename("5 log inactivation?" = Hazard_inactivation5D_dryfood),
                               options = list(
                                 language = list(
                                   zeroRecords = "There are no identified microbial hazards under the selected scenarios")))
    } else {
      to_display2 <- datatable(to_display2 %>% dplyr::rename("5 log inactivation?" = Hazard_inactivation5D),
      options = list(
        rowCallback = JS(rowCallback),
        language = list(
          zeroRecords = "There are no identified microbial hazards under the selected scenarios")))
    }            
                                              
  })
  
  ### Display a table with selection of recontamination possibilies
  tbl_recon <- reactive({
    tbl_2 <- tbl_2()
    
    ### Only allow output in recontamination if indeed processing outside of a closed container!
    
    if(input$outside_bag == TRUE) {
      if(input$include_drywet == TRUE) {
        if(input$env_wet == TRUE) {
          recontamination_to_add <- recontamination %>% filter(Wet_processing_environments == 'Yes') %>% select(Type, Genus, Species, Hazard_origin)
          tbl_2$Hazard_origin = "Original"
          tbl_3 <- add_row(tbl_2, recontamination_to_add)
        }  else {
          recontamination_to_add <- recontamination %>% filter(Dry_processing_environments == 'Yes') %>% select(Type, Genus, Species, Hazard_origin)
          tbl_2$Hazard_origin = "Original"
          tbl_3 <- add_row(tbl_2, recontamination_to_add)
        }
      } else {
        tbl_2$Hazard_origin = "Original"
        tbl_3 <- tbl_2
      }
      
      if(input$human_contact == TRUE) {
        recontamination_to_add <- recontamination %>% filter(Human_contact == 'Yes') %>% select(Type, Genus, Species, Hazard_origin)
        tbl_3 <- add_row(tbl_3, recontamination_to_add)
      }  else {
        tbl_3 <- tbl_3
      }
      
      if(input$dry_spices == TRUE) {
        recontamination_to_add <- recontamination %>% filter(Dry_ingredients_spices == 'Yes') %>% select(Type, Genus, Species, Hazard_origin)
        tbl_3 <- add_row(tbl_3, recontamination_to_add)
      }  else {
        tbl_3 <- tbl_3
      }
      
      if(input$dry_vitamins == TRUE) {
        recontamination_to_add <- recontamination %>% filter(Dry_ingredients_vitamins == 'Yes') %>% select(Type, Genus, Species, Hazard_origin)
        tbl_3 <- add_row(tbl_3, recontamination_to_add)
      }  else {
        tbl_3 <- tbl_3
      }
      
    } else {
      tbl_2$Hazard_origin = "Original"
      tbl_3 <- tbl_2
    }
    tbl_3 <- tbl_3 %>% group_by(Type,Genus,Species) %>%
      mutate(Hazard_origin2 = toString(Hazard_origin)) %>% distinct_at(vars(c(Type, Genus, Species, Hazard_origin2, Food_main_category))) %>%
      as.data.frame()
    #tbl_3 <- tbl_3 %>% distinct()
    
  })
  
  ## output code for tabpanel
  output$tblrecon <- DT::renderDT({
    to_display3 <- tbl_recon()
    to_display3 <- to_display3 %>% mutate(Food_main_category = str_to_sentence(Food_main_category)) %>% 
      dplyr::rename("Hazard origin" = Hazard_origin2,
                                                 "Food main category"= Food_main_category)
    datatable(to_display3,
              options = list(
                rowCallback = JS(rowCallback),
                language = list(
                  zeroRecords = "There are no identified microbial hazards under the selected scenarios"),
                columnDefs = list(
                  list(targets = "_all", render = JS(render))))
    )
    
  })
  
  ### Adapt the table based on product characteristics
  tbl_proces <- reactive({
    tbl_4 <- tbl_recon()
    
    ## Make a table with all columns
    tbl_4 <- left_join(tbl_4, PPC)
    
    ## Extract all rows with "no growth for pH"
    #No_growth_tbl_4 <- tbl_4 %>% filter(Growth_needed == "no")
    exbac <- tbl_4 %>% filter(Type != "Bacteria")
    
    
    ## Filter based on pH
    if(input$pH < 4.5) {
      tbl_4 <- tbl_4 %>% filter(Growth_below4.5 == "yes")
      tbl_4 <- tbl_4 %>% full_join(exbac)
    }  else {
      if(input$pH > 10){
        tbl_4 <- tbl_4 %>% filter(Growth_above10 == "yes")
        tbl_4 <- tbl_4 %>% full_join(exbac)
      }
      else {
        tbl_4 <- tbl_4
      }
    }
    
    ## filter based on Aw
    if(input$aw < 0.5) {
      tbl_4 <- tbl_4 %>% filter(Survival_Aw0205 == "yes")
      tbl_4 <- tbl_4 %>% full_join(exbac)
    }  else {
      if(input$aw >= 0.5 & input$aw < 0.9){
        ## Assume pathogens needing growth and aw above 0.9 to die between 0.5 and 0.9
       staph <- tbl_4 %>% filter(Genus == "Staphylococcus")
         tbl_4 <- tbl_4 %>% filter(Survival_Aw0509 == "yes")
        tbl_4 <- full_join(staph, tbl_4)
        tbl_4 <- tbl_4 %>% full_join(exbac)
      }
      else {
        tbl_4 <- tbl_4
      }
    }
    
    
    ## temperature during chain
    if(input$tempabuse == FALSE) {
      if(input$chaintemp == "Room temperature") {
        No_growth_tbl_4 <- tbl_4 %>% filter(Growth_needed == "no")
        tbl_4 <- tbl_4 %>% filter(grow_RT == "yes")
        tbl_4 <- full_join(No_growth_tbl_4, tbl_4)
      } else {
        if(input$chaintemp == "1-4 degrees Celcius") {
          tbl_4 <- tbl_4 %>% filter(survive_or_grow_1_4_degrees == "yes")
       # 
        } else {
          if(input$chaintemp == "Below 0 degrees Celcius") {
            tbl_4 <- tbl_4 %>% filter(survive_or_grow_below_0_degrees == "yes")
            
          }
        }
     }
      
    } else {
      No_growth_tbl_4 <- tbl_4 %>% filter(Growth_needed == "no")
      tbl_4 <- tbl_4 %>% filter(grow_RT == "yes")
      tbl_4 <- full_join(No_growth_tbl_4, tbl_4)
    }
    
    
    Hazard_count <- tbl_1() %>% select(c("Type", "Genus", "Species", "Count"))
    tbl_4 <- left_join(tbl_4, Hazard_count, by = c("Type", "Genus", "Species"))

  }) 

  ### Define a warning message for the part in which parasites or virusses are identified

  
  ## output code for tabpanel
  output$tbl_proces <- DT::renderDT({
    to_display4 <- tbl_proces()
    to_display4 <- to_display4 %>% arrange(desc(Count)) %>% mutate(Count = case_when(
      Count == 0 ~ "Low Risk (0)",
      Count == 1 ~ "Low Risk (1)",
      Count == 2 ~ "Low Risk (2)",
      Count == 3 ~ "Medium Risk (3)",
      Count == 4 ~ "High Risk (4)",
      Count == 5 ~ "High Risk (5)"),
      Food_main_category = str_to_sentence(Food_main_category))%>%
      relocate(Count, .after = Species)  %>%
      dplyr::rename("Risk Association" = Count,
                    "Hazard origin" = Hazard_origin2,
                    "Growth needed" = Growth_needed,
                    "Survival Aw 0.2 - 0.5" = Survival_Aw0205,
                    "Food main category"= Food_main_category,
                    "Growth below pH 4.5" = Growth_below4.5,
                    "Growth above pH 10" = Growth_above10)
    
    
    
    to_display4 <- to_display4 %>% 
      dplyr::select(c(`Food main category`,"Type", "Genus", "Species", `Risk Association`, `Hazard origin`, `Growth needed`))
    
    
    
    
    DT::datatable(to_display4,
              options = list(
                rowCallback = JS(rowCallback),
                language = list(
                  zeroRecords =  "There are no identified microbial hazards under the selected scenarios"),
                scrollX = TRUE,
                columnDefs = list(
                  list(targets = "_all", render = JS(render))))) %>% formatStyle(
                    columns = "Growth needed",
                    backgroundColor = styleEqual(c("no","yes"), c('tomato', 'khaki'))
                  )
  })
  
  
  
  ## Define code for tab 5
  tbl_filt <- reactive({
    tbl_5 <- tbl_proces()
    
  if(input$filtcount == TRUE) {

    tbl_5 <- tbl_5 %>% filter(Count >2)
    
  } else {
    tbl_5 <- tbl_5
  }
  
  })
  
  
  ## output code for tabpanel
  output$tbl_filt <- DT::renderDT({
    to_display5 <- tbl_filt()
    to_display5 %>% arrange(desc(Count)) %>% mutate(Count = case_when(
      Count == 0 ~ "Low Risk (0)",
      Count == 1 ~ "Low Risk (1)",
      Count == 2 ~ "Low Risk (2)",
      Count == 3 ~ "Medium Risk (3)",
      Count == 4 ~ "High Risk (4)",
      Count == 5 ~ "High Risk (5)")) %>%
      relocate(Count, .after = Species)  %>%
      dplyr::mutate(Food_main_category = str_to_sentence(Food_main_category)) %>%
      dplyr::rename("Risk Association" = Count,
                    "Hazard origin" = Hazard_origin2,
                    "Growth needed" = Growth_needed,
                    "Survival Aw 0.2 - 0.5" = Survival_Aw0205,
                    "Food main category"= Food_main_category)
    
    
    to_display5 <- to_display5 %>% 
      dplyr::select(c(`Food main category`,"Type", "Genus", "Species", `Risk Association`, `Hazard origin`, `Growth needed`))
    
    
    datatable(to_display5,
      options = list(
        rowCallback = JS(rowCallback),
        language = list(
          zeroRecords =  "There are no identified microbial hazards under the selected scenarios"),
        scrollX = TRUE,
        columnDefs = list(
          list(targets = "_all", render = JS(render))))) %>% formatStyle(
            columns = "Growth needed",
            backgroundColor = styleEqual(c("no","yes"), c('tomato', 'khaki'))
          )
  })
  
  
  
  ##define logic for next page buttons
  observeEvent(input$start, {
    updateTabItems(session, "sidebar", "Food_selec") 
  })
  
    observeEvent(input$next1, {
      updateTabItems(session, "sidebar", "Processing_variables") 
    })
    
    observeEvent(input$next2, {
      updateTabItems(session, "sidebar", "Recontamination") 
    })
    
    observeEvent(input$next3, {
      updateTabItems(session, "sidebar", "Product_char") 
    })
    
    observeEvent(input$next4, {
      updateTabItems(session, "sidebar", "Product_ass") 
    })
    
    observeEvent(input$next5, {
      updateTabItems(session, "sidebar", "Download") 
    })
  
  
  
  library(data.table)
  
  ## download all results
  data_list <- reactive({
    list(
      tab_1 = tbl_1(),
      tab_2 = tbl_2(),
      tab_3 = tbl_recon(),
      tab_4 = tbl_proces(),
      tab_5 = tbl_filt()
    )
  })
  
  output$dl <- downloadHandler(
    filename = function() {paste0(sysdate, " ", "all_results.xlsx")},
    content = function(file) {write_xlsx(data_list(), path = file)}
  )
  
  ### Enable a Processing table display on processing technique tab
  
  
  output$PTE_DES <- DT::renderDT({
    PTE_DES <- PTE_DES %>% dplyr::rename("Processing technique" = Processing_technique,
                                         "Processing condition" = Processing_condition)
    
    to_displaydes <- PTE_DES
    as.data.frame(to_displaydes)
    
  },
  options = list(
    autoWidth = FALSE,
    searching = FALSE,
    paging = FALSE
  ))
  
  
  ### Define the refresh button
  observeEvent(input$refresh, {
    refresh()
  })
  observeEvent(input$refresh2, {
    refresh()
  })
  observeEvent(input$refresh3, {
    refresh()
  })
  observeEvent(input$refresh4, {
    refresh()
  })
  observeEvent(input$refresh5, {
    refresh()
  })
  observeEvent(input$refresh6, {
    refresh()
  })

  ## Define a warning message when opening tabpanel 4
  observeEvent(input$sidebar, {
    if (input$sidebar == "Product_char")  {
      tabtemp <- tbl_proces()
      
      if("Viruses" %in% tabtemp$Type){
      showModal(modalDialog(
        title = h2("Important message"),
        h3("The survival of viruses and parasites in the selected scenario is unknown, but please note that no growth in foods 
        is needed for parasites or viruses to cause illness in humans."),
        easyClose = TRUE
      )) } else if ("Parasites" %in% tabtemp$Type) {
          showModal(modalDialog(
            title = h2("Important message"),
            h3("The survival of viruses and parasites in the selected scenario is unknown, but please note that no growth in foods 
        is needed for parasites or viruses to cause illness in humans."),
            easyClose = TRUE
          ))
      } else {}
    }else{}
    
    })
  
}



# Run the application 
shinyApp(ui = ui, server = server)
