#install.packages('leaflet.extras')
#install.packages("shinythemes")
#install.packages("shinyWidgets")
#install.packages("htmltools")
#install.packages("DT")


library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(htmltools)
shinyUI(
  
  
  
  
  navbarPage("Manhattan Off-Campus Housing",
             #theme=shinythemes::shinytheme("spacelab"),
             fluid=T,
           
             #####################################1. Home##############################################           
             tabPanel("Home",icon=icon("home"),
    
                      div(class="home",
                          
    
                          tags$head(
                            # Include our custom CSS
                            includeCSS("www/styles.css"),
                            includeScript("www/click_hover.js")
                          
                          ),
                          
                      align="center",
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      
                      #h5("Find housing around campus",),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      h1("Find Your Off-Campus Housing in Manhattan",style="color:white;font-family: Times New Roman;font-size: 300%;font-weight: bold;"),
                      br(),
                      br(),
                      br(),
                      h3("Gourp6- Fall 2017",style="color:white;font-family: Times New Roman;font-size: 200%;font-weight: bold;"),
                      br()
                  
                      #h4("")
                      
                      # tags$head(
                      #   # Include our custom CSS
                      #   includeCSS("styles.css")
                      #   
                      # )
                      )
             ),
             
  
             #######################################2. Maps#############################################           
             # tabPanel("Maps",
             #          
             #      sidebarLayout(
             #      sidebarPanel(
             #        top=60,
             #        
             #        textInput(inputId="place",label="", placeholder = "search your place..."),
             #        #actionButton("gobutton1","Enter"),
             #        verbatimTextOutput("test"),
             #        h4("Filters"),
             #                   ######Housing filter######
             #        # selectInput("university",
             #        #             label = "University",
             #        #             choices = c("Columbia",
             #        #                         "NYU",
             #        #                         "Fordham")
             #        #             
             #        # ),
             #        
             #        
             #        selectInput("manual_br",
             #                    label = "Number of bedrooms",
             #                    choices = c("Studio" = 0,
             #                                "1b" = 1,
             #                                "2b" = 2,
             #                                "3b" = 3,
             #                                "4b" = 4)
             #        ),
             #        
             #        sliderInput("manual_rent",
             #                    label = "Rent range",
             #                    min = 850,
             #                    max = 7900,
             #                    value = c(1200, 2000),
             #                    step = 100,
             #                    round = TRUE
             #          ),
             #      
             #                     ########Feature checkbox#######
             #        checkboxInput("Crime", label = "Crime",value= FALSE),
             #        checkboxInput("Bus", label = "Bus",value= FALSE),
             #        checkboxInput("Subway",label="Subway",value = FALSE),
             #        checkboxInput("Market", label = "Market",value = FALSE),
             #        checkboxInput("Restaurant", label = "Restaurant",value= FALSE)
             # 
             #                  
             #        ),#side bar panel
             #  
             #        ############main map#########
             #        mainPanel(
             #        leafletOutput("map", width = "100%", height = 650)
             #                 )
             #        
             #        )#sidebar layout
             #     ),#tabpanel
             ##################################2.2map###########################################
   
             tabPanel("Housing Explorer", icon = icon("map"),
                    
                      fluidRow(
                       
                        tags$div(id="searchBar",
                                 column(width=1,
                                        style = "width:270px;display:inline-block;margin-right: 0px;margin-bottom:0px;margin-top:0px;padding-right:0px",
                                        textInput(inputId="location",label="", value="", placeholder = "search your location...")
                                 ),
                                 column(width=1,
                                        style = "margin-top: 25px;display:inline-block;margin-right: 0px;margin-left: 0px;left:0px;bottom:5px;padding-left:0px",
                                        actionButton("button1",label="", icon = icon("search"))
                                        #,style="padding:12px; font-size:100%;color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                 )),
                        # column(width=1,style = "margin-top: 25px;display:inline-block;margin-right: 0px;",
                        #        actionButton("button2",label="Clear search")
                        #                     #, style="padding:10px; font-size:80%;color: #fff; background-color: #f29898")
                        # ),
                        column(width=1,
                               style = "margin-top: 25px;display:inline-block;margin-right: 0px;margin-left: 120px",
                               dropdownButton(circle = FALSE,
                                              label="Min price",  status = "default",
                                              numericInput(inputId="min_price", label = "choose",value=0, min=0,max=1000000,step=1000)
                               )
                               #selectInput(inputId="min_price",label="", choices = seq(0,10000,50) )
                               ),
                        column(width=1,
                               style = "margin-top: 25px;display:inline-block;margin-right: 0px;",
                               dropdownButton(circle = FALSE,
                                              label="Max price",  status = "default", 
                                              numericInput(inputId="max_price", value=1000000, label="choose",min=0,max=1000000,step=1000 )
                               )),
                        column(width=1, 
                               style="margin-top: 25px;display:inline-block;margin-right: 10px",
                               dropdownButton(circle = FALSE,
                                              label = "Bedrooms", status = "default",
                                              selectInput(inputId="min_bedrooms", label="choose", choices = c("studio"=0,"1b"=1,"2b"=2,"3b"=3,"4b"=4,"5b"=5,"6b"=6)

                                              ))
                               # selectInput(inputId="min_bedrooms", label="",choices = c("min bedroom"=0,"studio"=1,"1b"=2,"2b"=3,"3b"=4,"4b"=5,"5b"=6,"6b"=7))
                                              ),
                        
                        column(width=1,
                               style = "margin-top: 25px;;display:inline-block;margin-right: 10px;",
                               dropdownButton(circle = FALSE,
                                 label = "Bathroom", status = "default",
                                 selectInput(inputId="min_bathrooms", label="choose", choices = c("studio"=0,"1b"=1,"2b"=2,"3b"=3,"4b"=4,"5b"=5,"6b"=6)
                                 
                                 )
                               )),
                        column(width=1, 
                               style = "margin-top: 25px;display:inline-block;margin-right: 0px;",
                               actionButton("button2",label="Reset" 
                                            #,style="padding:12px; font-size:80%;color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                            ))
                        # ,
                        # column(width=1,style = "margin-top: 25px;;display:inline-block;margin-right: 0px;",
                        #        dropdownButton(circle = FALSE,
                        #                       label = "Filters", status = "primary",
                        #                       checkboxGroupInput("filters", label = "choose", choices = c("Crime","Bus","Subway","Market","Restaurant"))
                        #                       )
                        #        )
                           ),
                     
                      # # checkboxInput("Crime", label = "Crime",value= FALSE),
                      # # checkboxInput("Bus", label = "Bus",value= FALSE),
                      # # checkboxInput("Subway",label="Subway",value = FALSE),
                      # # checkboxInput("Market", label = "Market",value = FALSE),
                      # # checkboxInput("Restaurant", label = "Restaurant",value= FALSE),
                      # checkboxInput("filters",label = "filters",value = FALSE,width=NULL),
                      mainPanel(
                        
                        fluidRow(
                          
                          column(7, 
                                 br(),
                                 br(),
                                 # h3("current rank"),
                                 dataTableOutput("rank")
                                 
                          ),
                          
                          column(5,
                                 leafletOutput("map", width = "220%", height = 650),
                                 
                                 absolutePanel(id="legend",
                                               fixed = TRUE,
                                               draggable = TRUE, top = 140, left = "auto", right = 80, bottom = "auto",
                                               width = 125, height = 215,
                                               
                                               h5("Select Features"),
                                               checkboxInput("Crime", label = "Crime",value= FALSE),
                                               checkboxInput("Bus", label = "Bus",value= FALSE),
                                               checkboxInput("Subway",label="Subway",value = FALSE),
                                               checkboxInput("Market", label = "Market",value = FALSE),
                                               checkboxInput("Restaurant", label = "Restaurant",value= FALSE)                                 
                                               
                                 )#abs panel
                                 
                          )#column
                        )#row
                      )#main panel
             ),#tab panel
               
             
             
               #######################################3. Statistics######################################## 
             tabPanel("Recommendation",icon=icon("thumbs-up"),
                     
                ###############Table of Rank############### 
                      fluidRow(
                        br(),
                        br(),
                        
                        column(2,
                               
                               br(),
                               br(),
                               br(),
                               selectInput("university",
                                           label = "University",
                                           choices = c("columbia",
                                                       "nyu",
                                                       "fordham")),
                               
                               br(),
                        
                               h4("Set Your Own"),
                               selectInput("First",
                                           label = "First Preference",
                                           choices = c("Rent",
                                                       "Safety",
                                                       "Distance",
                                                       "Market",
                                                       "Restaurant"),
                                           selected="Rent"),
                               
                               selectInput("Second",
                                           label = "Second Preference",
                                           choices = c("Rent",
                                                       "Safety",
                                                       "Distance",
                                                       "Market",
                                                       "Restaurant"),
                                           selected="Safety"),
                               
                               selectInput("Third",
                                           label = "Third Preference",
                                           choices = c("Rent",
                                                       "Safety",
                                                       "Distance",
                                                       "Market",
                                                       "Restaurant"),
                                           selected="Distance")
                        ),
                        
                        column(10,
                               #plotOutput('plot')
                               DT::dataTableOutput('ranktable')
                                  )
                        ),#fluidrow 1
                      
                      br(),
                      br(),
                      br(),
                      br(),
                
                ###############Rent Change################## 
                      fluidRow(
                        
                        column(2,offset=0.8,
                               checkboxGroupInput("regionname", label = "Rent Change",
                                                  choices=c(
                                           "Battery Park",
                                           "Chelsea",
                                           "Clinton",
                                           "East Harlem",
                                           "East Village",
                                           "Financial District",
                                           "Flatiron District",
                                           "Garment District",
                                           "Gramercy",
                                           "Greenwich Village",
                                           "Harlem",
                                           "Little Italy",
                                           "Lower East Side",
                                           "Midtown",
                                           "Morningside Heights",
                                           "Murray Hill",
                                           "NoHo",
                                           "Tudor City",
                                           "Turtle Bay",
                                           "Upper East Side",
                                           "Upper West Side",
                                           "Washington Heights",
                                           "West Village"),
                                           selected=c(
                                             "Upper West Side",
                                             "Washington Heights"))
                                
                          
                      ),
                      
                      column(9,
                            
                             plotOutput('rentTrendgg',height =   "600px",width = "110%")
                                         
                             )
                        
                      )#fluidrow2
   

                      )#tab panel
)#navbar page
)#ui
