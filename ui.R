library(shiny)




ui <- dashboardPage(
  dashboardHeader(title="ClustToolApp",
                  tags$li(actionLink("openModal", label = "Info", icon = icon("info")),
                          class = "dropdown")
  ),
  dashboardSidebar(
    #useShinyjs(), 
    sidebarMenu(id="sidebarmenu",
                menuItem("Import Data Set", tabName = "importdata",
                         icon = icon("folder open")),
                menuItem("View Data Set",tabName = "dataset"),
                menuItem("Data Preparation",
                         tabName = "Variablentrans",icon =icon("square-root-alt") ),
                menuItem("Results",tabName = "dataset2"),
                menuItem("Visualisation Of Results", tabName = "dashboard", 
                         icon = icon("dashboard")),
                menuItem("Calculate Best Number Of Clusters",tabName = "calc")),
                tags$hr(
                style="height: 6px; color: #fff; background-color: #367fa9;
                                                 border-color: #3c8dbc"
                          ),
                titlePanel("Variable Selection"),
                radioButtons("check_composition",label = "Compositional Data/Analysis?",
                             choices = list("No",
                                            "Yes"),
                             selected = "No"),
                
                
                selectizeInput("selectize",label="Variables Of Non-Compositional Data:",
                               choices= if(exists("daten$dat")==TRUE){names(daten$dat)}
                               else{list()},
                               multiple=TRUE,options = list(create = TRUE))
                ,  
                conditionalPanel(condition = "input.check_composition ==
                                 'Yes'",
                                 selectizeInput("selectize_comp",
                                                label="Variables Of Compositional Data 
                                                (Select Multiple):",
                                                choices= 
                                                  if(exists("daten$dat")==TRUE){
                                                    names(daten$dat)}
                                                else{list()},
                                                multiple=TRUE,options=list(create=TRUE))
                ),
                tags$hr(),
                selectizeInput("id",label = "Probe ID: (Optional)",
                               choices =if(exists("dat")==TRUE){names(dat)}else{list()},
                               multiple = TRUE,options=(list(maxItems=1))),
                selectizeInput("lon",label = "Longitude",
                               choices =if(exists("dat")==TRUE){names(dat)}else{list()},
                               multiple = TRUE,options=(list(maxItems=1))),
                selectizeInput("lat",label = "Latitude",
                               choices =if(exists("dat")==TRUE){names(dat)}else{list()},
                               multiple = TRUE,options=(list(maxItems=1))), 
                tags$hr(),
                selectInput("methode", label = "Cluster Method", 
                            choices = list("k-means"="kmeans",
                                           "CLARA"="clara" ,
                                           "PAM"="pam",
                                           "Mclust"="Mclust",
                                           "cmeans"="cmeans",
                                           "Diana"="diana",
                                           "Agnes"="agnes",
                                           "hclust"="hclust"),
                            selected = "k-means"),
                selectInput("distance", label = "Distance Measure",
                            choices = {})
                ,
                conditionalPanel(condition = "input.methode == 'hclust' || 
                                 input.methode == 'agnes'  " ,
                                 selectInput("agglomeration_method",
                                             label = "Agglomeration Method", 
                                             choices = list())
                ),
                numericInput("num", label = "Number Of Clusters", value =3,min=2),
                tags$hr(),
                actionButton("lonlatok",label = "OK",icon("check"),
                style="color: #fff; background-color: #f50a31;
                                                 border-color: #f50a31")
  ),
  
  dashboardBody(
    useShinyjs(), 
    tabItems(
      tabItem(tabName = "importdata",
              fluidRow(box(width = 20,height = 6500,
                           selectInput("choosedatatyp",
                                       label="Import Data Set.",
                                       choices = c("From CSV","From Excel",
                                                   "Global Environment")),
                           
                           #fue excel und csv
                           conditionalPanel(condition =
                                              "input.choosedatatyp == 'From CSV' || 
                                            input.choosedatatyp == 'From Excel'" ,
                                            
                                            fileInput("file1",
                                                      "Upload File From Local Directory",
                                                      multiple = TRUE
                                            ),
                                            tags$hr(),
                                            checkboxInput("header", label="Include Header",
                                                          TRUE),
                                            radioButtons("sep", "Separator",
                                                         choices = c(Comma = ",",
                                                                     Semicolon = ";",
                                                                     Tab = "\t"),
                                                         selected = ","),
                                            # Input: Select quotes 
                                            radioButtons("quote", "Quote",
                                                         choices = c(None = "",
                                                                     "Double Quote"='"',
                                                                     "Single Quote"="'"),
                                                         selected = '"'),
                                            # Horizontal line 
                                            tags$hr(),
                                            # Input: Select number of rows to display 
                                            radioButtons("disp", "Display",
                                                         choices = c(Head = "head",
                                                                     All = "all"),
                                                         selected = "head")
                           ),
                           # global dataset
                           conditionalPanel(condition = 
                                              "input.choosedatatyp=='Global Environment'",
                                            textInput(inputId = "globalvar",
                                                      label="Type in the designation of
                                                      the data set identically from 
                                                      R's global environment 
                                                      to load.")             
                                            
                                            )  ,
                           #action button CSS Hexcolor code
                           disabled(actionButton("button", "Import", icon("file-import"), 
                                                 style="color: #fff; background-color: #0924ed;
                                                 border-color: #2e6da4")),
                           bsPopover(id = "button", title = "Import File", 
                                     content =  paste("No data detected.", 
                                                      "Browse local directory or load from R&#39s global environment.")),
                           uiOutput("trigger"),
                           titlePanel("Data Preview:"),
                           tableOutput("impdata")
                           ))),
      tabItem(tabName="Variablentrans",
              tabsetPanel(type="tabs",
                          tabPanel( "Transformation And Scaling"  , 
                                    fluidRow(
                                      box(titlePanel("Data Preparation"),
                                          htmlOutput(outputId = "trans"),
                                          htmlOutput(outputId = "trans2"),
                                          htmlOutput(outputId = "trans4"),
                                          tags$hr(),
                                          radioButtons("check_transformation",label = h3(""),
                                                       choices = list("Prepare Compositional Data",
                                                                      "Prepare Non-Compositional Data"),
                                                       selected = "Prepare Non-Compositional Data"),
                                          tags$hr(),
                                          selectizeInput(
                                            "selectvariableselecttrans",label="Select From Listed Variables",
                                            choices= {},multiple=TRUE,options = list(create = TRUE)),      
                                          selectInput("selecttrans",label="Transformation",choices= 
                                                        c("Select_Transformation","Logarithm","Root_Transformation",
                                                          "Logcentered", "BoxCox")), 
                                          selectInput("selectscaling",
                                                      label="Scaling",choices= c("Select_Scaling",
                                                                                 "Classical","Robust")),
                                          disabled(actionButton("buttontrans", "OK")),
                                          actionButton("buttondatareset","Reset"),
                                          bsButton("Question_2", label = "", icon = icon("question"),
                                                   style = "info", size = "extra-small"),
                                          bsPopover(id = "Question_2", title = "",
                                                    content =
                                                      paste("Selected variables can be transformed and scaled",
                                                            "independently. Already transformed variables are",
                                                            "excluded from the list for selection to prevent multiple",
                                                            "transformations. Toggling the Reset button",
                                                            "recovers the altered data to its initial state.",
                                                            "Compositional Data is subject to aDist that can",
                                                            "be applied if not transformed."),
                                                    trigger="click"
                                          ),
                                          tags$hr(),
                                          fluidRow(
                                            box(selectInput("select_validity_measures2",
                                                            label="Choose A Validity Measure:",
                                                            choices= if(exists("daten2$dat")==TRUE){
                                                              names(t(guete$guete_lokal))}else{list()}),
                                                withSpinner(plotOutput(outputId = "plotmeans2")),
                                                title = "Plotmeans",status = "primary",
                                                solidHeader = TRUE,width=12,collapsible = TRUE,collapsed=FALSE
                                            ))
                                      ))),
                          tabPanel("aDist",
                                   fluidRow(
                                     box(titlePanel("Data Preparation"),
                                         conditionalPanel(condition = "input.check_composition =='Yes' ",
                                                          tags$hr(),
                                                          HTML(paste(h5("Aitchison distance (aDist) only applyable for 
                                                                        the following clustering algorithms:"),
                                                                     h6("-Diana, PAM, hclust, Agnes"),
                                                                     h5("Reasons for disabled adist checkbox could be:"),
                                                                     h6("-Transformation or scaling allready applied to compositional 
                                                                        data (reset values in tab: Transformation And Scaling)"),
                                                                     h6("-Prohibited clustering algorithm selected 
                                                                        (adjust in: sidebar)"),
                                                                     h6("-aDist allready applied (reset values in tab: aDist)"))),
                                                          tags$hr(),
                                                          checkboxInput(inputId = "check_box_adist",
                                                                        label="Apply Aitchison Distance Measurement
                                                                        To Compositional Data?",value = FALSE)
                                                          ),#conditon have compositional data
                                         tags$hr(),
                                         conditionalPanel(condition = 
                                                            "input.check_box_adist == true",
                                                          numericInput("num_comp", label = h3("Number Of Compositions"), 
                                                                       value =1,min=1,max = 4),
                                                          sliderInput("controller_count_factors", label="Adjust Weighted Pivot 
                                                                      Coordinates For Non-Compositional Variables (In 
                                                                      Combination With Compositional Data)",
                                                                      0, 100, 50),
                                                          conditionalPanel(condition=
                                                                             "input.num_comp=='1'",fluidRow(column(width=6,
                                                                                                                   selectizeInput("selectvariable1_num1",label=
                                                                                                                                    "Composition 1",choices={}, multiple=TRUE,
                                                                                                                                  options = list(create = TRUE))),
                                                                                                            column(width=6,
                                                                                                                   sliderInput("controller_1_num2", label = 
                                                                                                                                 "Adjust Weighted Pivot Coordinates Of Composition 1", 
                                                                                                                               0, 100, 50)))
                                                          )
                                                          ,#condition num1
                                                          conditionalPanel(condition="input.num_comp=='2'",
                                                                           fluidRow(
                                                                             column(width=6,
                                                                                    selectizeInput("selectvariable1_num2",
                                                                                                   label="Composition 1",choices={},
                                                                                                   multiple=TRUE,options = list(create = TRUE))),
                                                                             column(width=6,
                                                                                    sliderInput("controller_1_num2", label=
                                                                                                  "Adjust Weighted Pivot Coordinates 
                                                                                                Of Composition 1", 0, 100, 50))),
                                                                           fluidRow(
                                                                             column(width=6,
                                                                                    selectizeInput("selectvariable2_num2",
                                                                                                   label="Composition 2",choices={},multiple=TRUE,
                                                                                                   options = list(create = TRUE))),
                                                                             column(width=6,
                                                                                    sliderInput("controller_2_num2",label= 
                                                                                                  "Adjust Weighted Pivot Coordinates 
                                                                                                Of Composition 2", 0, 100, 50)))
                                                                                    ),#condition num2
                                                          conditionalPanel(condition="input.num_comp=='3'",
                                                                           fluidRow(
                                                                             column(width=6,
                                                                                    selectizeInput("selectvariable1_num3",
                                                                                                   label="Composition 1",choices={},
                                                                                                   multiple=TRUE,options = list(create = TRUE))),
                                                                             column(width=6,
                                                                                    sliderInput("controller_1_num2",label= 
                                                                                                  "Adjust Weighted Pivot Coordinates 
                                                                                                Of Composition 1", 0, 100, 50))),
                                                                           fluidRow(
                                                                             column(width=6,
                                                                                    selectizeInput("selectvariable2_num3",
                                                                                                   label="Composition 2",choices={},
                                                                                                   multiple=TRUE,options = list(create = TRUE))),
                                                                             column(width=6,
                                                                                    sliderInput("controller_2_num2", label=
                                                                                                  "Adjust Weighted Pivot Coordinates 
                                                                                                Of Composition 2", 0, 100, 50))),
                                                                           fluidRow(
                                                                             column(width=6,
                                                                                    selectizeInput("selectvariable3_num3",
                                                                                                   label="Composition 3",choices={},
                                                                                                   multiple=TRUE,options = list(create = TRUE))),
                                                                             column(width=6,
                                                                                    sliderInput("controller_3_num2", label=
                                                                                                  "Adjust Weighted Pivot Coordinates
                                                                                                Of Composition 3", 0, 100, 50)))
                                                                                    ),#condition num3
                                                          conditionalPanel(condition="input.num_comp=='4'",
                                                                           fluidRow(
                                                                             column(width=6,
                                                                                    selectizeInput("selectvariable1_num4",
                                                                                                   label="Composition 1",choices={},
                                                                                                   multiple=TRUE,options = list(create = TRUE))),
                                                                             column(width=6,
                                                                                    sliderInput("controller_1_num2", label=
                                                                                                  "Adjust Weighted Pivot Coordinates
                                                                                                Of Composition 1", 0, 100, 50))),
                                                                           fluidRow(
                                                                             column(width=6,
                                                                                    selectizeInput("selectvariable2_num4",
                                                                                                   label="Composition 2",choices={},
                                                                                                   multiple=TRUE,options = list(create = TRUE))),
                                                                             column(width=6,
                                                                                    sliderInput("controller_2_num2", label=
                                                                                                  "Adjust Weighted Pivot Coordinates
                                                                                                Of Composition 2", 0, 100, 50))),
                                                                           fluidRow(
                                                                             column(width=6,
                                                                                    selectizeInput("selectvariable3_num4",
                                                                                                   label="Composition 3",choices={},multiple=TRUE,
                                                                                                   options = list(create = TRUE))),
                                                                             column(width=6,
                                                                                    sliderInput("controller_3_num2", 
                                                                                                label="Adjust Weighted Pivot Coordinates 
                                                                                                Of Composition 3", 0, 100, 50))),
                                                                           fluidRow(
                                                                             column(width=6,
                                                                                    selectizeInput("selectvariable4_num4",
                                                                                                   label="Composition 4",choices={},
                                                                                                   multiple=TRUE,options = list(create = TRUE))),
                                                                             column(width=6,
                                                                                    sliderInput("controller_4_num2", 
                                                                                                label="Adjust Weighted Pivot Coordinates
                                                                                                Of Composition 4", 0, 100, 50)))
                                                                                    ),#condition num4
                                                          fluidRow(column(6,
                                                                          actionButton("button_adist",label="Apply")),
                                                                   column(6,
                                                                          actionButton("button_adist_reset",label="Reset")))
                                                                                    )#condition ture checckbox
                                                                           )#box
                                                                             )#fluidrow
                                   )#tabpanel aDist
  )
    ),
  
  tabItem(tabName = "dataset",
          fluidRow(
            titlePanel("Data Overview Of The Imported Data:"),
            tags$hr(),
            dataTableOutput("datas")
          )
  ),
  tabItem(tabName = "dataset2",
          fluidPage(
            box(width = 20,height = 6500,
                DT::dataTableOutput("datas2"),
                tags$hr(),
                selectInput("choose_save",
                            label="Choose Data Set Format Or Save To R's Global Environment",
                            choices = c("CSV","Xlsx","Load Into R Global Environment")),
                tags$hr(),
                # for csv
                conditionalPanel(condition = "input.choose_save == 'CSV' " ,
                                 tags$hr(),
                                 textInput(inputId = "globalsave_csv",
                                           label="File Name:"),
                                 tags$hr(),
                                 downloadButton("downloadData", "Download"),
                                 tags$hr()
                ),
                conditionalPanel(condition = "input.choose_save == 'Xlsx' " ,
                                 tags$hr(),
                                 textInput(inputId = "globalsave_xlsx",
                                           label="File Name:"),
                                 tags$hr(),
                                 downloadButton("downloadData2", "Download"),
                                 tags$hr()
                ),
                #for global
                conditionalPanel(
                  condition="input.choose_save==
                  'Load Into R Global Environment'",
                  textInput(inputId = "globalsave",label="File Name:"),
                  actionButton("save_global", "Save")
                )#conditional panel global environment              
            ) #box
          )#fluidPage
          ),#tabintem
  
  tabItem(tabName = "dashboard",
          fluidRow(
            tabsetPanel(type="tabs",
                        tabPanel( "Dynamic Map"  ,
                                  box(width=12,
                                      withSpinner(leafletOutput(outputId = "mymap"
                                                                # ,
                                                                # width = 1200,height = 600
                                      ))
                                  )#withspinner
                        ),
                        tabPanel("Faceting Map",
                                 box(width=12,
                                     withSpinner(plotOutput("subplot1",height=800) ))),
                        tabPanel("MDS",
                                 box(width=12,
                                     withSpinner(plotOutput("subplot2")))),
                        tabPanel("Silhouette Plot",
                                 box(width=12,withSpinner(plotOutput("subplot3"))))
            )),
          # Plotmean mit header local validity measures
          fluidRow(
            box(selectInput("select_validity_measures",
                            label="Choose A Validity Measure:",
                            choices= if(exists("daten2$dat")==TRUE){
                              names(t(guete$guete_lokal))}else{list()}),
                withSpinner(plotOutput(outputId = "plotmeans")),
                title = "Plotmeans",status = "primary",
                solidHeader = TRUE,width=12,collapsible = TRUE,collapsed=FALSE
            )),
          # Heatmap dist plot
          fluidRow(
            box(
              radioButtons("heat_version",label = 
                             h3("Select Heatmap Option:"),
                           choices = 
                             list("Interactive Heatmap (Expect delayed output due to intensive computation! Up to 8 minutes)",
                                  "Fixed Heatmap (Expect delayed output due to intensive computation! Up to 2 minutes)"),
                           selected="non"),
              # the next two rows show the row below
              # conditionalPanel(condition = "input.heat_version =='Interactive Heatmap Used Plotly
              #(Expect delayed output due to intensive computation! Up to 8 minutes)' " ,
              conditionalPanel(condition ="input.heat_version =='Interactive Heatmap (Expect delayed output due to intensive computation! Up to 8 minutes)' " ,
                               tags$hr(),
                               withSpinner(plotlyOutput(outputId = "heatdist"))),
              # the next two rows show the row below
              # conditionalPanel(condition = "input.heat_version =='Fixed Heatmap
              # (Expect delayed output due to intensive computation! Up to 2 minutes)' " ,
              conditionalPanel(condition = "input.heat_version =='Fixed Heatmap (Expect delayed output due to intensive computation! Up to 2 minutes)' " ,
                               tags$hr(),
                               withSpinner(plotOutput(outputId = "heatdist2"))
              ),
              title = "Heatmap",status="warning",
              solidHeader = TRUE,width=12,collapsible = TRUE,collapsed=TRUE
            )),
          # Validity measure datatable
          fluidRow(
            box(
              fluidRow(
                column(width = 5,
                       titlePanel("Compare Validity Measures")),
                column(width=1,
                       bsButton("Question_1", label = "", icon = icon("question"),
                                style = "info", size = "extra-small"),
                       bsPopover(id = "Question_1", title = "Compare Data",
                                 content =
                                   paste("Provided Validity Measures can be temporarily", 
                                         "stored for comparison or downloaded to either a",
                                         "local directory or to R&#39s ",
                                         "global environment.The table of measurements",
                                         "displayed on the left-hand side represent the",
                                         "measurements of the current cluster analysis at",
                                         "all times. Comparisons can be drawn to an",
                                         "altered table after the data is stored",
                                         "and listed for selection."),
                                 trigger="click"
                       )),
                column(width=6)),
              fluidRow( tags$hr() ),
              fluidRow(
                column(
                  titlePanel("Local Validity Measure"), 
                  withSpinner(DTOutput(outputId = "guetemasse")),
                  width=6),
                
                conditionalPanel(condition="input.Secondary_validity!=''",
                                 column(
                                   h2(textOutput(outputId = "comparison_local")),
                                   withSpinner(DTOutput(outputId = "guetemasse_comparative")),
                                   width = 6)
                )),
              fluidRow(
                column(
                  titlePanel("Global Validity Measures"),
                  withSpinner(DTOutput("guetemasse2")),
                  width = 6),
                conditionalPanel(condition="input.Secondary_validity!=''",
                                 column(
                                   h2(textOutput(outputId = "comparison_global")),
                                   withSpinner(DTOutput("guetemasse2_comparative")),
                                   width = 6)
                )),
              fluidRow(
                column(width = 6,
                       titlePanel("Store Results For Comparison"), 
                       tags$hr(),
                       fluidRow(column(width=5,
                                       textInput(
                                         inputId = "reactive_validity",
                                         label="Store Results As:" )),
                                column(width=5,
                                       titlePanel(""),
                                       actionButton("append_list", "Store"))),
                       titlePanel("Download Validity Measures"),
                       tags$hr(),
                       selectInput("choose_save_validity_measures",
                                   label="Choose Data Set Format Or Save To R's Global Environment",
                                   choices = c("CSV","Xlsx",
                                               "Load Into R Global Environment")),
                       # for csv
                       conditionalPanel(
                         condition="input.choose_save_validity_measures=='CSV'",
                         textInput(inputId = "globalsave_csv_validity",
                                   label="File Name:"),
                         downloadButton("downloadData_validity", "Download")
                       ),
                       conditionalPanel(
                         condition="input.choose_save_validity_measures=='Xlsx'",
                         textInput(inputId = "globalsave_xlsx_validity",
                                   label="File name:"),
                         downloadButton("downloadData2_validity", "Download")
                       ),
                       
                       #for global
                       conditionalPanel(condition="input.choose_save_validity_measures ==
                                        'Load Into R Global Environment'",
                                        textInput(
                                          inputId = "globalsave_validity",
                                          label="File name:"),
                                        actionButton("save_global_validity", "Save")
                       )#conditional panel global environment 
                       
              ),#column
              
              column(width = 6,
                     titlePanel("Compare To Stored Data:"),
                     tags$hr(),
                     selectizeInput("Secondary_validity",
                                    label="Select From Stored Results",
                                    choices=list())
              )
              ),#fluidrow save measures
              title="Validity Measures",status = "success",solidHeader = TRUE,
              width=12,collapsible = TRUE,collapsed=FALSE
              )#box
          )#fluidrow data table validity measures
          ),  #tabitem dashboard
  tabItem(
    tabName = "calc",
    fluidRow(column(4, titlePanel("Best Number Of Clusters")
                    
    ), #column best number of clusters
    column(8,withSpinner(plotOutput(outputId = "nbplot") ) ) #column nbplot
    )#fluidrow best number of cluster
  ) #tabitem calc
  ) #tabitems
  ) #dashboardbody
  ) #dashboardpage
