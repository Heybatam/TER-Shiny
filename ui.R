library(shiny)
library(shinythemes)

ui <- fluidPage(
  
  shinythemes::themeSelector(),
  
  navbarPage("Home",
             
             
             #################################################### tab panel data ####################################################                          
             tabPanel("Data",
                      
                      sidebarLayout(
                        sidebarPanel(
                          strong("INPUT DATA"),
                          br(),
                          p ("Remember we are looking for this (click on the next image to view it in a new tab)"),
                          br(),
                          p("Before uploading your data, check that it is clean, especially ensure that the the numeric variables contain only the digits 0-9 or NA (to indicate missing data)."),
                          p("Rows that contain one or more NAs will be excluded from the PCA."),
                          br(),
                          p("Columns that contain a mixture of numbers and text will not be included in the computation of the PCA results."),
                          
                          tags$hr(),
                          p("Select the options that match your CSV file, then upload your file:"),
                          
                          fileInput("file1", "Choose CSV File",
                                    accept = c(
                                      'text/csv',
                                      'text/comma-separated-values',
                                      'text/tab-separated-values',
                                      'text/plain',
                                      '.csv',
                                      '.tsv')
                          ),
                          tags$hr(),
                          
                          radioButtons(inputId= 'header',
                                       label = 'Header',
                                       choices = c('Columns have header' = 'Yes',
                                                   'Columns do not have a header' = 'No'),
                                       selected = 'No'),#rbuttons header
                          
                          radioButtons('sep', 'Separator',
                                       c(Comma=',',
                                         Semicolon=';',
                                         Tab='\t'),
                                       '\t'),#rbuttons separator
                          radioButtons('quote','Quote',
                                       c(None='','Double Quote'='"','Single quote'="'"),
                                       ''),
                          tags$hr(),
                          actionButton(inputId = "go", label = "Load")
                          
                          
                          
                        ),#sidebar panel
                        
                        mainPanel(
                          h2("Basic information from the dataset"),
                          tabsetPanel(  
                            tabPanel("Summary",
                                     # Displaying summary
                                     verbatimTextOutput(outputId = "summary"),
                                     dataTableOutput("contents")
                            ),
                            tabPanel("Visualization",
                                     p("Pairs repartition :"),
                                     plotOutput("scatter"),
                                     tags$hr(),
                                     p("Frequency :"),
                                     plotOutput("histo")
                            )
                          )
                        )#main panel
                      )#sidebar layout
             ),
             
             
             #################################################### tab panel PCA ####################################################             
             tabPanel("PCA",
                      sidebarLayout(
                        sidebarPanel(
                          strong("INPUT PCA"),
                          br(),
                          p("Choose the columns of your data to include in the PCA."),
                          p("Only columns containing numeric data are shown here because PCA doesn't work with non-numeric data."),
                          p("The PCA is automatically re-computed each time you change your selection."),
                          p("Observations (ie. rows) are automatically removed if they contain any missing values."),
                          p("Variables with zero variance have been automatically removed because they're not useful in a PCA."),
                          uiOutput("col_pca"),
                          tags$hr(),
                          p("Select options for the PCA computation (we are using the prcomp function here)"),
                          radioButtons(inputId = 'center',  
                                       label = 'Center',
                                       choices = c('Shift variables to be zero centered'='Yes',
                                                   'Do not shift variables'='No'), 
                                       selected = 'Yes'),
                          
                          radioButtons('scale.', 'Scale',
                                       choices = c('Scale variables to have unit variance'='Yes',
                                                   'Do not scale variables'='No'), 
                                       selected = 'Yes'),
                          actionButton(inputId = "pcago", label = "Compute")
                        ),
                        
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Diagnostics",
                                     p("Among SPSS users, these tests are considered to provide some guidelines on the suitability of the data for a principal components analysis. However, they may be safely ignored in favour of common sense. Variables with zero variance are excluded."),
                                     tags$hr(),
                                     p("Here is the output of Bartlett's sphericity test. Bartlett's test of sphericity tests whether the data comes from multivariate normal distribution with zero covariances. If p > 0.05 then PCA may not be very informative"),
                                     verbatimTextOutput("bartlett"),
                                     tags$hr(),
                                     p("Here is the output of the Kaiser-Meyer-Olkin (KMO) index test. The overall measure varies between 0 and 1, and values closer to 1 are better. A value of 0.6 is a suggested minimum. "),
                                     verbatimTextOutput("kmo")
                            ), #end tab
                            
                            tabPanel("Cumulative variance",
                                     h2("PCA Summary"),
                                     verbatimTextOutput("pca_details"),
                                     tags$hr(),
                                     h2("Scree plot"),
                                     p("The scree plot shows the cumulative variance of each PC, and the cumulative variance explained by each PC (in %)"),
                                     plotOutput("plot2", height = "300px"),
                                     tags$hr(),
                                     h2("PC plot : zoom and select points"),
                                     p("select the grouping variable."),
                                     p("Only variables where the number of unique values is less than 10% of the total number of observations are shown here(because seeing groups with 1-2 observations is usually not very useful)."),
                                     uiOutput("the_grouping_variable"),
                                     tags$hr(),
                                     p("Select the PCs to plot"),
                                     uiOutput("pcs_to_plot_x"),
                                     uiOutput("pcs_to_plot_y"),
                                     tags$hr(),
                                     
                                     p("Click and drag on the first plot below to zoom into a region on the plot. Or you can go directly to the second plot below to select points to get more information about them."),
                                     p("Then select points on zoomed plot below to get more information about the points."),
                                     p("You can click on the 'Compute PCA' tab at any time to change the variables included in the PCA, and then come back to this tab and the plots will automatically update."),
                                     plotOutput ("z_plot1", height = 400),
                                     tags$hr(),
                                     
                                     p("Click and drag on the plot below to select points, and inspect the table of selected points below"),
                                     
                                     plotOutput("z_plot2", height = 400,
                                                brush = brushOpts(
                                                  id = "plot_brush_after_zoom",
                                                  resetOnNew = TRUE)),
                                     tags$hr(),
                                     
                                     p("Details of the brushed points"),
                                     tableOutput("brush_info_after_zoom")
                            )
                          ) # end tab
                        )
                      ) 
             ),# end nav tab
             
             #################################################### tab panel KMEANS ####################################################             
             tabPanel("Kmeans",
                      sidebarLayout(
                        sidebarPanel(
                          strong('INPUT K-MEANS'),
                          uiOutput("km_pcs_x"),
                          uiOutput("km_pcs_y"),
                          sliderInput('iter', 'Iterations:', 50, min = 1, max = 170),
                          sliderInput('clusters', 'Cluster count', 3, min = 1, max = 10),
                          checkboxInput("trace", label = "Trace", value = TRUE),
                          selectInput("kmalgo", label = h3("Algorithm :"), 
                                      choices = c("Hartigan-Wong", "Lloyd", "Forgy",
                                                  "MacQueen"),
                                      selected = "Lloyd"),
                          numericInput("nstart", label = h3("Random seed:"), value = 1),
                          tags$hr(),
                          p("If the dataset is labelised, select the label column :"),
                          uiOutput("km_label"),
                          actionButton("kmgo","Generate")
                        ), #end sidebar panel
                        
                        mainPanel(
                            fluidRow(column(
                            6,
                            h2("Kmeans clusters"),
                            p("yes yes"),
                            tags$hr(),
                            p("plot"),
                            plotOutput('plot_kmeans', 400)
                          ),
                          column(4,
                                 h2("Label truth"),
                                 p("coucou"),
                                 tags$hr(),
                                 plotOutput('plot_lkmeans', 400)
                          ))
                        )# end main panel
                      )#end sidebar layout
             ), # end nav tab
             #################################################### tab panel GMM ####################################################             
             tabPanel("GMM",
                      sidebarLayout(
                        sidebarPanel(
                          strong("INPUT GMM"),
                          uiOutput("gmm_pcs_x"),
                          uiOutput("gmm_pcs_y")
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Modèle 2",
                                     fluidRow(
                                       column(6,
                                              p("plot 1 : "),
                                              tags$hr(),
                                              plotOutput("mod4__dens_plot1")),
                                       column(6,
                                              p("plot 2 : "),
                                              tags$hr(),
                                              plotOutput("mod4__dens_plot2"))
                                     ),#end fluid
                                     fluidRow(
                                       column(6,
                                              p("plot 3 : "),
                                              tags$hr(),
                                              plotOutput("mod4__dens_plot3")),
                                       column(6,
                                              p("plot 4 : "),
                                              tags$hr(),
                                              plotOutput("mod4__dens_plot4"))
                                     )#end fluid
                            ),#end tab
                            tabPanel("Modèle 1",
                                     plotOutput("mod1_plot")
                            )# end tab
                          )# end tabset
                        )#end main
                      )#end sidebar
                    ), #end gmm
             
             
                      #################################################### tab panel SPECTRAL ####################################################             
                      tabPanel("Spectral clustering",
                               sidebarLayout(
                                 sidebarPanel(
                                   strong("INPUT SPECTRAL CLUSTERING"),
                                   uiOutput("specc_pcs_x"),
                                   uiOutput("specc_pcs_y"),
                                   sliderInput("specc_centers", "Centers :", min=2, max=15, value=3)
                                 ),
                                 mainPanel(
                                   plotOutput("specc")
                                 )
                               )
                      ),
                      ##################################################### tab panel AP ####################################################             
                       tabPanel("Affinity propagation",
                                sidebarLayout(
                                  sidebarPanel(
                                      strong("INPUT AFFINITY PROPAGATION"),
                                      uiOutput("ap_pcs_x"),
                                      uiOutput("ap_pcs_y")
                                    ),
                                    mainPanel(
                                      fluidRow(column(
                                        6,
                                        h2("Ap clusters"),
                                        p("yes yes"),
                                        tags$hr(),
                                        plotOutput("ap_plot",400)
                                      ),
                                      column(6,
                                             p("run affinity propagation with default preference of 10% quantile"),
                                             p("of similarities; this should lead to a smaller number of clusters"),
                                             p("reuse similarity matrix from previous run"),
                                             tags$hr(),
                                             plotOutput("ap_plot2",400)
                                      )),
                                      verbatimTextOutput("ap_print"),
                                      plotOutput("ap_hm")
                                    )
                                )
                                
                       ), #end tab AP
             
             #################################################### tab panel EC ####################################################             
                      tabPanel("Ensemble Clustering",
                               sidebarLayout(
                                  sidebarPanel(
                                  ),
                                  mainPanel(
                                  )
                               )
                      )
             )
  )
