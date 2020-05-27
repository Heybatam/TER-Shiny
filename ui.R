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
                                     br(),
                                     # Displaying summary
                                     verbatimTextOutput(outputId = "summary"),
                                     dataTableOutput("contents")
                            ),
                            tabPanel("Visualization",
                                     br(),
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
                                     br(),
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
                          br(),
                          uiOutput("km_pcs_x"),
                          uiOutput("km_pcs_y"),
                          sliderInput('iter', 'Iterations:', 50, min = 1, max = 170),
                          sliderInput('clusters', 'Cluster count', 3, min = 1, max = 10),
                          checkboxInput("trace", label = "Trace", value = TRUE),
                          selectInput("kmalgo", label = h3("Algorithm :"), 
                                      choices = c("Hartigan-Wong", "Lloyd", "Forgy",
                                                  "MacQueen"),
                                      selected = "Lloyd"),
                          numericInput("nstart", label = h3("Random seed:"), value = 17),
                          tags$hr(),
                          p("If the dataset is labelised, select the label column :"),
                          uiOutput("km_label")
                        ), #end sidebar panel
                        
                        mainPanel(
                            fluidRow(column(
                            6,
                            h2("Kmeans clusters"),
                            tags$hr(),
                            br(),
                            plotOutput('plot_kmeans', 500)
                          ),
                          column(6,
                                 h2("Label truth"),
                                 tags$hr(),
                                 br(),
                                 plotOutput("plot_kmeans_truth", 500)
                          ))
                        )# end main panel
                      )#end sidebar layout
             ), # end nav tab
             #################################################### tab panel GMM ####################################################             
             tabPanel("GMM",
                      sidebarLayout(
                        sidebarPanel(
                          strong("INPUT GMM"),
                          br(),
                          
                          uiOutput("gmm_pcs_x"),
                          uiOutput("gmm_pcs_y")
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Summary",
                                     br(),
                                     helpText("Mclust uses an identifier for each possible parametrization of the covariance matrix that has three letters:",
                                              "E for equal, V for variable and I for coordinate axes. The first identifier refers to volume, the second to shape and the third to orientation. For example:",
                                              "EEE means that the G clusters have the same volume, shape and orientation in p−dimensional space.",
                                              "VEI means variable volume, same shape and orientation equal to coordinate axes.",
                                              "EIV means same volume, spherical shape and variable orientation."),
                                     tags$hr(),
                                     p("optimal selected model"),
                                     verbatimTextOutput("gmm_iden"),
                                     p("optimal number of cluster"),
                                     verbatimTextOutput("gmm_cluster"),
                                     p("probality for an observation to be in a given cluster"),
                                     verbatimTextOutput("gmm_head"),
                                     tags$hr(),
                                     verbatimTextOutput("gmm_summary")
                                     
                            ),# end tab
                            
                            tabPanel("Plots",
                                     br(),
                                     fluidRow(
                                       column(6,
                                              p("Classification : "),
                                              tags$hr(),
                                              plotOutput("gmm_classification")),
                                       column(6,
                                              p("Uncertainty : "),
                                              tags$hr(),
                                              plotOutput("gmm_uncertainty"))
                                     ),#end fluid
                                     fluidRow(
                                       column(6,
                                              p("Density : "),
                                              tags$hr(),
                                              plotOutput("gmm_density")),
                                       column(6,
                                              p("Persp : "),
                                              tags$hr(),
                                              plotOutput("gmm_persp"))
                                     )#end fluid
                            )#end tab
                            
                          )# end tabset
                        )#end main
                      )#end sidebar
                    ), #end gmm
             
             
                      #################################################### tab panel SPECTRAL ####################################################             
                      tabPanel("Spectral clustering",
                               sidebarLayout(
                                 sidebarPanel(
                                   strong("INPUT SPECTRAL CLUSTERING"),
                                   br(),
                                   
                                   uiOutput("specc_pcs_x"),
                                   uiOutput("specc_pcs_y"),
                                   sliderInput("specc_centers", "Centers :", min=2, max=15, value=3),
                                   
                                   tags$hr(),
                                   p("If the dataset is labelised, select the label column :"),
                                   uiOutput("sc_label")
                                 ),
                                 mainPanel(
                                   p("Spectral clustering techniques make use of the spectrum (eigenvalues) of the similarity matrix of the data"),
                                   p("to perform dimensionality reduction before clustering in fewer dimensions. The similarity matrix"),
                                   p("is provided as an input and consists of a quantitative assessment of the relative similarity of each pair of points in the dataset."),
                                   tags$hr(),
                                   
                                   h3("Spectral clustering summary"),
                                   verbatimTextOutput("sc"),
                                   tags$hr(),
                                   
                                   fluidRow(
                                     column(6,
                                            h1("Spectral clustering"),
                                            plotOutput("specc",500)    
                                     ),
                                     column(6,
                                            h1("True labels"),
                                            plotOutput("specc_truth",500)    
                                     )
                                   )
                                 )
                               )
                      ),
                      ##################################################### tab panel AP ####################################################             
                       tabPanel("Affinity propagation",
                                sidebarLayout(
                                  sidebarPanel(
                                      strong("INPUT AFFINITY PROPAGATION"),
                                      br(),
                                      
                                      uiOutput("ap_pcs_x"),
                                      uiOutput("ap_pcs_y")
                                    ),
                                    mainPanel(
                                      h2("Affinity propagation clusters"),
                                      p("We run affinity propagation with the default preference then with a 10% quantile similarities preference;"),
                                      p("this should lead to a smaller number of clusters reuse similarity matrix from previous run"),
                                      tags$hr(),
                                      fluidRow(column(
                                        6,
                                        p("__"),
                                        plotOutput("ap_plot",400)
                                      ),
                                      column(6,
                                             p("10% quantile"),
                                             plotOutput("ap_plot2",400)
                                      )),
                                      verbatimTextOutput("ap_print"),
                                      
                                      tags$hr(),
                                      h3("Affinity propagation heatmap"),
                                      
                                      tags$hr(),
                                      plotOutput("ap_hm")
                                    )
                                )
                                
                       ), #end tab AP
             
             #################################################### tab panel EC ####################################################             
                      tabPanel("Ensemble Clustering",
                               sidebarLayout(
                                  sidebarPanel(
                                    strong("INPUT ENSEMBLE CLUSTERING"),
                                    br(),
                                    
                                    uiOutput("ec_pcs_x"),
                                    uiOutput("ec_pcs_y"),
                                    tags$hr(),
                                    checkboxGroupInput("nk_check", "Clusters :", choices = c(2:15), selected = c(2:4), inline=TRUE),
                                    sliderInput("pct_slider", label ="Percentage :", min=0, max = 1, value = 0.8, step = 0.01),
                                    sliderInput("reps_slider", label = "Repetitions :", min = 1, max = 100, value = 5),
                                    tags$hr(),
                                    uiOutput("ec_label"),
                                    checkboxGroupInput("ec_cons", "Consensus functions to use:",
                                                       choices= c("kmodes", "majority", "CSPA",
                                                                  "LCE", "LCA"), selected ="kmodes", inline=TRUE),
                                    
                                    tags$hr(),
                                    actionButton(inputId = "ec_go", label = "Compute")
                                  ),
                                  mainPanel( 
                                    tabsetPanel(
                                      tabPanel("Heatmap",
                                               br(),
                                               fluidRow(
                                                 column(3,
                                                        br(),
                                                        selectInput("ec_algos", "Choose algorithm :",
                                                                choices = c("km","gmm","sc","ap"),
                                                                selected = "km"),
                                                        uiOutput("hm_ev")
                                                        ),
                                                      
                                                 
                                                 column(9,
                                                        plotOutput("plot_hm_clus",700)
                                                 )
                                                ),
                                               strong("Number of clusters found by ensemble clustering:"),
                                               verbatimTextOutput("ec_k"),
                                               tags$hr(),
                                               br(),
                                               
                                               h3("Cluster repartition :"),
                                               dataTableOutput("ec_clusters")

                                      ),
                                      tabPanel("Internal and external validity",
                                               br(),
                                                 h3("External validity"),
                                                 dataTableOutput("ec_ei"),
                                                 tags$hr(),
                                               
                                                 br(),
                                                 # htmlOutput("ec_ii"),
                                                 h3("Internal validity"),
                                                 dataTableOutput("eck")
                                               ),#end tab validity
                                       tabPanel("Miscellaneous",
                                                br(),
                                                
                                                h3("Cumulative distribution function"),
                                                plotOutput("ec_cdf"),
                                                tags$hr(),
                                                br(),
                                                h3("Delta area"),
                                                
                                                plotOutput("ec_delta"),
                                                tags$hr(),
                                                br(),
                                                
                                                h3("Tracking"),
                                                plotOutput("ec_tracking")
                                                )#end tab misc
                                    ) #set
                                  ) # end main
                               )
                      )
             )
  )
