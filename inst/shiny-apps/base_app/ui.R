
header <- dashboardHeader(
  
  title = "prioritzrshiny"
)

body <-  dashboardBody(
  useShinyjs(),
  fluidRow(
    column(width = 5,
           tabBox(
             title = "Inputs",
             # The id lets us use input$tabset1 on the server to find the current tab
             id = "tabset1", width = NULL,height = "600px", 
             
             ## DATA
             tabPanel("Data", 
               h4("Upload your data"),
               p("By specifying the inputs below you can supply data to generate the problem"),
               br(),
               fileInput('x', 'x',multiple=TRUE),
               shinyjs::hidden(
                 selectizeInput("cost_col", "Select cost column", choices = NULL, # no choices before uploading 
                               selected = NULL, multiple = FALSE)
                ),
               
               shinyjs::hidden(
                 selectizeInput("feat_col", "Select colum names of features", choices = NULL, # no choices before uploading 
                                selected = NULL, multiple = TRUE)
               ),
               
               #fileInput('features', 'features',multiple=TRUE),
          #drop-down based on x type to optionally selct cost column
               
               #fileInput('rij', 'rij (optional)',multiple=TRUE),
               #fileInput('rij_matrix', 'rij_matrix (optional)',multiple=TRUE),
               
               tags$hr(),
          
              "Once"  
               ),
          
               ## OBJECTIVES
               tabPanel("Objective",
                        selectizeInput("objective", "What objective function do you want to use?", 
                                       choices = c("Minimum set" = "min_set",
                                                   "Maximum cover" = "max_cov",
                                                   "Maximum features" = "max_feat",
                                                   "Maximum phylogeny" = "max_phylo",
                                                   "Maximum untility" = "max_util"),
                                       selected = 'min_set', multiple = FALSE),
                        shinyjs::hidden(
                          div(id = "targets",
                              selectizeInput("tar_type", "What target type do you want to use?", 
                                             choices = c("Relative target" = "rel_tar",
                                                         "Absolute target" = "abs_tar",
                                                         "Loglinear target" = "log_tar"),
                                             selected = 'rel_tar', multiple = FALSE),
                              radioButtons('glob_tar', 'Use one target for all?',
                                           c("Yes, global Target" = 'global',
                                             "No, individual Targets" = 'ind_tar'),'global'),
                              shinyjs::hidden(
                                numericInput("tar_all", "Set the global target", value = 0.1)
                                )
                              )
                          ),
                        shinyjs::hidden(
                          numericInput("budget", "Set the budget", value = 0)
                        ),
                        shinyjs::hidden(
                          textAreaInput("phylo", "Add Maximum Phylogenetic Representation Objective", value = "Not sure what we need here")
                        )
                        
               ),
            
            ## CONSTRAINTS
            tabPanel("Constraints", "Tab content Constraints"),
            
            ## PENALTIES
            tabPanel("Penalties", 
                        p("A penalty can be applied to a conservation planning 
                        problem to penalize solutions according to a specific metric. 
                        Penalties---unlike constraints---act as an explicit trade-off with the 
                        objective being minimized or maximized (e.g. total solution cost given 
                        add_min_set_objective)."),
                      selectizeInput("penalty", "Do you want to use a penalty function?", 
                                     choices = c("No penalty" = "none",
                                                 "Boundary penalty" = "bound",
                                                 "Connectivity penalty" = "conn"),
                                     selected = NULL, multiple = FALSE),
                      shinyjs::hidden(
                        div(id = "pen_bound",
                            numericInput("penalty", "Penalty", 0),
                            numericInput("edge_factor", "Edge factor", 0),
                            fileInput('boundary_data', 'Boundary data (optional)',multiple=TRUE)
                            )
                      ),
                      
                      shinyjs::hidden(
                        div(id = "pen_conn",
                            numericInput("penalty", "Penalty", 0),
                            fileInput('connectivity_data', 'Connectivity data',multiple=TRUE)
                        )
                      )

                      )

                      
           )
    ),
    column(width = 7,
           tabBox(
             #title = "Oyt",
             id = "tabset2", width = NULL,height = "600px", 
             about_panel,
             howto_panel,
             tabPanel("Outputs",
                      box(
                        #                    solidHeader = TRUE,status = "primary",
                        leafletOutput("mymap", height=400),
                        width = NULL,
                        collapsible = FALSE),#,collapsed=TRUE),
                      box(
                        solidHeader = FALSE,#,status = "primary",
                        #background = "light-blue",
                        tableOutput("contents"),
                        width = NULL)
                      
                      #leafletOutput("mymap")#,height=600)
           )
    ))
    
  ),

  fluidRow(
    column(width = 5,
    box(width = NULL, solidHeader = TRUE,
      # Title can include an icon
      title = tagList(shiny::icon("gear"), "Setup and solve the problem"),
      actionButton("Bproblem","Create the prioritizr problem"),
      tags$hr(),
      shinyjs::hidden(
        div(id = "to_solve",
          p("If you are happy with your specification, go ahead and solve your conservation problem."),
          actionButton("Bsolve","Solve the prioritizr problem")
        )
      )
      
    ))
  )
)


dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)