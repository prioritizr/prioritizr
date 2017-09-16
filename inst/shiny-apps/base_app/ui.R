  
header <- dashboardHeader(
  
  title = "prioritizr Shiny"
)

body <-  dashboardBody(
  fluidRow(
    column(width = 5,
           tabBox(
             title = "",
             # The id lets us use input$tabset1 on the server to find the current tab
             id = "tabset1", width = NULL,height = "600px", 
             tabPanel("Problem", 
               h4("Create a conservation problem"),
               p("By specifying the inputs below you can supply data to generate the problem"),
               br(),
               fileInput('x', 'x',multiple=TRUE),
               fileInput('features', 'features',multiple=TRUE),
               fileInput('rij', 'rij (optional)',multiple=TRUE),
               fileInput('rij_matrix', 'rij_matrix (optional)',multiple=TRUE),
               
               tags$hr(),
               actionButton("rproblem","Create the prioritizr problem")
               

               ),
             tabPanel("Objective", 
                      h4("Select an objective function for the conservation problem"),
                      radioButtons("objective", label = "Please choose the objective function you 
                                   want to add to conservation problem.",
                                   choices = list("Minimum set objective (basic Marxan objective)" = 1, 
                                                  "Maximum cover (set budget)" = 2, 
                                                  "Maximum features (combination of min set and max cover)" = 3,
                                                  "Maximum phylogeny (silimar to max features, but emphasises phylogenetic representation" = 4), 
                                   selected = 1,
                                   width = "90%"),
                      tags$hr(),
                      actionButton("robjetive","Add the selected objective to the prioritizr problem")),
             tabPanel("Targets", "Tab content Targets"),
             tabPanel("Constraints", "Tab content Constraints"),
             tabPanel("Penalties", "Tab content Penalties")
             
           )
    ),
    column(width = 7,
           tabBox(
             title = "",
             id = "tabset2", width = NULL,height = "600px", 
             tabPanel("Set Targets", "Set your prioritization targets"),
             tabPanel("Results", 
               leafletOutput("mymap")#,height=600)
           )
    ))
    
  ),

  fluidRow(
    column(width = 5,
    box(width = NULL, solidHeader = TRUE,
      # Title can include an icon
      title = tagList(shiny::icon("gear"), "Solve the problem"),
      p("If you are happy with your specification, go ahead and solve your conservation problem."),
      actionButton("rsolve","Solve the prioritizr problem")
      
    ))
  )
)


dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)