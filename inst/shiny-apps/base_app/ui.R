
header <- dashboardHeader(
  
  title = "prioritzrshiny"
)

body <-  dashboardBody(
  fluidRow(
    column(width = 5,
           tabBox(
             title = "Inputs",
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
               actionButton("mrun","Create the prioritizr problem")
               

               ),
             tabPanel("Objective", "Tab content Objective"),
             tabPanel("Constraints", "Tab content Constraints"),
             tabPanel("Penalties", "Tab content Penalties")
             
           )
    ),
    column(width = 7,
           tabBox(
             title = "Oyt",
             id = "tabset2", width = NULL,height = "600px", 
             tabPanel("Problem", 
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
      actionButton("mrun","Solve teh prioritizr problem")
      
    ))
  )
)


dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)