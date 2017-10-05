
header <- shinydashboard::dashboardHeader(title = "prioritizr Shiny")

body <-  shinydashboard::dashboardBody(
  shiny::fluidRow(
    shiny::column(width = 5, shinydashboard::tabBox(title = "",
    # The id lets us use input$tabset1 on the server to find the
    # current tab
      id = "tabset1", width = NULL, height = "600px",
      shiny::tabPanel("Problem",
        shiny::h4("Create a conservation problem"),
        shiny::p(paste("By specifying the inputs below you can",
                       "supply data to generate the problem")),
        shiny::br(),
        shiny::fileInput('x', 'x',multiple = TRUE),
        shiny::fileInput('features', 'features',multiple = TRUE),
        shiny::fileInput('rij', 'rij (optional)',multiple = TRUE),
        shiny::fileInput('rij_matrix', 'rij_matrix (optional)',
                         multiple = TRUE),
        tags$hr(),
        shiny::actionButton("rproblem","Create the prioritizr problem")),
      shiny::tabPanel("Objective",
        shiny::h4("Select an objective function for the conservation problem"),
        shiny::radioButtons("objective",
         label = paste("Please choose the objective function you",
                       "want to add to conservation problem."),
         choices = list("Minimum set objective" = 1,
                        "Maximum cover" = 2,
                        "Maximum features" = 3,
                        "Maximum phylogeny" = 4),
         selected = 1, width = "90%"),
         tags$hr(),
         shiny::actionButton("robjetive", paste("Add the selected objective to",
                                            "the prioritizr problem"))),
      shiny::tabPanel("Targets", "Tab content Targets"),
      shiny::tabPanel("Constraints", "Tab content Constraints"),
      shiny::tabPanel("Penalties", "Tab content Penalties"))),
    shiny::column(width = 7, shinydashboard::tabBox(
      title = "", id = "tabset2", width = NULL, height = "600px",
      shiny::tabPanel("Set Targets", "Set your prioritization targets"),
      shiny::tabPanel("Results", leaflet::leafletOutput("mymap"))))),
  shiny::fluidRow(
    shiny::column(width = 5, shinydashboard::box(width = NULL,
                                                 solidHeader = TRUE,
      title = shiny::tagList(shiny::icon("gear"), "Solve the problem"),
      shiny::p(paste("If you are happy with your specification, go",
                      "ahead and solve your conservation problem."),
      shiny::actionButton("rsolve","Solve the prioritizr problem"))))))

shinydashboard::dashboardPage(header,
                              shinydashboard::dashboardSidebar(disable = TRUE),
                              body)
