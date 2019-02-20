library(shinydashboard)

appTitle <- "shiny-pred"


header <- dashboardHeader(titleWidth = 175,
    title = div(style = "display:flex;", div(class = 'deploy-state', 'alpha'), appTitle),
    tags$li(class = "dropdown",
            tags$li(class = "dropdown", textOutput("logged_user"), style = "padding-top: 15px; padding-bottom: 15px; padding-right:15px; color: #fff;"),
            tags$li(class = "dropdown", actionLink("info_btn", '', icon=icon('question', 'fa-lg')))
            #tags$li(class = "dropdown", actionLink("login_btn", 'Log in', icon=icon('user'), style = "padding-top: 15px; color: #fff;" ))
            )
)

body <- dashboardBody(
    useShinyjs(),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    tabItems(
    tabItem(tabName = "inputData",
            fluidPage(
                fluidRow(inputHandlingUI('alpha'))
                
            )),
    tabItem(tabName = "about",
            h3(
                "Convolutional neural network + alphabet reduction Protein Disorder Prediction",
                icon('sitemap', "fa-1x")
            ),
            br(),
                box(
                    width = NULL,
                    collapsible = FALSE,
                    solidHeader = FALSE,
                    status = 'success',
                    span(style = "font-size: 14px",
                         HTML("Intrinsically disordered regions (IDR) play an important role in key biological processes and are closely related to human diseases. They also have the potential to serve as targets for drug discovery, especially in disordered binding regions."),
                         HTML("<br><br>Accurate prediction of IDRs is challenging, their genome wide occurrence and low ratio of disordered residues make them a difficult target for traditional classification techniques. Existing methods mostly rely on sequence profiles to improve accuracy which is time consuming and computationally expensive.
                              Our method based on reduced amino acid alphabets and convolutional neural networks (CNN), tries to overcome this challenge by utilizing sequence only information. We experiment with 6 different alphabets
                              reducing the original 20 letter alphabet into 3 letter alphabets and several network architectures. We argue that the dimensional reduction in the input alphabet facilitates  the detection of complex patterns within the sequence by the convolutional step."),
                         HTML("<br><br>Experimental results show that our IDR prediction server performs at the same level or outperforms 
                              current state of the art sequence only ab-initio methods at faster speeds, achieving balanced accuracy levels of 0.76 and AUC of 0.85 on the publicly available Critical Assessment of protein Structure Prediction dataset (CASP10).")
                    )
            ),
            actionBttn(inputId= 'startHere', label = 'Start here', style = "material-flat", color = "success")
        ),
    tabItem(tabName = "results",
                fluidPage(sequencePlotUI('alpha'))
            ),
    tabItem(tabName = "benchmark",
            fluidPage(sequenceBenchmarkUI('alpha'))
    )
))

sidebar <- dashboardSidebar(width = 175,
                            sidebarMenu(
                                id = "tabs",
                                menuItem("About", icon = icon("info-circle"), tabName = "about"),
                                menuItem("Sequence input", tabName = "inputData", icon = icon('arrow-right')),
                                menuItem("Results", icon = icon("flask"), tabName = "results"),
                                menuItem("Benchmark", icon = icon("th"), tabName = "benchmark")
                            )
                        )

dashboardPage(header,
              title = appTitle,
              sidebar,
              body)