library(shinydashboard)

header <- dashboardHeader(title = tagList("cnnAlpha"),
                          titleWidth = 175)

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
                    span(style = "font-size: 12px",
                         HTML("Intrinsically disordered regions (IDR) play an important role in key biological processes and are closely related to human diseases. They also have the potential to serve as targets for drug discovery, especially in disordered binding regions."),
                         HTML("<br><br>Accurate prediction of IDRs is challenging, their genome wide occurrence and low ratio of disordered residues make them a difficult target for traditional classification techniques.Existing methods mostly rely on sequence profiles to improve accuracy which is time consuming and computationally expensive.
                              This article describes a method based on reduced amino acid alphabets and convolutional neural networks (CNN), which tries to overcome this challenge by utilizing sequence only information. We experiment with 6 different alphabets,
                              reducing the original 20 letter alphabet into 3 and 4 letter alphabets. We argue that the dimensional reduction in the input alphabet facilitates  the detection of complex patterns within the sequence by the convolutional step."),
                         HTML("<br><br>Experimental results show that our proposed IDR prediction approach performs at the same level or outperforms 
                              other state of the art sequence only ab-initio methods, achieving accuracy levels of 0.76 and AUC of 0.85 on the publicly available Critical Assessment of protein Structure Prediction dataset (CASP10).
                              Therefore, our method is suitable for fast proteome-wide disorder prediction yielding similar or better accuracy than current existing approaches.")
                    )
            ),
            actionBttn(inputId= 'startHere', label = 'Start here', style = "pill", color = "success")
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
              sidebar,
              body)