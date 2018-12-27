# Module UI function
inputHandlingUI <- function(id) {
    # Create a namespace function using the provided id
    ns <- NS(id)
    
    tagList(
        box(
            title =  tagList(
                div(style="display: inline-block;vertical-align:top; width: 50px;",
                    actionBttn(inputId= 't1', label = '1', style = "material-circle", color = "success")
                ),
                div(style="display: inline-block;vertical-align:top;",
                    h4('Select protein sequence')
                )
            ),
            width = NULL,
            collapsible = TRUE,
            h4('Upload a file with protein sequences or paste them into text area. You may also select from predefined sequence examples'),
            br(),
            div(style="display: inline-block;vertical-align:top; width: 275px;",
                fileInput(ns("sequenceFile"), NULL,
                          multiple = FALSE,
                          width = 250,
                          buttonLabel = "Choose Protein file",
                          placeholder = "No file chosen",
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".fasta"))
            ),
            div(style="display: inline-block;vertical-align:top;  width: 150px;",
                h5(tags$b('or load and example'))
            ),
            div(style="display: inline-block;vertical-align:top;",
                selectizeInput(ns("exampleFile"), NULL, width = 250, selected = NULL, multiple = TRUE, options = list(maxItems = 1),
                               choices = list(`Benchmark Sets` = c("CASP10_minimal", "CASP10_sub30", "CASP10_sub70", "CASP10_all"),
                                              `PDB` = c("1AGJA", "1C1KA", "4PIOA"))
                )
            ),
            textAreaInput(ns("sequenceData"), label = NULL, height = 300)
        ),
        box(
            title =  tagList(
                div(style="display: inline-block;vertical-align:top; width: 50px;",
                    actionBttn(inputId= 't2', label = '2', style = "material-circle", color = "success")
                ),
                div(style="display: inline-block;vertical-align:top;",
                    h4('Algorithm parameters')
                )
            ),
            width = NULL,
            collapsible = TRUE,
            collapsed = TRUE,
            column(width = 4,
                   selectizeInput(ns("cnnModel"), "CNN model", width = NULL, selected = NULL, multiple = FALSE,
                                  choices = c('cnn-128-ker-local','cnn-2-conv-local', 'cnn-64-ker-local')),
                   textOutput(ns("selectedModelDescription"))
            ),
            column(width = 1),
            column(width = 4,
                   materialSwitch(inputId = ns("edgeCorrection"), label = "Apply 2 layer correction?", value = FALSE, status = "success")
            )
        ),
        box(
            title =  tagList(
                div(style="display: inline-block;vertical-align:top; width: 50px;",
                    actionBttn(inputId= 't3', label = '3', style = "material-circle", color = "success")
                ),
                div(style="display: inline-block;vertical-align:top;",
                    h4('Run prediction')
                )
            ),
            br(),
            div(style="display:inline-block", style="float:right",
                actionBttn(inputId = ns('predict'), label = "run shiny-pred",  style = "material-flat", color = "success", icon = icon("sliders"))
            ),
            
            width = NULL,
            collapsible = FALSE
        )
    )
}

# Module server function
inputHandling <- function(input, output, session) {
    ns <- session$ns
    
    output$selectedModelDescription <- renderText({ 
        req(input$cnnModel)
        
       if (input$cnnModel == 'cnn_128-ker-local.h5')
         paste("One layer convolutional network with 128 kernels", input$cnnModel)
    })
    
    observe({
        req(input$sequenceFile)
        sequencesRaw <- read_file(input$sequenceFile$datapath)
        updateTextAreaInput(session, "sequenceData", value = toString(sequencesRaw))
    })
    
    observeEvent(input$exampleFile, {
        req(input$exampleFile)
        sequencesRaw <- read_file(paste0('examples/', input$exampleFile, '.fasta'))
        updateTextAreaInput(session, "sequenceData", value = toString(sequencesRaw))
    })
    
    sequenceData <- reactive({
        tmp <- tempfile()
        write(input$sequenceData, tmp)
        sequencesRaw <- readBStringSet(tmp)
        
        return(sequencesRaw)
    })

    cnnModel <- reactive({
        return (paste0('models/', input$cnnModel, '.h5'))
    })
    
    edgeCorrection <- reactive({
        return (input$edgeCorrection)
    })
    
    predictButton <- reactive({
        return(input$predict)
    })
    
    return (
        list("sequenceData" = sequenceData, "predictButton" = predictButton , "cnnModel" = cnnModel, "edgeCorrection" = edgeCorrection )
    )
    
}