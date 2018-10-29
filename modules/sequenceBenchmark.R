# Module UI function
sequenceBenchmarkUI <- function(id) {
    ns <- NS(id)
    
    tagList(
        tabBox(
            width = NULL,
            selected = 'Benchmarking',
            id = "benchmark",
            tabPanel("Benchmarking",
                     box(
                         title = "",
                         width = NULL,
                         solidHeader = TRUE,
                         collapsible = FALSE,   
                         box(
                             title = "ROC Curve",
                             width = 6,
                             status = "success",
                             collapsible = FALSE,
                             solidHeader = FALSE,
                             plotOutput(ns("rocOut"), height = 700, width = 700)
                         ),
                         box(
                             title = "Performance summary",
                             width = 6,
                             status = "success",
                             solidHeader = TRUE,
                             collapsible = FALSE,
                             uiOutput(ns("performanceSummary"))
                         )
                     )
            )
        )
    )
}

# Module server function
sequenceBenchmark <- function(input, output, session, disorderPredictions, confusionMatrix) {
    
    
    rocData <- reactive({
        req(disorderPredictions)
        ROC <- roc(disorderPredictions$actualValues, disorderPredictions$predictions, levels=c("O", "D"))
        return(ROC)
    })
    
    output$performancePlot <- renderPlot(
        performanceData()  
    )
    
    output$rocOut <- renderPlot(
        rocobj <- plot.roc(disorderPredictions$actualValues, disorderPredictions$predictions, levels=c("O", "D"),
                           
                           main="", percent=TRUE,
                           
                           ci=TRUE, # compute AUC (of AUC by default)
                           
                           print.auc=TRUE)
    )
    
    output$performanceSummary <- renderUI({
        tp <- as.numeric(confusionMatrix$table[1])
        fp <- as.numeric(confusionMatrix$table[2])
        fn <- as.numeric(confusionMatrix$table[3])
        tn <- as.numeric(confusionMatrix$table[4])
        sensitivity <- confusionMatrix[["byClass"]][["Sensitivity"]]
        balAcc <- confusionMatrix[["byClass"]][["Balanced Accuracy"]]
        specificity <- confusionMatrix[["byClass"]][["Specificity"]]
        MCC <- (tp*tn - fp*fn) / sqrt((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn))
        tagList(
            span(style = "font-size: 10px",
                 HTML("<b>AUC:</b>", round(rocData()[["auc"]][1],2)),
                 HTML("<br><b>Sensitivity:</b>", round(sensitivity,2)),
                 HTML("<br><b>Specificity:</b>", round(specificity,2)),
                 HTML("<br><b>Balance accuracy:</b>", round(balAcc,2)),
                 HTML("<br><b>Matthews Correlation Coefficient:</b>", round(MCC,2))
            )
        )
    })
    
    predictionData <- reactive({
        req(disorderPredictions$data()$actualValues)
        
        ROC <- roc(disorderPredictions$actualValues, disorderPredictions$predictions, levels=c("O", "D"))
        
        return(ROC)
    })
    
    output$rocCurve <- renderPlotly({
        req(disorderPredictions$actualValues)
        plot_ly(x = 1- rocData()$specificities, y= rocData()$sensitivities, type = 'scatter', mode = 'lines')
    })
}