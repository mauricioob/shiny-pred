# Module UI function
sequencePlotUI <- function(id) {
    ns <- NS(id)
    
    tagList(
        tabBox(
            width = NULL,
            selected = 'Predictions',
            id = "results",
            tabPanel("Predictions",
                     downloadButton(ns("downloadData"), "Download results"),
                     br(),
                     uiOutput(ns("predictionPlots"))
            )
        )
    )
}

# Module server function
sequencePlot <- function(input, output, session, disorderPredictions, hasDisorderInfo) {
    
    ns <- session$ns
    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("cnnAlphaPredictions.csv", sep = "")
        },
        content = function(file) {
            seqPositions <- as.integer(disorderPredictions$residuePosition) - 1
            residueId <- paste(disorderPredictions$windowsSequenceNames, seqPositions, sep = '_')
            predictionsTypeD <- disorderPredictions$predictions
            predictionsTypeO <- 1 - predictionsTypeD
            predictionClass <- ifelse(predictionsTypeD > 0.5, "D", "O")
            write.csv(cbind(residueId, Prediction=predictionClass, 'P (type=D)'=predictionsTypeD, 'P (type=O)'=predictionsTypeO), file, row.names = FALSE)
        }
    )
    
    observe({
        req(disorderPredictions)
        
        output$predictionPlots <- renderUI({
            p <- disorderPredictions
            data <- cbind(p$residuePosition, p$predictions, p$windowsSequenceNames, p$residues, p$actualValues) %>% as.data.frame(stringsAsFactors=FALSE)
            names(data) <- c('position', 'prediction', 'sequenceName', 'residue', 'actualDisorder')
            data <- transform(data, prediction = as.numeric(prediction))
            data <- transform(data, position = as.numeric(position))
            
            # find all the cut off (0.5) crossings
            pred <- data$prediction
            lagPred <- c(pred[1],pred[1:(length(pred)-1)])
            crs <- sign(pred-0.5)!=sign(lagPred-0.5)
            data$crossings <- crs
            
            # insert a cut off row where there is a crossing
            insertRow <- function(df,i,value){ 
                n <- nrow(df)
                ndf1 <- df[1:i,] # note these overlap by 1
                ndf2 <- df[i:n,] # that is the row we insert
                ndf1$prediction[i] <- value
                ndf <- rbind(ndf1,ndf2)
            }
            
            i <- 1
            while(i<nrow(data)){
                if (data$crossings[i]){
                    data <- insertRow(data,i, 0.5)
                    i <- i+1
                }
                i <- i+1
            }
            
            data$order <- ifelse(data$prediction <= 0.5, data$prediction, NA)
            data$disorder <- ifelse(data$prediction >= 0.5, data$prediction, NA)
            
            sequenceList <- unique(data[,3])

            plot_output_list <- lapply(1:length(sequenceList), function(i) {
                sequence <- toString(sequenceList[i])
                plotData <- data %>% filter(sequenceName == sequence)
                tableData <- data %>% filter(sequenceName == sequence) %>% filter(!(order == 0.5 & disorder == 0.5)) %>% mutate(predictedClass = ifelse(prediction >=0.5, 'disorder', 'order'), actualClass = ifelse(actualDisorder == 'O', 'order', 'disorder'), match = ifelse(predictedClass == actualClass, 'hit', 'miss'))
                
                if (hasDisorderInfo)
                    tableData <-  tableData %>% select(position, residue, "actual class" = actualClass, predictedClass, "disorder probability" = prediction, match)
                else
                    tableData <-  tableData %>% select(position, residue, predictedClass, "disorder probability" = prediction)
                
                
                createSequenceTags <- lapply(1:nrow(tableData), function(i) {
                    residue <-  tableData$residue[i]
                    residueStyle <- ""
                    if (plotData$prediction[i] >= 0.5)
                        residueStyle = "color: red;"
                    
                    tags$span(style = residueStyle, residue)
                })
                
                tagList(
                    box(
                        title = sequence,
                        width = NULL,
                        collapsible = TRUE,
                        renderPlotly({
                            p <- plot_ly(plotData) %>%
                                add_trace(x = ~position, y = ~order,  fill = 'tozeroy',
                                          type = 'scatter', mode = 'lines', hoverinfo = 'text', connectgaps = FALSE,
                                          line = list(color = '#28b78d'),
                                          name = 'Order',
                                          text = ~paste('</br> Residue: ', residue,
                                                        '</br> Position: ', position,
                                                        '</br> Pred: ', prediction,
                                                        '</br> ActualValue: ', actualDisorder)
                                ) %>%
                                add_trace(x = ~position, y = ~disorder, fill = 'none',
                                          type = 'scatter', mode = 'lines', hoverinfo = 'text', connectgaps = FALSE,
                                          line = list(color = '#ff704d'),
                                          name = 'Disorder',
                                          text = ~paste('</br> Residue: ', residue,
                                                        '</br> Position: ', position,
                                                        '</br> Pred: ', prediction,
                                                        '</br> ActualValue: ', actualDisorder) 
                                ) %>%
                                layout(title = NULL, showlegend = FALSE,
                                       xaxis = list(
                                           #type = 'category',
                                           #title = 'Code'
                                       ))
                            p
                        }),
                        box(
                            width = NULL,
                            collapsible = TRUE,
                            collapsed = FALSE,
                            solidHeader = TRUE,
                            title = tagList(icon('receipt'), ""),
                            tagList(
                                tags$div(style = "background-color: #eff0f1; font-family: 'monospace'; font-size: 10px; color:#b3acac; overflow: auto; width: 100%; word-wrap: break-word",
                                    tags$span(paste0(">", sequence)),
                                    tags$br(),
                                    do.call(tagList, createSequenceTags)
                                )
                            )
                        ),
                        box(
                            width = NULL,
                            collapsible = TRUE,
                            collapsed = FALSE,
                            solidHeader = TRUE,
                            title = tagList(icon('table'), ""),
                            renderDataTable(
                                tableData,
                                options = list(
                                    pageLength = 5,
                                    searching = FALSE,
                                    lengthChange = TRUE
                                )
                            )
                        )
                    )
                )
            })
            do.call(tagList, plot_output_list)
        }) 
    })
}