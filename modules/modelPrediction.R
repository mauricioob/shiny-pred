predict <- function(sequencesRaw, disorderInfo = FALSE, cnnModel = 'models/cnn_model2_pbd70_test.h5') {
    tryCatch(
        {
            withProgress(message = '>_', value = 0, {
                if (disorderInfo) {
                    incProgress(3/10, detail = "Parsing disorder information")
                    sequencesData <- parseDisorderInformation(sequencesRaw, disorderInfo)
                    sequencesAA <- sequencesData$sequencesAA
                    sequencesDisorder <- sequencesData$sequencesDisorder
                    disorderData <- sequencesData$disorderAA
                } else {
                    sequencesAA <- sequencesRaw                   
                } 
                
                incProgress(1/10, detail = "Extracting windows")
                windows <- extractWindows(sequencesAA)
                windowsSequenceNames <- unlist(lapply(windows, function(x) { return(lapply(x, `[[`, 1))}))
                residuePosition <- unlist(lapply(windows, function(x) { return(lapply(x, `[[`, 2))}))
                residues <- unlist(lapply(windows, function(x) { return(lapply(x, `[[`, 3))})) 
                windows <- unlist(lapply(windows, function(x) { return(lapply(x, `[[`, 4))})) 
                
                incProgress(3/10, detail = "Encoding and translating windows")
                x_encoded <- getData3LetterTranslated(windows)
                
                incProgress(1/10, detail = "Loading prediction model")
                model <- load_model_hdf5(cnnModel)
                
                incProgress(2/10, detail = "Running predictions")
                predictProbabilities <- model %>% keras::predict_proba(x_encoded)
                predictions <-  predictProbabilities[,2] %>% as.numeric 
                
                if (disorderInfo) {
                    actualValues <- unlist(disorderData)
                } else {
                    actualValues <- rep(NA, length(predictions))
                }
            })
            
            return(list(
                    "predictions" = predictions, 
                    "predictionsWithClasses" = ifelse(predictions > 0.5, 'D', 'O'),
                    "residues" = residues,
                    "residuePosition" = residuePosition,
                    "actualValues" = actualValues,
                    "windowsSequenceNames" = windowsSequenceNames
                )
            )
        },
        error = function(e) {
            # return a safeError if a parsing error occurs
            stop(safeError(e))
        }
    )
}