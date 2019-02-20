encode <- function(sequencesRaw, disorderInfo = FALSE) {

    if (disorderInfo) {
        sequencesData <- parseDisorderInformation(sequencesRaw, disorderInfo)
        sequencesAA <- sequencesData$sequencesAA
        sequencesDisorder <- sequencesData$sequencesDisorder
        disorderData <- sequencesData$disorderAA
    } else {
        sequencesAA <- sequencesRaw                   
    } 
    
    windows <- extractWindows(sequencesAA)
    windowsSequenceNames <- unlist(lapply(windows, function(x) { return(lapply(x, `[[`, 1))}))
    residuePosition <- unlist(lapply(windows, function(x) { return(lapply(x, `[[`, 2))}))
    residues <- unlist(lapply(windows, function(x) { return(lapply(x, `[[`, 3))})) 
    relativePosition <- unlist(lapply(windows, function(x) { return(lapply(x, `[[`, 5))})) 
    windows <- unlist(lapply(windows, function(x) { return(lapply(x, `[[`, 4))})) 
    
    x_encoded <- getData3LetterTranslated_flat(windows)

    return(x_encoded)
    
}

predict <- function(sequencesRaw, disorderInfo = FALSE, cnnModel = 'models/cnn-128-ker-local.h5', edgeCorrection = FALSE) {
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
                relativePosition <- unlist(lapply(windows, function(x) { return(lapply(x, `[[`, 5))})) 
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
                
                residuePosition <- as.numeric(residuePosition)
                startPositions <- which(residuePosition %in% c(1:20))

                if (edgeCorrection) {
                    edges <-  which(relativePosition %in% c('S','E'))
                    predictions[edges] <-  (predictions[edges] / 2) + 0.1                    
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