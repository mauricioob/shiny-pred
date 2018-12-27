#Constants settings
aaNotFoundStart <- "#";
aaNotFoundEnd <- "#";
CHANNELS <- 4
INPUT_FEATURES <- 101

translateSequence <- function(seq) {
    strSeq <- toString(seq)
    charSeq <-  strsplit(strSeq, "")[[1]]
    translated <- lapply(charSeq,function(aa) { 
        if( aa %in% c('C','I','L','M','V','F','W','Y')) 
            return('B')
        if( aa %in% c('A','G','H','S','T')) 
            return('J')
        if( aa %in% c('E','K','D','N','R','P','Q','B','Z')) 
            return('U')
        return('X')
    })
    return(paste(unlist(translated), collapse = ''))
}

parseDisorderInformation <- function(sequences, disorderInfo) {
    seqAA <- list()
    seqDisorder <- list()
    disorderList <- list()
    names <- names(sequences)
    lapply(sequences,function(seq) {
        name <- names[length(seqAA)+1]
        seq <- AAStringSet(seq)
        names(seq) <- name
        seqAA[[length(seqAA)+1]] <<- subseq(seq, 1,width(seq)/2)
        disorderSubseq <<- subseq(seq, width(seq)/2 +1)
        seqDisorder[[length(seqDisorder)+1]] <<- disorderSubseq
        disorderList <<-append(disorderList, as.list(strsplit(toString(disorderSubseq), "")[[1]]))
    })
    
    return(list('sequencesAA' = seqAA, 'sequencesDisorder' = seqDisorder, 'disorderAA' = disorderList))
}

getDisorderFrequencies <- function(sequences, excludeTerminalDisorder = TRUE) {
    f <- list()
    
    for(seqIdx in 1:length(sequences)) {
        sequence <- sequences[[seqIdx]]
        sequence <- AAStringSet(sequence)
        seqLen <- width(sequence)
        currentDisorderLen <- 0
        isEndTerminal <- FALSE
        isStartTerminal <- FALSE
        for(i in 1:seqLen) {
            currentAA <- substring(sequence, i, i)
            
            isEndTerminal <- ifelse(i ==  seqLen, TRUE, FALSE)
            isStartTerminal <- ifelse(currentDisorderLen == (i-1) & currentDisorderLen > 0, TRUE, FALSE)
            
            if ( currentAA == 'O') { 
                if (currentDisorderLen > 0) {
                    if (isStartTerminal & excludeTerminalDisorder) {
                        #do nothing
                    } else {
                        #stores current disorder region
                        f[toString(currentDisorderLen)] <- ifelse(is.null(f[toString(currentDisorderLen)][[1]]), 1, f[toString(currentDisorderLen)][[1]] + 1)
                    }
                }
                #resets current disorder region
                currentDisorderLen <- 0
            }
            
            #increments current disorder count
            if ( currentAA == 'D') {
                currentDisorderLen <- currentDisorderLen + 1
            }
            
            #takes care of border case            
            if (!excludeTerminalDisorder & isEndTerminal) {
                if (currentDisorderLen > 0) {
                    f[toString(currentDisorderLen)] <- ifelse(is.null(f[toString(currentDisorderLen)][[1]]), 1, f[toString(currentDisorderLen)][[1]] + 1)
                }
            }
            
            #print(paste("isStartTerminal:",isStartTerminal, "isEndTerminal", isEndTerminal, "dLen:",currentDisorderLen, "i:", i, "AA:",currentAA))
        }
    }   
    return(f)
}

getDisorderBins <- function(file, excludeTerminalDisorder = FALSE) {
    ALL_SEQ <- readAAStringSet(file)
    ALL_DIS <- parseDisorderInformation(ALL_SEQ)$sequencesDisorder
    
    f <- getDisorderFrequencies(ALL_DIS, excludeTerminalDisorder = excludeTerminalDisorder)
    uf <- unlist(f)
    dff <- data.frame(names(uf),uf,stringsAsFactors = FALSE)
    names(dff) <- c('seqLength', 'frequency')
    dff <- dff %>% arrange(frequency)
    dffn <- as.data.frame(sapply(dff, as.numeric))
    
    bin1to5 <- as.data.frame(dffn %>% summarize(sum = sum(frequency[seqLength>=1 & seqLength<=5])))[[1]]
    bin6to15 <- as.data.frame(dffn %>% summarize(sum = sum(frequency[seqLength>=6 & seqLength<=15])))[[1]]
    bin16to25 <- as.data.frame(dffn %>% summarize(sum = sum(frequency[seqLength>=16 & seqLength<=25])))[[1]]
    bin25andUp <- as.data.frame(dffn %>% summarize(sum = sum(frequency[seqLength>=26])))[[1]]
    
    return(
        list('bin1to5' = bin1to5, 'bin6to15' = bin6to15, 'bin16to25' = bin16to25, 'bin25andUp' = bin25andUp, "freq" = dffn)
    )
    
}

extractWindows <- function(sequenceListAA) {
    windows <- list()
    windows <- lapply(seq_along(sequenceListAA), extractWindow, seqData=sequenceListAA, seqNames=names(sequenceListAA))
    return(windows)
}

extractWindow <- function(sequenceIdx, seqData, seqNames) {
    sequenceData <- seqData[[sequenceIdx]]
    sequenceName <- seqNames[sequenceIdx]
    
    windows <- list()  
    position <- NULL
    sequenceAA <- toString(sequenceData)
    sequenceLength <- nchar(sequenceAA)
    currentSequenceWindowSize <- WINDOW_SIZE 
    
    if (is.null(sequenceName))
        sequenceName = names(sequenceData)[1]
    
    if (sequenceLength < currentSequenceWindowSize)
        currentSequenceWindowSize = sequenceLength
    
    side <- (currentSequenceWindowSize-1) / 2
    start <- 0; end <- 0
    
    idx <- 0 
    while (idx < sequenceLength) {
        position = 'MIDDLE'
        
        relativePosition = 'M'
        if (idx < 51 & idx > 10)
            relativePosition = 'S'
        
        if ((sequenceLength-idx) < 51 & (sequenceLength-idx) > 9)
            relativePosition = 'E'
        
        
        #beginning of the window
        if (idx <= side) {
            start = 1
            end = idx + side + 1
            position = 'START'
        }
        #middle of the window
        if (idx >= side) {
            start = idx - side + 1
            end = idx + side + 1 
            position = 'MIDDLE'
        }
        #end of the window
        if ((idx+side) >= sequenceLength) {
            start = idx - side + 1
            end = nchar(sequenceAA)
            position = 'END'
        }
        
        sliderAA <- ""
        end <- ifelse(end > sequenceLength, sequenceLength, end)
        start <- ifelse(start < 0, 1, start)
        sliderAA <- substring(sequenceAA, start, end)
        
        if (nchar(sliderAA) < WINDOW_SIZE) {
            if (position == 'START') {
                #pad left
                sliderAA <- str_pad(sliderAA, WINDOW_SIZE, side =  "left", pad = aaNotFoundEnd)
            }
            else {
                #pad right
                sliderAA <- str_pad(sliderAA, WINDOW_SIZE, side =  "right", pad = aaNotFoundEnd)
            }
        }
        #adds window to the list
        windows[[length(windows) + 1]] <- c(sequenceName, idx+1, substring(sequenceAA, idx+1, idx+1), sliderAA, relativePosition)

        idx <- idx + 1
    }
    return(windows)
}

getEncodingByAlphabet <- function(alphabetNbr, aa) {
    if (aa == '#')
        return(c(0, 0, 0, 1))
    if (aa == '?')
        return(c(0, 0, 0, 0))
    if (aa == 'X')
        return(c(0, 0, 0, 0))
    if (alphabetNbr == 6) {
        if (aa %in% c('C','I','L','M','V','F','W','Y'))
            return(c(0, 0, 1, 0))
        if (aa %in% c('A','G','H','S','T'))
            return(c(0, 1, 0, 0))
        if (aa %in% c('E','K','D','N','R','P','Q','B','Z'))
            return(c(1, 0, 0, 0))
    }
}

getData3LetterTranslated <- function(windowList, alphabetNbr=6) {
    numberOfExamples <- length(windowList)
    encodedList = list()

    encodedList <- lapply(windowList, function(window) {
        encodedWinList <- list()
        windowAA <- strsplit(window, "")[[1]]
        
        encodedWinList <- lapply(windowAA, function(aa) {
            encoding = getEncodingByAlphabet(alphabetNbr, aa)
            return(encoding)
        })
        
        return(encodedWinList)
    })
    encodedList <- unlist(encodedList)

    x <- array(unlist(lapply(encodedList, unlist)))
    x <- array_reshape(x, c(numberOfExamples,CHANNELS, 1, INPUT_FEATURES))
    
    return(x)
}