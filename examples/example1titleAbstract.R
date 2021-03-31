#-----load library
# install.packages("githubinstall")
# library(githubinstall)
# githubinstall("AutoAnnotation")
# library(AutoAnnotation)
# source("R/AutoAnnotationFunctions.R")

#-------- Set up file folders, different for different projects ----------
runTimestamp <- format(Sys.time(), "%Y%m%d%H%M%S")

datafolder <- "examples/"
dir.create(datafolder, showWarnings = F)
outputFolder <- "output/"
dir.create(outputFolder, showWarnings = F)

dataFileName <- "example4studies.csv"
# didctionaryName <- 'SampleRegexDictionary.txt'
didctionaryName <- 'example4regex.csv'

#-------- Method1: Read in full information from the file, process and feed in ----------
originalData <- read.csv(paste0(datafolder, dataFileName),row.names = NULL, stringsAsFactors = F)
myData <- originalData
annotationResults <- CountTermsInStudies(searchingData = paste0(datafolder, dataFileName)
                                         , dictionary = paste0(datafolder, didctionaryName)
                                         , textSearchingHeaders <- c("Title","Abstract")
                                         , ignoreCase = T
                                         )

# -------- Method2: Directly feed in the file -----
# --- Counter the terms in the dictionary
# annotationResults <- CountTermsInStudies(searchingData = paste0(datafolder, dataFileName)
#                                , dictionary = didctionaryName
#                                , textSearchingHeaders <- c("Title","Abstract"))

annotationOnlyResults <- as.data.frame(lapply(annotationResults[, -1],function(x) as.numeric(as.character(x))))

print(colSums(annotationOnlyResults))

# -------- write output data -----------
outputData <- cbind(myData, annotationResults)

write.table(outputData, paste(outputFolder,  runTimestamp, "DataAnnotated.txt", sep=""), quote = F, sep = "\t", row.names = F)

