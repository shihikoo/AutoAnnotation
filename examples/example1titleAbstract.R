#-----load library
# install.packages("githubinstall")
# library(githubinstall)
# githubinstall("AutoAnnotation")
library(AutoAnnotation)
# source("R/AutoAnnotationFunctions.R")

#-------- Set up file folders, different for different projects ----------
runTimestamp <- format(Sys.time(), "%Y%m%d%H%M%S")

datafolder <- "examples/"
dir.create(datafolder, showWarnings = F)
outputFolder <- "output/"
dir.create(outputFolder, showWarnings = F)

dataFileName <- "example1titleAbstract.txt"
didctionaryName <- 'examples/SampleRegexDictionary.txt'

#-------- Read full information from the file ----------
originalData <- read.delim(paste0(datafolder, dataFileName),row.names = NULL, stringsAsFactors = F)
myData <- originalData

#---- Edits for data ----

# -------- Counter the terms in the dictionary  ------------
annotationResults <- CountTermsInStudies(searchingData = myData
                               , dictionary = didctionaryName
                               , textSearchingHeaders <- c("Title","Abstract"))

annotationOnlyResults <- as.data.frame(lapply(annotationResults[, -1],function(x) as.numeric(as.character(x))))

print(colSums(annotationOnlyResults))

# -------- write output data -----------
outputData <- cbind(myData, annotationResults)

write.table(outputData, paste(outputFolder,  runTimestamp, "DataAnnotated.txt", sep=""), quote = F, sep = "\t")
