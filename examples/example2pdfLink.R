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

dataFileName <- "example2pdfLink.csv"
didctionaryName <- 'examples/SampleRegexDictionary.txt'

#-------- Read full information from the file ----------
originalData <- read.csv(paste0(datafolder, dataFileName),row.names = NULL, stringsAsFactors = F)
myData <- originalData

#---- Edits for data ----

# -------- Count the terms in the pdf in the dictionary  ------------
annotationResults <- CountTermsInStudies(searchingData = myData
                               , dictionary = didctionaryName
                               , linkSearchHeaders = "PdfRelativePath")

annotationOnlyResults <- as.data.frame(lapply(annotationResults[, -1],function(x) as.numeric(as.character(x))))

print(colSums(annotationOnlyResults))

# -------- OR Count the terms in the pdf in the dictionary and cut introduction and references out of PDF ------------
annotationResults <- CountTermsInStudies(searchingData = myData
                                         , dictionary = didctionaryName
                                         , linkSearchHeaders = "PdfRelativePath"
                                         , cutIntro = T
                                         , cutRefs = T)

annotationOnlyResults <- as.data.frame(lapply(annotationResults[, -1],function(x) as.numeric(as.character(x))))

print(colSums(annotationOnlyResults))

# -------- OR Count the terms in the pdf in the dictionary and get the matching text strings ------------
annotationResults <- CountTermsInStudies(searchingData = myData
                                         , dictionary = didctionaryName
                                         , linkSearchHeaders = "PdfRelativePath"
                                         , extractStrings = T
                                         )


annotationOnlyResults <- as.data.frame(lapply(annotationResults[, -1],function(x) as.numeric(as.character(x))))

print(colSums(annotationOnlyResults))


# -------- write output data -----------
outputData <- cbind(myData, annotationResults)

write.table(outputData, paste(outputFolder,  runTimestamp, "DataAnnotated.txt", sep=""), quote = F, sep = "\t")
