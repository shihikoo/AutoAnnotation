#-----load library
# install.packages("githubinstall")
# library(githubinstall)
# githubinstall("AutoAnnotation")
library(AutoAnnotation)

#-------- Set up file folders, different for different projects ----------
runTimestamp <- format(Sys.time(), "%Y%m%d%H%M%S")

datafolder <- "examples/"
dir.create(datafolder, showWarnings = F)

outputFolder <- "output/"
dir.create(outputFolder, showWarnings = F)

dataFileName <- "RExport.csv"
didctionaryName <- 'examples/Sample_Regex_Dictionary_Electrophysiology.csv'
filefolder <- ''

#-------- Read full information from the file ----------
originalData <- read.csv(paste0(datafolder, dataFileName),row.names = NULL, stringsAsFactors = F)

# #-------- some specific correction on pdf names ------------
# myData$PdfRelativePath <- paste0('S:/TRIALDEV/CAMARADES/Alexandra/For Jing/PDF', gsub('#','',myData$PdfRelativePath),sep="/")
index <- which(originalData$PdfRelativePath != "")
originalData$PdfRelativePath[index] <- paste0(filefolder, originalData$PdfRelativePath[index], sep="")

myData <- originalData[1,]

annotationResults <- CountTermsInStudies(searchingData = myData
                               , dictionary = didctionaryName
                               , linkSearchHeaders = "PdfRelevantPath"
                               )



annotationOnlyResults <- sapply(annotationResults[, -1], function(x) as.numeric(as.character(x)))

print(colSums(annotationOnlyResults))
# robRegexFlag <- RiskOfBiasIdentification(myData)

# -------- write output data -----------
outputData <- cbind(myData, annotationResults)

write.table(outputData, paste(outputFolder,  runTimestamp, "DataAnnotated.txt", sep=""), quote = F, sep = "\t")

# # ----- print confusion Matrix --------------
# cmRandomisation <- confusionMatrix(robRegexFlag$RandomisationRegex,myData$Randomisation, positive = "TRUE")
# cmBlinding <- confusionMatrix(robRegexFlag$BlindingRegex,myData$Blinding, positive = "TRUE")
# cmSSC <- confusionMatrix(robRegexFlag$SampleSizeCalculationRegex,myData$SampleSizeCalculation, positive = "TRUE")
#
# print(cmRandomisation)
# print(cmBlinding)
# print(cmSSC)
#
# # -------- write confusion Matrix -----------
# write(cmRandomisation, file = paste(outputFolder,  runTimestamp, "StatisticsResult.txt", sep=""), append = T)
# write(cmBlinding$byClass, file = paste(outputFolder,  runTimestamp, "StatisticsResult.txt", sep=""), append = T)
# write(cmSSC$byClass, file = paste(outputFolder,  runTimestamp, "StatisticsResult.txt", sep=""), append = T)
