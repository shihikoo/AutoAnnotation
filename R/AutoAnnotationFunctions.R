# Functions for identifying text appreance frequency or appearences in different combination of fields.

#   http://r-pkgs.had.co.nz/
#
#   Build and Reload Package:             'Ctrl + Shift + B'
#   Check Package:                        'Ctrl + Shift + E'
#   Test Package:                         'Ctrl + Shift + T'
#   Create Roxygen documentation in code: 'Ctrl + Shift + Alt + R'

#load libraries
#' loadLibraries
#'

loadLibraries <- function()
{
  library(tools)
  library(foreach)
  library(doParallel)
}

#load libraries
#' setParSettings
#'

setParSettings <- function()
{
  #Count the pattern in each studies parallaly
  ncores <- detectCores(all.tests = FALSE, logical = TRUE)
  cl <-
    makeCluster(round(ncores) / 2, outfile = "") #determines how many parallel processes are used for the pdf downloading
  registerDoParallel(cl)
}

#' GetData
#'
#' The function will first judge wether the input data is a dataset (matrix, data.frame, list, array) or a link.
#' For a dataset, it converts the input data to a data.frame.
#' For a link, it checks the type of the file and read accordingly. Acceptable extensions include 'txt', 'tsv', 'csv'
#'
#' @param data  Either a dataset, or a link to the dataset
#'
#' @return Data in the format of data frame or NULL for anything illegal
#' @export
#'

GetData <- function(myData) {
  tryCatch({
    if (class(myData) == 'matrix' |
        class(myData) == 'data.frame' |
        class(myData) == 'list' | class(myData) == 'array') {
      df = as.data.frame(myData)
    } else if (class(myData) == 'character') {
      require(tools)
      fileExtension <- file_ext(myData)
      if (fileExtension == 'txt' |
          fileExtension == 'tsv')
        df <- read.delim(myData, stringsAsFactors = F)
      else if (fileExtension == 'csv')
        df <- read.csv(myData, stringsAsFactors = F)
      # else if(fileExtension == 'xlsx' | fileExtension == 'xls')
      #   {
      #   df <- read.xlsx(myData, 1)
      #
      #   return(df)
      # }
      else
      {
        print(paste0(myData, "File type not recognized. "))
        return(NULL)
      }
    } else {
      return(NULL)
    }
  }, warning = function(war) {
    # warning handler picks up where error was generated
    print(paste("MY_WARNING:  ", war))
    return(NULL)

  }, error = function(err) {
    # error handler picks up where error was generated
    print(paste("MY_ERROR:  ", err))
    return(NULL)

  }, finally = {

  })
}

#' ExtractColumns
#'
#' Extract the columns in the df matching the
#'
#' @param df a data frame
#' @param columnNames a list of character object
#'
#' @return a data frame with column names matching columnNames. If not mathing is found, NULL is returned.
#' @export
#'
ExtractColumns <- function(df, columnNames) {
  if (!class(columnNames) == 'character' &
      !class(columnNames[1]) == 'character')
    return(NULL)
  if (sum(columnNames %in% names(df)) >= 1)
  {
    newDf <- df[columnNames[columnNames %in% names(df)]]
    print(paste0('Valid column: ', names(newDf)))
    return(newDf)
  } else {
    return(NULL)
  }
}

#' ValidateDictionary
#'
#' Validate Dictionary
#'
#' @param myDictionary  my Dictionary
#' @param dictionaryNameHeader dictionary Name Header
#' @param dictionaryRegexHeader dictionary Regex Header
#'
#' @return valid myDictionary

ValidateDictionary <-
  function(myDictionary,
           dictionaryNameHeader,
           dictionaryRegexHeader) {
    if (length(names(myDictionary)) == 0)
      return(NULL)
    if (length(names(myDictionary)) == 1) {
      myDictionary <- cbind(myDictionary, myDictionary)
      names(myDictionary) <-
        c(dictionaryNameHeader, dictionaryRegexHeader)
      return(myDictionary)
    }

    if (dictionaryNameHeader %in% names(myDictionary) &
        dictionaryRegexHeader %in% names(myDictionary))
      return(myDictionary[, c(dictionaryNameHeader, dictionaryRegexHeader)])
    else {
      print(
        "Dictionary headers don't match. Plesae provide the correct header. Either indicate the correct
        header for dictionaryNameHeader and dictionaryRegexHeader,
        or use 'Name' and 'Regex' in the dictionary document and omit the parameters"
      )
      return(NULL)
    }

  }

#' ReadLink
#'
#' Read Link
#'
#' @param link
#'
#' @return  linkstatus and fulltext

ReadLink <- function(link){
  if(tolower(file_ext(link)) == 'pdf'){
    linkStatus <- ConvertPdftoText(link, ignoreExistingTextFile = FALSE)
    textLink <- ifelse(grepl('OK', linkStatus), gsub('pdf','txt', link), '')
  }
  else {
    if(link == "") linkStatus <- 'Error: No file' else linkStatus <- 'Ok: Not pdf'
    textLink <- link
    }

  if(textLink == '') return(c(linkStatus,''))
  if(!file.exists(textLink))
  {
    linkStatus <- "Error: Text file not found"
    return(c(linkStatus,''))
  }

  try(
    {
      fullText <- ReadFullText(textLink)
      linkStatus <- 'OK: File is read Successfully'
      return(c(linkStatus, fullText))
    }
  )

  linkStatus <- "Error: Failed to read file"
  return(c(linkStatus,''))
}

#' ConvertPdftoText
#'
#'  Convert pdf to text
#'
#' @param pdfLink pdfLink
#' @param ignoreExistingTextFile ignore ExistingTextFile
#' @param convertSoftware convertSoftware
#'
#' @return status

ConvertPdftoText <- function(pdfLink, ignoreExistingTextFile = FALSE, convertSoftware = ''){
 try({
  if(!file.exists(pdfLink)) return('Error: Pdf not found')

  txtLink <- sub('.pdf', '.txt', pdfLink)
  if (file.exists(txtLink) && ignoreExistingTextFile == FALSE) return('OK: Text file exists')

  if(convertSoftware == '') {
  if(Sys.info()['sysname'] == 'Linux') convertSoftware = '"pdftotext"'
  else if(Sys.info()['sysname'] == 'Windows') convertSoftware = '"packrat/pdftotext.exe"'
  else if(Sys.info()['sysname'] == 'Windows') convertSoftware = '"pdftotext"'
  }
  com <- paste(convertSoftware, paste('"', pdfLink, '"', sep = ''))

  try(
    {system(com, wait = T)
      return("OK: Pdf Converted")}
  )}, TRUE
 )
  return("Error: Fail to convert pdf")
}

#' ReadFullText
#'
#' # Read full text
#'
#' @param txtFileName
#'
#' @return full text

ReadFullText <- function(txtFileName) {

  # Read regular expression from file names
  ReadText <- function(fileName){
    readChar(fileName, file.info(fileName)$size)
  }

  if(!file.exists(txtFileName)) return(NULL)

  #Read text from file into fulltext
  fulltext <- sapply(txtFileName , ReadText)

  return(CleanText(fulltext))
}

# Clean up text from pdfExtractor new lines, dashlines, and lower text
#' CleanText
#'
#' @param text text
#' @param pdfExtractor pdfExtractor
#' @param newLine newLine
#' @param dashLine dashLine
#' @param lowerCase lowerCase
#'
#' @return CleanText

CleanText <- function(text, pdfExtractor = F, newLine = T, dashLine = F, lowerCase = F) {
  if (pdfExtractor == T)
    text <-
      gsub("[(][a-zA-Z0-9. ]*PDF Extractor SDK[a-zA-Z0-9 .]*[)]",
           "",
           text,
           perl = T)

  if (newLine == T)
    text <- gsub("\r|\n|\f", " ", text, perl = T)

  if (dashLine == T)
    text <- gsub("-", "", text, perl = T)

  if (lowerCase == T)
    text <- tolower(text)

  return(text)
}

#' CountPattern
#'
#' The function count any pattern, regular expression acceptable, in text. The function uses perl
#'
#' @param text Character.
#' @param pattern Character.
#' @param ignoreCase Boolean.
#' @return number of pattern found in text
#'

CountPattern <- function(text, pattern, ignoreCase = T) {
  locations <-
    gregexpr(pattern, text, ignore.case = ignoreCase, perl = T)
  if (is.null(locations[[1]]) | locations[[1]][1] == -1)
    return(0)
  return (length(locations[[1]]))
}

#' CountPatternOverMatrix
#'
#' Count Pattern Over Matrix
#'
#' @param fullText  A list, arry or data frame of character vectors where matches are sought, or a list or array of objects which can be coerced by as.character to a character vector. Long vectors are supported.
#' @param pattern  Character string containing a regular expression to be matched in the given character vector.
#' Coerced by as.character to a character string if possible.
#' If a character vector of length 2 or more is supplied, the first element is used with a warning. Missing values are allowed.
#' If the data frame contains more than one column, then the only first column is used.
#' @param ignoreCase Boolean
#'
#' @return a list of integers showing the number frequency of the pattern match over columns and over rows
#'
CountPatternOverMatrix <-
  function(text, pattern, margin = margin, ignoreCase = ignoreCase) {
    return(apply(
      text,
      margin,
      CountPattern,
      pattern = pattern,
      ignoreCase = ignoreCase
    ))
  }

#' CountPatternInPar
#'
#' Count Pattern In Par
#'
#' @param myStudies myStudies
#' @param myDictionary myDictionary
#' @param searchingHeaders searchingHeaders
#' @param textSearchingHeaders textSearchingHeaders
#' @param linkSearchHeaders linkSearchHeaders
#' @param dictionaryNameHeader dictionaryNameHeader
#' @param dictionaryRegexHeader dictionaryRegexHeader
#' @param ingorneBrokenLinks ingorneBrokenLinks
#' @param ignoreCase ignoreCase
#'
#' @return frequency

CountPatternInPar <- function(myStudies = myStudies
                              ,
                              myDictionary = myDictionary
                              ,
                              searchingHeaders = searchingHeaders
                              ,
                              textSearchingHeaders = textSearchingHeaders
                              ,
                              linkSearchHeaders = linkSearchHeaders
                              ,
                              dictionaryNameHeader = dictionaryNameHeader
                              ,
                              dictionaryRegexHeader = dictionaryRegexHeader
                              ,
                              ingorneBrokenLinks = ingorneBrokenLinks
                              ,
                              ignoreCase = ignoreCase) {
  linkStatusHeader <-
    paste0(linkSearchHeaders, "Status")
  linkFullTextHeader <-
    paste0(linkSearchHeaders, "FullText")

  results <- foreach(i = 1:nrow(myStudies)) %dopar% {
    source('R/AutoAnnotationFunctions.R')
    loadLibraries()
    options(stringsAsFactors = F)

    myStudy <- myStudies[i, ]

    # Read fulltext. Convert if the link is a pdf link. Return Status and fulltext
    myStudy[, c(linkStatusHeader,linkFullTextHeader)] <-
      as.list(t(sapply(myStudy[, linkSearchHeaders], ReadLink)))

    myRegex <- myDictionary[, dictionaryRegexHeader]
    result<- sapply(myRegex,
           CountPatternOverMatrix,
           margin = 1,
           text = myStudy[, !(names(myStudy) %in% c(linkSearchHeaders, linkStatusHeader))],
           ignoreCase = ignoreCase)

    return(c(myStudy[,linkStatusHeader], result))
  }

  results <- as.data.frame(t(as.matrix(as.data.frame(results))))
  colnames(results) <- c(linkStatusHeader, myDictionary[, dictionaryNameHeader])
  rownames(results) <- NULL

  return(results)
}

#' CountTermsInStudies
#'
#'CountTermsInStudies
#'
#' @param searchingData Either a dataset or a link to the dataset to search from
#' @param dictionary Either a dictionary dataset, or a link to the dictionary dataset to run the function on.
#'  It should consist two columns: name of the term and search string of the term. Regular expression (Perl) is accepted for the search string.
#'  If there is only one column, that column will be used both as name and regular expression.
#'
#' @param textSearchingHeaders A list of the headers of the columns to search from. A list of character. Default value is c('Title', 'Abstract', 'FullText')
#' @param pdfLinkSearchHeaders A list of the headers of the columns to convert, read and search from. A list of character. Default value is c('Title', 'Abstract', 'FullText')
#' @param textLinkSearchHeaders A list of the headers of the columns to read and search from. A list of character. Default value is c('Title', 'Abstract', 'FullText')
#'
#' @param dictionaryNameHeader The header string of name column in dictionary
#' @param dictionaryRegexHeader The header string of regular expression column in dictionary
#'
#' @return A data frame with result of the dictionary search. One column for each term in the dictionary, with the name of the term as header.
#' @export
#'
CountTermsInStudies <- function(searchingData = NULL
                                ,
                                dictionary = NULL
                                ,
                                searchingHeaders = c('Title', 'Abstract')
                                ,
                                textSearchingHeaders = c('Title', 'Abstract')
                                ,
                                linkSearchHeaders = c('PdfRelativePath')
                                ,
                                dictionaryNameHeader = 'Name'
                                ,
                                dictionaryRegexHeader = 'Regex'
                                ,
                                ignoreCase = 'T'
                                ) {
  loadLibraries()
  setParSettings()
  # options(stringsAsFactors = F)
  #Read in the data.
  myStudies <-
    ExtractColumns(GetData(searchingData),
                   c(textSearchingHeaders, linkSearchHeaders))
  if (is.null(myStudies))
    return(NULL)

  #Read in the dictionary.
  myDictionary <-
    ValidateDictionary(myDictionary <-
                         GetData(dictionary),
                       dictionaryNameHeader,
                       dictionaryRegexHeader)
  if (is.null(myDictionary))
    return(NULL)

 results <-
    # as.data.frame(
    CountPatternInPar(
      myStudies = myStudies
      ,
      myDictionary = myDictionary
      ,
      searchingHeaders = searchingHeaders
      ,
      textSearchingHeaders = textSearchingHeaders
      ,
      linkSearchHeaders = linkSearchHeaders
      ,
      dictionaryNameHeader = dictionaryNameHeader
      ,
      dictionaryRegexHeader = dictionaryRegexHeader
      ,
      ignoreCase = ignoreCase
    )

  return(results)
}

#' IdentifyTermsInStudies
#'
#' IdentifyTermsInStudies
#'
#' @param searchingData Either a dataset or a link to the dataset to search from
#' @param dictionary Either a dictionary dataset, or a link to the dictionary dataset to run the function on.
#'  It should consist two columns: name of the term and search string of the term. Regular expression (Perl) is accepted for the search string.
#'  If there is only one column, that column will be used both as name and regular expression.
#' @param searchingHeaders A list of the headers of the columns to search from. A list of character. Default value is c('Title', 'Abstract', 'FullText')
#' @param dictionaryNameHeader The header string of name column in dictionary
#' @param dictionaryRegexHeader The header string of regular expression column in dictionary
#'
#' @return A data frame with result of the dictionary search. One column for each term in the dictionary, with the name of the term as header.
#' @export
#'
IdentifyTermsInStudies <- function(searchingData = NULL
                                   ,
                                   dictionary = NULL
                                   ,
                                   searchingHeaders = c('Title', 'Abstract')
                                   ,
                                   textSearchingHeaders = c('Title', 'Abstract')
                                   ,
                                   pdfLinkSearchHeaders = c('')
                                   ,
                                   textLinkSearchHeaders = c('')
                                   ,
                                   dictionaryNameHeader = 'Name'
                                   ,
                                   dictionaryRegexHeader = 'Regex') {
  results <-
    CountTermsInStudies(searchingData = searchingData
                           ,
                           dictionary = dictionary
                           ,
                           searchingHeaders = searchingHeaders
                           ,
                           textSearchingHeaders = textSearchingHeaders
                           ,
                           linkSearchHeaders = linkSearchHeaders
                           ,
                           dictionaryNameHeader = dictionaryNameHeader
                           ,
                           dictionaryRegexHeader = dictionaryRegexHeader
                           ,
                           ignoreCase = ignoreCase)

  return(results != 0)
}

#' RiskOfBiasIdentification
#'
#' # Identify Risk of Bias in myData$cleanText
#'
#' @param searchingData
#'
#' @return result flags

RiskOfBiasIdentification <- function(searchingData) {
  results <-
    IdentifyTermsInStudies(searchingData = searchingData, dictionary = 'extra/ROBRegularExpression.txt')

  return(results)
}
