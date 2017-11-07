# Functions for identifying text appreance frequency or appearences in different combination of fields.

#' GetData
#'
#' The function will first judge wether the input data is a dataset (matrix, data.frame, list, array) or a link.
#' For a dataset, it converts the input data to a data.frame.
#' For a link, it checks the type of the file and read accordingly. Acceptable extensions include 'txt', 'tsv', 'csv'
#'
#' @param myData  Either a dataset, or a link to the dataset
#'
#' @return Data in the format of data frame or NULL for anything illegal
#'
#' @export
#'
GetData <- function(myData) {
  tryCatch({
    if (class(myData) == 'matrix' |
        class(myData) == 'data.frame' |
        class(myData) == 'list' | class(myData) == 'array') {
      df = as.data.frame(myData)
    } else if (class(myData) == 'character') {
      fileExtension <- tools::file_ext(myData)
      if (fileExtension == 'txt' |
          fileExtension == 'tsv')
        df <- utils::read.delim(myData, stringsAsFactors = F)
      else if (fileExtension == 'csv')
        df <- utils::read.csv(myData, stringsAsFactors = F)
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
#'
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
#'
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
#' @param link read full text from link
#' @param ignoreExistingTextFile boolean varible to indicate whether a pdf should be converted
#' if there is already text file with its file name.
#' @param conversionSoftware indicate the location and name of the software to use to convert pdf to text.
#' Default value is pdftotext
#' @return  linkstatus and fulltext
#'
#' @export
#'
ReadLink <-
  function(link,
           ignoreExistingTextFile = FALSE,
           conversionSoftware = '') {
    ConvertPdftoText <-
      function(pdfLink,
               ignoreExistingTextFile = FALSE,
               conversionSoftware = '') {
        try({
          if (!file.exists(pdfLink))
            return('Error: Pdf not found')

          txtLink <- sub('.pdf', '.txt', pdfLink)
          if (file.exists(txtLink) &&
              ignoreExistingTextFile == FALSE)
            return('OK: Text file exists')

          if (conversionSoftware == '') {
            if (Sys.info()['sysname'] == 'Linux')
              conversionSoftware = 'pdftotext'
            else if (Sys.info()['sysname'] == 'Windows')
              conversionSoftware = 'pdftotext'
            else if (Sys.info()['sysname'] == 'Mac')
              conversionSoftware = 'pdftotext'
            else
              conversionSoftware = 'pdftotext'
          }

          com <-
            paste(paste('"', conversionSoftware, '"', sep = '') ,
                  paste('"', pdfLink, '"', sep = ''))

          try({
            system(com, wait = T)
            return("OK: Pdf Converted")
          })
        }, TRUE)
        return("Error: Fail to convert pdf")
      }

    ReadFullText <- function(txtFileNames) {
      # Read regular expression from file names
      ReadText <- function(fileName) {
        readChar(fileName, file.info(fileName)$size)
      }

      try(if (!file.exists(txtFileNames))
        return(NULL))
      #Read text from file into fulltext
      fulltext <- sapply(txtFileNames , ReadText)

      return(CleanText(fulltext))
    }

    CleanText <-
      function(text,
               pdfExtractor = F,
               newLine = T,
               dashLine = F,
               lowerCase = F) {
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


    if (tolower(tools::file_ext(link)) == 'pdf') {
      linkStatus <-
        ConvertPdftoText(link,
                         ignoreExistingTextFile = ignoreExistingTextFile
                         ,
                         conversionSoftware = conversionSoftware)
      textLink <-
        ifelse(grepl('OK', linkStatus), gsub('pdf', 'txt', link), '')
    }
    else {
      if (link == "")
        linkStatus <- 'Error: No file'
      else
        linkStatus <- 'Ok: Not pdf'
      textLink <- link
    }

    if (textLink == '')
      return(c(linkStatus, ''))

    try(if (!file.exists(textLink)) {
      linkStatus <- "Error: Text file not found"
      return(c(linkStatus, ''))
    })

    try({
      fullText <- ReadFullText(textLink)
      linkStatus <- 'OK: File is read Successfully'
      return(c(linkStatus, fullText))
    })

    linkStatus <- "Error: Failed to read file"
    return(c(linkStatus, ''))
  }


#' CountPatternOverMatrix
#'
#' Count Pattern Over Matrix
#'
#' @param text  A list, arry or data frame of character vectors where matches are sought, or a list or array of objects which can be coerced by as.character to a character vector. Long vectors are supported.
#' @param pattern  Character string containing a regular expression to be matched in the given character vector.
#' Coerced by as.character to a character string if possible.
#' If a character vector of length 2 or more is supplied, the first element is used with a warning. Missing values are allowed.
#' If the data frame contains more than one column, then the only first column is used.
#' @param ignoreCase Boolean
#' @param margin a vector giving the subscripts which the CountPattern function will be applied over. E.g., for a matrix 1 indicates rows, 2 indicates columns, c(1, 2) indicates rows and columns. Where X has named dimnames, it can be a character vector selecting dimension names.
#'
#' @return a list of integers showing the number frequency of the pattern match over columns and over rows
#'
CountPatternOverMatrix <-
  function(text,
           pattern,
           margin = margin,
           ignoreCase = ignoreCase) {
    CountPattern <- function(text, pattern, ignoreCase = T) {
      locations <-
        gregexpr(pattern, text, ignore.case = ignoreCase, perl = T)
      if (is.null(locations[[1]]) | locations[[1]][1] == -1)
        return(0)
      return (sum(sapply(locations,length)))
    }


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
#' @param textSearchingHeaders textSearchingHeaders
#' @param linkSearchHeaders linkSearchHeaders
#' @param dictionaryNameHeader dictionaryNameHeader
#' @param dictionaryRegexHeader dictionaryRegexHeader
#' @param ignoreCase ignoreCase
#' @param ignoreExistingTextFile ignoreExistingTextFile
#' @param conversionSoftware Software used to covert pdf to text. Default value is 'pdftotext'
#'
#' @return frequency
#'
CountPatternInPar <- function(myStudies = NULL
                              ,
                              myDictionary
                              ,
                              textSearchingHeaders = c('Title', 'Abstract')
                              ,
                              linkSearchHeaders = 'PdfRelativePath'
                              ,
                              dictionaryNameHeader = 'Name'
                              ,
                              dictionaryRegexHeader = 'Regex'
                              ,
                              ignoreCase = TRUE
                              ,
                              ignoreExistingTextFile = TRUE
                              ,
                              conversionSoftware = 'pdftotext') {
  linkStatusHeader <-
    paste0(linkSearchHeaders, "Status")
  linkFullTextHeader <-
    paste0(linkSearchHeaders, "FullText")

  #Count the pattern in each studies parallaly
  ncores <- parallel::detectCores(all.tests = FALSE, logical = TRUE)
  cl <-
    parallel::makeCluster(round(ncores) / 2, outfile = "") #determines how many parallel processes are used for the pdf downloading
  doParallel::registerDoParallel(cl)

  '%dopar%' <- foreach::'%dopar%'

  results <-
    foreach::foreach(
      i = 1:nrow(myStudies),
      .packages = c('tools')
      ,
      .export = c("ReadLink", "CountPatternOverMatrix")
    ) %dopar% {
      options(stringsAsFactors = F)
      myStudy <- myStudies[i,]

      # Read fulltext. Convert if the link is a pdf link. Return Status and fulltext
      myStudy[, c(linkStatusHeader, linkFullTextHeader)] <-
        as.list(t(
          sapply(
            myStudy[, linkSearchHeaders],
            ReadLink,
            ignoreExistingTextFile = ignoreExistingTextFile
            ,
            conversionSoftware = conversionSoftware
          )
        ))

      myRegex <- myDictionary[, dictionaryRegexHeader]
      result <- sapply(
        myRegex,
        CountPatternOverMatrix,
        margin = 1,
        text = myStudy[,!(names(myStudy) %in% c(linkSearchHeaders, linkStatusHeader))],
        ignoreCase = ignoreCase
      )

      return(c(myStudy[, linkStatusHeader], result))
    }

  parallel::stopCluster(cl)

  results <- as.data.frame(t(as.matrix(as.data.frame(results))))
  colnames(results) <-
    c(linkStatusHeader, myDictionary[, dictionaryNameHeader])
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
#' @param textSearchingHeaders A list of the headers of the columns to search from. A list of character. Default value is c('Title', 'Abstract')
#' @param linkSearchHeaders A list of the headers of the columns to links to read and search from. A list of character. Default value is c('PdfRelativePath')
#' @param dictionaryNameHeader The header string of name column in dictionary. Default value is 'Name'.
#' @param dictionaryRegexHeader The header string of regular expression column in dictionary. Default value is 'Regex'.
#' @param ignoreCase boolean to decide whether to ignore the case in searching the content in dictionary in the searchingData or not. Default value is TRUE.
#' @param ignoreExistingTextFile ignoreExistingTextFile
#' @param conversionSoftware Software used to covert pdf to text. Default value is 'pdftotext'
#'
#' @return A data frame with result of the dictionary search. One column for each term in the dictionary, with the name of the term as header.
#'
#' @export
#'
CountTermsInStudies <- function(searchingData = NULL
                                ,
                                dictionary = NULL
                                ,
                                textSearchingHeaders = c('Title', 'Abstract')
                                ,
                                linkSearchHeaders = c('PdfRelativePath')
                                ,
                                dictionaryNameHeader = 'Name'
                                ,
                                dictionaryRegexHeader = 'Regex'
                                ,
                                ignoreCase = TRUE
                                ,
                                ignoreExistingTextFile = TRUE
                                ,
                                conversionSoftware = 'pdftotext') {
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

  #Count Pattern in parallel
  results <-
    # as.data.frame(
    CountPatternInPar(
      myStudies = myStudies
      ,
      myDictionary = myDictionary
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
      ,
      ignoreExistingTextFile = ignoreExistingTextFile
      ,
      conversionSoftware = conversionSoftware
    )

  return(results)
}

#' IdentifyTermsInStudies
#'
#' Identify terms in studies
#'
#' @param searchingData Either a dataset or a link to the dataset to search from
#' @param dictionary Either a dictionary dataset, or a link to the dictionary dataset to run the function on.
#'  It should consist two columns: name of the term and search string of the term. Regular expression (Perl) is accepted for the search string.
#'  If there is only one column, that column will be used both as name and regular expression.
#' @param textSearchingHeaders A list of the headers of the columns to search from. A list of character. Default value is c('Title', 'Abstract', 'FullText')
#' @param linkSearchHeaders A list of the headers of the columns to read and search from. A list of character. Default value is c('Title', 'Abstract', 'FullText')
#' @param dictionaryNameHeader The header string of name column in dictionary
#' @param dictionaryRegexHeader The header string of regular expression column in dictionary
#' @param ignoreCase boolean to decide whether to ignore the case in searching the content in dictionary in the searchingData or not
#' @param ignoreExistingTextFile ignoreExistingTextFile
#' @param conversionSoftware Software used to covert pdf to text. Default value is 'pdftotext'
#'
#' @return A data frame with result of the dictionary search. One column for each term in the dictionary, with the name of the term as header.
#'
#' @export
#'
IdentifyTermsInStudies <- function(searchingData = NULL
                                   ,
                                   dictionary = NULL
                                   ,
                                   textSearchingHeaders = c('Title', 'Abstract')
                                   ,
                                   linkSearchHeaders = c('PdfRelativePath')
                                   ,
                                   dictionaryNameHeader = 'Name'
                                   ,
                                   dictionaryRegexHeader = 'Regex'
                                   ,
                                   ignoreCase = TRUE
                                   ,
                                   ignoreExistingTextFile = TRUE
                                   ,
                                   conversionSoftware = 'pdftotext') {
  results <-
    CountTermsInStudies(
      searchingData = searchingData
      ,
      dictionary = dictionary
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
      ,
      ignoreExistingTextFile = ignoreExistingTextFile
      ,
      conversionSoftware = conversionSoftware
    )

  return(results != 0)
}

#' RiskOfBiasIdentification
#'
#' Identify Risk of Bias in input data within columns 'Title', 'Abstract' and 'PdfRelevantPath'
#'
#'
#' @param searchingData data to search from. Either a link to the file or a data frame or a matrix
#'
#' @return result flags
#'
#' @export
#'
RiskOfBiasIdentification <- function(searchingData) {
  results <-
    IdentifyTermsInStudies(searchingData = searchingData, dictionary = 'extra/ROBRegularExpression.txt')

  return(results)
}
