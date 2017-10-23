# Automatically annotate articles with custom dictionary

This is a R package for automatical data annotation with terms in a dictionary. The main function is 'CountTermsInStudies'. It takes 'searchingData' to search from as either data frame/matrix or a link to the data in 'tsv','csv' or 'txt' formate. It also takes another parameter 'dictionary', which could be any dictionary with one name column and one search term (regular expression accepted) column. Please refer to manual for specific header names.

installation:

```{r}
install.packages("devtools")
library(devtools)
install_github("shihikoo/AutoAnnotation")
```

The package is in alpha testing stage. 


