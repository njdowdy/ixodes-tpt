# import libraries
library(stringdist)
library(data.table)
library(stringi)

# define function: make capitalization proper case
toproper <- function(x) ifelse(!is.na(x), paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2))),NA)
# define function: remove punctuation except spaces
removePunc <- function(x) ifelse(!is.na(x), gsub('[[:punct:]]+','',x))
# define function: find stuff with punctuation
containsPunc <- function(x) ifelse(!is.na(x), grepl('[[:punct:]]', x, perl = TRUE))
# define function: remove '\xa0' chars
removeEncoding <- function(x) ifelse(!is.na(x), gsub("\xa0", "", x))

# fix capitalization for both genus and species
# ignore author, publication, scientificName, and TPTID
# toproper all other fields
for(i in 1:ncol(df)) {
  name <- colnames(df)[i]
  if(grepl('infraspecificepithet', tolower(name), perl = TRUE)|
     grepl('variety', tolower(name), perl = TRUE) |
     grepl('form', tolower(name), perl = TRUE) |
     grepl('specificepithet', tolower(name), perl = TRUE)
  ) {
    df[,i] <- sapply(df[,i], tolower)
  } else if(grepl('author', tolower(name), perl = TRUE) |
            grepl('publi', tolower(name), perl = TRUE) | 
            grepl('tptid', tolower(name), perl = TRUE) | 
            grepl('scientific', tolower(name), perl = TRUE) |
            grepl('accepted', tolower(name), perl = TRUE)) {
  } else {
    df[,i] <- sapply(df[,i], toproper)
  }
}

# strip spaces from ends of strings
setDT(df)
cols_to_be_rectified <- names(df)[vapply(df, is.character, logical(1))]
df[,c(cols_to_be_rectified) := lapply(.SD, trimws), .SDcols = cols_to_be_rectified]

# remove remove '\xa0' chars
setDT(df)
cols_to_be_rectified <- names(df)[vapply(df, is.character, logical(1))]
df[,c(cols_to_be_rectified) := lapply(.SD, removeEncoding), .SDcols = cols_to_be_rectified]

# proceed to tpt_names_for_review script
