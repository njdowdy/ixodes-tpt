# import
library(taxotools)

# load data
df <- read.csv('input/Tick Taxonomy NMNH - Sheet1.csv')

# number of starting records for verification
starting_records <- nrow(df)

colnames(df) <- tolower(colnames(df)) # lower case column names

containsTaxonomy <- function(x) ifelse(!is.na(x), 
                                       grepl('domain', tolower(x), perl = TRUE) |
                                       grepl('kingdom', tolower(x), perl = TRUE) |
                                       grepl('regnum', tolower(x), perl = TRUE) |
                                       grepl('phylum', tolower(x), perl = TRUE) |
                                       grepl('class', tolower(x), perl = TRUE) |
                                       grepl('legio', tolower(x), perl = TRUE) |
                                       grepl('cohort', tolower(x), perl = TRUE) |
                                       grepl('order', tolower(x), perl = TRUE) |
                                       grepl('famil', tolower(x), perl = TRUE) |
                                       grepl('trib', tolower(x), perl = TRUE) |
                                       grepl('genus', tolower(x), perl = TRUE) |
                                       grepl('species', tolower(x), perl = TRUE) |
                                       grepl('sectio', tolower(x), perl = TRUE) |
                                       grepl('variet', tolower(x), perl = TRUE) |
                                       grepl('form', tolower(x), perl = TRUE) |
                                       grepl('clade', tolower(x), perl = TRUE) |
                                       grepl('series', tolower(x), perl = TRUE) |
                                       grepl('author', tolower(x), perl = TRUE) |
                                       grepl('publi', tolower(x), perl = TRUE) |
                                       grepl('year', tolower(x), perl = TRUE) |
                                       grepl('status', tolower(x), perl = TRUE) |
                                       grepl('rank', tolower(x), perl = TRUE) |
                                       grepl('name', tolower(x), perl = TRUE) |
                                       grepl('epithet', tolower(x), perl = TRUE)) 

df <- df[ , -which(!(names(df) %in% names(which(sapply(names(df), containsTaxonomy) == TRUE))))] # remove columns that do not relate to taxonomy

# convert to DarwinCore terms
convert2DwC <- function(df_colname) {
                       x <- gsub('.*subspecies.*','infraspecificEpithet',df_colname)
                       x <- gsub('.*rank.*','taxonRank',x)
                       x <- gsub('.*author.*','scientificNameAuthorship',x)
                       x <- gsub('.*year.*','namePublishedInYear',x)
                       x
                       } # this needs work

colnames(df) <- convert2DwC(colnames(df)) # convert to DarwinCore terms

# darwinCoreTaxonTerms <- c("kingdom", "phylum", "class", "order", "family",
#                           "genus", "subgenus", "species", "specificEpithet", 
#                           "scientificName", "infraspecificEpithet", "taxonRank",
#                           "higherClassification", "namePublishedInYear", 
#                           "scientificNameAuthorship", "taxonomicStatus", 
#                           "nomenclaturalStatus", "namePublishedIn")

# basic string cleaning functions
toproper <- function(x) ifelse(!is.na(x), paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2))),NA) # fix capitalization
removePunc <- function(x) ifelse(!is.na(x), gsub('[[:punct:]]+','',x)) # remove punctuation (but not spaces)
containsPunc <- function(x) ifelse(!is.na(x), grepl('[[:punct:]]', x, perl = TRUE))

# fix capitalization for both genus and species
for(i in 1:ncol(df)) {
  name <- colnames(df)[i]
  if(grepl('infraspecificepithet', tolower(name), perl = TRUE)|
     grepl('variety', tolower(name), perl = TRUE) |
     grepl('form', tolower(name), perl = TRUE) |
     grepl('species', tolower(name), perl = TRUE)
  ) {
    df[,i] <- sapply(df[,i], tolower)
  } else if(grepl('author', tolower(name), perl = TRUE) |
            grepl('publi', tolower(name), perl = TRUE)) {
  } else {
    df[,i] <- sapply(df[,i], toproper)
  }
}

# select single-word specific_epithets
name_length <- function(x) ifelse(!is.na(x), length(unlist(strsplit(x, ' '))), 0)
incomplete_epithet <- df[which(lapply(df$species, name_length) == 0 | lapply(df$genus, name_length) == 0),] # no-name species OR genus
single_epithet <- df[which(lapply(df$species, name_length) == 1 & lapply(df$genus, name_length) == 1),] # single-name species AND genus
multi_epithet <- df[which(lapply(df$species, name_length) > 1 | lapply(df$genus, name_length) > 1),] # multi-name species OR genus

multi_subsp <- single_epithet[which(lapply(single_epithet$infraspecificEpithet, name_length) > 1),] # multi-name subspecies
single_epithet <- single_epithet[which(lapply(single_epithet$infraspecificEpithet, name_length) <= 1),] # single subspecific name OR no subspecies

# strip spaces from ends of strings
single_epithet$genus <- lapply(single_epithet$genus, trimws)
single_epithet$species <- lapply(single_epithet$species, trimws)
single_epithet$infraspecificEpithet <- lapply(single_epithet$infraspecificEpithet, trimws)

# test for names containing punctuation
punctuated_species <- single_epithet[which(lapply(single_epithet$genus, containsPunc) == TRUE |
                                             lapply(single_epithet$species, containsPunc) == TRUE |
                                             lapply(single_epithet$infraspecificEpithet, containsPunc) == TRUE),]
single_epithet <- single_epithet[which(lapply(single_epithet$genus, containsPunc) == FALSE &
                                         lapply(single_epithet$species, containsPunc) == FALSE &
                                         lapply(single_epithet$infraspecificEpithet, containsPunc) == FALSE),]

# remove sp's
removed_sp <- single_epithet[which(single_epithet$species == 'sp'), ]
single_epithet <- single_epithet[which(single_epithet$species != 'sp'), ]

# remove very short names for manual verification
short_names_CHECK <- single_epithet[which(lapply(single_epithet$species, nchar) < 4 |
                         lapply(single_epithet$genus, nchar) < 4),] # very short specific_epithet OR genus
single_epithet <- single_epithet[which(lapply(single_epithet$species, nchar) >= 4 &
                                         lapply(single_epithet$genus, nchar) >= 4),] 

# verify no records were lost
verification_passed = FALSE
if(starting_records != nrow(single_epithet) + 
   nrow(punctuated_species) + 
   nrow(multi_epithet) + 
   nrow(multi_subsp) + 
   nrow(incomplete_epithet) + 
   nrow(removed_sp) +
   nrow(short_names_CHECK)) {
} else {
  verification_passed = TRUE
}

if(verification_passed) {
  
  # generate canonical name
  single_epithet <- cast_canonical(single_epithet,
                                   canonical="canonical", 
                                   genus = "genus", 
                                   species = "species",
                                   subspecies = "infraspecificEpithet")
  
  # check Levenshtein's Distance (e.g., misspellings) [may need to do before canonical name generation]
  # Watch for: Ornithodoros vunkeri; Ornithodoros yukeri; Ornithodoros yunkeri
  library(stringdist)
  temp <- c()
  similar_names <-c()
  compared_names <- c()
  df2 <- c()
  io <- FALSE
  for(i in 1:length(single_epithet$canonical)){
    if(!(single_epithet$canonical[i] %in% similar_names)){ # testing
      for(j in 1:length(single_epithet$canonical)){
        score <- stringdist(single_epithet$canonical[i], single_epithet$canonical[j], "dl")
        temp <- c(temp, score)
      }
      if(any(temp %in% c(1:3))){
        if(io){
          df2 <- cbind(df2, temp)
          wc = wc + 1
        } else {
          df2 <- as.data.frame(temp)
          rownames(df2) <- single_epithet$canonical
          io <- TRUE
          wc <- 1
        }
        colnames(df2)[which(colnames(df2) == "temp")] <- single_epithet$canonical[i]
        similar <- rownames(df2)[which(df2[,wc]==min(df2[,wc][which(df2[,wc]>0)]))]
        comp_name <- rep(single_epithet$canonical[i], length(similar))
        similar_names <- c(similar_names, similar)
        compared_names <- c(compared_names, comp_name)
      }
      temp <- c()
    }
  }
  check_mat <- as.data.frame(cbind(compared_names, similar_names))
  
  # check for duplicate names 
  duplicates <- single_epithet[which(duplicated(single_epithet$canonical)),]
  single_epithet <- single_epithet[which(!duplicated(single_epithet$canonical)),] # deduplicated list
  
  # synonymize subspecies example: Amblyomma triguttatum triguttatum = Amblyomma triguttatum
  single_epithet <- synonymize_subspecies(single_epithet)
  
  # number unique
  nominate_species <- single_epithet[single_epithet$accid == 0, ]
  subspecies <- single_epithet[single_epithet$accid != 0, ]
  
  # handle incomplete_epithet
  # handle multi-word names
  # handle authors, years
} else {
  print('Verification was not passed. Some records appear to have been lost. Script was terminated. Please address errors.')
}