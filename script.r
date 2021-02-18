# import
library(taxotools)

# load data
df <- read.csv('input/Tick Taxonomy NMNH - Sheet1.csv')

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

# select single-word specific_epithets
name_length <- function(x) ifelse(!is.na(x), length(unlist(strsplit(x, ' '))), 0)
incomplete_epithet <- df[which(lapply(df$species, name_length) == 0 | lapply(df$genus, name_length) == 0),] # no-name species OR genus
single_epithet <- df[which(lapply(df$species, name_length) == 1 & lapply(df$genus, name_length) == 1),] # single-name species AND genus
multi_epithet <- df[which(lapply(df$species, name_length) > 1 | lapply(df$genus, name_length) > 1),] # multi-name species OR genus

multi_subsp <- single_epithet[which(lapply(single_epithet$infraspecificEpithet, name_length) > 1),] # multi-name subspecies
single_epithet <- single_epithet[which(lapply(single_epithet$infraspecificEpithet, name_length) <= 1),] # single subspecific name OR no subspecies

# basic string cleaning functions
toproper <- function(x) ifelse(!is.na(x), paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2))),NA) # fix capitalization
removePunc <- function(x) ifelse(!is.na(x), gsub('[[:punct:] ]+','',x)) # remove punctuation (but not spaces)
containsPunc <- function(x) ifelse(!is.na(x), grepl('[[:punct:]]', x, perl = TRUE))

# fix capitalization for both genus and species
single_epithet$genus <- toproper(single_epithet$genus)
single_epithet$species <- tolower(single_epithet$species)
single_epithet$infraspecificEpithet <- tolower(single_epithet$infraspecificEpithet)

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
single_epithet <- single_epithet[which(single_epithet$species != 'sp'), ]

# remove very short names for manual verification
short_names_CHECK <- single_epithet[which(lapply(single_epithet$species, nchar) < 4 |
                         lapply(single_epithet$genus, nchar) < 4),] # very short specific_epithet OR genus
single_epithet <- single_epithet[which(lapply(single_epithet$species, nchar) >= 4 &
                                         lapply(single_epithet$genus, nchar) >= 4),] 

# generate canonical name
single_epithet <- cast_canonical(single_epithet,
                                 canonical="canonical", 
                                 genus = "genus", 
                                 species = "species",
                                 subspecies = "infraspecificEpithet")
# check for duplicate names 
duplicates <- single_epithet[which(duplicated(single_epithet$canonical)),]
single_epithet <- single_epithet[which(!duplicated(single_epithet$canonical)),] # deduplicated list

# check Levenshtein's Distance (e.g., misspellings) [may need to do before canonical name generation]
# Watch for: Ornithodoros vunkeri; Ornithodoros yukeri; Ornithodoros yunkeri

# synonymize subspecies example: Amblyomma triguttatum triguttatum = Amblyomma triguttatum
synonymize_subspecies()



# handle no-word names
# incomplete_epithet

# handle multi-word names
# multi_epithet

# handle authors

