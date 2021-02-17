# import
library(taxotools)

# load data
df <- read.csv('input/Tick Taxonomy NMNH - Sheet1.csv')

# select single-word specific_epithets
name_length <- function(x) ifelse(!is.na(x), length(unlist(strsplit(x, ' '))), 0)
no_epithet <- df[which(lapply(df$Species, name_length) == 0 | lapply(df$Genus, name_length) == 0),] # no-name species OR genus
single_epithet <- df[which(lapply(df$Species, name_length) == 1 & lapply(df$Genus, name_length) == 1),] # single-name species AND genus
multi_epithet <- df[which(lapply(df$Species, name_length) > 1 | lapply(df$Genus, name_length) > 1),] # multi-name species OR genus

multi_subsp <- single_epithet[which(lapply(single_epithet$Subspecies, name_length) > 1),] # multi-name subspecies
single_epithet <- single_epithet[which(lapply(single_epithet$Subspecies, name_length) <= 1),] # single subspecific name OR no subspecies

# basic string cleaning functions
toproper <- function(x) ifelse(!is.na(x), paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2))),NA) # fix capitalization
removePunc <- function(x) ifelse(!is.na(x), gsub('[[:punct:] ]+','',x)) # remove punctuation (but not spaces)
containsPunc <- function(x) ifelse(!is.na(x), grepl('[[:punct:]]', x, perl = TRUE))

# fix capitalization for both Genus and Species
single_epithet$Genus <- toproper(single_epithet$Genus)
single_epithet$Species <- tolower(single_epithet$Species)
single_epithet$Subspecies <- tolower(single_epithet$Subspecies)

# strip spaces from ends of strings
single_epithet$Genus <- lapply(single_epithet$Genus, trimws)
single_epithet$Species <- lapply(single_epithet$Species, trimws)
single_epithet$Subspecies <- lapply(single_epithet$Subspecies, trimws)

# test for names containing punctuation
punctuated_species <- single_epithet[which(lapply(single_epithet$Genus, containsPunc) == TRUE |
                                             lapply(single_epithet$Species, containsPunc) == TRUE |
                                             lapply(single_epithet$Subspecies, containsPunc) == TRUE),]
single_epithet <- single_epithet[which(lapply(single_epithet$Genus, containsPunc) == FALSE &
                                         lapply(single_epithet$Species, containsPunc) == FALSE &
                                         lapply(single_epithet$Subspecies, containsPunc) == FALSE),]

# remove sp's
single_epithet <- single_epithet[which(single_epithet$Species != 'sp'), ]

# remove very short names for manual verification
short_names_CHECK <- single_epithet[which(lapply(single_epithet$Species, nchar) < 4 |
                         lapply(single_epithet$Genus, nchar) < 4),] # very short specific_epithet OR genus
single_epithet <- single_epithet[which(lapply(single_epithet$Species, nchar) >= 4 &
                                         lapply(single_epithet$Genus, nchar) >= 4),] 

# generate canonical name
single_epithet <- cast_canonical(single_epithet,
                                 canonical="canonical", 
                                 genus = "Genus", 
                                 species = "Species",
                                 subspecies = "Subspecies")
# check for duplicate names 
duplicates <- single_epithet[which(duplicated(single_epithet$canonical)),]
single_epithet <- single_epithet[which(!duplicated(single_epithet$canonical)),] # deduplicated list

# check Levenshtein's Distance (e.g., misspellings) [may need to do before canonical name generation]
# Watch for: Ornithodoros vunkeri; Ornithodoros yukeri; Ornithodoros yunkeri

# synonymize subspecies example: Amblyomma triguttatum triguttatum = Amblyomma triguttatum

# handle no-word names
# no_epithet

# handle multi-word names
# multi_epithet

# handle authors

