# import libraries
library(taxotools)
library(stringdist)
library(data.table)

# load data with UTF-8 encoding
# df <- read.csv('input/Tick Taxonomy NMNH - Sheet1.csv')
# df <- read.csv('input/Flea checklist-full taxonomy udpated 12.2019.csv', encoding = 'UTF-8')
# df <- read.csv("~/GitHub/ixodes-tpt/input/Flea checklist-full taxonomy udpated 12 (version 1).csv", encoding="UTF-8")
df <- read.csv("~/GitHub/ixodes-tpt/input/Test Data.csv", encoding="UTF-8")

# Begin prep df for cleaning

# number of original records for verification
starting_records <- nrow(df)

# create unique ID for each row (use to match removed column data if needed?)
df$number <- seq.int(nrow(df))

# number of original columns for verification
starting_columns <- ncol(df)

# lower case all column names
colnames(df) <- tolower(colnames(df))

# Select columns that contain taxonomy
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


# Extract columns that do not relate to taxonomy
nonTaxonomyColumns <- df[ , -which(!(names(df) %in% names(which(sapply(names(df), containsTaxonomy) == FALSE))))]

# Retain only columns that relate to taxonomy plus number column
df <- df[ , -which(!(names(df) %in% names(which(sapply(names(df), containsTaxonomy) == TRUE | names(df) == "number"))))]

# darwinCoreTaxonTerms <- c("kingdom", "phylum", "class", "order", "family",
#                           "genus", "subgenus", "species", "specificEpithet", 
#                           "scientificName", "infraspecificEpithet", "taxonRank",
#                           "higherClassification", "namePublishedInYear", 
#                           "scientificNameAuthorship", "taxonomicStatus", 
#                           "nomenclaturalStatus", "namePublishedIn")

# define function: DarwinCore column name conversion function
# warning - this needs work see issue with order in flea file
convert2DwC <- function(df_colname) {
  x <- gsub('.*subspecies.*','infraspecificEpithet',df_colname)
  x <- gsub('.*species.*','specificEpithet',x)
  x <- gsub('.*rank.*','taxonRank',x)
  x <- gsub('.*author.*','scientificNameAuthorship',x) # note - this is really only part of scientificNameAuthorship, which should also include namePublishedInYear will be corrected later
  x <- gsub('.*year.*','namePublishedInYear',x)
  x
}

# convert column headers to DarwinCore terms
colnames(df) <- convert2DwC(colnames(df))

# ensure scientificNameAuthorship meets DarwinCore standard for ICZN
# TODO - end up with ",NA" when there is no author or year
df$scientificNameAuthorship <- paste(df$scientificNameAuthorship,
                                     df$namePublishedInYear, sep = ', ')

# fix cases like: (Jordan & Rothschild), 1922 need to make sure this is necessary
fixAuth <- function(x) ifelse(grepl('[a-z]),',x), paste(gsub(')', '',x),')',sep=''),x)
# TODO apply to the data

# End prep df for cleaning

#Begin basic cleaning of df

# basic string cleaning functions
# define function: make capitalization proper case
toproper <- function(x) ifelse(!is.na(x), paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2))),NA)
# define function: remove punctuation except spaces
removePunc <- function(x) ifelse(!is.na(x), gsub('[[:punct:]]+','',x))
# define function: find stuff with punctuation
containsPunc <- function(x) ifelse(!is.na(x), grepl('[[:punct:]]', x, perl = TRUE))
# define function: remove '\xa0' chars
removeEncoding <- function(x) ifelse(!is.na(x), gsub("\xa0", "", x))

# fix capitalization for both genus and species
# ignore author, publication
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
            grepl('publi', tolower(name), perl = TRUE)) {
  } else {
    df[,i] <- sapply(df[,i], toproper)
  }
}

# strip spaces from ends of strings
# warning - we should do this for ALL columns...see code below copied from https://stackoverflow.com/questions/20760547/removing-whitespace-from-a-whole-data-frame-in-r
setDT(df)
cols_to_be_rectified <- names(df)[vapply(df, is.character, logical(1))]
df[,c(cols_to_be_rectified) := lapply(.SD, trimws), .SDcols = cols_to_be_rectified]

# remove remove '\xa0' chars
setDT(df)
cols_to_be_rectified <- names(df)[vapply(df, is.character, logical(1))]
df[,c(cols_to_be_rectified) := lapply(.SD, removeEncoding), .SDcols = cols_to_be_rectified]

# TODO NOTE: there are other encoding problems of accented characters
# I am not familiar with this encoding style
# search: '<e' in author names in particular
# I think this is fixed by loading data at UTF-8

# End basic cleaning of df

# Begin selecting records for review

# define function: name length
name_length <- function(x) ifelse(!is.na(x), length(unlist(strsplit(x, ' '))), 0)
# define function: is not in
'%!in%' <- function(x,y)!('%in%'(x,y))

# extract higher taxa
higher_taxa <- df[which(lapply(df$infraspecificEpithet, name_length) == 0 & lapply(df$specificEpithet, name_length) == 0),]
df <- df[which(lapply(df$infraspecificEpithet, name_length) != 0 | lapply(df$specificEpithet, name_length) != 0),]

# extract rows with missing information
# extract rows with no genus, but has species
missing_genus <- df[which(lapply(df$specificEpithet, name_length) != 0 & lapply(df$genus, name_length) == 0),]
# add review reason column
missing_genus$reason <- "missing genus"
df <- df[which(lapply(df$genus, name_length) != 0),]
# extract rows with no genus, but has subspecies, these get caught above
# no_genus_has_subspecies <- df[which(lapply(df$infraspecificEpithet, name_length) != 0 & lapply(df$genus, name_length) == 0),]
# extract rows with no species, but has subspecies
missing_species <- df[which(lapply(df$specificEpithet, name_length) == 0 & lapply(df$infraspecificEpithet, name_length) != 0),]
# add review reason column
missing_species$reason <- "missing specificEpithet"
df <- df[which(lapply(df$specificEpithet, name_length) != 0 & lapply(df$infraspecificEpithet, name_length) != 0 | lapply(df$specificEpithet, name_length) != 0 & lapply(df$infraspecificEpithet, name_length) == 0)]
# combine extracted rows that are missing terms to df_review data frame
df_review <- rbind(missing_genus, missing_species)
# add higher taxa back to df
df <- rbind(higher_taxa, df)

# Missing data check warning: add this back later
#if(starting_records != nrow(df) + 
#   nrow(df_review)){print('Some records appear to have been lost. Script was terminated. Please address errors.')
#} else {

# extract rows with unexpected data
# extract rows with a multi-name genus, specificEpithet OR infraspecificEpithet
multi_epithet <- df[which(lapply(df$specificEpithet, name_length) > 1 | lapply(df$genus, name_length) > 1 | lapply(df$infraspecificEpithet, name_length) > 1),]
# add review reason column
multi_epithet$reason <- "multi term genus, specificEpithet or infraspecificEpithet"
#more_epithet <- df[which(lapply(df$infraspecificEpithet, name_length) > 1),]
df_review <- rbind(df_review, multi_epithet)

# retain single term genus, species and subspecies rows
# TODO why doesn't this one-liner work? the three separate lines below get the job done
# df <- df[which(lapply(df$specificEpithet, name_length) <= 1 | lapply(df$genus, name_length) <= 1 | lapply(df$infraspecificEpithet, name_length) <= 1),]
df <- df[which(lapply(df$specificEpithet, name_length) <= 1),]
df <- df[which(lapply(df$infraspecificEpithet, name_length) <= 1),]
df <- df[which(lapply(df$genus, name_length) <= 1),]

# extract sp's in specificEpithet
sp_wildcards <- c('sp', 'sp.', 'spp', 'spp.', 'sp.nov.', 'sp nov', 'sp. nov.', 
                  'prob', 'prob.', 'probably', 'unid', 'unidentified',
                  'spnov1')
variable_sp1 <- paste('sp', as.character(c(0:9)), sep='')
variable_sp2 <- paste('sp.', as.character(c(0:9)), sep='')
variable_sp3 <- paste('sp. ', as.character(c(0:9)), sep='')
sp_wildcards <- c(sp_wildcards, variable_sp1, variable_sp2, variable_sp3)
removed_sp <- df[which(df$specificEpithet %in% sp_wildcards), ] 
# add review reason column
removed_sp$reason <- "specificEpithet flagged"
removed_spp <- df[(df$infraspecificEpithet %in% sp_wildcards), ]
# add review reason column
removed_spp$reason <- "infraspecificEpithet flagged"
# add extracted records to df_review
df_review <- rbind(df_review, removed_sp, removed_spp)

# remove extracted records from df
df <- df[which(df$specificEpithet %!in% sp_wildcards), ]
df <- df[which(df$infraspecificEpithet %!in% sp_wildcards), ]

# extract names containing punctuation
# warning: we should check ALL names for punctuation
punctuated_species <- df[which(lapply(df$genus, containsPunc) == TRUE |
                                 lapply(df$specificEpithet, containsPunc) == TRUE |
                                 lapply(df$infraspecificEpithet, containsPunc) == TRUE),]

# add review reason column
punctuated_species$reason <- "contains punctuation"
# add extracted names to df_review
df_review <- rbind(df_review, punctuated_species)
# remove punctuated names from df
df <- df[which(lapply(df$genus, containsPunc) == FALSE &
                 lapply(df$specificEpithet, containsPunc) == FALSE &
                 lapply(df$infraspecificEpithet, containsPunc) == FALSE),]


# extract higher taxa
higher_taxa <- df[which(lapply(df$infraspecificEpithet, name_length) == 0 & lapply(df$specificEpithet, name_length) == 0),]
df <- df[which(lapply(df$infraspecificEpithet, name_length) != 0 | lapply(df$specificEpithet, name_length) != 0),]

# extract very short specific_epithet OR genus
short_names_CHECK <- df[which(lapply(df$specificEpithet, nchar) < 4 |
                                lapply(df$genus, nchar) < 4),]
# add review reason column
short_names_CHECK$reason <- "short name"

# add extracted rows to df_review
df_review <- rbind(df_review, short_names_CHECK)
# remove short names from df
df <- df[which(lapply(df$specificEpithet, nchar) >= 4 &
                 lapply(df$genus, nchar) >= 4),] 

# TODO BEGIN NOT TESTED
# Look for missing higher taxa (warning: don't forget to add back higher taxa later!)
# Get unique list of genera
unique_genera <- unique(df$genus)
data.frame(t(sapply(unique_genera,c)))
data.frame(Reduce(rbind, unique_genera))

# insert some code to check that all "incomplete_epithet" higher taxonomy is present in "single_epithet"
# if not add that genus to suggested additions data frame
# this needs work
# !(unique(incomplete_epithet$genus) %in% unique(single_epithet$genus))
incomplete_epithet_genera <- unique(df$genus)
incomplete_epithet_genera <- incomplete_epithet_genera[-which(incomplete_epithet_genera == '')] # remove any empty strings as genera
single_epithet_genera <- array(as.character(unlist(unique(df$genus))))
missing_genera <- incomplete_epithet_genera[!(incomplete_epithet_genera %in% single_epithet_genera)]

if(length(missing_genera) != 0){
  missing_genera <- incomplete_epithet[incomplete_epithet$genus %in% missing_genera,]
  missing_genera$species <- rep('sp', nrow(missing_genera)) # set species to 'sp'
  missing_genera$infraspecificEpithet <- rep('sp', nrow(missing_genera)) # set subspecies to 'sp'
  # single_epithet <- rbind(single_epithet, missing_genera) # not sure if we want to do this without checking first
  # if the above line is approved, we may need to adjust the 'verification passed' check below
}

# END NOT TESTED

# verify no records were lost
verification_passed = FALSE
if(starting_records != nrow(df) + 
   nrow(df_review) + 
   nrow(suggested_adds)) {
} else {
  verification_passed = TRUE
}

if(verification_passed) {
  # generate canonical name
  df <- cast_canonical(df,
                       canonical="canonical", 
                       genus = "genus", 
                       species = "specificEpithet",
                       subspecies = "infraspecificEpithet")
  
  # extract duplicate names 
  duplicates <- df[which(duplicated(df$canonical)),]
  # deduplicated list
  df <- df[which(!duplicated(df$canonical)),]
  

  
  # check Levenshtein's Distance (e.g., misspellings) [may need to do before canonical name generation]
  # Watch for: Ornithodoros vunkeri; Ornithodoros yukeri; Ornithodoros yunkeri
  
  temp <- c()
  similar_names <-c()
  compared_names <- c()
  cutoff_distance <- 2
  df2 <- c()
  io <- FALSE
  for(i in 1:length(df$canonical)){
    if(!(df$canonical[i] %in% similar_names)){ # testing
      for(j in 1:length(df$canonical)){
        score <- stringdist(df$canonical[i], df$canonical[j], "dl")
        temp <- c(temp, score)
      }
      if(any(temp %in% c(1:cutoff_distance))){
        if(io){
          df2 <- cbind(df2, temp)
          wc = wc + 1
        } else {
          df2 <- as.data.frame(temp)
          rownames(df2) <- df$canonical
          io <- TRUE
          wc <- 1
        }
        colnames(df2)[which(colnames(df2) == "temp")] <- df$canonical[i]
        similar <- rownames(df2)[which(df2[,wc]==min(df2[,wc][which(df2[,wc]>0)]))]
        comp_name <- rep(df$canonical[i], length(similar))
        similar_names <- c(similar_names, similar)
        compared_names <- c(compared_names, comp_name)
      }
      temp <- c()
    }
    if(i %% 10 == 0){
      print(paste('Completed iteration:', i, 'out of', length(parsed$canonical), 'iterations (', round(i/length(parsed$canonical),2)*100,'% DONE)'))
    }
  }
  print('FINISHED!')
  check_mat <- as.data.frame(cbind(compared_names, similar_names))

  
  

  # synonymize subspecies example: Amblyomma triguttatum triguttatum = Amblyomma triguttatum
  parsed <- synonymize_subspecies(parsed)
  # parsed$genus <- array(as.character(unlist((parsed$genus)))) # sometimes needed to sort by variable in RStudio
  # parsed$species <- array(as.character(unlist((parsed$species)))) # sometimes needed to sort by variable in RStudio
  
  # number unique
  nominate_species <- parsed[parsed$accid == 0, ]
  trinomial_nominate_names <- nominate_species[which(nominate_species$infraspecificepithet != ''),]
  binomial_nominate_names <- nominate_species[which(nominate_species$infraspecificepithet == ''),]
  if(nrow(trinomial_nominate_names) + 
     nrow(binomial_nominate_names) == nrow(nominate_species)){
    rm(nominate_species)
  } else {
    print("Please check your nominate_species data frame for issues.")
  }
  subspecies <- parsed[parsed$accid != 0, ]
  
  # End of data validation  

} else {
  print('Verification was not passed. Some records appear to have been lost. Script was terminated. Please address errors.')
}
#} End of Missing data check 1
