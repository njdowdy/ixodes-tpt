# import libraries
library(taxotools)
library(stringdist)
library(data.table)
library(stringi)

# load data with UTF-8 encoding
# df <- read.csv('input/Tick Taxonomy NMNH - Sheet1.csv')
# df <- read.csv('input/Flea checklist-full taxonomy udpated 12.2019.csv', encoding = 'UTF-8')
# df <- read.csv("~/GitHub/ixodes-tpt/input/Flea checklist-full taxonomy udpated 12 (version 1).csv", encoding="UTF-8")
# df <- read.csv("~/GitHub/ixodes-tpt/input/Test Data.csv", encoding="UTF-8")
df <- read.csv("~/GitHub/ixodes-tpt/input/nmnhsearch-20200320142359.csv", encoding="UTF-8")

# Begin prep df for cleaning

starting_records <- nrow(df) # number of original records for verification
df$number <- seq.int(nrow(df)) # create unique ID for each row (use to match removed column data if needed)
starting_columns <- ncol(df) # number of original columns for verification
colnames(df) <- tolower(colnames(df))# lower case all column names

# define function: Select columns that contain taxonomy
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


nonTaxonomyColumns <- df[ , -which(!(names(df) %in% names(which(sapply(names(df), containsTaxonomy) == FALSE))))] # Extract columns that do not relate to taxonomy to review file
df <- df[ , -which(!(names(df) %in% names(which(sapply(names(df), containsTaxonomy) == TRUE | names(df) == "number"))))] # Retain only columns that relate to taxonomy plus number column in working file

# darwinCoreTaxonTerms <- c("kingdom", "phylum", "class", "order", "family",
#                           "genus", "subgenus", "species", "specificEpithet", 
#                           "scientificName", "infraspecificEpithet", "taxonRank",
#                           "higherClassification", "namePublishedInYear", 
#                           "scientificNameAuthorship", "taxonomicStatus", 
#                           "nomenclaturalStatus", "namePublishedIn")

# define function: DarwinCore column name conversion function
# TODO - this needs work see issue with order in flea file
convert2DwC <- function(df_colname) {
  x <- gsub('.*subspecies.*','infraspecificEpithet',df_colname)
  x <- gsub('.*species.*','specificEpithet',x)
  x <- gsub('.*rank.*','taxonRank',x)
  x <- gsub('.*author.*','scientificNameAuthorship',x) # note - this is really only part of scientificNameAuthorship, which should also include namePublishedInYear will be corrected later
  x <- gsub('.*year.*','namePublishedInYear',x)
  x <- gsub('.*scientific.*','scientificName',x)
  x
}

colnames(df) <- convert2DwC(colnames(df)) # convert column headers to DarwinCore terms

# if no genus, but scientificName need to parse scientificName
# TODO someone please write this if statement!
df <- melt_scientificname (df,
                    sciname = "scientificName",
                    genus = "genus",
                    species = "specificEpithet",
                    subspecies = "infraspecificEpithet",
                    author = "scientificNameAuthorship")

df$scientificNameAuthorship <- paste(df$scientificNameAuthorship,
                                     df$namePublishedInYear, sep = ', ') # format scientificNameAuthorship to DarwinCore standard for ICZN

# define functions: remove 'NA , NA' chars
removeNA1 <- function(x) ifelse(!is.na(x), gsub("NA, NA", "", x))
removeNA2 <- function(x) ifelse(!is.na(x), gsub("NA, ", "", x))
removeNA3 <- function(x) ifelse(!is.na(x), gsub(", NA", "", x))
removeNA4 <- function(x) ifelse(!is.na(x), gsub("NA", "", x))

# remove remove 'NA , NA'
setDT(df)
cols_to_be_rectified <- names(df)[vapply(df, is.character, logical(1))]
df[,c(cols_to_be_rectified) := lapply(.SD, removeNA1), .SDcols = cols_to_be_rectified]

# remove remove 'NA, '
setDT(df)
cols_to_be_rectified <- names(df)[vapply(df, is.character, logical(1))]
df[,c(cols_to_be_rectified) := lapply(.SD, removeNA2), .SDcols = cols_to_be_rectified]

# remove remove ', NA'
setDT(df)
cols_to_be_rectified <- names(df)[vapply(df, is.character, logical(1))]
df[,c(cols_to_be_rectified) := lapply(.SD, removeNA3), .SDcols = cols_to_be_rectified]

# remove remove 'NA'
setDT(df)
cols_to_be_rectified <- names(df)[vapply(df, is.character, logical(1))]
df[,c(cols_to_be_rectified) := lapply(.SD, removeNA4), .SDcols = cols_to_be_rectified]

# define function: fix parentheses in author names
fixAuth <- function(x) ifelse(grepl('[a-z]),',x), paste(gsub(')', '',x),')',sep=''),x)
df$scientificNameAuthorship <- fixAuth(df$scientificNameAuthorship) # fix cases like: (Jordan & Rothschild), 1922

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

# TODO there are other encoding problems of accented characters
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

# extract rows with missing information to review file
missing_genus <- df[which(lapply(df$specificEpithet, name_length) != 0 & lapply(df$genus, name_length) == 0),] # select rows with no genus, but has species
missing_genus$reason <- "missing genus" # add review reason column
df <- df[which(lapply(df$genus, name_length) != 0),] # extract rows with no genus, but has species from working dataframe
missing_species <- df[which(lapply(df$specificEpithet, name_length) == 0 & lapply(df$infraspecificEpithet, name_length) != 0),] # extract rows with no species, but has subspecies
missing_species$reason <- "missing specificEpithet" # add review reason column
df_review <- rbind(missing_genus, missing_species) # combine extracted rows that are missing terms to df_review data frame
# retain only rows with complete information in working file
df <- df[which(lapply(df$specificEpithet, name_length) != 0 &
                 lapply(df$infraspecificEpithet, name_length) != 0 | 
                 lapply(df$specificEpithet, name_length) != 0 & lapply(df$infraspecificEpithet, name_length) == 0)] # extract rows with no species, but has subspecies from working dataframe
df <- rbind(higher_taxa, df) # add higher taxa back to working data frame

# # Missing data check 1
# if(starting_records != nrow(df) + 
#    nrow(df_review)) {print('Some records appear to have been lost. Script was terminated. Please address errors.')
#   } else {
  
    # extract rows with unexpected data to review file
    multi_epithet <- df[which(lapply(df$specificEpithet, name_length) > 1 | lapply(df$genus, name_length) > 1 | lapply(df$infraspecificEpithet, name_length) > 1),] # extract rows with a multi-name genus, specificEpithet OR infraspecificEpithet
    multi_epithet$reason <- "multi term genus, specificEpithet or infraspecificEpithet" # add review reason column
    df_review <- rbind(df_review, multi_epithet)
    # retain only single term genus, species and subspecies rows in working file
    df <- df[which(lapply(df$specificEpithet, name_length) <= 1),]
    df <- df[which(lapply(df$infraspecificEpithet, name_length) <= 1),]
    df <- df[which(lapply(df$genus, name_length) <= 1),]
  
    # extract sp's in specificEpithet and infraspecificEpithet
    sp_wildcards <- c('sp', 'sp.', 'spp', 'spp.', 'sp.nov.', 'sp nov', 'sp. nov.', 
                      'prob', 'prob.', 'probably', 'unid', 'unidentified',
                      'spnov1')
    variable_sp1 <- paste('sp', as.character(c(0:9)), sep='')
    variable_sp2 <- paste('sp.', as.character(c(0:9)), sep='')
    variable_sp3 <- paste('sp. ', as.character(c(0:9)), sep='')
    sp_wildcards <- c(sp_wildcards, variable_sp1, variable_sp2, variable_sp3)
    removed_sp <- df[which(df$specificEpithet %in% sp_wildcards), ] 
    removed_sp$reason <- "specificEpithet flagged" # add review reason column
    removed_spp <- df[(df$infraspecificEpithet %in% sp_wildcards), ]
    removed_spp$reason <- "infraspecificEpithet flagged" # add review reason column
    df_review <- rbind(df_review, removed_sp, removed_spp) # add extracted records to df_review
    df <- df[which(df$specificEpithet %!in% sp_wildcards), ] # remove extracted spcificEpithet records from df
    df <- df[which(df$infraspecificEpithet %!in% sp_wildcards), ] # remove extracted infraspecificEpithet records from df
  
    # extract names containing punctuation
    # TODO we should check ALL names for punctuation
    punctuated_species <- df[which(lapply(df$genus, containsPunc) == TRUE |
                                     lapply(df$specificEpithet, containsPunc) == TRUE |
                                     lapply(df$infraspecificEpithet, containsPunc) == TRUE),]
    
    punctuated_species$reason <- "contains punctuation" # add review reason column
    df_review <- rbind(df_review, punctuated_species) # add extracted names to df_review
    # remove punctuated names from df
    df <- df[which(lapply(df$genus, containsPunc) == FALSE &
                     lapply(df$specificEpithet, containsPunc) == FALSE &
                     lapply(df$infraspecificEpithet, containsPunc) == FALSE),]
    
    # extract higher taxa for next set of review
    higher_taxa <- df[which(lapply(df$infraspecificEpithet, name_length) == 0 & lapply(df$specificEpithet, name_length) == 0),]
    df <- df[which(lapply(df$infraspecificEpithet, name_length) != 0 | lapply(df$specificEpithet, name_length) != 0),]
      
    # extract very short specific_epithet OR genus
    short_genus <- df[which(lapply(df$genus, nchar) < 4),]
    short_genus$reason <- "short name" # add review reason column
    df <- df[which(lapply(df$genus, nchar) >= 4),] # remove short genera from df
    df_review <- rbind(df_review, short_genus) # add extracted rows to df_review
    short_specific <- df[which(lapply(df$specificEpithet, nchar) < 4),]
    short_specific$reason <- "short name" # add review reason column
    df <- df[which(lapply(df$specificEpithet, nchar) >= 4),] # remove short specificEpithets from df
    df_review <- rbind(df_review, short_specific) # add extracted rows to df_review
    short_infra <- df[which(lapply(df$infraspecificEpithet, nchar) != 0 & 
                              lapply(df$infraspecificEpithet, nchar) < 4),]
    short_infra$reason <- "short name" # add review reason column
    df <- df[which(lapply(df$infraspecificEpithet, nchar) == 0 | 
                     is.na(df$infraspecificEpithet) | 
                     lapply(df$infraspecificEpithet, nchar) >= 4),] # remove short infraspecificEpithets from df
    df_review <- rbind(df_review, short_infra) # add extracted rows to df_review
    
    # Look for missing higher taxa
    # TODO can we streamline this process and make sure it repeats for all columns in higher_taxa?
    suggested_adds <- data.frame (taxonName  = c(),
                                  taxonRank = c())
    missing_taxa <- data.frame (taxonName  = c(),
                                taxonRank = c())
    
    # genus
    # Get unique list of genera from df and higher_taxa
    unique_df <- unique(df$genus) # unique genera in the working data frame
    unique_ht <- unique(higher_taxa$genus) # unique genera in the higher taxa data frame
    unique_ht <- stri_remove_empty(unique_ht, na_empty = FALSE) # remove empty values from higher_taxa
    # compare the two lists and return any names in the working data frame, but not in higher_taxa. These are higher level taxa that we suggest adding.
    missing_taxa <- unique_df[!unique_df %in% unique_ht] # there is not a unique genus entry for these genera
    taxa_not_used <- unique_ht[!unique_ht %in% unique_df] # there is a genus taxon, but no species for it
    suggested_append <- as.data.frame(missing_taxa) # create data frame for suggested adds
    suggested_append$taxonRank <- "genus" # add reason
    suggested_adds <- rbind(suggested_adds,suggested_append) # add rows to suggested_adds data frame
    missing_taxa <- as.data.frame(taxa_not_used) # create data frame for suggested removals
    missing_taxa$taxonRank <- "genus" # add reason
    missing_taxa <- rbind(missing_taxa, taxa_not_used) # add rows to missing_taxa data frame
    # End genus
    
    # family
    # Get unique list of families from df and higher_taxa
    unique_df <- unique(df$family) # unique genera in the working data frame
    unique_ht <- unique(higher_taxa$family) # unique genera in the higher taxa data frame
    unique_ht <- stri_remove_empty(unique_ht, na_empty = FALSE) # remove empty values from higher_taxa
    # compare the two lists and return any names in the working data frame, but not in higher_taxa. These are higher level taxa that we suggest adding.
    missing_taxa <- unique_df[!unique_df %in% unique_ht] # there is not a unique genus entry for these genera
    taxa_not_used <- unique_ht[!unique_ht %in% unique_df] # there is a genus taxon, but no species for it
    suggested_append <- as.data.frame(missing_taxa) # create data frame for suggested adds
    suggested_append$taxonRank <- "family"
    suggested_adds <- rbind(suggested_adds,suggested_append)
    missing_append <- as.data.frame(taxa_not_used) # create data frame for suggested adds
    missing_append$taxonRank <- "family"
    missing_taxa <- rbind(missing_taxa, taxa_not_used)
    # End family
  
    # order
    # Get unique list of orders from df and higher_taxa
    unique_df <- unique(df$order) # unique genera in the working data frame
    unique_ht <- unique(higher_taxa$order) # unique genera in the higher taxa data frame
    unique_ht <- stri_remove_empty(unique_ht, na_empty = FALSE) # remove empty values from higher_taxa
    # compare the two lists and return any names in the working data frame, but not in higher_taxa. These are higher level taxa that we suggest adding.
    missing_taxa <- unique_df[!unique_df %in% unique_ht] # there is not a unique genus entry for these genera
    taxa_not_used <- unique_ht[!unique_ht %in% unique_df] # there is a genus taxon, but no species for it
    suggested_append <- as.data.frame(missing_taxa) # create data frame for suggested adds
    suggested_append$taxonRank <- "order"
    suggested_adds <- rbind(suggested_adds,suggested_append)
    missing_append <- as.data.frame(taxa_not_used) # create data frame for suggested adds
    missing_append$taxonRank <- "order"
    missing_taxa <- rbind(missing_taxa, taxa_not_used)
    # End order
  
    # verify no records were lost
    verification_passed = FALSE
    if(starting_records != nrow(df) + 
       nrow(df_review) + 
       nrow(higher_taxa)) {
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
      # TODO higher_taxa$canonical <- get the lowest ranking term and put it here!
      higher_taxa$canonical <- "" # add canonical column for merging with working file
      
      # extract blank canonical names for review
      no_canonical <- df[which(lapply(df$canonical, nchar) == 0),]
      df_review <- rbind(df_review, no_canonical, fill = TRUE)
      df <- df[which(lapply(df$canonical, nchar) != 0),] # remove blank canonicals from working file
      
      # extract duplicate names 
      duplicates <- df[which(duplicated(df$canonical)),]
      df <- df[which(!duplicated(df$canonical)),] # deduplicated list
    
    # add higher taxa back to df for remainder of cleaning
    df <- rbind(higher_taxa, df)
    
    #} End of Missing data check 1
    
    # End of data validation  
    
  } else {
    print('Verification was not passed. Some records appear to have been lost. Script was terminated. Please address errors.')

# write output
write.csv(df,"~/GitHub/ixodes-tpt/output/taxa_no_issues.csv", row.names = FALSE) # these should be good to go
write.csv(df_review,"~/GitHub/ixodes-tpt/output/taxa_need_review.csv", row.names = FALSE) # these need review
write.csv(duplicates,"~/GitHub/ixodes-tpt/output/duplicates.csv", row.names = FALSE) # these were removed as duplicates
write.csv(suggested_adds,"~/GitHub/ixodes-tpt/output/suggested_adds.csv", row.names = FALSE) # these are higher taxa that probably need to be added
write.csv(missing_taxa,"~/GitHub/ixodes-tpt/output/higher_taxa_not_used.csv", row.names = FALSE) # these higher taxa are in the file, but not used by any children
# write.csv(check_mat,"~/GitHub/ixodes-tpt/output/similar_names.csv", row.names = FALSE) # these names seem awful alike

}

# TODO this isn't working - what did I break?
  # check Levenshtein's Distance (e.g., misspellings) [may need to do before canonical name generation]
  # Watch for: Ornithodoros vunkeri; Ornithodoros yukeri; Ornithodoros yunkeri

# temp <- c()
# similar_names <-c()
# compared_names <- c()
# cutoff_distance <- 2
# df2 <- c()
# io <- FALSE
# for(i in 1:length(df$canonical)){
#   if(!(df$canonical[i] %in% similar_names)){ # testing
#     for(j in 1:length(df$canonical)){
#       score <- stringdist(df$canonical[i], df$canonical[j], "dl")
#       temp <- c(temp, score)
#     }
#     if(any(temp %in% c(1:cutoff_distance))){
#       if(io){
#         df2 <- cbind(df2, temp)
#         wc = wc + 1
#       } else {
#         df2 <- as.data.frame(temp)
#         rownames(df2) <- df$canonical
#         io <- TRUE
#         wc <- 1
#       }
#       colnames(df2)[which(colnames(df2) == "temp")] <- df$canonical[i]
#       similar <- rownames(df2)[which(df2[,wc]==min(df2[,wc][which(df2[,wc]>0)]))]
#       comp_name <- rep(df$canonical[i], length(similar))
#       similar_names <- c(similar_names, similar)
#       compared_names <- c(compared_names, comp_name)
#     }
#     temp <- c()
#   }
#   if(i %% 10 == 0){
#     print(paste('Completed iteration:', i, 'out of', length(df$canonical), 'iterations (', round(i/length(df$canonical),2)*100,'% DONE)'))
#   }
# }
# print('FINISHED!')
# check_mat <- as.data.frame(cbind(compared_names, similar_names))