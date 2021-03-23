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

cast_scientificname <- function(dat=NULL,sciname="scientificname", genus="", 
                                subgenus="", species="",
                                subspecies="", author="", 
                                verbose=FALSE){
  if(is.null(dat) ){
    warning("No data supplied to process. Nothing to do...")
    return(NULL)  
  }
  newdat <- as.data.frame(dat)
  newdat$sciname_ <- NA
  if(is.empty(sciname)){
    warning("sciname not provided. Using default scientificname")
    sciname <- "scientificname"
  }
  if(genus==""){
    warning("genus field not specified")
    return(NULL)
  } else {
    newdat <- rename_column(newdat,genus,"genus_")
  }
  if(species==""){
    warning("species field not specified")
    return(NULL)
  } else {
    newdat <- rename_column(newdat,species,"species_")
  }
  if(subgenus!=""){
    newdat <- rename_column(newdat,subgenus,"subgenus_")
  } else {
    warning("subgenus field not specified. Assuming empty")
    newdat$subgenus_ <- NA
  }
  if(subspecies!=""){
    newdat <- rename_column(newdat,subspecies,"subspecies_")
  } else {
    warning("subspecies field not specified. Assuming empty")
    newdat$subspecies_ <- NA
  }
  if(author!=""){
    newdat <- rename_column(newdat,author,"author_")
  } else {
    warning("author field not specified. Assuming empty")
    newdat$author_ <- NA
  }
  if(verbose){pb = txtProgressBar(min = 0, max = nrow(newdat), initial = 0)}
  for(i in 1:nrow(newdat)){
    if(!is.empty(newdat$genus_[i])){
      scn <- toproper(newdat$genus_[i])
    }
    if(!is.empty(newdat$subgenus_[i])){
      scn <- paste(scn," (",toproper(newdat$genus_[i]),") ",sep = "")
    }
    if(!is.empty(newdat$species_[i])){
      scn <- paste(scn,newdat$species_[i])
    }
    if(!is.empty(newdat$subspecies_[i])){
      scn <- paste(scn,newdat$subspecies_[i])
    }
    if(!is.empty(newdat$author_[i])){
      scn <- paste(scn,trimws(newdat$author_[i]))
    }
    newdat$sciname_[i] <- scn
    if(verbose){setTxtProgressBar(pb,i)}
  }
  if(verbose){cat("\n")}
  newdat <- rename_column(newdat,"genus_",genus)
  newdat <- rename_column(newdat,"species_",species)
  if(subgenus!=""){
    newdat <- rename_column(newdat,"subgenus_",subgenus)
  } else {
    newdat <- newdat[ , !(names(newdat) %in% c("subgenus_"))]
  }
  if(subspecies!=""){
    newdat <- rename_column(newdat,"subspecies_",subspecies)
  } else {
    newdat <- newdat[ , !(names(newdat) %in% c("subspecies_"))]
  }
  if(author!=""){
    newdat <- rename_column(newdat,"author_",author)
  } else {
    newdat <- newdat[ , !(names(newdat) %in% c("author_"))]
  }
  if((sciname == "scientificname") & ("scientificname" %in% names(newdat))){
    newdat$sciname <- newdat$sciname_
    newdat <- newdat[ , !(names(newdat) %in% c("sciname_"))]
  } else {
    if(sciname %in% names(newdat)){
      newdat <- newdat[ , !(names(newdat) %in% sciname)]
    }
    newdat <- rename_column(newdat,"sciname_",sciname)
  }
  return(newdat)
}

df <- cast_scientificname (df,
                           sciname="scientificName", 
                           genus="genus", 
                           subgenus="subgenus", 
                           species="specificEpithet", 
                           subspecies="infraspecificEpithet", 
                           author="scientificNameAuthorship", 
                           verbose=FALSE)

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