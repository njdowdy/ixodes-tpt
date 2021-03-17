# Terrestrial Parasite Tracker Taxonomy Cleaning

The R script in this repository was created to clean the various taxonomic classification sources received from Terrestrial Parasite Tracker (TPT) participants to be added to the TPT Taxonomy Reource. This document describes the process completed by the script.

## Library Import
The following R packages will be installed by the script:

 - taxotools - [ReadMe](https://github.com/vijaybarve/taxotools/commit/7dfa7a0fbde290966482bf8741b042c80efbff19?branch=7dfa7a0fbde290966482bf8741b042c80efbff19&diff=unified&short_path=b335630#diff-b335630551682c19a781afebcf4d07bf978fb1f8ac04c6bf87428ed5106870f5)
 - stringdist
 - data.table
 - stringi

## Preparation
These steps prepare the file for the processing that will follow:

1. Record the number of original records for later verification that no information was lost.
2. Create a unique ID for each row which can be used to match removed column data if needed.
3. Record the number of original columns for later verification that no information was lost.
4. Change all column headers to lower case.
5. Remove all columns that do not contain taxonomy except the unique ID created above. 

### columns with names including terms like the following will remain in the working file:
domain, kingdom, regnum, phylum, class, legio, cohort, order, famil, trib, genus, species, sectio, variet, form, clade, series, author, publi, year, status, rank, name, epithet

6. Convert column headers to align with <a href="https://dwc.tdwg.org/terms/#taxon" class="external">Darwin Core (DwC) Taxon terms</a>. Terms not in this list will be handled by the cleaning process, but may not be included by any resource that adheres to DwC.

### Darwin Core Taxon Terms 
kingdom, phylum, class, order, family, genus, subgenus, species, specificEpithet, scientificName, infraspecificEpithet, taxonRank, higherClassification, namePublishedInYear, scientificNameAuthorship, taxonomicStatus, nomenclaturalStatus, namePublishedIn

7. Convert <a href="https://dwc.tdwg.org/terms/#dwc:scientificNameAuthorship" class="external">scientificNameAuthorship to DarwinCore standard</a> for ICZN

## Basic Cleaning
These steps complete some basic data cleanup.

1. Apply "Proper" capitalization to all classification terms except specificEpithet and infraspecificEpithet.
2. Strip spaces from beginning and ends of strings
3. Remove all '\xa0' chars

## Extract data that needs review
The following data will be removed from the working file and placed in the output file taxa_need_review.csv for further review.

1. Rows that include specificEpithet or infraspecificEpithet but genus is blank.
2. Rows that include infraspecificEpithet but specificEpithet is blank.
3. Rows with more than one term in genus, specificEpithet, or infraspecificEpithet.
5. Rows with any of the following in specificEpithet or infraspecificEpithet.

### Questionable name terms
sp, sp., spp, spp., sp.nov., sp nov, sp. nov., prob, prob., probably, unid, unidentified, spnov plus a number, sp plus a number

6. Rows that include any classification term with punctuation (excluding scientificNameAuthorship).
7. Rows that include names shorter than four letters.

## Suggest taxa to add
In order to ensure the entirety of the taxon tree of interest is included. A list of all higher taxa that are used in the working file as part of a classification but that are NOT included as their own row in the file will be generated in the suggested_adds.csv file. These terms may be missing from the working file or they may be included but misspelled. A thourough review of this list as compared to the working file is encourged. If it is determined that a term from the list should be added, then the term along with all of it's associated higher taxa, authorship, etc. should be added to the taxa_no_issues.csv output file for further processing.

## Suggest taxa to remove
If a higher taxon name is included in the file, but is NOT used as a higher taxon in any lower taxa, it will be listed in the output file higher_taxa_not_used.csv. These terms may be missing from the working file or they may be included but misspelled. A thourough review of this list as compared to the working file is encourged. If it is determined that a term from the list should be removed, then the term along with all of it's associated higher taxa, authorship, etc. should be removed to the taxa_no_issues.csv output file for further processing.

## Remove duplicate names for review
1. Generate "canonical name" - Construct canonical names using Genus, Species and Subspecies fields. See [taxotools](https://github.com/vijaybarve/taxotools/blob/master/man/cast_canonical.Rd).
2. Remove one of any duplicate canonical names to the output file duplicates.csv for review. A comparison of the removed term with the one in the working file is suggested to ensure that the removed rows are duplicates.

## Suggest name similarities for review
Perform Levenshtein's Distance check  to review for misspellings and other issues (eg. Ornithodoros vunkeri; Ornithodoros yukeri; Ornithodoros yunkeri). A file will be generated for review - similar_names.csv. These terms may be indicate misspellings but they may also be "false positives" - names that really are different, but are just similar. A thourough review of this list is encourged. If it is determined that a term from the list should be removed or change, then the change should be made to the taxa_no_issues.csv output file for further processing.

## Validation
The script will perform a validation to ensure that the number of rows in the original input file is equal to the sum of the number of rows in the taxa_no_issues.csv, taxa_need_review.csv, and duplicates.csv files. If the validation fails, there has been an issue and a more detailed review will be necessary. If the validation passes, the output files will be generated.

## Repeat
The process above should be repeated using the out put file taxa_no_issues.csv as the new input until there are no longer any rows in the taxa_need_review.csv and duplicates.csv files and any items in the suggested_adds.csv, higher_taxa_not_used.csv and similar_names.csv are considered and determined to require no further investigation.

