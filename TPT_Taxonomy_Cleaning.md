# Terrestrial Parasite Tracker Taxonomy Cleaning

The R script in this repository was created to clean the various taxonomic classification sources received from Terrestrial Parasite Tracker (TPT) participants to be added to the TPT Taxonomy Reource. This document describes the process completed by the script.

## Library Import
The following libraries will be installed in R:

taxotools - [ReadMe](https://github.com/vijaybarve/taxotools/commit/7dfa7a0fbde290966482bf8741b042c80efbff19?branch=7dfa7a0fbde290966482bf8741b042c80efbff19&diff=unified&short_path=b335630#diff-b335630551682c19a781afebcf4d07bf978fb1f8ac04c6bf87428ed5106870f5)
stringdist
data.table
stringi

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

1. Apply "Proper" capitalization to all classification terms except specific epithet and infraspecific epithet.
2. Strip spaces from beginning and ends of strings
3. Remove all '\xa0' chars

