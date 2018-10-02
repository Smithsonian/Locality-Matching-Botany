library(data.table)
library(RSQLite)
library(XML)
library(stringr)
#library(R.utils)

create_table_query <- 'CREATE TABLE gbif(gbifID INTEGER PRIMARY KEY, abstract, accessRights, accrualMethod, accrualPeriodicity, accrualPolicy, alternative, audience, available, bibliographicCitation, conformsTo, contributor, coverage, created, creator, date, dateAccepted, dateCopyrighted, dateSubmitted, description, educationLevel, extent, format, hasFormat, hasPart, hasVersion, identifier, instructionalMethod, isFormatOf, isPartOf, isReferencedBy, isReplacedBy, isRequiredBy, isVersionOf, issued, language, license, mediator, medium, modified, provenance, publisher, "references", relation, replaces, requires, rights, rightsHolder, source, spatial, subject, tableOfContents, temporal, title, type, valid, institutionID, collectionID, datasetID, institutionCode, collectionCode, datasetName, ownerInstitutionCode, basisOfRecord, informationWithheld, dataGeneralizations, dynamicProperties, occurrenceID, catalogNumber, recordNumber, recordedBy, individualCount, organismQuantity, organismQuantityType, sex, lifeStage, reproductiveCondition, behavior, establishmentMeans, occurrenceStatus, preparations, disposition, associatedReferences, associatedSequences, associatedTaxa, otherCatalogNumbers, occurrenceRemarks, organismID, organismName, organismScope, associatedOccurrences, associatedOrganisms, previousIdentifications, organismRemarks, materialSampleID, eventID, parentEventID, fieldNumber, eventDate, eventTime, startDayOfYear, endDayOfYear, year, month, day, verbatimEventDate, habitat, samplingProtocol, samplingEffort, sampleSizeValue, sampleSizeUnit, fieldNotes, eventRemarks, locationID, higherGeographyID, higherGeography, continent, waterBody, islandGroup, island, countryCode, stateProvince, county, municipality, locality, verbatimLocality, verbatimElevation, verbatimDepth, minimumDistanceAboveSurfaceInMeters, maximumDistanceAboveSurfaceInMeters, locationAccordingTo, locationRemarks, decimalLatitude, decimalLongitude, coordinateUncertaintyInMeters, coordinatePrecision, pointRadiusSpatialFit, verbatimCoordinateSystem, verbatimSRS, footprintWKT, footprintSRS, footprintSpatialFit, georeferencedBy, georeferencedDate, georeferenceProtocol, georeferenceSources, georeferenceVerificationStatus, georeferenceRemarks, geologicalContextID, earliestEonOrLowestEonothem, latestEonOrHighestEonothem, earliestEraOrLowestErathem, latestEraOrHighestErathem, earliestPeriodOrLowestSystem, latestPeriodOrHighestSystem, earliestEpochOrLowestSeries, latestEpochOrHighestSeries, earliestAgeOrLowestStage, latestAgeOrHighestStage, lowestBiostratigraphicZone, highestBiostratigraphicZone, lithostratigraphicTerms, "group", formation, member, bed, identificationID, identificationQualifier, typeStatus, identifiedBy, dateIdentified, identificationReferences, identificationVerificationStatus, identificationRemarks, taxonID, scientificNameID, acceptedNameUsageID, parentNameUsageID, originalNameUsageID, nameAccordingToID, namePublishedInID, taxonConceptID, scientificName, acceptedNameUsage, parentNameUsage, originalNameUsage, nameAccordingTo, namePublishedIn, namePublishedInYear, higherClassification, kingdom, phylum, class, "order", family, genus, subgenus, specificEpithet, infraspecificEpithet, taxonRank, verbatimTaxonRank, vernacularName, nomenclaturalCode, taxonomicStatus, nomenclaturalStatus, taxonRemarks, datasetKey, publishingCountry, lastInterpreted, elevation, elevationAccuracy, depth, depthAccuracy, distanceAboveSurface, distanceAboveSurfaceAccuracy, issue, mediaType, hasCoordinate, hasGeospatialIssues, taxonKey, kingdomKey, phylumKey, classKey, orderKey, familyKey, genusKey, subgenusKey, speciesKey, species, genericName, typifiedName, protocol, lastParsed, lastCrawled, repatriated);'

col_names <- c("gbifID","abstract","accessRights","accrualMethod","accrualPeriodicity","accrualPolicy","alternative","audience","available","bibliographicCitation","conformsTo","contributor","coverage","created","creator","date","dateAccepted","dateCopyrighted","dateSubmitted","description","educationLevel","extent","format","hasFormat","hasPart","hasVersion","identifier","instructionalMethod","isFormatOf","isPartOf","isReferencedBy","isReplacedBy","isRequiredBy","isVersionOf","issued","language","license","mediator","medium","modified","provenance","publisher","references","relation","replaces","requires","rights","rightsHolder","source","spatial","subject","tableOfContents","temporal","title","type","valid","institutionID","collectionID","datasetID","institutionCode","collectionCode","datasetName","ownerInstitutionCode","basisOfRecord","informationWithheld","dataGeneralizations","dynamicProperties","occurrenceID","catalogNumber","recordNumber","recordedBy","individualCount","organismQuantity","organismQuantityType","sex","lifeStage","reproductiveCondition","behavior","establishmentMeans","occurrenceStatus","preparations","disposition","associatedReferences","associatedSequences","associatedTaxa","otherCatalogNumbers","occurrenceRemarks","organismID","organismName","organismScope","associatedOccurrences","associatedOrganisms","previousIdentifications","organismRemarks","materialSampleID","eventID","parentEventID","fieldNumber","eventDate","eventTime","startDayOfYear","endDayOfYear","year","month","day","verbatimEventDate","habitat","samplingProtocol","samplingEffort","sampleSizeValue","sampleSizeUnit","fieldNotes","eventRemarks","locationID","higherGeographyID","higherGeography","continent","waterBody","islandGroup","island","countryCode","stateProvince","county","municipality","locality","verbatimLocality","verbatimElevation","verbatimDepth","minimumDistanceAboveSurfaceInMeters","maximumDistanceAboveSurfaceInMeters","locationAccordingTo","locationRemarks","decimalLatitude","decimalLongitude","coordinateUncertaintyInMeters","coordinatePrecision","pointRadiusSpatialFit","verbatimCoordinateSystem","verbatimSRS","footprintWKT","footprintSRS","footprintSpatialFit","georeferencedBy","georeferencedDate","georeferenceProtocol","georeferenceSources","georeferenceVerificationStatus","georeferenceRemarks","geologicalContextID","earliestEonOrLowestEonothem","latestEonOrHighestEonothem","earliestEraOrLowestErathem","latestEraOrHighestErathem","earliestPeriodOrLowestSystem","latestPeriodOrHighestSystem","earliestEpochOrLowestSeries","latestEpochOrHighestSeries","earliestAgeOrLowestStage","latestAgeOrHighestStage","lowestBiostratigraphicZone","highestBiostratigraphicZone","lithostratigraphicTerms","group","formation","member","bed","identificationID","identificationQualifier","typeStatus","identifiedBy","dateIdentified","identificationReferences","identificationVerificationStatus","identificationRemarks","taxonID","scientificNameID","acceptedNameUsageID","parentNameUsageID","originalNameUsageID","nameAccordingToID","namePublishedInID","taxonConceptID","scientificName","acceptedNameUsage","parentNameUsage","originalNameUsage","nameAccordingTo","namePublishedIn","namePublishedInYear","higherClassification","kingdom","phylum","class","order","family","genus","subgenus","specificEpithet","infraspecificEpithet","taxonRank","verbatimTaxonRank","vernacularName","nomenclaturalCode","taxonomicStatus","nomenclaturalStatus","taxonRemarks","datasetKey","publishingCountry","lastInterpreted","elevation","elevationAccuracy","depth","depthAccuracy","distanceAboveSurface","distanceAboveSurfaceAccuracy","issue","mediaType","hasCoordinate","hasGeospatialIssues","taxonKey","kingdomKey","phylumKey","classKey","orderKey","familyKey","genusKey","subgenusKey","speciesKey","species","genericName","typifiedName","protocol","lastParsed","lastCrawled","repatriated")



create_datasettable_query <- 'CREATE TABLE datasets(datasetKey PRIMARY KEY, title);'


gbif_db <- dbConnect(RSQLite::SQLite(), "gbif.sqlite")


#table with data
n <- dbExecute(gbif_db, create_table_query)
# n <- dbExecute(gbif_db, 'CREATE INDEX gbifID ON gbif(gbifID)')
# n <- dbExecute(gbif_db, 'CREATE INDEX issue ON gbif(issue)')
# n <- dbExecute(gbif_db, 'CREATE INDEX gb_datasetKey ON gbif(datasetID)')
# n <- dbExecute(gbif_db, 'CREATE INDEX basisOfRecord ON gbif(basisOfRecord)')
# n <- dbExecute(gbif_db, 'CREATE INDEX scientificName ON gbif(scientificName)')
# #INDEX FOR LOCATION
# n <- dbExecute(gbif_db, 'CREATE INDEX scientificName ON gbif(scientificName)')

#datasets table
n <- dbExecute(gbif_db, create_datasettable_query)
n <- dbExecute(gbif_db, 'CREATE INDEX ds_datasetKey ON datasets(datasetKey)')

datasets_xml <- list.files("dataset/", pattern = "*.xml")
no_datasets <- length(datasets_xml)

for (i in 1:no_datasets){
  cat(paste("Dataset ", i, "\n"))
  meta_file <- xmlToList(paste0("dataset/", datasets_xml[i]))
  datasetKey <- str_replace(datasets_xml[i], ".xml", "")
  datasetTitle <- meta_file$dataset$title
  datasetTitle <- stringr::str_replace_all(datasetTitle, "'", "''")
  dbExecute(gbif_db, paste0("INSERT INTO datasets (datasetKey, title) VALUES ('" , datasetKey, "', '" , datasetTitle, "')"))
}


occ_file <- "occurrence.txt"

#how big?
#no_lines <- R.utils::countLines(occ_file)[1]
no_lines <- 63638734

#rows at a time
no_rows <- 100000

#how many steps?
no_steps <- floor(no_lines/no_rows)

#loop to occ_file
for (i in 1:no_steps){
  cat(paste("Step ", i, "of", no_steps, "\n"))
  if (i == 1){
    gbif_data <- data.table::fread(input = occ_file, header = FALSE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", quote = "", nrows = (no_rows - 1), skip = 1)
  }else{
    skip_rows <- i * no_rows
    gbif_data <- data.table::fread(input = occ_file, header = FALSE, sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8", quote = "", nrows = no_rows, skip = skip_rows)
  }

  #Set names to df  
  names(gbif_data) <- col_names
  
  #write rows
  dbWriteTable(gbif_db, "gbif", gbif_data, append = TRUE)
  rm(gbif_data)
}

#Close db to cleanup
dbDisconnect(gbif_db)
