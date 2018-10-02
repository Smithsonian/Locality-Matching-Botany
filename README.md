# Locality-Matching-Botany

Shiny app that takes locations from transcription that are not clear and matches them to known locations. Runs an approximate match using databases from EMu and GBIF.

## Requirements

This app requires these R packages:

 * shiny
 * DT
 * dplyr
 * stringr
 * RSQLite
 * stringdist
 * futile.logger
 * countrycode
 * httr
