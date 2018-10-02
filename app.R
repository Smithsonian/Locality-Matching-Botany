
# Load packages -----------------------------------------------------------
library(shiny)
library(DT)
library(dplyr)
library(stringr)
library(RSQLite)
library(stringdist)
library(futile.logger)
library(countrycode)
library(httr)



# Settings -----------------------------------------------------------

#how many cpu cores to use for matching
this_cpu_cores <- 2

#Larger size for input file
options(shiny.maxRequestSize=10*1024^2)

app_name <- "Locality Matching"
app_ver <- "0.6.0"
github_link <- "https://github.com/Smithsonian/Locality-Matching-Botany"

options(stringsAsFactors = FALSE)
options(encoding = 'UTF-8')

#Keep the ip address
#mount_point <- "Z:\\"

#Only do locs with [*!]
unknownlocs <- TRUE

#IDS
ids_url = "http://ids-internal.si.edu/ids/dynamic?id="
ids_prefix = "NMNH-"


csv_database <- paste0("data/csv_", format(Sys.time(), "%Y%m%d%H%M%S"), ".sqlite3")


#Logfile
logfile <- paste0("logs/", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")


# Check data ----
nmnh_db_file <- "data/nmnh.sqlite"
if (file.exists(nmnh_db_file)){
  use_nmnh_db <- TRUE
}else{
  use_nmnh_db <- FALSE
}

gbif_db_file <- "data/gbif.sqlite"
if (file.exists(gbif_db_file)){
  use_gbif_db <- TRUE
}else{
  use_gbif_db <- FALSE
}








# UI -----------------------------------------------------------
ui <- fluidPage(
  
  titlePanel(app_name),
  
  tabsetPanel(type = "tabs",
              tabPanel("CSV",  
                       br(),
                       fluidRow(
                         column(width = 5, 
                                uiOutput("csv_info"),
                                uiOutput("choose_string"),
                                uiOutput("nextBin")
                         ),
                         column(width = 4,
                                uiOutput("filters")),
                         column(width = 3, 
                                uiOutput("database"))
                        ),
                       
                       hr(),
                       fluidRow(
                         column(width = 6, 
                                uiOutput("string_to_check"),
                                uiOutput("downloadData"),
                                tags$br(),
                                uiOutput("downloadData2")),
                         column(width = 6, 
                                uiOutput("ids")
                         )
                       ),
                       
                       fluidRow(
                         column(width = 4, 
                                DT::dataTableOutput("table1")
                         ),
                         column(width = 4, 
                                DT::dataTableOutput("table2")
                         ),
                         column(width = 4,
                                DT::dataTableOutput("table3")
                         )
                       )
                       
                       ),
              tabPanel("Indiv entry",  
                       
                       br(),
                       fluidRow(
                         column(width = 5, 
                                uiOutput("indiventory"),
                                uiOutput("choose_string_ind")
                         ),
                         column(width = 4,
                                #uiOutput("filters2"),
                                br()),
                         column(width = 3, 
                                uiOutput("database2"))
                       ),
                       
                       hr(),
                       fluidRow(
                         column(width = 6, 
                                uiOutput("string_to_check2")
                                ),
                         column(width = 6, 
                                uiOutput("downloadData_ind")
                         )
                       ),
                       
                       fluidRow(
                         column(width = 4, 
                                DT::dataTableOutput("table1_2")
                         ),
                         column(width = 4, 
                                DT::dataTableOutput("table2_2")
                         ),
                         column(width = 4,
                                DT::dataTableOutput("table3_2")
                         )
                       )
                       
                       
                       ),
              tabPanel("Help",  
                       br(),
                       fluidRow(
                         column(width = 6, 
                              uiOutput("help1")
                         )
                       )
              )
  ),
  
  hr(),
  HTML(paste0("<p><a href=\"http://dpo.si.edu\" target = _blank>Digitization Program Office</a> | ", app_name, " ver. ", app_ver, " | <a href=\"", github_link, "\" target = _blank>Source code</a></p>"))
  
)



# Server -----------------------------------------------------------
server <- function(input, output, session) {
  
  # Set logging ----
  dir.create('logs', showWarnings = FALSE)
  flog.logger("locations", INFO, appender=appender.file(logfile))
  
  
  
  
  
  
  
  # choose_string ----
  output$choose_string <- renderUI({
    req(input$csvinput)
    
    #Save input file for debugging
    local_inputfile <- paste0("logs/", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    file.copy(input$csvinput$datapath, local_inputfile)
    
    flog.info(paste0("inputfile: ", local_inputfile), name = "locations")
    
    csvinput <<- try(read.table(file = input$csvinput$datapath, header = TRUE, stringsAsFactors = FALSE, quote = '"', sep = ","), silent=TRUE)
    
    # Process any error messages
    if (class(csvinput) == "try-error"){
      cat("error")
      flog.error(paste0("INVALID CSV. Please check that is is saved as comma-separated and that all the strings are quoted. Details: ", csvinput), name = "locations")
    }else{
      
      #process only unknown locations
      #if (input$unknownlocs == TRUE){
      if (unknownlocs == TRUE){
        csvinput <<- dplyr::filter(csvinput, grepl('[!*]', precise_locality))
      }
      
      try(file.remove(csv_database), silent = TRUE)
      flog.info(paste0("csv_database: ", csv_database), name = "locations")
      
      #save csvinput to sqlite
      csvinput_db <- dbConnect(RSQLite::SQLite(), csv_database)
      cat("Loading table to db")
      csvinput_2db <- cbind(csvinput, "match_location" = NA, "match_country" = NA, "match_collector" = NA, "match_method" = NA, "match_score" = NA, "match_no_records" = NA, "match_db" = NA)
      n <- dbExecute(csvinput_db, 'PRAGMA encoding="UTF-8";')
      dbWriteTable(csvinput_db, "csv", csvinput_2db)
      n <- dbExecute(csvinput_db, "CREATE INDEX sheetbc ON csv(sheet_barcode);")
      dbDisconnect(csvinput_db)
      
      options_list <- as.list(paste0(csvinput$precise_locality, " (", csvinput$sheet_barcode, ")"))
      flog.info(paste0("options_list: ", options_list), name = "locations")
      
      selectInput("row", "Select location to match", options_list, width = "100%")
    }
  })
  
  


  
  
  
  # string_to_check ----
  output$string_to_check <- renderUI({
    
    req(input$row)
    
    row_split <- base::strsplit(input$row, " [(]")[[1]]
    this_row_id <- row_split[length(row_split)]
    this_row_id <- base::strsplit(this_row_id, "[)]")[[1]][1]
    
    #this_row <- csvinput[csvinput$sheet_barcode == this_row_id,]
    this_query <- paste0("SELECT * FROM csv WHERE sheet_barcode = '", this_row_id, "'")
    flog.info(paste0("this_query: ", this_query), name = "locations")
    csvinput_db <- dbConnect(RSQLite::SQLite(), csv_database)
    this_row <- dbGetQuery(csvinput_db, this_query)
    dbDisconnect(csvinput_db)
    
    cntr_alarm <- ""
    if (!is.na(this_row$country)){
      if (this_row$country == "" || this_row$country == "Unknown"){
        cntr_alarm <- " <span class=\"label label-danger\"><abbr title=\"Value may be an error, filter not used\"><span class=\"glyphicon glyphicon-exclamation-sign\" aria-hidden=\"true\"></span></abbr></span>"
      }
    }else{
      cntr_alarm <- " <span class=\"label label-danger\"><abbr title=\"Value is missing\"><span class=\"glyphicon glyphicon-exclamation-sign\" aria-hidden=\"true\"></span></abbr></span>"
    }
    
    
    year_alarm <- ""
    if (!is.na(this_row$coll_year_from)){
      if (this_row$coll_year_from == "" || as.numeric(this_row$coll_year_from) < 1700 || as.numeric(this_row$coll_year_from) > 2018){
        year_alarm <- " <span class=\"label label-danger\"><abbr title=\"Value may be an error, filter not used\"><span class=\"glyphicon glyphicon-exclamation-sign\" aria-hidden=\"true\"></span></abbr></span>"
      }
    }else{
      year_alarm <- " <span class=\"label label-danger\"><abbr title=\"Value is missing\"><span class=\"glyphicon glyphicon-exclamation-sign\" aria-hidden=\"true\"></span></abbr></span>"
    }
    
    HTML(paste0("<div class=\"panel panel-primary\"><div class=\"panel-heading\"><h3 class=\"panel-title\">Row data</h3></div><div class=\"panel-body\"><dl class=\"dl-horizontal\"><dt>precise_locality</dt><dd>", this_row$precise_locality, "</dd><dt>sheet_barcode</dt><dd>", this_row$sheet_barcode, "<dd><dt>collector_1</dt><dd>", this_row$collector_1, "</dd><dt>Country</dt><dd>", this_row$country, cntr_alarm, "</dd><dt>coll_year_from</dt><dd>", this_row$coll_year_from, year_alarm, "</dd></dl></div></div>"))
    
  })
  
  







  
  
  
  
  # IDS ----
  output$ids <- renderUI({
    req(input$row)
    
    row_split <- base::strsplit(input$row, " [(]")[[1]]
    this_row_id <- row_split[length(row_split)]
    this_row_id <- base::strsplit(this_row_id, "[)]")[[1]][1]
    
    
    #Get image from IDS
    jpg_ids = paste0(ids_url, ids_prefix, this_row_id)
    ids_error <- ""
    
    r <- httr::GET(url = jpg_ids)
    if (r$status_code == 404){
      #Did excel drop the leading 0?
      jpg_ids = paste0(ids_url, ids_prefix, "0", this_row_id)
      r1 <- httr::GET(url = jpg_ids)
      if (r1$status_code == 404){
        
        #Did excel drop the leading 00?
        jpg_ids = paste0(ids_url, ids_prefix, "00", this_row_id)
        r1 <- httr::GET(url = jpg_ids)
        if (r1$status_code == 404){
          
          #Did excel drop the leading 000?
          jpg_ids = paste0(ids_url, ids_prefix, "000", this_row_id)
          r1 <- httr::GET(url = jpg_ids)
          if (r1$status_code == 404){
            ids_error <- paste0("<p class=\"text-danger\"><strong>Error: Could not find the image. Is the sheet_barcode '", this_row_id, "' correct and the image public?</strong></p>")
            
            csvinput_db <- dbConnect(RSQLite::SQLite(), csv_database)
            path_jpg <- try(dbGetQuery(csvinput_db, paste0("SELECT path_jpg FROM csv WHERE sheet_barcode = '", this_row_id, "'")), silent = TRUE)
            dbDisconnect(csvinput_db)
            
            if (class(path_jpg)=="try-error"){
              ids_error <- paste0("<p class=\"text-danger\"><strong>Error: Could not find the image. Is the sheet_barcode '", this_row_id, "' correct and the image public?</strong></p>")
            }else{
              this_jpg <- stringr::str_replace_all(path_jpg, "#", "")
              ids_error <- paste0("<p class=\"text-danger\"><strong>Error: Could not find the image. Is the sheet_barcode '", this_row_id, "' correct and the image public?<br>", this_jpg, "</strong></p>")
            }
              
            flog.error(paste0("ids_error: ", this_row_id), name = "locations")
          }else{
            ids_error <- paste0("<p class=\"text-danger\"><strong>Note: It seems your input file dropped the leading zeroes from sheet_barcode. Trying to display the right image by appending zeroes to '", this_row_id, "'.</strong></p>")
            flog.warn(paste0("ids_error: ", this_row_id, ", but worked with appended 000"), name = "locations")
          }
          
        }else{
          ids_error <- paste0("<p class=\"text-danger\"><strong>Note: It seems your input file dropped the leading zeroes from sheet_barcode. Trying to display the right image by appending zeroes to '", this_row_id, "'.</strong></p>")
          flog.warn(paste0("ids_error: ", this_row_id, ", but worked with appended 00"), name = "locations")
        }
        
      }else{
        ids_error <- paste0("<p class=\"text-danger\"><strong>Note: It seems your input file dropped the leading zeroes from sheet_barcode. Trying to display the right image by appending zeroes to '", this_row_id, "'.</strong></p>")
        flog.warn(paste0("ids_error: ", this_row_id, ", but worked with appended 0"), name = "locations")
      }
    }
    
    flog.info(paste0("jpg_ids: ", jpg_ids), name = "locations")
    
    #from https://stackoverflow.com/a/16485533
    HTML(paste0("<div class=\"panel panel-primary\"><div class=\"panel-heading\"><h3 class=\"panel-title\">Image</h3></div><div class=\"panel-body\">", ids_error, "<br><div class=\"holds-the-iframe\"><iframe src=\"", jpg_ids, "\" width=\"100%\" height = \"460px\"></iframe></div><br><a href=\"", jpg_ids, "\" target = \"_blank\">Open in a new tab</a></div></div>
                <style>.holds-the-iframe {
  background:url(ajax-loader.gif) no-repeat;
  background-position:50px 50px;
  }</style>"))
    
  })
  
  
  
  
  
  
  # Filters ----
  output$countryfilterbox <- renderUI({
    checkboxInput("countryfilter", "Country", TRUE)
  })
  
  output$yearfilterbox <- renderUI({
    checkboxInput("yearfilter", "Year", TRUE)
  })
  
  output$collectorfilterbox <- renderUI({
    checkboxInput("collectorfilter", "Collector", TRUE)
  })
  
  output$filters <- renderUI({
    req(input$csvinput)
    tagList(
      HTML("<div class=\"panel panel-info\"><div class=\"panel-heading\"><h3 class=\"panel-title\">Filters</h3></div><div class=\"panel-body\">"),
      fluidRow(         
        column(width = 3, p(strong("Filters:"))),
        column(width = 3, uiOutput("countryfilterbox")),
        column(width = 3, uiOutput("collectorfilterbox")),
        column(width = 3, uiOutput("yearfilterbox")),
        HTML("</div></div>")
      )
    )
  })
  
  
  



  
  
  
  # Which DB ----
  output$which_db <- renderUI({
    choices <- list()
    if (use_nmnh_db){
      choices[1] <- c("NMNH")
    }
    if (use_gbif_db){
      choices[2] <- c("GBIF")
    }
    radioButtons("db_to_use", "Which database to use", choices = choices, inline = TRUE)
  })
  
  output$database <- renderUI({
    req(input$csvinput)
    tagList(
      HTML("<div class=\"panel panel-info\"><div class=\"panel-heading\"><h3 class=\"panel-title\">Database</h3></div><div class=\"panel-body\">"),
      uiOutput("which_db"),
      HTML("</div></div>")
    )
  })
  
  
  





  
  
  
  
  # Table 1 ----
  output$table1 <- DT::renderDataTable({
    
    req(input$row)
    req(input$db_to_use)
    
    flog.info(paste0("input$row: ", input$row), name = "locations")
    flog.info(paste0("input$db_to_use: ", input$db_to_use), name = "locations")
    
    #Progress bar
    progress0 <- shiny::Progress$new()
    on.exit(progress0$close())
    
    progress0$set(message = 'Querying database for candidate matches...', value = 0.3)
    
    source("find_matching_str.R")
    
    #method <- input$method
    str_method <- "osa"
    threshold <- 10
    
    this_row_id <- base::strsplit(input$row, " [(]")[[1]]
    this_row_id <- this_row_id[length(this_row_id)]
    this_row_id <- base::strsplit(this_row_id, "[)]")[[1]][1]
    
    csvinput_db <- dbConnect(RSQLite::SQLite(), csv_database)
    this_row <- dbGetQuery(csvinput_db, paste0("SELECT * FROM csv WHERE sheet_barcode = '", this_row_id, "'"))
    dbDisconnect(csvinput_db)
    
    this_year <- this_row$coll_year_from
    this_country <- this_row$country
    this_collector <- this_row$collector_1

    year_filter <- input$yearfilter
    country_filter <- input$countryfilter
    collector_filter <- input$collectorfilter
    
    if (is.na(this_year) || this_year == ""){
      year_filter <- FALSE
    }
     
    if (this_country == "" || this_country == "Unknown"){
      country_filter <- FALSE
    }
    
    if (this_collector == "" || this_collector == "Unknown"){
      collector_filter <- FALSE
    }
    
    
    
    if (input$db_to_use == "NMNH"){
      db <- "data/nmnh.sqlite"
      
      #Collector, also include partial match
      if (grepl("[,]", this_collector)){
        this_collector_partial <- strsplit(this_collector, ",")[[1]][1]
      }else{
        this_collector_part <- gsub("[^A-Za-z0-9 ]", "", this_collector)
        this_collector_part <- strsplit(this_collector_part, " ")[[1]]
        
        this_collector_partial <- this_collector_part[1]
        no_parts <- length(this_collector_part)
        #Select the longest word
        if (no_parts > 1){
          for (n in 1:length(this_collector_part)){
            if (nchar(this_collector_part[n]) > nchar(this_collector_partial)){
              this_collector_partial <- this_collector_part[n]
            }
          }
        }
      }
      
      collector_query <- paste0("(collector IN (select collector from collectors where irn in (select irn from collectors where lower(collector) = lower('", this_collector, "'))) OR lower(collector) LIKE lower('%", this_collector_partial, "%'))")
      
      
      this_query <- "SELECT location, ID FROM locations "
      
      if (year_filter && country_filter && collector_filter){
        this_query <- paste0(this_query, " WHERE country = '", this_country, "' AND year LIKE '%", this_year, "' AND ", collector_query)
      }else if (year_filter && country_filter && collector_filter == FALSE){
        this_query <- paste0(this_query, " WHERE country = '", this_country, "' AND year LIKE '%", this_year, "'")
      }else if (year_filter && country_filter == FALSE && collector_filter){
        this_query <- paste0(this_query, " WHERE year LIKE '%", this_year, "' AND ", collector_query)
      }else if (year_filter == FALSE && country_filter && collector_filter){
        this_query <- paste0(this_query, " WHERE country = '", this_country, "' AND ", collector_query)
      }else if (year_filter && country_filter == FALSE && collector_filter == FALSE){
        this_query <- paste0(this_query, " WHERE year LIKE '%", this_year, "'")
      }else if (year_filter == FALSE && country_filter == FALSE && collector_filter){
        this_query <- paste0(this_query, " WHERE ", collector_query)
      }else if (year_filter == FALSE && country_filter && collector_filter == FALSE){
        this_query <- paste0(this_query, " WHERE country = '", this_country, "'")
      }
      
    }else if (input$db_to_use == "GBIF"){
      
      db <- "data/gbif.sqlite"
      
      #TRANSLATE USING COUNTRYCODE
      #countrycode::countrycode(this_country, "country.name", "iso2c")
      
      if (country_filter){
        countryCode <- countrycode::countrycode(this_country, "country.name", "iso2c")
        if (is.na(countryCode)){
          #country_filter <- FALSE
          countryCode <- "Unknown"
        }
      }
      
      # * Temp fix for GBIF ----
      # To avoid trying to search against the whole database
      country_filter <- TRUE
      
      #Collector, also include partial match
      if (grepl("[,]", this_collector)){
        this_collector_partial <- strsplit(this_collector, ",")[[1]][1]
      }else{
        this_collector_part <- gsub("[^A-Za-z0-9 ]", "", this_collector)
        this_collector_part <- strsplit(this_collector_part, " ")[[1]]
        
        this_collector_partial <- this_collector_part[1]
        no_parts <- length(this_collector_part)
        #Select the longest word
        if (no_parts > 1){
          for (n in 1:length(this_collector_part)){
            if (nchar(this_collector_part[n]) > nchar(this_collector_partial)){
              this_collector_partial <- this_collector_part[n]
            }
          }
        }
      }
      
      collector_query <- paste0(" (lower(recordedBy) = lower('", this_collector, "') OR lower(recordedBy) LIKE lower('%", this_collector_partial, "%'))")
      
      this_query <- "SELECT locality as location, sum(no_records) AS no_records, min(ROWID) as ROWID FROM gbif "
      
      if (year_filter && country_filter && collector_filter){
        this_query <- paste0(this_query, " WHERE countryCode = '", countryCode, "' AND year LIKE '%", this_year, "' AND ", collector_query)
      }else if (year_filter && country_filter && collector_filter == FALSE){
        this_query <- paste0(this_query, " WHERE countryCode = '", countryCode, "' AND year LIKE '%", this_year, "'")
      }else if (year_filter && country_filter == FALSE && collector_filter){
        this_query <- paste0(this_query, " WHERE year LIKE '%", this_year, "' AND ", collector_query)
      }else if (year_filter == FALSE && country_filter && collector_filter){
        this_query <- paste0(this_query, " WHERE countryCode = '", countryCode, "' AND ", collector_query)
      }else if (year_filter && country_filter == FALSE && collector_filter == FALSE){
        this_query <- paste0(this_query, " WHERE year LIKE '%", this_year, "'")
      }else if (year_filter == FALSE && country_filter == FALSE && collector_filter){
        this_query <- paste0(this_query, " WHERE ", collector_query)
      }else if (year_filter == FALSE && country_filter && collector_filter == FALSE){
        this_query <- paste0(this_query, " WHERE countryCode = '", countryCode, "'")
      }

    }
    
    this_query <- paste0(this_query, " GROUP BY location")
    
    progress0$set(message = 'Querying database for candidate matches...', value = 0.4)
    
    loc_db <<- dbConnect(RSQLite::SQLite(), db)
  
    flog.info(paste0("precise_locality: ", this_row$precise_locality), name = "locations")
    flog.info(paste0("table_query: ", this_query), name = "locations")
    flog.info(paste0("db: ", input$db_to_use), name = "locations")
    
    progress0$set(message = 'Querying database for candidate matches...', value = 0.5)
    
    locations_db <<- dbGetQuery(loc_db, this_query)
    
    progress0$set(message = 'Querying database for candidate matches...', value = 0.9)
    progress0$close()
    #Progress bar
    progress1 <- shiny::Progress$new()
    on.exit(progress1$close())
    
    progress1$set(message = 'Searching for matches using method "osa"...', value = 0.3)
    
    if (input$db_to_use == "NMNH"){
      loc_match <- find_matching_str(this_row$precise_locality, database = locations_db, method = str_method, no_cores = this_cpu_cores, db = "nmnh")
      results <- data.frame(match = loc_match$match, ID = as.numeric(loc_match$ID), score = as.numeric(loc_match$score), stringsAsFactors = FALSE)
      results_filtered <- dplyr::filter(results, score < threshold)
      results1 <<- results_filtered[order(results_filtered$score),]
      
      results1_table <- data.frame(match = results1$match, score = as.numeric(results1$score), stringsAsFactors = FALSE)
      
      DT::datatable(results1_table, escape = FALSE, options = list(searching = FALSE, ordering = FALSE, pageLength = 15, paging = FALSE, language = list(zeroRecords = "No matches found")), rownames = FALSE, selection = 'single', caption = paste0("Method: ", str_method), colnames = c('String matched', 'Score (lower is better)'))
      
    }else if (input$db_to_use == "GBIF"){
      loc_match <- find_matching_str(this_row$precise_locality, database = locations_db, method = str_method, no_cores = this_cpu_cores, db = "gbif")
      results <- data.frame(match = loc_match$match, no_records = as.numeric(loc_match$no_records), ROWID = as.numeric(loc_match$ROWID), score = as.numeric(loc_match$score), stringsAsFactors = FALSE)
      results_filtered <- dplyr::filter(results, score < threshold)
      results1 <<- results_filtered[order(results_filtered$score, -results_filtered$no_records),]
      
      results1_table <- data.frame(match = results1$match, no_records = as.numeric(results1$no_records), score = as.numeric(results1$score), stringsAsFactors = FALSE)
      
      DT::datatable(results1_table, escape = FALSE, options = list(searching = FALSE, ordering = FALSE, pageLength = 15, paging = FALSE, language = list(zeroRecords = "No matches found")), rownames = FALSE, selection = 'single', caption = paste0("Method: ", str_method), colnames = c('String matched', 'No. records', 'Score (lower is better)'))
      
      }
    
  })
  
  
  
  
  # Table 2 ----
  output$table2 <- DT::renderDataTable({
    
    req(input$row)
    req(input$db_to_use)
    
    #Progress bar
    progress2 <- shiny::Progress$new()
    on.exit(progress2$close())
    
    progress2$set(message = 'Searching for matches using method "dl"...', value = 0.3)
    
    source("find_matching_str.R")
    
    #method <- input$method
    str_method <- "dl"
    threshold <- 10
    
    this_row_id <- base::strsplit(input$row, " [(]")[[1]]
    this_row_id <- this_row_id[length(this_row_id)]
    this_row_id <- base::strsplit(this_row_id, "[)]")[[1]][1]
    
    csvinput_db <- dbConnect(RSQLite::SQLite(), csv_database)
    this_row <- dbGetQuery(csvinput_db, paste0("SELECT * FROM csv WHERE sheet_barcode = '", this_row_id, "'"))
    dbDisconnect(csvinput_db)
    
    
    
    progress2$set(message = 'Searching for matches using method "dl"...', value = 0.7)
      
    if (input$db_to_use == "NMNH"){
      loc_match <- find_matching_str(this_row$precise_locality, database = locations_db, method = str_method, no_cores = this_cpu_cores, db = "nmnh")
      results <- data.frame(match = loc_match$match, ID = as.numeric(loc_match$ID), score = as.numeric(loc_match$score), stringsAsFactors = FALSE)
      results_filtered <- dplyr::filter(results, score < threshold)
      results2 <<- results_filtered[order(results_filtered$score),]
      
      results2_table <- data.frame(match = results2$match, score = as.numeric(results2$score), stringsAsFactors = FALSE)
      
      DT::datatable(results2_table, escape = FALSE, options = list(searching = FALSE, ordering = FALSE, pageLength = 15, paging = FALSE, language = list(zeroRecords = "No matches found")), rownames = FALSE, selection = 'single', caption = paste0("Method: ", str_method), colnames = c('String matched', 'Score (lower is better)'))
      
    }else if (input$db_to_use == "GBIF"){
      loc_match <- find_matching_str(this_row$precise_locality, database = locations_db, method = str_method, no_cores = this_cpu_cores, db = "gbif")
      results <- data.frame(match = loc_match$match, no_records = as.numeric(loc_match$no_records), ROWID = as.numeric(loc_match$ROWID), score = as.numeric(loc_match$score), stringsAsFactors = FALSE)
      results_filtered <- dplyr::filter(results, score < threshold)
      results2 <<- results_filtered[order(results_filtered$score, -results_filtered$no_records),]
      
      results2_table <- data.frame(match = results2$match, no_records = as.numeric(results2$no_records), score = as.numeric(results2$score), stringsAsFactors = FALSE)
      
      DT::datatable(results2_table, escape = FALSE, options = list(searching = FALSE, ordering = FALSE, pageLength = 15, paging = FALSE, language = list(zeroRecords = "No matches found")), rownames = FALSE, selection = 'single', caption = paste0("Method: ", str_method), colnames = c('String matched', 'No. records', 'Score (lower is better)'))
      
    }
    
  })
  
  
  
  # Table 3 ----
  output$table3 <- DT::renderDataTable({
    
    req(input$row)
    req(input$db_to_use)
    
    #Progress bar
    progress3 <- shiny::Progress$new()
    on.exit(progress3$close())
    
    progress3$set(message = 'Searching for matches using method "jw"...', value = 0.3)
    
    source("find_matching_str.R")
    
    #method <- input$method
    str_method <- "jw"
    threshold <- 0.2
    
    this_row_id <- base::strsplit(input$row, " [(]")[[1]]
    this_row_id <- this_row_id[length(this_row_id)]
    this_row_id <- base::strsplit(this_row_id, "[)]")[[1]][1]
    
    csvinput_db <- dbConnect(RSQLite::SQLite(), csv_database)
    this_row <- dbGetQuery(csvinput_db, paste0("SELECT * FROM csv WHERE sheet_barcode = '", this_row_id, "'"))
    dbDisconnect(csvinput_db)
    
          
    progress3$set(message = 'Searching for matches using method "jw"...', value = 0.7)
    
    if (input$db_to_use == "NMNH"){
      loc_match <- find_matching_str(this_row$precise_locality, database = locations_db, method = str_method, no_cores = this_cpu_cores, db = "nmnh")
      results <- data.frame(match = loc_match$match, ID = as.numeric(loc_match$ID), score = round(as.numeric(loc_match$score), 4), stringsAsFactors = FALSE)
      results_filtered <- dplyr::filter(results, score < threshold)
      results3 <<- results_filtered[order(results_filtered$score),]
      
      results3_table <- data.frame(match = results3$match, score = as.numeric(results3$score), stringsAsFactors = FALSE)
      
      DT::datatable(results3_table, escape = FALSE, options = list(searching = FALSE, ordering = FALSE, pageLength = 15, paging = FALSE, language = list(zeroRecords = "No matches found")), rownames = FALSE, selection = 'single', caption = paste0("Method: ", str_method), colnames = c('String matched', 'Score (lower is better)'))
      
    }else if (input$db_to_use == "GBIF"){
      loc_match <- find_matching_str(this_row$precise_locality, database = locations_db, method = str_method, no_cores = this_cpu_cores, db = "gbif")
      results <- data.frame(match = loc_match$match, no_records = as.numeric(loc_match$no_records), ROWID = as.numeric(loc_match$ROWID), score = round(as.numeric(loc_match$score), 4), stringsAsFactors = FALSE)
      results_filtered <- dplyr::filter(results, score < threshold)
      results3 <<- results_filtered[order(results_filtered$score, -results_filtered$no_records),]
      
      results3_table <- data.frame(match = results3$match, no_records = as.numeric(results3$no_records), score = as.numeric(results3$score), stringsAsFactors = FALSE)
      
      DT::datatable(results3_table, escape = FALSE, options = list(searching = FALSE, ordering = FALSE, pageLength = 15, paging = FALSE, language = list(zeroRecords = "No matches found")), rownames = FALSE, selection = 'single', caption = paste0("Method: ", str_method), colnames = c('String matched', 'No. records', 'Score (lower is better)'))
      
    }
    
  })
  
  
  
  
  








  






  
  
  
  # downloadData ----
  output$downloadData <- renderUI({
    req(input$row)
    
    res <- ""
    
      if(!is.null(input$table1_rows_selected)){
        if (input$db_to_use == "NMNH"){
            res <- results1[input$table1_rows_selected, 1]
            score <- results1[input$table1_rows_selected, 3]
            id <- results1[input$table1_rows_selected, 2]
            no_records <- ""
        }else if (input$db_to_use == "GBIF"){
            res <- results1[input$table1_rows_selected, 1]
            no_records <- results1[input$table1_rows_selected, 2]
            id <- results1[input$table1_rows_selected, 3]
            score <- results1[input$table1_rows_selected, 4]
        }
        
        method <- "osa"
      }else if(!is.null(input$table2_rows_selected)){
        if (input$db_to_use == "NMNH"){
          res <- results2[input$table2_rows_selected, 1]
          score <- results2[input$table2_rows_selected, 3]
          id <- results2[input$table2_rows_selected, 2]
          no_records <- ""
        }else if (input$db_to_use == "GBIF"){
          res <- results2[input$table2_rows_selected, 1]
          no_records <- results2[input$table2_rows_selected, 2]
          id <- results2[input$table2_rows_selected, 3]
          score <- results2[input$table2_rows_selected, 4]
        }
        method <- "dl"
      }else if(!is.null(input$table3_rows_selected)){
        if (input$db_to_use == "NMNH"){
          res <- results3[input$table3_rows_selected, 1]
          score <- results3[input$table3_rows_selected, 3]
          id <- results3[input$table3_rows_selected, 2]
          no_records <- ""
        }else if (input$db_to_use == "GBIF"){
          res <- results3[input$table3_rows_selected, 1]
          no_records <- results3[input$table3_rows_selected, 2]
          id <- results3[input$table3_rows_selected, 3]
          score <- results3[input$table3_rows_selected, 4]
        }
        
        method <- "jw"
      }
    
    if (res != ""){
      
      #Get details for the location
      if (input$db_to_use == "NMNH"){
        
        res_country <- dbGetQuery(loc_db, paste0("SELECT country, collector FROM locations WHERE ID = ", id))
        res_collector <-  res_country[2]
        res_country <-  res_country[1]

      }else if (input$db_to_use == "GBIF"){
        site_query <- paste0("SELECT countryCode, recordedBy FROM gbif WHERE ROWID = ", id)
        flog.info(paste0("query:", site_query), name = "locations")
        this_country <- dbGetQuery(loc_db, site_query)
        res_country <- countrycode::countrycode(as.character(this_country[1]), "iso2c", "country.name")
        res_collector <- this_country[2]
        
        }
      
      
      row_split <- base::strsplit(input$row, " [(]")[[1]]
      this_row_id <- row_split[length(row_split)]
      this_row_id <- base::strsplit(this_row_id, "[)]")[[1]][1]
      
      
      flog.info(paste0("this_row_id:", this_row_id), name = "locations")
      flog.info(paste0("res:", res), name = "locations")
      flog.info(paste0("country:", res_country), name = "locations")
      flog.info(paste0("collector:", res_collector), name = "locations")
      flog.info(paste0("table_id:", id), name = "locations")
      flog.info(paste0("method:", method), name = "locations")
      flog.info(paste0("score:", score), name = "locations")
      flog.info(paste0("no_records:", no_records), name = "locations")
      
      selected_row <<- c("sheet_barcode" = this_row_id, "location" = res, "country" = res_country, "collector" = res_collector, "method" = method, "score" = score, "no_records" = no_records, "db" = input$db_to_use)
      
      tagList(
        HTML("<div class=\"panel panel-success\"> <div class=\"panel-heading\"> <h3 class=\"panel-title\">Result selected</h3> </div> <div class=\"panel-body\">"),
        pre(paste0("Locality: ", res, "\nCountry: ", res_country, "\nCollector: ", res_collector,"\nMethod (and score): ", method, " (", score, ")\nNo. of records: ", no_records)),
        # Save button
        actionButton("saverow", "Save this location", class = "btn btn-primary", icon = icon("ok", lib = "glyphicon")),
        HTML("</div></div>")
        
      )
    }
    
    })
  
  
  
  
  
  # Save results to db ----
  observeEvent(input$saverow, {
    req(input$csvinput)
    req(selected_row)
    
    query <- paste0("UPDATE csv SET match_location = '", selected_row[2], "', match_country = '", selected_row[3], "', match_collector = '", selected_row[4], "', match_method = '", selected_row[5], "', match_score = '", selected_row[6], "', match_no_records = '", selected_row[7], "', match_db = '" , selected_row[8], "' WHERE sheet_barcode = '", selected_row[1], "'")
    
    csvinput_db <- dbConnect(RSQLite::SQLite(), csv_database)
    n <- dbExecute(csvinput_db, query)
    dbDisconnect(csvinput_db)
    
  })
  
  
  
  
  
  # ind_iventory ----
  output$indiventory <- renderUI({
    tagList(
      #numericInput("sheet_barcode_ind", "sheet_barcode", value = 0),
      textInput("precise_locality_ind", "precise_locality"),
      numericInput("coll_year_from_ind", "coll_year_from", value = as.integer(format(Sys.Date(), "%Y")), min = 1750, max = as.integer(format(Sys.Date(), "%Y"))),
      textInput("collector_1_ind", "collector_1"),
      textInput("country_ind", "country"),

      actionButton("submit_ind", "Submit")
    )
  })
  
  # Ind query react ----
  observeEvent(input$submit_ind, {
    
    #save input to vector
    ind_input <<- c(precise_locality = input$precise_locality_ind, coll_year_from = input$coll_year_from_ind, collector_1 = input$collector_1_ind, country = input$country_ind)
    
    output$choose_string_ind <- renderUI({
    
      flog.info(paste0("sheet_barcode: ind_entry"), name = "locations")
      flog.info(paste0("precise_locality: ", ind_input['precise_locality']), name = "locations")
      
      p(paste("Location to match: ", ind_input['precise_locality']))
    })
    


    # Which DB2 ----
    output$which_db2 <- renderUI({
      choices <- list()
      if (use_nmnh_db){
        choices[1] <- c("NMNH")
      }
      if (use_gbif_db){
        choices[2] <- c("GBIF")
      }
      radioButtons("db_to_use2", "Which database to use", choices = choices, inline = TRUE)
    })
    
    output$database2 <- renderUI({
      tagList(
        HTML("<div class=\"panel panel-info\"><div class=\"panel-heading\"><h3 class=\"panel-title\">Database</h3></div><div class=\"panel-body\">"),
        uiOutput("which_db2"),
        HTML("</div></div>")
      )
    })
    



    # string_to_check2 ----
    output$string_to_check2 <- renderUI({
            
      HTML(paste0("<div class=\"panel panel-primary\"><div class=\"panel-heading\"><h3 class=\"panel-title\">Row data</h3></div><div class=\"panel-body\"><dl class=\"dl-horizontal\"><dt>precise_locality</dt><dd>", ind_input['precise_locality'], "</dd><dt>collector_1</dt><dd>", ind_input['collector_1'], "</dd><dt>Country</dt><dd>", ind_input['country'], "</dd><dt>coll_year_from</dt><dd>", ind_input['coll_year_from'], "</dd></dl></div></div>"))
      
    })


    
    
    
    
    
    # downloadData ----
    output$downloadData_ind <- renderUI({

      res <- ""
      
      if(!is.null(input$table1_2_rows_selected)){
        row_sel <- input$table1_2_rows_selected
        if (input$db_to_use2 == "NMNH"){
          res <- results1[row_sel, 1]
          score <- results1[row_sel, 3]
          id <- results1[row_sel, 2]
          no_records <- ""
        }else if (input$db_to_use2 == "GBIF"){
          res <- results1[row_sel, 1]
          no_records <- results1[row_sel, 2]
          id <- results1[row_sel, 3]
          score <- results1[row_sel, 4]
        }
        
        method <- "osa"
      }else if(!is.null(input$table2_2_rows_selected)){
        row_sel2 <- input$table2_2_rows_selected
        if (input$db_to_use2 == "NMNH"){
          res <- results2[row_sel2, 1]
          score <- results2[row_sel2, 3]
          id <- results2[row_sel2, 2]
          no_records <- ""
        }else if (input$db_to_use2 == "GBIF"){
          res <- results2[row_sel2, 1]
          no_records <- results2[row_sel2, 2]
          id <- results2[row_sel2, 3]
          score <- results2[row_sel2, 4]
        }
        method <- "dl"
      }else if(!is.null(input$table3_rows_selected)){
        row_sel3 <- input$table3_2_rows_selected
        if (input$db_to_use2 == "NMNH"){
          res <- results3[row_sel3, 1]
          score <- results3[row_sel3, 3]
          id <- results3[row_sel3, 2]
          no_records <- ""
        }else if (input$db_to_use2 == "GBIF"){
          res <- results3[row_sel3, 1]
          no_records <- results3[row_sel3, 2]
          id <- results3[row_sel3, 3]
          score <- results3[row_sel3, 4]
        }
        
        method <- "jw"
      }
      
      if (res != ""){
        
        #Get details for the location
        if (input$db_to_use2 == "NMNH"){
          
          res_country <- dbGetQuery(loc_db, paste0("SELECT country, collector FROM locations WHERE ID = ", id))
          res_collector <-  res_country[2]
          res_country <-  res_country[1]
          
        }else if (input$db_to_use2 == "GBIF"){
          site_query <- paste0("SELECT countryCode, recordedBy FROM gbif WHERE ROWID = ", id)
          flog.info(paste0("query:", site_query), name = "locations")
          this_country <- dbGetQuery(loc_db, site_query)
          res_country <- countrycode::countrycode(as.character(this_country[1]), "iso2c", "country.name")
          res_collector <- this_country[2]
          
        }
        
        
        # row_split <- base::strsplit(input$row, " [(]")[[1]]
        # this_row_id <- row_split[length(row_split)]
        # this_row_id <- base::strsplit(this_row_id, "[)]")[[1]][1]
        
        
        #flog.info(paste0("this_row_id:", this_row_id), name = "locations")
        flog.info(paste0("res:", res), name = "locations")
        flog.info(paste0("country:", res_country), name = "locations")
        flog.info(paste0("collector:", res_collector), name = "locations")
        flog.info(paste0("table_id:", id), name = "locations")
        flog.info(paste0("method:", method), name = "locations")
        flog.info(paste0("score:", score), name = "locations")
        flog.info(paste0("no_records:", no_records), name = "locations")
        
        #selected_row <<- c("sheet_barcode" = this_row_id, "location" = res, "country" = res_country, "collector" = res_collector, "method" = method, "score" = score, "no_records" = no_records, "db" = input$db_to_use)
        
        tagList(
          HTML("<div class=\"panel panel-success\"> <div class=\"panel-heading\"> <h3 class=\"panel-title\">Result selected</h3> </div> <div class=\"panel-body\">"),
          pre(paste0("Locality: ", res, "\nCountry: ", res_country, "\nCollector: ", res_collector,"\nMethod (and score): ", method, " (", score, ")\nNo. of records: ", no_records)),
          # Save button
          #actionButton("saverow", "Save this location", class = "btn btn-primary", icon = icon("ok", lib = "glyphicon")),
          HTML("</div></div>")
          
        )
      }
      
    })
    
    
    
    
    
    


    # Table 1_2 ----
    output$table1_2 <- DT::renderDataTable({
            
      req(input$db_to_use2)
      #Progress bar
      progress0 <- shiny::Progress$new()
      on.exit(progress0$close())
      
      progress0$set(message = 'Querying database for candidate matches...', value = 0.3)
      
      source("find_matching_str.R")
      
      #method <- input$method
      str_method <- "osa"
      threshold <- 10
      
      this_year <- ind_input['coll_year_from']
      this_country <- ind_input['country']
      this_collector <- ind_input['collector_1']

      year_filter <- TRUE
      if (!is.na(this_year)){
        if (this_year == ""){
          year_filter <- FALSE
        }
      }else{
        year_filter <- FALSE
      }
       
      country_filter <- TRUE
      if (!is.na(this_country)){
        if (this_country == ""){
          country_filter <- FALSE
        }
      }else{
        country_filter <- FALSE
      }
      
      collector_filter <- TRUE
      if (!is.na(this_country)){
        if (this_collector == ""){
          collector_filter <- FALSE
        }
      }else{
        collector_filter <- FALSE
      }
      
      
      if (input$db_to_use2 == "NMNH"){
        db <- "data/nmnh.sqlite"
        
        #Collector, also include partial match
        if (grepl("[,]", this_collector)){
          this_collector_partial <- strsplit(this_collector, ",")[[1]][1]
        }else{
          this_collector_part <- gsub("[^A-Za-z0-9 ]", "", this_collector)
          this_collector_part <- strsplit(this_collector_part, " ")[[1]]
          
          this_collector_partial <- this_collector_part[1]
          no_parts <- length(this_collector_part)
          #Select the longest word
          if (no_parts > 1){
            for (n in 1:length(this_collector_part)){
              if (nchar(this_collector_part[n]) > nchar(this_collector_partial)){
                this_collector_partial <- this_collector_part[n]
              }
            }
          }
        }
        
        collector_query <- paste0("(collector IN (select collector from collectors where irn in (select irn from collectors where lower(collector) = lower('", this_collector, "'))) OR lower(collector) LIKE lower('%", this_collector_partial, "%'))")
        
        
        this_query <- "SELECT location, ID FROM locations "
        
        if (year_filter && country_filter && collector_filter){
          this_query <- paste0(this_query, " WHERE country = '", this_country, "' AND year LIKE '%", this_year, "' AND ", collector_query)
        }else if (year_filter && country_filter && collector_filter == FALSE){
          this_query <- paste0(this_query, " WHERE country = '", this_country, "' AND year LIKE '%", this_year, "'")
        }else if (year_filter && country_filter == FALSE && collector_filter){
          this_query <- paste0(this_query, " WHERE year LIKE '%", this_year, "' AND ", collector_query)
        }else if (year_filter == FALSE && country_filter && collector_filter){
          this_query <- paste0(this_query, " WHERE country = '", this_country, "' AND ", collector_query)
        }else if (year_filter && country_filter == FALSE && collector_filter == FALSE){
          this_query <- paste0(this_query, " WHERE year LIKE '%", this_year, "'")
        }else if (year_filter == FALSE && country_filter == FALSE && collector_filter){
          this_query <- paste0(this_query, " WHERE ", collector_query)
        }else if (year_filter == FALSE && country_filter && collector_filter == FALSE){
          this_query <- paste0(this_query, " WHERE country = '", this_country, "'")
        }
        
      }else if (input$db_to_use2 == "GBIF"){
        
        db <- "data/gbif.sqlite"
        
        #TRANSLATE USING COUNTRYCODE
        #countrycode::countrycode(this_country, "country.name", "iso2c")
        
        if (country_filter){
          countryCode <- countrycode::countrycode(this_country, "country.name", "iso2c")
          if (is.na(countryCode)){
            #country_filter <- FALSE
            countryCode <- "Unknown"
          }
        }
        
        # * Temp fix for GBIF ----
        # To avoid trying to search against the whole database
        country_filter <- TRUE
        
        #Collector, also include partial match
        if (grepl("[,]", this_collector)){
          this_collector_partial <- strsplit(this_collector, ",")[[1]][1]
        }else{
          this_collector_part <- gsub("[^A-Za-z0-9 ]", "", this_collector)
          this_collector_part <- strsplit(this_collector_part, " ")[[1]]
          
          this_collector_partial <- this_collector_part[1]
          no_parts <- length(this_collector_part)
          #Select the longest word
          if (no_parts > 1){
            for (n in 1:length(this_collector_part)){
              if (nchar(this_collector_part[n]) > nchar(this_collector_partial)){
                this_collector_partial <- this_collector_part[n]
              }
            }
          }
        }
        
        collector_query <- paste0(" (lower(recordedBy) = lower('", this_collector, "') OR lower(recordedBy) LIKE lower('%", this_collector_partial, "%'))")
        
        this_query <- "SELECT locality as location, sum(no_records) AS no_records, min(ROWID) as ROWID FROM gbif "
        
        if (year_filter && country_filter && collector_filter){
          this_query <- paste0(this_query, " WHERE countryCode = '", countryCode, "' AND year LIKE '%", this_year, "' AND ", collector_query)
        }else if (year_filter && country_filter && collector_filter == FALSE){
          this_query <- paste0(this_query, " WHERE countryCode = '", countryCode, "' AND year LIKE '%", this_year, "'")
        }else if (year_filter && country_filter == FALSE && collector_filter){
          this_query <- paste0(this_query, " WHERE year LIKE '%", this_year, "' AND ", collector_query)
        }else if (year_filter == FALSE && country_filter && collector_filter){
          this_query <- paste0(this_query, " WHERE countryCode = '", countryCode, "' AND ", collector_query)
        }else if (year_filter && country_filter == FALSE && collector_filter == FALSE){
          this_query <- paste0(this_query, " WHERE year LIKE '%", this_year, "'")
        }else if (year_filter == FALSE && country_filter == FALSE && collector_filter){
          this_query <- paste0(this_query, " WHERE ", collector_query)
        }else if (year_filter == FALSE && country_filter && collector_filter == FALSE){
          this_query <- paste0(this_query, " WHERE countryCode = '", countryCode, "'")
        }

      }
      
      this_query <- paste0(this_query, " GROUP BY location")
      
      progress0$set(message = 'Querying database for candidate matches...', value = 0.4)
      
      loc_db <<- dbConnect(RSQLite::SQLite(), db)
    
      flog.info(paste0("precise_locality: ", ind_input['precise_locality']), name = "locations")
      flog.info(paste0("table_query: ", this_query), name = "locations")
      flog.info(paste0("db: ", input$db_to_use2), name = "locations")
      
      progress0$set(message = 'Querying database for candidate matches...', value = 0.5)
      
      locations_db <<- dbGetQuery(loc_db, this_query)
      
      progress0$set(message = 'Querying database for candidate matches...', value = 0.9)
      progress0$close()
      #Progress bar
      progress1 <- shiny::Progress$new()
      on.exit(progress1$close())
      
      progress1$set(message = 'Searching for matches using method "osa"...', value = 0.3)
      
      if (input$db_to_use2 == "NMNH"){
        loc_match <- find_matching_str(ind_input['precise_locality'], database = locations_db, method = str_method, no_cores = this_cpu_cores, db = "nmnh")
        results <- data.frame(match = loc_match$match, ID = as.numeric(loc_match$ID), score = as.numeric(loc_match$score), stringsAsFactors = FALSE)
        results_filtered <- dplyr::filter(results, score < threshold)
        results1 <<- results_filtered[order(results_filtered$score),]
        
        results1_table <- data.frame(match = results1$match, score = as.numeric(results1$score), stringsAsFactors = FALSE)
        
        DT::datatable(results1_table, escape = FALSE, options = list(searching = FALSE, ordering = FALSE, pageLength = 15, paging = FALSE, language = list(zeroRecords = "No matches found")), rownames = FALSE, selection = 'single', caption = paste0("Method: ", str_method), colnames = c('String matched', 'Score (lower is better)'))
        
      }else if (input$db_to_use2 == "GBIF"){
        loc_match <- find_matching_str(ind_input['precise_locality'], database = locations_db, method = str_method, no_cores = this_cpu_cores, db = "gbif")
        results <- data.frame(match = loc_match$match, no_records = as.numeric(loc_match$no_records), ROWID = as.numeric(loc_match$ROWID), score = as.numeric(loc_match$score), stringsAsFactors = FALSE)
        results_filtered <- dplyr::filter(results, score < threshold)
        results1 <<- results_filtered[order(results_filtered$score, -results_filtered$no_records),]
        
        results1_table <- data.frame(match = results1$match, no_records = as.numeric(results1$no_records), score = as.numeric(results1$score), stringsAsFactors = FALSE)
        
        DT::datatable(results1_table, escape = FALSE, options = list(searching = FALSE, ordering = FALSE, pageLength = 15, paging = FALSE, language = list(zeroRecords = "No matches found")), rownames = FALSE, selection = 'single', caption = paste0("Method: ", str_method), colnames = c('String matched', 'No. records', 'Score (lower is better)'))
        
        }
      
    })
    
    
    
    
    # Table 2_2 ----
    output$table2_2 <- DT::renderDataTable({
      
      req(input$db_to_use2)
      #Progress bar
      progress2 <- shiny::Progress$new()
      on.exit(progress2$close())
      
      progress2$set(message = 'Searching for matches using method "dl"...', value = 0.3)
      
      source("find_matching_str.R")
      
      #method <- input$method
      str_method <- "dl"
      threshold <- 10
      
      progress2$set(message = 'Searching for matches using method "dl"...', value = 0.7)
        
      if (input$db_to_use2 == "NMNH"){
        loc_match <- find_matching_str(ind_input['precise_locality'], database = locations_db, method = str_method, no_cores = this_cpu_cores, db = "nmnh")
        results <- data.frame(match = loc_match$match, ID = as.numeric(loc_match$ID), score = as.numeric(loc_match$score), stringsAsFactors = FALSE)
        results_filtered <- dplyr::filter(results, score < threshold)
        results2 <<- results_filtered[order(results_filtered$score),]
        
        results2_table <- data.frame(match = results2$match, score = as.numeric(results2$score), stringsAsFactors = FALSE)
        
        DT::datatable(results2_table, escape = FALSE, options = list(searching = FALSE, ordering = FALSE, pageLength = 15, paging = FALSE, language = list(zeroRecords = "No matches found")), rownames = FALSE, selection = 'single', caption = paste0("Method: ", str_method), colnames = c('String matched', 'Score (lower is better)'))
        
      }else if (input$db_to_use2 == "GBIF"){
        loc_match <- find_matching_str(ind_input['precise_locality'], database = locations_db, method = str_method, no_cores = this_cpu_cores, db = "gbif")
        results <- data.frame(match = loc_match$match, no_records = as.numeric(loc_match$no_records), ROWID = as.numeric(loc_match$ROWID), score = as.numeric(loc_match$score), stringsAsFactors = FALSE)
        results_filtered <- dplyr::filter(results, score < threshold)
        results2 <<- results_filtered[order(results_filtered$score, -results_filtered$no_records),]
        
        results2_table <- data.frame(match = results2$match, no_records = as.numeric(results2$no_records), score = as.numeric(results2$score), stringsAsFactors = FALSE)
        
        DT::datatable(results2_table, escape = FALSE, options = list(searching = FALSE, ordering = FALSE, pageLength = 15, paging = FALSE, language = list(zeroRecords = "No matches found")), rownames = FALSE, selection = 'single', caption = paste0("Method: ", str_method), colnames = c('String matched', 'No. records', 'Score (lower is better)'))
        
      }
      
    })
    
    
    
    # Table 3_2 ----
    output$table3_2 <- DT::renderDataTable({
      
      req(input$db_to_use2)
      #Progress bar
      progress3 <- shiny::Progress$new()
      on.exit(progress3$close())
      
      progress3$set(message = 'Searching for matches using method "jw"...', value = 0.3)
      
      source("find_matching_str.R")
      
      #method <- input$method
      str_method <- "jw"
      threshold <- 0.2
      
      progress3$set(message = 'Searching for matches using method "jw"...', value = 0.7)
      
      if (input$db_to_use2 == "NMNH"){
        loc_match <- find_matching_str(ind_input['precise_locality'], database = locations_db, method = str_method, no_cores = this_cpu_cores, db = "nmnh")
        results <- data.frame(match = loc_match$match, ID = as.numeric(loc_match$ID), score = round(as.numeric(loc_match$score), 4), stringsAsFactors = FALSE)
        results_filtered <- dplyr::filter(results, score < threshold)
        results3 <<- results_filtered[order(results_filtered$score),]
        
        results3_table <- data.frame(match = results3$match, score = as.numeric(results3$score), stringsAsFactors = FALSE)
        
        DT::datatable(results3_table, escape = FALSE, options = list(searching = FALSE, ordering = FALSE, pageLength = 15, paging = FALSE, language = list(zeroRecords = "No matches found")), rownames = FALSE, selection = 'single', caption = paste0("Method: ", str_method), colnames = c('String matched', 'Score (lower is better)'))
        
      }else if (input$db_to_use2 == "GBIF"){
        loc_match <- find_matching_str(ind_input['precise_locality'], database = locations_db, method = str_method, no_cores = this_cpu_cores, db = "gbif")
        results <- data.frame(match = loc_match$match, no_records = as.numeric(loc_match$no_records), ROWID = as.numeric(loc_match$ROWID), score = round(as.numeric(loc_match$score), 4), stringsAsFactors = FALSE)
        results_filtered <- dplyr::filter(results, score < threshold)
        results3 <<- results_filtered[order(results_filtered$score, -results_filtered$no_records),]
        
        results3_table <- data.frame(match = results3$match, no_records = as.numeric(results3$no_records), score = as.numeric(results3$score), stringsAsFactors = FALSE)
        
        DT::datatable(results3_table, escape = FALSE, options = list(searching = FALSE, ordering = FALSE, pageLength = 15, paging = FALSE, language = list(zeroRecords = "No matches found")), rownames = FALSE, selection = 'single', caption = paste0("Method: ", str_method), colnames = c('String matched', 'No. records', 'Score (lower is better)'))
        
      }
      
    })










  })
  
  
  
  
  
  
  
  # Downloadable csv ----
  output$downloadcsv1 <- downloadHandler(
    filename = function() {
      paste("results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      csvinput_db <- dbConnect(RSQLite::SQLite(), csv_database)
      this_data <- dbGetQuery(csvinput_db, "SELECT * FROM csv")
      #Convert everything to characters
      this_data %>% dplyr::mutate_all(as.character)
      write.csv(this_data, file, row.names = FALSE, quote = TRUE)
      dbDisconnect(csvinput_db)
    }
  )
  
  output$downloadData2 <- renderUI({
    req(input$csvinput)
    downloadButton("downloadcsv1", "Download results", class = "btn-primary")  
    
  })
  
  
  
  
  
  # Next button ----
  output$nextBin <- renderUI({
    req(input$row)
    
    csvinput_db <- dbConnect(RSQLite::SQLite(), csv_database)
    csvinput <- dbGetQuery(csvinput_db, paste0("SELECT * FROM csv"))
    dbDisconnect(csvinput_db)
    
    actionButton("nextBin", 
                 label = "Next", 
                 class = "btn btn-primary",
                 icon = icon("triangle-right", lib = "glyphicon"))
  })
  
  observeEvent(input$nextBin, {
    
    seloptions <- as.list(paste0(csvinput$precise_locality, " (", csvinput$sheet_barcode, ")"))
    
    current <- which(seloptions == input$row)
    if(current < length(seloptions)){
      updateSelectInput(session, "row",
                        choices = as.list(seloptions),
                        selected = seloptions[current + 1])
    }
    
    #Set filters to true
    updateCheckboxInput(session, "countryfilter", value = TRUE)
    updateCheckboxInput(session, "yearfilter", value = TRUE)
    updateCheckboxInput(session, "collectorfilter", value = TRUE)
    
  })
  
  
  
  
  # csv_info ----
  output$csv_info <- renderUI({
    if (is.null(input$csvinput)){
      tagList(
        fileInput("csvinput", "Select csv File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
        HTML("<div class=\"panel panel-primary\"> <div class=\"panel-heading\"> <h3 class=\"panel-title\">The csv file should be encoded using \"UTF-8\", columns should be comma-separated, and have at least these 5 columns</h3> </div> <div class=\"panel-body\"> <ul>
           <li>sheet_barcode</li>
           <li>coll_year_from</li>
           <li>collector_1</li>           
           <li>country</li>
           <li>precise_locality</li>
           </ul></div></div>")
      )
    }
  })
  
  
  
  # Help1 ----
  output$help1 <- renderUI({
    HTML("<div class=\"panel panel-primary\"> <div class=\"panel-heading\"> <h3 class=\"panel-title\">Process</h3></div><div class=\"panel-body\">
         <p>This system will take the string in the column \"precise_locality\" and match it with the locality from NMNH or GBIF databases. The process returns three tables, one for each matching method (see below), and the score.</p>
         <p>When matching to <strong>NMNH</strong>, each match returns the value and the score.</p>
         <p>When matching to <strong>GBIF</strong>, each match returns the value, the number of records for that location/country/collector and the score. Searching in GBIF requires a country to avoid trying to match a string against 23 million rows. Future versions will try to work around this problem.</p>
         <p>Matching methods:</p>
         <ul>
         <li>osa: Optimal string aligment. Score is an integer where 0 is exact match and the lower the better.</li>
         <li>dl: Full Damerau-Levenshtein distance. Score is an integer where 0 is exact match and the lower the better.</li>
         <li>jw: Jaro-Winker distance. Score is between 0 and 1 and the lower the better.</li>
         </ul>
       
         For details on the matching methods, see van der Loo (2014):</p>
         <ul>
         <li>van der Loo, Mark (2014). The stringdist Package for Approximate String Matching</a>. The R Journal 6: 111-122.</li></ul>
         </div></div>")
  })
  
  
}




# Run app ----
shinyApp(ui = ui, server = server, onStart = function() {
  cat("Loading\n")
  #Mount path
  onStop(function() {
    cat("Closing\n")
    #Close databases
    try(dbDisconnect(loc_db), silent = TRUE)
    try(dbDisconnect(csvinput_db), silent = TRUE)
    #try(file.remove(csv_database), silent = TRUE)
    #Remove variables and objects in memory
    cat("Removing objects\n")
    rm(list = ls())
  })
})
