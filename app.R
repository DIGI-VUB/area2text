library(shiny)
library(knitr)
library(recogito)
library(magick)
library(xml2)
library(opencv)
library(sortable)
library(htmltools)
library(bs4Dash)
library(shinyWidgets)
library(tippy)
library(DT)
library(jsonlite)
library(sf)
library(tools)
library(data.table)
library(rappdirs)
library(RSQLite)
file_db <- function(x){
  path <- user_data_dir(appname = "area2text")
  if(missing(x)){
    x <- path
  }else{
    x <- file.path(path, sprintf("%s.db", file_path_sans_ext(x)))
  }
  x
}

settings <- list()
settings$local_images <- file.path(getwd(), "src", "area2text", "img")
settings$local_images <- tempdir()
settings$default_db   <- "area2text.RData"


## Function to read/write to the database and initialise the database
db_init <- function(db, data, fresh = !file.exists(db)){
  if(fresh){
    input <- data
    db_write(db, data = input, table = "docs", overwrite = TRUE, append = FALSE)
    output <- data.frame(doc_id = character(), timepoint = character(), text = character(), text_chunks = character(), areas = character(), n_areas = integer(), stringsAsFactors = FALSE)
    db_write(db, data = output, table = "annotations", overwrite = TRUE, append = FALSE)
  }else{
    input   <- data
    current <- db_read(db, "select * from docs")
    input   <- subset(input, !input$id %in% current$id)
    db_write(db, data = input, table = "docs", overwrite = FALSE, append = TRUE)
  }
  anno <- list(docs        = db_read(db, "select * from docs"),
               annotations = db_read(db, "select * from annotations"))
  anno
}
db_list_tables <- function(db){
  con <- db_con(db)
  on.exit(dbDisconnect(con))
  x <- dbListTables(con)
  x
}
db_con <- function(db){
  if(db != ":memory:"){
    wd  <- getwd()
    on.exit(setwd(wd))
    folder <- dirname(db)
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
    setwd(dirname(db))
    con <- dbConnect(RSQLite::SQLite(), basename(db))
  }else{
    con <- dbConnect(RSQLite::SQLite(), db)
  }
}
db_read <- function(db = ":memory:", sql){
  con <- db_con(db)
  on.exit(dbDisconnect(con))
  x <- dbGetQuery(con, sql)
  x
}
db_write <- function(db = ":memory:", data, table, ...){
  con <- db_con(db)
  on.exit(dbDisconnect(con))
  x <- dbWriteTable(con, name = table, value = data, ...)
  x
}


## Data frame with document id, text and
#x <- readRDS("/mnt/digidisk/home/digi/ShinyApps/getuigenissen2-area2text/getuigenissen_2_0.rds")
#x <- x$brugse_vrije
#x$id     <- x$id
#x$text   <- sapply(x$value, FUN = function(x) xml_text(read_html(x)))
#x$image  <- x$image_url
x <- data.frame(id = "digi-vub", text = "Brussels Platform for Digital Humanities\n\nResearch Group", image_url = "https://raw.githubusercontent.com/DIGI-VUB/recogito/master/tools/logo.png", stringsAsFactors = FALSE)

# file.remove(settings$default_db)
# x <- db_init(settings$default_db, data = x[, sapply(x, is.atomic)])

## IDEA:
##   1. show image + paragraphs
##   2. allow selecting area, if area is shown, assign one or 2 paragraphs to it
##   3. define order in the paragraphs by showing the image chunks and letting them order it
addResourcePath(prefix = "img", directoryPath = settings$local_images)

ui <- dashboardPage(
  header = dashboardHeader(disable = TRUE),
  sidebar = dashboardSidebar(id = "sidebar", icon = icon("draw-polygon"), collapsed = FALSE, fixed = FALSE, expandOnHover = TRUE, skin = "light", status = "info",
                             sidebarMenu(
                               id = "tabs_sidebar",
                               menuItem("Work", tabName = "tabs_sidebar_work", icon = icon("draw-polygon"), selected = TRUE),
                               menuItem("Upload another dataset", tabName = "tabs_sidebar_upload", icon = icon("upload"), selected = FALSE)
                             ),
                             uiOutput(outputId = "uo_database"),
                             progressBar(id = "uo_box_images_progress", status = "info", value = 0, total = 100, display_pct = TRUE),
                             infoBoxOutput(outputId = "uo_box_images", width = 12),
                             infoBoxOutput(outputId = "uo_box_images_done", width = 12),
                             uiOutput(outputId = "uo_current_image"),
                             infoBoxOutput(outputId = "uo_box_chunk_amount", width = 12),
                             infoBoxOutput(outputId = "uo_box_chunk_amount_done", width = 12),
                             div(downloadButton(outputId = "ui_export", label = "Export data", icon = icon("download")), style = "text-align: center")

                             ),
  footer = dashboardFooter(left = "Brussels Platform for Digital Humanities", right = tags$span(id = "ui_footer_right", textOutput(outputId = "uo_footer_right"))),
  body = dashboardBody(
    fluidRow(
      tabBox(width = 12,
        tabPanel("Annotation",
                 fluidRow(
                   column(width = 8,
                          tippy_this(elementId = "anno-outer-container", tooltip = "In order to select an area on the image, press the shift key when drawing the area",
                                     duration = 1000, arrow = TRUE, theme = "dark", placement = "top", sticky = TRUE),
                          openseadragonOutput(outputId = "anno", width = "100%", height = "900px"),
                          ),
                   column(width = 4,
                          actionButton(inputId = "ui_next", label = "SAVE AND GO TO NEXT IMAGE", icon = icon("play"), width = "100%", status = "secondary", flat = TRUE),
                          tags$br(),
                          tags$br(),
                          box(title = "Texts", icon = icon("table"),
                              solidHeader = FALSE, status = "lightblue", collapsible = TRUE, width = 12,
                              blockQuote("When done, reorder the chunks to put them in the right reading sequence", color = "info"),
                              uiOutput(outputId = "uo_rankme"))
                          ))),
        tabPanel("Technical information",
                 tags$h5("Image and transcription paragraphs"),
                 fluidRow(verbatimTextOutput(outputId = "image_info")),
                 tags$h5("Current annotations"),
                 fluidRow(verbatimTextOutput(outputId = "annotations_current")),
                 tags$h5("Results"),
                 fluidRow(verbatimTextOutput(outputId = "annotation_result_r")))
      )
    )
  )
)


popup_upload <- modalDialog(title = "Upload data",
                            tags$blockquote("Load here a set of images and text chunks which you like to align."),
                            fileInput(inputId = "ui_tid_input", label = "Select an .rds file",   buttonLabel = "Browse...", width = "100%"),
                            tags$details(tags$summary("More information about the required structure of this file"),
                                         tags$ul(
                                           tags$li("This file should contain a data.frame with columns id, text and image_url."),
                                           tags$li("Text chunks  in column text should be separated by 2 newline characters."),
                                           tags$li("The image_url's should be publically accessible images."))),
                                  size = "xl", easyClose = TRUE, footer = NULL)



server <- function(input, output, session) {
  ## Intro
  sendSweetAlert(
    session = session,
    title = "Welcome",
    text = tags$span(
      "The objective of this app is to align texts with image regions.",
      "You upload images and the text chunks and indicate which text chunk belongs to which part of the image"),
    html = TRUE, type = "info", btn_labels = "Ok")

  ## DB_DATA: raw data, DB_TODO: things to do, DB_APP: current image, DB_OUT: results of the annotations
  DB_DATA <- reactiveValues(rawdata = x, dataset_label = settings$default_db, db = file_db(settings$default_db))
  DB_TODO <- reactiveValues(data = x)
  DB_APP  <- reactiveValues(image_nr = 0, url = NULL, doc_id = NULL)
  DB_OUT  <- reactiveValues(items = list())

  ## In case of new data, create a new SQLite DB
  observe({
    if(input$tabs_sidebar == "tabs_sidebar_upload"){
      showModal(popup_upload)
    }
  })
  observe({
    input$ui_tid_input
    if(!is.null(input$ui_tid_input) && file.exists(input$ui_tid_input$datapath)){
      if(file_ext(input$ui_tid_input$name) %in% c("rds", "RDS", "Rds", "txt", "csv")){
        fname    <- basename(input$ui_tid_input$name)
        filetype <- file_ext(input$ui_tid_input$name)
        if(filetype %in% c("txt", "csv")){
          x <- setDF(fread(input$ui_tid_input$datapath))
        }else{
          x <- readRDS(input$ui_tid_input$datapath)
        }
        if(!inherits(x, "data.frame") || !all(c("id", "text", "image_url") %in% colnames(x))){
          showModal(modalDialog(title = "New data upload failed", "Uploaded data is not a data.frame with columns id, text and image_url"))
        }else{
          #x <- head(x, n = 100)
          x$id            <- x$id
          ## Update the reactives which trigger the app
          DB_DATA$rawdata       <- x
          DB_DATA$dataset_label <- fname
          DB_DATA$db            <- file_db(fname)
          showModal(modalDialog(title = "New data uploaded",
                                tags$ul(
                                  tags$li(sprintf("New data uploaded with %s rows", nrow(x)))
                                )))
        }
      }else{
        showModal(modalDialog(title = "New data upload failed", "Uploaded data is not a .rds/.csv file"))
      }
    }
  })
  observe({
    x <- DB_DATA$rawdata
    x <- db_init(DB_DATA$db, data = x[, sapply(x, is.atomic)])
    # start from what is not done already
    isolate({
      DB_TODO$data    <- x$docs[order(x$docs$id %in% x$annotations$doc_id, decreasing = TRUE), ]
      DB_APP$image_nr <- sum(DB_TODO$data$id %in% x$annotations$doc_id)
    })
  })
  ## FOOTER showing the path to the database
  output$uo_footer_right <- renderText(DB_DATA$db)

  ## Export data
  output$ui_export <- downloadHandler(
    filename = function() {
      paste(file_path_sans_ext(basename(DB_DATA$db)), ".rds", sep = "")
    },
    content = function(file) {
      docs                    <- db_read(DB_DATA$db, "select * from docs")
      annotations             <- db_read(DB_DATA$db, "select * from annotations")
      annotations$text_chunks <- lapply(annotations$text_chunks, fromJSON)
      annotations$areas       <- lapply(annotations$areas, unserializeJSON)
      saveRDS(list(docs = docs, anno = annotations), file, compress = FALSE, version = 2)
    }
  )

  ## Main function current_image: get the image and the texts
  observeEvent(input$ui_next, {
    ## SAVE
    output <- data.frame(doc_id           = DB_APP$doc_id,
                         timepoint        = as.character(Sys.time()),
                         text             = current_image()$text,
                         text_chunks      = as.character(toJSON(list(text = current_image()$text_chunks, order = as.integer(input$rank_list_multi)))),
                         areas            = as.character(serializeJSON(DB_OUT$items)),
                         n_areas          = length(DB_OUT$items),
                         stringsAsFactors = FALSE)
    if(DB_APP$image_nr < nrow(DB_TODO$data)){
      db_write(DB_DATA$db, data = output, table = "annotations", overwrite = FALSE, append = TRUE)
    }
    DB_APP$image_nr <- DB_APP$image_nr + 1
  })
  current_image <- reactive({
    x <- DB_TODO$data
    i <- DB_APP$image_nr + 1
    if(i > nrow(x)){
      i <- nrow(x)
      sendSweetAlert(session = session, title = "Yippie",
        text = tags$span("All images are handled ", icon("smile")),
        html = TRUE, type = "success", btn_labels = "Ok")
    }
    #i            <- sample(seq_len(nrow(x)), size = 1)
    item               <- x[i, ]
    isolate({
      DB_APP$doc_id <- item$id
    })
    item$text_chunks   <- strsplit(item$text, split = "\n\n")
    text               <- item$text
    text_chunks  <- unlist(item$text_chunks)
    url          <- head(item$image_url, n = 1)

    img_orig     <- image_read(url)

    #img <- image_resize(img, "2000x")
    #img <- image_resize(img_orig, "x800")
    img     <- img_orig
    outfile <- basename(url)
    outfile <- file.path(settings$local_images, outfile)
    image_write(img, path = outfile)
    ## original texts
    labels <- mapply(text_chunks, seq_along(text_chunks), FUN = function(x, i) tags$div(tags$em(x)), SIMPLIFY = FALSE)
    rank_list_multi_original <- rank_list(text = "Text chunks", labels = labels, input_id = "rank_list_multi_original", options = sortable_options(multiDrag = TRUE))
    bbox <- image_info(img_orig)
    bbox <- c(xmin = 0, ymin = 0, xmax = bbox$width - 1, ymax = bbox$height - 1)
    list(item = item,
         url = url,
         img = img_orig,
         img_ocv = ocv_read(outfile),
         img_info = image_info(img_orig),
         bbox = bbox,
         rank_list_multi_original = rank_list_multi_original,
         text_chunks = text_chunks,
         text = text,
         labels = labels)
  })

  output$image_info <- renderPrint({
    info <- current_image()
    list(info$text_chunks,
         img = info$url,
         info = image_info(info$img),
         resize_ratio = image_info(info$img)$width / 1000
    )
  })

  ##
  ## Left side overview statistics
  ##
  output$uo_box_chunk_amount <- renderInfoBox({
    info <- current_image()
    infoBox(title = "# of text chunks", value = length(info$text_chunks), icon = icon("align-justify"), width = 4)
  })
  output$uo_box_chunk_amount_done <- renderInfoBox({
    info <- current_image()
    info <- last_anno()
    infoBox(title = "# of areas labelled", value = info$n, icon = icon("draw-polygon"), width = 4)
  })
  output$uo_box_images_done <- renderInfoBox({
    infoBox(title = "# images handled", value = DB_APP$image_nr, icon = icon("image"), width = 4)
  })
  output$uo_box_images <- renderInfoBox({
    infoBox(title = "# images in data", value = nrow(DB_DATA$rawdata), icon = icon("database"), width = 4)
  })
  output$uo_database <- renderUI({
    div(
      blockQuote(sprintf("Database: %s", DB_DATA$dataset_label), color = "info")
    )
  })
  observe({
    updateProgressBar(session = session, id = "uo_box_images_progress", value = DB_APP$image_nr, total = nrow(DB_DATA$rawdata))
  })
  output$uo_current_image <- renderUI({
    info  <- current_image()
    if("id_label" %in% colnames(info$item)){
      label <- info$item$id_label
    }else if(all(c("manifest_label", "canvas_id") %in% colnames(info$item))){
      label <- sprintf("%s, Canvas %s", info$item$manifest_label, info$item$canvas_id)
    }else{
      label <- info$item$id
    }
    div(
      blockQuote("Image: ", label, color = "info")
    )
  })
  ##
  ## Output results
  ##
  output$annotations_current <- renderPrint({
    x <- read_annotorious(input$annotations)
    x
  })
  output$annotation_result_r <- renderPrint({
    str(DB_OUT$items)
  })
  # output$annotation_result <- renderDataTable({
  #   x <- read_annotorious(input$annotations)
  #   x <- data.frame(id = x$id, type = x$type, label = x$label, comment = x$comment, x = x$x, y = x$y, width = x$width, height = x$height, polygon = sapply(x$polygon, toJSON))
  #   datatable(x)
  # })
  ##
  ## Main area of selection
  ##
  output$anno <- renderOpenSeaDragon({
    info <- current_image()
    #url <- "img/default.jpg"
    url  <- info$url
    annotorious("annotations", tags = c("question", "answer", "intro/end", "other", "unclear"), src = url, type = "openseadragon")
  })

  last_anno <- reactive({
    anno <- read_annotorious(input$annotations)
    #saveRDS(msg, file = "areas.rds")
    if(nrow(anno) > 0){
      isolate({
        info <- current_image()
        anno <- ocv_crop_annotorious(anno, bbox = info$bbox)
      })
      msg <- tail(anno, n = 1)
      if(msg$type %in% "POLYGON"){
        area <- ocv_polygon(info$img_ocv, pts = msg$polygon[[1]], crop = TRUE)
      }else if(msg$type %in% "RECTANGLE"){
        area <- ocv_rectangle(info$img_ocv, x = msg$x, y = msg$y, width = msg$width, height = msg$height)
      }
      area <- ocv_bitmap(area)
      area <- image_read(area)
      path <- file.path(settings$local_images, "default1.jpg")
      path <- file.path(settings$local_images, basename(tempfile(fileext = '.jpg')))
      image_write(area, path = path)
      list(n = nrow(anno), img = area, data = msg, path = path)
    }else{
      list(n = nrow(anno), data = anno)
    }
  })
  observe({
    areas <- last_anno()
    isolate({
      info                 <- current_image()
      existing_assignments <- unlist(lapply(DB_OUT$items, FUN = function(x) x$areas$id))
    })
    if(areas$n > 0 && !areas$data$id %in% existing_assignments){
      popup_assignment <- modalDialog(title = "Assign area to text - drag the text on the image from left to right",
                                      actionButton(inputId = "ui_save_assignment", label = "Assignment done", status = "success", flat = FALSE, width = "100%"),
                                      openseadragonOutputNoToolbar(outputId = "uo_area", height = "200px"),
                                      bucket_list(
                                        header = NULL,
                                        group_name = "bucket_list_group",
                                        orientation = "horizontal",
                                        add_rank_list(text = "Drag text from here", input_id = "rank_list_from",
                                                      labels = setNames(mapply(info$text_chunks, seq_along(info$text_chunks),
                                                                               FUN = function(x, i) tags$div(tags$em(textAreaInput(inputId = sprintf("ui_paragraph_%s", i), value = x, label = NULL))), SIMPLIFY = FALSE),
                                                                        seq_along(info$text_chunks))),
                                        #add_rank_list(text = "Drag text from here", input_id = "rank_list_from",
                                        #              labels = setNames(mapply(info$text_chunks, seq_along(info$text_chunks),
                                        #                                       FUN = function(x, i) tags$div(tags$em(x)), SIMPLIFY = FALSE),
                                        #                                seq_along(info$text_chunks))),
                                        add_rank_list(text = "to here", input_id = "rank_list_to", labels = NULL)
                                      ),
                                      size = "xl", easyClose = TRUE, footer = NULL)
      showModal(popup_assignment)
    }
  })
  observeEvent(input$ui_save_assignment, {
    anno <- read_annotorious(input$annotations)
    if(length(input$rank_list_to) > 0){
      info                  <- current_image()
      text_chunks           <- info$text_chunks
      text_chunks_selected  <- as.integer(input$rank_list_to)
      text_chunks_corrected <- text_chunks[text_chunks_selected]
      text_chunks_corrected <- sapply(text_chunks_selected, FUN = function(i) input[[sprintf("ui_paragraph_%s", i)]])
      DB_OUT$items[[length(DB_OUT$items) + 1]] <- list(doc_id = DB_APP$doc_id,
                                                       item   = info$item,
                                                       area   = head(anno, n = 1),
                                                       texts  = text_chunks[text_chunks_selected],
                                                       texts_corrected = text_chunks_corrected,
                                                       areas  = anno, anno = input$annotations)
      removeModal()
    }else{
      showNotification("First assign text to the image by dragging the corresponding text from left to right", type = "error")
    }
  })
  output$uo_area <- renderOpenSeaDragonNoToolbar({
    areas <- last_anno()
    if(areas$n > 0){
      annotorious("selected-area-viewer", src = sprintf("img/%s", basename(areas$path)), type = "openseadragon-notoolbar")
    }
  })
  output$uo_rankme <- renderUI({
    areas  <- last_anno()
    info   <- current_image()
    values <- info$labels
    values <- setNames(values, sprintf("paragraph_%s", seq_along(values)))
    values <- setNames(values, seq_along(values))
    #values[[4]] <- tags$div(
    #  em("Complex"), " html tag without a name", tags$img(src = "img/default1.jpg")
    #)
    #print(values)
    rank_list(
      text = NULL,
      labels = values,
      input_id = "rank_list_multi",
      options = sortable_options(multiDrag = TRUE)
    )
  })
}
shinyApp(ui, server)
