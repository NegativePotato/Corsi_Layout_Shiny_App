library(shiny)
library(ggplot2)
library(dplyr)
library(shinyjs)
library(stringr)
library(bslib)

# Function to compute square vertices
generate_square_vertices <- function(x, y, side) {
  half_side <- side / 2
  data.frame(
    x_TR = x + half_side, y_TR = y + half_side,
    x_TL = x - half_side, y_TL = y + half_side,
    x_BR = x + half_side, y_BR = y - half_side,
    x_BL = x - half_side, y_BL = y - half_side
  )
}

# Generate unique labels (1-9, then A-Z)
generate_label <- function(excl) {
  labels <- c(as.character(1:9), LETTERS)
  # print(paste("labels : ", excl))
  labels[!(labels %in% excl)][1]
}

# Check direction of sequence
add_directional_columns <- function(df) {
  output <- df %>%
    # rowwise() %>% 
    # mutate(
    #   x_TR = generate_square_vertices(x, y, side)$x_TR,
    #   y_TR = generate_square_vertices(x, y, side)$y_TR,
    #   x_TL = generate_square_vertices(x, y, side)$x_TL,
    #   y_TL = generate_square_vertices(x, y, side)$y_TL,
    #   x_BR = generate_square_vertices(x, y, side)$x_BR,
    #   y_BR = generate_square_vertices(x, y, side)$y_BR,
    #   x_BL = generate_square_vertices(x, y, side)$x_BL,
    #   y_BL = generate_square_vertices(x, y, side)$y_BL
    # ) %>%
    # ungroup() %>%
    mutate(
      left_to_right_next = lead(x) > x,   # TRUE if the next element is to the right
      left_to_right_previous = lag(x) < x,  # TRUE if the previous element is to the left
      bottom_to_top_next = lead(y) > y,   # TRUE if the next element is above
      bottom_to_top_previous = lag(y) < y  # TRUE if the previous element is below
    ) %>%
    # tidyr::replace_na(list(
    #   left_to_right_next = FALSE,
    #   left_to_right_previous = FALSE,
    #   bottom_to_top_next = FALSE,
    #   bottom_to_top_previous = FALSE
    # )) %>%  # Replace NA values (first or last elements) with FALSE
    mutate(
      x1_current = ifelse(left_to_right_next & bottom_to_top_next, x_TL,
                          ifelse(!left_to_right_next & bottom_to_top_next, x_BL,
                                 ifelse(left_to_right_next & !bottom_to_top_next, x_TR, x_BR))),
      y1_current = ifelse(left_to_right_next & bottom_to_top_next, y_TL, 
                          ifelse(!left_to_right_next & bottom_to_top_next, y_BL, 
                                 ifelse(left_to_right_next & !bottom_to_top_next, y_TR, y_BR))), 
      x2_current = ifelse(left_to_right_next & bottom_to_top_next, x_BR, 
                          ifelse(!left_to_right_next & bottom_to_top_next, x_TR, 
                                 ifelse(left_to_right_next & !bottom_to_top_next, x_BL, x_TL))),
      y2_current = ifelse(left_to_right_next & bottom_to_top_next, y_BR, 
                          ifelse(!left_to_right_next & bottom_to_top_next, y_TR, 
                                 ifelse(left_to_right_next & !bottom_to_top_next, y_BL, y_TL))), 
      x1_next = ifelse(left_to_right_next & bottom_to_top_next, lead(x_TL), 
                       ifelse(!left_to_right_next & bottom_to_top_next, lead(x_BL), 
                              ifelse(left_to_right_next & !bottom_to_top_next, lead(x_TR), lead(x_BR)))),
      y1_next = ifelse(left_to_right_next & bottom_to_top_next, lead(y_TL), 
                       ifelse(!left_to_right_next & bottom_to_top_next, lead(y_BL), 
                              ifelse(left_to_right_next & !bottom_to_top_next, lead(y_TR), lead(y_BR)))), 
      x2_next = ifelse(left_to_right_next & bottom_to_top_next, lead(x_BR), 
                       ifelse(!left_to_right_next & bottom_to_top_next, lead(x_TR), 
                              ifelse(left_to_right_next & !bottom_to_top_next, lead(x_BL), lead(x_TL)))),
      y2_next = ifelse(left_to_right_next & bottom_to_top_next, lead(y_BR), 
                       ifelse(!left_to_right_next & bottom_to_top_next, lead(y_TR), 
                              ifelse(left_to_right_next & !bottom_to_top_next, lead(y_BL), lead(y_TL)))), 
      edge = paste0(label, lead(label))
    ) %>%
    select(label, edge, x1_current, y1_current, x2_current, y2_current, x1_next, y1_next, x2_next, y2_next) %>% 
    filter(!is.na(x1_current))
  print(output)
  
  output <- output %>%
    tidyr::pivot_longer(cols = contains("_"),
                        names_to = c(".value", "element"), 
                        names_sep = "_")
  
  return(output)
}

find_intersecting_squares <- function(squares_df, polygon_coords) {
  polygon <- st_polygon(list(as.matrix(polygon_coords))) %>% st_sfc() %>% st_sf()
  
  square_geoms <- squares_df %>%
    rowwise() %>%
    mutate(geometry = list(st_polygon(list(matrix(c(x_TL, y_TL, x_TR, y_TR, x_BR, y_BR, x_BL, y_BL, x_TL, y_TL), ncol = 2, byrow = TRUE))))) %>%
    st_as_sf()
  
  intersections <- st_intersects(square_geoms, polygon, sparse = FALSE)
  squares_df[apply(intersections, 1, any), ]
}


make_clutterness_dataframe <- function(squares_df, sequence_df) {
  sequence_df_2 <- sequence_df %>%
    mutate(edge_number = rep(seq(nrow(sequence_df)/2), each = 2)) %>%
    group_by(edge) %>% 
    dplyr::reframe(
      edge_number = rep(edge_number[1], 5),
      edge = rep(edge[1], 5),
      vertex_id = seq(5),
      x = c(x1[1], x2, x1[2], x1[1]), 
      y = c(y1[1], y2, y1[2], y1[1])) %>% 
    arrange(edge_number, vertex_id)
  intersections <- sequence_df_2 %>% 
    group_by(edge_number, edge) %>% 
    dplyr::reframe(find_intersecting_squares(squares_df[!(squares_df$label %in% strsplit(edge, "")[[1]]),], 
                                             data.frame(x = x, y = y)))
  # intersections <- intersections %>% 
  #   rowwise() %>%
  #   mutate(shared_clutter = ifelse(intersections %>% 
  #                                    count(label_count = label) %>% 
  #                                    filter(label_count == label) %>% 
  #                                    .$n > 1, "Shared", "Not Shared"))
  
  intersections_count <- intersections %>% 
    count(edge_number, edge, name = "n_clutter_squares") %>% 
    merge(intersections %>% select(edge, label)) %>% 
    group_by(edge_number, edge, n_clutter_squares) %>% 
    dplyr::summarize(clutter_square_labels = paste0(label, collapse = ", ")) %>% 
    ungroup() %>%
    rbind(data.frame(
      edge_number = unique(sequence_df_2[!(sequence_df_2$edge %in% .$edge),]$edge_number), 
      edge = unique(sequence_df_2[!(sequence_df_2$edge %in% .$edge),]$edge), 
      n_clutter_squares = rep(0, length(unique(unique(sequence_df_2[!(sequence_df_2$edge %in% .$edge),]$edge)))),
      clutter_square_labels = rep("", length(unique(unique(sequence_df_2[!(sequence_df_2$edge %in% .$edge),]$edge))))
    )) %>% 
    arrange(edge_number)
  
  return(list(clutterness_df = intersections,
              clutterness_count_df = intersections_count)
  )
}


ui <- page_fluid(
  useShinyjs(), 
  
  tags$head(
    tags$style(HTML(
      ".form-control { height:auto; padding:3px 5px;}"
    ))
  ),
  title = "Corsi layout creator tool",
  layout_sidebar(
    sidebar = sidebar(
      width = 300, 
      fluidRow(
        column(6, style = "padding-right: 2px;", numericInput("xlim", "X-axis limit:", value = 500, min = 100, step = 50)),
        column(6, style = "padding-left: 2px;", numericInput("ylim", "Y-axis limit:", value = 500, min = 100, step = 50))
      ),
      fluidRow(
        column(4, style = "padding-right: 2px;", numericInput("x", "Square center X:", value = 250)),
        column(4, style = "padding-left: 2px; padding-right: 2px;", numericInput("y", "Square center Y:", value = 250)),
        column(4, style = "padding-left: 2px;", numericInput("side", "Side length:", value = 50, min = 10),)
      ),
      actionButton("add", "Add Square"),
      actionButton("update", "Update Squares", disabled = TRUE),
      uiOutput("square_controls")
    ),
    card(
      card_header("Corsi Layout"),
      fluidRow(
        column(8, plotOutput("plot", height = "600px", 
                             click = "plot_click", brush = "plot_brush", dblclick = "plot_dblclick")),
        column(4, 
               textInput("save_plot_name", value = "", placeholder = "Enter a name for the plot file", label = NULL), 
               downloadButton('download_gglot','Download Plot', disabled = TRUE)),
        # column(3, actionButton("save_ggplot", "Save Plot", style = "height: 100%;", disabled = TRUE)),
        #  column(2, downloadButton('download_gglot','Download Plot', style = "height: 100%;", disabled = TRUE))
      ),
    ),
    verbatimTextOutput("error_message"),
    card(
      card_header("Draw Line and Clutterness"),
      fluidRow(
        column(5, textInput("line_path", value = "", placeholder = "Sequence (e.g., 38A4):", label = NULL)),
        column(4, textInput("overlap_path", value = "", placeholder = "Overlap pair: ", label = NULL)),
        column(3, actionButton("draw_line", "Draw Line")),
      ), 
      # verbatimTextOutput("overlap_text")
    ), 
    card(
      card_header("Layout info dataframe"),
      fluidRow(
        column(6, fileInput("upload_csv", "Upload CSV File Squares", accept = ".csv")),
        column(6, downloadButton("download", "Download CSV"))
      ),
      tableOutput("table")
    ),
    card(
      card_header("Layout info dataframe"),
      fluidRow(
        column(6, fileInput("upload_csv_sequence", "Upload CSV File Sequence", accept = ".csv")),
        column(6, downloadButton("download_csv_sequence", "Download CSV Sequence"))
      ),
      tableOutput("sequence_table")
    ),
    layout_columns(
      card(card_header("Clutterness info dataframe"),
           downloadButton("download_csv_clutterness", "Download CSV Clutterness"),
           tableOutput("clutterness_df")),
      card(card_header("Clutterness count dataframe"), 
           downloadButton("download_csv_clutterness_count", "Download CSV Clutterness Count"), 
           tableOutput("clutterness_count_df")),
      col_widths = c(6, 6)
    )
  )
)



# ui <- fluidPage(
#   useShinyjs(), 
#   
#   tags$head(
#     tags$style(HTML(
#       ".form-control { height:auto; padding:3px 5px;}"
#     ))
#   ),
# 
#   titlePanel("Corsi layout creator tool"),
#   
#   sidebarLayout(
#     sidebarPanel(
#       tags$div(style = "height: 95vh; overflow-y: auto;",  # Make sidebar scrollable
#                fluidRow(
#                  column(6, style = "padding-right: 2px;", numericInput("xlim", "X-axis limit:", value = 500, min = 100, step = 50)),
#                  column(6, style = "padding-left: 2px;", numericInput("ylim", "Y-axis limit:", value = 500, min = 100, step = 50))
#                ),
#                fluidRow(
#                  column(4, style = "padding-right: 2px;", numericInput("x", "Square center X:", value = 250)),
#                  column(4, style = "padding-left: 2px; padding-right: 2px;", numericInput("y", "Square center Y:", value = 250)),
#                  column(4, style = "padding-left: 2px;", numericInput("side", "Side length:", value = 50, min = 10),),
#                ),
#                actionButton("add", "Add Square"),
#                actionButton("update", "Update Squares", disabled = TRUE),
#                uiOutput("square_controls")
#       ),
#       width = 4
#     ),
#     
#     mainPanel(
#       plotOutput("plot", click = "plot_click", brush = "plot_brush", dblclick = "plot_dblclick"),
#       verbatimTextOutput("error_message"),
#       div(
#         fluidRow(
#           column(5, textInput("line_path", value = "", placeholder = "Sequence (e.g., 38A4):", label = NULL)),
#           column(4, textInput("overlap_path", value = "", placeholder = "Overlap pair: ", label = NULL)),
#           column(3, actionButton("draw_line", "Draw Line", style = "height: 100%;")),
#         )
#       ),
#       verbatimTextOutput("overlap_text"),
#       fluidRow(
#         column(6, textInput("save_plot_name", value = "", placeholder = "Enter a name for the plot file", label = NULL)),
#         column(3, actionButton("save_ggplot", "Save Plot", style = "height: 100%;", disabled = TRUE)),
#         column(3, downloadButton('download_gglot','Download Plot', style = "height: 100%;", disabled = TRUE))
#       ),
#       fluidRow(
#         column(6, fileInput("upload_csv", "Upload CSV File Squares", accept = ".csv")),
#         column(6, downloadButton("download", "Download CSV", style = "height: 100%;"))
#       ),
#       tableOutput("table"),
#       fluidRow(
#         column(6, fileInput("upload_csv_sequence", "Upload CSV File Sequence", accept = ".csv")),
#         column(6, downloadButton("download_csv_sequence", "Download CSV Sequence", style = "height: 100%;"))
#       ),
#       tableOutput("sequence_table"),
#       fluidRow(
#         # column(6, fileInput("upload_csv_clutterness", "Upload CSV File Sequence", accept = ".csv")),
#         column(6, downloadButton("download_csv_clutterness", "Download CSV Clutterness", style = "height: 100%;")),
#         # column(6, fileInput("upload_csv_sequence", "Upload CSV File Sequence", accept = ".csv")),
#         column(6, downloadButton("download_csv_clutterness_count", "Download CSV Clutterness count", style = "height: 100%;"))
#       ),
#       tableOutput("clutterness_df"),
#       tableOutput("clutterness_count_df"),
#       width = 8
#     )
#   )
# )

server <- function(input, output, session) {
  squares <- reactiveVal(data.frame(label = character(), x = numeric(), y = numeric(), side = numeric()))
  sequence_table <- reactiveVal(data.frame(label = character(), edge = character(), element = character(),
                                           x1 = numeric(), y1 = numeric(), 
                                           x2 = numeric(), y2 = numeric()))
  clutterness_df <- reactiveVal(tibble(edge = character(), label = character(), 
                                           x = numeric(), y = numeric(), side = numeric(), 
                                           x_TR = numeric(), y_TR = numeric(), x_TL = numeric(), 
                                           y_TL = numeric(), x_BR = numeric(), y_BR = numeric(), 
                                           x_BL = numeric(), y_BL = numeric()))
  clutterness_count_df <- reactiveVal(tibble(edge = character(), n_clutter_squares = numeric(), 
                                                 label = character(), shared_clutter = character()))
  error_message <- reactiveVal("")
  overlap_text <- reactiveVal("TODO! Text will be : Overlap area = XXX ")
  plot_file_name <- reactive(input$save_plot_name)
  line_data <- reactiveVal(NULL)
  line_data_overlap <- reactiveVal(NULL)
  # 
  observeEvent(squares(), {
    updateActionButton(session, "update", disabled = nrow(squares()) == 0)
  })
  
  observe({
    if (nrow(squares()) == 0) {
      shinyjs::disable("download")
      shinyjs::disable("download_gglot")
      shinyjs::disable("upload_csv_sequence")
    } else {
      shinyjs::enable("download")
      shinyjs::enable("download_gglot")
      shinyjs::enable("upload_csv_sequence")
    }
    
    if (nrow(sequence_table()) == 0) {
      shinyjs::disable("download_csv_sequence")
    } else {
      shinyjs::enable("download_csv_sequence")
    }
  })
  
  observeEvent(plot_file_name(), {
    updateActionButton(session, "save_ggplot", disabled = plot_file_name() == "")
  })
  
  observeEvent(input$upload_csv, {
    req(input$upload_csv)
    df <- read.csv(input$upload_csv$datapath, stringsAsFactors = FALSE)
    if (!all(c("label", "x", "y", "side") %in% names(df))) {
      error_message("Error: CSV file must contain columns 'label', 'x', 'y', 'side'")
    } else {
      squares(df)
      error_message("")
    }
  })
  
  observeEvent(input$upload_csv_sequence, {
    req(input$upload_csv_sequence)
    df <- read.csv(input$upload_csv_sequence$datapath, stringsAsFactors = FALSE)
    if (!all(c("label", "edge", "element", "x1", "y1", "x2", "y2") %in% names(df))) {
      error_message("Error: CSV file must contain columns 'label', 'edge', 'element', 'x1', 'y1', 'x2', 'y2'")
    } else {
      sequence_table(df)
      sequence_str <- paste0(stringr::str_unique(strsplit(paste0(df$edge, collapse = ""), "")[[1]]), collapse = "")
      print(sequence_str)
      updateTextInput(session, "line_path", value = sequence_str)
      updateTextInput(session, "overlap_path", value = sequence_str)
      error_message("")
    }
  })
  
  output$download_gglot <- downloadHandler(
    filename = function(){sprintf("%s_%s.png", input$save_plot_name, ifelse(input$line_path == "", "blank", input$line_path))},
    content = function(file){
      ggsave(file, 
             plot = last_plot(), 
             width = 6*input$xlim/input$ylim, 
             height = 6, 
             dpi = 300)
    })
  
  output$download <- downloadHandler(
    filename = function() { "squares_data.csv" },
    content = function(file) {
      write.csv(squares(), file, row.names = FALSE)
    }
  )
  
  output$download_csv_sequence <- downloadHandler(
    filename = function() { "sequence_data.csv" },
    content = function(file) {
      write.csv(sequence_table(), file, row.names = FALSE)
    }
  )
  
  output$download_csv_clutterness <- downloadHandler(
    filename = function() { "clutterness_data.csv" },
    content = function(file) {
      write.csv(clutterness_df(), file, row.names = FALSE)
    }
  )
  
  output$download_csv_clutterness_count <- downloadHandler(
    filename = function() { "clutterness_count_data.csv" },
    content = function(file) {
      write.csv(clutterness_count_df(), file, row.names = FALSE)
    }
  )
  
  add_new_square <- function(new_label, new_x, new_y, new_side) {
    existing <- squares()
    # new_label <- generate_label(existing$label)
    # new_x <- round(click$x)
    # new_y <- round(click$y)
    # new_side <- input$side
    
    # Check for overlap
    overlap <- existing %>% 
      filter(abs(x - new_x) < (side/2 + new_side/2) & abs(y - new_y) < (side/2 + new_side/2))
    out_of_bounds <- (new_x < new_side/2 | new_y < new_side/2 | 
                        (input$xlim - new_x) < new_side/2 | (input$ylim - new_y) < new_side/2 ) 
    # print(paste('Is out of bound?', out_of_bounds))
    
    if (nrow(overlap) > 0) {
      error_message(paste("Error: The new square overlaps with square", overlap$label[1]))
    } else if (out_of_bounds) {
      error_message("Error: The new square is out of bounds.")
    } else {
      error_message("")
      
      # Use this if you want squares to be added at random locations instead
      # while(overlaps) {
      #   new_x <- sample(seq(new_side/2, input$xlim - new_side/2, by = 10), 1)
      #   new_y <- sample(seq(new_side/2, input$ylim - new_side/2, by = 10), 1)
      #   
      #   overlaps <- any(
      #     abs(existing$x - new_x) < (existing$side/2 + new_side/2) &
      #       abs(existing$y - new_y) < (existing$side/2 + new_side/2)
      #   )
      # }
      new_data <- data.frame(label = new_label, 
                             x = new_x, 
                             y = new_y, 
                             side = new_side, 
                             stringsAsFactors = FALSE) %>% 
        rowwise() %>% 
        mutate(
          x_TR = generate_square_vertices(x, y, side)$x_TR,
          y_TR = generate_square_vertices(x, y, side)$y_TR,
          x_TL = generate_square_vertices(x, y, side)$x_TL,
          y_TL = generate_square_vertices(x, y, side)$y_TL,
          x_BR = generate_square_vertices(x, y, side)$x_BR,
          y_BR = generate_square_vertices(x, y, side)$y_BR,
          x_BL = generate_square_vertices(x, y, side)$x_BL,
          y_BL = generate_square_vertices(x, y, side)$y_BL
        ) %>% 
        ungroup()
      new_data <- rbind(existing, new_data)
      squares(new_data)
    }
  }
  
  observeEvent(input$add, {
    existing <- squares()
    # new_label <- generate_label(nrow(existing) + 1)
    add_new_square(new_label <- generate_label(existing$label), 
                   new_x <- input$x, 
                   new_y <- input$y, 
                   new_side <- input$side)
  })
  
  observeEvent(input$plot_click, {
    click <- input$plot_click
    if (!is.null(click)) {
      existing <- squares()
      # new_label <- generate_label(nrow(existing) + 1)
      add_new_square(new_label <- generate_label(existing$label), 
                     new_x <- round(click$x), 
                     new_y <- round(click$y), 
                     new_side <- input$side)
    }
  })
  
  observeEvent(input$plot_dblclick, {
    click <- input$plot_dblclick
    if (!is.null(click)) {
      existing <- squares()
      filtered_data <- existing[!(abs(existing$x - click$x) < existing$side/2 & abs(existing$y - click$y) < existing$side/2), ]
      squares(filtered_data)
    }
  })

  observeEvent(input$update, {
    existing <- squares()
    req(nrow(existing) > 0)
    
    for (i in seq_len(nrow(existing))) {
      existing$x[i] <- input[[paste0("x_", existing$label[i])]]
      existing$y[i] <- input[[paste0("y_", existing$label[i])]]
      existing$side[i] <- input[[paste0("side_", existing$label[i])]]
      existing$label[i] <- input[[paste0("label_", existing$label[i])]]
    }
    
    existing <- existing %>% 
      rowwise() %>% 
      mutate(
        x_TR = generate_square_vertices(x, y, side)$x_TR,
        y_TR = generate_square_vertices(x, y, side)$y_TR,
        x_TL = generate_square_vertices(x, y, side)$x_TL,
        y_TL = generate_square_vertices(x, y, side)$y_TL,
        x_BR = generate_square_vertices(x, y, side)$x_BR,
        y_BR = generate_square_vertices(x, y, side)$y_BR,
        x_BL = generate_square_vertices(x, y, side)$x_BL,
        y_BL = generate_square_vertices(x, y, side)$y_BL
      ) 
    
    squares(existing)
    draw_line_func(strsplit(input$line_path, "")[[1]], 
                   strsplit(input$overlap_path, "")[[1]])
  })
  
  output$square_controls <- renderUI({
    existing <- squares()
    if (nrow(existing) == 0) return(NULL)
    
    tagList(
      lapply(1:nrow(existing), function(i) {
        fluidRow(
          offset = -10, style='padding:0px;',
          column(3, style = "padding-right: 2px;", textInput(paste0("label_", existing$label[i]), "Name:", existing$label[i])),
          column(3, style = "padding-right: 2px; padding-left: 2px;", numericInput(paste0("x_", existing$label[i]), "X:", existing$x[i])),
          column(3, style = "padding-right: 2px; padding-left: 2px;", numericInput(paste0("y_", existing$label[i]), "Y:", existing$y[i])),
          column(3, style = "padding-left: 2px;", numericInput(paste0("side_", existing$label[i]), "Side:", existing$side[i]))
        )
      })
    )
  })
  
  output$error_message <- renderText({
    error_message()
  })
  
  output$overlap_text <- renderText({
    overlap_text()
  })
  
  observeEvent(input$save_ggplot, {
    file_name <- sprintf("%s_%s.png", input$save_plot_name, ifelse(input$line_path == "", "blank", input$line_path))
    save_dir <- "./Plots"
    height_plot <- 6;
    ggsave(file.path(save_dir, file_name), 
           plot = last_plot(), 
           width = height_plot*input$xlim/input$ylim, height = height_plot, dpi = 300)
  })
  
  make_plot <- function() {
    plot_data <- squares()
    
    p <- ggplot() +
      geom_rect(
        data = plot_data,
        aes(
          xmin = x - side/2, xmax = x + side/2, 
          ymin = y - side/2, ymax = y + side/2
        ),
        fill = "darkgray", color = "black"
      ) +
      geom_text(
        data = plot_data,
        aes(x = x, y = y, label = label),
        color = "white",
        size = 10
      ) +
      coord_fixed() +
      xlim(0, input$xlim) +
      ylim(0, input$ylim) +
      theme_minimal(base_size = 14) +
      theme(panel.background = element_rect(fill = "lightgray"),
            panel.grid.major = element_blank(),  # Remove major gridlines
            panel.grid.minor = element_blank(), 
            axis.line=element_blank())  # Remove minor gridlines
    return(p)
  }
  
  draw_line_func <- function(labels, labels_overlap) {
    existing <- squares()
    # labels <- line_path_labels()
    # labels_overlap <- line_overlap_labels()
    
    if (!all(labels %in% existing$label)) {
      missing_labels <- labels[!(labels %in% existing$label)]
      error_message(paste("Error: The following labels do not exist:", paste(missing_labels, collapse = ", ")))
      return()
    } else {
      error_message("")
      line_data(existing %>% filter(label %in% labels) %>% arrange(match(label, labels)))
    }
    
    if (!all(labels_overlap %in% existing$label)) {
      missing_labels <- labels_overlap[!(labels_overlap %in% existing$label)]
      error_message(paste("Error: The following labels do not exist:", paste(missing_labels, collapse = ", ")))
      return()
    } else {
      error_message("")
      line_data_overlap(existing %>% filter(label %in% labels_overlap) %>% arrange(match(label, labels_overlap)))
    }
    
    output$plot <- renderPlot({
      req(nrow(squares()) > 0)
      
      # plot_data <- squares()
      line_coords <- line_data()
      line_coords_overlap <- line_data_overlap()
      p <- make_plot()
      
      if (!is.null(line_coords) && nrow(line_coords) > 1) {
        p <- p +
          geom_path(
            data = line_coords,
            aes(x = x, y = y),
            color = "black",
            arrow = arrow(type = "closed", length = unit(0.1, "inches")),
            size = 0.7
          ) +
          geom_point(data = line_coords[1, ], aes(x = x, y = y), color = "blue", size = 3)
      }
      
      if (!is.null(line_coords_overlap) && nrow(line_coords_overlap) > 1) {
        line_coords_overlap <- add_directional_columns(line_coords_overlap)
        sequence_table(line_coords_overlap)
        
        clutterness_list <- make_clutterness_dataframe(existing, line_coords_overlap) 
        clutterness_df(clutterness_list$clutterness_df)
        clutterness_count_df(clutterness_list$clutterness_count_df)
        
        labels_overlap_plot <- unique(line_coords_overlap$label)
        
        for (label_current in labels_overlap_plot) {
          p <- p +
            geom_line(
              data = line_coords_overlap %>% filter(label == label_current),
              aes(x = x1, y = y1),
              color = "red",
              linetype = 'dashed',
              linewidth = 0.5
            ) + 
            geom_line(
              data = line_coords_overlap %>% filter(label == label_current),
              aes(x = x2, y = y2),
              color = "red",
              linetype = 'dashed',
              linewidth = 0.5
            ) 
        }
      }
      p
    })
  }
  
  observeEvent(input$draw_line, {
    draw_line_func(strsplit(input$line_path, "")[[1]], 
                   strsplit(input$overlap_path, "")[[1]])
  })
  
  
  output$plot <- renderPlot({
    req(nrow(squares()) > 0)
    
    make_plot()
  })
  
  output$table <- renderTable({
    req(nrow(squares()) > 0)
    squares() 
  })
  
  output$sequence_table <- renderTable({
    req(nrow(sequence_table()) > 0)
    sequence_table() 
  })
  
  output$clutterness_df <- renderTable({
    req(nrow(clutterness_df()) > 0)
    clutterness_df() 
  })
  
  output$clutterness_count_df <- renderTable({
    req(nrow(clutterness_count_df()) > 0)
    clutterness_count_df() 
  })
}

shinyApp(ui, server)
