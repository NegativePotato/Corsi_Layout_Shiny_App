library(shiny)
library(ggplot2)
library(dplyr)
library(shinyjs)

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
generate_label <- function(n) {
  labels <- c(as.character(1:9), LETTERS)
  labels[n]
}

# Check direction of sequence
add_directional_columns <- function(df) {
  output <- df %>%
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
    ungroup() %>%
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
                              ifelse(left_to_right_next & !bottom_to_top_next, lead(y_BL), lead(y_TL))))
    ) %>%
    select(label, x1_current, y1_current, x2_current, y2_current, x1_next, y1_next, x2_next, y2_next) %>% 
    filter(!is.na(x1_current))
  print(output)
  
  output <- output %>%
    tidyr::pivot_longer(cols = contains("_"),
                        names_to = c(".value", "element"), 
                        names_sep = "_")
  
  return(output)
}


ui <- fluidPage(
  useShinyjs(), 
  
  tags$head(
    tags$style(HTML(
      ".form-control { height:auto; padding:3px 5px;}"
    ))
  ),

  titlePanel("Corsi layout creator tool"),
  
  sidebarLayout(
    sidebarPanel(
      tags$div(style = "height: 600px; overflow-y: auto;",  # Make sidebar scrollable
               fluidRow(
                 column(6, style = "padding-right: 2px;", numericInput("xlim", "X-axis limit:", value = 500, min = 100, step = 50)),
                 column(6, style = "padding-left: 2px;", numericInput("ylim", "Y-axis limit:", value = 500, min = 100, step = 50))
               ),
               fluidRow(
                 column(4, style = "padding-right: 2px;", numericInput("x", "Square center X:", value = 250)),
                 column(4, style = "padding-left: 2px; padding-right: 2px;", numericInput("y", "Square center Y:", value = 250)),
                 column(4, style = "padding-left: 2px;", numericInput("side", "Side length:", value = 50, min = 10),),
               ),
               actionButton("add", "Add Square"),
               actionButton("update", "Update Squares", disabled = TRUE),
               uiOutput("square_controls")
      ),
      width = 4
    ),
    
    mainPanel(
      plotOutput("plot", click = "plot_click", brush = "plot_brush", dblclick = "plot_dblclick"),
      verbatimTextOutput("error_message"),
      div(
        fluidRow(
          column(5, textInput("line_path", value = "", placeholder = "Sequence (e.g., 38A4):", label = NULL)),
          column(4, textInput("overlap_path", value = "", placeholder = "Overlap pair: ", label = NULL)),
          column(3, actionButton("draw_line", "Draw Line", style = "height: 100%;")),
        )
      ),
      verbatimTextOutput("overlap_text"),
      fluidRow(
        column(6, textInput("save_plot_name", value = "", placeholder = "Enter a name for the plot file", label = NULL)),
        column(3, actionButton("save_ggplot", "Save Plot", style = "height: 100%;", disabled = TRUE)),
        column(3, downloadButton('download_gglot','Download Plot', style = "height: 100%;", disabled = TRUE))
      ),
      fluidRow(
        column(6, fileInput("upload_csv", "Upload CSV File", accept = ".csv")),
        column(6, downloadButton("download", "Download CSV", style = "height: 100%;"))
      ),
      tableOutput("table"),
      width = 8
    )
  )
)

server <- function(input, output, session) {
  squares <- reactiveVal(data.frame(label = character(), x = numeric(), y = numeric(), side = numeric()))
  error_message <- reactiveVal("")
  overlap_text <- reactiveVal("TODO! Text will be : Overlap area = XXX ")
  plot_file_name <- reactive(input$save_plot_name)
  line_data <- reactiveVal(NULL)
  line_data_overlap <- reactiveVal(NULL)
  line_path_labels <- reactive(strsplit(input$line_path, "")[[1]])
  
  observeEvent(squares(), {
    updateActionButton(session, "update", disabled = nrow(squares()) == 0)
  })
  
  observe({
    if (nrow(squares()) == 0) {
      shinyjs::disable("download")
      shinyjs::disable("download_gglot")
    } else {
      shinyjs::enable("download")
      shinyjs::enable("download_gglot")
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
  
  observeEvent(input$add, {
    existing <- squares()
    new_label <- generate_label(nrow(existing) + 1)
    
    # Ensure non-overlapping placement
    new_x <- input$x
    new_y <- input$y
    new_side <- input$side
    
    # Check for overlap
    overlap <- existing %>% 
      filter(abs(x - new_x) < (side/2 + new_side/2) & abs(y - new_y) < (side/2 + new_side/2))
    out_of_bounds <- (new_x < new_side/2 | new_y < new_side/2 | 
                        (input$xlim - new_x) < new_side/2 | (input$ylim - new_y) < new_side/2 ) 
    print(out_of_bounds)
    
    if (nrow(overlap) > 0) {
      error_message(paste("Error: The new square overlaps with square", overlap$label[1]))
    } else if (out_of_bounds) {
      error_message("Error: The new square is out of bounds.")
    } else {
      error_message("")
      
      # Use this if you want squares to be added at rqandom locations instead
      # while(overlaps) {
      #   new_x <- sample(seq(new_side/2, input$xlim - new_side/2, by = 10), 1)
      #   new_y <- sample(seq(new_side/2, input$ylim - new_side/2, by = 10), 1)
      #   
      #   overlaps <- any(
      #     abs(existing$x - new_x) < (existing$side/2 + new_side/2) &
      #       abs(existing$y - new_y) < (existing$side/2 + new_side/2)
      #   )
      # }
      
      new_data <- rbind(existing, data.frame(label = new_label, x = new_x, y = new_y, side = new_side, stringsAsFactors = FALSE))
      squares(new_data)
    }
      
  })
  
  draw_line_func <- function() {
    existing <- squares()
    labels <- line_path_labels()
    labels_overlap <- strsplit(input$overlap_path, "")[[1]]
    
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
            size = 1
          ) +
          geom_point(data = line_coords[1, ], aes(x = x, y = y), color = "blue", size = 4)
      }
      
      if (!is.null(line_coords_overlap) && nrow(line_coords_overlap) > 1) {
        line_coords_overlap <- add_directional_columns(line_coords_overlap)
        print(line_coords_overlap)
        
        p <- p +
          geom_line(
            data = line_coords_overlap,
            aes(x = x1, y = y1),
            color = "red",
            linetype = 'dashed',
            linewidth = 0.5
          ) + 
          geom_line(
            data = line_coords_overlap,
            aes(x = x2, y = y2),
            color = "red",
            linetype = 'dashed',
            linewidth = 0.5
          ) 
      }
      
      p
    })
  }
  
  
  observeEvent(input$draw_line, {
    draw_line_func()
  })
  # 
  # observeEvent(input$overlap_path_button, { 
  # 
  # })
  
  observeEvent(input$plot_click, {
    click <- input$plot_click
    if (!is.null(click)) {
      existing <- squares()
      new_label <- generate_label(nrow(existing) + 1)
      new_x <- click$x
      new_y <- click$y
      new_side <- input$side
      
      # Check for overlap
      overlap <- existing %>% 
        filter(abs(x - new_x) < (side/2 + new_side/2) & abs(y - new_y) < (side/2 + new_side/2))
      out_of_bounds <- (new_x < new_side/2 | new_y < new_side/2 | 
                          (input$xlim - new_x) < new_side/2 | (input$ylim - new_y) < new_side/2 ) 
      print(out_of_bounds)
      
      if (nrow(overlap) > 0) {
        error_message(paste("Error: The new square overlaps with square", overlap$label[1]))
      } else if (out_of_bounds) {
        error_message("Error: The new square is out of bounds.")
      } else {
        error_message("")
        
        # Use this if you want squares to be added at rqandom locations instead
        # while(overlaps) {
        #   new_x <- sample(seq(new_side/2, input$xlim - new_side/2, by = 10), 1)
        #   new_y <- sample(seq(new_side/2, input$ylim - new_side/2, by = 10), 1)
        #   
        #   overlaps <- any(
        #     abs(existing$x - new_x) < (existing$side/2 + new_side/2) &
        #       abs(existing$y - new_y) < (existing$side/2 + new_side/2)
        #   )
        # }
        
        new_data <- rbind(existing, data.frame(label = new_label, x = new_x, y = new_y, side = new_side, stringsAsFactors = FALSE))
        squares(new_data)
      }
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
  
  # observe({
  #   existing <- squares()
  #   lapply(1:nrow(existing), function(i) {
  #     updateNumericInput(session, paste0("x_", existing$label[i]), value = existing$x[i])
  #     updateNumericInput(session, paste0("y_", existing$label[i]), value = existing$y[i])
  #     updateNumericInput(session, paste0("side_", existing$label[i]), value = existing$side[i])
  #   })
  # })

  observeEvent(input$update, {
    existing <- squares()
    req(nrow(existing) > 0)
    
    for (i in seq_len(nrow(existing))) {
      existing$x[i] <- input[[paste0("x_", existing$label[i])]]
      existing$y[i] <- input[[paste0("y_", existing$label[i])]]
      existing$side[i] <- input[[paste0("side_", existing$label[i])]]
      existing$label[i] <- input[[paste0("label_", existing$label[i])]]
    }
    squares(existing)
    draw_line_func()
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
        size = 5
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
  
  output$plot <- renderPlot({
    req(nrow(squares()) > 0)
    
    make_plot()
  })
  
  output$table <- renderTable({
    req(nrow(squares()) > 0)
    squares() %>% 
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
  })
}

shinyApp(ui, server)
