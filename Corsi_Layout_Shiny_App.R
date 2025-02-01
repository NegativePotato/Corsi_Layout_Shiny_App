library(shiny)
library(ggplot2)
library(dplyr)

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

ui <- fluidPage(
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
               downloadButton("download", "Download Data"),
               uiOutput("square_controls")
      ),
      width = 4
    ),
    
    mainPanel(
      plotOutput("plot", click = "plot_click", brush = "plot_brush", dblclick = "plot_dblclick"),
      verbatimTextOutput("error_message"),
      fluidRow(
        column(6, textInput("line_path", "Enter square sequence (e.g., 38A4):", "")),
        # column(2, style = "padding-left: 2px;", actionButton("draw_line", "Draw Line")),
        column(4, textInput("save_plot_name", "Enter a file name:", "")),
        column(2, style = "padding-left: 2px;", actionButton("save_ggplot", "Save Plot"))
      ),
      tableOutput("table"),
      width = 8
    )
  )
)

server <- function(input, output, session) {
  squares <- reactiveVal(data.frame(label = character(), x = numeric(), y = numeric(), side = numeric()))
  error_message <- reactiveVal("")
  line_data <- reactiveVal(NULL)
  line_path_labels <- reactive(strsplit(input$line_path, "")[[1]])
  
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
    
    if (!all(labels %in% existing$label)) {
      missing_labels <- labels[!(labels %in% existing$label)]
      error_message(paste("Error: The following labels do not exist:", paste(missing_labels, collapse = ", ")))
      return()
    } else {
      error_message("")
      line_data(existing %>% filter(label %in% labels) %>% arrange(match(label, labels)))
    }
    
    output$plot <- renderPlot({
      req(nrow(squares()) > 0)
      
      # plot_data <- squares()
      line_coords <- line_data()
      
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
      
      p
    })
  }
  
  # observeEvent(input$draw_line, {
  #   draw_line_func()
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

  observe({
    inputs <- reactiveValuesToList(input)
    existing <- squares()
    req(nrow(existing) > 0)
  
    for (i in seq_len(nrow(existing))) {
      req(
        !is.null(inputs[[paste0("x_", existing$label[i])]]),
        !is.null(inputs[[paste0("y_", existing$label[i])]]),
        !is.null(inputs[[paste0("side_", existing$label[i])]]),
        !is.null(inputs[[paste0("label_", existing$label[i])]])
      )

      
      for (i in seq_len(nrow(existing))) {
        observeEvent(input[[paste0("x_", existing$label[i])]], {
          updated_data <- squares()
          updated_data$x[i] <- input[[paste0("x_", existing$label[i])]]
          squares(updated_data)
        }, ignoreInit = TRUE)
        
        observeEvent(input[[paste0("y_", existing$label[i])]], {
          updated_data <- squares()
          updated_data$y[i] <- input[[paste0("y_", existing$label[i])]]
          squares(updated_data)
        }, ignoreInit = TRUE)
        
        observeEvent(input[[paste0("side_", existing$label[i])]], {
          updated_data <- squares()
          updated_data$side[i] <- input[[paste0("side_", existing$label[i])]]
          squares(updated_data)
        }, ignoreInit = TRUE)
        
        observeEvent(input[[paste0("label_", existing$label[i])]], {
          updated_data <- squares()
          updated_data$label[i] <- input[[paste0("label_", existing$label[i])]]
          squares(updated_data)
        }, ignoreInit = TRUE)
      }
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
  
  observeEvent(input$save_ggplot, {
    file_name <- sprintf("%s_%s.png", input$save_plot_name, ifelse(input$line_path == "", "blank", input$line_path))
    save_dir <- "./Plots"
    height_plot <- 6;
    ggsave(file.path(save_dir, file_name), 
           plot = last_plot(), 
           width = height_plot*input$xlim/input$ylim, height = height_plot, dpi = 300)
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
