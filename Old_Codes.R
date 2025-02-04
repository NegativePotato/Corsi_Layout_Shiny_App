# observeEvent(input$update, {
#   inputs <- reactiveValuesToList(input)
#   existing <- squares()
#   req(nrow(existing) > 0)
# 
#   for (i in seq_len(nrow(existing))) {
#     req(
#       !is.null(inputs[[paste0("x_", existing$label[i])]]),
#       !is.null(inputs[[paste0("y_", existing$label[i])]]),
#       !is.null(inputs[[paste0("side_", existing$label[i])]]),
#       !is.null(inputs[[paste0("label_", existing$label[i])]])
#     )
# 
#     
#     for (i in seq_len(nrow(existing))) {
#       observeEvent(input[[paste0("x_", existing$label[i])]], {
#         updated_data <- squares()
#         updated_data$x[i] <- input[[paste0("x_", existing$label[i])]]
#         squares(updated_data)
#       }, ignoreInit = TRUE)
#       
#       observeEvent(input[[paste0("y_", existing$label[i])]], {
#         updated_data <- squares()
#         updated_data$y[i] <- input[[paste0("y_", existing$label[i])]]
#         squares(updated_data)
#       }, ignoreInit = TRUE)
#       
#       observeEvent(input[[paste0("side_", existing$label[i])]], {
#         updated_data <- squares()
#         updated_data$side[i] <- input[[paste0("side_", existing$label[i])]]
#         squares(updated_data)
#       }, ignoreInit = TRUE)
#       
#       observeEvent(input[[paste0("label_", existing$label[i])]], {
#         updated_data <- squares()
#         updated_data$label[i] <- input[[paste0("label_", existing$label[i])]]
#         squares(updated_data)
#       }, ignoreInit = TRUE)
#     }
#   }
# 
#   squares(existing)
#   draw_line_func()
# })