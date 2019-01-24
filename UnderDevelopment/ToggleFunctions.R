# # Toggle points that are clicked
# ClickToggle <- function(click.coord, keepObs, discardObs){
#   
#   
#   
# }
# 
# 
# observeEvent(input$plot.temp_click, {
#   click.coord <- input$plot.temp_click
#   res <- nearPoints(values$temp.keep, click.coord, threshold = 4, xvar = "y", yvar = "x", allRows = TRUE)
#   values$keeprows.temp <- xor(values$keeprows.temp, res$selected_)
#   
#   res <- nearPoints(values$temp.discard, click.coord, threshold = 4, xvar = "y", yvar = "x", allRows = TRUE)
#   values$discardrows.temp <- xor(values$discardrows.temp, res$selected_)
# })
# 
# # Toggle points that are brushed, when button is clicked
# observeEvent(input$exclude.temp_toggle, {
#   res.keep.temp <- brushedPoints(values$temp.keep, input$plot.temp_brush, allRows = TRUE)
#   values$keeprows.temp <- xor(values$keeprows.temp, res.keep.temp$selected_)
#   
#   res.discard.temp <- brushedPoints(values$temp.discard, input$plot.temp_brush, allRows = TRUE)
#   values$discardrows.temp <- xor(values$discardrows.temp, res.discard.temp$selected_)
# })
# 
# # Clear all selected points
# observeEvent(input$exclude.temp_clear, {
#   values$keeprows.temp[values$keeprows.temp == FALSE] <- TRUE 
#   values$discardrows.temp[values$discardrows.temp == TRUE] <- FALSE
# })
# 
# # Reset all points
# observeEvent(input$exclude.temp_reset, {
#   values$keeprows.temp <- rep(TRUE, nrow(values$temp.keep))
#   values$discardrows.temp <- rep(TRUE, nrow(values$temp.discard))
# })
