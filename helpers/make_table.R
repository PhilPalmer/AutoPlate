############################################################
# Make an interactive table
# http://stla.github.io/stlapblog/posts/shiny_editTable.html
############################################################

make_table <- function(input,output,df,table_name,values) {
  observe({
    if (!is.null(input[[table_name]])) {
      values[["previous"]] <- isolate(values[["df"]])
      df = hot_to_r(input[[table_name]])
    } else {
      if (is.null(values[["df"]]))
        df <- df
      else
        df <- values[["df"]]
    }
    values[["df"]] <- df
  })
  output[[table_name]] <- renderRHandsontable({
    df <- values[["df"]]
    if (!is.null(df))
      rhandsontable(df, stretchH = "all")
  })
}