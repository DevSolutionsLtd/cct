# helpers.R

##
## Draws a plot (ggplot2 dialect)
##
make_plot <- function(x,
                      y = NULL,
                      title = character(),
                      set.x = NULL,
                      set.y = NULL,
                      pos = "stack",
                      coloured = TRUE)
{
  df <- if (has.one.var <- is.null(y))
    drop_na(df, x)
  else
    drop_na(df, x, y)
  
  ggObj <- ggplot(df, aes_string(x)) +
    theme(
      legend.title = element_blank(),
      axis.ticks.x = element_blank(),
      plot.title = element_text(size = 11)
    )
  
  ggObj <- if (has.one.var) {
    ggObj <- if (coloured) {
      ggObj +
        geom_bar(aes_string(fill = x)) +
        theme(axis.text.x = element_blank())
    }
    else {
      ggObj +
        geom_bar() +
        theme(axis.text.x = element_text())
    }
    ggObj 
  }
  else {
    ggObj +
      geom_bar(aes_string(fill = y), position = pos) +
      theme(axis.text.x = element_text())
  }
  invisible(ggObj +
              labs(title = title, x = set.x, y = set.y))
}


