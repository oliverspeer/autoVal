fun_add_scatterplot <- function(df) {
df |> 
  #select(Analyt) |> 
  rowwise() |> 
    mutate(
      plot =  list({
          plot_title <- paste0("Test: ", Analyt)
          ggplot(data, aes(x = DxI800, y = DxI9000)) +
            geom_point() +
            geom_smooth(method = "lm") +
            labs(title = plot_title) +
            theme_minimal()
        })
      ) |> 
    ungroup()
      }

#' @title Add scatterplot to dataframe
#' @description
#' This function adds a scatterplot to a dataframe.
#' It uses the `ggplot2` package to create a scatterplot of two variables.
#' The function takes a dataframe as input and returns the same dataframe with an additional column containing the scatterplot.
#' The scatterplot is created using the `ggplot2` package and is based on the `DxI800` and `DxI9000` variables.
#' The function also adds a title to the plot, which is based on the `Analyt` variable in the dataframe.
#' The function uses the `pmap` function from the `purrr` package to iterate over the rows of the dataframe and create a scatterplot for each row.
#' The function uses the `rowwise` function from the `dplyr` package to ensure that the dataframe is processed row by row.
#' @param df A dataframe containing the data to be plotted.
#' @return A dataframe with an additional column containing the scatterplot.
#' @examples
#' df <- data.frame(
#'  Analyt = c("Test1", "Test2"),
#'  DxI800 = c(1, 2),
#'  DxI9000 = c(3, 4)
#'  )
#'  
