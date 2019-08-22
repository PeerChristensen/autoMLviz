#' Functions for plotting variable importance
#'
#' @param H2OAutoML_object An object containing multiple models
#' @param save_pngs (Optional) Whether to save a png files with ggsave(). Default is FALSE.
#' @param return_data (Optional) Whether to save a data frame. Default is set to FALSE.
#' @param n_vars (Optional) The number of variables to include. Default is 25.
#'
#' @export
#' @import purrr
#' @import dplyr
#' @import ggplot2
#' @import h2o
#' @import tidyr
#'
#'

# variable importance with ggplot

varImp_ggplot <- function(H2OAutoML_object, save_pngs = F, return_data = F, n_vars = 25) {

  model <- as.vector(as.character(H2OAutoML_object@leaderboard$model_id)) %>%
    map(h2o.getModel) %>% .[[1]]

  if (model@algorithm == "stackedensemble") {
    print("Ensemble model: Plotting Model importance and Variable importances of model with highest importance")
    metaLearner <- h2o.getModel(model@model$metalearner$name)

    # plot model importance using ggplot2
    metaLearner_df <- metaLearner@model$coefficients_table[-1,] %>%
      arrange(desc(standardized_coefficients)) %>%
      mutate(order = row_number()) %>%
      filter(coefficients > 0.000)

    metaLearner_df$names1 <- str_split(metaLearner_df$names, "_AutoML") %>%
      map_chr(1) %>%
      paste0(metaLearner_df$order,": ",.)

    metaLearner_df$names2 <- str_split(metaLearner_df$names,"(?<=_)(?=[_model])") %>%
      map(2) %>%
      paste("_",.) %>%
      str_remove(" ")

    metaLearner_df$names <- paste0(metaLearner_df$names1,metaLearner_df$names2)
    metaLearner_df$names <- str_remove(metaLearner_df$names,"_NULL")

    p1 <- metaLearner_df %>%
      ggplot(aes(x=reorder(names,rev(order)),standardized_coefficients, fill = factor(order))) +
      geom_col() +
      coord_flip() +
      scale_fill_viridis_d(guide=F) +
      labs(x= "Models", y = "Standard. coefficients") +
      ggtitle("Model importance in ensemble") +
      theme_light() +
      theme(plot.title = element_text(size = 16),
            axis.title = element_text(size = 12),
            axis.text  = element_text(size = 12),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())

    print(p1)

    if (save_pngs == T) {
      ggsave("modelImp.png")
    }

    # VarImp of most important model
    modelImp <- h2o.varimp(metaLearner) # data frame

    highestImpName <- modelImp[1,1]

    model  <- h2o.getModel(as.character(highestImpName))
    varImp <- h2o.varimp(model)
  } else {
    varImp <- h2o.varimp(model)
  }

  if (model@algorithm == "glm") {

    p2 <- varImp %>%
      drop_na() %>%
      top_n(n_vars,variable) %>%
      ggplot(aes(x=reorder(names,coefficients),coefficients, fill = factor(sign))) +
      geom_col() +
      coord_flip() +
      scale_fill_viridis_d("Sign") +
      labs(x= "Variables", y = "Coefficients") +
      ggtitle(paste("Variable importance for", model@algorithm, "model")) +
      theme_light() +
      theme(plot.title = element_text(size = 16),
            axis.title = element_text(size = 12),
            axis.text  = element_text(size = 12),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())

  } else {

    p2 <- varImp %>%
      drop_na() %>%
      top_n(n_vars,scaled_importance) %>%
      ggplot(aes(x=reorder(variable,scaled_importance ),scaled_importance, fill = factor(scaled_importance))) + #fill = factor(sign)
      geom_col() +
      coord_flip() +
      scale_fill_viridis_d(guide = FALSE) +
      labs(x= "Variables", y = "Coefficients") +
      ggtitle(paste("Variable importance for", model@algorithm, "model")) +
      theme_light() +
      theme(plot.title = element_text(size = 16),
            axis.title = element_text(size = 12),
            axis.text  = element_text(size = 12),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())
  }
  print(p2)

  if (save_pngs == T) {
    ggsave("varImp.png")
  }

  if (return_data == T) {
    return(list(modelImp,varImp))
  }
}


