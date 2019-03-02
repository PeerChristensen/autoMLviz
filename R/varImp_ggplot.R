#' Functions for plotting variable importance
#'
#' @param H2OAutoML_object provided by user
#' @param save_pngs provided by user
#' @param return_data provided by user
#'
#' @export
#' @import purrr
#' @import dplyr
#' @import ggplot2
#' @import h2o
#'
#'

# variable importance with ggplot

varImp_ggplot <- function(H2OAutoML_object, save_pngs = F, return_data = F) {

  model <- as.vector(as.character(H2OAutoML_object@leaderboard$model_id)) %>%
    map(h2o.getModel) %>% .[[1]]

  if (model@algorithm == "stackedensemble") {
    print("Ensemble model: Plotting Model importance and Variable importances of model with highest importance")
    metaLearner <- h2o.getModel(model@model$metalearner$name)

    # plot model importance using ggplot2
    metaLearner_df <- metaLearner@model$coefficients_table[-1,] %>%
      arrange(desc(standardized_coefficients)) %>%
      mutate(order = row_number())

    metaLearner_df$names <- str_split(metaLearner_df$names, "_AutoML") %>%
      map_chr(1)

    p1 <-metaLearner_df %>%
      ggplot(aes(x=reorder(names,standardized_coefficients),standardized_coefficients, fill = factor(order))) +
      geom_col() +
      coord_flip() +
      scale_fill_viridis_d(guide=F) +
      labs(x= "Models", y = "Standard. coefficients") +
      ggtitle("Model importance in ensemble") +
      theme(plot.title = element_text(size = 16),
            axis.title = element_text(size = 12),
            axis.text  = element_text(size = 12))

    print(p1)

    if (save_pngs == T) {
      ggsave("modelImp.png")
    }

    # VarImp of most important model
    modelImp <- h2o.varimp(metaLearner_df) # data frame

    highestImpName <- modelImp[1,1]

    model  <- h2o.getModel(highestImpName)
    varImp <- h2o.varimp(model)
  } else {
    varImp <- h2o.varimp(model)
  }

  if (model@algorithm == "glm") {

    p2 <- varImp %>%
      ggplot(aes(x=reorder(names,coefficients),coefficients), fill = factor(sign)) +
      geom_col() +
      coord_flip() +
      scale_fill_viridis_d("Sign") +
      labs(x= "Variables", y = "Coefficients") +
      ggtitle(paste("Variable importance for", model@algorithm, "model")) +
      theme_light() +
      theme(plot.title = element_text(size = 16),
            axis.title = element_text(size = 12),
            axis.text  = element_text(size = 12))

  } else {
  p2 <- varImp %>%
    ggplot(aes(x=reorder(variable,scaled_importance ),scaled_importance)) + #fill = factor(sign)
    geom_col() +
    coord_flip() +
    scale_fill_viridis_d("Sign") +
    labs(x= "Variables", y = "Coefficients") +
    ggtitle(paste("Variable importance for", model@algorithm, "model")) +
    theme_light() +
    theme(plot.title = element_text(size = 16),
          axis.title = element_text(size = 12),
          axis.text  = element_text(size = 12))
  }
  print(p2)

  if (save_pngs == T) {
    ggsave("varImp.png")
  }

  if (return_data == T) {
    return(list(modelImp,varImp))
  }
}

# # variable importance with ggplot
#
# varImp_ggplot <- function(H2OAutoML_object, save_pngs = F, return_data = F) {
#
#   model <- as.vector(as.character(H2OAutoML_object@leaderboard$model_id)) %>%
#     map(h2o.getModel) %>% .[[1]]
#
#   if (model@algorithm == "stackedensemble") {
#     print("Ensemble model: Plotting Model importance and Variable importances of model with highest importance")
#     metaLearner <- h2o.getModel(model@model$metalearner$name)
#
#     # plot model importance using ggplot2
#     metaLearner_df <- metaLearner@model$coefficients_table[-1,] %>%
#       arrange(desc(standardized_coefficients)) %>%
#       mutate(order = row_number())
#
#     metaLearner_df$names <- str_split(metaLearner_df$names, "_AutoML") %>%
#       map_chr(1)
#
#     p1 <-metaLearner_df %>%
#       ggplot(aes(x=reorder(names,standardized_coefficients),standardized_coefficients, fill = factor(order))) +
#       geom_col() +
#       coord_flip() +
#       scale_fill_viridis_d(guide=F) +
#       labs(x= "Models", y = "Standard. coefficients") +
#       ggtitle("Model importance in ensemble") +
#       theme(plot.title = element_text(size = 16),
#             axis.title = element_text(size = 12),
#             axis.text  = element_text(size = 12))
#
#     print(p1)
#
#     if (save_pngs == T) {
#       ggsave("modelImp.png")
#     }
#
#     # VarImp of most important model
#     modelImp <- h2o.varimp(metaLearner) # data frame
#
#     highestImpName <- modelImp[1,1]
#
#     model  <- h2o.getModel(highestImpName)
#     varImp <- h2o.varimp(model)
#   } else {
#     varImp <- h2o.varimp(model)
#   }
#
#   p2 <- varImp %>%
#     ggplot(aes(x=reorder(names,coefficients),coefficients, fill = factor(sign))) +
#     geom_col() +
#     coord_flip() +
#     scale_fill_viridis_d("Sign") +
#     labs(x= "Variables", y = "Coefficients") +
#     ggtitle(paste("Variable importance for", metaLearner_df$names[1])) +
#     theme(plot.title = element_text(size = 16),
#           axis.title = element_text(size = 12),
#           axis.text  = element_text(size = 12))
#
#   print(p2)
#
#   if (save_pngs == T) {
#     ggsave("varImp.png")
#   }
#
#   if (return_data == T) {
#     return(list(modelImp,varImp))
#   }
# }


