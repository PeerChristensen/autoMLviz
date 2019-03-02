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

varImp_plot <- function(H2OAutoML_object, save_pngs = F, return_data = F) {

  model <- as.vector(as.character(H2OAutoML_object@leaderboard$model_id)) %>%
    map(h2o.getModel) %>% .[[1]]

  if (model@algorithm == "stackedensemble") {
    print("Ensemble model: Plotting Model importance and Variable importances of model with highest importance")
    metaLearner <- h2o.getModel(model@model$metalearner$name)

    # plot model importance using H2O
    h2o.varimp_plot(metaLearner)
    if (save_pngs == T) {
      ggsave("modelImp.png")
    }

    # VarImp of most important model
    modelImp <- h2o.varimp(metaLearner) # data frame

    highestImpName <- modelImp[1,1]

    model  <- h2o.getModel(highestImpName)
    varImp <- h2o.varimp(model)
  } else {
    varImp <- h2o.varimp(model)
  }
  h2o.varimp_plot(model)
  if (save_pngs == T) {
    ggsave("varImp.png")
  }

  if (return_data == T) {
    return(list(modelImp,varImp))
  }
}
