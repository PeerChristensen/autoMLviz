#' Function for plotting AUC bars
#'
#' @param H2OAutoML_object An object containing multiple models trained in H2O.
#' @param save_png (Optional) Whether to save a .png with ggsave(). Default is FALSE.
#' @param test_data The test data set
#' @param n_models (Optional) The number of trained models to include
#'
#' @export
#' @import purrr
#' @import dplyr
#' @import ggplot2
#' @import h2o
#'

auc_bars <- function(H2OAutoML_object, save_png = F, test_data, n_models = 5) {

  models <- as.vector(as.character(H2OAutoML_object@leaderboard$model_id))[1:n_models] %>%
    map(h2o.getModel)

  df <- tibble()

  for (i in 1:length(models)) {

    perf <- h2o.performance(models[[i]], test_data)
    auc  <- perf@metrics$AUC

    model_id  <- models[[i]]@model_id
    algorithm <- models[[i]]@algorithm

    d <- tibble(model_id,algorithm,auc)
    d <- add_column(d, model_rank = i)

    df <- rbind(df,d)
  }

  df$model_id1 <- str_split(df$model_id, "_AutoML") %>%
    map_chr(1) %>%
    paste0(df$model_rank,": ",.)

  df$model_id2 <- str_split(df$model_id,"(?<=_)(?=[_model])") %>%
    map(2) %>%
    paste("_",.) %>%
    str_remove(" ")

  df$model_id <- paste0(df$model_id1,df$model_id2)
  df$model_id <- str_remove(df$model_id,"_NULL")

  p <- df %>%
    ggplot(aes(model_id, auc, fill = reorder(model_id,model_rank))) +
    geom_col() +
    xlab('Models') +
    ylab('AUC') +
    scale_fill_viridis_d(guide=F) +
    geom_text(aes(label=round(auc,3)), position=position_dodge(width=0.9), vjust=-0.9, size = 6) +
    ggtitle('AUC of the best models') +
    theme_light() +
    theme(plot.title = element_text(size = 16),
          axis.title = element_text(size = 12),
          axis.text.x  = element_text(size = 12,angle = 45, hjust = 1),
          axis.text.y  = element_text(size = 12)) +
    ylim(c(0,1))

  print(p)

  if (save_png == T) {
    ggsave("auc_bars.png")
  }
}
