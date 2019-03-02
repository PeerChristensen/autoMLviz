#' Function for plotting AUC bars
#' @param H2OAutoML_object provided by user
#' @param save_png provided by user
#' @param test_data provided by user
#'
#' @export
#' @import purrr
#' @import dplyr
#' @import ggplot2
#' @import h2o
#'

auc_bars <- function(H2OAutoML_object, save_png = F, test_data) {

  models <- as.vector(as.character(H2OAutoML_object@leaderboard$model_id)) %>%
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

  df$model_id <- str_split(df$model_id, "_AutoML") %>%
    map_chr(1) %>%
    paste0(df$model_rank,": ",.)

  p <- df %>%
    ggplot(aes(model_id, auc, fill = model_id)) +
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
