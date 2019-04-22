#' Functions for plotting cumulative gains, lift and response charts
#'
#' @param H2OAutoML_object An object containing multiple models trained in H2O.
#' @param response_ref (Optional) You can include a reference line in the response chart by providing the rate of occurence of the target class, i.e. the proportion of the target class in the data.
#' @param save_pngs (Optional) Whether to save a png files with ggsave(). Default is FALSE.
#' @param n_models (Optional) The number of trained models to include.
#'
#' @export
#' @import purrr
#' @import dplyr
#' @import ggplot2
#' @import h2o
#'

# best models

lift4gains2 <- function(H2OAutoML_object, response_ref = NULL, save_pngs = F, n_models = 5) {

  models <- as.vector(as.character(H2OAutoML_object@leaderboard$model_id))[1:n_models]

  df <- models %>% map(h2o.getModel) %>% map(h2o.gainsLift) %>% reduce(rbind) %>% as_tibble()

  lengths <- models %>% map(h2o.getModel) %>% map(h2o.gainsLift) %>% map(nrow)

  df$model_id <- factor(rep(models,lengths))
  df$model_rank <- rep(1:length(lengths),lengths)

  df$model_id1 <- str_split(df$model_id, "_AutoML") %>%
    map_chr(1) %>%
    paste0(df$model_rank,": ",.)

  df$model_id2 <- str_split(df$model_id,"(?<=_)(?=[_model])") %>%
    map(2) %>%
    paste("_",.) %>%
    str_remove(" ")

  df$model_id <- paste0(df$model_id1,df$model_id2)
  df$model_id <- str_remove(df$model_id,"_NULL")

  # Gains
  p1 <- df %>%
    ggplot(aes(x=cumulative_data_fraction,y=cumulative_capture_rate, colour = reorder(model_id,model_rank))) +
    geom_line(size = .8) +
    geom_point(size = 1) +
    geom_segment(aes(x=0,y=0,xend = 1, yend = 1),size = 1,linetype = 2,col='grey')+
    scale_colour_viridis_d("Model") +
    ggtitle("Gains chart",
            subtitle = "When we apply the model and select x % of observations,\nwhat % of the target class observations can we expect to hit?") +
    labs(x = "Data fraction",
         y = "Cumulative gains") +
    theme_light() +
    theme(plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 14, face = "italic",vjust=-1))

  print(p1)
  if (save_pngs == T) {
    ggsave("gains.png")
  }

  # Lift
  p2 <- df %>%
    ggplot(aes(x=cumulative_data_fraction,y=cumulative_lift, colour = reorder(model_id,model_rank))) +
    geom_line(size = .8) +
    geom_point(size = 1) +
    geom_segment(aes(x=0,y=1,xend = 1, yend = 1),size = 1,linetype = 2,col='grey')+
    scale_colour_viridis_d("Model") +
    ggtitle("Lift chart",
            subtitle = "When we apply the model and select x % of observations,\nhow many times better is that than using no model?") +
    labs(x = "Data fraction",
         y = "Cumulative lift") +
    theme_light() +
    theme(plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 14, face = "italic",vjust=-1))

  print(p2)
  if (save_pngs == T) {
    ggsave("lift.png")
  }

  # Response
  p3 <- df %>%
    ggplot(aes(x=cumulative_data_fraction,y=cumulative_response_rate, colour = reorder(model_id,model_rank))) +
    geom_line(size = .8) +
    geom_point(size = 1) +
    ylim(c(0,1)) +
    scale_colour_viridis_d("Model",alpha=.6) +
    ggtitle("Response chart",
            subtitle = "When we apply the model and select x % of observations, \nwhat is the expected % of target class observations in the selection?") +
    labs(x = "Data fraction",
         y = "Cumulative response") +
    theme_light() +
    theme(plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 14, face = "italic",vjust=-1))

  if (!is.null(response_ref)) {
    p3 <- p3 + geom_segment(aes(x=0,y = response_ref,xend = 1, yend = response_ref),size = 1,linetype = 2,col='grey')
  }

  print(p3)
  if (save_pngs == T) {
    ggsave("response.png")
  }
}


