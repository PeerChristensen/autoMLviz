#' Functions for plotting cumulative gains, lift and response charts
#'
#' @param H2OAutoML_object provided by user
#' @param response_ref provided by user
#' @param save_pngs provided by user
#'
#' @export
#' @import purrr
#' @import dplyr
#' @import ggplot2
#' @import h2o
#'

# best models

lift4gains2 <- function(H2OAutoML_object, response_ref = NULL, save_pngs = F) {

  models <- as.vector(as.character(H2OAutoML_object@leaderboard$model_id))

  df <- models %>% map(h2o.getModel) %>% map(h2o.gainsLift) %>% reduce(rbind) %>% as_tibble()

  lengths <- models %>% map(h2o.getModel) %>% map(h2o.gainsLift) %>% map(nrow)

  df$model_id <- factor(rep(models,lengths))
  df$model_rank <- rep(1:length(lengths),lengths)

  df$model_id <- str_split(df$model_id, "_AutoML") %>%
    map_chr(1) %>%
    paste0(df$model_rank,": ",.)

  # Gains
  p1 <- df %>%
    ggplot(aes(x=cumulative_data_fraction,y=cumulative_capture_rate, colour = model_id)) +
    geom_line(size = .8) +
    geom_point(size = 1) +
    geom_segment(aes(x=0,y=0,xend = 1, yend = 1),size = 1,linetype = 2,col='grey')+
    scale_colour_viridis_d("Model") +
    ggtitle("Gains chart",
            subtitle = "When we apply the model and select x % of customers,\nwhat % of the target class observations can we expect to hit?") +
    labs(x = "Data fraction",
         y = "Cumulative gains") +
    theme(plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 14, face = "italic",vjust=-1))

  print(p1)
  if (save_pngs == T) {
    ggsave("gains.png")
  }

  # Lift
  p2 <- df %>%
    ggplot(aes(x=cumulative_data_fraction,y=cumulative_lift, colour = model_id)) +
    geom_line(size = .8) +
    geom_point(size = 1) +
    geom_segment(aes(x=0,y=1,xend = 1, yend = 1),size = 1,linetype = 2,col='grey')+
    scale_colour_viridis_d("Model") +
    ggtitle("Lift chart",
            subtitle = "When we apply the model and select x % of customers,\nhow many times better is that than using no model?") +
    labs(x = "Data fraction",
         y = "Cumulative lift") +
    theme(plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 14, face = "italic",vjust=-1))

  print(p2)
  if (save_pngs == T) {
    ggsave("lift.png")
  }

  # Response
  p3 <- df %>%
    ggplot(aes(x=cumulative_data_fraction,y=cumulative_response_rate, colour = model_id)) +
    geom_line(size = .8) +
    geom_point(size = 1) +
    ylim(c(0,1)) +
    scale_colour_viridis_d("Model",alpha=.6) +
    ggtitle("Response chart",
            subtitle = "When we apply the model and select x % of customers, \nwhat is the expected % of target class observations in the selection?") +
    labs(x = "Data fraction",
         y = "Cumulative response") +
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


