#' Functions for plotting cumulative gains, lift and response charts
#'
#' @param H2OAutoML_object An object containing multiple models trained in H2O.
#' @param response_ref (Optional) You can include a reference line in the response chart by providing the rate of occurence of the target class, i.e. the proportion of the target class in the data.
#' @param save_pngs (Optional) Whether to save a png files with ggsave(). Default is FALSE.
#'
#' @export
#' @import purrr
#' @import dplyr
#' @import ggplot2
#' @import h2o
#'

# single model (best model)
lift4gains <- function(H2OAutoML_object, response_ref = NULL, save_pngs = F) {

  model <- as.vector(as.character(H2OAutoML_object@leaderboard$model_id))[1]

  df <- h2o.getModel(model) %>% h2o.gainsLift()

  # Gains
  p1 <- df %>%
    ggplot(aes(x=cumulative_data_fraction,y=cumulative_capture_rate)) +
    geom_line(size = .8) +
    geom_point(size = 1) +
    geom_segment(aes(x=0,y=0,xend = 1, yend = 1),size = 1,linetype = 2,col='grey')+
    ggtitle("Gains chart",
            subtitle = "When we apply the model and select x % of observations,\nwhat % of the target class observations can we expect to hit?") +
    labs(x = "Data fraction",
         y = "Cumulative gains") +
    theme_light() +
    theme(plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 12, face = "italic",vjust=-1))

  print(p1)
  if (save_pngs == T) {
    ggsave("gains.png")
  }

  # Lift
  p2 <- df %>%
    ggplot(aes(x=cumulative_data_fraction,y=cumulative_lift)) +
    geom_line(size = .8) +
    geom_point(size = 1) +
    geom_segment(aes(x=0,y=1,xend = 1, yend = 1),size = 1,linetype = 2,col='grey')+
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
    ggplot(aes(x=cumulative_data_fraction,y=cumulative_response_rate)) +
    geom_line(size = .8) +
    geom_point(size = 1) +
    ylim(c(0,1)) +
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
