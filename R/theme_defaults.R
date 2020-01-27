#' Default ggplot2 themes
#' 
#' @export
my_theme <- function() {
  theme_bw() +
    theme(panel.grid = element_blank(),
          strip.background = element_rect(fill = "white"),
          plot.title = element_text(hjust = .5, size = 25,
                                    margin = ggplot2::margin(t = 0, r = 0, 
                                                             b = 20, l = 0)),
          plot.subtitle = element_text(hjust = .5, size = 20,
                                       margin = ggplot2::margin(t = 0, r = 0, 
                                                                b = 10, l = 0)),
          axis.title = element_text(hjust = .5, size = 20),
          axis.text = element_text(hjust = .5, size = 15),
          strip.text = element_text(hjust = .5, size = 20),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 20),
          axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 10, 
                                                               b = 0, l = 0)),
          axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, 
                                                               b = 0, l = 0)))
}


my_theme <- function(base_size = 11, base_family = "",
                     base_line_size = base_size / 22,
                     base_rect_size = base_size / 22) {
  # Starts with theme_bw and then modify some parts
  theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(panel.grid = element_blank(),
          strip.background = element_rect(fill = "white"),
          plot.title = element_text(hjust = .5, size = 25,
                                    margin = ggplot2::margin(t = 0, r = 0, 
                                                             b = 20, l = 0)),
          plot.subtitle = element_text(hjust = .5, size = 20,
                                       margin = ggplot2::margin(t = 0, r = 0, 
                                                                b = 10, l = 0)),
          axis.title = element_text(hjust = .5, size = 20),
          axis.text = element_text(hjust = .5, size = 15),
          strip.text = element_text(hjust = .5, size = 20),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 20),
          axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 10, 
                                                               b = 0, l = 0)),
          axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, 
                                                               b = 0, l = 0)), 
          complete = TRUE)
}

