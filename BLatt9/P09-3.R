# hier Namen einfügen

library(dplyr)
library(rlang)
library(ggplot2)
library(readr)
library(stringr)
library(tidyr)




#1
rate <- function(tb, expr, name) {
  expr_quo <- enquo(expr)
  
  tb %>%
    mutate(
      !!name := {
        x <- eval_tidy(expr_quo, data = tb)
        x / sum(x)
      }
    )
}

#2
show_na <- function(tb, expr) {
  expr_quo <- enquo(expr)
  
  tb %>%
    filter(is.na(eval_tidy(expr_quo, data = tb)))
}

#3
ggplot_line <- function(tb, expr1, expr2) {
  expr1_quo <- enquo(expr1)
  expr2_quo <- enquo(expr2)
  
  tb_eval <- tb %>%
    mutate(
      .x = eval_tidy(expr1_quo, data = tb),
      .y = eval_tidy(expr2_quo, data = tb)
    )
  
  ggplot(tb_eval, aes(x = .x, y = .y)) +
    geom_line()
}

#4
athletes <- read_csv("D:/01_Uni/Programmiersprache R/athletes.csv",
                     show_col_types = FALSE)

medal_colors <- c(
  Bronze = "#6A3805",
  Silver = "#B4B4B4",
  Gold   = "#AF9500"
)



plot_olympic <- function(event, Sex, metric) {
  metric_quo <- enquo(metric)
  
  d <- athletes %>%
    filter(Event == event, Sex == Sex) %>%
    mutate(
      Year = as.factor(Year),
      metric_val = eval_tidy(metric_quo, data = cur_data())
    )
  
  ggplot(d, aes(x = Year, y = metric_val)) +
    geom_boxplot(na.rm = TRUE) +
    geom_point(
      data = d %>% filter(!is.na(metric_val)),
      aes(color = Medal)
    ) +
    scale_color_manual(values = medal_colors) +
    ggtitle(str_c(event, ", ", Sex))
}
plot_olympic("10,000 metres", "M", Weight / (Height / 100)^2)