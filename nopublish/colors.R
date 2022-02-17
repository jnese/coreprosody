
library(colorfindr)
library(tidyverse)

get_colors(
  img = "https://storage.googleapis.com/cdn.vinoshipper.com/wine/5edbcc33-bbec-499c-a601-631d59677ed3.jpg",
  top_n = 60
) %>% 
#slice(50:60) %>% 
  plot_colors(sort = "size")

red = #A5352A; or #902B24
blue = #6699AC; or #82A1B7
yellow = #C69932; or #CEB14D
green = #B6B246; or #B4B045
beige = #D2C196 (background)