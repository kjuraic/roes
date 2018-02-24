library(stringi)
library(stringr)
library(readr)
library(ggplot2)
library(plyr)

View(stri_replace(list.files("data/Database/"), "",fixed = ".txt"))




chem <- c("TII_I", "O_I", "N_I")
d <- read_nist_oes_multi(element_list = chem)

ggplot(data = d) +
  geom_segment(aes(x = wavelength, xend = wavelength, y = 0, yend = rel., color= element)) +
  theme_bw()+ ylim(0,30000) +
  xlim(200,1000)
