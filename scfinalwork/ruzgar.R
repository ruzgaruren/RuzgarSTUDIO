install.packages("tidyverse")
install.packages("DataExplorer")



library(tidyverse)
library(magrittr)
library(rvest)
library(stringr)
library(DataExplorer)

url<- "https://www.blockchain.com/prices"
html <-  read_html(url)
view(html)

node<- html %>% html_nodes('.atmdef') %>% html_text()
node
