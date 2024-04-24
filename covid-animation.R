
# geographical functions
if (!require("devtools", quietly = TRUE)) install.packages("devtools")
if (!require("tidyverse", quietly = TRUE)) install.packages("tidyverse")

# sudo apt-get install -y make libcurl4-openssl-dev libssl-dev libprotobuf-dev
# sudo apt-get install -y protobuf-compiler libgeos-dev libproj-dev libudunits2-dev libjq-dev
# sudo apt-get install -y libicu-dev libgdal-dev gdal-bin libv8-dev

if (!require("geographr", quietly = TRUE)) devtools::install_github("terminological/geographr")
if (!require("arear", quietly = TRUE)) devtools::install_github("terminological/arear")
if (!require("growthrates", quietly = TRUE)) devtools::install_github("bristol-vaccine-centre/growthrates")

library(tidyverse)
devtools::load_all("~/Git/interfacer")
devtools::load_all("~/Git/growthrates")

source("utils.R")

ggrrr::gg_pedantic(fontSize = 12)

cache = memoise::cache_filesystem("~/tmp/dashboard")
cached_csv = memoise::memoise(readr::read_csv, cache = cache)
cached_poisson_locfit_model = memoise::memoise(growthrates::poisson_locfit_model, cache = cache)
cached_proportion_locfit_model = memoise::memoise(growthrates::proportion_locfit_model, cache = cache)



## map shapes ----

lad20map = geographr::boundaries_ltla20 %>% 
  rename(areaCode = ltla20_code, areaName = ltla20_name) %>%
  lad_merge() %>% ungroup()

lad20cov = lad20map %>% 
  arear::popoutArea(popoutPosition = "NW",popoutScale = 4)

# ggplot(lad20cov)+geom_sf()

lad20cov_ew = lad20map %>% 
  filter(areaCode %>% stringr::str_starts("E|W")) %>%
  arear::popoutArea(popoutPosition = "NE",popoutScale = 3)

# ggplot(lad20cov_ew)+geom_sf()  


source("data.R")


source("timelines.R")

## Plot ----

source("plot.R")


## Run





# dates = as.Date("2021-07-15")+seq(-10,10,1)*7
# dates = seq(range(raw_cases$date)[1],range(raw_cases$date)[2],7)
# 
# for (date in dates) {
#   file = suppressWarnings(doplot(date, messages, overwrite=TRUE))
#   message(file)
# }

dates2 = seq(range(raw_cases$date)[1],range(raw_cases$date)[2],1)
for (date in dates2) {
  file = suppressMessages(suppressWarnings(doplot(date, messages, overwrite=FALSE)))
  message(file)
}