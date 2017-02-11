#Place holder for project info and comments
#
#
#
#

#prepare environment
library(tidyverse)
library(lubridate)
library(xtable)

#load data from zip
activity_data <- readr::read_csv(file = "data/activity.zip", col_names = TRUE, col_types = readr::cols(readr::col_integer(),readr::col_date(format = "%Y-%m-%d"),readr::col_integer()))

