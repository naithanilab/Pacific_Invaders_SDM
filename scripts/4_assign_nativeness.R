library(tidyverse)

rm(list = ls())
setwd("~/ownCloud/Projects/Berlin/10_Pacific_invaders")

########## Assign nativeness status from Micheals table ############
load("data/occ_gift_intersect.RData")
native_ref = read_delim("data/Pacific_Invaders_GIFT_22_01.csv", delim = ";") %>% filter_all(any_vars(!is.na(.)))

