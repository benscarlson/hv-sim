
#Make sure to include carriage return after last line
#This script is called automatically from .Rprofile

message('Loading tidyverse...')
library(tidyverse)

message('Loading glue...')
library(glue)

message('Aliasing filter <- dplyr::filter')
filter <- dplyr::filter

message('Aliasing select <- dplyr::select')
select <- dplyr::select
