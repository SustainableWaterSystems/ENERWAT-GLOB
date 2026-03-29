rm(list=ls())

library(dplyr)
library(vroom)


inputDir <- '../../../../../output/wtt/model/dwt/0_population/2_output/'

#### AQUASTAT == basic water access

#### read files #### 
# read all files (treated / untreated)
# and merge into a single table

files.to.merge.treated <- list.files(
  paste0(inputDir, 'trtd_BASIC/'))
files.to.merge.untreated <- list.files(
  paste0(inputDir, 'untrtd_BASIC/'))


#### processing #### 
#### treated population pixels
treated.pop.list <- list()
for(idx in seq(1, length(files.to.merge.treated))){
  
  file.treated <- vroom(
    paste0(inputDir, 'trtd_BASIC/', files.to.merge.treated[idx]),
    show_col_types = F)
  
  treated.pop.list[[idx]] <- file.treated
  
}

treated.pop.df <- do.call(rbind, treated.pop.list)


#### untreated population pixels
untreated.pop.list <- list()
for(idx in seq(1, length(files.to.merge.untreated))){

  file.untreated <- vroom(
    paste0(inputDir, 'untrtd_BASIC/', files.to.merge.untreated[idx]),
    show_col_types = F)
  
  untreated.pop.list[[idx]] <- file.untreated


}

untreated.pop.df <- do.call(rbind, untreated.pop.list)

vroom_write(treated.pop.df, paste0(inputDir, 'treated_population_BASIC.csv'), ',')
vroom_write(untreated.pop.df, paste0(inputDir, 'untreated_population_BASIC.csv'), ',')





#### WORLD BANK #### AQUASTAT

#### read files #### 
# read all files (treated / untreated)
# and merge into a single table

files.to.merge.treated <- list.files(
  paste0(inputDir, 'trtd_SAFE/'))
files.to.merge.untreated <- list.files(
  paste0(inputDir, 'untrtd_SAFE/'))


#### processing #### 
#### treated population pixels
treated.pop.list <- list()
for(idx in seq(1, length(files.to.merge.treated))){
  
  file.treated <- vroom(
    paste0(inputDir, 'trtd_SAFE/', files.to.merge.treated[idx]),
    show_col_types = F)
  
  treated.pop.list[[idx]] <- file.treated
  
}

treated.pop.df <- do.call(rbind, treated.pop.list)


#### untreated population pixels
untreated.pop.list <- list()
for(idx in seq(1, length(files.to.merge.untreated))){
  
  file.untreated <- vroom(
    paste0(inputDir, 'untrtd_SAFE/', files.to.merge.untreated[idx]),
    show_col_types = F)
  
  untreated.pop.list[[idx]] <- file.untreated
  
  
}

untreated.pop.df <- do.call(rbind, untreated.pop.list)

vroom_write(treated.pop.df, paste0(inputDir, 'treated_population_SAFE.csv'), ',')
vroom_write(untreated.pop.df, paste0(inputDir, 'untreated_population_SAFE.csv'), ',')
