# #### the script assigns unassigned gdp to the closest population cell
# #case when only gdp to reassign (all population pixels already match with a gdp value)
# 
# #do per case
# # Level 1. GDP to assign but no more population (*)
# #assign gdp to closest population cell 
countries.only.gdp.to.assign.df <- first.join.5arcmin.filtered %>%
  filter(Country %in% setdiff(countries.unassigned.gdp,
                              countries.unassigned.both))

countries.loop.gdp.only <- unique(countries.only.gdp.to.assign.df$Country)

checksum.unassigned.gdp <- gdp.5arcmin.matching %>%
  filter(Country %in% countries.loop.gdp.only) %>%
  filter(!cell_ID %in% countries.only.gdp.to.assign.df$cell_ID)

sum(checksum.unassigned.gdp$GDP_total_2015)
sum(gdp.5arcmin.matching$GDP_total_2015)
sum(gdp.5arcmin.matching$GDP_total_2015) -
  sum(checksum.unassigned.gdp$GDP_total_2015)


#### final verdict : remove these pixels #### 
vroom_write(checksum.unassigned.gdp, 
            paste0(outputDirTemp, '01_removed_gdp_5arcmin.csv'), ',')
vroom_write(countries.only.gdp.to.assign.df, 
            paste0(outputDirTemp, '01_countries_matching_extra_gdp.csv'), ',')
