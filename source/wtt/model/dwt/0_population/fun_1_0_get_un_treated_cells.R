get_un_treated_cells <- function(idx){
  
  country.looping <- countries.loop[idx]
  
  replaceMessage(paste0('Country ', idx, '/', length(countries.loop),
                        ': ', country.looping))
  
  country.level.safe.water.data <- countries.water.access.matched %>% 
    filter(Country == country.looping)
  
  country.population.data.5arcmin <- pop.gdp.5arcmin %>% 
    filter(Country == country.looping)
  
  #### 2. split urban (128,639 cells) and rural (1,185,026 cells)
  # if urban > rural -> consider cell as urban (1,642 cells)
  # if rural > urban -> consider cell as rural (1,406 cells)
  
  country.urban <- country.population.data.5arcmin %>% 
    filter(pop_type_cell == "U" | pop_type_cell == "UR")
  
  country.rural <- country.population.data.5arcmin %>% 
    filter(pop_type_cell == "R" | pop_type_cell == "RU")
  
  # unique(country.rural$pop_type_cell)
  # unique(country.urban$pop_type_cell)  
  
  
  #### 3. order frames by assignment variable (for now density, but then GDP/capita)
  # prioritizing richer pixels in assuming that their are better connected to safely managed water
  country.urban.ordered <- country.urban %>% 
    arrange(desc(GDP_capita_2015))
  
  country.rural.ordered <- country.rural %>% 
    arrange(desc(GDP_capita_2015))
  
  
  #### 4. assign treatment starting from highest variable value until cap 
  #----urban
  df.selection.urban <- 1
  repeat{
    
    #get cap for assigned people 
    #basic = AQUASTAT, safe = WORLD BANK
    
    if(treatment.type == 'BASIC'){
      country.cap.urban <- country.level.safe.water.data$access_pop_urban_aquastat
    }
    else{country.cap.urban <- country.level.safe.water.data$access_pop_urban_wb}
    
    
    #select number of rows until cap is exceeded
    pixels.assigned.urban <- country.urban.ordered[1:df.selection.urban, ]
    
    # if total population assigned exceeds the cap, break the loop
    if(sum(pixels.assigned.urban$pop_2015_total) > country.cap.urban |
       df.selection.urban > nrow(country.urban.ordered)){break}
    
    else{df.selection.urban <- df.selection.urban + 1}
    
    
  }
  
  
  
  #----rural
  #initiate counter to select n rows 
  df.selection.rural <- 1
  
  repeat{
    
    #get cap for assigned people 
    
    if(treatment.type == 'BASIC'){
      country.cap.rural <- country.level.safe.water.data$access_pop_rural_aquastat
    }
    else{country.cap.rural <- country.level.safe.water.data$access_pop_rural_wb}
    
    
    #select number of rows until cap is exceeded
    pixels.assigned.rural <- country.rural.ordered[1:df.selection.rural, ]
    
    # if total population assigned exceeds the cap, break the loop
    if(sum(pixels.assigned.rural$pop_2015_total) > country.cap.rural |
       df.selection.rural > nrow(country.rural.ordered)){break}
    
    else{df.selection.rural <- df.selection.rural + 1}
    
    
  }
  
  
  #### final assigned pixels
  assigned.country.urban <- country.urban.ordered[1:df.selection.urban-1, ]
  assigned.country.rural <- country.rural.ordered[1:df.selection.rural-1, ]
  
  # #verify that sum of assigned is less than cap
  # sum(assigned.country.rural$pop_total) < country.cap.rural
  # sum(assigned.country.urban$pop_total) < country.cap.urban
  
  assigned.country.df <- rbind(assigned.country.urban, assigned.country.rural)
  
  unassigned.country.df <- setdiff(country.population.data.5arcmin, assigned.country.df)
  
  vroom_write(assigned.country.df, 
              paste0(outputDir, 'trtd_', treatment.type, '/trtd_pop_', country.looping, '.csv'), ',')
  vroom_write(unassigned.country.df, 
              paste0(outputDir, 'untrtd_', treatment.type, '/untrtd_pop_', country.looping, '.csv'), ',')
  
}