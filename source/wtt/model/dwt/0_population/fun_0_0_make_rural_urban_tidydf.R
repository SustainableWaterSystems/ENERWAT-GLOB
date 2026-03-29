#script to make tidy dataframe of rural and urban access rates per country
rural.urban.raw <- aquastat.urban.rural.2015

countries.loop <- unique(countries.rural.urban$Country)
countries.loop.aquastat <- unique(rural.urban.raw$Country)
setdiff(countries.loop, countries.loop.aquastat)
setdiff(countries.loop.aquastat, countries.loop)

countries.loop.final <- intersect(countries.loop,
                                   countries.loop.aquastat)

countries.rural.urban.df.list <- list()


for(idx in seq(1, length(countries.loop.final))){
  
  country.data <- rural.urban.raw %>% 
    filter(Country == countries.loop.final[idx])
  
  total.value <- country.data$value[
    
    country.data$Variable == "Total population with access to safe drinking-water (JMP)"
    
  ]
  
  
  rural.value <- country.data$value[
    
    country.data$Variable == "Rural population with access to safe drinking-water (JMP)"
    
  ]
  
  urban.value <- country.data$value[
    
    country.data$Variable == "Urban population with access to safe drinking-water (JMP)"
    
  ]
  
  if(length(rural.value) == 0){rural.value <- urban.value}
  
  
  country.sel.df <- data.frame(
    countries.loop.final[idx],
    total.value,
    urban.value,
    rural.value
    
  )
  
  
  colnames(country.sel.df) <- c('Country','access.aquastat.total', 
                                'access.aquastat.urban','access.aquastat.rural')
  
  
  countries.rural.urban.df.list[[idx]] <-  country.sel.df
  
}


aquastat.rural.urban.df <- do.call(rbind, countries.rural.urban.df.list)
