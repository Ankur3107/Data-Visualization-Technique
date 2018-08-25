library(dplyr)
library(highcharter)
library(countrycode)

worldMapVisualization <- function(df, x, title=NULL){
 
    Country <- enquo(x)

    #Load worldgeojson for word map plot
    data(worldgeojson, package = "highcharter")

    #Group By Country
    by_country <- df %>% select(!!Country) %>% filter(!is.na(!!Country)) %>%group_by(!!Country) %>% summarise(n1=n())
    code <- countrycode(by_country$Country, 'country.name', 'iso3c')
    by_country$iso3 <- code
    
    #Making highchart plot of world map
    p_by_country <- highchart() %>% 
                  hc_add_series_map(worldgeojson, by_country, value = "n1", joinBy = "iso3") %>% 
                  hc_colorAxis(stops = color_stops()) %>% 
                  hc_legend(enabled = TRUE) %>%  
                  hc_mapNavigation(enabled = TRUE) %>%
                  hc_title(text = title)  %>%
                  hc_tooltip(useHTML = TRUE, headerFormat = "",
                            pointFormat = "Country: {point.Country} Total: {point.n1}") %>%  hc_add_theme(hc_theme_google())
    
    p_by_country
}