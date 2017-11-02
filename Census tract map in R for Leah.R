
library(tidyverse)
library(tidycensus)
library(viridis)
library(forcats)
library(leaflet)
library(sf)
library(stringr)

#Use my census API key command below if you are using the Census API for the first time.
#census_api_key("019de273aa03a8e012e73e59e1da430aa4e7a224")

#To view the variables available in ACS 2015 (5-year pooled data ending in 2015)
v15 <- load_variables(2015, "acs5", cache = TRUE)

race_vars = c("B03002_003", "B03002_004", "B03002_006", "B03002_012")

dfTracts = get_acs(
  geography = "tract", state="AL", variables = race_vars, year = 2015, 
  output = "wide",geometry=TRUE
)

dfTracts = transmute(
  dfTracts, state = substr(GEOID, 1, 2), tract = GEOID, 
  name = NAME, white = B03002_003E, black = B03002_004E, asian = B03002_006E, 
  hispanic = B03002_012E)

dfTracts<-dfTracts %>% mutate(pct_blk=(black/(white+black+asian+hispanic))*100)
dfTracts<-dfTracts %>% mutate(pct_hisp=(hispanic/(white+black+asian+hispanic))*100)

tract_popup <- paste0(str_extract(dfTracts$name,"^([^,]*)"), "<br>Percent Latinx: ",round(dfTracts$pct_hisp,2),"%")
pal<-colorNumeric(palette="viridis",domain=dfTracts$pct_hisp)

tract_popup2 <- paste0(str_extract(dfTracts$name,"^([^,]*)"), "<br>Percent Black: ",round(dfTracts$pct_blk,2),"%")
pal2<-colorNumeric(palette="viridis",domain=dfTracts$pct_blk)


dfTracts %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%",height=750) %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = tract_popup,
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(pct_hisp)) %>%
  addLegend("bottomright", 
            pal = pal, 
            values = ~ pct_hisp,
            title = "Pct Latinx by Census Tract, Alabama",
            labFormat = labelFormat(suffix = "%"),
            opacity = 1)

#################################################################################################

#Add donors to the map as a point layer

MMW_ALABAMA_FY17_DONORS <- read_csv("P:/Development/DirectMarketing/Monica Whatley/Voting/Census tract data/MMW_ALABAMA_FY17_DONORS.csv")
Parsed with column specification:
  cols(
    NAME = col_character(),
    LOOKUPID = col_character(),
    LATITUDE = col_double(),
    LONGITUDE = col_double(),
    CITY = col_character(),
    COUNTY = col_character(),
    ZIP = col_character()
  )

MMW_ALABAMA_FY17_DONORS_cut<-MMW_ALABAMA_FY17_DONORS %>% filter(LONGITUDE>-90)

#remove a few people who had bad geocoded lat/long
ALABAMA_DONORS_SMALL<-MMW_ALABAMA_FY17_DONORS[sample(nrow(MMW_ALABAMA_FY17_DONORS),200),]

dfTracts %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%",height=750) %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = tract_popup2,
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal2(pct_blk)) %>%
  addLegend("bottomright", 
            pal = pal2, 
            values = ~ pct_blk,
            title = "Pct Black by Census Tract, Alabama",
            labFormat = labelFormat(suffix = "%"),
            opacity = 1) %>%
  addMarkers(data=ALABAMA_DONORS_SMALL)
