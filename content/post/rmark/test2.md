---
title: "Lynchings in the US, by Race, 1883-1941"
output: 
  flexdashboard::flex_dashboard:
    orientation: columns
    vertical_layout: fill
---

<style>
.colored {
  background-color: #e4e2d8;
}
</style>

```{r setup, include=FALSE}
#installed.packages("tigris")
library(tidyverse)
library(crosstalk)
library(flexdashboard)
library(plotly)
#library(summarywidget)
library(DT)
library(leaflet)
library(numform)
library(USAboundaries)
library(gridExtra)
library(ggmap)
library(DT)
library(knitr)
library(tigris)
library(devtools)
library(rappdirs)
library(leaflet)
library(htmlwidgets)
library(shiny)
library(sp)
library(rsconnect)
library(dplyr)
library(rgeos)
library(maptools)
```


Lynchings {data-icon="fa-map"}
===========================================================


```{r}

setwd('/Users/david/Dropbox/Lynchings_full/Socius Revisions/Supplementary Files/')
dot_data <- read.csv("/Users/david/Dropbox/Lynchings_full/Socius Revisions/make_dot_map_with_this_03.csv", colClasses='character', stringsAsFactors=F)
dot_data$verified <- as.numeric(dot_data$verified)
dot_data$verified <- replace_na(dot_data$verified)

dot_data$county[dot_data$caseid=="5710"] <- "NEW YORK"
dot_data$GEOID[dot_data$caseid=="5710"] <- "36061"

sl <- unique(dot_data$state_fips)

invisible(capture.output(us_county_shape <- counties(sl, cb = TRUE, year = NULL)))
cds <- coordinates(us_county_shape)
dot_coords <- cbind(as.data.frame(cds) %>% setNames(c("long","lat")),us_county_shape@data['GEOID'])
county_merge <- sp::merge(us_county_shape,dot_coords, by = "GEOID")
dot_merged <- sp::merge(county_merge, dot_data, by = "GEOID", duplicateGeoms = TRUE)

invisible(capture.output(us_county_small <- us_county_shape))
invisible(capture.output(us_county_small@data <- us_county_small@data[1]))

color_race <- c("White" = "#1e3f9d", "Black" = "#D4820F", "Chinese" = "#8d371b", "Japanese" = "#5134a6", "Mexican" = "#146c2a", "American Indian" = "#1e9d7e", "Other" = "#41330c")

lynchings <- dplyr::filter(dot_merged@data, verified==1)
lynchings$colors <- color_race[lynchings$race_count]
lynchings$long_jit <- jitter(lynchings$long, factor = 80)
lynchings$lat_jit <- jitter(lynchings$lat, factor = 80)
lynchings$year <- as.Date(paste(lynchings$year, 1, 1, sep = "-")) # beginning of year

lynchings$gender[lynchings$gender=="Male (?)"] <- "Male"

columns <- c("GEOID", "year", "county", "state", "victim_name", "gender", "race_count",
         "alleged_offense", "lynch_method", "vic_detes", "colors", "long_jit", "lat_jit")

lynching <- lynchings[columns]

lynch_share <- SharedData$new(lynching)

popup <- paste0("<B>County: ",lynch_share$data()$county, " - </B>", lynch_share$data()$vic_detes)
share_dot_map <- lynch_share %>%
  leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  
  addPolylines(data =us_county_shape,                  
               stroke = TRUE,
               weight = .5,
               color="grey") %>%
  
  addCircleMarkers(lng = lynch_share$data()$long_jit, lat = lynch_share$data()$lat_jit, weight=7, opacity = .1,     radius =1.5,color=lynch_share$data()$colors, stroke = TRUE, fillOpacity = .6, popup = popup) %>%

addLegend( 
  colors =  color_race,
  opacity=.5,
  labels = names(color_race),
  position = "bottomright", 
  title = "Race of Victim") 

centered_dot <- setView(share_dot_map, lat = 39, lng = -97, zoom = 5)
#centered_dot

lynch_table <- datatable(lynch_share, extensions="Scroller", style="bootstrap", class="compact", width="100%", options=list(deferRender=TRUE, scrollY=300, scroller=TRUE))
```


Sidebar2 {.sidebar}
-----------------------------------------------------------------------

```{r}
filter_slider("year", "Year", lynch_share, column=~`year`, step=1, timeFormat = "%Y")
filter_checkbox("race_count", "Race of Victim", lynch_share, ~`race_count`, inline = FALSE)
filter_checkbox("gender", "Gender of Victim", lynch_share, ~`gender`, inline = TRUE)
filter_checkbox("state", "State", lynch_share, ~`state`, inline = FALSE, columns = 4)
```


Column
-----------------

###

```{r}
centered_dot
```

> Data Source:  [Lynching Map Dataset](https://s3.amazonaws.com/davidrigbysociology/make_dot_map_with_this_02.csv)

