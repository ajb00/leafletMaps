library(raster)
library(sp)
library(leaflet)
library(rgdal)

## prepare y county shapefile
shap_ky <- shapefile('dat/County_Polygon_1z.shp')
shap_ky_WGS84 <- spTransform(shap_ky, CRS('+proj=longlat +ellps=WGS84 +datum=WGS84'))


# leaftlet cases map -------------------------------------------------------------------

## map cases - step 1
shap_ky_cases <- sp::merge(shap_ky_WGS84, johns_cases_ky_map,
                           by.x = 'FIPS_ID', by.y = 'FIPS')

## map cases - step 2
qpal_cases <- colorQuantile(colorRamp(c("#A0B6EA", "#152A5B"), interpolate = "spline"),
                            unique(shap_ky_cases$cases_cum),
                            ## unik error handling
                            reverse = F)
# bins = c(0,1,5,30,50, 100, 500, 1000, 2000, 5000))
labz_cases <- sprintf("%s",paste0("County: <b>", shap_ky_cases$NAME2, '</b><br>', 
              'Cases: <b>', format(shap_ky_cases$cases_cum, big.mark = ","),"</b>",
              "<br>Rate: <b>", format(shap_ky_cases$cases_county_rate, trim=F), "</b> per 10K",
              "<br>Rank: <b>", shap_ky_cases$cases_rank,"</b> of 120",
              "<br>Statewide Rate: <b>", format(shap_ky_cases$cases_statewide_rate, trim=F),"</b> per 10K",
              "<hr>",
              "Pop: <b>", format(shap_ky_cases$pop2018, big.mark = ","), "</b>")) %>% 
  lapply(htmltools::HTML)

## map cases - step 3
leaflet(shap_ky_cases) %>%
  addPolygons(color = '#004DFF',
              weight = 1.1,
              fillColor = ~qpal_cases(cases_cum),
              fillOpacity = 1.5,
              label = labz_cases,
              labelOptions = labelOptions(textsize = "16px")) %>%
  addProviderTiles('Stamen.TonerHybrid') %>%
  addLegend(title = 'KY COVID-19 Cases Heat Map',
            position = 'topright',
            values = ~cases_cum,
            pal = qpal_cases,
            opacity = 1,
            labFormat = function(type, cuts, p) {  
              paste0(c('Less', '', '', 'More')) }
            ) %>%
  setView(lng = -85.711244, lat = 37.735969, zoom = 6)


# leaflet deaths map ------------------------------------------------------------------

## map deaths - step 1
shap_ky_deaths <- sp::merge(shap_ky_WGS84, johns_deaths_ky_map,
                            by.x = 'FIPS_ID', by.y = 'FIPS')

## map deaths - step 2
qpal_deaths <- colorBin(colorRamp(c("#EAA0A0", "#5B1515"), interpolate = "spline"),
                             unique(shap_ky_deaths$deaths_cum),
                             bins=c(0,5,15,50,500),
                             reverse = F)
labz_deaths <- sprintf("%s",paste0("County: <b>", shap_ky_deaths$NAME2, '</b><br>', 
                                   'Deaths: <b>', format(shap_ky_deaths$deaths_cum, big.mark = ","),"</b>",
                                   "<br>Rate: <b>", format(shap_ky_deaths$deaths_county_rate, trim=F), "</b> per 10K",
                                   "<br>Rank: <b>", shap_ky_deaths$deaths_rank,"</b> of 120",
                                   "<br>Statewide Rate: <b>", format(shap_ky_deaths$deaths_statewide_rate, trim=F),"</b> per 10K",
                                   "<hr>",
                                   "Pop: <b>", format(shap_ky_deaths$pop2018, big.mark = ","), "</b>")) %>% 
  lapply(htmltools::HTML)

## map deaths - step 3
leaflet(shap_ky_deaths) %>%
  addPolygons(color = '#FF0000',
              weight = 1.1,
              fillColor = ~qpal_deaths(deaths_cum),
              fillOpacity = 1.5,
              label = labz_deaths,
              labelOptions = labelOptions(textsize = "16px")) %>%
  addProviderTiles('Stamen.TonerHybrid') %>%
  addLegend(title = 'KY COVID-19 Deaths Heat Map',
            position = 'topright',
            values = ~deaths_cum,
            pal = qpal_deaths,
            opacity = 1,
            labFormat = function(type, cuts, p) {  
              paste0(c('Less', ' ', ' ', 'More')) }
  ) %>%
  setView(lng = -85.711244, lat = 37.735969, zoom = 6)
