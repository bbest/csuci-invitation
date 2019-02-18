library(leaflet)
library(htmltools)
library(markdown)

map_feature <- function(key){
  pt <- filter(pts, key == !!key)
  
  shiny::absolutePanel(
    leaflet(
      pt, 
      width=382, height=200, 
      options = leafletOptions(
        zoomControl = FALSE, 
        attributionControl = FALSE)) %>% 
      addProviderTiles(
        #providers$Stamen.Watercolor,
        providers$Stamen.TonerLite) %>% 
      #options = providerTileOptions(opacity=0.3)) %>% 
      addMarkers(
        label = ~ place) %>% 
      setView(pt$lon, pt$lat, zoom=5) %>% 
      addMiniMap(
        #tiles = providers$Stamen.Watercolor,
        tiles = providers$Stamen.TonerLite,
        zoomLevelFixed = 1,
        toggleDisplay = TRUE), bottom=0) 
}

map_all <- function(pts){
  
  md_to_html <- function(md){
    if(is.na(md)) md = "NA"
    HTML(markdownToHTML(text= md, fragment.only=T))
  }
  
  pts = pts %>% 
    mutate(
      content = map_chr(project_title, md_to_html))
  
  leaflet(pts) %>% 
    addProviderTiles(
      providers$Stamen.Watercolor,
      options = providerTileOptions(
        opacity=0.3)) %>% 
    addMarkers(popup = ~content) %>% 
    addPopups(popup = ~content)
}