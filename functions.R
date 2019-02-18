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

modal_parts <- function(id, size = "large"){
  #id        = "siting_framework"
  
  title     = str_replace_all(id, "_", " ") %>% str_to_title()
  btn_title = glue("View {title}")
  path      = here(glue("docs/modal/{id}.md"))
  
  if (!file.exists(path)) stop(glue("{md}: {path} does not exist!"))
  
  list(id=id, title=title, btn_title=btn_title, path=path, size=size)
}

modal_id <- function(id){
  m <- modal_parts(id)
  bs_modal(id=m$id, title=m$title, size=m$size, body = includeMarkdown(m$path))
}

modal_btn <- function(id){
  m <- modal_parts(id)
  bs_button(m$btn_title, button_type = "primary") %>% bs_attach_modal(m$id)
}

icon <- function(icon){
  glue('<span class="fa-stack">
  <i class="fas fa-square fa-stack-2x"></i>
       <i class="fas fa-{icon} fa-stack-1x fa-inverse"></i>
       </span>')  
}
