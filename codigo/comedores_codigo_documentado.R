#Comedores entregable 07/10/2024
#setwd("rasters/comedores/Documentacion/Documentacion/")
library(sf)
library(foreign)
library(readxl)
library(dplyr)
library(raster)
library(leafem)
#Base previo a indice
base_previo_indice=read_sf("output/base_previo_a_indice.shp")
#Cobertura de Comedores
cobertura_ageb_comedores=read_sf("output/cobertura_comedores.dbf")

base_previo_indice=merge(x=base_previo_indice,y=cobertura_ageb_comedores,by.x='cvegeo',by.y='CVEGEO',all.x=T)
base_previo_indice$prc_at_cmd=
  100*base_previo_indice$pob_atend/(base_previo_indice$pob_impact*(base_previo_indice$cr_p/100))
base_previo_indice$prc_at_cmd[base_previo_indice$prc_at_cmd|>is.infinite()]=0

base_indice=base_previo_indice|>dplyr::select(-pob_atend,-pob_impact)
base_indice$benef[base_indice$benef|>is.nan()]=0
base_indice$prc_at_cmd[base_indice$prc_at_cmd|>is.nan()]=0
base_indice$prc_at_cmd[base_indice$prc_at_cmd|>is.na()]=0
#pesos
municipios = sf::read_sf("../../Importantes_documentos_usar/Municipios/municipiosjair.shp")
#municipios=read_sf("../../../../../Reutilizables/Cartografia/municipiosjair.shp")
pesos=read_excel("output/ponderaciones.xlsx")
pesos$Rank=(max(pesos$Rank)+1-pesos$Rank)/sum(max(pesos$Rank)+1-pesos$Rank)

pie(pesos$Rank,labels = paste(pesos$Var, ": ",round(pesos$Rank*100,2),"%"))
#Nuevo índice
pesos_f=pesos$Rank
library(sf)
indice=function(transformacion,datos,pesos=pesos_f,interpretacion=c(rep(1,14),-1)){
  switch (substr(transformacion,1,1),
          "z" = {
            datos= scale(datos)
          },
          "l" = {
            datos=log(datos+1)
          },
          "m" = {
            datos[c(1,4:15)]=lapply(datos[c(1,4:15)],function(x){##Evitamos aplicarlo para las variables de accesibilidad.
              (x-median(x))/(IQR(x))
            })|>as.data.frame()
            
          },
          "r" = {
            datos=lapply(datos,function(x){
              rank(x)/length(x)
            })|>as.data.frame()
          },
  )
  return(as.matrix(datos)%*%(pesos*interpretacion))
}
his
indice(transformacion = "z",datos=base_indice[,3:17]|>st_drop_geometry())|>hist(main='Z-score',breaks=100)
indice(transformacion = "l",datos=base_indice[,3:17]|>st_drop_geometry())|>hist(main='Log')
indice(transformacion = "m",datos=base_indice[,3:17]|>st_drop_geometry())|>hist(main='Median/IQR')
indice(transformacion = "r",datos=base_indice[,3:17]|>st_drop_geometry())|>hist(main='Ranking')

ageb_con_pob=read_sf("input/Otros recursos/agebs_con_pobtot.dbf")

indices_pesos=base_indice|>
  cbind(z_score=indice(transformacion = "z",datos=base_indice[,3:17]|>st_drop_geometry()))|>
  # cbind(log=indice(transformacion = "l",datos=base_indice[,3:17]|>st_drop_geometry()))|>
  # cbind(median_IQR=indice(transformacion = "m",datos=base_indice[,3:17]|>st_drop_geometry()))|>
  # cbind(ranking=indice(transformacion = "r",datos=base_indice[,3:17]|>st_drop_geometry()))|>
  merge(y=ageb_con_pob|>dplyr::select(CVEGEO,POB1),by.x='cvegeo',by.y='CVEGEO')
indices_pesos$mun=iconv(indices_pesos$mun, to = "latin1", from = "UTF-8")
#Mapa web

comedores=read_sf("input/Comedores DIF/comedores_GWR.shp")

library(leaflet)
library(reshape2)
library(htmlwidgets)
library(leaflet.extras)
library(htmltools)

info.box <- HTML(paste0(
  HTML(
    '<div class="modal fade" id="infobox" role="dialog"><div class="modal-dialog"><!-- Modal content--><div class="modal-content"><div class="modal-header"><button type="button" class="close" data-dismiss="modal">&times;</button>'
  ),
  
  # Header / Title
  HTML(
    '</div><div class="modal-body">'
  ),
  
  # Body
  HTML('<h4>Información Adicional:</h4>
<p> Usando datos del  <a href="https://www.inegi.org.mx" target="_blank">INEGI</a> y del <a href="https://www.coneval.org.mx" target="_blank">CONEVAL</a> a nivel AGEB (Áreas Geoestadísticas Básicas) sobre población y carencias por acceso a la alimentación, además de información georreferenciada de comedores comunitarios, se estimó la cobertura y población impactada a nivel AGEB. Esto permitió identificar cuáles comedores realmente atienden a la población que les corresponde, cuáles incluso la superan y, del mismo modo, cuáles no alcanzan a cubrir a la población que deberían atender. </p>
<p> A partir de esa cobertura y de variables sociodemográficas adicionales (población de adultos mayores, personas con discapacidad, población infante) así como de variables geográficas y de accesibilidad (Accesibilidad a carretera pavimentada, índice de rezago social) se estandarizaron las variables mediante Z-scores y posteriormente se ponderaron para construir un ranking de prioridad alimentaria por AGEB. De esta manera nos permite identificar los AGEB con mayor prioridad de atención y su población objetivo.</p>'),
  
  # Closing divs
  HTML('</div><div class="modal-footer"><button type="button" class="btn btn-default" data-dismiss="modal">Cerrar</button></div></div>')
))


pal_z_score = colorNumeric(palette = c("gray","yellow","orange","red"), domain = indices_pesos$z_score)
pal_log = colorNumeric(palette = c("gray","gray","yellow", "yellow","orange","red", "red"), domain = indices_pesos$log)
pal_median_IQR = colorNumeric(palette = c("gray","gray","gray","yellow","yellow", "yellow","yellow","yellow","orange","orange","orange","red", "red"), domain = indices_pesos$median_IQR)
pal_ranking = colorNumeric(palette = c("gray","gray","yellow", "yellow","orange","red", "red"), domain = indices_pesos$ranking)

quantile(x = indices_pesos$z_score, c(0.1,0.35,0.7))

pal_z_score = colorQuantile(
  palette = c("gray", "yellow", "orange", "red"),
  domain  = indices_pesos$z_score,
  n = 4,
  na.color = "green"
)

mapa <- leaflet() |>
  # Agregar capa de fondo
  addProviderTiles("OpenStreetMap", options = providerTileOptions(opacity = 1),group = "100%") |>
  addProviderTiles("OpenStreetMap", options = providerTileOptions(opacity = 0.5),group = "50%") |>
  addProviderTiles("OpenStreetMap", options = providerTileOptions(opacity = 0.25),group = "25%") |>
  # Agregar los polígonos de A
  addPolygons(data = municipios, group = "municipios", fillColor = "grey", fillOpacity = 0.1, color = "white", stroke = 2, dashArray = "-",label = municipios$NOM_MUN) |> 
  # addPolygons(data = indices_pesos|>st_transform(crs(comedores))|>as("Spatial"),fillColor= 
  #               pal_log(indices_pesos$log)
  #             , color = pal_log(indices_pesos$log), fillOpacity = 0.6, 
  #                            "<br> Poblacion: ",indices_pesos$POB1,
  #                            "<br> Carencia Alimentaria: ",indices_pesos$cr_p|>round(2)," %",
  #                            "<br> Atencion por comedores: ",indices_pesos$prc_at_cmd|>round(2)," %",
  #                            "<br> Ageb de Prioridad: ",indices_pesos$priortr==1),
  #             
  #             group = "Log") |>
  # addPolygons(data = indices_pesos|>st_transform(crs(comedores))|>as("Spatial"),fillColor= 
  #               pal_median_IQR(indices_pesos$median_IQR)
  #             , color = pal_median_IQR(indices_pesos$median_IQR), fillOpacity = 0.6, 
  #             popup = ~paste("Municipio:", indices_pesos$mun,"<br> Indice : ",indices_pesos$median_IQR|>round(2),
  #                            "<br> Poblacion: ",indices_pesos$POB1,
  #                            "<br> Carencia Alimentaria: ",indices_pesos$cr_p|>round(2)," %",
  #                            "<br> Atencion por comedores: ",indices_pesos$prc_at_cmd|>round(2)," %",
  #                            "<br> Ageb de Prioridad: ",indices_pesos$priortr==1),
  #             group = "median_IQR") |>
  # addPolygons(data = indices_pesos|>st_transform(crs(comedores))|>as("Spatial"),fillColor= 
  #               pal_ranking(indices_pesos$ranking)
  #             , color = pal_ranking(indices_pesos$ranking), fillOpacity = 0.6, 
  #             popup = ~paste("Municipio:", indices_pesos$mun,
  #                            "<br> Indice : ",indices_pesos$ranking|>round(2),
  #                            "<br> Poblacion: ",indices_pesos$POB1,
  #                            "<br> Carencia Alimentaria: ",indices_pesos$cr_p|>round(2)," %",
  #                            "<br> Atencion por comedores: ",indices_pesos$prc_at_cmd|>round(2)," %",
  #                            "<br> Ageb de Prioridad: ",indices_pesos$priortr==1),
  #             group = "ranking") |>
  
  # Agregar los puntos que están fuera de los polígonos
  addCircles(data = comedores|>as("Spatial"), color = "blue", radius = 2e3, opacity = 0.1,fillOpacity = 0.1,
             popup = ~paste("Raciones:", round(comedores$RACIONES, 2),
                            "<br> Nombre: ",comedores$NOM_COMEDO,
                            "<br> Tipo: ",comedores$MOD
             ),group = "Comedores")|>
  addPolygons(data = indices_pesos|>st_transform(crs(comedores))|>as("Spatial"),
              label = ~paste(mun, "-", cvegeo),fillColor= 
                pal_z_score(indices_pesos$z_score)
              , color = pal_z_score(indices_pesos$z_score), fillOpacity = 0.6, 
              popup = ~paste0(
                "<b>Municipio:</b> ", indices_pesos$mun, "<br>",
                "<hr style='margin: 5px 0;'>", # A subtle horizontal line for separation
                "<b style='color:#E4572E;'>Índice de Prioridad: ", round(indices_pesos$z_score, 2), "</b><br>", # Highlighted
                "<b>Población Total:</b> ", format(indices_pesos$POB1, big.mark = ","), "<br>",
                "<b>Carencia Alimentaria:</b> ", round(indices_pesos$cr_p, 2), " %<br>",
                "<b>Atención por Comedores:</b> ", round(indices_pesos$prc_at_cmd, 2), " %<br>",
                "<b>AGEB en Zona Prioritaria:</b> ", ifelse(indices_pesos$priortr == 1, "Sí", "No")
              )
              ,
              group = "Índice (Ranking)") |>
  addLayersControl(
    baseGroups = c("100%","50%","25%"),
    overlayGroups = c("Índice (Ranking)",
                      #,"Log","median_IQR","ranking","Comedores"
                      "Comedores"),
    options = layersControlOptions(collapsed = FALSE)
  )|>
  
  addSearchFeatures(targetGroups = "municipios",
                    options = searchFeaturesOptions(
                      zoom = 12, 
                      openPopup = F,
                      firstTipSubmit =F,
                      hideMarkerOnCollapse =T))|>
  hideGroup(c( "Log","median_IQR","ranking"))|>
  addLegend(
    position = "bottomright",
    colors = c("#187EBA", "grey", "yellow", "orange", "red"), # 'transparent' for the circle icon
    labels = c("Cobertura Comedor Comunitario", "No Prioritario", "Prioridad Baja", "Prioridad Media", "Prioridad Alta"),
    title = "Nivel de Prioridad y Cobertura", # More descriptive title
    opacity = 1,
  ) |> 
  onRender(
    "function(el, x) {
      // Find the main legend container using its class name.
      // This returns an HTMLCollection, so we access the first element with [0].
      var legend = document.getElementsByClassName('info legend leaflet-control')[0];

      // console.log(legend) // Good for debugging!

      if (legend && legend.children[1]) { // Check if legend and its child exist
        // Access the specific child element that represents your first color swatch.
        // Based on your testing, this element is legend.children[1].
        var firstSwatch = legend.children[1];

        // Apply CSS to make it a circle
        firstSwatch.style.borderRadius = '50%'; // Makes it a perfect circle
        firstSwatch.style.width = '12px';      // Adjust size as desired
        firstSwatch.style.height = '12px';     // Keep width and height equal for a circle

        // --- NEW STYLES FOR BLUE FILL (0.3 opacity) AND VERY BLUE BORDER ---

        // Set the fill color with opacity using RGBA.
        // For a blue fill, choose your desired blue color (e.g., RGB values for #187EBA)
        // #187EBA is approximately RGB(24, 126, 186)
        firstSwatch.style.backgroundColor = 'rgba(24, 126, 186, 0.3)';

        // Set the border: width, style (solid), and a very blue color.
        // Using a darker or more vibrant blue for the border.
        firstSwatch.style.border = '2px solid #0056B3'; // A darker, strong blue
                                                        // You can adjust '2px' for thickness
      }
      
      
      var map = this;

      // 1) Insertar un mensaje encima del control de capas
      var layersControl = document.getElementsByClassName('leaflet-control-layers-list')[0];  // Devuelve un HTMLCollection con todos los nodos que tienen esa clase.
      var instrEmpty = document.createElement('div');   // Crea un <div> vacío en memoria.
      instrEmpty.style.height = '10px'; // Para dejar un margen visual.
      instrEmpty.innerHTML = ' ';  // Ponemos un espacio como contenido, para asegurar que el <div> no colapse totalmente.
      var instr = document.createElement('span'); // Línea que contendrá el mensaje.
      instr.style.fontSize = '12px';  
      instr.innerHTML = 'Transparencia del mapa';
      layersControl.insertBefore(instrEmpty, layersControl.firstChild);  // Inserta newNode justo antes de referenceNode.
      layersControl.insertBefore(instr, layersControl.firstChild);
      
    }"
  ) |> 
  addLogo(img ="img/fondo_transparent.png",  position ="bottomleft" , width = "350", height = "175", offset.y = "0") |> 
  addBootstrapDependency() %>% # Add Bootstrap to be able to use a modal
  addEasyButton(easyButton(
    icon = "fa-info-circle", title = "Map Information",
    onClick = JS("function(btn, map){ $('#infobox').modal('show'); }")
  )) %>% # Trigger the infobox
  htmlwidgets::appendContent(info.box)


mapa

### Falta la paleta de colores



library(htmlwidgets)
library(leafem)
#saveWidget(mapa, "output/mapas_web/leaflet_indices.html",selfcontained = T)
#install.packages("htmlwidgets")
htmlwidgets::saveWidget(widget = mapa, file = "tmp.html", selfcontained = TRUE) 
file.rename("tmp.html", "output/mapas_web/leaflet_indices.html")

