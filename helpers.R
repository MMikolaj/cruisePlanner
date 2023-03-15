
library(leaflet)
library(dplyr)
library(googlesheets4)
library(ggplot2)


gs4_deauth()

address <-  "https://docs.google.com/spreadsheets/d/1Sqxe8bkgopHXzCKf3Dcl1hZ7V1-DEHxBjd66velCbqY/edit?usp=sharing"

data <- read_sheet(address, sheet=1, skip = 0, na = c("NA", "missing", ""),
                   col_types = "ccccccccccc-c") 

data <- data %>% 
  filter(!is.na(`Latitude [decimal]`)) %>% 
  mutate(lng=clean_data(`Longitude [decimal]`),
         lat=clean_data(`Latitude [decimal]`),
         `Time on station [h]`=as.numeric(`Time on station [h]`),
         `Time after station [h]`=as.numeric(`Time after station [h]`)
  )


data$`Time on station [h]`

data %>% 
  group_by(`Departmenet/Lab`, `fjord/area`) %>% 
  summarise(`Time on station [h]`=sum(`Time on station [h]`, na.rm = T),
            `Time after station [h]`=sum(`Time after station [h]`, na.rm = T)) %>% 
  
  pivot_longer(cols = `Time on station [h]`:`Time after station [h]`, names_to = "Time_variable", values_to = "Time") %>% 
  
  ggplot(aes(x=`Departmenet/Lab`, y=Time_variable, size=Time, label=Time)) +
  geom_point(color="tomato")+
  geom_text(data=. %>% filter(Time>0), size=4)+
  coord_flip()+
  facet_wrap(~`fjord/area`)+
  theme_minimal()
  
  
  

data %>% 
  distinct(Gear) %>% 
  mutate(Gear=strsplit(Gear, split=";")) %>% 
  unnest(Gear) %>% 
  mutate(Gear=gsub("^\\s|\\s$", "", Gear)) %>% 
  distinct() %>% 
  arrange(Gear)


require(leaflet)

pal <- colorFactor(palette = "Spectral", 
            domain = data$`Departmenet/Lab`)

leaflet(data) %>% 
  addTiles() %>% 
  addCircleMarkers(lng=~lng, lat=~lat, stroke = F, opacity = 0.8, color = ~pal(`Departmenet/Lab`),
                   popup = ~`Description (filled automatically)`, 
                   label = ~`Description (filled automatically)`) %>% 
  addLegend("bottomright", pal = pal, values = ~`Departmenet/Lab`, opacity = 1
  )




people <- unique(data$`person responsible`)

extract_names <- function(x) {
  
  x <- strsplit(x, "/")
  x <- unlist(x)
  x <- gsub("^\\s|\\s$", "", x)
  x <- unique(x)
  x <- sort(x)
  
  return(x)
}

people <- extract_names(data$`person responsible`)

grepl(paste(NULL, collapse = "|"), data$`person responsible`, )

data[grepl(paste(people[], collapse = "|"), data$`person responsible`), ]

subset(data, subset= grepl(paste(people, collapse = "|"), `person responsible`))

data[0,]