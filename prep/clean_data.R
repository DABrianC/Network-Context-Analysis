

#script to clean the raw data and format it for use
#with igraph, ggraph

#All data has been anonymized and locations have been changed
#to protect identities and locations. This involved some cleaning steps
#that are not shared here to correct misspellings in survey data

#runs the prep.R script so all functions, packages, and colors
#can be accessed
source(here::here("prep/prep.R"))

set.seed(1245)
###cleaning for descriptive stats-Mentors-----

#Read in network of trainees and mentors 

edges <- read_xlsx(here::here("data/edges_mentor.xlsx"))
  
nodes <- read_xlsx(here::here("data/nodes_mentor.xlsx"))  
  
#find the individual softwares in the software column


softwares <- paste(c("R", "Stata", "Excel", "Atlas.ti")
                   , collapse = "|")

#create a new column that extracts the names of software used 
#then create another column that unnests any lists
nodes2 <- nodes |>
  mutate(software = str_extract_all(software, pattern = softwares))


#make a graph object in order to understand the network
#characteristics
g_mentor <- graph_from_data_frame(edges
                                       , directed = FALSE
                                       , vertices = nodes2)

#Testing that everything looks correct
#Use the graph object to make a network diagram
ggraph(g_mentor, "with_kk") +
  geom_node_point()+
  geom_edge_link() +
  geom_node_text(aes(label = name))


###Add geocoordinates to for each city-------
#Port-au_prince, St. Marc, and Cap-Haitian are the three cities 
#where participants are based

#download Haiti geopackage and save as an object
hti <-st_read("https://geodata.ucdavis.edu/gadm/gadm4.1/gpkg/gadm41_HTI.gpkg")

#identify coordinates for each city
#PAP <- lat = 18.5333, lon = -72.3333
#STM <- lat = 19.1167, lon = -72.7000
#CAP <- lat = 19.7600, lon = -72.2000

#add city location data to nodes dataset
nodes3 <- nodes2 |>
  mutate(lat = case_when(str_detect(city, "PAP") ~ 18.5333
                              , str_detect(city, "STM") ~ 19.1167
                              , str_detect(city, "CAP") ~ 19.7600)
        , lon = case_when(str_detect(city, "PAP") ~ -72.3333
                          , str_detect(city, "STM") ~ -72.7000
                          , str_detect(city, "CAP") ~ -72.2000)
        , name = case_when(str_detect(city, "PAP") ~ "Port-au-Prince"
                           , str_detect(city, "STM") ~ "St. Marc"
                           , str_detect(city, "CAP") ~ "Cap Haitian"))

#create an sf object to use in a map
#use the lon and lat columns to create a new "geometry" column

#set the crs to 4326 to match the crs of the country object 
#can check this using st_crs(hti)
nodes_sf <- st_as_sf(nodes3
                     , coords = c("lon", "lat")
                     , crs = 4326) 


