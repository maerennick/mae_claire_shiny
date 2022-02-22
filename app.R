

library(shiny)
library(tidyverse)
library(bslib)
library(shinythemes)
library(here)
library(readxl)
library(janitor)
library(kableExtra)
library(sf)
library(tmap)
library(gstat)
library(stars)
library(viridis)
library(png)


##################################################################################################### Read In Data


### Map Data

ca_counties_sf <- read_sf(here("ca_counties"), layer = "CA_Counties_TIGER2016") %>%
  janitor::clean_names() %>%
  select(name)

# Check the projection
st_crs(ca_counties_sf)

#ca_counties_sf <- read_sf(here("ca_shp"), layer = "CA_cst12nm") %>%
#janitor::clean_names() %>%
#select(name)

# Check the projection
#st_crs(ca_counties_sf)


# Read in the farm data:
ca_aquaculture_sf_1 <- read_sf(here("shp_data"), layer = "MAN_CA_Aquaculture") %>%
  janitor::clean_names() %>%
  separate(species, c("species1", "Species2", "Species3", "Species4", "Species5", "Species6"), sep = ",", remove = TRUE) %>%
  gather(species_number, species, species1:Species6, factor_key=TRUE) %>%
  #drop_na() %>%
  select(-species_number) %>%
  mutate(group = case_when(species== "Pacific oyster" ~ "oyster",
                           species== "and Bay mussel" ~ "mussel",
                           species== "and European flat oyster" ~ "oyster",
                           species== "and Innkeeper worms" ~ "other",
                           species== "and Red abalone" ~ "abalone",
                           species== "bay mussel" ~ "mussel",
                           species== "Bay mussels" ~ "mussel",
                           species== "Gelidium spp." ~ "other",
                           species== "Rock scallops" ~ "other",
                           species== "Manila clam" ~ "clam",
                           species== " Manila clams" ~ "clam",
                           species== " Sea mussel" ~ "mussel",
                           species== "NA" ~ "NA",
                           species== " Macrocystis pyrifera" ~ "other",
                           species== " Speckled scallops" ~ "other",
                           species== " Suminoe oyster" ~ "oyster",
                           species== " Eastern oyster" ~ "oyster",
                           species== " European flat oyster" ~ "oyster",
                           species== " Eastern flat oyster" ~ "other",
                           species== " Rock scallops" ~ "other",
                           species== " Manila clam" ~ "clam",
                           species== " Blue mussel" ~ "mussel",
                           species== " Northern Quahog clam" ~ "clam",
                           species== " and European flat oyster" ~ "oyster",
                           species== " Pacific oyster" ~ "oyster",
                           species== " M. angustifolia" ~ "other",
                           species== " Japanese bay scallop" ~ "other",
                           species== " and Bay mussel" ~ "mussel",
                           species== " European oyster" ~ "oyster",
                           species== " bay mussel" ~ "mussel",
                           species== " Pacific giant oyster" ~ "oyster",
                           species== " Bay mussels" ~ "mussel",
                           species== " Mussels" ~ "mussel",
                           species== " Rock scallop" ~ "other",
                           species== " M. intergrifolia" ~ "other",
                           species== " Flat oyster" ~ "oyster",
                           species== " Olympia oyster" ~ "oyster",
                           species== " Ghost shrimp" ~ "other",
                           species== " Pelagophycus spp.  Euchema uncinatum (male plants only)" ~ "other",
                           species== " Kumamoto oyster" ~ "oyster",
                           species== " Native oyster" ~ "oyster",
                           species== " Japanese littleneck clams" ~ "clam",
                           species== " and Innkeeper worms" ~ "other",
                           species== " Gooseneck barnacle" ~ "other",
                           species== " and Red abalone" ~ "abalone",
                           species== " Mussel" ~ "mussel",
                           species== " Native littleneck clams" ~ "clam",
                           species== "Bay mussel" ~ "clam",
                           species== "Red abalone" ~ "abalone"))


ca_aquaculture_sf <- ca_aquaculture_sf_1 %>%
  group_by(parcel, group) %>%
  summarise() %>%
  filter(group == "oyster" | group == "clam" | group == "abalone" | group == "mussel")


# Check the projection
st_crs(ca_aquaculture_sf)


### distirict info data

district_info_data <- read_csv(here("district_info.csv"))

## Aquaculture Production Data
cal_data <- read_csv(here("aquaculture_data.csv")) %>%
    drop_na() %>%
    clean_names() %>%
    pivot_longer(!year, names_to = "species", values_to = "landings") %>%
  mutate(group = case_when(species== "pacific_oyster" ~ "oyster",
                           species== "kumamoto_oyster" ~ "oyster",
                           species== "eastern_oyster" ~ "oyster",
                           species==  "european_flat_oysters"~ "oyster",
                           species==  "olympia_oysters"~ "oyster",
                           species==  "clams"~ "clam",
                           species==  "mussels"~ "mussel",
                           species==  "abalone"~ "abalone")) %>%
  group_by(group, year) %>%
  summarise(landings= sum(landings))

## Species Description Data
text_data<- read_csv(here("species_info.csv")) %>%
  select(group, species) %>%
  rename("Description" =
           "species")


### Nutrition Data
nutrition_data <- read_csv(here("nutrition_100gportion.csv")) %>%
  mutate(value= paste(amount, unit)) %>%
  select(-amount, -unit) %>%
  spread(name, value) %>%
  select("Calcium (Ca)", "Carbohydrate", "Cholesterol", "Energy", "Fatty acids, total monounsaturated", "Fatty acids, total polyunsaturated", "Fatty acids, total saturated", "Iron (Fe)", "Magnesium (Mg)", "Phosphorus (P)", "Potassium (K)", "Protein", "Retinol", "Riboflavin", "Selenium (Se)") %>%
  mutate(group = case_when( Carbohydrate == "12 g" ~ "abalone",
                            Carbohydrate == "3.57 g" ~ "clam",
                            Carbohydrate == "3.69 g" ~ "mussel",
                            Carbohydrate == "2.72 g" ~ "oyster")) %>%
  gather(name, value, "Calcium (Ca)":"Selenium (Se)", factor_key=TRUE)


##################################################################################################### App Setup

# custom theme
shiny_theme <- bs_theme(bootswatch = "yeti")

##################################################################################################### Input Setup

ui <- fluidPage(theme = shiny_theme,
                navbarPage("California Aquaculture",
                           tabPanel("Home",
                                    sidebarLayout(
                                        sidebarPanel(),
                                        mainPanel(
                                            h1("California Aquauclture through time"),
                                            h5("An evaluation of development by production through time, species cultivated and the scape of current leasing processes"),
                                            br(),
                                            h3("Overview"),
                                            p("The purpose of this Shiny App is to explore aquaculture landings data from the California Department of Fish and Wildlife (1971-2018) to better understand how aquaculture practices have changed through time."
                                            ),
                                            br(),
                                            h3("Background Information"),
                                            p(" Aquaculture is one of the fastest growing food sectors in the world (Cotrell 2019, Love 2020, FAO 2020). Sustainable aquaculture -- balancing economic, ecological and social objectives to reduce negative impacts on a system (FAO 2020, Boyd et al. 2020, World Commission on Environment and Development 1987) -- is seen as a key path in addressing future food and economic development goals in the ‘Blue Economy’ (FAO 2020, FAO 2019, Tigchelaar et al. 2021, Short et al. 2021, Naylor et al. 2021, Österblom et al. 2020, Costello 2020). The state of California hosts 20 (SD ± 2) (mean # state farms freshwater and marine = 47) operational marine farms for the past 22 years, with marine production legally restricted to bivalve mollusks and select seaweeds (USDA National Agricultural Statistics Service 2018, Fong et al. 2022). Shellfish operations occur primarily in estuarine and intertidal state waters, although some production also occurs in land-based facilities. Further, most shellfish culture operations have some land- based facilities that can be used for hatching, early rearing, and processing of shellfish."),
                                            #br(),
                                            #HTML('<center><img src = "aquaculture.jpg"></center>'),
                                            br(),
                                            h3("Citations:"),
                                            h5("Farm Map Data:"),
                                            p("California Department of Fish and Wildlife Marine Resources Region. (2011). Aquaculture Leases: California, 2011. California Department of Fish and Wildlife. Marine Resources Region. Available at: http://purl.stanford.edu/zk621ch0195."),
                                            p("California Department of Fish and Wildlife GIS Maps & Data"),
                                            h5("Nutrition Data:"),
                                            p("U.S. Department of Agriculture, Agricultural Research Service. FoodData Central, 2019. fdc.nal.usda.gov."),
                                            h5("Farm Production Data:"),
                                            p("The aquaculture production data used in this study was collected from Figure 2.2 of the 2020 CDFW Report on the Status of Commercial Marine Aquaculture in California (CDFW 2020) using WebPlotDigitizer (a web-based tool to extract numerical data from plots, images, and maps). This data includes production (metric tonnes) of mussels, clams, abalone, and oysters (including: olympia oysters, European flat oysters, Eatern Oysters, Kumamoto Oysters, and Pacific Oysters) from 1971-2018.")

                                        ) #end main
                                    ) # end sidebar
                           ), #end tab
                           tabPanel("Farm Map",
                                    sidebarLayout(
                                        sidebarPanel(
                                          radioButtons(inputId = "pick_species",
                                                               label = "Species:",
                                                               choices = unique(ca_aquaculture_sf$group),
                                                       selected = "oyster"
                                            )# end checkboxGroupInput
                                        ), #end sidebarPanel
                                        mainPanel("FARM MAP",
                                                  tmapOutput("cal_map"),
                                                  br(),
                                                  tableOutput("districtinfo")
                                                  #HTML('<img src="species_districts.JPEG"></center>')
                                                  )
                                    ) # end sidebarLayout
                           ), # end tab
                           tabPanel("Species Information",
                                    sidebarLayout(
                                        sidebarPanel(
                                            checkboxGroupInput(inputId = "species_info",
                                                               label = "Group:",
                                                               choices = unique(cal_data$group),
                                                               selected = "abalone"
                                            )# end checkboxGI
                                        ), #end sidebarPanel
                                        mainPanel(h4("History of Farmed Species in California"),
                                                  plotOutput("cal_plot2"),
                                                  br(),
                                                  h4("Organism Information, Description and Farming Techniques"),
                                                  tableOutput("text"))
                                    ) # end sidebarLayout
                           ), # end tab
                           tabPanel("Seafood Consumption",
                                    sidebarLayout(
                                      sidebarPanel(h4("Seafood Preferences Poll"),
                                                                      textInput("text0", "Name"),
                                                   textInput("text1", "State"),
                                                   checkboxGroupInput("text2", "Choose Your Favorite Seafood Item",
                                                                      choices = unique(nutrition_data$group)),
                                                   actionButton("update", "Update Table")),
                                      mainPanel("Consumption Preferences",
                                                plotOutput("consumption_plot"),
                                                tableOutput("table"),
                                                tableOutput("nutrition_kable"))
                                    ) # end sidebarLayout
                           ) # end tab
                ) #end navbar
)


##################################################################################################### Output

server <- function(input, output) {


# Build Map

  cal_reactive_map <- reactive({
    ca_aquaculture_sf %>%
      filter(group %in% input$pick_species)
  })

 output$cal_map <- renderTmap({ tmap_mode("view")

  tm_shape(cal_reactive_map()) +
    tm_dots("group", palette = 'viridis')+
    tm_layout(legend.position = c("RIGHT","TOP"),
              legend.frame = TRUE)

  })


## Table for district info

 output$districtinfo<- function() {
   district_info_data %>%
     knitr::kable("html") %>%
     kable_styling("striped", full_width = F)
 }

   #renderTable({kable(district_info_data, caption = "The status of Commercial marine Aquauclture in California CDFW 2020") %>%
   #kable_styling(font_size = 8, full_width = T)})



# output text for tab 3- species descriptions
  cal_reactive_text <- reactive({
    text_data %>%
      filter(group %in% input$species_info)
  })


  output$text <- renderTable({cal_reactive_text()})


# Production  plot for tab 3
cal_reactive2 <- reactive({
  cal_data %>%
    filter(group %in% input$species_info) %>%
    drop_na()
}) # end output$cal_plot 2


output$cal_plot2 <- renderPlot(
  ggplot(data = cal_reactive2(), aes(x = year, y = landings)) +
    geom_point(aes(color = group))+
    theme_minimal()+
    labs(x= "Year",
         y= "Landings (lbs)")


) # end output$cal_plot1


##consuption Table

tableStart <- data.frame('Name'= 'Mae', 'State' = 'California', 'Favorite Seafood Item' = 'oyster')
newEntry <- reactive({
  input$update
  newLine <- isolate(c(input$text0, input$text1, input$text2))
})

consumption_table <- renderTable({rbind(tableStart, newEntry())})

output$table<- consumption_table


## Nutritional Information Table

#nutrition_table <- reactive({
 # input$update
  #newtable<- isolate(c(input$text2))
#})


nutrition_table <- function() {
  req(input$update)
  nutrition_data %>%
    #dplyr::select(group, everything()) %>%
    dplyr::filter(group == input$text2) %>%
    knitr::kable("html") %>%
    kable_styling(full_width = F)
}

output$nutrition_kable<- nutrition_table


##consumption plot

#output$consumption_plot <- renderPlot(
  #ggplot(data = table, aes(x = Favorite.Seafood.Item)) +
    #geom_bar(aes(color = state))+
    #theme_minimal()+
    #labs(x= "Seafood Item"))



}


shinyApp(ui = ui, server = server)
