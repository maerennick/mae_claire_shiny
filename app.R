

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
library(leaflet)
library(gghighlight)
library(yonder)
library(ggbeeswarm)
library(shinyWidgets)


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
                           navbarMenu("About",
                           tabPanel("The App",
                                        h1("California aquauclture through time"),
                                        h4("An evaluation of aquaculture development and status by production, species cultivated and the nutritional benefits of farmed seafood in the state of California"),
                                        br(),
                                        p("The purpose of this Shiny App is to explore aquaculture landings data from the California Department of Fish and Wildlife (1971-2018) to better understand how aquaculture practices have changed through time both spatially and and thorugh production. Additionally, this app is designed to survey consumption preferences to better understand local market demand, and relay nutritional information of preferred farmed species"
                                        ),
                                        br(),
                                    img(src = "aquaculture_2.jpg", height = 800, width = 500),
                                    br(),
                                    br(),
                                        h5("Background Information"),
                                        p(" Aquaculture is one of the fastest growing food sectors in the world (Cotrell 2019, Love 2020, FAO 2020). Sustainable aquaculture -- balancing economic, ecological and social objectives to reduce negative impacts on a system (FAO 2020, Boyd et al. 2020, World Commission on Environment and Development 1987) -- is seen as a key path in addressing future food and economic development goals in the ‘Blue Economy’ (FAO 2020, FAO 2019, Tigchelaar et al. 2021, Short et al. 2021, Naylor et al. 2021, Österblom et al. 2020, Costello 2020). The state of California hosts 20 (SD ± 2) (mean # state farms freshwater and marine = 47) operational marine farms for the past 22 years, with marine production legally restricted to bivalve mollusks and select seaweeds (USDA National Agricultural Statistics Service 2018, Fong et al. 2022). Shellfish operations occur primarily in estuarine and intertidal state waters, although some production also occurs in land-based facilities. Further, most shellfish culture operations have some land- based facilities that can be used for hatching, early rearing, and processing of shellfish."),
                                    br(),
                                    h3(HTML('<a href= "https://caseagrant.ucsd.edu/california-aquaculture" target="_blank">Click here to learn more</a>')),
                                    br(),
                                    br(),
                                    #end main
                                    ), # end sidebar

                           # tabPanel("The Taxa",
                           #          h3("Check out the most popular local invertebrate seafood in the Santa Barbara Channel!"),
                           #          sidebarLayout(
                           #            sidebarPanel("",
                           #                         actionLink("species1", "oyster"),
                           #                         br(),
                           #                         actionLink("species2", "clam"),
                           #                         br(),
                           #                         actionLink("species3", "mussel"),
                           #                         br(),
                           #                         actionLink("species4", "abalone"),
                           #
                           #            ),
                           #            mainPanel(
                           #              imageOutput("display"),
                           #              textOutput("information"),
                           #              uiOutput("link")
                           #            ))),

                           tabPanel("The Data",
                                    h2("The Data Used in this App"),
                                    h5("Farm Map Data:"),
                                    p("California Department of Fish and Wildlife Marine Resources Region. (2011). Aquaculture Leases: California, 2011. California Department of Fish and Wildlife. Marine Resources Region. Available at: http://purl.stanford.edu/zk621ch0195."),
                                    p("California Department of Fish and Wildlife GIS Maps & Data"),
                                    h5("Nutrition Data:"),
                                    p("U.S. Department of Agriculture, Agricultural Research Service. FoodData Central, 2019. fdc.nal.usda.gov."),
                                    h5("Farm Production Data:"),
                                    p("The aquaculture production data used in this study was collected from Figure 2.2 of the 2020 CDFW Report on the Status of Commercial Marine Aquaculture in California (CDFW 2020) using WebPlotDigitizer (a web-based tool to extract numerical data from plots, images, and maps). This data includes production (metric tonnes) of mussels, clams, abalone, and oysters (including: olympia oysters, European flat oysters, Eatern Oysters, Kumamoto Oysters, and Pacific Oysters) from 1971-2018.")

                           ),
                           tabPanel("The Author",
                                      h2("The Author"),
                                    br(),
                                        h3(HTML('<a href= "https://maerennick.github.io/" target="_blank">Mae Rennick</a>')),
                                    img(src = "mae.png", height = 800, width = 500),
                                        br(),
                                    br(),
                                    p("Mae Rennick is a second year PhD student at the University of Santa Barbara California in the department of Ecology, Evolution and Marine Biology. She is a member of the Froehlich Lab where she studies the role of aquaculture—or marine farming— in developing sustainable long-term food security systems and initiatives, and more broadly how to make these systems more equitable and accessible. Through her work, Mae continues to prioritize the involvement of underrepresented identities in science through activism, outreach and her roles in education, first as a science instructor at the Ocean Institute, later as a Kindergarten teacher at Hollister Elementary School, and in her current position as a graduate student.")

                                    )
                           ), #end tab


                           tabPanel("Farm Map",
                                    sidebarLayout(
                                      sidebarPanel(
                                        h6("Select a group below to view where it is farmed in the state of California"),
                                        pickerInput(inputId = "pick_species",
                                                     label = "Taxa:",
                                                     choices = unique(ca_aquaculture_sf$group),
                                                     selected = "abalone",
                                                    options = list(
                                                      `actions-box` = TRUE,
                                                      size = 10,
                                                      `selected-text-format` = "count > 3"
                                                    ),
                                                    multiple = TRUE
                                        ),
                                        img(src = "abalone.jpg", height = 800, width = 500),# end checkboxGroupInput
                                      ), #end sidebarPanel
                                        mainPanel(h1("FARM MAP"),
                                                  tmapOutput("cal_map"),
                                                  br(),
                                                  br(),
                                                  h4("Information on marine aquauclture farms in the state of California by district:"),
                                                  tableOutput("districtinfo")
                                                  #HTML('<img src="species_districts.JPEG"></center>')
                                                  )
                                    ) # end sidebarLayout
                           ), # end tab
                           tabPanel("Taxa Information",
                                    sidebarLayout(
                                        sidebarPanel(
                                          h6("Select a group below to view its production history and to learn more about how the group is farmed"),
                                            checkboxGroupInput(inputId = "species_info",
                                                               label = "Group:",
                                                               choices = unique(cal_data$group),
                                                               selected = "abalone"
                                            ),
                                            img(src = "oysterfarm.jpg", height = 800, width = 500)# end checkboxGI
                                        ), #end sidebarPanel
                                        mainPanel(h1("History of Farmed Species in California"),
                                                  plotOutput("cal_plot2"),
                                                  br(),
                                                  h4("Organism Information, Description and Farming Techniques"),
                                                  tableOutput("text"))
                                    ) # end sidebarLayout
                           ), # end tab
                           tabPanel("Seafood Consumption",
                                    sidebarLayout(
                                      sidebarPanel(h4("Seafood Preferences Poll"),
                                                   h6("Select your favorite seafood item below, fill-in how often you eat seafood and submit the poll to see the nutritional information of your selected item"),
                                                                      textInput("text0", "Name"),
                                                   textInput("text1", "State"),
                                                   br(),
                                                   # sliderTextInput(
                                                   #   inputId = "mySliderText",
                                                   #   label = "How often do you eat seafood?",
                                                   #   grid = TRUE,
                                                   #   force_edges = TRUE,
                                                   #   choices = c("never",
                                                   #               "rarely", "sometimes",
                                                   #               "frequently")
                                                   # ),
                                                   knobInput(
                                                     inputId = "myKnob",
                                                     label = "How many days a month do you eat seafood?",
                                                     value = 0,
                                                     min = 30,
                                                     displayPrevious = TRUE,
                                                     lineCap = "round",
                                                     fgColor = "#428BCA",
                                                     inputColor = "#428BCA"
                                                   ),

                                                   radioButtons("text2", "Choose Your Favorite Seafood Item",
                                                                      choices = unique(nutrition_data$group)),
                                                   actionButton("update", "submit")),
                                      mainPanel(h1("Seafood Consumption"),
                                                br(),
                                                img(src = "seafoodconsumption.jpg", height = 1000, width = 800),
                                                tableOutput("table"),
                                                br(),
                                                h1("Nutritional Information"),
                                                tableOutput("nutrition_kable"))
                                    ) # end sidebarLayout
                           ) # end tab
                ) #end navbar
)



##################################################################################################### Output

server <- function(input, output) {




### Opening Page


  # output$information <- renderText(
  #   {
  #
  #     values <- reactiveValues(species_1 = 0, species_2 = 0, species_3 = 0)
  #
  #     observeEvent(input$species1, {
  #       values$species_1 <- 1
  #       values$species_2 <- 0
  #       values$species_3 <- 0
  #
  #     })
  #
  #     observeEvent(input$species2, {
  #       values$species_1 <- 0
  #       values$species_2 <- 1
  #       values$species_3 <- 0
  #
  #     })
  #
  #     observeEvent(input$species3, {
  #       values$species_1 <- 0
  #       values$species_2 <- 0
  #       values$species_3 <- 1
  #
  #     })
  #
  #
  #     if(values$species_1)
  #       paste("Visit UC Davis' report of California oysters culture to learn about oyster aquaculture on the West Coast")
  #     else
  #       if(values$species_2)
  #         paste("CLAM")
  #     else
  #       if(values$species_3)
  #         paste("MUSSEL")
  #     else
  #       if(values$species_4)
  #         paste("ABALONE")
  #     else
  #       return(
  #         paste("SOMETHING SOMETHING")
  #       )
  #   })
  #
  #
  # # Define links
  #
  # url1 <- a("UC Davis: California Oyster Culture", href="https://marine-aquaculture.extension.org/wp-content/uploads/2019/05/California-Oyster-Culture.pdf")
  # url2 <- a("California Sea Grant: Seafood profile: Red Sea Urchin", href="https://caseagrant.ucsd.edu/seafood-profiles/red-sea-urchin")
  # url3 <- a("California Sea Grant: Seafood profile: Red Sea Urchin", href="https://caseagrant.ucsd.edu/seafood-profiles/red-sea-urchin")
  # url4 <- a("Commercial Fishermen of Santa Barbara", href="https://www.cfsb.info/species-seasons")
  # url5 <- a("Commercial Fishermen of Santa Barbara", href="https://www.cfsb.info/species-seasons")
  #
  # output$link <- renderUI({
  #   if(values$species_1)
  #     tagList("URL link:", url1)
  #   else
  #     if(values$species_2)
  #       tagList("URL link:", url2)
  #   else
  #     if(values$species_3)
  #       tagList("URL link:", url3)
  #   else
  #     if(values$species_4)
  #       tagList("URL link:", url4)
  #   else
  #     return(
  #       tagList("URL link:", url5)
  #     )
  # })
  #
  # #url <- a("California Sea Grant: California Seafood Profiles: California Spiny Lobster, href=https://caseagrant.ucsd.edu/seafood-profiles/california-spiny-lobster")


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

#tableStart <- data.frame('Name'= 'Mae', 'State' = 'California', 'Favorite Seafood Item' = 'oyster')
# newEntry <- reactive({
#   input$update
#   newLine <- isolate(c(input$text0, input$text1, input$text2))
# })

#consumption_table <- renderTable({rbind(tableStart, newEntry())})

#output$table<- consumption_table


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
    kable_styling(full_width = T)
}

output$nutrition_kable<- nutrition_table



##consumption plot

#output$consumption_plot <- renderPlot(
  #ggplot(data = table, aes(x = Favorite.Seafood.Item)) +
    #geom_bar(aes(color = state))+
    #theme_minimal()+
    #labs(x= "Seafood Item"))

### COnsumpiton Image

}


shinyApp(ui = ui, server = server)
