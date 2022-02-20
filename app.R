

library(shiny)
library(tidyverse)
library(bslib)
library(shinythemes)
library(here)
library(readxl)
library(janitor)


# read in data
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
                           species==  "abalone"~ "abalone"))

text_data<- read_csv(here("species_info.csv"))

# custom theme
shiny_theme <- bs_theme(bootswatch = "yeti")

# start shiny app
ui <- fluidPage(theme = shiny_theme,
                navbarPage("California Aquaculture",
                           tabPanel("Home",
                                    sidebarLayout(
                                        sidebarPanel(),
                                        mainPanel(
                                            h1("California Aquauclture through time"),
                                            h3("An evaluation of development by production through time, species cultivated and the scape of current leasing processes"),
                                            br(),
                                            h3("Overview"),
                                            p("The purpose of this Shiny App is to explore aquaculture landings data from the California Department of Fish and Wildlife (1971-2018) to better understand how aquaculture practices have changed through time."
                                            ),
                                            br(),
                                            h3("Background Information"),
                                            p(" Aquaculture is one of the fastest growing food sectors in the world (Cotrell 2019, Love 2020, FAO 2020). Sustainable aquaculture -- balancing economic, ecological and social objectives to reduce negative impacts on a system (FAO 2020, Boyd et al. 2020, World Commission on Environment and Development 1987) -- is seen as a key path in addressing future food and economic development goals in the ‘Blue Economy’ (FAO 2020, FAO 2019, Tigchelaar et al. 2021, Short et al. 2021, Naylor et al. 2021, Österblom et al. 2020, Costello 2020). The state of California hosts 20 (SD ± 2) (mean # state farms freshwater and marine = 47) operational marine farms for the past 22 years, with marine production legally restricted to bivalve mollusks and select seaweeds (USDA National Agricultural Statistics Service 2018, Fong et al. 2022). Shellfish operations occur primarily in estuarine and intertidal state waters, although some production also occurs in land-based facilities. Further, most shellfish culture operations have some land- based facilities that can be used for hatching, early rearing, and processing of shellfish."),
                                            br(),
                                            imageOutput("aquaculture"),
                                            br(),
                                            h3("Citations:")
                                        ), #end main
                                    ) # end sidebar
                           ), #end tab
                           tabPanel("Farm Map",
                                    sidebarLayout(
                                        sidebarPanel(
                                            checkboxGroupInput(inputId = "pick_species",
                                                               label = "Species:",
                                                               choices = list("oysters" = 1, "abalone" = 2, "clams" = 3, "mussels" = 4)
                                            ),
                                            dateInput("date3", "Date:", value = "2012-02-29", format = "mm/dd/yy")# end checkboxGroupInput
                                        ), #end sidebarPanel
                                        mainPanel("FARM MAP",
                                                  plotOutput("cal_plot1"))
                                    ) # end sidebarLayout
                           ), # end tab
                           tabPanel("Species Information",
                                    sidebarLayout(
                                        sidebarPanel(
                                            checkboxGroupInput(inputId = "species_info",
                                                               label = "PIER maps:",
                                                               choices = unique(cal_data$group)
                                            )# end checkboxGI
                                        ), #end sidebarPanel
                                        mainPanel("Production by Species",
                                                  plotOutput("cal_plot2"),
                                                  textOutput("text"))
                                    ) # end sidebarLayout
                           ) # end tab
                ) #end navbar
)

server <- function(input, output) {


# output text for tab 3
  cal_reactive_text <- reactive({
    text_data %>%
      filter(group %in% input$species_info)
  })

  # graph for tab 3

  tableStart <- data.frame(Column1 = NA, Column2 = NA)

  output$text <- renderTable({rbind(tableStart, cal_reactive_text())})


# output plot for tab 3
cal_reactive2 <- reactive({
  cal_data %>%
    filter(group %in% input$species_info)
}) # end output$cal_plot 2

# graph for tab 3
output$cal_plot2 <- renderPlot(
  ggplot(data = cal_reactive2(), aes(x = year, y = landings)) +
    geom_point(aes(color = group))+
    theme_minimal()+
    labs(x= "Year",
         y= "Landings (lbs)")


) # end output$cal_plot1




}


shinyApp(ui = ui, server = server)
