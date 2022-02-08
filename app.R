

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
    pivot_longer(!year, names_to = "species", values_to = "landings")

# custom theme
shiny_theme <- bs_theme(bootswatch = "yeti")

# start shiny app
ui <- fluidPage(theme = shiny_theme,
                navbarPage("California Aquaculture",
                           tabPanel("Home",
                                    sidebarLayout(
                                        sidebarPanel(),
                                        mainPanel(
                                            h1("App Description"),
                                            p("California Aquauclture through time by year, species and leasing processes"
                                            ),
                                            br(),
                                            p("The purpose of this Shiny App is to explore aquaculture landings data from the California Department of Fish and Wildlife (1971-2018) to better understand how aquaculture practices have changed through time."
                                            ),
                                        ), #end main
                                    ) # end sidebar
                           ), #end tab
                           tabPanel("Farm Map",
                                    sidebarLayout(
                                        sidebarPanel(
                                            checkboxGroupInput(inputId = "pick_species",
                                                               label = "Species:",
                                                               choices = list("Oysters" = 1, "Abalone" = 2, "Clams" = 3, "Mussels" = 4)
                                            ),
                                            sliderInput("slider2", label = h3("Slider Range"), min = 1971,
                                                        max = 2018, value = c(1990, 2010))# end checkboxGroupInput
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
                                                               choices = list("Oysters" = 1, "Abalone" = 2, "Clams" = 3, "Mussels" = 4)
                                            )# end checkboxGI
                                        ), #end sidebarPanel
                                        mainPanel("Species Information",
                                                  plotOutput("cal_plot2"))
                                    ) # end sidebarLayout
                           ) # end tab
                ) #end navbar
)

server <- function(input, output) {

}


shinyApp(ui = ui, server = server)
