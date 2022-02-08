

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
shiny_theme <- bs_theme(bootswatch = "minty")

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
                                        ), #end mainpanel
                                    ) # end sidebarlayout
                           ), #end tabPanel 1
                           tabPanel("Farm Map",
                                    sidebarLayout(
                                        sidebarPanel(
                                            checkboxGroupInput(inputId = "pick_species",
                                                               label = "Species:",
                                                               choices = unique(cal_data$species)
                                            ) # end checkboxGroupInput
                                        ), #end sidebarPanel
                                        mainPanel("FARM MAP",
                                                  plotOutput("cal_plot1"))
                                    ) # end sidebarLayout
                           ), # end tabpanel 2
                           tabPanel("PIER Maps",
                                    sidebarLayout(
                                        sidebarPanel(
                                            checkboxGroupInput(inputId = "pier_maps",
                                                               label = "PIER maps:",
                                                               choices = unique(cal_data$species)
                                            ) # end checkboxGroupInput
                                        ), #end sidebarPanel
                                        mainPanel("PIER Map",
                                                  plotOutput("cal_plot2"))
                                    ) # end sidebarLayout
                           ) # end tabpanel 3
                ) #end navbar
) # end ui

server <- function(input, output) {

} # end output$cal_plot2


shinyApp(ui = ui, server = server)
