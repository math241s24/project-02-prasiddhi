#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Load required libraries
library(shiny)
library(tmap)
library(tmaptools)
library(dplyr)
library(readr)
data("World")

# Load each dataset
countries <- World %>%
  select(name, iso_a3) %>%
  rename("Country of Origin" = name)

arabica <- read_csv("data/arabica_data_cleaned.csv") %>%
  rename("Country of Origin" = Country.of.Origin,
         "Total Cup Points" = Total.Cup.Points) %>%
  select("Country of Origin","Acidity", "Aroma", "Flavor", "Sweetness", "Total Cup Points") %>%
  mutate(`Country of Origin` = recode(`Country of Origin`,
                                      "Cote d?Ivoire" = "Cote d'Ivoire",
                                      "Tanzania, United Republic Of" = "Tanzania",
                                      "Laos" = "Lao PDR",
                                      "United States (Puerto Rico)" = "Puerto Rico",
                                      "United States (Hawaii)" = "United States")) %>% 
  filter(`Country of Origin` != "Mauritius") %>%
  filter(!is.na(`Country of Origin`)) %>%
  group_by(`Country of Origin`) %>%
  summarize(`Acidity` = mean(Acidity),
            `Aroma` = mean(Aroma),
            `Flavor` = mean(Flavor),
            `Sweetness` = mean(Sweetness),
            `Total Cup Points` = mean(`Total Cup Points`)) 

arabica_world <- merge(countries, arabica, 
      by.x = "Country of Origin", by.y = "Country of Origin", 
      all.x = TRUE, all.y = TRUE)

robusta <- read_csv("data/robusta_data_cleaned.csv") %>%
  rename("Country of Origin" = Country.of.Origin,
         "Total Cup Points" = Total.Cup.Points,
         "Acidity" = `Salt...Acid`,
         "Aroma" = `Fragrance...Aroma`,
         "Sweetness" = `Bitter...Sweet`) %>%
  select("Country of Origin","Acidity", "Aroma", "Flavor", "Sweetness", "Total Cup Points") %>% 
  filter(!is.na(`Country of Origin`)) %>%
  group_by(`Country of Origin`) %>%
  summarize(`Acidity` = mean(Acidity),
            `Aroma` = mean(Aroma),
            `Flavor` = mean(Flavor),
            `Sweetness` = mean(Sweetness),
            `Total Cup Points` = mean(`Total Cup Points`))

robusta_world <- merge(countries, robusta, 
                       by.x = "Country of Origin", by.y = "Country of Origin", 
                       all.x = TRUE, all.y = TRUE)

merged <- read_csv("data/merged_data_cleaned.csv") %>%
  rename("Country of Origin" = Country.of.Origin,
         "Total Cup Points" = Total.Cup.Points) %>%
  select("Country of Origin","Acidity", "Aroma", "Flavor", "Sweetness", "Total Cup Points") %>%
  mutate(`Country of Origin` = recode(`Country of Origin`,
                                      "Cote d?Ivoire" = "Cote d'Ivoire",
                                      "Tanzania, United Republic Of" = "Tanzania",
                                      "Laos" = "Lao PDR",
                                      "United States (Puerto Rico)" = "Puerto Rico",
                                      "United States (Hawaii)" = "United States")) %>% 
  filter(`Country of Origin` != "Mauritius") %>% 
  filter(!is.na(`Country of Origin`)) %>%
  group_by(`Country of Origin`) %>%
  summarize(`Acidity` = mean(Acidity),
            `Aroma` = mean(Aroma),
            `Flavor` = mean(Flavor),
            `Sweetness` = mean(Sweetness),
            `Total Cup Points` = mean(`Total Cup Points`))

both_world <- merge(countries, arabica, 
                       by.x = "Country of Origin", by.y = "Country of Origin", 
                       all.x = TRUE, all.y = TRUE)



if (require("shiny")) {
  
  arabica_vars <- setdiff(names(arabica_world), c("Country of Origin", "geometry", "iso_a3"))
  robusta_vars <- setdiff(names(robusta_world), c("Country of Origin", "geometry", "iso_a3"))
  both_vars <- setdiff(names(both_world), c("Country of Origin", "geometry", "iso_a3"))
  coffee_palette <- c("#FFEBCD", "#D2B48C", "#8B4513")
  species_names <- c("Arabica", "Robusta", "Both") 
  
  ui <- fluidPage(
    titlePanel("Coffee by Country"),
    tmapOutput("map"),
    selectInput("var", "Characteristic", arabica_vars),
    selectInput("species", "Species", choices = species_names)  # Updated input label
  )
  
  server <- function(input, output, session) {
    output$map <- renderTmap({
      species_label <- switch(input$species,
                              "Arabica" = "arabica_world",
                              "Robusta" = "robusta_world",
                              "Both" = "both_world")
      
      dataset <- get(species_label)
      
      tm_shape(dataset) +
        tm_polygons(input$var, zindex = 401, palette = coffee_palette, id = input$var) +
        tm_text("iso_a3", size = 0.4, just = "center")
    })
    
    observe({
      var <- input$var
      species_name <- input$species
      
      species_label <- switch(species_name,
                              "Arabica" = "arabica_world",
                              "Robusta" = "robusta_world",
                              "Both" = "both_world")
      
      dataset <- get(species_label)
      
      tmapProxy("map", session, {
        tm_remove_layer(401) +
          tm_shape(dataset) +
          tm_polygons(title = paste(var, "(",species_name,")"), var, zindex = 401, palette = coffee_palette, id = input$var) +
          tm_text("iso_a3", size = 0.4, just = "center")
      })
    })
  }	
  
  app <- shinyApp(ui, server)
  if (interactive()) app
}
