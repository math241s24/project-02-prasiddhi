
library(shiny)
library(tmap)
library(tidyverse)
library(sf)
library(tidymodels)

# Data stuff
data("World")

countries <- World %>%
  st_drop_geometry() %>%
  select(name, continent) %>%
  filter(continent %in% c("North America", "South America", "Asia", "Africa")) %>%  
  rename("Country" = name) %>%
  mutate(continent = as.character(continent)) # So I can assign Mauritius a continent

merged <- read_csv("../data/merged_data_cleaned.csv") %>%
  rename("Country" = Country.of.Origin,
         "Total" = Total.Cup.Points) %>%
  mutate(Country = recode(Country,
                          "Cote d?Ivoire" = "Cote d'Ivoire",
                          "Tanzania, United Republic Of" = "Tanzania",
                          "Laos" = "Lao PDR",
                          "United States (Puerto Rico)" = "Puerto Rico",
                          "United States (Hawaii)" = "United States")) %>% 
  filter(!is.na(Country)) %>%
  group_by(Country)

both_world <- merge(countries, merged, 
                    by.x = "Country", by.y = "Country", 
                    all.x = TRUE, all.y = TRUE) 

coffee <- both_world %>%
  filter(complete.cases(Species, Acidity, Aroma, Flavor, Sweetness, Total)) %>%
  mutate(continent = ifelse(Country == "Mauritius", "Africa", continent)) %>%
  filter(continent %in% c("North America", "South America", "Asia", "Africa")) %>%
  mutate(Species = as.factor(Species))

coffee$continent <- factor(coffee$continent) # Changing it back to factor

class(coffee$continent)

# Training the model!

coffee_parts <- coffee %>%
  initial_split(prop = 0.8)

train <- coffee_parts %>%
  training()

test <- coffee_parts %>%
  testing()

form_all <- as.formula(
  "continent ~ Sweetness + Acidity + Total + Flavor + Aroma + Species"
)

mod_tree_all <- decision_tree(mode = "classification") %>%
  set_engine("rpart") %>%
  fit(form_all, data = train)

mod_forest <- rand_forest(
  mode = "classification", 
  mtry = 6, 
  trees = 201) %>%
  set_engine("randomForest") %>%
  fit(formula = form_all, data = train)

# Here is shinyapp

ui <- fluidPage(
  titlePanel("Guess the Continent based on Coffee Taste!"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("Sweetness", "Sweetness:", min = 0, max = 10, value = 5),
      sliderInput("Acidity", "Acidity:", min = 0, max = 10, value = 5),
      sliderInput("Total", "Total:", min = 0, max = 10, value = 5),
      sliderInput("Flavor", "Flavor:", min = 0, max = 10, value = 5),
      sliderInput("Aroma", "Aroma:", min = 0, max = 10, value = 5),
      selectInput("Species", "Species:", choices = c("Arabica", "Robusta")),
      actionButton("predict_button", "Predict") 
    ),
    mainPanel(
      textOutput("prediction")
    )
  )
)

server <- function(input, output) {
  
  class_to_continent <- c("1" = "North America",
                          "2" = "South America",
                          "3" = "Asia",
                          "4" = "Africa")
  
  observeEvent(input$predict_button, {
      input_data <- data.frame(
      Sweetness = input$Sweetness,
      Acidity = input$Acidity,
      Total = input$Total,
      Flavor = input$Flavor,
      Aroma = input$Aroma,
      Species = input$Species
    )
    
    
      pred_tree <- predict(mod_tree_all, new_data = input_data, type = "class")
      pred_tree <- as.character(pred_tree)  
      
      pred_tree_continent <- class_to_continent[pred_tree]
      print(paste("Decision Tree Prediction:", pred_tree, "=>", pred_tree_continent)) 
      # I wanted to check to see that the number prediction corresponded with the right continent
      
      pred_forest <- predict(mod_forest, new_data = input_data, type = "class")
      pred_forest <- as.character(pred_forest)
      
      pred_forest_continent <- class_to_continent[pred_forest]
      print(paste("Random Forest Prediction:", pred_forest, "=>", pred_forest_continent))  
      
      output$prediction <- renderText({
        paste("Decision Tree Prediction:", pred_tree_continent, "\n",
              "Random Forest Prediction:", pred_forest_continent)
    })
  })
}

shinyApp(ui = ui, server = server)
