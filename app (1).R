library(shiny)
library(ggplot2)
library(ggvis)
library(dplyr)
library(tidyr)

titanic_data <- read.csv("/Studium/WiSe25/bewerbungen/titanic_data.csv")

# UI-Definition
ui <- fluidPage(
  titlePanel("Das Unglück der Titanic - Wer hat überlebt?"),
  sidebarLayout(
    sidebarPanel(
      selectInput("gender", "Geschlecht", choices = c("Alle", unique(as.character(titanic_data$Sex)))),
      sliderInput("ageRange", "Altersbereich", min = 0, max = 100, value = c(0, 100)),
      selectInput("pclass", "Passagierklasse", choices = c("Alle", unique(as.character(titanic_data$Pclass)))),
      h3("Sources"), p("https://chat.openai.com/share/399fe3f1-04fd-43f5-bcb8-789a87545e15")
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Altersverteilung der Passagiere", plotOutput("ageDistribution")),
                  tabPanel("Überlebensrate nach Altersgruppen", plotOutput("survivalByAge")),
                  tabPanel("Überlebensraten nach Geschlecht und Klasse", plotOutput("survivalByGenderClass")),
                  tabPanel("Korrelation zwischen Ticketpreis und Überlebenschance", plotOutput("fareSurvivalCorrelation")),
                  tabPanel("Korrelation zwischen Alter und Ticketpreis", ggvisOutput("titanicScatterPlot")),
                  tabPanel("Heatmap Familiengröße + Passagierklasse", plotOutput("familySizeClassHeatmap"))
      ),
      uiOutput("htmlContent")
    )
  )
)

# Server-Logik
server <- function(input, output) {
  filteredData <- reactive({
    data <- titanic_data
    if (input$gender != "Alle") {
      data <- data[data$Sex == input$gender, ]
    }
    data <- data[data$Age >= input$ageRange[1] & data$Age <= input$ageRange[2], ]
    if (input$pclass != "Alle") {
      data <- data[data$Pclass == as.numeric(input$pclass), ]
    }
    data
  })
  
  output$survivalByGenderClass <- renderPlot({
    data <- filteredData()
    # Entfernen von NA-Werten aus den relevanten Spalten
    data <- data[!is.na(data$Sex) & !is.na(data$Pclass), ]
    ggplot(data, aes(x = factor(Pclass), fill = factor(Survived))) +
      geom_bar(position = position_fill()) +
      facet_wrap(~ Sex) +
      scale_fill_manual(values = c("red", "green"), labels = c("Gestorben", "Überlebt")) +
      labs(fill = "Status", x = "Passagierklasse", y = "Anteil") +
      theme_minimal()
  })
  
  output$ageDistribution <- renderPlot({
    data <- filteredData()
    ggplot(data, aes(x = Age)) +
      geom_histogram(binwidth = 5, fill = "blue", color = "black") +
      labs(x = "Alter", y = "Anzahl der Passagiere")
  })
  
  output$survivalByAge <- renderPlot({
    data <- filteredData() %>%
      group_by(AgeGroup = cut(Age, breaks = seq(0, 80, by = 10), include.lowest = TRUE)) %>%
      summarise(Überlebensrate = mean(Survived, na.rm = TRUE))
    
    ggplot(data, aes(x = AgeGroup, y = Überlebensrate)) +
      geom_line(group = 1, color = "#2C3E50") +
      geom_point(size = 4, color = "#E74C3C") +
      expand_limits(y = 0) +
      labs(x = "Altersgruppe", y = "Überlebensrate (%)") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom"
      ) +
      scale_y_continuous(labels = scales::percent)
  })
  
  output$fareSurvivalCorrelation <- renderPlot({
    data <- filteredData()
    ggplot(data, aes(x = factor(Survived), y = Fare, fill = factor(Survived))) +
      geom_boxplot() +
      scale_fill_manual(values = c("red", "green"), labels = c("Gestorben", "Überlebt")) +
      labs(fill = "Status", x = "Überlebt (1 = ja, 0 = nein)", y = "Ticketpreis") +
      theme_minimal()
  })
  
  output$titanicScatterPlot <- reactive({
    data <- filteredData()
    # Survived muss als Faktor vorliegen
    data$Survived <- as.factor(data$Survived)
    
    data %>%
      ggvis(x = ~Age, y = ~Fare, fill = ~Survived) %>%
      layer_points(size := 100, opacity := 0.5) %>%
      scale_nominal("fill", domain = c("0", "1"),
                    range = c("red", "green")) %>%
      add_axis("x", title = "Alter") %>%
      add_axis("y", title = "Ticketpreis") %>%
      set_options(width = 600, height = 400) %>%
      add_legend("fill", title = "Überlebt", values = c("0", "1")) %>%
      bind_shiny("titanicScatterPlot")
  })
  
  output$familySizeClassHeatmap <- renderPlot({
    data <- filteredData()
    
    heatmap_data <- data %>%
      group_by(FamilySize = SibSp + Parch, Pclass) %>%
      summarise(Survived = mean(Survived, na.rm = TRUE), .groups = 'drop') %>%
      drop_na(Survived) # Entfernt Zeilen mit NA in der Spalte Survived
    
    ggplot(heatmap_data, aes(x = factor(FamilySize), y = factor(Pclass), fill = Survived)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "red", high = "green") +
      labs(title = "Überlebensrate nach Familiengröße und Passagierklasse",
           x = "Familiengröße",
           y = "Passagierklasse",
           fill = "Überlebensrate") +
      theme_minimal()
  })
  
  # HTML content
  html_content <- '
    <h1>Überlebenschancen auf der Titanic</h1>

    <h2>Geschlechtsspezifische Überlebensraten</h2>
    <p>Nach Analyse der überlebenden Passagiere von fast 300 Personen zeichnet sich deutlich ab, dass Frauen im Vergleich zu Männern eine erheblich höhere Überlebensrate aufwiesen. Von den knapp 200 überlebenden Frauen übertraf ihre Zahl die der überlebenden Männer um das Zweifache, was auf eine starke Korrelation hinweist.</p>

    <h2>Klassenbasierte Überlebensraten bei Frauen</h2>
    <p>Interessanterweise hatten die meisten überlebenden Frauen eine Suite der Klasse 1, während die Überlebensrate von Frauen in der ersten Klasse niedriger war. Eine ähnliche Tendenz zeigt sich in der zweiten Klasse, wobei die Anzahl der nicht überlebenden Frauen in der dritten Klasse am höchsten war. Der Vergleich von 3 nicht überlebenden Frauen in der ersten Klasse und 55 nicht überlebenden Frauen in der dritten Klasse legt nahe, dass eine Korrelation zwischen Wohlstand und Überlebensfähigkeit besteht.</p>

    <h2>Klassenbasierte Überlebensraten bei Männern</h2>
    <p>Bei den Männern gestaltet sich die Korrelation komplexer. Die geringste Überlebensrate zeigte sich bei Personen der zweiten Klasse, während zwischen der ersten und dritten Klasse kein signifikanter Unterschied erkennbar ist. Dennoch weist die erste Klasse eine höhere Anzahl überlebender Männer auf.</p>

    <h2>Bevorzugte Behandlung von Frauen und Kindern</h2>
    <p>Es deutet sich an, dass Frauen und Kinder bevorzugt behandelt wurden, obwohl Kinder auf dem Schiff in der Minderheit waren. Andere Faktoren wie SibSp, Parch, Ticket, Cabin, Embarked und FamilySize zeigten keine nachvollziehbaren Korrelationen.</p>
  '
  
  # Render HTML content as UI
  output$htmlContent <- renderUI({
    tags$div(HTML(html_content))
  })
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)
