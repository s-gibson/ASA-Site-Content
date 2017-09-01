############################# 
##  ASA NFL Site Content   ##
##  Player Comparison App  ##
##  Stewart Gibson         ##
##  9/1/17                 ##
#############################

### Load data and packages
load("~/Documents/ASA/ASA Site Content/NFL/data/clean_data.RData")
require(shiny)
require(ggplot2)

ui <- fluidPage(
  # Player dropdown
  selectInput(inputId = "players", label = "Select players to compare:",
              choices = sort(unique(Fantasy.2016$First.Last)), multiple = T),
  selectInput(inputId = "chart.type", label = "Select chart type:",
              choices = c("DKP Distributions","DKP vs. Point Total","DKP vs. Spread")),
  #actionButton(inputId = "create.chart", label = "Create Chart"),
  
  plotOutput(outputId = "chart")
)

server <- function(input, output) {
  
    output$chart <- renderPlot({
      if (input$chart.type == "DKP Distributions") {
        ggplot(data = Fantasy.2016[which(Fantasy.2016$First.Last %in% input$players),],
               aes(x = DK.points)) +
          geom_density(aes(color = Initial.Last,fill = Initial.Last),alpha = 0.5) +
          ggtitle("Fantasy Points Distributions") +
          xlab("Fantasy Points") +
          ylab("Density") +
          theme(plot.title = element_text(hjust = 0.5)) +
          scale_x_continuous(limits = c(-10,60))
        } else if (input$chart.type == "DKP vs. Point Total") {
          ggplot(data = Fantasy.2016[which(Fantasy.2016$First.Last %in% input$players),],
               aes(x = Actual.Points, y = DK.points)) +
            geom_point(aes(color = Initial.Last)) +
            geom_smooth(aes(color = Initial.Last), se = F, method = 'loess') +
            ggtitle("Fantasy Points vs. Actual Points") +
            xlab("Actual Points") +
            ylab("Fantasy Points") +
            theme(plot.title = element_text(hjust = 0.5))
         } else if (input$chart.type == "DKP vs. Spread") {
           ggplot(data = Fantasy.2016[which(Fantasy.2016$First.Last %in% input$players),],
                  aes(x = Actual.Spread, y = DK.points)) +
             geom_point(aes(color = Initial.Last)) +
             geom_smooth(aes(color = Initial.Last), se = F, method = 'loess') +
             ggtitle("Fantasy Points vs. Spread") +
             xlab("Spread") +
             ylab("Fantasy Points") +
             theme(plot.title = element_text(hjust = 0.5))
           } else {}
      })
  }

shinyApp(ui = ui, server = server)

#####################











ggplot(data = samp.df[which(samp.df$Player %in% c("Andrew Luck","Tom Brady",
                                                  "Cam Newton","Tyrod Taylor")),],
       aes(x = DKP)) +
  geom_density(aes(color = Player,fill = Player),alpha = 0.5) +
  scale_x_continuous(limits = c(-5,50))

ggplot(data = samp.df[which(samp.df$Player %in% c("Andrew Luck","Tom Brady",
                                                  "Cam Newton","Tyrod Taylor")),],
       aes(x = Actual.Points, y = DKP)) +
  geom_point(aes(color = Player)) +
  geom_smooth(aes(color = Player), se = F, method = 'loess')

ggplot(data = samp.df[which(samp.df$Player %in% c("Andrew Luck","Tom Brady",
                                                  "Cam Newton","Tyrod Taylor")),],
       aes(x = Spread, y = DKP)) +
  geom_point(aes(color = Player)) +
  geom_smooth(aes(color = Player), se = F, method = 'loess')

