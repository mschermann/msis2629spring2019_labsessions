library(tidyverse)
library(shiny)

raw_data <- read_csv("step2.v3.csv", col_names = c("station_id", "year", "jan", "feb","mar","apr","may","jun","jul","aug","sep","oct", "nov","dec"), na = c("-9999"), col_types = cols(station_id = col_double(), year = col_integer(), jan = col_integer(), feb = col_integer(), mar = col_integer(), apr = col_integer(), may = col_integer(), jun = col_integer(), jul = col_integer(), aug = col_integer(), sep = col_integer(), oct = col_integer(), nov = col_integer(), dec = col_integer()))
summer <- raw_data[,c("station_id","year","jun","jul","aug")]
summer <- na.omit(summer)
summer$jja <- rowMeans(summer[, c("jun","jul","aug")], na.rm = TRUE)
summer <-  summer[,c("station_id","year","jja")]
base <- summer %>% filter(year>1950, year<1981) %>% group_by(station_id) %>% summarise(c = n(), m = mean(jja), s = sd(jja))
summer_base <- summer %>% left_join(base, by=c("station_id"))
summer_base$z_base <-  (summer_base$jja -summer_base$m) / summer_base$s
summer_base <- summer_base %>% filter(abs(z_base)<5)
summer_base$decade <- (summer_base$year %/% 10) * 10 +1
summer_base$decade <- ifelse(summer_base$year %% 10 == 0, summer_base$decade - 10, summer_base$decade)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("It’s Not Your Imagination. Summers Are Getting Hotter"),
   
   sidebarLayout(
      sidebarPanel(
         sliderInput("decade",
                     "Decade:",
                     min = min(summer_base$decade),
                     max = max(summer_base$decade),
                     value = 1951,
                     step = 10,
                     sep = "")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     
     ggplot(summer_base %>% filter(decade == input$decade) %>% select(station_id, decade, z_base), aes(x=z_base)) + 
       geom_density(alpha=.8, fill="#fee0d2") +
       geom_density(data = summer_base, aes(x=z_base), fill = "#CCCCCC", alpha=.3, color= "#CCCCCC") +
       geom_density(data = summer_base, aes(x=z_base, color=factor(decade)), color = "#CCCCCC", alpha=.1) +
       geom_vline(xintercept=1, alpha=.3) + 
       geom_vline(xintercept=-1, alpha=.3) + 
       geom_vline(xintercept=3, alpha=.2) + 
       geom_vline(xintercept=-3, alpha=.2) + 
       ylim(0, .5) + 
       xlim(-5, 5) + 
       theme_minimal() + 
       theme(panel.border = element_blank(), 
             panel.grid.major = element_blank(), 
             panel.grid.minor = element_blank(),
             axis.text = element_blank(),
             axis.title = element_blank())
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

