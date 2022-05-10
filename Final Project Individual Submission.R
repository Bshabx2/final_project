library(fpp3)
library(shiny)

# Path where data is
file_path <- "C:\\Users\\Bshab\\OneDrive\\Documents\\BAS 475\\Final Project Individual\\multiTimeline.csv"

# Data starts in 3rd row, skip first 2 rows
g_trends <- read.csv(file_path, skip = 2)
# Rename columns
names(g_trends) <- c("Month", "Interest")
# Convert Month to date
g_trends$Month <- yearmonth(g_trends$Month)
# Convert to tsibble
g_trends <- tsibble(g_trends)
g_trends$Interest
ifelse(g_trends$Interest == "<1", 0 , g_trends$Interest)
as.numeric(ifelse(g_trends$Interest == "<1", 0 , g_trends$Interest))
g_trends$Interest <- as.numeric(ifelse(g_trends$Interest == "<1", 0 , g_trends$Interest))


library(shiny)

ui <- fluidPage(
  
  img(src = "https://tse1.mm.bing.net/th?id=OIP.PHVQe_0PN3_U5dBic0wpdgHaC9&pid=Api&P=0&w=466&h=186", position = "right",
      width = "30%"
  ),
  
  selectInput(
    
    inputId = 'selected',
    label = "Select Plot",
    choices = c("Seasonality","Autocorrelation","Decomposition")
  
  ),
  
  
  textOutput("Intro"),
plotOutput("GTPlot", width = "60%",height = "250"),
textOutput("Interpretations"),
plotOutput("my_plot", height = "250"),
#textOutput("Interpretations"),
plotOutput("Forecast"),
textOutput("ForecastText"),

selectInput( inputId = "Final",
             label = "Select Forecast",
             choices = c("Mean", "NAIVE", "Seasonal Naive", "Drift")),
plotOutput("FinalFour"),
selectInput(inputId = "BitchSauce",
            label = "Select graph to be displayed",
            choices = c("Holts", "Holts/Winters","Manual Arima")),
plotOutput("Ariba")
)

autoplot(g_trends)


##################################################################################################
##################################################################################################


server <- function(input, output, session) {

  output$Intro <- renderText({
    c( "Welcome! Use the drop down box above to select which ","graph you'd like to be presented with. You'll then be presented with an interpretation directly below the graph. The graph found at the bottom of the app is an uninteractive forecast. Enjoy! :)")
  })
  
   output$GTPlot <- renderPlot({
    autoplot(g_trends) + labs(title = "Interest in League of Legends represented as a time series")
  })
  
  output$my_plot <- renderPlot({
    if(input$selected == "Seasonality"){
      gg_season(g_trends) + labs(title = "Seasonality of Interest in League of Legends")
    } else if(input$selected == "Autocorrelation"){
      ACF(g_trends) %>% autoplot() + labs(title = "Autocorrelation on League of Legends interest")
    } else if(input$selected == "Decomposition"){
      g_trends %>% model(classical_decomposition(Interest, type = "additive") 
                         ) %>% components() %>% autoplot() + labs(title =  "Classical Additive decomposition of League of Legends Interest ")
     
    }
  })
  
  output$Interpretations <- renderText({
    if(input$selected == "Seasonality"){
      c("There is almost no seasonality, however we do (very occasionaly) see peaks of interest later in the fall and earlier in the winter. This could be for a multitude of reasons, such as the League of Legends 'Worlds' championship tournament or possibly in-demand seasonal skin-lines being released by the game developers. Overall, there is no mentionable or statistically significant seasonality concerning the interest in League of Legends." )
    } else if(input$selected == "Autocorrelation"){
        c("We see strong values when investigating the autocorrelation on our data. Given their strong historical correlation, we can strongly rely on data from 1-2 months ago to infer on how our data may behave in the next month. Simply put, if you're trying to gague what next month's data will look like, you should take note of the previous two months.")
    } else if(input$selected == "Decomposition"){
        c("When running a classical additive decomposition on our time series we gain an insight on how trends, seasonality, and randomness contribute to the interest in League of Legends. Excluding peaks and troughs, Trend contributes to the smoothed out shape of the time series (note how it models the time series curve). We can see that seasonality is present, though in an arguably negligable manner as it doesnt explain much interest in League of Legends at all. However, it's good practice to note that it does indeed have an effect. If you'll take a look at the 'random' graph (bottom of the series) you'll see the shape of the time series wihtout the effects of the seasonal or trend graphs. Random is jagged and shows very slight skew in spread. Randomness is of course to be expected, and so long as we avoid any hard patterns (as we do here), we are in a good position.  ")
      }
  })
  
  output$Forecast <- renderPlot({
    fit2 <- g_trends %>% 
      model(TSLM(Interest ~ trend() + season()))
    
    fit2 %>% forecast(h = "1 years") %>% autoplot(g_trends) + labs(title = "Interest in League of Legends Forecasted 1 Year into the Future")
  })
  
  output$ForecastText <- renderText({
    c("This forecast was made using trend and seasonality. As you can see, there is an extreme amount of uncertainty. This is because trend and seasonality only account for approximately 14% of interest in League of Legends. Therefore, seasonality and trend have very little practical significance when predicting interest in the game, and more research should be done to find the other variables at play.")
  })

  output$FinalFour <- renderPlot({
    if(input$Final == "Mean"){
      g_trends %>% model(MEAN(Interest)) %>% 
        forecast() %>% autoplot(g_trends) + labs(title = "Forecast using Mean Model")
    } else if(input$Final == "NAIVE"){
      g_trends %>% model(NAIVE(Interest)) %>% 
        forecast() %>% autoplot(g_trends) + labs(title = "Forecast using Naive Model")
    } else if(input$Final == "Seasonal Naive") {
      g_trends %>% model(SNAIVE(Interest ~ lag("year"))) %>% 
        forecast() %>% autoplot(g_trends) + labs(title = "Forecast using Seasonal Naive Model")
    } else if(input$Final == "Drift") {
      g_trends %>% model(RW(Interest ~ drift())) %>% 
        forecast() %>% 
        autoplot(g_trends) + labs(title = "Forecast using Drift Model")
    }
    
  })
  
  
  output$Ariba <- renderPlot({
    if(input$BitchSauce == "Holts"){
      g_trends %>% model(`Holt's method` = ETS(Interest ~ error("A") + trend("A") + season("N"))) %>%
        forecast() %>% 
        autoplot(g_trends) + labs(title = "Holt's Method")
    } else if(input$BitchSauce == "Holts/Winters"){g_trends %>%
        model(
          additive = ETS(Interest ~ error("A") + trend("A") +
                           season("A")),
          multiplicative = ETS(Interest ~ error("M") + trend("A") +
                                 season("M"))) %>% forecast() %>% autoplot(g_trends) + labs(title = "Holt/Winters Method")
    } else if(input$BitchSauce == "Manual Arima"){
      g_trends %>%  model(ARIMA(Interest)) %>% forecast() %>% autoplot(g_trends) + labs(title = "ARIMA Method Manual Selection")
      }
  
   
  })
  
  
  
}

shinyApp(ui, server)








