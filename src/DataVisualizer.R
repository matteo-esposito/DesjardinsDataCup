library(shiny)
library(dplyr)
library(plotly)
library(readr)
library(stringr)
library(caret)
library(car)
library(data.table)
library(shiny)
library(plotly)
library(binr)
library(ineq)
library(pROC)
library(shinydashboard)
library(shinyjs)
ui <- 
  
  dashboardPage(
    
    #Header is now green
    skin = "green",
    
    
    #Adds a  logo to the top
    dashboardHeader(title = div(
      img(src = "Logo.jpg", style = "position:absolute;left:10px;top:7px;height:40px;width:40px"),
      "Model Graphs"
    )),
    
    
    
    #This adds in the inputs on the sidebar
    dashboardSidebar(
      #Loading script that runs whenever data is busy loading
      tagList(
        tags$head(
          tags$link(rel="stylesheet", type="text/css",href="style-handler2.css"),
          tags$script(type="text/javascript", src = "busy-handler.js"),
          tags$style(
            type = "text/css",
            "
            #loadmessage {
            position: fixed;
            top: 0px;
            left: 0px;
            width: 100%;
            padding: 5px 0px 5px 0px;
            text-align: center;
            font-weight: bold;
            font-size: 100%;
            color: #000000;
            background-color: #C0C0C0;
            z-index: 105;
            }
            "
          )
          )
          ),
      div(class="busy", 
          p("Importing data in progress..."), 
          img(src="Loading_icon.gif"),
          style="text-align: center;"
      ),
      
      
      conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                       tags$div("Loading...", id = "loadmessage")),
      
      #The location to input your data path
      textInput(inputId = "dataPath",
                label = "Data Path File"),
      #To be pressed after path is entered
      actionButton("do", "Download"),
      
      #These are reactive based on the data being inported. They are the variables to be chosen by user.
      uiOutput("Variable_Input"),
      uiOutput("Variable_Input_2"),
      uiOutput("Variable_Input_3"),
      uiOutput("Variable_Input_4"),
      
      
      #Used for bucketing the numeric variables that would not be possible to graph without bucketing
      sliderInput(
        inputId = "bins",
        label = "Number of Bins Obs vs Pred",
        min = 1,
        max = 250,
        value = 5
      ),
      
      #Determine how many points to put for the double lift hcart. 
      sliderInput(
        inputId = "quant",
        label = "Number of Quantiles Double Lift",
        min = 1,
        max = 250,
        value = 10
      ),
      
      #To be pressed on the first iteration of loading graphs. Afterwards is not necessary to push once 
      #loaded first time.
      actionButton("adj", "Load Graphs")
      
          ),
    
    #Where the graphs appear in the main body
    dashboardBody(
      fluidRow(
        tabBox(
          
          tabPanel("Observed Vs Predicted",
                   br(),
                   #plotly is processed in server
                   plotlyOutput("plotly") ),
          tabPanel("Double Lift",
                   br(),
                   #doublelift is processed in server
                   plotlyOutput("doublelift") ),
          tabPanel(
            "Lorenz Curve",
            br(),
            plotlyOutput("lorenz"),
            textOutput("gini"),
            textOutput("gini2")
          ),
          tabPanel(
            "ROC Curve",
            br() ,
            plotlyOutput("roc") ,
            textOutput("aroc"),
            textOutput("aroc2")
          ),
          width = 12
        )
      )
    )
      )


library(shiny)
library(dplyr)
library(plotly)
library(readr)
library(stringr)
library(caret)
library(car)
library(data.table)
library(shiny)
library(plotly)
library(binr)
library(TDTools)
library(ineq)
library(pROC)
library(shinydashboard)
library(shinyjs)
library(shinyjqui)

server <- shinyServer(function(input, output) {
  DATAS <<- reactiveValues()
  observeEvent(input$do,
               #Observe event so when button is clicked command is done
               #try so that app doesnt crash if theres an error made by user
               try(DATAS$ModelResults <<-
                     fread(input = input$dataPath, data.table = FALSE))
  )
  #Creating my data as a global variable so It can be used by the rest of my program
  
  
  #These 4 outputs using renderUI depend on the data I uplaoded. the colnames of data will give all 
  #variables.
  output$Variable_Input <- renderUI({
    
    selectInput(
      inputId = "obs",
      label = "Observed",
      choices = colnames(DATAS$ModelResults)
    )
  })
  output$Variable_Input_2 <- renderUI({
    selectInput(
      inputId = "mod1",
      label = "Predicted of Model 1",
      choices = colnames(DATAS$ModelResults)
    )
  })
  output$Variable_Input_3 <- renderUI({
    selectInput(
      inputId = "mod2",
      label = "Predicted of Model 2",
      choices = colnames(DATAS$ModelResults)
    )
  })
  output$Variable_Input_4 <- renderUI({
    selectInput(
      inputId = "var",
      label = "Variable",
      choices = colnames(DATAS$ModelResults)
    )
  })
  
  observeEvent(input$adj,
               ({
                 #preparing data to be used by lorenz curve
                 DATA = DATAS$ModelResults
                 
                 attach(DATA)
                 
                 #Sorted my data by the model 1 prediction
                 srtData <-
                   DATA[order(eval(parse(text = input$mod1))), ]
                 
                 
                 detach(DATA)
                 
                 srtData$Exposure = 1
                 
                 totObs <-
                   sum(srtData[, input$obs])
                 #Finding total number of observations
                 totExp <- sum(srtData$Exposure)
                 #Finding total exposures
                 
                 #Will give 2 new columns with cumulative exposure and cumulative default. 
                 #These are ordered by model 1 prediction so these x y will provide for lorenz curve
                 srtData$cloSum1 = cumsum(srtData[, input$obs]) / totObs
                 srtData$expSum1  = cumsum(srtData$Exposure) / totExp
                 
                 attach(srtData)
                 
                 srtData <-
                   srtData[order(eval(parse(text = input$mod2))), ]
                 #Sorted my data by the model 2 prediction
                 
                 detach(srtData)
                 
                 srtData$cloSum2 = cumsum(srtData[, input$obs]) / totObs
                 srtData$expSum2  = cumsum(srtData$Exposure) / totExp
                 #Will give 2 new columns with cumulative exposure and cumulative default. 
                 #These are ordered by model 2 prediction so these x y will provide for lorenz curve
                 DATAS$srtData <<- srtData
               }))
  
  
  observeEvent(input$adj,
               ({
                 #Creating ROC objects to be used in roc curve. Has methods that make
                 #graphing and calculating area easier
                 DATAS$rocObj = roc(response = DATAS$ModelResults[, input$obs],
                                    predictor = DATAS$ModelResults[, input$mod1])
                 
                 DATAS$rocObj2 = roc(response = DATAS$ModelResults[, input$obs],
                                     predictor = DATAS$ModelResults[, input$mod2])
                 
               }))
  
  observeEvent(input$adj,
               #Determining when to bucket. I decided to do that if there were more than 1 tenth of the lines were unique,
               #then it would be appropriate to bucket. Bins are quantiles
               output$plotly <- renderPlotly({
                 if (length(unique(DATAS$ModelResults[, input$var])) > 250 &
                     is.numeric(DATAS$ModelResults[, input$var]))
                 {
                   #C gives me the reactive value of what my seq should depend on to bin.
                   c = 1  /  input$bins
                   #Making new data set
                   data_Grouped = DATAS$ModelResults %>%
                     mutate(Grouped = cut(
                       #Cutting the input variable
                       x = DATAS$ModelResults[, input$var],
                       #Cuts are based on quantiles
                       breaks =  unique(quantile(
                         x = DATAS$ModelResults[, input$var],
                         na.rm = TRUE,
                         #the probs of sequqnece between 0 and 1. Will have the probs based on
                         #the input variable mentioned earlier
                         probs = seq(0, 1, c)
                       ))
                     ))  %>%
                     
                     mutate(Exposure = 1) %>%
                     group_by(Grouped) %>%
                     summarise(
                       #Use eval(parse()) since inputs are texts.
                       #Creating new variables for my grouped data
                       Clo_Sale_In_Mean = mean(eval(parse(text = input$obs))),
                       Clo_Sale_In_Sum  = sum(eval(parse(text = input$obs))),
                       Mod_Est_Mean     = mean(eval(parse(text = input$mod1))),
                       Mod_Est_Mean_2   = mean(eval(parse(text = input$mod2))),
                       Xpo          = sum(Exposure)
                     ) %>%
                     as.data.frame()
                   
                   plot_ly(data = data_Grouped) %>%
                     add_bars(
                       x = ~ Grouped,
                       y =  ~ Xpo,
                       yaxis = "y2",
                       opacity = 0.35,
                       name = "Exposure"
                     )  %>%
                     add_lines(
                       x = ~ Grouped,
                       y =    ~ Clo_Sale_In_Mean,
                       name = "Observed",
                       #Comparison line is dotted and black so can be seen easilty
                       line = list(color = 'rgba(0, 0, 0, 1)', dash = "dot")
                     )  %>%
                     add_lines(x = ~ Grouped,
                               y =    ~ Mod_Est_Mean,
                               name = "Model")  %>%
                     add_lines(x = ~ Grouped,
                               y =    ~ Mod_Est_Mean_2,
                               name = "Model 2")  %>%
                     layout(
                       #margins are adjusted otherwise certain numbers are cut off
                       # Numbers are still cut off but is better
                       margin = list( b = 125),
                       barmode  =  "stack",
                       yaxis = list(title = "default"),
                       xaxis = list(title = ""),
                       title = paste(input$var, "default Graphs", sep  =  " "),
                       yaxis2 = list(
                         title = "Exposure",
                         side  =  "right",
                         overlaying = "y"
                       )
                     )
                 }
                 else {
                   data = DATAS$ModelResults  %>%
                     mutate(Exposure = 1) %>%
                     group_by_(input$var) %>%
                     summarise(
                       Clo_Sale_In_Mean = mean(eval(parse(text = input$obs))),
                       Clo_Sale_In_Sum  = sum(eval(parse(text = input$obs))),
                       Mod_Est_Mean     = mean(eval(parse(text = input$mod1))),
                       Mod_Est_Mean_2     = mean(eval(parse(text = input$mod2))),
                       Xpo          = sum(Exposure)
                     ) %>%
                     as.data.frame()
                   
                   plot_ly(data = data) %>%
                     add_bars(
                       x = data[, input$var],
                       y =  ~ Xpo,
                       yaxis = "y2",
                       opacity = 0.35,
                       name = "Exposure"
                     )  %>%
                     add_lines(
                       x = data[, input$var],
                       y =    ~ Clo_Sale_In_Mean,
                       name = "Observed",
                       line = list(color = 'rgba(0, 0, 0, 1)', dash = "dot")
                     )  %>%
                     add_lines(x = data[, input$var],
                               y =    ~ Mod_Est_Mean,
                               name = "Model")  %>%
                     add_lines(x = data[, input$var],
                               y =    ~ Mod_Est_Mean_2,
                               name = "Model 2")  %>%
                     layout(
                       margin = list(b = 125),
                       barmode  =  "stack",
                       xaxis = list(title = ""),
                       yaxis = list(title = "default"),
                       title = paste(input$var, "default Graphs", sep  =  " "),
                       yaxis2 = list(
                         title = "Exposure",
                         side  =  "right",
                         overlaying = "y"
                       )
                     )
                 }
                 
               }))
  #Making the double lift chart, with x value as sort ratio.
  observeEvent(input$adj,
               # see above. Same logic as before regarding cutting using quantiles and sequences,
               #for equal exposure cuts
               output$doublelift <-  renderPlotly({
                 c = (1 / input$quant)
                 
                 DATA = DATAS$ModelResults  %>%
                   mutate(SortRatio = eval(parse(text = input$mod2)) / eval(parse(text = input$mod1)))
                 
                 DATA_Grouped = DATA %>%
                   mutate(Grouped = cut(
                     x = DATA$SortRatio,
                     breaks = quantile(
                       x = DATA$SortRatio,
                       na.rm = TRUE,
                       probs = seq(0, 1, c)
                     )
                   )) %>%
                   mutate(Exposure = 1) %>%
                   group_by(Grouped) %>%
                   summarise(
                     Clo_Sale_In_Sum  = sum(eval(parse(text = input$obs))),
                     y1 = mean(eval(parse(text = input$mod1))) / mean(eval(parse(text = input$obs))),
                     y2 = mean(eval(parse(text = input$mod2))) / mean(eval(parse(text = input$obs))),
                     Xpo          = sum(Exposure)
                   ) %>%
                   as.data.frame()
                 
                 plot_ly(data = DATA_Grouped) %>%
                   
                   
                   
                   add_lines(x =  ~ Grouped,
                             y =  ~ y1,
                             name = "Model 1/Obs") %>%
                   add_lines(x =  ~ Grouped,
                             y =  ~ y2,
                             name = "Model 2/Obs") %>%
                   add_lines(
                     x =  ~ Grouped,
                     y = 1,
                     name = "Standard",
                     line = list(color = 'rgba(0, 0, 0, 1)', dash = "dot")
                   ) %>%
                   
                   layout(
                     margin = list( b = 125),
                     xaxis = list(title = "SortRatio"),
                     yaxis = list(title = "Model/Observed"),
                     title = "Double Lift default"
                   )
               }))
  
  observeEvent(input$adj,
               output$lorenz <-  renderPlotly({
                 #  Using the data prepared before to create lorenz curve.
                 
                 
                 plot_ly(data = DATAS$srtData) %>%
                   add_lines(x =  ~ expSum1,
                             y =  ~ cloSum1,
                             name = "Lorenz 1") %>%
                   add_lines(x =  ~ expSum2,
                             y =  ~ cloSum2,
                             name = "Lorenz 2") %>%
                   add_lines(
                     x =  ~ expSum1,
                     y =  ~ expSum1,
                     name = "Regular",
                     line = list(color = 'rgba(0, 0, 0, 1)', dash = "dot")
                   ) %>%
                   layout(
                     xaxis = list(title = "% of Exposures"),
                     yaxis = list(title = "% of default"),
                     title = "Lorenz Curve"
                   )
                 
                 
               }))
  
  
  
  observeEvent(input$adj,
               output$gini <-  renderText({
                 paste(
                   "Gini Index Model 1: ",
                   1 - 2 * flux::auc(
                     x = DATAS$srtData$expSum1,
                     y = DATAS$srtData$cloSum1
                   )
                 )
                 
               }, quoted = TRUE))
  
  observeEvent(input$adj,
               output$gini2 <-  renderText({
                 paste(
                   "Gini Index Model 2: ",
                   1 - 2 * flux::auc(
                     x = DATAS$srtData$expSum2,
                     y = DATAS$srtData$cloSum2
                   )
                 )
                 
               }, quoted = TRUE))
  
  
  observeEvent(input$adj,
               #ROC can be graphed but I want it to be plotly instead of plot
               output$roc <-  renderPlotly({
                 #Extract specifities and sensitivites from roc object.
                 # 1- specificities is false positive rate
                 x = 1 - DATAS$rocObj$specificities
                 # Sensitivities is true positive rate
                 y = DATAS$rocObj$sensitivities
                 x2 = 1 - DATAS$rocObj2$specificities
                 y2 = DATAS$rocObj2$sensitivities
                 
                 # I am getting out the specificity and the sensitivites
                 #They however do not have same amount of lines so must add in blanks
                 #So I can merge and include in one data set for plotly
                 #For loop will determine which one has more lines
                 #Will afterwards add in the amount of lines needed until both have same amount.
                 if (length(x) < length (x2)) {
                   for (i in (length(x) + 1):length(x2))
                   {
                     x[i] = 1
                     y[i] = 1
                   }
                 }
                 if (length (x) > length(x2)) {
                   for (i in (length(x2) + 1):length(x)) {
                     x2[i] = 1
                     y2[i] = 1
                   }
                 }
                 
                 #Make a data frame out of the four variables
                 df = data.frame(x, y, x2, y2)
                 
                 
                 #plotly is made using the new data frame with the variables
                 plot_ly(data = df) %>%
                   add_lines(x = ~ x,
                             y = ~ y,
                             name = "Roc Curve 1")  %>%
                   
                   add_lines(x = ~ x2,
                             y = ~ y2,
                             name = "Roc Curve 2")  %>%
                   
                   add_lines(
                     x =  ~ x,
                     y =  ~ x,
                     name = "Regular",
                     line = list(color = 'rgba(0, 0, 0, 1)', dash = "dot")
                   ) %>%
                   layout(
                     xaxis = list(title = "False Positive Rate"),
                     yaxis = list(title = "True Positive Rate"),
                     title = "ROC Curve"
                   )
               }))
  observeEvent(input$adj,
               output$aroc <-  renderText({
                 paste("Area under ROC curve 1: ",
                       #came in the roc package to calculate roc auc. Same for second roc
                       auc(DATAS$rocObj))
                 
               }, quoted = TRUE))
  observeEvent(input$adj,
               output$aroc2 <-  renderText({
                 paste("Area under ROC curve 2: ",
                       auc(DATAS$rocObj2))
                 
               }, quoted = TRUE))
})

shinyApp(
  ui = ui,
  server = server,
  options = list("launch.browser" = T)
)

