library(ggplot2)
library(shinydashboard)
library(shiny)
library(DT)
library(knitr)
library(dplyr)
library(ggplot2)
library(rgl)
library(plotly)
library(readr)
library(readxl)
library(randomForest)

pharmData<-read_csv("https://github.com/pcobb316/FinalProject/blob/master/data.csv")
names(pharmData)<-c('location','time','perHealthSpend','percGDP','USDperCap','flag','total')

dashboardPage(skin='blue', 
              
              dashboardHeader(title="Pharmaceutical Drug Spending by Countries" , titleWidth =750 ),
              
              dashboardSidebar( sidebarMenu(
                menuItem("About", tabName = 'about'),
                menuItem("Data Summaries", tabName = 'datasum'),
                menuItem("Cluster/PCA", tabName = "clus"),
                menuItem("Modeling", tabName = 'model'),
                menuItem("Data", tabName = 'dataset')
              )), 
              dashboardBody(
                tabItems(
#--------------------------------------1st Tab-------------------------------------------#
                  tabItem(tabName = 'about', fluidPage(
                    column(5, 
                           h1("About This APP"), 
                           box(background='black', width = 10, 
                              h4('This app will allow us analyze the amount of money spent by countries 
                                                                 on pharmaceutical products as it relates to that countries GDP and other factors.'), 
                              h4("We will look at some graphical and numeric summaries."), 
                              h4("We will look at some cluster analysis and principal component analysis."),
                              h4("We will look at some model for predictions."),
                              h4("Finally, you will have access to the data."))
                    ),
                    column(5, 
                           h1("About The Data"), 
                           box(background = 'black', width = 10, 
                               h4('This data comes from Organization for Economic Cooperation and Development. It consists of useful information about 
                                  percent of health spending, perecent of GDP, and US Dollars per capita for specific countries.'),
                               h3('Variables Include:'),
                               h4("location = Location by Country Code"),
                               h4('time = Year'), 
                               h4('perHealthSpend = Percent of Health Spending'), 
                               h4('percGDP = Perecent of GDP'), 
                               h4('USDperCap = in US Dollars per Capita (using economy-wide PPPs'), 
                               h4('total= Total spending in millions'),
                               h4("For more info"), h4(a("click here", href="https://data.oecd.org/healthres/pharmaceutical-spending.htm"))
                               )
                    )
                    
                  )
                  ),
#---------------------------------------2nd Tab-----------------------------------------#
                tabItem(tabName = 'datasum', fluidPage(
                  withMathJax(),
                  pageWithSidebar(
                    headerPanel('Data Summaries and Grahps'),
                    sidebarPanel(
                      selectInput('xval1', 'X variable', c('time','perHealthSpend','percGDP','USDperCap','total')),
                       selectInput('yval1', 'Y variable', c('time','perHealthSpend','percGDP','USDperCap','total'),
                              selected='total'), 
                      downloadButton("save", "Download Data"),
                 
                  
                  
                  checkboxGroupInput('avgs', "Select to find the means", 
                                     choiceNames= c('Percent of Health Spending','Percent of GPD','US Dollars per Capita','Total'), 
                                     choiceValues = round(c(mean(pharmData$perHealthSpend),
                                                      mean(pharmData$percGDP),
                                                      mean(pharmData$USDperCap),
                                                     mean(pharmData$total)),2)), 
                 
                  verbatimTextOutput('means'), h2("$$\\frac{\\sum x}{n}$$")
                  ),
                  mainPanel(
                    plotlyOutput('datasumm'), DTOutput('table1'))
                  )
                  )
                  ),
#--------------------------------------3rd Tab-------------------------------------------#
                  tabItem(tabName = 'clus',   fluidPage(
  
                      pageWithSidebar(
                        headerPanel('Pharmaceutical k-means clustering'),
                        sidebarPanel(
                          selectInput('graph', "Type of Graph", c('biplot', 'k-means clustering',  'Dendogram')),
                          conditionalPanel(
                          condition = "input.graph=='k-means clustering'", 
                              selectInput('xcol', 'X Variable', c('time','perHealthSpend','percGDP','USDperCap','total')),
                              selectInput('ycol', 'Y Variable', c('time','perHealthSpend','percGDP','USDperCap','total'),
                                      selected='total'),
                              numericInput('clusters', 'Cluster count', 3,
                                       min = 1, max = 9)),
                          conditionalPanel(
                            condition = "input.graph=='biplot'", 
                            selectInput('xval2', 'Variable 1', c('time','perHealthSpend','percGDP','USDperCap','total')),
                            selectInput('yval2', 'Variable 2', c('time','perHealthSpend','percGDP','USDperCap','total'),
                                        selected='total'),
                            sliderInput("cex", "CEX",
                                        min = 0, max = 5, value = 2, step = 0.2)),
                          conditionalPanel(
                            condition = "input.graph=='Dendogram'",
                            selectInput('xval3', 'Variable 1', c('time','perHealthSpend','percGDP','USDperCap','total')),
                            selectInput('yval3', 'Variable 2', c('time','perHealthSpend','percGDP','USDperCap','total'), 
                                        selected='total'))
        
                        ),
                        mainPanel(
                          plotOutput('cluspg')
                        )
                      ))), 
#--------------------------------------------4th Tab--------------------------------#
tabItem(tabName = "model", fluidPage(
  pageWithSidebar(
    headerPanel('Pharmaceutical Modeling'),
    sidebarPanel(
      selectInput('modeltype', 'What type of model would you like', c('Linear', 'Bagged Fit', 'Random Forest')),
      conditionalPanel(condition="input.modeltype=='Linear'", 
                       selectInput('linemod', 'What type of fit?', c('Interaction','Main Effect','All Possible Combinations' )), 
                       checkboxInput("pred", "Would you like to make a Prediction?", value=FALSE), 
                       conditionalPanel(condition="input.pred==true", 
                                        numericInput('value1', "PerHealthSpend Value", 10), 
                                        numericInput('value2', "percGDP value", 10), 
                                        numericInput('value3', "USDperCap Value", 10))) 
      
      
      
    ), 
    mainPanel(h3('Model Data'), 
              tableOutput('fit'), 
              h3("Prediction Output for Total"), h5('Prediction will not show for bagged Trees and Random Forest'),
              textOutput('prediction'))
    
))), 

#--------------------------------------5th Tab-----------------------------------------------#
tabItem(tabName = "dataset", fluidPage(
  pageWithSidebar(
    headerPanel('Pharmaceutical Data Tables'),
    sidebarPanel(
      selectInput('filter', 'Select Which Country You would Like to see?', c('AUS', 'AUT', 'BEL', 'CAN', 'CHE', 'CZE', 'DEU', 'DNK', 'ESP', 'EST', 'FIN', 'FRA', 'GBR', 'GRC', 
                                                                             'HUN', 'IRL', 'ISL', 'ISR' ,'ITA' ,'JPN' ,'KOR', 'LTU', 'LUX', 'LVA', 'MEX', 'NLD', 'NOR', 'NZL', 
                                                                             'POL', 'PRT', 'RUS', 'SVK', 'SVN', 'SWE', 'TUR', 'USA'))
        ), 
    mainPanel(h3('Data Table'), 
              tableOutput('country')
             )
    
  )))
                


)))



















