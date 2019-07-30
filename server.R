library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(rgl)
library(plotly)
library(readr)
library(readxl)

pharmData<-read_csv("C:/Users/pcobb/Desktop/FinalProject/data.csv")
names(pharmData)<-c('location','time','perHealthSpend','percGDP','USDperCap','flag','total')


shinyServer(
  
  function(input, output, session) {
    
#------------------------------------Data Summaries Page-------------------------------------#
    
    
output$datasumm <- renderPlotly({ 
  selectedData <- as.data.frame(pharmData[, c(input$xval1 , input$yval1)])
  #ggplot(selectedData, aes(x=selectedData[,1], y=selectedData[,2])) + geom_point() + labs(x=input$xval1, y=input$yval1)
  
  x<-list(
    title = input$xval1)
    y <- list(
      title = input$yval1)
  
  plot_ly(data=selectedData, x= ~selectedData[,1], y= ~selectedData[,2], type = 'scatter') %>% layout(xaxis=x, yaxis=y)
  
 })    
  
output$table1<-renderDT(pharmData[, c(input$xval1 , input$yval1)])


saveData<-reactive({pharmData[, c(input$xcol, input$ycol)]})

output$save <- downloadHandler(
  filename = function() {
    paste(input$xval1,input$yval1, ".csv", sep = "")
  },
  content = function(file) {
   write.csv(saveData(), file, row.names = FALSE)}
)

output$means<-renderText({ paste(input$avgs, collapse=",")
  
  })

  

  

 #---------------------------Cluster/PCA page---------------------------------------------------#   
     output$cluspg<-renderPlot({
      if(input$graph=="k-means clustering"){
      
      selectedData <- pharmData[, c(input$xcol, input$ycol)]
      clusters <- kmeans(selectedData, input$clusters)
      
        palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3","#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
        par(mar = c(5.1, 4.1, 0, 1))
        plot(selectedData,
             col = clusters$cluster,
             pch = 20, cex = 3)
        points(clusters$centers, pch = 4, cex = 4, lwd = 4)
    }
     else if(input$graph=="biplot"){
      PCs<-prcomp(select(pharmData, input$xval2, input$yval2), scale = TRUE)
      
      biplot(PCs, xlabs=rep(".", nrow(pharmData)), cex=input$cex)}
      
      else{ selectedData <- pharmData[, c(input$xval3, input$yval3)]
      hierClust<-hclust(dist(selectedData))
      plot(hierClust, xlab='')
      }
      
      
    })
#------------------------------------Models Page---------------------------------# 
     pharmData1<-select(pharmData, -flag)
     pharmData1 <- filter(pharmData1, !is.na(location) & !is.na(time) & !is.na(perHealthSpend)& !is.na(percGDP)& !is.na(USDperCap)& !is.na(total))
     
     compareFitStats <- function(fit1, fit2){
       require(MuMIn)
       fitStats <- data.frame(fitStat = c("Adj R Square", "AIC", "AICc", "BIC"),
                              col1 = round(c(summary(fit1)$adj.r.squared, AIC(fit1), 
                                             MuMIn::AICc(fit1), BIC(fit1)), 3),
                              col2 = round(c(summary(fit2)$adj.r.squared, AIC(fit2), 
                                             MuMIn::AICc(fit2), BIC(fit2)), 3))
       #put names on returned df
       calls <- as.list(match.call())
       calls[[1]] <- NULL
       names(fitStats)[2:3] <- unlist(calls)
       fitStats}
     
     set.seed(111)
     
     train <- sample(1:nrow(pharmData1), size = nrow(pharmData1)*0.8)
     test <- dplyr::setdiff(1:nrow(pharmData1), train)
     
     dataTrain <- pharmData1[train, ]
     dataTest <- pharmData1[test, ]
     dataTrain2<-select(dataTrain, -location)
     dataTest2<-select(dataTest, -location)
     
     fit<-lm(total ~ perHealthSpend:percGDP:USDperCap, pharmData)
    
     fit2<-lm(total~ perHealthSpend + percGDP + USDperCap, pharmData)
   
     fit3<-lm(total ~ perHealthSpend * percGDP * USDperCap, pharmData )
    
     bagfit<-randomForest(total ~ .,data=dataTrain2, method = treebag)
    
     rfFit<-randomForest(total ~ ., data=dataTrain2, mtry=round(sqrt(ncol(dataTrain)),0), ntree=100, importance=TRUE)
  
     
     #3 interaction variables
     output$fit<-renderTable({
       
       #c('Interaction','Main Effect','All Possible Combinations' )
     if(input$modeltype=="Linear"){
         if(input$linemod=="Interaction"){
        fit<-lm(total ~ perHealthSpend:percGDP:USDperCap, pharmData1)
        table1<-as.data.frame(fit$coefficients)
        table1 }
         else if(input$linemod=="Main Effect"){
         fit<-lm(total~ perHealthSpend + percGDP + USDperCap, pharmData1)
         table1<-as.data.frame(fit$coefficients)
         table1 }
        else if(input$linemod=="All Possible Combinations"){
        fit<-lm(total ~ perHealthSpend * percGDP * USDperCap, pharmData1 )
        table1<-as.data.frame(fit$coefficients)
        table1 }}
      else if(input$modeltype=="Bagged Fit"){
         dataTrain2<-select(dataTrain, -location)
         dataTest2<-select(dataTest, -location)
         #bagged tree
         bagfit<-randomForest(total ~ .,data=dataTrain2, method = treebag)
         #creates the prediction data frame
         bagPred<-predict(bagfit, newdata = select(dataTest2, -total))
         bagRMSE<-sqrt(mean((bagPred-dataTest2$total)^2))
         c("Root Mean Squared Error", bagRMSE)}
       else if(input$modeltype=="Random Forest"){
         #randomforest
         rfFit<-randomForest(total ~ ., data=dataTrain2, mtry=round(sqrt(ncol(dataTrain)),0), ntree=100, importance=TRUE)
         rfPred<-predict(rfFit, newdata = select(dataTest2, -total))
         rfRMSE<-sqrt(mean((rfPred-dataTest2$total)^2))
         c("Root Mean Squared Error", rfRMSE)}
       
      },rownames=TRUE, colnames=TRUE)
     
     output$prediction<- renderText({if(input$pred==TRUE){
       if(input$modeltype=="Linear" & input$linemod=='Interaction'){ x<-as.data.frame(cbind(input$value1,input$value2,input$value3))
       names(x)<-c('perHealthSpend','percGDP','USDperCap')
       predict(fit, x)}
       else if(input$modeltype=="Linear" & input$linemod=='Main Effect'){ x<-as.data.frame(cbind(input$value1,input$value2,input$value3))
       names(x)<-c('perHealthSpend','percGDP','USDperCap')
       predict(fit2, x)}
       else if(input$modeltype=="Linear" & input$linemod=='All Possible Combinations'){ x<-as.data.frame(cbind(input$value1,input$value2,input$value3))
       names(x)<-c('perHealthSpend','percGDP','USDperCap')
       predict(fit3, x)}
      }
       
       
       
     })
     
  #---------------------------------------------------Data Page--------------------------------------------------#   
     
    output$country<-renderTable({
      pharmData %>% filter(location==input$filter)
    })
    
     
     
     
     
     
     
  }
)
