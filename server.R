library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  dfoutput<-reactiveValues()
   output$eventtable<-renderDataTable({
    if(is.null(input$Columns)){
      return(NULL)
    } else if(input$update == 0){
      return(NULL)}
    isolate({
      if(!is.null(input$mydata$result)&&(input$mydata$result$totalRows=="0")){
        return(as.data.frame("No Query Results"))
      }
      querydata<-input$mydata$result$rows;
      if(!is.null(querydata)){
             pcols<<-as.numeric(input$Columns);
             nullToNA <- function(x) {
               x[sapply(x, is.null)] <- NA
               return(x)
             }
             non.null.list=vector("list", length(querydata))
             for(i in 1:length(querydata)){non.null.list[[i]]<-lapply(querydata[[i]]$f, nullToNA)}
             querydatavec=unlist(non.null.list)
             querydatamat=matrix(querydatavec, nrow=length(querydata), byrow=TRUE)
             df=as.data.frame(querydatamat)
             names(df)=c("Latitude", "Longitude", "Location", "Date", "Eventcode", "URL", "Avg Tone", "Actor 1", "Actor 2", "Number of Articles", "Event ID Code")
             dfoutput<<-df[,pcols];
             return(df[,pcols])
      } else {
        if(is.null(querydata)){
          return(as.data.frame(input$mydata$message))
        } 
      }
             })
  })
  
   output$downloadData <- downloadHandler(
     
       filename = function() {paste('data-', Sys.Date(), '.csv', sep='')}, content = function(file){write.csv(dfoutput, file)}

   )

 
  
  
})