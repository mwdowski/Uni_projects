library("shiny")
library("dplyr")
library("stringi")
library("plotly")

Activity <- read.csv("AnimeActivity.csv")

Activity %>%
    distinct(TagName) -> TagList

Activity %>%
    mutate(Month=stri_pad(Month, width=2, side="left", pad="0")) %>%
    mutate(Date=stri_paste(Year, Month, sep="-")) -> Activity

Activity %>% 
    distinct(Date) %>%
    select(Date2=Date) %>%
    arrange(Date2) -> Dates

ui <- fluidPage(
    
    titlePanel("Wykres aktywności tagów"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId="ChosenTag", label="Wybierz tagi z listy", choices=TagList, multiple = TRUE),
            width=2
        ),
        
        mainPanel(
            plotlyOutput(outputId="acPlot"),
            width=10
        )
    )
    
)

server <- function(input, output){
    
    output$acPlot <- renderPlotly({
        
        if(length(input$ChosenTag)>1){
            Results <- as.data.frame(Dates)
            colnames(Results)[1] <- "Date"
            
            for(i in (1:length(input$ChosenTag))){
                Activity %>%
                    filter(TagName==input$ChosenTag[i]) %>%
                    group_by(Year, Month, Date) %>%
                    summarise(Count=n()) %>%
                    right_join(Dates, by=c("Date"="Date2")) %>%
                    select(Date, Count) %>%
                    arrange(Date) -> data
                data[is.na(data$Count), "Count"] <- 0
                Results <- cbind(Results, data$Count)
                colnames(Results)[i+1] <- input$ChosenTag[i]
            }
            print(head(Results, 10))
            
            p <- plot_ly(x=Results$Date, y=Results[, 2], type="scatter", mode="lines", name=input$ChosenTag[1])
            for(i in (2:length(input$ChosenTag))){
                p <- add_trace(p, x=Results$Date, y=Results[,i+1], type="scatter", mode="lines", inherit=FALSE, name=input$ChosenTag[i])
                }
            p
        }
        else{
            
            Activity %>%
                filter(TagName==input$ChosenTag) %>%
                group_by(Year, Month, Date) %>%
                summarise(Count=n()) %>%
                right_join(Dates, by=c("Date"="Date2")) %>%
                select(Date, Count) %>%
                arrange(Date) -> data
            data[is.na(data$Count), "Count"] <- 0
            p <- plot_ly(x=data$Date, y=data$Count, type="scatter", mode="lines", name=input$ChosenTag)
        }
    })
}

shinyApp(ui=ui, server=server)