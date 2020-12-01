library(shiny)
library(shinythemes)
library(magrittr)

originData <- read.csv("2020urldata.csv")

ui <- fluidPage(
    
    theme = shinytheme("cyborg"),
    titlePanel("Presidential Election Forecast"),
    
    sidebarLayout(
        sidebarPanel(
            uiOutput("GetDataButton"),
            uiOutput("MakeGraphButton"),
            uiOutput("InputShiftNum")
        ),
        
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Select",
                                 fluidRow(
                                     column(1,uiOutput("Party1")),
                                     column(1,uiOutput("Party2")),
                                     column(1,uiOutput("Party3")),
                                     column(1,uiOutput("Party4")),
                                     column(1,uiOutput("Party5")),
                                     column(1,uiOutput("Party6")),
                                     column(1,uiOutput("Party7")),
                                     column(1,uiOutput("Party8")),
                                     column(1,uiOutput("Party9")),
                                     column(1,uiOutput("Party10")),
                                     column(1,uiOutput("Party11")),
                                     column(1,uiOutput("Party12"))
                                 ),
                                 fluidRow(
                                     column(1,uiOutput("Party13")),
                                     column(1,uiOutput("Party14")),
                                     column(1,uiOutput("Party15")),
                                     column(1,uiOutput("Party16")),
                                     column(1,uiOutput("Party17")),
                                     column(1,uiOutput("Party18")),
                                     column(1,uiOutput("Party19")),
                                     column(1,uiOutput("Party20")),
                                     column(1,uiOutput("Party21")),
                                     column(1,uiOutput("Party22")),
                                     column(1,uiOutput("Party23")),
                                     column(1,uiOutput("Party24"))
                                 ),
                                 fluidRow(
                                     column(1,uiOutput("Party25")),
                                     column(1,uiOutput("Party26")),
                                     column(1,uiOutput("Party27")),
                                     column(1,uiOutput("Party28")),
                                     column(1,uiOutput("Party29")),
                                     column(1,uiOutput("Party30")),
                                     column(1,uiOutput("Party31")),
                                     column(1,uiOutput("Party32")),
                                     column(1,uiOutput("Party33")),
                                     column(1,uiOutput("Party34")),
                                     column(1,uiOutput("Party35")),
                                     column(1,uiOutput("Party36"))
                                 ),
                                 fluidRow(
                                     column(1,uiOutput("Party37")),
                                     column(1,uiOutput("Party38")),
                                     column(1,uiOutput("Party39")),
                                     column(1,uiOutput("Party40")),
                                     column(1,uiOutput("Party41")),
                                     column(1,uiOutput("Party42")),
                                     column(1,uiOutput("Party43")),
                                     column(1,uiOutput("Party44")),
                                     column(1,uiOutput("Party45")),
                                     column(1,uiOutput("Party46")),
                                     column(1,uiOutput("Party47")),
                                     column(1,uiOutput("Party48"))
                                 ),
                                 fluidRow(
                                     column(1,uiOutput("Party49")),
                                     column(1,uiOutput("Party50")),
                                     column(1,uiOutput("Party51")),
                                     column(1,uiOutput("Party52")),
                                     column(1,uiOutput("Party53")),
                                     column(1,uiOutput("Party54")),
                                     column(6)
                                 )
                                 ),
                        tabPanel("Result",
                                 uiOutput("BidenWinPercent"),
                                 plotOutput("HistGram")
                                 )
                        )
        )
    )
)

server <- function(input, output) {
   # functions
    MakeAllParty <- function(x) { lapply(1:54, function(x) MakeParty(x,originData$num2[x],originData$sName[x])) }
    MakeParty <- function(x,supportingParty,stateName){
        output[[paste0("Party",x)]] <- renderUI({
            radioButtons(paste0("party",x), label = h5(stateName),
                         # choices = list("Republican" = 1, "Democratic" = 2,"Other" = 3),selected = supportingParty)
                         choices = list("R" = 1, "D" = 2,"O" = 3),selected = supportingParty) # 共和党 民主党 その他
        })
    }
    
    MakeAllParty()
    output$GetDataButton <- renderUI({ actionButton("getDataButton",label = "Get Data") })
    output$MakeGraphButton <- renderUI({ actionButton("makeGraphButton",label = "Make Graph") })
    output$InputShiftNum <- renderUI({ sliderInput("inputShiftNum",label = h3("Shift Num"),min=-10,max=10,value = 0) })
    
    MakeGraph <- function(){
        tmp_num <- lapply(1:54,function(x){ input[[paste0("party",x)]] }) %>% unlist() %>% as.numeric()
        shiftNum <- input$inputShiftNum
        
        df <- read.csv("2020ave.csv")
        row_sum <- df$Biden + df$Trump
        Biden_data <- df$Biden * 100 / row_sum
        Biden_data[47 < Biden_data & Biden_data < 53] <- 50
        Biden_data[tmp_num == 2] <- 100
        Biden_data[tmp_num == 1] <- 0
        Biden_data[Biden_data != 100 & Biden_data != 0] <- Biden_data[Biden_data != 100 & Biden_data != 0] - shiftNum
        print(Biden_data)
        
        BidenGetNum <- lapply(1:10000,function(x){
            eachBidenGetNum <- lapply(1:length(Biden_data),function(y){
                ifelse(100 * runif(1,0,1) < Biden_data[y] ,originData$num1[y],0) %>% return()
            }) %>% unlist()
            return(sum(eachBidenGetNum))
        }) %>% unlist()
        
        output$HistGram <- renderPlot({hist(BidenGetNum,breaks = c(seq(100,500,length=41)))})
        output$BidenWinPercent <- renderUI({
            BidenGetNum[BidenGetNum > 269] %>% length() -> my_tmp
            my_tmp <- my_tmp / 100
            h6(paste0("Percent of Biden's victory : ",my_tmp,"%"))
        })
    }
    
    observeEvent(input$getDataButton,{ source("makeAveList.R") })
    observeEvent(input$makeGraphButton,{ MakeGraph() })
}

shinyApp(ui = ui, server = server)