#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(fs)
library(dplyr)
#i=1
#vector_list <- list(agust$mon,agust$tue,agust$wed,agust$thur,agust$fri,agust$sat,agust$sun)
#students <- c()
#while (i <= 6) {
#  temp <- union(vector_list[[i]],vector_list[[i+1]]) %>% na.omit()
#  student_temp <- c(temp,students) %>% unique()
#  students <- student_temp
#  i = i+1
#}
# 管红林

files <- list.files("data/") 


for (file in files){
  read_excel(file)
}
# Jan；Feb；Mar；Apr； May； Jun；Jul； Aug；Sept；Oct；Nov；Dec
normal_students <- c( "王子怡","管红琳","王刘宇","薛雨杭","张萌","葛睿",
                      "周雪榕","郭益欣","付晓研","刘彤","焦宇","王爽","杜宇昂",
                      "刘文晗" ,"丁心","张露丹","孟祥月","郑焱华")
new_cols <- c("type1","mon","tue","wed","thur","fri","sat","sun","type2")
weekday_type_commm <- c("主院夜班","主院连班","层流夜班","浑南夜班","浑南连班","骨穿班")
weekend_type_commm <- c("主院白班","层流白班","浑南夜班","浑南白班","层流夜班","主院夜班")

get_hospital_timetalbe <- function(dir,new_cols = new_cols){
  aguddd <- read_excel(dir)
  month_data <- str_sub(dir,1,nchar(dir)-5)
  colnames(aguddd) <- new_cols
  agust_clean <- aguddd %>% 
    filter(!is.na(type1) | !is.na(type2) ) %>% 
    filter(type2 != "日期" ) %>% 
    filter(type2 != "值班") %>% 
    mutate(weekday_type = ifelse(is.na(type1)& !is.na(type2),type2,type1)) %>% 
    mutate(weekend_type = ifelse(is.na(type2)& !is.na(type1),type1,type2)) %>%
    mutate(weekend_type = ifelse(weekend_type == "骨穿班","主院白班",weekend_type)) %>% 
    select(-type1) %>% 
    relocate(weekday_type) %>% 
    select(-type2) %>% 
    mutate(month = month_data)
}

Batch_hospital_table <- function(root,new_cols = new_cols){
  files <- list.files(root)
  dflist <- list()
  for (i in 1:length(files)) {
    dflist[[i]] <- get_hospital_timetalbe(paste0(root,files[i]),
                                          new_cols = new_cols)
  }
  df_total = data.table::rbindlist(dflist,fill = TRUE)
}

get_name_score <- function(df,name = "王子怡",mod = c("weekday","weekend"),type){
  if (mod == "weekday"){
    df <- df %>% 
      select(-all_of(c("sat","sun","weekend_type"))) %>% 
      filter(weekday_type == type)
    #row_level <- levels(factor(df$weekday_type))
    
  }else{
    df <- df %>% 
      select(all_of(c("weekend_type","sat","sun"))) %>% 
      filter(weekend_type == type)
  }
  for (i in 1:ncol(df)){
    score = 0
    for (a in 1:nrow(df)) {
      if (str_detect(df[a,i],name)){
        score_temp = score + 1
        score = score_temp
      } else {
        next
      }
    }
  }
  return(score)
}





# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
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
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
