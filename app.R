#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(ggplot2)
library(plotly)
library(maps)
library(mapdata)
#library(maptools)
library(sf)
library(dplyr)
library(stringr)
library(shiny)
library(openxlsx)

BJ2 <- sf::st_read(".\\ShinyApp\\BJ.json")
sites <- read.xlsx('.\\ShinyApp\\Location.xlsx',sheet = 1)
for (i in 1:nrow(sites)){
  if (is.na(sites$Restaurant[i]))
    sites$Restaurant[i] <- sites$Restaurant[i-1]
}
sites$YN[which(sites$Type=='地标')] <- 2
sites$YN[is.na(sites$YN)] <- 0

###相同店，合并美食记录和是否去过的记录
for (j in unique(sites[duplicated(sites$Restaurant),]$Restaurant)){
  sites$Food[which(sites$Restaurant==j)] <- str_c(sites$Food[which(sites$Restaurant==j&sites$Food!='NA')],collapse = '，')#把一个字符串多个向量参数拼接为大字符串
  sites$YN[which(sites$Restaurant==j)] <- max(sites$YN[which(sites$Restaurant==j)])
  sites$Type[which(sites$Restaurant==j)] <-  sort(sites$Type[which(sites$Restaurant==j)])[1]
}
sites$YN[is.na(sites$Food)] <- ''
sites <- subset(sites,sites$loc!='NA')
sites$X <- str_split_fixed(sites$loc,',',2)[,1]
sites$Y <- str_split_fixed(sites$loc,',',2)[,2]
sites <- st_as_sf(sites, coords = c("X", "Y"), 
                  crs = 4326, agr = "constant")
TypeList <- c(unique(na.omit(sites$Type)))
TypeList <- c('地标',TypeList[TypeList!='地标'])

# Define UI for application that draws a histogram
# User interface ----
ui <- fluidPage(
  titlePanel("DQ's 美食地图 v2.0.3"),
  
  sidebarLayout(
    sidebarPanel(
      #helpText("选择美食类型"),
      
      textInput("searchText", 
                        label = "搜索菜品", value=''),
      
      
      checkboxGroupInput("typ", 
                         "选择种类",
                         TypeList,
                         selected = TypeList),
      actionLink("selectall","全选/取消全选"),
      width=3
    ),
    
    mainPanel(
      p('更新说明：补充了推荐店铺'),
      br(),
      strong('这是由DQ和他的股东们共同完成的美食地图，不定期更新。'),
      p('地图可通过框选放大，双击缩小，点击地图边界上方会出现具体工具栏。交互使用电脑端体验更佳。'),
      p('左侧搜索框可以用于搜索招牌菜品，例如：“饺子”、“馅饼”。选项栏可以选择不同食品种类。'),
      #p('店铺颜色代表DQ去/没去过此餐厅，也欢迎大家一同见证美食地图的拓宽！'),
      p('由于现有数据量较少，欢迎大家作为项目股东，积极向DQ本人投稿，投稿需含有',
        span("店铺名称", style = "color:blue"),
        '；尽可能还能包括',
        span("推荐菜品", style = "color:blue"),
        '；考虑到后续可能更新评分机制，可以附加',
        span("10分制评分", style = "color:blue"),
        '。'),
      p('如果你不能直接联系到DQ，可以通过发送以上内容至deulofeu@foxmail.com入股'),
      
      
      plotlyOutput("map"),
      tableOutput("table"))
  )
)

server <- function(input, output, session) {
  
  observe({
    if(input$selectall == 0) return(NULL) 
    else if (input$selectall%%2 == 0)
    {
      updateCheckboxGroupInput(session,"typ","选择食品种类",choices=TypeList)
    }
    else
    {
      updateCheckboxGroupInput(session,"typ","选择食品种类",choices=TypeList,selected=TypeList)
    }
  })
  
  datatable <- reactive({
    data <- subset(sites,Type %in% input$typ)
    
    if(input$searchText!='')
      data <- na.omit(data[str_detect(data$Food,input$searchText),])
    data
  })
  
  output$map <- renderPlotly({

    gg2 <- ggplotly(ggplot(BJ2) + 
                      #geom_sf(aes(fill=as.factor(num)),alpha=0.5)+
                      #scale_fill_hue(c=10)+
                      geom_sf(fill='#ffefde',alpha=0.5,aes(text=paste('District',name)))+
                      geom_sf(data=datatable(), aes(fill=as.factor(YN),color=as.factor(YN),text=paste('Restaurant:',Restaurant,'</br>Food:',Food,'</br>Type:',Type)),size = 1.5,shape=4) +
                      coord_sf(xlim = c(115.4, 117.6), ylim = c(39.4, 41.2), expand = FALSE)+
                      geom_sf_text(data = datatable(), aes(label = Restaurant,col=as.factor(YN)), 
                                   size = 3, fontface = "bold",nudge_y = -0.005,check_overlap=T)+
                      guides(fill='none',shape='none',col='none')+
                      theme_bw())
  })
  
  output$table <- renderTable({
    tab <- data.frame('类型'=datatable()$Type,'店铺'=datatable()$Restaurant,'推荐菜'=datatable()$Food)
    tab %>% unique() %>% arrange(factor(类型, levels = TypeList))
  },align = 'ccc',width = 'auto')
}

# Run the application 
shinyApp(ui = ui, server = server)
