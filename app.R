packageList = c('shiny', 'ggplot2', 'reshape2','dplyr','tidyr','stringr')
for (i in packageList) {
  if(!(i %in% rownames(installed.packages()))) {
    install.packages(packageList[i])
  }
}
library(shiny)
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(stringr)

#setwd('/Users/linda/msan622/lesson4/')

expect <- read.csv("API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv", skip = 4, header = TRUE) %>%
  gather('Year',Expectancy,X1960:X,na.rm=TRUE) %>%
  mutate(Year = as.integer(str_extract(Year, "\\d+"))) %>%
  select(c(Country.Name,Country.Code,Year,Expectancy))

fert <- read.csv("API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv", skip = 4, header = TRUE) %>%
  gather('Year',Fertility,X1960:X,na.rm=TRUE) %>%
  mutate(Year = as.integer(str_extract(Year, "\\d+"))) %>%
  select(c(Country.Code,Year,Fertility))

pop <- read.csv('API_SP.POP.TOTL_DS2_en_csv_v2.csv',skip = 4, header = TRUE) %>%
  gather('Year',Population,X1960:X,na.rm=TRUE) %>%
  mutate(Year = as.integer(str_extract(Year, "\\d+"))) %>%
  select(c(Country.Code,Year,Population))

region<- read.csv('Metadata_Country_API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv')

d <- expect  %>% 
  inner_join(fert,by=c('Country.Code','Year')) %>%
  inner_join(pop,by = c('Country.Code','Year')) %>%
  inner_join(region[c('Country.Code','Region')],by='Country.Code') %>%
  filter(Region!="")


shinyUI<-fluidPage(
  # Application title
  titlePanel("Motion Chart by Linda Liu"),
  
  sidebarLayout(
    mainPanel(    
      div(
        style = "position:relative",
        plotOutput(outputId="motionchart",width = "100%",
                   hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")
        ),
        uiOutput("hover_info")
      ),width=7
    ),
    sidebarPanel(
      sliderInput("theYear", "Year", 1960, 2014, 1960, step = 1, 
                  animate=animationOptions(interval=1200, loop = F,
                                           playButton = NULL, pauseButton = NULL)),
      checkboxGroupInput('theRegion', label= 'Region',
                         choices = c("Latin America & Caribbean"="Latin America & Caribbean",
                                     "South Asia" = "South Asia","Sub-Saharan Africa"="Sub-Saharan Africa",
                                     "Europe & Central Asia"="Europe & Central Asia", 
                                     "Middle East & North Africa"="Middle East & North Africa", 
                                     "North America"="North America","East Asia & Pacific"="East Asia & Pacific"))
      )
    )
)

shinyServer<- function(input, output){
  anigraph<-reactive({
    #select data
    a <- d[d$Year==input$theYear,]
    if (is.null(input$theRegion)){
      p <-ggplot(a,aes(x=Expectancy,y=Fertility,frame=Year))+
        geom_point(aes(size = Population, color = Region), alpha = 0.7)
    }else{
      p <-ggplot(a,aes(x=Expectancy,y=Fertility,frame=Year))+
        geom_point(data=a[a$Region%in%input$theRegion,],aes(size = Population, color = Region), alpha =1)+
        geom_point(data=a[!a$Region%in%input$theRegion,],aes(size = Population,color = Region),alpha=0.3)
      
    }
    
    p <- p + xlim(10,90)+ylim(0,10)+xlab("Life expectancy") +ylab("Fertility rate") 
    p <- p +scale_size_area(guide = FALSE, max_size = 15) +
      scale_color_brewer(name = "", palette = "Set1") +
      theme(legend.position='bottom')
    
    p
    
  })
  output$motionchart <- renderPlot({
    anigraph()
  })
  
  ##find this via https://gitlab.com/snippets/16220
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    data<-d[d$Year==input$theYear,]
    point <- nearPoints(data, hover, threshold = 5, maxpoints = 5, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Country Name: </b>", point$Country.Name, "<br/>",
                    "<b> Fertility: </b>", point$Fertility, "<br/>",
                    "<b> Life Expectancy: </b>", point$Expectancy, "<br/>",
                    "<b>Population </b>", point$Population,"<br/>")))
    )
  })
  
  
}

   
shinyApp(list(ui=shinyUI,server=shinyServer))
