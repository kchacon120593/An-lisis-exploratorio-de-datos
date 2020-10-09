install.packages("tidyverse")
install.packages("shiny")
install.packages("RColorBrewer")


#Se cargan las librerías a utilizar

library(shiny)
library(tidyverse)
library(RColorBrewer)


#Se ingiere el dataset
fao <- read.csv("C:/Users/kchacon/Desktop/WebApp/FAO.csv")


#Se colapsan las columnas "Year"
df <- gather(fao, id=c("Y1961","Y1962","Y1963","Y1964","Y1965","Y1966","Y1967","Y1968","Y1969","Y1970","Y1971","Y1972","Y1973","Y1974","Y1975","Y1976","Y1977","Y1978","Y1979","Y1980","Y1981","Y1982","Y1983","Y1984","Y1985","Y1986","Y1987","Y1988","Y1989","Y1990","Y1991","Y1992","Y1993","Y1994","Y1995","Y1996","Y1997","Y1998","Y1999","Y2000","Y2001","Y2002","Y2003","Y2004","Y2005","Y2006","Y2007","Y2008","Y2009","Y2010","Y2011","Y2012","Y2013"))

#Se eliminan columnas que no se utilizarán
df_clean <- select(df, c(-"Area.Code", -"Item.Code",-"Element.Code",-"Unit"))

#Se cambia nombre de la columna "key" por "Year"
colnames(df_clean)
df_clean <- rename(df_clean, Year=key, Production_1000_tonnes=value)


#Se genera un groupby con los datos agrupados que se utilizarán en el app
df <- group_by(df_clean,Area,Item,Element) %>%
  summarise(Production_1000_tonnes=sum(Production_1000_tonnes,na.rm=T))%>%
  arrange(Area,desc(Production_1000_tonnes))


#*****************Se crea app con Shiny*****************


#Se crea la interfaz de usuario

ui <- fluidPage(title="Producción de alimentos por país",
                
                selectInput("area",
                            "Area:",
                            c("Seleccione un país",
                              unique(as.character(df$Area)))),
                
                tabsetPanel(
                  tabPanel("Información",dataTableOutput(outputId = "Table")),
                  
                  tabPanel("Distribución de Food vs Feed",
                           plotOutput(outputId="FoodHist"),
                           plotOutput(outputId="FeedHist")),
                  
                  tabPanel("Alimentos más producidos",
                           plotOutput(outputId = "MayoresFood"),
                           plotOutput(outputId="MayoresFeed"))
                  
                  
                )
                
)

#Se especifica el funcionamiento de las funciones Input y Output

server <- function(input, output) {
  
  # Filtrado basado en la inputs de cada seleccion
  df_reative <- reactive(df[df$Area == input$area,])
  
  
  output$Table <- renderDataTable({
    
    if (input$area != "Seleccione un país") {
      df_reative() 
    }})
  
  
  output$FoodHist <- renderPlot({
    food_mask <- df_reative()$Element == "Food"
    df_reative <- df_reative()[food_mask,]
    
    hist(df_reative$Production_1000_tonnes, 
         breaks = 5, 
         main="Distribución de producción de Food", 
         xlab="Toneladas producidas", 
         ylab="Frecuencia",
         col=brewer.pal(n=5, "Purples"))
    
  })
  
  output$FeedHist <- renderPlot({
    feed_mask <- df_reative()$Element == "Feed"
    df_reative <- df_reative()[feed_mask,]
    
    hist(df_reative$Production_1000_tonnes, 
         main="Distribución de producción de Feed",
         breaks = 5,
         xlab="Toneladas producidas", 
         ylab="Frecuencia",
         col=brewer.pal(n=5, "Greens"))
    
  })  
  
  #Bar plots
  
  output$MayoresFood <- renderPlot({
    
    food_mask <- df_reative()$Element == "Food" 
    df_reative <- df_reative()[food_mask,]
    
    df_mayoresfood <- head(df_reative,5)
    
    barplot(height=df_mayoresfood$Production_1000_tonnes, names.arg =df_mayoresfood$Item,
            main="5 alimentos de mayor producción (Food)",
            xlab = "Artículos",
            ylab = "Toneladas producidas (*1000)",
            col=brewer.pal(n=5, "YlOrRd"))
    
    
  })
  
  
  output$MayoresFeed <- renderPlot({
    
    feed_mask <- df_reative()$Element == "Feed" 
    df_reative_feed <- df_reative()[feed_mask,]
    
    df_mayoresfeed <- head(df_reative_feed,5)
    
    barplot(height=df_mayoresfeed$Production_1000_tonnes, names.arg =df_mayoresfeed$Item,
            main = "5 alimentos de mayor producción (Feed)",
            xlab = "Artículos",
            ylab = "Toneladas producidas (*1000)",
            col=brewer.pal(n=5, "PuBuGn"))
    
    
  })
  
  
  
}
shinyApp(ui, server)



