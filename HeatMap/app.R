library(shiny)
library(httr)
library(dplyr)
library(jsonlite)

####################################### PObranie ID kampanii na koncie 

urlKampanie <-'http://api.semstorm.com/api-v3/monitoring/monitoring-campaign/get-list.json'

postDaneKampanii <- POST(urlKampanie, content_type("application/json"), body = list(
    pager = list(items_per_page = 100),
    services_token = "rAM1Tm7joQPLAXNeMXwewkjij_9fxjh-6kEaUTMhPo4"
), encode = c("json"))

GET_DaneKampanii <-postDaneKampanii %>% content("text") %>% fromJSON(flatten = F)

ListaKampani <- c()

ListaKampani$ID <- GET_DaneKampanii$results$campaigns$id
ListaKampani$Nazwa <- GET_DaneKampanii$results$campaigns$title

ListaKampani <- as.data.frame(ListaKampani)

##################################

ui <- fluidPage(
    h3("Pozycje za dzień"), 
    selectInput("wyb_kamp", "Wybierz kampanie", ListaKampani$Nazwa),
    #tableOutput("get1"),
    selectInput("serwis", "Dla akiego serwisu dane ?", c('pracuj.pl', 'olx.pl')),
    tableOutput("final")
)


############ Serwer
server <- function(input, output, session) {

observe({
        req(input$wyb_kamp)        
kamp_wyb1 <- as.numeric( (subset(ListaKampani,Nazwa == input$wyb_kamp, ID )) )
output$kamp_wyb <- renderPrint({kamp_wyb1})
output$serwis_wyb <- renderPrint({input$serwis})
serwis_wyb1 <- input$serwis


##################

output$final <- renderTable(
    {
        
        #############
        Final <- as.data.frame(Final)
        Final <- subset(Final, Domena == serwis_wyb1)
        Final <- Final%>%left_join(GET_DaneSlowa, by = c("Fraza" = "results.keywords.id"))
        Final <- Final[,c(8,2,3)]
        #############
        
        return(Final)
    }
)

################################ pobranie danych heatmap dla fraz w wybranej kampanii

#urlKampanie <-'http://api.semstorm.com/api-v3/monitoring/monitoring-campaign/get-list.json'
urlSlowa <- 'http://api.semstorm.com/api-v3/monitoring/monitoring-keyword/get-list.json'

postDaneSlowa <- POST(urlSlowa, content_type("application/json"), body = list(
    services_token = "rAM1Tm7joQPLAXNeMXwewkjij_9fxjh-6kEaUTMhPo4",
    campaign_id = kamp_wyb1,
    pager = list(items_per_page = 100)
), encode = c("json"))

GET_DaneSlowa <-postDaneSlowa %>% content("text") %>% fromJSON(flatten = F)

GET_DaneSlowa <- as.data.frame(GET_DaneSlowa)

################################ pobranie pozycji dla fraz w kampanii 

urlHeatmap <- 'http://api.semstorm.com/api-v3/monitoring/monitoring-keyword/get-details.json'
slowaHeatMap <- c()
slowaHeatMapFinal <- c()
postSlowaHeatMap <- c()
Final <- c()
licz <- 1

for (b in 1:length(GET_DaneSlowa$results.keywords.id))
    
{
    postSlowaHeatMap <- POST(urlHeatmap, content_type("application/json"), body = list(
        services_token = "rAM1Tm7joQPLAXNeMXwewkjij_9fxjh-6kEaUTMhPo4",
        id = GET_DaneSlowa$results.keywords.id[b],
        type = "heatmap",
        datemin = "20200825",
        datemax = "20200825",
        gap = "daily"
    ), encode = c("json"))
    
    GET_SlowaHeatMap <-postSlowaHeatMap %>% content("text") %>% fromJSON(flatten = F)
    
    # 
    for (a in 1: length(GET_SlowaHeatMap[["results"]][[1]][[1]])) {
        print(a)
        slowaHeatMap$Fraza[a] <- names(GET_SlowaHeatMap[["results"]][1])
        Final$Fraza[licz] <-  names(GET_SlowaHeatMap[["results"]][1])
        
        if (length(GET_SlowaHeatMap[["results"]][[1]][[1]][[a]][[1]][["position"]])!= 0) {
            Final$Pozycja[licz] <- GET_SlowaHeatMap[["results"]][[1]][[1]][[a]][[1]][["position"]]} else {Final$Pozycja[licz] <- 0}
        
        Final$Domena[licz] <- names(GET_SlowaHeatMap[["results"]][[1]][[1]][a])
        
        licz <- licz +1
    }

}




##################
#print(kamp_wyb1)
#print(GET_DaneSlowa)
})
}

shinyApp(ui, server)