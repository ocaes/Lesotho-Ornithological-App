#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button 
#
library(shiny)
library(DT)
library(bslib)
library(dplyr)
library(ggplot2)
library(ggridges)
library(graphics)
library(leaflet)
library(raster)
library(sf)
library(shinythemes)


nonyana<-read.csv("Lesotho Species.csv")


# Define UI for application
ui <- fluidPage(
  theme = shinytheme("superhero"),
  # Application title
  titlePanel("MyLINONYANA"),
  
  
  # Sidebar with a sidebar panel
  sidebarLayout(
    sidebarPanel(
      selectInput("vars_g", "Group by e.g. (species,decimalLon,decimalLat,locality,stateProvi etc simultaneously) or you can select one variable", names(nonyana), multiple =
                    TRUE),
      selectInput("vars_s", "Summarise.(select which variable(s) to summarise)", names(nonyana), multiple =
                    TRUE),
      selectInput(inputId = "BirdViz",label = "Bird Data Visualization(Taxa with most avifaunal sightings and based on levels of taxonomic hierarchy)",choices = c("Family Frequency","Order Frequency","Bird Genus Frequency","Bird Species Frequency"),selected = "Family Frequency"),
      checkboxInput("TaxaStas",label = "How many birds are there in each taxon? Tick here",value = FALSE,width = '100%'),
      checkboxInput("BirdSPList",label = "Bird Species Checklist",value = FALSE,width = '100%'),
      checkboxInput("AviMap",label = "Lesotho Avifaunal Map:Be patient while web map is loading. click on the dot to view a bird species",value = FALSE)
      ),
    
    
    
    # Show an output on the main panel
    mainPanel(
      textOutput("AppInfo"),
      verbatimTextOutput("TaxaInfo"),
      textOutput("AviInfo"),
      verbatimTextOutput("speciesList"),
      tableOutput("summary"),
      plotOutput("dataviz"),
      leafletOutput("BirdMapViz"),
      textOutput("DesignerInfo")
      
      
    )
  )
)


# Define server logic required for app functions
server <- function(input, output, session) {
  output$AppInfo<-renderText({
    a<-"This MyLINONYANA App uses bird species data acquired from Global Biodiversity Information Facility(GBIF).It shows bird species sightings in Lesotho.Explore the app in various ways.Use various input controls on the sidebar panel to explore different bird species in the Kingdom of Lesotho.This app has been developed for taxonomists, nature conservationists,environmentalists,environmental scientists,bird enthusiasists,bird watchers,ornithologists,environmental impact assessors and anyone interested in citizen bird science."
    a
  })
  
  
  output$TaxaInfo<-renderPrint({
    if(input$TaxaStas == TRUE) {
    UF<-length(unique(nonyana$family))
    UO<-length(unique(nonyana$order))
    UG<-length(unique(nonyana$genus))
    U_S<-length(unique(nonyana$species))
    
    paste0("Unique number of bird families,orders,genuses and species:" ,UF,",",UO,",",UG, " and " ,U_S," ", "respectively,according to this dataset.")}
  })
  
  output$speciesList<-renderPrint({
    if(input$BirdSPList == TRUE) {
    #LIST OF LESOTHO BIRD SPECIES IN ASCENDING ORDER
    b<-sort(unique(nonyana$acceptedSc))
    paste0(b)}
  })
  
  output$summary <- renderTable({
    nonyana %>%
      group_by(across(all_of(input$vars_g))) %>%
      summarise(across(all_of(input$vars_s), mean), n = n())
  })
  
  output$dataviz = renderPlot({
    
    if(input$BirdViz == "Family Frequency") {
      z<-nonyana %>%
        group_by(stateProvi,family) %>%
        summarise(count=n()) %>%
        mutate(freq=count/sum(count)) %>%
        filter(count>2) %>%
        ggplot(aes(x=reorder(family, -freq),y=freq)) + geom_bar(stat="identity", fill= "red") +theme_bw(base_size=11) +
        theme(axis.text.x = element_text(angle= 90, hjust=0)) + xlab("Lesotho Bird Families") + ylab("Frequency")+facet_wrap(~stateProvi, ncol=2)}
    
    
    if(input$BirdViz == "Order Frequency") {
      z<-nonyana %>%
        group_by(stateProvi,order) %>%
        summarise(count=n()) %>%
        mutate(freq=count/sum(count)) %>%
        filter(count>2) %>%
        ggplot(aes(x=reorder(order, -freq),y=freq)) + geom_bar(stat="identity", fill= "tomato") +theme_bw(base_size=10) +
        theme(axis.text.x = element_text(angle= 90, hjust=0)) + xlab("Lesotho Bird Orders") + ylab("Frequency")+facet_wrap(~stateProvi, ncol=2)}
    
    if(input$BirdViz == "Bird Genus Frequency") {
      z<-nonyana %>%
        group_by(stateProvi,genus) %>%
        summarise(count=n()) %>%
        mutate(freq=count/sum(count)) %>%
        filter(count>20) %>%
        ggplot(aes(x=reorder(genus, -freq),y=freq)) + geom_bar(stat="identity", fill= "purple") +theme_bw(base_size=10) +
        theme(axis.text.x = element_text(angle= 90, hjust=0)) + xlab("Lesotho Bird Genuses") + ylab("Frequency")+facet_wrap(~stateProvi, ncol=2)}
    
    if(input$BirdViz == "Bird Species Frequency") {
      z<-nonyana %>%
        group_by(stateProvi,species) %>%
        summarise(count=n()) %>%
        mutate(freq=count/sum(count)) %>%
        filter(count>50) %>%
        ggplot(aes(x=reorder(species, -freq),y=freq)) + geom_bar(stat="identity", fill= "seagreen") +theme_bw(base_size=12) +
        theme(axis.text.x = element_text(angle= 90, hjust=0)) + xlab("Lesotho Bird Species") + ylab("Frequency")+facet_wrap(~stateProvi, ncol=2)}
    z
  
    })
  output$BirdMapViz <- renderLeaflet({
    if(input$AviMap == TRUE) {
    nonyana %>%
        leaflet() %>%
        addTiles()%>%
        #addTiles("https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G") %>%
        addCircleMarkers(lng = ~decimalLon, lat = ~decimalLat,stroke = TRUE, color = "seagreen", popup = ~species, labelOptions = labelOptions(noHide = TRUE))}
  })
  
  output$DesignerInfo<-renderText({
    print("©2023 OCAES. Designed by TSITSO MAFANTIRI, Mohale`s Hoek`,Lesotho,Southern Africa. www.ocaes.weebly.com. www.heylink.me/tsitsomafantiri. www.tsitsomafantiri.blogspot.com")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
