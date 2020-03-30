library(shiny)

ui <- fluidPage(
  titlePanel("Développement du coronoavirus au vu d'indicateurs macro-économiques"),
  fluidRow(
    radioButtons(inputId = 'df_covid_used',label = 'Date de début des données',
                           choices = names(list_covid),
                           selected = 'à partir de 10 décès')
  ),
  fluidRow(
    column(4, 'Choisissez un indicateur macro',
           uiOutput('indic_macro')),
    column(8, 'Indicateur macro',
           plotOutput('plot_macro'))
  ),
  
  fluidRow(
    column(3, 'Choisissez une mesure du coronavirus', 
          uiOutput('df_covid')),
    column(8,
           "Comparaison de l'évolution du virus",
           plotOutput('plot_covid'))
  )
)

server <- function(input, output, session) {
  #select df 10 morts ou 1 cas
  df_covid <- reactive({
    df_covid <- list_covid[[input$df_covid_used]]
    df_covid
  })

  #plot_macro 
  plot_covid <- reactive({
    df_covid() %>%
      ggplot() + 
      geom_line(aes( x = as.numeric(covid_daynr), y = !!as.name(input$variable_covid), col= country_code)) +
      theme(panel.grid.major = element_blank(), panel.background = element_blank(), legend.key = element_blank())
  })
  plot_macro <- reactive({
    df_covid() %>%
      ggplot()+
      geom_bar(aes(x= factor(country_code), y = !!as.name(input$indic_macro), fill = country_code), 
               position = 'dodge', stat = 'identity') +
      theme(panel.grid.major = element_blank(), panel.background = element_blank())
    
  })
  
  output$df_covid = 
    renderUI({
      varSelectInput('variable_covid', label= 'Mesure choisie', 
                     data = df_covid() %>% 
                       select (cum_cases, cum_deaths, ln_cases, ln_deaths,
                               death_case_ratio,  case_100khab, death_100khab),
                     multiple = F)
    })
  output$indic_macro=
    renderUI({
      varSelectInput('indic_macro',label = 'Indicateur choisi',
                     data = df_covid() %>%
                       select(eldly_pop,pop,gdp_pc,HEALTHEXP,nb_nurse,internet,nb_doc),
                     multiple=F)
    })
  output$plot_covid <- renderPlot({plot_covid()})
  output$plot_macro <- renderPlot({plot_macro()})
}

shinyApp(ui, server)


