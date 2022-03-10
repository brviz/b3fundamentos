# Consulta de fundamentos de empresas da B3
# Codado durante a disciplina de Financiamentos e Investimentos - Fatec Sebrae
# Junho 2021

library(shiny)
library(GetDFPData2)
library(dplyr)
library(tibble)
library(ggplot2)
library(shinycssloaders)  # withSpinner() 
options(scipen = 999)

# Empresas ativas
df_info <- get_info_companies() %>% 
  filter(SIT_REG == "ATIVO")

# Vetor para select de empresas
vetor_empresas <- df_info %>% 
  select(DENOM_SOCIAL, CD_CVM) %>% 
  deframe()

# Quantidade de empresas (ALGAR vem duplicada)
qt_empresas <- df_info %>% 
  distinct(CD_CVM) %>% 
  nrow()

# UI ----------------------------------------------------------------------

ui <- fluidPage(
  
  # Meta
  tags$head(includeHTML("www/google-analytics.html"),
            # CSS
            tags$style(HTML("
              #grafico {
                margin-top: 30px;
              }
            "))
            ),
  
  # Header
  titlePanel("B3 Fundamentos"),
  p(paste("Painel de consulta dos demonstrativos de resultados das",
          qt_empresas, "empresas listadas na B3.")),
  p("Selecione a empresa e aguarde o carregamento das contas."),
  
  # Select Empresa
  selectInput("empresa", "Empresa",
              c(`Selecione ou digite` = "", vetor_empresas), selectize = TRUE),
  
  # Select Conta
  uiOutput("select_conta") %>% 
    withSpinner(color = "#f8766d"),
  
  # Gráfico
  plotOutput("grafico"),
  
  # Créditos
  textOutput("creditos")
)

# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  # Coleta dados da empresa selecionada
  df <- reactive({
    req(input$empresa)
    data <- get_dfp_data(as.numeric(input$empresa),
                         type_docs = "DRE",  # Demonstrativo
                         type_format = "con",  # Consolidado do grupo
                         #cache_folder = paste0("cache/", input$empresa),
                         #use_memoise = TRUE,  # Considerar usar
                         do_shiny_progress = TRUE)
    data <- data$`DF Consolidado - Demonstração do Resultado`
  })
  
  # Renderiza select de contas da empresa selecionada
  output$select_conta <- renderUI({
    req(df())
    contas <- df() %>% 
      mutate(label_select = paste(CD_CONTA, DS_CONTA, sep = " - ")) %>% 
      select(label_select, CD_CONTA) %>% 
      # Converts the first column to names and second column to values
      deframe()  # https://stackoverflow.com/a/56479548/8549574
    selectInput("conta", "Conta", c(`Selecione ou digite` = "", contas))
  })
  
  # Gráfico da conta selecionada
  output$grafico <- renderPlot({
    req(df(), input$conta)
    df() %>%
      filter(CD_CONTA == input$conta) %>%
      ggplot(aes(y = VL_CONTA, x = DT_INI_EXERC)) +
      geom_col(fill = "#f8766d") +
      labs(x = "", y = "", title = paste0(
        first(df_info[df_info$CD_CVM == input$empresa, "DENOM_SOCIAL"]), " - ",
        first(df()[df()$CD_CONTA == input$conta, "DS_CONTA"]), " em ",
        first(df()[df()$CD_CONTA == input$conta, "MOEDA"]), " x ",
        first(df()[df()$CD_CONTA == input$conta, "ESCALA_MOEDA"]), " (Conta ",
        first(df()[df()$CD_CONTA == input$conta, "CD_CONTA"]), ")")) +
      scale_y_continuous(labels = function(x) format(x, big.mark = ".",
                                                     decimal.mark = ",")) +
      scale_x_date(date_breaks = "1 year", date_labels = ("%Y"),
                   minor_breaks = NULL) +
      geom_label(aes(label = format(round(VL_CONTA, 2),
                                    big.mark = ".", decimal.mark = ",")),
                 size = 3) +
      theme_minimal() +
      theme(plot.title = element_text(size = 15),
            axis.text = element_text(size = 12))
  })
  
  # Créditos
  output$creditos <- renderText({
    req(df(), input$conta)
    data <- paste("Dashboard por Júlio Boaro em R usando o pacote GetDFPData2",
                  "de Marcelo Perlin.")
  })
  
}

shinyApp(ui, server)
