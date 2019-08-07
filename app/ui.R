


# Objetivo
#'        - Criar uma interface em shiny para exibir os indicadores calculados.



# 1. User interface -------------------------------------------------------

## Secao correspondente a interface que o usuario visualizara

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML(".navbar .navbar-nav {float: left}
                    .navbar .navbar-header {float: right}"))),
  
  
  title = "CEPESP Indicadores", ## Titulo da pagina do aplicativo em versao web
  
  navbarPage(id = "CepespIndicadores",theme = shinytheme("flatly"),
             
             
            
             tags$div(class = "header", checked = NA,
                      
                      
                      
                      tags$a(href = "http://www.cepesp.io/cepesp-data/", class = 
                               "ir-cepesp-data w-hidden-medium w-hidden-small w-hidden-tiny" ,
                             "CEPESP Data", ## Link que redireciona para
                             style =        ## a pagina do CEPESP Data
                               "top: -2px;
                             background-color:white;
                             border-bottom-color:#1897d5;
                             right: 0;
                             width: 170px;
                             hover: background:#1897d5;
                             padding: 10px 15px; 
                             padding-top: 10px;
                             padding-right: 15px;
                             padding-bottom: 10px;
                             padding-left: 15px;
                             border-style: none solid solid;
                             border-width: 0 1px 1px;
                             border-radius: 0 0 3px 3px;
                             border-color: #1897d5;
                             border-image-source: initial;
                             border-image-slice: initial;
                             border-image-width: initial;
                             border-image-outset: initial;
                             border-image-repeat: initial;
                             text-align: center;
                             vertical-align: middle;
                             text-decoration: none;
                             text-decoration-line: none;
                             text-decoration-style: initial;
                             text-decoration-color: initial;
                             user-select: none;
                             border: 1px solid transparent;
                             color: #1897d5;
                             font-size: 14px;
                             font-weight: bold;
                             line-height: 12px;
                             text-transform: uppercase;
                             display: inline-block!important;
                             display: flex;
                             flex-direction: column;
                             padding-left: 0;
                             margin-bottom: 0;
                             font-family: Gotham,Open Sans,sans-serif!important;
                             box-sizing: border-box;
                             transition-property: color, border-color, box-shadow;
                             transition-duration: 0.15s, 0.15s, 0.15s, 0.15s;
                             -timing-function: ease-in-out, ease-in-out, ease-in-out, ease-in-out;
                             transition-delay: 0s, 0s, 0s, 0s;
                             webkit-tap-highlight-color: rgba(0,0,0,0);
                             list-style: none;
                             list-style-type: none;
                             list-style-position: initial;
                             list-style-image: initial"
                             
                      )),
             
             
             
             tabPanel("Distribuição de cadeiras", ## Definicao das ferramentas de selecao para a guia
                                                  ## "Distribuicao de cadeiras"
                      
                      sidebarLayout(
                        
                        sidebarPanel(h4("Opções:"),width = 3,
                                     
                                     
                                     selectizeInput(inputId = "INDICADORES_DISTR",
                                                    label = NULL, 
                                                    choices = c("","Quociente eleitoral", "Quociente partidário"), ## Indicadores 
                                                    selected = NULL,                                               ## disponiveis
                                                    options = list(placeholder = 'Escolha um indicador')),
                                     
                                     selectizeInput(inputId = "DESCRICAO_CARGO1",
                                                    label = NULL,
                                                    choices = c("","Deputado Federal", "Deputado Estadual"), ## Cargos 
                                                    selected = NULL,                                         ## disponiveis
                                                    options = list(placeholder = 'Escolha um cargo')),
                                     
                                     selectizeInput(inputId = "UF",
                                                    label = NULL,
                                                    choices = c("", "Todas UFs", "AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", 
                                                                "GO", "MA", "MG","MS", "MT", "PA", "PB", "PE", "PI", "PR", 
                                                                "RJ", "RN", "RO", "RR","RS", "SC", "SE", "SP", "TO"),
                                                    selected =  NULL,
                                                    options = list(placeholder = 'Escolha uma UF')),
                                     
                                     
                                     actionButton(inputId = "BCALC1",
                                                  label = strong("Calcular"), ## Botao de acao "Calcular"
                                                  width = "95%")
                                     
                        ),
                        
                        
                        mainPanel(
                          
                          absolutePanel(top = 0, right = 0, left = 100,
                                        tabsetPanel(type = "pills", 
                                                    tabPanel("Tabelas", br(),
                                                             DT::dataTableOutput("quoce_fed"), ## Tabelas que serao exibidas
                                                             DT::dataTableOutput("quoce_est"),
                                                             DT::dataTableOutput("quocp_fed"),
                                                             DT::dataTableOutput("quocp_est")),
                                                   tabPanel("Dados agregados", br(),
                                                            DT::dataTableOutput("agreg_quocefed"),
                                                            DT::dataTableOutput("agreg_quoceest"),
                                                            DT::dataTableOutput("agreg_quocpfed"),
                                                            DT::dataTableOutput("agreg_quocpest")),
                                                   tabPanel("Definição", htmlOutput("def_distc"))))))), ## Definicao dos indicadores  
             
             tabPanel("Fragmentação legislativa",  ## Definicao das ferramentas de selecao para a guia
                                                   ## "Fragmentacao legislativa"
                      
                      
                      sidebarLayout(
                        
                        sidebarPanel(h4("Opções:"),width = 3,
                                     
                                     
                                     selectizeInput(inputId = "INDICADORES_FRAG",
                                                    label = NULL, 
                                                    choices = c("","Desproporcionalidade de gallagher","Fracionalização", 
                                                                "Fracionalização máxima", ## Indicadores disponiveis
                                                                "Fragmentação", "Número efetivo de partidos por cadeiras",
                                                                "Número efetivo de partidos por votos"),
                                                    selected = NULL,
                                                    options = list(placeholder = 'Escolha um indicador')),
                                     
                                     selectizeInput(inputId = "DESCRICAO_CARGO2",
                                                    label = NULL,
                                                    choices = c("","Deputado Federal", "Deputado Estadual"), ## Cargos disponiveis
                                                    selected = NULL,
                                                    options = list(placeholder = 'Escolha um cargo')),
                                     
                                     selectizeInput(inputId = "AGREGACAO_REGIONAL2",
                                                    label = NULL,
                                                    choices = c("","Brasil"),
                                                    selected = NULL,
                                                    options = list(placeholder = 'Escolha uma agregação regional')),
                                     
                                     uiOutput("UF2"),
                                     
                                     
                                     actionButton(inputId = "BCALC2",
                                                  label = strong("Calcular"), ## Botao de acao calcular
                                                  width = "95%")
                                     
                                     
                        ),
                        
                        mainPanel(
                          
                          absolutePanel(top = 0, right = 0, left = 100,
                                        tabsetPanel(type = "pills",
                                                    tabPanel("Tabelas", br(),
                                                             DT::dataTableOutput("dpg_fed"),
                                                             DT::dataTableOutput("fracio_fed"),## Tabelas que serao exibidas
                                                             DT::dataTableOutput("fraciomax_fed"),
                                                             DT::dataTableOutput("frag_fed"),
                                                             DT::dataTableOutput("nepc_fed"),
                                                             DT::dataTableOutput("nepc_est"),
                                                             DT::dataTableOutput("nepv_fed")),
                                                   tabPanel("Dados agregados", br(),
                                                            DT::dataTableOutput("agreg_dpgfed"),
                                                            DT::dataTableOutput("agreg_fracfed"),
                                                            DT::dataTableOutput("agreg_fracmaxfed"),
                                                            DT::dataTableOutput("agreg_fragfed"),
                                                            DT::dataTableOutput("agreg_nepfed"),
                                                            DT::dataTableOutput("agreg_nepest"),
                                                            DT::dataTableOutput("agreg_nepvfed")),  
                                                   tabPanel("Definição", htmlOutput("def_frag"))))))), ## Definicao dos indicadores
             
             tabPanel("Renovação parlamentar",  ## Definicao das ferramentas de selecao para a guia
                      ## "Renovação parlamentar"
                      
                      
                      sidebarLayout(
                        
                        sidebarPanel(h4("Opções"),width = 3,
                                     
                                     
                                     selectizeInput(inputId = "INDICADORES_RENOV",
                                                    label = NULL, 
                                                    choices = c("","Conservação", "Renovação bruta",
                                                                "Renovação líquida", "Volatilidade eleitoral"), ## Indicadores disponiveis
                                                    selected = NULL,
                                                    options = list(placeholder = 'Escolha um indicador')),
                                     
                                     selectizeInput(inputId = "DESCRICAO_CARGO3",
                                                    label = NULL,
                                                    choices = c("","Deputado Federal", ## Cargos disponiveis
                                                                "Deputado Estadual"),
                                                    selected = NULL,
                                                    options = list(placeholder = 'Escolha um cargo')),
                                     
                                     selectizeInput(inputId = "AGREGACAO_REGIONAL3",
                                                    label = NULL,
                                                    choices = c("","Brasil", "UF"),
                                                    selected = NULL,
                                                    options = list(placeholder = 'Escolha uma agregação regional')),
                                     
                                     
                                     uiOutput("UF3"),
                                     
                                     
                                     actionButton(inputId = "BCALC3",
                                                  label = strong("Calcular"), ## Botao de acao "Calcular"
                                                  width = "95%")
                                     
                                     
                        ),
                        
                        mainPanel(
                          
                          absolutePanel(top = 0, right = 0, left = 100,
                                        tabsetPanel(type = "pills",
                                                    tabPanel("Tabelas", br(), 
                                                             DT::dataTableOutput("conserv"),
                                                             DT::dataTableOutput("renov_br"),
                                                             DT::dataTableOutput("renov_liq"),
                                                             DT::dataTableOutput("vol_ele")), ## Tabelas que serao exibidas
                                                             tabPanel("Dados agregados", br(), 
                                                             DT::dataTableOutput("agreg_conserv"),
                                                             DT::dataTableOutput("agreg_renov_br"),
                                                             DT::dataTableOutput("agreg_renov_liq"),
                                                             DT::dataTableOutput("agreg_vol_ele")),
                                                    tabPanel("Definição", htmlOutput(""))))))),
             
             tabPanel("Alienação",  ## Definicao das ferramentas de selecao para a guia
                                    ## "Alienacao"
                      
                      
                      sidebarLayout(
                        
                        sidebarPanel(h4("Opções"),width = 3,
                                     
                                     
                                     selectizeInput(inputId = "INDICADORES_ALIE",
                                                    label = NULL, 
                                                    choices = c("","Alienação Absoluta", "Alienação Percentual"), ## Indicadores disponiveis
                                                    selected = NULL,
                                                    options = list(placeholder = 'Escolha um indicador')),
                                     
                                     selectizeInput(inputId = "DESCRICAO_CARGO4",
                                                    label = NULL,
                                                    choices = c("","Presidente", "Governador", "Senador", "Deputado Federal", ## Cargos disponiveis
                                                                "Deputado Estadual"),
                                                    selected = NULL,
                                                    options = list(placeholder = 'Escolha um cargo')),
                                     
                                     selectizeInput(inputId = "AGREGACAO_REGIONAL4",
                                                    label = NULL,
                                                    choices = c("","Brasil", "UF"),
                                                    selected = NULL,
                                                    options = list(placeholder = 'Escolha uma agregação regional')),
                                     
                                     
                                     uiOutput("UF4"),
                                     
                                     
                                     actionButton(inputId = "BCALC4",
                                                  label = strong("Calcular"), ## Botao de acao "Calcular"
                                                  width = "95%")
                                     
                                     
                        ),
                        
                        mainPanel(
                          
                          absolutePanel(top = 0, right = 0, left = 100,
                                        tabsetPanel(type = "pills",
                                                    tabPanel("Tabelas", br(), 
                                                             DT::dataTableOutput("alien_feda_br"), ## Tabelas que serao exibidas
                                                             DT::dataTableOutput("alien_fedp_br"), 
                                                             DT::dataTableOutput("alien_feda_uf"), 
                                                             DT::dataTableOutput("alien_fedp_uf")),
                                                    tabPanel("Dados agregados", br(),
                                                             DT::dataTableOutput("agreg_alifeda_br"),
                                                             DT::dataTableOutput("agreg_alifeda_uf"),
                                                             DT::dataTableOutput("agreg_alifedp_br"),
                                                             DT::dataTableOutput("agreg_alifedp_uf")),
                                                    tabPanel("Definição", htmlOutput("def_alien"))))))), ## Definicao dos indicadores
             
             
             
             
             tabPanel("Sobre", htmlOutput("sobre")), ## Guia correspondente ao "Sobre"
             
             
             tags$footer(class = "rodape",
                         
                         style =
                           
                           "max-width: 100%;
                         noprint: none; 
                         padding: 10px 0;
                         min-height: 40px;
                         position: relative;
                         clear: both;
                         background-color: #222d32;;
                         color: #fff;
                         font-family: 'Segoe UI';
                         font-size: 14px;
                         text-align: left;
                         z-index: 10;
                         height: 3em;
                         margin-top: 41em;",
                         
                         tags$div(class = "rodape-container",
                                  
                                  style =
                                    
                                  "max-width: 960px;
                                  margin: 0 auto;
                                  position: relative;
                                  display: flex;
                                  flex-wrap: wrap;
                                  box-sizing: border-box;
                                  padding: 0;",
                                  
                                  
                                  tags$div(class = "rodape-texto", "© 2019 CEPESP Todos os direitos reservados.",
                                           
                                           style = 
                                             
                                             "
                                           max-width: 50%;
                                           align: left;
                                           flex: 1 1 200px;
                                           display: flex;
                                           padding-left: 5%;
                                           padding-top: 10px;
                                           font-size: .9em;
                                           box-sizing: border-box;
                                           margin: 0;
                                           padding: 0;")))
             
             
             
  ))

