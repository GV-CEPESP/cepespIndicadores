


# Objetivo
#'        - Criar uma interface em shiny para exibir os indicadores calculados.


# 1. User interface -------------------------------------------------------

## Secao correspondente a interface que o usuario visualizara

ui <- 
  
  fluidPage(
    
    useShinyjs(),
    
    
  tags$head(
    tags$style(HTML(".navbar .navbar-nav {float: left}
                    .navbar .navbar-header {float: right}"))),
  
 
  title = "CEPESP Indicadores", ## Titulo da pagina do aplicativo em versao web
  
  
    navbarPage(collapsible=F,id = "CepespIndicadores", 
             theme = shinytheme("flatly"), 
             
            
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
                             transition-property: color, border-color, box-shadow;"
                             
                      )),


# 1.1. Fragmentacao legislativa ---------------------------------------------

             
            
            tabPanel("Fragmentação legislativa", useShinydashboardPlus(),  
                      

# 1.1.1. SidebarLayout ----------------------------------------------------

                     
                     sidebarLayout( 
                        
                      
                        div(id ="Sidebar1",sidebarPanel(h5(align = "center","Faça sua consulta:"),width = 3,
                                                        
                                                       
                                                       
                                    selectizeInput(inputId = "INDICADORES_FRAG",
                                                    label = NULL, 
                                                    choices = c("", "Número efetivo de partidos eleitoral",
                                                                "Número efetivo de partidos legislativo","Fracionalização", 
                                                                "Fracionalização máxima",              ## Indicadores disponiveis
                                                                "Fragmentação", "Desproporcionalidade",
                                                                "Quociente eleitoral", "Quociente partidário"),
                                                    selected = NULL,
                                                    options = list(placeholder = 'Escolha um indicador')),
                                     
                                     uiOutput("DESCRICAO_CARGO1"),
                                     
                                     uiOutput("AGREGACAO_REGIONAL1"),
                                     
                                     uiOutput("UF1"),
                                    
                                     uiOutput("MUN1"),
                                    
                                     uiOutput("INT1"),
                                     
                                     h5(align = "center",
                                     actionButton(inputId = "BCALC1",
                                                  label = strong("Calcular"), ## Botao de acao calcular
                                                  width = "50%"))
                                     
                                     
                        )),
                        

# 1.1.2. MainPanel --------------------------------------------------------

                        
                        mainPanel(id = "Main1",
                          
                                         bsButton("showpanel1", 
                                         label = NULL, 
                                         icon = icon("bars"),
                                         type = "toggle", 
                                         value = TRUE),
                                  
                                 
                                
                                
                          absolutePanel(top = 0, right = 0, left = 65,
                                        
                                        absolutePanel(top = 0, right = 0, left = 260,
                                        
                                            actionBttn(inputId = "modal_frag",
                                                       color = "default",
                                                       icon = icon("question"), 
                                                       style = "material-circle",
                                                       size = "md")),
                                        
                                        
                                        tabsetPanel(type = "pills",


# 1.1.3. Resumo -----------------------------------------------------------

                                                    
                                                                                                        
                                                    tabPanel("Resumo", br(),
                                                             
                                                                 column(12,  
                                                                    absolutePanel(top = 0, 
                                                                                  right = 0 , 
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("nepl_br", width = "100%"))),
                                                                 column(12,
                                                                    absolutePanel(top = 0,
                                                                                  right = 0, 
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("nepl_uf", width = "100%"))),
                                                             
                                                                 column(12,
                                                                    absolutePanel(top = 0,
                                                                                  right = 0, 
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("nepl_mun", width = "100%"))),
                                                                 column(12,
                                                                    absolutePanel(top = 0,
                                                                                  right = 0, 
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("nepl_int", width = "100%"))),
                                                                 column(12,
                                                                    absolutePanel(top = 0, 
                                                                                  right = 0 ,  
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("nepel_br", width = "100%"))),
                                                                 column(12,
                                                                    absolutePanel(top = 0, 
                                                                                  right = 0,  
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("nepel_uf", width = "100%"))),
                                                                 column(12,
                                                                    absolutePanel(top = 0, 
                                                                                  right = 0 ,  
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("nepel_mun", width = "100%"))),
                                                                 column(12,
                                                                    absolutePanel(top = 0, 
                                                                                  right = 0 ,  
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("nepel_int", width = "100%"))),
                                                                 column(12,
                                                                        absolutePanel(top = 0,
                                                                                   right = 0 , 
                                                                                  left = 15,
                                                                                   DT::dataTableOutput("fracio_br", width = "100%"))),
                                                                 column(12,
                                                                        absolutePanel(top = 0,
                                                                                   right = 0 , 
                                                                                   left = 15,
                                                                                   DT::dataTableOutput("fracio_uf", width = "100%"))),
                                                                  column(12,
                                                                    absolutePanel(top = 0,
                                                                                  right = 0 , 
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("fracio_mun", width = "100%"))),
                                                                 column(12,
                                                                    absolutePanel(top = 0,
                                                                                  right = 0 , 
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("fracio_int", width = "100%"))),
                                                                  column(12,
                                                                        absolutePanel(top = 0,
                                                                                   right = 0 , 
                                                                                  left = 15,
                                                                                   DT::dataTableOutput("fracio_max_br", width = "100%"))),
                                                            
                                                                 column(12,
                                                                        absolutePanel(top = 0, 
                                                                                   right = 0 , 
                                                                                  left = 15,
                                                                                   DT::dataTableOutput("fracio_max_uf", width = "100%"))),
                                                             
                                                                 column(12,
                                                                    absolutePanel(top = 0, 
                                                                                  right = 0 , 
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("fracio_max_mun", width = "100%"))),
                                                                 column(12,
                                                                    absolutePanel(top = 0, 
                                                                                  right = 0 , 
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("fracio_max_int", width = "100%"))),
                                                                 column(12,
                                                                        absolutePanel(top = 0,
                                                                                   right = 0 , 
                                                                                  left = 15,
                                                                                   DT::dataTableOutput("frag_br", width = "100%"))),
                                                                 column(12,
                                                                        absolutePanel(top = 0,
                                                                                   right = 0 , 
                                                                                  left = 15,
                                                                                   DT::dataTableOutput("frag_uf", width = "100%"))),
                                                                 column(12,
                                                                    absolutePanel(top = 0,
                                                                                  right = 0 , 
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("frag_mun", width = "100%"))),
                                                                column(12,
                                                                    absolutePanel(top = 0,
                                                                                  right = 0 , 
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("frag_int", width = "100%"))),
                                                                 column(12,
                                                                    absolutePanel(top = 0, 
                                                                                  right = 0 ,  
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("dpg_br", width = "100%"))),
                                                                 column(12,
                                                                    absolutePanel(top = 0, 
                                                                                  right = 0 ,  
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("dpg_uf", width = "100%"))),
                                                                 column(12,
                                                                    absolutePanel(top = 0, 
                                                                                  right = 0 ,  
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("dpg_mun", width = "100%"))),
                                                                 column(12,
                                                                    absolutePanel(top = 0, 
                                                                                  right = 0 ,  
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("dpg_int", width = "100%"))),
                                                                 column(12,
                                                                    absolutePanel(top = 0,
                                                                                  right = 0 , 
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("quoce_br", width = "100%"))),                                                               
                                                                 column(12,
                                                                    absolutePanel(top = 0, 
                                                                                  right = 0 , 
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("quoce_uf", width = "100%"))),
                                                                  column(12,
                                                                    absolutePanel(top = 0, 
                                                                                  right = 0 , 
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("quoce_mun", width = "100%"))),
                                                                  column(12,
                                                                    absolutePanel(top = 0, 
                                                                                  right = 0 , 
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("quoce_int", width = "100%"))),
                                                                  column(12,
                                                                    absolutePanel(top = 0, 
                                                                                  right = 0 ,  
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("quocp_br", width = "100%"))),
                                                                  column(12,
                                                                    absolutePanel(top = 0, 
                                                                                  right = 0,  
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("quocp_uf", width = "100%"))),
                                                                  column(12,
                                                                    absolutePanel(top = 0, 
                                                                                  right = 0 ,  
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("quocp_mun", width = "100%"))),
                                                                                  
                                                                  column(12,
                                                                    absolutePanel(top = 0, 
                                                                                  right = 0 ,  
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("quocp_int", width = "100%")))),

# 1.1.4. Dados desagregados -----------------------------------------------

                                                                  
                                                                  
                                                  
                                                     tabPanel("Dados desagregados", br(),
                                                           
                                                                 column(12,
                                                                    absolutePanel(top = 0,
                                                                                 right = 0 , 
                                                                                 left = 15,
                                                                                 DT::dataTableOutput("agreg_nepl_br"))),
                                                                 column(12,
                                                                    absolutePanel(top = 0, 
                                                                                 right = 0 ,  
                                                                                 left = 15,
                                                                                 DT::dataTableOutput("agreg_nepl_uf"))),
                                                                 column(12,
                                                                   absolutePanel(top = 0, 
                                                                                 right = 0 ,  
                                                                                 left = 15,
                                                                                 DT::dataTableOutput("agreg_nepl_mun"))),
                                                                column(12,
                                                                     absolutePanel(top = 0, 
                                                                                   right = 0 ,  
                                                                                   left = 15,
                                                                                   DT::dataTableOutput("agreg_nepl_int"))),
                                                                column(12,
                                                                   absolutePanel(top = 0,
                                                                                 right = 0 ,  
                                                                                 left = 15,
                                                                                 DT::dataTableOutput("agreg_nepel_br"))),
                                                                column(12,
                                                                   absolutePanel(top = 0,
                                                                                 right = 0 , 
                                                                                 left = 15,
                                                                                 DT::dataTableOutput("agreg_nepel_uf"))),
                                                                column(12,
                                                                   absolutePanel(top = 0,
                                                                                 right = 0 , 
                                                                                 left = 15,
                                                                                 DT::dataTableOutput("agreg_nepel_mun"))),
                                                                column(12,
                                                                     absolutePanel(top = 0,
                                                                                   right = 0 , 
                                                                                   left = 15,
                                                                                   DT::dataTableOutput("agreg_nepel_int"))),
                                                                column(12,
                                                                       absolutePanel(top = 0,
                                                                                  right = 0 ,  
                                                                                 left = 15,
                                                                                  DT::dataTableOutput("agreg_fracio_br"))),
                                                                column(12,
                                                                       absolutePanel(top = 0,
                                                                                  right = 0 , 
                                                                                 left = 15,
                                                                                  DT::dataTableOutput("agreg_fracio_uf"))),
                                                                column(12,
                                                                   absolutePanel(top = 0,
                                                                                 right = 0 , 
                                                                                 left = 15,
                                                                                 DT::dataTableOutput("agreg_fracio_mun"))),
                                                                column(12,
                                                                     absolutePanel(top = 0,
                                                                                   right = 0 , 
                                                                                   left = 15,
                                                                                   DT::dataTableOutput("agreg_fracio_int"))),
                                                            
                                                                column(12,
                                                                       absolutePanel(top = 0, 
                                                                                  right = 0 , 
                                                                                 left = 15,
                                                                                  DT::dataTableOutput("agreg_fracio_max_br"))),
                                                                column(12,
                                                                       absolutePanel(top = 0,
                                                                                  right = 0 , 
                                                                                 left = 15,
                                                                                  DT::dataTableOutput("agreg_fracio_max_uf"))),
                                                                column(12,
                                                                   absolutePanel(top = 0,
                                                                                 right = 0 , 
                                                                                 left = 15,
                                                                                 DT::dataTableOutput("agreg_fracio_max_mun"))),
                                                                column(12,
                                                                     absolutePanel(top = 0,
                                                                                   right = 0 , 
                                                                                   left = 15,
                                                                                   DT::dataTableOutput("agreg_fracio_max_int"))),
                                                                column(12,
                                                                       absolutePanel(top = 0,
                                                                                  right = 0 , 
                                                                                 left = 15,
                                                                                  DT::dataTableOutput("agreg_frag_br"))),
                                                                column(12,
                                                                       absolutePanel(top = 0, 
                                                                                  right = 0 ,  
                                                                                 left = 15,
                                                                                  DT::dataTableOutput("agreg_frag_uf"))),
                                                                column(12,
                                                                   absolutePanel(top = 0, 
                                                                                 right = 0 ,  
                                                                                 left = 15,
                                                                                 DT::dataTableOutput("agreg_frag_mun"))),
                                                                column(12,
                                                                     absolutePanel(top = 0, 
                                                                                   right = 0 ,  
                                                                                   left = 15,
                                                                                   DT::dataTableOutput("agreg_frag_int"))),
                                                                column(12,
                                                                   absolutePanel(top = 0, 
                                                                                 right = 0 ,  
                                                                                 left = 15,
                                                                                 DT::dataTableOutput("agreg_dpg_br"))),
                                                                column(12,
                                                                   absolutePanel(top = 0, 
                                                                                 right = 0 , 
                                                                                 left = 15,
                                                                                 DT::dataTableOutput("agreg_dpg_uf"))),
                                                                column(12,
                                                                   absolutePanel(top = 0, 
                                                                                 right = 0 , 
                                                                                 left = 15,
                                                                                 DT::dataTableOutput("agreg_dpg_mun"))),
                                                                column(12,
                                                                     absolutePanel(top = 0, 
                                                                                   right = 0 , 
                                                                                   left = 15,
                                                                                   DT::dataTableOutput("agreg_dpg_int"))),
                                                                column(12,
                                                                   absolutePanel(top = 0, 
                                                                                 right = 0 ,  
                                                                                 left = 15,
                                                                                 DT::dataTableOutput("agreg_quoce_br"))),
                                                                column(12,
                                                                   absolutePanel(top = 0, 
                                                                                 right = 0 ,  
                                                                                 left = 15,
                                                                                 DT::dataTableOutput("agreg_quoce_uf"))),
                                                                column(12,
                                                                   absolutePanel(top = 0, 
                                                                                 right = 0 ,  
                                                                                 left = 15,
                                                                                 DT::dataTableOutput("agreg_quoce_mun"))),
                                                                column(12,
                                                                     absolutePanel(top = 0, 
                                                                                   right = 0 ,  
                                                                                   left = 15,
                                                                                   DT::dataTableOutput("agreg_quoce_int"))),
                                                                column(12,
                                                                   absolutePanel(top = 0, 
                                                                                 right = 0 ,  
                                                                                 left = 15,
                                                                                 DT::dataTableOutput("agreg_quocp_br"))),
                                                                column(12,
                                                                   absolutePanel(top = 0, 
                                                                                 right = 0 ,  
                                                                                 left = 15,
                                                                                 DT::dataTableOutput("agreg_quocp_uf"))),
                                                                column(12,
                                                                   absolutePanel(top = 0, 
                                                                                 right = 0 ,  
                                                                                 left = 15,
                                                                                 DT::dataTableOutput("agreg_quocp_mun"))),
                                                                                 
                                                                column(12,
                                                                   absolutePanel(top = 0, 
                                                                                 right = 0 ,  
                                                                                 left = 15,
                                                                                 DT::dataTableOutput("agreg_quocp_int"))))))))), 

# 1.2. Renovacao parlamentar ------------------------------------------------

          
             
             tabPanel("Renovação parlamentar",  ## Definicao das ferramentas de selecao para a guia
                      ## "Renovação parlamentar"
                      

# 1.2.1. SidebarLayout ----------------------------------------------------

                      
                      sidebarLayout(
                        
                        div(id ="Sidebar2",sidebarPanel(h5(align = "center","Faça sua consulta:"),width = 3,
                                     
                                     
                                     selectizeInput(inputId = "INDICADORES_RENOV",
                                                    label = NULL, 
                                                    choices = c("","Conservação", "Renovação bruta",
                                                                "Renovação líquida"), ## Indicadores disponiveis
                                                    selected = NULL,
                                                    options = list(placeholder = 'Escolha um indicador')),
                                     
                                     selectizeInput(inputId = "DESCRICAO_CARGO2",
                                                    label = NULL,
                                                    choices = c("","Deputado Federal", ## Cargos disponiveis
                                                                "Deputado Estadual",
                                                                "Vereador"),
                                                    selected = NULL,
                                                    options = list(placeholder = 'Escolha um cargo')),
                                     
                                     uiOutput("AGREGACAO_REGIONAL2"),
                                     
                                     uiOutput("UF2"),
                                     
                                     uiOutput("MUN2"),
                                     
                                     uiOutput("INT2"),
                                     
                                     h5(align = "center",
                                     actionButton(inputId = "BCALC2",
                                                  label = strong("Calcular"), ## Botao de acao "Calcular"
                                                  width = "50%"))
                                     
                                     
                        )),


# 1.2.2. MainPanel --------------------------------------------------------

                                                
                        mainPanel(id = "Main2",
                                  
                                  bsButton("showpanel2", 
                                           label = NULL, 
                                           icon = icon("bars"),
                                           type = "toggle", 
                                           value = TRUE),
                          
                         
                          absolutePanel(top = 0, right = 0, left = 65,
                                        
                                        absolutePanel(top = 0, right = 0, left = 260,
                                                      actionBttn(inputId = "modal_renovp",
                                                                 color = "default",
                                                                 icon = icon("question"), 
                                                                 style = "material-circle",
                                                                 size = "md")),
                                        tabsetPanel(type = "pills",
                                                    

# 1.2.3. Resumo -----------------------------------------------------------

                                                    
                                                         tabPanel("Resumo", br(),
                                                                 
                                                                 column(12,
                                                                    absolutePanel(top = 0, 
                                                                               right = 0 ,  
                                                                              left = 15,
                                                                               DT::dataTableOutput("conserv_br"))),
                                                                 column(12,
                                                                    absolutePanel(top = 0,
                                                                                 right = 0 ,  
                                                                                 left = 15,
                                                                                 DT::dataTableOutput("conserv_uf"))),
                                                                 column(12,
                                                                        absolutePanel(top = 0,
                                                                                      right = 0 ,  
                                                                                      left = 15,
                                                                                      DT::dataTableOutput("conserv_mun"))),
                                                                 column(12,
                                                                        absolutePanel(top = 0,
                                                                                      right = 0 ,  
                                                                                      left = 15,
                                                                                      DT::dataTableOutput("conserv_int"))),
                                                                 column(12,
                                                                    absolutePanel(top = 0, 
                                                                               right = 0 ,  
                                                                              left = 15,
                                                                               DT::dataTableOutput("renov_bt_br"))),
                                                                 column(12,
                                                                    absolutePanel(top = 0, 
                                                                               right = 0 ,  
                                                                              left = 15,
                                                                               DT::dataTableOutput("renov_bt_uf"))),
                                                                 column(12,
                                                                        absolutePanel(top = 0, 
                                                                                      right = 0 ,  
                                                                                      left = 15,
                                                                                      DT::dataTableOutput("renov_bt_mun"))),
                                                                 column(12,
                                                                        absolutePanel(top = 0, 
                                                                                      right = 0 ,  
                                                                                      left = 15,
                                                                                      DT::dataTableOutput("renov_bt_int"))),
                                                                 column(12,
                                                                    absolutePanel(top = 0, 
                                                                               right = 0 ,  
                                                                              left = 15,
                                                                               DT::dataTableOutput("renov_liq_br"))),
                                                                 column(12,
                                                                    absolutePanel(top = 0, 
                                                                               right = 0 ,  
                                                                              left = 15,
                                                                               DT::dataTableOutput("renov_liq_uf"))),
                                                                 column(12,
                                                                        absolutePanel(top = 0, 
                                                                                      right = 0 ,  
                                                                                      left = 15,
                                                                                      DT::dataTableOutput("renov_liq_mun"))),
                                                                 column(12,
                                                                        absolutePanel(top = 0, 
                                                                                      right = 0 ,  
                                                                                      left = 15,
                                                                                      DT::dataTableOutput("renov_liq_int")))), 
                                                    ## Tabelas que serao exibidas
                                                             tabPanel("Dados desagregados", br(),
                                                                      
                                                                      column(12,
                                                                             absolutePanel(top = 0, 
                                                                                        right = 0 ,  
                                                                                       left = 15,
                                                                                        DT::dataTableOutput("agreg_conserv_br"))),
                                                                          column(12,
                                                                             absolutePanel(top = 0, 
                                                                                        right = 0 ,  
                                                                                       left = 15,
                                                                                        DT::dataTableOutput("agreg_conserv_uf"))),
                                                                          column(12,
                                                                             absolutePanel(top = 0, 
                                                                                           right = 0 ,  
                                                                                           left = 15,
                                                                                           DT::dataTableOutput("agreg_conserv_mun"))),
                                                                          column(12,
                                                                             absolutePanel(top = 0, 
                                                                                           right = 0 ,  
                                                                                           left = 15,
                                                                                           DT::dataTableOutput("agreg_conserv_int"))),
                                                                          column(12,
                                                                             absolutePanel(top = 0,
                                                                                        right = 0 , 
                                                                                       left = 15,
                                                                                        DT::dataTableOutput("agreg_renov_bt_br"))),
                                                                          column(12,
                                                                             absolutePanel(top = 0, 
                                                                                        right = 0 , 
                                                                                       left = 15,
                                                                                        DT::dataTableOutput("agreg_renov_bt_uf"))),
                                                                          column(12,
                                                                             absolutePanel(top = 0, 
                                                                                           right = 0 , 
                                                                                           left = 15,
                                                                                           DT::dataTableOutput("agreg_renov_bt_mun"))),
                                                                          column(12,
                                                                             absolutePanel(top = 0, 
                                                                                           right = 0 , 
                                                                                           left = 15,
                                                                                           DT::dataTableOutput("agreg_renov_bt_int"))),
                                                                          column(12,
                                                                             absolutePanel(top = 0,
                                                                                        right = 0 , 
                                                                                       left = 15,
                                                                                        DT::dataTableOutput("agreg_renov_liq_br"))),
                                                                          column(12,
                                                                             absolutePanel(top = 0, 
                                                                                        right = 0 ,  
                                                                                       left = 15,
                                                                                        DT::dataTableOutput("agreg_renov_liq_uf"))),
                                                                      column(12,
                                                                             absolutePanel(top = 0, 
                                                                                           right = 0 ,  
                                                                                           left = 15,
                                                                                           DT::dataTableOutput("agreg_renov_liq_mun"))),
                                                                      column(12,
                                                                             absolutePanel(top = 0, 
                                                                                           right = 0 ,  
                                                                                           left = 15,
                                                                                           DT::dataTableOutput("agreg_renov_liq_int"))))))))),
             

# 1.3. Participacao e alienacao ---------------------------------------------

            
             
             tabPanel("Participação e Alienação",  ## Definicao das ferramentas de selecao para a guia
                                    ## "Alienacao"
                      
                      
                      sidebarLayout(
                        
                        div(id ="Sidebar3",sidebarPanel(h5(align = "center","Faça sua consulta:"),width = 3,
                                     
                                     
                                     selectizeInput(inputId = "INDICADORES_ALIE",
                                                    label = NULL, 
                                                    choices = c("","Alienação absoluta", "Alienação percentual",
                                                                "Abstenção absoluta","Abstenção percentual",
                                                                "Votos brancos absolutos", "Votos brancos percentuais",
                                                                "Votos nulos absolutos", "Votos nulos percentuais"), ## Indicadores disponiveis
                                                    selected = NULL,
                                                    options = list(placeholder = 'Escolha um indicador')),
                                     
                                     selectizeInput(inputId = "DESCRICAO_CARGO3",
                                                    label = NULL,
                                                    choices = c("","Presidente", "Governador", 
                                                                "Senador","Deputado Federal", ## Cargos disponiveis
                                                                "Deputado Estadual", "Prefeito", 
                                                                "Vereador"),
                                                    selected = NULL,
                                                    options = list(placeholder = 'Escolha um cargo')),
                                     
                                     uiOutput("AGREGACAO_REGIONAL3"), 
                                    
                                     uiOutput("UF3"),
                                     
                                     uiOutput("MUN3"),
                                     
                                     uiOutput("INT3"),
                                     
                                     h5(align = "center",
                                     actionButton(inputId = "BCALC3",
                                                  label = strong("Calcular"), ## Botao de acao "Calcular"
                                                  width = "50%"))
                                     
                                     
                        )),
                        
                        mainPanel(id = "Main3",
                                  
                                  bsButton("showpanel3", 
                                           label = NULL, 
                                           icon = icon("bars"),
                                           type = "toggle", 
                                           value = TRUE),
                          
                          
                          
                          absolutePanel(top = 0, right = 0, left = 65,
                                        
                                        absolutePanel(top = 0, right = 0, left = 260,
                                                      actionBttn(inputId = "modal_alien",
                                                                 color = "default",
                                                                 icon = icon("question"), 
                                                                 style = "material-circle",
                                                                 size = "md")),
                                        tabsetPanel(type = "pills",
                                                       tabPanel("Resumo", br(),
                                                               
                                                                   column(12,
                                                                    absolutePanel(top = 0, 
                                                                               right = 0 ,  
                                                                              left = 15,
                                                                               DT::dataTableOutput("alien_abs_br"))), ## Tabelas que serao exibidas
                                                                 column(12,
                                                                    absolutePanel(top = 0, 
                                                                               right = 0 ,  
                                                                              left = 15,
                                                                               DT::dataTableOutput("alien_abs_uf"))),
                                                                 column(12,
                                                                       absolutePanel(top = 0, 
                                                                                     right = 0 ,  
                                                                                     left = 15,
                                                                                     DT::dataTableOutput("alien_abs_mun"))),
                                                                  column(12,
                                                                       absolutePanel(top = 0, 
                                                                                     right = 0 ,  
                                                                                     left = 15,
                                                                                     DT::dataTableOutput("alien_perc_br"))), 
                                                                column(12,
                                                                       absolutePanel(top = 0, 
                                                                                     right = 0 ,  
                                                                                     left = 15,
                                                                                     DT::dataTableOutput("alien_perc_uf"))),
                                                                column(12,
                                                                       absolutePanel(top = 0, 
                                                                                     right = 0 ,  
                                                                                     left = 15,
                                                                                     DT::dataTableOutput("alien_perc_mun"))),
                                                                column(12,
                                                                       absolutePanel(top = 0, 
                                                                                     right = 0 ,  
                                                                                     left = 15,
                                                                                     DT::dataTableOutput("abst_abs_br"))),
                                                                column(12,
                                                                       absolutePanel(top = 0, 
                                                                                     right = 0 ,  
                                                                                     left = 15,
                                                                                     DT::dataTableOutput("abst_abs_uf"))),
                                                                column(12,
                                                                       absolutePanel(top = 0, 
                                                                                     right = 0 ,  
                                                                                     left = 15,
                                                                                     DT::dataTableOutput("abst_abs_mun"))),
                                                                
                                                                column(12,
                                                                       absolutePanel(top = 0, 
                                                                                     right = 0 ,  
                                                                                     left = 15,
                                                                                     DT::dataTableOutput("abst_perc_br"))), 
                                                                column(12,
                                                                       absolutePanel(top = 0, 
                                                                                     right = 0 ,  
                                                                                     left = 15,
                                                                                     DT::dataTableOutput("abst_perc_uf"))),
                                                                column(12,
                                                                       absolutePanel(top = 0, 
                                                                                     right = 0 ,  
                                                                                     left = 15,
                                                                                     DT::dataTableOutput("abst_perc_mun"))),
                                                                column(12,
                                                                       absolutePanel(top = 0, 
                                                                                     right = 0 ,  
                                                                                     left = 15,
                                                                                     DT::dataTableOutput("vtbr_abs_br"))), 
                                                                 column(12,
                                                                    absolutePanel(top = 0, 
                                                                               right = 0 ,  
                                                                              left = 15,
                                                                               DT::dataTableOutput("vtbr_abs_uf"))),
                                                                column(12,
                                                                       absolutePanel(top = 0, 
                                                                                     right = 0 ,  
                                                                                     left = 15,
                                                                                     DT::dataTableOutput("vtbr_abs_mun"))),
                                                                 column(12,
                                                                    absolutePanel(top = 0,
                                                                                  right = 0 , 
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("vtbr_perc_br"))),
                                                                column(12,
                                                                       absolutePanel(top = 0,
                                                                                     right = 0 , 
                                                                                     left = 15,
                                                                                     DT::dataTableOutput("vtbr_perc_uf"))),
                                                                column(12,
                                                                       absolutePanel(top = 0,
                                                                                     right = 0 , 
                                                                                     left = 15,
                                                                                     DT::dataTableOutput("vtbr_perc_mun"))),
                                                                column(12,
                                                                       absolutePanel(top = 0,
                                                                                     right = 0 , 
                                                                                     left = 15,
                                                                                     DT::dataTableOutput("vtnl_abs_br"))),
                                                                column(12,
                                                                       absolutePanel(top = 0,
                                                                                     right = 0 , 
                                                                                     left = 15,
                                                                                     DT::dataTableOutput("vtnl_abs_uf"))),
                                                                column(12,
                                                                       absolutePanel(top = 0,
                                                                                     right = 0 , 
                                                                                     left = 15,
                                                                                     DT::dataTableOutput("vtnl_abs_mun"))),
                                                                column(12,
                                                                       absolutePanel(top = 0,
                                                                                     right = 0 , 
                                                                                     left = 15,
                                                                                     DT::dataTableOutput("vtnl_perc_br"))),
                                                                column(12,
                                                                       absolutePanel(top = 0,
                                                                                     right = 0 , 
                                                                                     left = 15,
                                                                                     DT::dataTableOutput("vtnl_perc_uf"))),
                                                                column(12,
                                                                       absolutePanel(top = 0,
                                                                                     right = 0 , 
                                                                                     left = 15,
                                                                                     DT::dataTableOutput("vtnl_perc_mun")))),
                                                    
                                                    tabPanel("Dados desagregados", br(),
                                                             
                                                             column(12,
                                                                    absolutePanel(top = 0, 
                                                                                  right = 0 ,  
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("agreg_alien_abs_br"))), ## Tabelas que serao exibidas
                                                             column(12,
                                                                    absolutePanel(top = 0, 
                                                                                  right = 0 ,  
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("agreg_alien_abs_uf"))),
                                                             column(12,
                                                                    absolutePanel(top = 0, 
                                                                                  right = 0 ,  
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("agreg_alien_abs_mun"))),
                                                             column(12,
                                                                    absolutePanel(top = 0, 
                                                                                  right = 0 ,  
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("agreg_alien_perc_br"))), 
                                                             column(12,
                                                                    absolutePanel(top = 0, 
                                                                                  right = 0 ,  
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("agreg_alien_perc_uf"))),
                                                             column(12,
                                                                    absolutePanel(top = 0, 
                                                                                  right = 0 ,  
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("agreg_alien_perc_mun"))),
                                                             column(12,
                                                                    absolutePanel(top = 0, 
                                                                                  right = 0 ,  
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("agreg_abst_abs_br"))),
                                                             column(12,
                                                                    absolutePanel(top = 0, 
                                                                                  right = 0 ,  
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("agreg_abst_abs_uf"))),
                                                             column(12,
                                                                    absolutePanel(top = 0, 
                                                                                  right = 0 ,  
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("agreg_abst_abs_mun"))),
                                                             
                                                             column(12,
                                                                    absolutePanel(top = 0, 
                                                                                  right = 0 ,  
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("agreg_abst_perc_br"))), 
                                                             column(12,
                                                                    absolutePanel(top = 0, 
                                                                                  right = 0 ,  
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("agreg_abst_perc_uf"))),
                                                             column(12,
                                                                    absolutePanel(top = 0, 
                                                                                  right = 0 ,  
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("agreg_abst_perc_mun"))),
                                                             column(12,
                                                                    absolutePanel(top = 0, 
                                                                                  right = 0 ,  
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("agreg_vtbr_abs_br"))), 
                                                             column(12,
                                                                    absolutePanel(top = 0, 
                                                                                  right = 0 ,  
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("agreg_vtbr_abs_uf"))),
                                                             column(12,
                                                                    absolutePanel(top = 0, 
                                                                                  right = 0 ,  
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("agreg_vtbr_abs_mun"))),
                                                             column(12,
                                                                    absolutePanel(top = 0,
                                                                                  right = 0 , 
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("agreg_vtbr_perc_br"))),
                                                             column(12,
                                                                    absolutePanel(top = 0,
                                                                                  right = 0 , 
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("agreg_vtbr_perc_uf"))),
                                                             column(12,
                                                                    absolutePanel(top = 0,
                                                                                  right = 0 , 
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("agreg_vtbr_perc_mun"))),
                                                             column(12,
                                                                    absolutePanel(top = 0,
                                                                                  right = 0 , 
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("agreg_vtnl_abs_br"))),
                                                             column(12,
                                                                    absolutePanel(top = 0,
                                                                                  right = 0 , 
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("agreg_vtnl_abs_uf"))),
                                                             column(12,
                                                                    absolutePanel(top = 0,
                                                                                  right = 0 , 
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("agreg_vtnl_abs_mun"))),
                                                             column(12,
                                                                    absolutePanel(top = 0,
                                                                                  right = 0 , 
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("agreg_vtnl_perc_br"))),
                                                             column(12,
                                                                    absolutePanel(top = 0,
                                                                                  right = 0 , 
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("agreg_vtnl_perc_uf"))),
                                                             column(12,
                                                                    absolutePanel(top = 0,
                                                                                  right = 0 , 
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("agreg_vtnl_perc_mun"))))))))),

# 1.4. Volatilidade ---------------------------------------------------------



            tabPanel("Volatilidade",  ## Definicao das ferramentas de selecao para a guia
                     ## "Renovação parlamentar"
                     
                     
                     sidebarLayout(
                       
                       div(id ="Sidebar4",sidebarPanel(h5(align = "center","Faça sua consulta:"),width = 3,
                                                       
                                                       
                                                       selectizeInput(inputId = "INDICADORES_VOL",
                                                                      label = NULL, 
                                                                      choices = c("","Volatilidade eleitoral",
                                                                                  "Volatilidade parlamentar"), ## Indicadores disponiveis
                                                                      selected = NULL,
                                                                      options = list(placeholder = 'Escolha um indicador')),
                                                       
                                                       selectizeInput(inputId = "DESCRICAO_CARGO4",
                                                                      label = NULL,
                                                                      choices = c("","Deputado Federal", ## Cargos disponiveis
                                                                                  "Deputado Estadual",
                                                                                  "Vereador"),
                                                                      selected = NULL,
                                                                      options = list(placeholder = 'Escolha um cargo')),
                                                       
                                                       uiOutput("AGREGACAO_REGIONAL4"),
                                                       
                                                       uiOutput("UF4"),
                                                       
                                                       uiOutput("MUN4"),
                                                       
                                                       uiOutput("INT4"),
                                                       
                                                       h5(align = "center",
                                                          actionButton(inputId = "BCALC4",
                                                                       label = strong("Calcular"), ## Botao de acao "Calcular"
                                                                       width = "50%"))
                                                       
                                                       
                       )),
                       
                       mainPanel(id = "Main4",
                                 
                                 bsButton("showpanel4", 
                                          label = NULL, 
                                          icon = icon("bars"),
                                          type = "toggle", 
                                          value = TRUE),
                                 
                                 
                                 absolutePanel(top = 0, right = 0, left = 65,
                                               
                                               absolutePanel(top = 0, right = 0, left = 260,
                                                             actionBttn(inputId = "modal_vol",
                                                                        color = "default",
                                                                        icon = icon("question"), 
                                                                        style = "material-circle",
                                                                        size = "md")),
                                               tabsetPanel(type = "pills",
                                                           tabPanel("Resumo", br(),
                                                                   
                                                           column(12,
                                                                  absolutePanel(top = 0, 
                                                                                right = 0 ,  
                                                                                left = 15, 
                                                                                DT::dataTableOutput("vol_ele_br"))),
                                                           column(12,
                                                                  absolutePanel(top = 0, 
                                                                                right = 0 ,  
                                                                                left = 15, 
                                                                                DT::dataTableOutput("vol_ele_uf"))),
                                                           column(12,
                                                                  absolutePanel(top = 0, 
                                                                                right = 0 ,  
                                                                                left = 15, 
                                                                                DT::dataTableOutput("vol_ele_mun"))),
                                                           column(12,
                                                                  absolutePanel(top = 0, 
                                                                                right = 0 ,  
                                                                                left = 15, 
                                                                                DT::dataTableOutput("vol_parl_br"))),
                                                           column(12,
                                                                  absolutePanel(top = 0, 
                                                                                right = 0 ,  
                                                                                left = 15, 
                                                                                DT::dataTableOutput("vol_parl_uf"))),
                                                           column(12,
                                                                  absolutePanel(top = 0, 
                                                                                right = 0 ,  
                                                                                left = 15, 
                                                                                DT::dataTableOutput("vol_parl_mun")))),
                                                           
                                                                     ## Tabelas que serao exibidas
                                                           tabPanel("Dados desagregados", br(),
                                                                    column(12,
                                                                           absolutePanel(top = 0, 
                                                                                         right = 0 ,  
                                                                                         left = 15, 
                                                                                         DT::dataTableOutput("agreg_vol_ele_br"))),
                                                                    column(12,
                                                                           absolutePanel(top = 0, 
                                                                                         right = 0 ,  
                                                                                         left = 15, 
                                                                                         DT::dataTableOutput("agreg_vol_ele_uf"))),
                                                                    column(12,
                                                                           absolutePanel(top = 0, 
                                                                                         right = 0 ,  
                                                                                         left = 15, 
                                                                                         DT::dataTableOutput("agreg_vol_ele_mun"))),
                                                                    column(12,
                                                                           absolutePanel(top = 0, 
                                                                                         right = 0 ,  
                                                                                         left = 15, 
                                                                                         DT::dataTableOutput("agreg_vol_parl_br"))),
                                                                    column(12,
                                                                           absolutePanel(top = 0, 
                                                                                         right = 0 ,  
                                                                                         left = 15, 
                                                                                         DT::dataTableOutput("agreg_vol_parl_uf"))),
                                                                    column(12,
                                                                           absolutePanel(top = 0, 
                                                                                         right = 0 ,  
                                                                                         left = 15, 
                                                                                         DT::dataTableOutput("agreg_vol_parl_mun"))))))))),
             

# 1.5. Sobre ----------------------------------------------------------------

           
       
    tabPanel("Sobre", htmlOutput("sobre"))),


# 1.6. Busy spinner ---------------------------------------------------------


  
  add_busy_spinner(spin = "fulfilling-bouncing-circle",
                   position = "top-right",
                   margins = c(300, 650)
  ),


# 1.7. Rodape ---------------------------------------------------------------


  
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
              margin-top: 90em;",
                         
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
                                           padding: 0;"))))




