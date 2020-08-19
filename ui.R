


# Objetivo
#'        - Criar uma interface em shiny para exibir os indicadores calculados.


# 1. User interface -------------------------------------------------------

## Secao correspondente a interface que o usuario visualizara

ui <- 
  
  fluidPage(
    
    useShinyjs(),
    
    
    tags$head(includeCSS("styles.css")),
    
    
    tags$head(tags$link(rel="shortcut icon", 
                        href="favicon_cepesp.ico",
                        type="image/vnd.microsoft.icon")),
    
    tags$div(class = "btn-header", 
             checked = NA,
             tags$a(id = "cepesp",
                    href = "http://cepespdata.io/",
                    class="btn btn-primary cepesp", 
                    "CEPESP DATA")),
  
    
  title = "CEPESP Indicadores", ## Titulo da pagina do aplicativo em versao web
  
  
    navbarPage(title = div(tags$a(href = "http://www.cepesp.io/",
                             img(src="logo_cepesp.png",
                               style="width: 220px;
                                      height: 48px;"))),
               
               collapsible= TRUE,
               fluid = TRUE,
               id = "CEPESP Indicadores", 
                     
          

# 1.1. Fragmentacao legislativa ---------------------------------------------

             
            
            tabPanel("FRAGMENTAÇÃO", useShinydashboardPlus(),  
                      

# 1.1.1. SidebarLayout ----------------------------------------------------

                     
                     sidebarLayout( 
                        
                      
                        div(id ="Sidebar1",sidebarPanel(h5(class = "h5 consulta","FAÇA SUA CONSULTA"),width = 3,
                                                        
                                   
                                    selectizeInput(inputId = "DESCRICAO_CARGO1",
                                                   label = NULL,
                                                   choices = c("","Senador",
                                                               "Deputado Federal", 
                                                               "Deputado Estadual",
                                                               "Prefeito",
                                                               "Vereador"), ## Cargos disponiveis
                                                   selected = NULL,
                                                   options = list(placeholder = 'Escolha um cargo')),
                                    
                                     uiOutput("INDICADORES_FRAG"),
                                     
                                     
                                     uiOutput("AGREGACAO_REGIONAL1"),
                                     
                                     uiOutput("UF1"),
                                    
                                     uiOutput("MUN1"),
                                    
                                     uiOutput("INT1"),
                                     
                                     h5(align = "center",
                                     actionButton(inputId = "BCALC1",
                                                  label = strong("CALCULAR"), ## Botao de acao calcular
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
                                        
                                        absolutePanel(top = 0, right = 0, left = 320,
                                        
                                            actionBttn(inputId = "modal_frag",
                                                       color = "default",
                                                       icon = icon("question"), 
                                                       style = "material-circle",
                                                       size = "md")),
                                        
                                        
                                        tabsetPanel(type = "pills",


# 1.1.3. Resumo -----------------------------------------------------------

                                                    
                                                                                                        
                                                    tabPanel("RESUMO", br(),
                                                             
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
                                                                                  DT::dataTableOutput("nepl_med", width = "100%"))),
                                                                 column(12,
                                                                    absolutePanel(top = 200,
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
                                                                                  DT::dataTableOutput("nepel_med", width = "100%"))),
                                                                 column(12,
                                                                    absolutePanel(top = 200, 
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
                                                                                  DT::dataTableOutput("fracio_med", width = "100%"))),
                                                                 column(12,
                                                                    absolutePanel(top = 200,
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
                                                                                  DT::dataTableOutput("fracio_max_med", width = "100%"))),
                                                                 column(12,
                                                                    absolutePanel(top = 200, 
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
                                                                                  DT::dataTableOutput("frag_med", width = "100%"))),
                                                                column(12,
                                                                    absolutePanel(top = 200,
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
                                                                                  DT::dataTableOutput("dpg_med", width = "100%"))),
                                                                 column(12,
                                                                    absolutePanel(top = 200, 
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
                                                                                  DT::dataTableOutput("quoce_med", width = "100%"))),
                                                                  column(12,
                                                                    absolutePanel(top = 200, 
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
                                                                                  DT::dataTableOutput("quocp_med", width = "100%"))),
                                                                                  
                                                                  column(12,
                                                                    absolutePanel(top = 200, 
                                                                                  right = 0 ,  
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("quocp_int", width = "100%")))),

# 1.1.4. Dados desagregados -----------------------------------------------

                                                                  
                                                                  
                                                  
                                                     tabPanel("DADOS DESAGREGADOS", br(),
                                                           
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

          
             
             tabPanel("RENOVAÇÃO",  ## Definicao das ferramentas de selecao para a guia
                      ## "Renovação parlamentar"
                      

# 1.2.1. SidebarLayout ----------------------------------------------------

                      
                      sidebarLayout(
                        
                        div(id ="Sidebar2",sidebarPanel(h5(class = "h5 consulta","FAÇA SUA CONSULTA"),width = 3,
                                     
                                     
                                    selectizeInput(inputId = "DESCRICAO_CARGO2",
                                                    label = NULL,
                                                    choices = c("","Deputado Federal", ## Cargos disponiveis
                                                                "Deputado Estadual",
                                                                "Vereador"),
                                                    selected = NULL,
                                                    options = list(placeholder = 'Escolha um cargo')),
                                    
                                     uiOutput("INDICADORES_RENOV"),
                                     
                                     uiOutput("AGREGACAO_REGIONAL2"),
                                     
                                     uiOutput("UF2"),
                                     
                                     uiOutput("MUN2"),
                                     
                                     uiOutput("INT2"),
                                     
                                     h5(align = "center",
                                     actionButton(inputId = "BCALC2",
                                                  label = strong("CALCULAR"), ## Botao de acao "Calcular"
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
                                        
                                        absolutePanel(top = 0, right = 0, left = 320,
                                                      actionBttn(inputId = "modal_renovp",
                                                                 color = "default",
                                                                 icon = icon("question"), 
                                                                 style = "material-circle",
                                                                 size = "md")),
                                        tabsetPanel(type = "pills",
                                                    

# 1.2.3. Resumo -----------------------------------------------------------

                                                    
                                                         tabPanel("RESUMO", br(),
                                                                 
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
                                                                                      DT::dataTableOutput("conserv_med"))),
                                                                 
                                                                 
                                                                 column(12,
                                                                        absolutePanel(top = 200,
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
                                                                                      DT::dataTableOutput("renov_bt_med"))),
                                                                 column(12,
                                                                        absolutePanel(top = 200, 
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
                                                                                      DT::dataTableOutput("renov_liq_med"))),
                                                                 column(12,
                                                                        absolutePanel(top = 200, 
                                                                                      right = 0 ,  
                                                                                      left = 15,
                                                                                      DT::dataTableOutput("renov_liq_int")))),


# 1.2.4. Dados desagregados -----------------------------------------------


                                                                                                                                                                            
                                                    ## Tabelas que serao exibidas
                                                             tabPanel("DADOS DESAGREGADOS", br(),
                                                                      
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

            
             
             tabPanel("PARTICIPAÇÃO E ALIENAÇÃO",  ## Definicao das ferramentas de selecao para a guia
                                    ## "Alienacao"
                      

# 1.3.1. SidebarPanel -----------------------------------------------------

                      
                      sidebarLayout(
                        
                        div(id ="Sidebar3",sidebarPanel(h5(class = "h5 consulta","FAÇA SUA CONSULTA"),width = 3,
                                     
                                     
                                     selectizeInput(inputId = "DESCRICAO_CARGO3",
                                                    label = NULL,
                                                    choices = c("","Presidente", "Governador", 
                                                                "Senador","Deputado Federal", ## Cargos disponiveis
                                                                "Deputado Estadual", "Prefeito", 
                                                                "Vereador"),
                                                    selected = NULL,
                                                    options = list(placeholder = 'Escolha um cargo')),
                                     
                                     uiOutput("INDICADORES_ALIE"),
                                     
                                     uiOutput("AGREGACAO_REGIONAL3"), 
                                    
                                     uiOutput("UF3"),
                                     
                                     uiOutput("MUN3"),
                                     
                                     uiOutput("INT3"),
                                     
                                     h5(align = "center",
                                     actionButton(inputId = "BCALC3",
                                                  label = strong("CALCULAR"), ## Botao de acao "Calcular"
                                                  width = "50%"))
                                     
                                     
                        )),
                        

# 1.3.2. MainPanel --------------------------------------------------------


                        
                                mainPanel(id = "Main3",
                                  
                                  bsButton("showpanel3", 
                                           label = NULL, 
                                           icon = icon("bars"),
                                           type = "toggle", 
                                           value = TRUE),
                          
                          
                          
                          absolutePanel(top = 0, right = 0, left = 65,
                                        
                                        absolutePanel(top = 0, right = 0, left = 320,
                                                      actionBttn(inputId = "modal_alien",
                                                                 color = "default",
                                                                 icon = icon("question"), 
                                                                 style = "material-circle",
                                                                 size = "md")),
                                        tabsetPanel(type = "pills",
                                                    

# 1.3.3. Resumo -----------------------------------------------------------

                                                    
                                                       tabPanel("RESUMO", br(),
                                                               
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
                                                                                     DT::dataTableOutput("alien_abs_med"))),
                                                                 column(12,
                                                                       absolutePanel(top = 200, 
                                                                                     right = 0 ,  
                                                                                     left = 15,
                                                                                     DT::dataTableOutput("alien_abs_int"))),
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
                                                                                     DT::dataTableOutput("alien_perc_med"))),
                                                                column(12,
                                                                       absolutePanel(top = 200, 
                                                                                     right = 0 ,  
                                                                                     left = 15,
                                                                                     DT::dataTableOutput("alien_perc_int"))),
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
                                                                                     DT::dataTableOutput("abst_abs_med"))),
                                                                column(12,
                                                                       absolutePanel(top = 200, 
                                                                                     right = 0 ,  
                                                                                     left = 15,
                                                                                     DT::dataTableOutput("abst_abs_int"))),
                                                                
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
                                                                                     DT::dataTableOutput("abst_perc_med"))),
                                                                column(12,
                                                                       absolutePanel(top = 200, 
                                                                                     right = 0 ,  
                                                                                     left = 15,
                                                                                     DT::dataTableOutput("abst_perc_int"))),
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
                                                                                     DT::dataTableOutput("vtbr_abs_med"))),
                                                                column(12,
                                                                       absolutePanel(top = 200, 
                                                                                     right = 0 ,  
                                                                                     left = 15,
                                                                                     DT::dataTableOutput("vtbr_abs_int"))),
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
                                                                                     DT::dataTableOutput("vtbr_perc_med"))),
                                                                column(12,
                                                                       absolutePanel(top = 200,
                                                                                     right = 0 , 
                                                                                     left = 15,
                                                                                     DT::dataTableOutput("vtbr_perc_int"))),
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
                                                                                     DT::dataTableOutput("vtnl_abs_med"))),
                                                                column(12,
                                                                       absolutePanel(top = 200,
                                                                                     right = 0 , 
                                                                                     left = 15,
                                                                                     DT::dataTableOutput("vtnl_abs_int"))),
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
                                                                                     DT::dataTableOutput("vtnl_perc_mun"))),
                                                                column(12,
                                                                       absolutePanel(top = 0,
                                                                                     right = 0 , 
                                                                                     left = 15,
                                                                                     DT::dataTableOutput("vtnl_perc_med"))),
                                                                column(12,
                                                                       absolutePanel(top = 200,
                                                                                     right = 0 , 
                                                                                     left = 15,
                                                                                     DT::dataTableOutput("vtnl_perc_int")))),

# 1.3.4. Dados desagregados -----------------------------------------------


                                                    
                                                    tabPanel("DADOS DESAGREGADOS", br(),
                                                             
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
                                                                                  DT::dataTableOutput("agreg_alien_abs_int"))),
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
                                                                                  DT::dataTableOutput("agreg_alien_perc_int"))),
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
                                                                                  DT::dataTableOutput("agreg_abst_abs_int"))),
                                                             
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
                                                                                  DT::dataTableOutput("agreg_abst_perc_int"))),
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
                                                                                  DT::dataTableOutput("agreg_vtbr_abs_int"))),
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
                                                                                  DT::dataTableOutput("agreg_vtbr_perc_int"))),
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
                                                                                  DT::dataTableOutput("agreg_vtnl_abs_int"))),
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
                                                                                  DT::dataTableOutput("agreg_vtnl_perc_mun"))),
                                                             column(12,
                                                                    absolutePanel(top = 0,
                                                                                  right = 0 , 
                                                                                  left = 15,
                                                                                  DT::dataTableOutput("agreg_vtnl_perc_int"))))))))),

# 1.4. Volatilidade ---------------------------------------------------------



            tabPanel("VOLATILIDADE",  ## Definicao das ferramentas de selecao para a guia
                     ## "Renovação parlamentar"
                     

# 1.4.1. SidebarPanel -----------------------------------------------------

                     
                     sidebarLayout(
                       
                       div(id ="Sidebar4",sidebarPanel(h5(class = "h5 consulta","FAÇA SUA CONSULTA"),width = 3,
                                                       
                                                      selectizeInput(inputId = "DESCRICAO_CARGO4",
                                                                      label = NULL,
                                                                      choices = c("","Deputado Federal", ## Cargos disponiveis
                                                                                  "Deputado Estadual",
                                                                                  "Prefeito",
                                                                                  "Vereador"),
                                                                      selected = NULL,
                                                                      options = list(placeholder = 'Escolha um cargo')),
                                                       
                                                       uiOutput("INDICADORES_VOL"),
                                                       
                                                       uiOutput("AGREGACAO_REGIONAL4"),
                                                       
                                                       uiOutput("UF4"),
                                                       
                                                       uiOutput("MUN4"),
                                                       
                                                       uiOutput("INT4"),
                                                       
                                                       h5(align = "center",
                                                          actionButton(inputId = "BCALC4",
                                                                       label = strong("CALCULAR"), ## Botao de acao "Calcular"
                                                                       width = "50%"))
                                                       
                                                       
                       )),
                       

# 1.4.2. MainPanel --------------------------------------------------------

                       
                       
                       mainPanel(id = "Main4",
                                 
                                 bsButton("showpanel4", 
                                          label = NULL, 
                                          icon = icon("bars"),
                                          type = "toggle", 
                                          value = TRUE),
                                 
                                 
                                 absolutePanel(top = 0, right = 0, left = 65,
                                               
                                               absolutePanel(top = 0, right = 0, left = 320,
                                                             actionBttn(inputId = "modal_vol",
                                                                        color = "default",
                                                                        icon = icon("question"), 
                                                                        style = "material-circle",
                                                                        size = "md")),
                                               tabsetPanel(type = "pills",
                                                           

# 1.4.3. Resumo -----------------------------------------------------------

                                                           
                                                           
                                                           tabPanel("RESUMO", br(),
                                                                   
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
                                                                                DT::dataTableOutput("vol_ele_med"))),
                                                           column(12,
                                                                  absolutePanel(top = 200, 
                                                                                right = 0 ,  
                                                                                left = 15, 
                                                                                DT::dataTableOutput("vol_ele_int"))),
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
                                                                                DT::dataTableOutput("vol_parl_mun"))),
                                                           column(12,
                                                                  absolutePanel(top = 0, 
                                                                                right = 0 ,  
                                                                                left = 15, 
                                                                                DT::dataTableOutput("vol_parl_med"))),
                                                           column(12,
                                                                  absolutePanel(top = 200, 
                                                                                right = 0 ,  
                                                                                left = 15, 
                                                                                DT::dataTableOutput("vol_parl_int")))),

# 1.4.4. Dados desagregados -----------------------------------------------


                                                           
                                                                     ## Tabelas que serao exibidas
                                                           tabPanel("DADOS DESAGREGADOS", br(),
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
                                                                                         DT::dataTableOutput("agreg_vol_ele_int"))),
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
                                                                                         DT::dataTableOutput("agreg_vol_parl_mun"))),
                                                                    column(12,
                                                                           absolutePanel(top = 0, 
                                                                                         right = 0 ,  
                                                                                         left = 15, 
                                                                                         DT::dataTableOutput("agreg_vol_parl_int"))))))))),
             

# 1.5. Sobre ----------------------------------------------------------------

           
       
    tabPanel("SOBRE", htmlOutput("sobre"))),




# 1.6. Busy spinner ---------------------------------------------------------


  
  add_busy_spinner(spin = "fulfilling-bouncing-circle",
                   position = "top-right",
                   margins = c(300, 650)
  ),


# 1.7. Rodape ---------------------------------------------------------------


tags$div(class="container-fluid p-4",
    img(src="logo_cepesp.png",
        style="width: 220px;
        height: 48px;")),
    
    tags$footer(class = "rodape",
              tags$div(class = "rodape-container",
                       tags$div(class = "rodape-texto", 
                                "© 2020 CEPESP Todos os direitos reservados."))))




