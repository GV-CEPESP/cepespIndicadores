

# Objetivo
#'        - Criar graficos e tabelas para a exibicao dos indicadores.



# 1. Server ---------------------------------------------------------------


server <- function(input, output,session){
  
 ## Funcao para descricao do sobre
  
  output$Note <- renderUI({
    note <- paste0("
                   <h2 align = 'center'>
                   <font size ='6' color = 'black'><strong>
                   
                   Sobre </font></h4>
                   
                   <font size = '1' color = 'black'>

                   <h4 align = 'justify'><br />
                   Os indicadores eleitorais são uma iniciativa de disseminar análise de dados eleitorais. 
                   Os indicadores aqui calculados foram inspirados pelo livro 'Votos e Partidos - Almanaque 
                   de Dados Eleitorais' de Wanderley Guilherme dos Santos. Todos os indicadores foram calculados 
                   a partir dos dados do <a href='http://www.cepesp.io/cepesp-data/'> CepespData </a>. Desenvolvido 
                   por Rebeca Carvalho, Gabriela Campos e apoio da <a href='http://cepespdata.io/sobre'> 
                   equipe CEPESP </h4></font>")
    HTML(note)
  })
  
  
## Funcao que retorna uma nova caixa de selecao quando o usuario seleciona "UF" na agregacao regional    
  
### Fragmentacao partidaria
  
  
  agreg <- reactive({
    agregacao <- input$AGREGACAO_REGIONAL2
    if(agregacao == "UF"){
      return(input$UF2)
    } 
  })
  
  
  output$UF2 <- renderUI({
    agregacao <- input$AGREGACAO_REGIONAL2
    if(agregacao == "UF"){
      selectizeInput("UF2",
                     label = NULL,
                     choices = c("", "AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", 
                                 "GO", "MA", "MG","MS", "MT", "PA", "PB", "PE", "PI", 
                                 "PR", "RJ", "RN", "RO", "RR","RS", "SC", "SE", "SP", "TO"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha uma UF'))
    }
  })
  
  
### Alienacao
  
  
  agreg <- reactive({
    agregacao <- input$AGREGACAO_REGIONAL3
    if(agregacao == "UF"){
      return(input$UF3)
    } 
  })
  
  
  output$UF3 <- renderUI({
    agregacao <- input$AGREGACAO_REGIONAL3
    if(agregacao == "UF"){
      selectizeInput("UF3",
                     label = NULL,
                     choices = c("","Todas UFs","AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", 
                                 "GO", "MA", "MG","MS", "MT", "PA", "PB", "PE", "PI", 
                                 "PR", "RJ", "RN", "RO", "RR","RS", "SC", "SE", "SP", "TO"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha uma UF'))
    }
  })
  
  
  
# 2. Indicadores ------------------------------------------------------------
  
  ## Definicao das atribuicoes das tabela dos indicadores e seus respectivos botoes de acao
  ## Definicao de cada indicador
  
# 2.1. Distribuicao de cadeiras -------------------------------------------  
 
  ## Funcao para descricao dos indicadores de distribuicao de cadeiras
  
  output$def_distc <- renderUI({
    note <- paste0("<h3>Defini&ccedil;&atilde;o dos indicadores</h3>
                   <h4><br />Quociente Eleitoral</h4>
                   <p>&Eacute; o n&uacute;mero m&iacute;nimo de votos que um partido ou coliga&ccedil;
                   &atilde;o deve atingir em determinada UF e elei&ccedil;&atilde;o para garantir uma vaga.</p>
                   <p>Quociente Eleitoral (QE) = (Votos V&aacute;lidos)/(N&uacute;mero de vagas existentes)&nbsp;</p>
                   <p><br /><strong>Fonte:</strong>&nbsp;Tribunal Superior Eleitoral - TSE -&nbsp;
                   <a href='http://www.tse.jus.br/eleitor/glossario/termos/quociente-eleitoral'>Link</a></p>
                   <h4>Quociente Partid&aacute;rio</h4>
                   <p>O indicador representa o n&uacute;mero de vagas que o partido ou coliga&ccedil;&atilde;o obteve, 
                   excluindo as vagas distribu&iacute;das por m&eacute;dia.</p>
                   <p>Quociente partid&aacute;rio (QP) = n&uacute;mero de votos v&aacute;lidos do partido ou coliga&ccedil;&atilde;o
                   / quociente eleitoral</p>
                   <p><strong>Fonte:</strong>&nbsp;Tribunal Superior Eleitoral - TSE -&nbsp;
                   <a href='http://www.tse.jus.br/eleitor/glossario/termos/quociente-partidario'>Link</a></p>")
    HTML(note)
  }) 
  
  
  ## Funcao que permite o download dos dados agregados dos indicadores 
  ## de distribuicao de cadeiras
  
  #indic_distr <- reactive({ ## Funcao reativa que coincide o indicador escolhido com sua respectiva tabela
    #switch(input$INDICADORES_DISTR,
           #"Quociente eleitoral" = vags_fed,
           #"Quociente partidário" = vags_est
    #)
  #})  
  
  output$BD1 <- downloadHandler( ## Atribuicoes da funcao download
    filename = function () {
      paste("indicadores_distribuicao .csv", sep = "")
    },
    content = function (filename) {
      write.csv(vags_fed, filename)
    }
  )
  
  
# 2.1.1. Quociente eleitoral -----------------------------------------------
  


### Deputado federal
  
  depfed <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_DISTR
    cargo <- input$DESCRICAO_CARGO1
    uf <- input$UF
    if(indicador == "Quociente eleitoral" & cargo == "Deputado Federal"){
      return(input$quoce_fed)
    }
  })
  
  output$quoce_fed <- DT::renderDataTable({ ## Tabela que devera ser chamada na ui
    bquoce_fed()
  })
  
  
  bquoce_fed <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(ordering = TRUE, searching = FALSE,lengthChange = FALSE,lengthMenu = FALSE),{
      indicador <- input$INDICADORES_DISTR
      cargo <- input$DESCRICAO_CARGO1
      uf <- input$UF
      if(indicador == "Quociente eleitoral" & cargo == "Deputado Federal"){
        if(input$UF == "Todas UFs"){
          vags_fed %>% 
            select(`Ano da eleição`, UF, `Quociente eleitoral`) %>% 
            unique() %>% 
            spread(`Ano da eleição`, `Quociente eleitoral`)
        }else{
          vags_fed %>% 
            dplyr::filter(UF == input$UF) %>% 
            select(`Ano da eleição`, UF, `Quociente eleitoral`) %>% 
            unique() %>% 
            spread(`Ano da eleição`, `Quociente eleitoral`)}
        
      }
    })
  })  
  
### Deputado estadual
  
  depest <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_DISTR
    cargo <- input$DESCRICAO_CARGO1
    if(indicador == "Quociente eleitoral" & cargo == "Deputado Estadual"){
      return(input$quoce_est)
    }
  })
  
  output$quoce_est <- DT::renderDataTable({ ## Tabela que devera ser chamada na ui
    bquoce_est()
  })
  
  
  bquoce_est <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(ordering = TRUE, searching = FALSE,lengthChange = FALSE,lengthMenu = FALSE),{
      indicador <- input$INDICADORES_DISTR
      cargo <- input$DESCRICAO_CARGO1
      uf <- input$UF
      if(indicador == "Quociente eleitoral" & cargo == "Deputado Estadual"){
        if(input$UF=="Todas UFs"){
          expr = vags_est %>% 
            select(`Ano da eleição`, UF, `Quociente eleitoral`) %>% 
            unique %>% 
            spread(`Ano da eleição`, `Quociente eleitoral`)
          
        }else{
          expr = vags_est %>% 
            dplyr::filter(UF == input$UF) %>% 
            select(`Ano da eleição`, UF, `Quociente eleitoral`) %>% 
            unique %>% 
            spread(`Ano da eleição`, `Quociente eleitoral`)}
      }
    })
  }) 
  
  
  
# 2.1.2. Quociente partidario ---------------------------------------------
  
### Deputado federal
  
  depfedp <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_DISTR
    cargo <- input$DESCRICAO_CARGO1
    if(indicador == "Quociente partidário" & cargo == "Deputado Federal"){
      return(input$quocp_fed)
    }
  })
  
  output$quocp_fed <- DT::renderDataTable({ ## Tabela que devera ser chamada na ui
    bquocp_fed()
  })
  
  bquocp_fed <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(ordering = TRUE, lengthChange = FALSE,lengthMenu = FALSE),{
      indicador <- input$INDICADORES_DISTR
      cargo <- input$DESCRICAO_CARGO1
      uf <- input$UF
      if(indicador == "Quociente partidário" & cargo == "Deputado Federal"){
        if(input$UF=="Todas UFs"){
          expr = vags_fed %>% 
            select(`Ano da eleição`, UF, `Sigla do partido`, `Quociente partidário`)
          
        }else{
          expr = vags_fed %>% 
            dplyr::filter(UF == input$UF) %>% 
            select(`Ano da eleição`, UF, `Sigla do partido`, `Quociente partidário`)
        }
        
      }
    })
  })
  
### Deputado estadual
  
  depestp <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_DISTR
    cargo <- input$DESCRICAO_CARGO1
    if(indicador == "Quociente partidário" & cargo == "Deputado Estadual"){
      return(input$quocp_est)
    }
  })
  
  output$quocp_est <- DT::renderDataTable({ ## Tabela que devera ser chamada na ui
    bquocp_est()
  })
  
  bquocp_est <- eventReactive(input$BCALC1, { ## Botao de acao
    datatable(options = list(ordering = TRUE, lengthChange = FALSE,lengthMenu = FALSE),{
      indicador <- input$INDICADORES_DISTR
      cargo <- input$DESCRICAO_CARGO1
      uf <- input$UF
      if(indicador == "Quociente partidário" & cargo == "Deputado Estadual"){
        if(input$UF=="Todas UFs"){
          expr = vags_est %>% 
            select(`Ano da eleição`, UF, `Sigla do partido`, `Quociente partidário`)
          
        }else{
          expr = vags_est %>% 
            dplyr::filter(UF == input$UF) %>% 
            select(`Ano da eleição`, UF, `Sigla do partido`, `Quociente partidário`)
        }}
    })
  })
  

# 2.2. Fragmentacao partidaria --------------------------------------------    
 
  ## Funcao para descricao dos indicadores de fragmentacao partidaria
  
  output$def_frag <- renderUI({
    note <- paste0("<h3>Defini&ccedil;&atilde;o dos indicadores</h3>
                   <h4><br />Fracionaliza&ccedil;&atilde;o</h4>
                   <p>Este indicador tem por objetivo medir a dispers&atilde;o partid&aacute;ria de um parlamento. 
                   Ele indica qual a probabilidade de dois parlamentares desse parlamento, tomados ao acaso, pertecerem a partidos diferentes.</p>
                   <p><strong>Formula:</strong>&nbsp;</p>
                   <p>Fracionaliza&ccedil;&atilde;o = 1 - &sum;(pe<sup>2</sup>), onde pe = percentual de cadeiras ocupadas por partido</p>
                   <p>&nbsp;</p>                   
                   <h4>Fracionaliza&ccedil;&atilde;o M&aacute;xima</h4>
                   <p>A fracionaliza&ccedil;&atilde;o m&aacute;xima n&atilde;o depende da vota&ccedil;&atilde;o dos partidos, mas da quantidade 
                   de cadeiras e partidos com representa&ccedil;&atilde;o parlamentar.&nbsp;</p>
                   <p><strong>Formula:</strong>&nbsp;</p>
                   <p>Fracionaliza&ccedil;&atilde;o m&aacute;xima = N*(n-1)/n*(N-1), onde N = n&uacute;mero de cadeiras e n = n&uacute;mero 
                   de partidos com representa&ccedil;&atilde;o parlamentar</p>
                   <p>&nbsp;</p>                   
                   <h4>Fragmenta&ccedil;&atilde;o</h4>
                   <p>A fragmenta&ccedil;&atilde;o mede quanto o &iacute;ndice de fracionaliza&ccedil;&atilde;o se aproxima da fracionaliza&ccedil;
                   &atilde;o m&aacute;xima.</p>
                   <p><strong>Formula</strong></p>
                   <p>(&Iacute;ndice de fracionaliza&ccedil;&atilde;o)/(&Iacute;ndice de fracionaliza&ccedil;&atilde;o m&aacute;xima)</p>
                   <p>&nbsp;</p>                   
                   <h4>N&uacute;mero efetivo de partidos por cadeiras&nbsp;</h4>
                   <p>O conceito de n&uacute;mero efetivo de partidos define o grau de fragmenta&ccedil;&atilde;o do sistema partid&aacute;rio, 
                   atrav&eacute;s da pondera&ccedil;&atilde;o da for&ccedil;a relativa das legendas que comp&otilde;em o parlamento. O valor calculado 
                   aponta a quantidade de partidos com alguma relev&acirc;ncia em um dado sistema pol&iacute;tico.</p>
                   <p><strong>Formula</strong></p>
                   <p>NEPC = 1/ &sum;(pe<sup>2</sup>)</p>
                   <p>&nbsp;</p>
                   <p>&nbsp;</p>
                   <p>Fonte: Votos e partidos: almanaque de dados eleitorais (Organiza&ccedil;&atilde;o de Wanderley Guilherme dos Santos, 2002) e 
                   <a href='http://datapolitica.com.br/eleicao/metodologia.html'>Data Politica</a></p>")
    HTML(note)
  }) 
  
  
  ## Funcao que permite o download dos dados agregados dos indicadores 
  ## de fragmentacao partidaria
  
  #indic_frag <- reactive({ ## Funcao reativa que coincide o indicador escolhido com sua respectiva tabela
    #switch(input$INDICADORES_DISTR,
           #"Fracionalização" = frag_partdf,
           #"Fracionalização máxima" = frag_partdf,
           #"Fragmentação" = frag_partdf,
           #"Número efetivo de partidos por cadeiras" = frag_partdf
    #)
  #})  
  
  output$BD2 <- downloadHandler( ## Atribuicoes da funcao download
    filename = function () {
      paste("indicadores_fragmentacao.csv", sep = "")
    },
    content = function (filename) {
      write.csv(frag_partdf, filename)
    }
  )
  

# 2.2.1. Fracionalizacao -------------------------------------------------- 
  
### Deputado Federal
  
  
  depfedf <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Fracionalização" & cargo == "Deputado Federal" & agregacao == "Brasil"){
      return(input$fracio_fed)
    }
  })
  
  output$fracio_fed <- DT::renderDataTable({ ## Tabela que devera ser chamada na ui
    bfracio_fed()
  })
  
  bfracio_fed <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(dom = 't'),{
      indicador <- input$INDICADORES_FRAG
      cargo <- input$DESCRICAO_CARGO2
      agregacao <- input$AGREGACAO_REGIONAL2
      uf <- input$UF2
      if(indicador == "Fracionalização" & cargo == "Deputado Federal" 
         & agregacao == "Brasil"){
        frag_partdf %>% 
          ungroup() %>% 
          dplyr::select(`Ano da eleição`,Fracionalização) %>% 
          unique() %>% 
          spread(`Ano da eleição`,Fracionalização)
        
        
      }
    })
  })  
  
  
# 2.2.2. Fracionalizacao maxima -------------------------------------------
  
### Deputado Federal
  
  depfedfm <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Fracionalização máxima" & cargo == "Deputado Federal" & agregacao == "Brasil"){
      return(input$fraciomax_fed)
    }
  })
  
  output$fraciomax_fed <- DT::renderDataTable({ ## Tabela que devera ser chamada na ui
    bfraciomax_fed()
  })
  
  bfraciomax_fed <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(dom = 't'),{
      indicador <- input$INDICADORES_FRAG
      cargo <- input$DESCRICAO_CARGO2
      agregacao <- input$AGREGACAO_REGIONAL2
      uf <- input$UF2
      if(indicador == "Fracionalização máxima" & cargo == "Deputado Federal" 
         & agregacao == "Brasil"){
        frag_partdf %>% 
          ungroup() %>% 
          dplyr::select(`Ano da eleição`,`Fracionalização máxima`) %>% 
          unique() %>% 
          spread(`Ano da eleição`,`Fracionalização máxima`)
        
      }
    })
  })
  
  
# 2.2.3. Fragmentacao -----------------------------------------------------
  
### Deputado Federal
  
  depfed_frag <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Fragmentação" & cargo == "Deputado Federal" & agregacao == "Brasil"){
      return(input$frag_fed)
    }
  })
  
  output$frag_fed <- DT::renderDataTable({ ## Tabela que devera ser chamada na ui
    bfrag_fed()
  })
  
  
  bfrag_fed <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(dom = 't'),{
      indicador <- input$INDICADORES_FRAG
      cargo <- input$DESCRICAO_CARGO2
      agregacao <- input$AGREGACAO_REGIONAL2
      uf <- input$UF2
      if(indicador == "Fragmentação" & cargo == "Deputado Federal" 
         & agregacao == "Brasil"){
        frag_partdf %>% 
          ungroup() %>% 
          dplyr::select(`Ano da eleição`,Fragmentação) %>% 
          unique() %>% 
          spread(`Ano da eleição`,Fragmentação)
        
      }
    })
  }) 
  
# 2.2.4. Numero efetivo de partidos por cadeiras -----------------------------------------------------
  
### Deputado Federal
  
  depfedn <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    agregacao <- input$AGREGACAO_REGIONAL2
    uf <- input$UF2
    if(indicador == "Número efetivo de partidos por cadeiras" & cargo == "Deputado Federal" 
       & agregacao == "Brasil"){
      return(input$nepc_fed)
    }
  })
  
  output$nepc_fed <- DT::renderDataTable({ ## Tabela que devera ser chamada na ui
    bnepc_fed()
  })
  
  bnepc_fed <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(dom = 't'),{
      indicador <- input$INDICADORES_FRAG
      cargo <- input$DESCRICAO_CARGO2
      agregacao <- input$AGREGACAO_REGIONAL2
      uf <- input$UF2
      if(indicador == "Número efetivo de partidos por cadeiras" & cargo == "Deputado Federal" 
         & agregacao == "Brasil"){
        frag_partdf %>% 
          dplyr::select(`Ano da eleição`,`Numero efetivo de partidos por cadeiras`) %>% 
          unique() %>% 
          spread(`Ano da eleição`,`Numero efetivo de partidos por cadeiras`)
        
      }
    })
  }) 
  
### Deputado Estadual
  
  depest_nep <- reactive({ ## Atributos da tabela
    indicador <- input$INDICADORES_FRAG
    cargo <- input$DESCRICAO_CARGO2
    uf <- input$UF2
    if(indicador == "Número efetivo de partidos por cadeiras" & cargo == "Deputado Estadual"){
      return(input$nepc_est)
    }
  })
  
  output$nepc_est <- DT::renderDataTable({ ## Tabela que devera ser chamada na ui
    bnepc_est()
  })   
  
  bnepc_est <- eventReactive(input$BCALC2, { ## Botao de acao
    datatable(options = list(dom = 't'),{
      indicador <- input$INDICADORES_FRAG
      cargo <- input$DESCRICAO_CARGO2
      agregacao <- input$AGREGACAO_REGIONAL2
      uf <- input$UF2
      if(indicador == "Número efetivo de partidos por cadeiras" & cargo == "Deputado Estadual" 
         & agregacao == "Brasil"){
        frag_partdf %>% 
          dplyr::select(`Ano da eleição`,`Numero efetivo de partidos por cadeiras`) %>% 
          unique() %>% 
          spread(`Ano da eleição`,`Numero efetivo de partidos por cadeiras`)
        
      }
    })
  })  
 
  
# 2.3. Alienacao ----------------------------------------------------------  
  
  ## Funcao para descricao dos indicadores de alienacao
  
  output$def_alien <- renderUI({
    note <- paste0("<h3>Defini&ccedil;&atilde;o dos indicadores</h3>
                   <p>Indicadores de aliena&ccedil;&atilde;o medem a participa&ccedil;&atilde;o nas elei&ccedil;&otilde;es, por unidade eleitoral.</p>
                   <h4><br />Aliena&ccedil;&atilde;o Absoluta</h4>
                   <p>A aliena&ccedil;&atilde;o &eacute; a soma de quantidade de absten&ccedil;&otilde;es, votos brancos e votos nulos
                   de determinada elei&ccedil;&atilde;o.</p>
                   <p><strong>Formula:</strong>&nbsp;</p>
                   <p>(Absten&ccedil;&otilde;es + Votos Brancos + Votos Nulos)</p>
                   <p>&nbsp;</p>
                   <h4>Aliena&ccedil;&atilde;o Percentual</h4>
                   <p>Aliena&ccedil;&atilde;o percentual &eacute; alieana&ccedil;&atilde;o (absten&ccedil;&otilde;es, brancos e nulos), 
                   dividido pelo total de eleitores aptos da unidade eleitoral.</p>
                   <p><strong>Formula:</strong>&nbsp;</p>
                   <p>(Absten&ccedil;&otilde;es + Votos Brancos + Votos Nulos)/(Total de eleitores aptos)</p>
                   <p>&nbsp;</p>
                   <p>Fonte: Votos e partidos: almanaque de dados eleitorais (Organiza&ccedil;&atilde;o de Wanderley Guilherme dos Santos, 2002)")
    HTML(note)
  })
   
  
  ## Funcao que permite o download dos dados agregados dos indicadores 
  ## de alienacao
  
  #indic_alien <- reactive({ ## Funcao reativa que coincide o indicador escolhido com sua respectiva tabela
    #switch(input$INDICADORES_DISTR,
           #"Alienação absoluta" = alienacao,
           #"Alienação percentual" = alienacao
    #)
  #})  
  
  output$BD3 <- downloadHandler( ## Atribuicoes da funcao download
    filename = function () {
      paste("indicadores_alienacao.csv", sep = "")
    },
    content = function (filename) {
      write.csv(alienacao, filename)
    }
  )
  
# 2.3.1. Alienacao --------------------------------------------------------
  
### Deputado Federal BR
  
  depfeda_br <- reactive({ ## Atributos das tabelas de alienacao absoluta e percentual
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$DESCRICAO_CARGO3
    if(indicador == "Alienação Absoluta" & agregacao == "Brasil"){
      return(input$alien_feda_br)}
    else if(indicador == "Alienação Percentual" & agregacao == "Brasil"){
      return(input$alien_fedp_br)
  }
  })
  
  output$alien_feda_br <- DT::renderDataTable({ ## Tabela da alienacao absoluta que devera ser chamada na ui
    balien_feda_br()
  })
  
  balien_feda_br <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao absoluta
    datatable(options = list(ordering = TRUE, searching = FALSE,lengthChange = FALSE,lengthMenu = FALSE),{
      indicador <- input$INDICADORES_ALIE
      cargo <- input$DESCRICAO_CARGO3
      agregacao <- input$AGREGACAO_REGIONAL3
      if(indicador == "Alienação Absoluta" & agregacao == "Brasil"){
        alienacao_br %>% 
          dplyr::filter(Cargo ==input$DESCRICAO_CARGO3) %>% 
          dplyr::select(`Ano da eleição`,Turno,`Alienação Absoluta`) %>% 
          spread(`Ano da eleição`,`Alienação Absoluta`)
        
      }
    })
  }) 
  
  output$alien_fedp_br <- DT::renderDataTable({ ## Tabela da alienacao percentual que devera ser chamada na ui
    balien_fedp_br()
  })
  
  balien_fedp_br <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao percentual
    datatable(options = list(ordering = TRUE, searching = FALSE,lengthChange = FALSE,lengthMenu = FALSE),{
      indicador <- input$INDICADORES_ALIE
      cargo <- input$DESCRICAO_CARGO3
      agregacao <- input$AGREGACAO_REGIONAL3
      if(indicador == "Alienação Percentual" & agregacao == "Brasil"){
        alienacao_br %>% 
          dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
          dplyr::select(`Ano da eleição`,Turno, `Alienação Percentual`) %>% 
          spread(`Ano da eleição`,`Alienação Percentual`)
        
      }
    })
  }) 
  
### Deputado Federal UF
  
  depfeda_uf <- reactive({ ## Atributos das tabelas de alienacao absoluta e alienacao percentual
    indicador <- input$INDICADORES_ALIE
    cargo <- input$DESCRICAO_CARGO3
    agregacao <- input$DESCRICAO_CARGO3
    uf <- input$UF3
    if(indicador == "Alienação Absoluta" & agregacao == "UF"){
      return(input$alien_feda_uf)}
    else if(indicador == "Alienação Percentual" & agregacao == "UF"){
      return(input$alien_fedp_uf)
  }    
  })
  
  output$alien_feda_uf <- DT::renderDataTable({ ## Tabela da alienacao absoluta que devera ser chamada na ui
    balien_feda_uf()
  })
  
  balien_feda_uf <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao absoluta
    datatable(options = list(ordering = TRUE, searching = FALSE,lengthChange = FALSE,lengthMenu = FALSE),{
      indicador <- input$INDICADORES_ALIE
      cargo <- input$DESCRICAO_CARGO3
      agregacao <- input$AGREGACAO_REGIONAL3
      uf <- input$UF3
      if(indicador == "Alienação Absoluta" & agregacao == "UF"){
        if(uf=="Todas UFs"){
          alienacao_uf %>% 
            dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
            dplyr::select(`Ano da eleição`,UF,Turno, `Alienação Absoluta`) %>% 
            spread(`Ano da eleição`,`Alienação Absoluta`)
        }else{
          alienacao_uf %>% 
            dplyr::filter(UF == input$UF3 & Cargo==input$DESCRICAO_CARGO3) %>% 
            dplyr::select(`Ano da eleição`,UF,Turno, `Alienação Absoluta`) %>% 
            spread(`Ano da eleição`,`Alienação Absoluta`)}
        
      }
    })
  })  
  
   output$alien_fedp_uf <- DT::renderDataTable({ ## Tabela da alienacao percentual que devera ser chamada na ui
    balien_fedp_uf()
  })
   
   balien_fedp_uf <- eventReactive(input$BCALC3, { ## Botao de acao da alienacao percentual
     datatable(options = list(ordering = TRUE, searching = FALSE,lengthChange = FALSE,lengthMenu = FALSE),{
       indicador <- input$INDICADORES_ALIE
       cargo <- input$DESCRICAO_CARGO3
       agregacao <- input$AGREGACAO_REGIONAL3
       uf <- input$UF3
       if(indicador == "Alienação Percentual" & agregacao == "UF"){
         if(uf=="Todas UFs"){
           alienacao_uf %>% 
             dplyr::filter(Cargo==input$DESCRICAO_CARGO3) %>% 
             dplyr::select(`Ano da eleição`,UF, Cargo, Turno, `Alienação Percentual`) %>% 
             spread(`Ano da eleição`,`Alienação Percentual`)
         }
         else{
           alienacao_uf %>% 
             dplyr::filter(UF == input$UF3 & Cargo==input$DESCRICAO_CARGO3) %>% 
             dplyr::select(`Ano da eleição`,UF,Turno, `Alienação Percentual`) %>% 
             spread(`Ano da eleição`,`Alienação Percentual`)}
         
       }
     })
   })
}
  

