
## OBJETIVOS

#'        - Calcular a média de todos os indicadores nos seguintes 
#'          níveis de agregação regional:
#'        
#'        - 1. Brasil;
#'        - 2. UF; e 
#'        - 3. Quantidade agregada de eleitores aptos.

#'        - Limpeza e padronização dos dados gerados.

# 1. Data -----------------------------------------------------------------

## Cria uma lista com os nomes dos arquivos finais

arquivos <- list.files(path = "data/output",
                       pattern = "final",
                       full.names = T) %>% 
  set_names(nm = basename(.) %>% 
            tools::file_path_sans_ext())

## Lendo os arquivos e salvando em uma lista de data frames

arquivos <- purrr::map(arquivos, 
                       readRDS)

## Salva cada data frame da lista em seu próprio objeto 

purrr::pmap(.l = list(.x = names(arquivos), .y = arquivos), 
            .f = ~assign(.x, .y, envir = .GlobalEnv))

## Removendo a lista

rm(arquivos)

# 2. Indicador ------------------------------------------------------------

# 2.1. Fragmentação -------------------------------------------------------

# 2.1.1. Brasil -----------------------------------------------------------

media_fragmentacao_br <- fragmentacao_final %>% 
  filter(`Agregação regional` == "UF") %>% 
  select(`Ano da eleição`,
         Cargo,
         `Quociente eleitoral`,
         `Quociente partidário`,
         `Número efetivo de partidos eleitoral`:Desproporcionalidade) %>% 
  group_by(`Ano da eleição`,
           Cargo) %>% 
  summarise(across(`Quociente eleitoral`:Desproporcionalidade,
            ~mean(., na.rm = TRUE)))
