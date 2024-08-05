
# CEPESP INDICADORES - Repositório de Indicadores Eleitorais

Repositório de indicadores eleitorais desenvolvido pelo Centro de Política e Economia do Setor Público (CEPESP) da Fundação Getulio Vargas (FGV) a partir dos dados disponibilizados no **CEPESP DATA**.

## Organização do repositório

Este repositório se organiza nas seguintes pastas:

- `scripts`: códigos para limpeza, processamento e cálculo dos indicadores eleitorais:
  - `cepesp`: códigos adaptados para os dados no formato do CEPESP DATA;
  - `tse`: códigos adaptados para os dados no formato original do TSE.

- `data`: bases de dados utilizadas nos scripts:
  - `input`: dados brutos extraídos de fontes diversas;
  - `output`: dados processados e consolidados.
  
- `functions`: funções auxiliares utilizadas nos scripts principais.

### Códigos existentes na pasta e seus objetivos

| Subdiretório | Etapa                  | Arquivo                             | Descrição                                                                                                                          |
|--------------|------------------------|-------------------------------------|------------------------------------------------------------------------------------------------------------------------------------|
| scripts      | Coleta e Processamento | `00_index.R`                        | Configura o ambiente de trabalho, carrega os pacotes necessários e define funções utilitárias.                                      |
| scripts      | Importação de Dados    | `01_data.R`                         | Importa os dados brutos, realizando limpeza e transformação para um formato adequado às análises posteriores.                       |
| scripts      | Cálculo de Indicadores | `02_calc_quociente_eleitoral.R` | Calcula o quociente eleitoral e o número de eleitos por partido, agrupando os votos por partidos, calculando o total de votos válidos e aplicando as fórmulas necessárias. |
| scripts      | Cálculo de Indicadores | `03_calc_fragmentacao.R`            | Calcula índices de fragmentação partidária, como o Índice de Laakso-Taagepera, utilizando dados de votação para medir a dispersão dos votos entre partidos.  |
| scripts      | Cálculo de Indicadores | `04_calc_participacao_alienacao.R`  | Calcula os índices de participação e alienação eleitoral, processando os dados de votação para determinar o percentual de eleitores que votaram e que se abstiveram. |
| scripts      | Cálculo de Indicadores   | `05_calc_reeleicao.R`               | Analisa os dados de reeleição para identificar quais candidatos foram reeleitos, cruzando dados de diferentes eleições para verificar a continuidade dos mandatos. |
| scripts      | Cálculo de Indicadores| `06_calc_volatilidade.R`            | Calcula a volatilidade eleitoral, medindo as mudanças no comportamento dos eleitores entre diferentes eleições, comparando os resultados de diferentes anos.          |
| functions    | Funções Auxiliares     | `abstencao_percentual.R`            | Calcula o percentual de abstenção eleitoral a partir dos dados de votação.                                                             |
| functions    | Funções Auxiliares     | `alienacao_absoluta.R`              | Calcula o número absoluto de eleitores alienados (não participantes) em uma eleição.                                                  |
| functions    | Funções Auxiliares     | `alienacao_percentual.R`            | Calcula o percentual de eleitores alienados em uma eleição.                                                                            |
| functions    | Funções Auxiliares     | `desproporcionalidade_gallagher.R`  | Calcula o índice de desproporcionalidade de Gallagher, que mede a diferença entre a proporção de votos e cadeiras recebidas por partidos. |
| functions    | Funções Auxiliares     | `eleitos.R`                         | Identifica os candidatos eleitos a partir dos resultados de votação.                                                                   |
| functions    | Funções Auxiliares     | `fracionalizacao.R`                 | Calcula o índice de fracionalização partidária, que mede a fragmentação do sistema partidário.                                          |
| functions    | Funções Auxiliares     | `fracionalizacao_maxima.R`          | Calcula o índice de fracionalização máxima possível no sistema partidário.                                                              |
| functions    | Funções Auxiliares     | `fragmentacao.R`                    | Calcula o índice de fragmentação partidária, indicando a dispersão de votos entre diferentes partidos.                                  |
| functions    | Funções Auxiliares     | `indic_distribuicao_cadeiras.R`     | Calcula a distribuição de cadeiras entre partidos a partir dos resultados eleitorais.                                                   |
| functions    | Funções Auxiliares     | `indic_fragmentacao.R`              | Calcula os indicadores de fragmentação partidária.                                                                                         |
| functions    | Funções Auxiliares     | `indic_participacao_alienacao.R`    | Calcula os indicadores de participação e alienação eleitoral.                                                                           |
| functions    | Funções Auxiliares     | `indic_percentual_votos_cadeiras.R` | Calcula o percentual de votos e cadeiras conquistadas por partido.                                                                                   |
| functions    | Funções Auxiliares     | `indic_reeleicao.R`                 | Calcula os indicadores de reeleição.                                                                                        |
| functions    | Funções Auxiliares     | `indic_volatilidade.R`              | Calcula os indicadores de volatilidade eleitoral e parlamentar.                                                                                          |
| functions    | Funções Auxiliares     | `num_efetivo_partidos.R`            | Calcula o número efetivo de partidos.                                                                                                   |
| functions    | Funções Auxiliares     | `padroniz_fragmentacao.R`           | Padroniza os dados de fragmentação partidária.                                                                                          |
| functions    | Funções Auxiliares     | `padroniz_participacao_alienacao.R` | Padroniza os dados de participação e alienação eleitoral.                                                                               |
| functions    | Funções Auxiliares     | `padroniz_quociente_eleitoral.R` | Padroniza os dados de quociente eleitoral e número de eleitos por partido.                                                                      |
| functions    | Funções Auxiliares     | `padroniz_reeleicao.R`              | Padroniza os dados de reeleição.                                                                                                        |
| functions    | Funções Auxiliares     | `padroniz_volatilidade.R`           | Padroniza os dados de volatilidade eleitoral e parlamentar.                                                                                           |
| functions    | Funções Auxiliares     | `quociente_eleitoral.R`             | Calcula o quociente eleitoral.                                                                                                          |
| functions    | Funções Auxiliares     | `quociente_partidario.R`            | Calcula o quociente partidário.                                                                                                         |
| functions    | Funções Auxiliares     | `recandidaturas.R`                  | Identifica candidatos que se recandidataram.                                                                                            |
| functions    | Funções Auxiliares     | `reeleicao.R`                       | Calcula os índices de reeleição.                                                                                                          |
| functions    | Funções Auxiliares     | `reeleicao_liquida.R`               | Calcula a reeleição líquida.                                                                                                            |
| functions    | Funções Auxiliares     | `renovacao.R`                       | Calcula os índices de renovação.                                                                                                            |
| functions    | Funções Auxiliares     | `renovacao_liquida.R`               | Calcula a renovação líquida.                                                                                                            |
| functions    | Funções Auxiliares     | `volatilidade.R`                    | Calcula a volatilidade eleitoral e parlamentar.                                                                                                        |
| functions    | Funções Auxiliares     | `votos_brancos_percentuais.R`       | Calcula o percentual de votos brancos.                                                                                                  |
| functions    | Funções Auxiliares     | `votos_nulos_percentuais.R`         | Calcula o percentual de votos nulos.                                                                                                    |

### Linguagem e Idioma
Todo o processamento das bases de dados foi conduzido em linguagem R. Pacotes utilizados no tratamento dos dados estão especificados no arquivo `00_index.R`. Scripts, tabelas auxiliares e bases finais foram padronizadas em português (PT-BR).

## Contribuições 
O **CEPESP DATA** não é um projeto *open-source*, mas estamos sempre abertos a sugestões de melhorias! Caso tenha identificado algum erro em nossos *scripts* ou bases consolidadas, por favor, deposite seu comentário na aba de [*Issues*](https://github.com/GV-CEPESP/dados_eleitorais/issues) (no canto superior esquerdo).  

Checamos os *issues* com frequência. Se possível, não deixe de se identificar para que possamos retornar sua mensagem prontamente. 

