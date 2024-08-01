
### CEPESP INDICADORES - REPOSITÓRIO DE INDICADORES ELEITORAIS

Repositório de indicadores eleitorais desenvolvido pelo Centro de Política e Economia do Setor Público (CEPESP) da Fundação Getulio Vargas (FGV) a partir dos dados disponibilizados no _CEPESP DATA_.

### Organização do Repositório

Este repositório se organiza nas seguintes pastas:

- **scripts**: códigos para cálculo dos indicadores eleitorais:
  - `00_index.R`: configura o ambiente de trabalho e carrega os pacotes necessários.
  - `01_data.R`: importa e prepara os dados brutos.
  - `02_calc_quociente_eleitoral_partidario.R`: calcula índices de quociente eleitoral e número de eleitos por partido.
  - `03_calc_fragmentacao.R`: calcula índices de fragmentação partidária.
  - `04_calc_participacao_alienacao.R`: calcula índices de participação e alienação eleitoral.
  - `05_calc_reeleicao.R`: calcula índices de reeleição.
  - `06_calc_volatilidade.R`: calcula índices de volatilidade eleitoral e parlamentar.

- **data**: bases de dados utilizadas nos scripts:
  - `input`: dados brutos extraídos de fontes diversas.
  - `output`: dados processados e consolidados.
  
- **functions**: funções auxiliares utilizadas nos scripts principais:
  - `abstencao_percentual.R`: calcula o percentual de abstenção eleitoral.
  - `alienacao_absoluta.R`: calcula o número absoluto de eleitores alienados.
  - `alienacao_percentual.R`: calcula o percentual de eleitores alienados.
  - `desproporcionalidade_gallagher.R`: calcula o índice de desproporcionalidade de Gallagher.
  - `eleitos.R`: identifica os candidatos eleitos.
  - `fracionalizacao.R`: calcula o índice de fracionalização partidária.
  - `fracionalizacao_maxima.R`: calcula o índice de fracionalização máxima.
  - `fragmentacao.R`: calcula o índice de fragmentação partidária.
  - `indic_cadeiras_conquistadas.R`: calcula o número de cadeiras conquistadas por partido.
  - `indic_distribuicao_cadeiras.R`: calcula a distribuição de cadeiras entre partidos.
  - `indic_fragmentacao.R`: calcula o indicador de fragmentação partidária.
  - `indic_participacao_alienacao.R`: calcula os indicadores de participação e alienação eleitoral.
  - `indic_percentual_votos_cadeiras.R`: calcula o percentual de votos e cadeiras por partido.
  - `indic_reeleicao.R`: calcula o indicador de reeleição dos candidatos.
  - `indic_volatilidade.R`: calcula o indicador de volatilidade eleitoral.
  - `num_efetivo_partidos.R`: calcula o número efetivo de partidos.
  - `padroniz_fragmentacao.R`: padroniza os dados de fragmentação partidária.
  - `padroniz_participacao_alienacao.R`: padroniza os dados de participação e alienação eleitoral.
  - `padroniz_quociente_eleitoral_partidario.R`: padroniza os dados de quociente eleitoral e partidário.
  - `padroniz_reeleicao.R`: padroniza os dados de reeleição.
  - `padroniz_volatilidade.R`: padroniza os dados de volatilidade eleitoral.
  - `quociente_eleitoral.R`: calcula o quociente eleitoral.
  - `quociente_partidario.R`: calcula o quociente partidário.
  - `recandidaturas.R`: identifica candidatos que se recandidataram.
  - `reeleicao.R`: calcula as taxas de reeleição.
  - `reeleicao_liquida.R`: calcula a reeleição líquida.
  - `renovacao.R`: calcula a taxa de renovação.
  - `renovacao_liquida.R`: calcula a renovação líquida.
  - `volatilidade.R`: calcula a volatilidade eleitoral e parlamentar.
  - `votos_brancos_percentuais.R`: calcula o percentual de votos brancos.
  - `votos_nulos_percentuais.R`: calcula o percentual de votos nulos.

### Códigos existentes na pasta e seus objetivos

| Subdiretório | Etapa                  | Arquivo                             | Descrição                                                                                                                          |
|--------------|------------------------|-------------------------------------|------------------------------------------------------------------------------------------------------------------------------------|
| scripts      | Coleta e Processamento | `00_index.R`                        | Configura o ambiente de trabalho, carrega os pacotes necessários e define funções utilitárias.                                      |
| scripts      | Importação de Dados    | `01_data.R`                         | Importa os dados brutos, realizando limpeza e transformação para um formato adequado às análises posteriores.                       |
| scripts      | Cálculo de Indicadores | `02_calc_quociente_eleitoral_partidario.R` | Calcula o quociente eleitoral e partidário, agrupando os votos por partidos, calculando o total de votos válidos e aplicando as fórmulas necessárias. |
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
| functions    | Funções Auxiliares     | `indic_cadeiras_conquistadas.R`     | Calcula o número de cadeiras conquistadas por cada partido.                                                                             |
| functions    | Funções Auxiliares     | `indic_distribuicao_cadeiras.R`     | Calcula a distribuição de cadeiras entre partidos a partir dos resultados eleitorais.                                                   |
| functions    | Funções Auxiliares     | `indic_fragmentacao.R`              | Calcula o indicador de fragmentação partidária.                                                                                         |
| functions    | Funções Auxiliares     | `indic_participacao_alienacao.R`    | Calcula os indicadores de participação e alienação eleitoral.                                                                           |
| functions    | Funções Auxiliares     | `indic_percentual_votos_cadeiras.R` | Calcula o percentual de votos e cadeiras por partido.                                                                                   |
| functions    | Funções Auxiliares     | `indic_reeleicao.R`                 | Calcula o indicador de reeleição dos candidatos.                                                                                        |
| functions    | Funções Auxiliares     | `indic_volatilidade.R`              | Calcula o indicador de volatilidade eleitoral.                                                                                          |
| functions    | Funções Auxiliares     | `num_efetivo_partidos.R`            | Calcula o número efetivo de partidos.                                                                                                   |
| functions    | Funções Auxiliares     | `padroniz_fragmentacao.R`           | Padroniza os dados de fragmentação partidária.                                                                                          |
| functions    | Funções Auxiliares     | `padroniz_participacao_alienacao.R` | Padroniza os dados de participação e alienação eleitoral.                                                                               |
| functions    | Funções Auxiliares     | `padroniz_quociente_eleitoral_partidario.R` | Padroniza os dados de quociente eleitoral e partidário.                                                                      |
| functions    | Funções Auxiliares     | `padroniz_reeleicao.R`              | Padroniza os dados de reeleição.                                                                                                        |
| functions    | Funções Auxiliares     | `padroniz_volatilidade.R`           | Padroniza os dados de volatilidade eleitoral.                                                                                           |
| functions    | Funções Auxiliares     | `quociente_eleitoral.R`             | Calcula o quociente eleitoral.                                                                                                          |
| functions    | Funções Auxiliares     | `quociente_partidario.R`            | Calcula o quociente partidário.                                                                                                         |
| functions    | Funções Auxiliares     | `recandidaturas.R`                  | Identifica candidatos que se recandidataram.                                                                                            |
| functions    | Funções Auxiliares     | `reeleicao.R`                       | Calcula as taxas de reeleição.                                                                                                          |
| functions    | Funções Auxiliares     | `reeleicao_liquida.R`               | Calcula a reeleição líquida.                                                                                                            |
| functions    | Funções Auxiliares     | `renovacao.R`                       | Calcula a taxa de renovação.                                                                                                            |
| functions    | Funções Auxiliares     | `renovacao_liquida.R`               | Calcula a renovação líquida.                                                                                                            |
| functions    | Funções Auxiliares     | `volatilidade.R`                    | Calcula a volatilidade eleitoral.                                                                                                        |
| functions    | Funções Auxiliares     | `votos_brancos_percentuais.R`       | Calcula o percentual de votos brancos.                                                                                                  |
| functions    | Funções Auxiliares     | `votos_nulos_percentuais.R`         | Calcula o percentual de votos nulos.                                                                                                    |

### Linguagem e Idioma

Todas as manipulações das bases de dados foram conduzidas em linguagem R. Pacotes utilizados no tratamento dos dados estão especificados no arquivo `00_index.R`. Scripts, tabelas auxiliares e bases finais foram padronizadas em português (PT-BR).
