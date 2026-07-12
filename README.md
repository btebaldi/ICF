# ICF
Indice de Condicoes Financeiras



## Aquisicao de dados

0. Atualizar a curva de juros SWAP Pre-DI

Os dados de swap pre-di sao construidos a partir dos dados fornecidos pela B3. Disponivel em: https://www.b3.com.br/pt_br/market-data-e-indices/servicos-de-dados/market-data/consultas/mercado-de-derivativos/precos-referenciais/taxas-referenciais-bm-fbovespa/

1. Construcao dos dados de swap pre-di

Os dados sao baixados por um programa em python que realiza webscrapping dos dados. A periodicidade dos dados é diaria. Os dados possuem diversos vertices, não possuindo necessariamente vertices de 252, 2*252, 3*252, 4*252, 5* 252, 10*252. A partir dos dados diarios um processo de interpolacao (Perform cubic (or Hermite) spline interpolation of given data points, returning either a list of points obtained by the interpolation or a function performing the interpolation.) é utilizado para se obter diariamente os dados dos vertices  252, 2*252, 3*252, 4*252, 5* 252, 10*252.

A partir dos dados diarios de Swap Pre Di, as taxas sao agrupadas mensalmente utilizando uma media geometrica.

