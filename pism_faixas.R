#scaneando coisas do pism

library(tabulizer)
library(tidyverse)


#construindo funcao

leitora_pdf <- function(pdf){
  data.frame(tabulizer::extract_tables(pdf)[1]) %>% 
    slice(n()) %>% 
    set_names('de 0 a 40', 'de 40 a 80', 'de 80 a 120', 'de 120 a 160', 
              'de 160 a 200', 'de 200 a 240') 
}

#pegando os pdfs
pdf15<- 'https://www2.ufjf.br/copese/wp-content/uploads/sites/42/2015/10/FAIXAS-PONTUA%C3%87%C3%83O-PISM1E2-2016.pdf'
pdf16 <- 'https://www2.ufjf.br/copese/wp-content/uploads/sites/42/2016/08/faixas_pontuacao_pism1_pism2_2017.pdf'
pdf17<-'https://www2.ufjf.br/copese/wp-content/uploads/sites/42/2017/05/faixas_pontuacao_pism1_pism2_2018.pdf'
pdf18<- 'https://www2.ufjf.br/copese/wp-content/uploads/sites/42/2018/04/faixas_pontuacao_pism1_pism2_2019.pdf' 
pdf19 <- 'https://www2.ufjf.br/copese/wp-content/uploads/sites/42/2020/03/faixas_pontuacao_pism1_pism2_2020.pdf'

#aplicar

lista_pism_anos <- map(list(pdf15,pdf16,pdf17,pdf18,pdf19), leitora_pdf) %>% 
  set_names('2015':'2019') %>% 
  bind_rows() %>% 
  mutate(ano = 2015:2019)


base2<- lista_pism_anos %>%
  mutate(across(everything(), as.numeric)) %>% 
  mutate(mais_metade = `de 120 a 160`+ `de 160 a 200` + `de 200 a 240`,
         menos_metade = `de 0 a 40` + `de 40 a 80` + `de 80 a 120`) %>%
  mutate(prop_menos = (menos_metade/(mais_metade+menos_metade))*100,
         prop_mais = (mais_metade/(mais_metade+menos_metade))*100) %>% 
  select(mais_metade, menos_metade, prop_mais, prop_menos, ano)

base3 <- lista_pism_anos %>% 
  mutate(across(everything(), as.numeric),
         alto = `de 160 a 200` + `de 200 a 240`,
         baixo = `de 0 a 40` + `de 40 a 80`,
         medio = `de 80 a 120` + `de 120 a 160`)  
  

#exportar

rio::export(lista_pism_anos, file = 'Desktop/pism/faixas.csv')
rio::export(base2, file = 'Desktop/pism/faixas_mais_menos.csv')

rio::export(base3,'Desktop/pism/3niveis.csv' )


#36489e
#a50c0c

