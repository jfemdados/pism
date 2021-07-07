#The best way to get into Litoral's Federal: PISM
library(tidyverse)
library(tabulizer)

#importing
pdf_16_18 <- ('https://www2.ufjf.br/copese/wp-content/uploads/sites/42/2018/04/PISM-2019-ponto-de-corte-PISM-3.pdf')

pdf_17_19 <- ('https://www2.ufjf.br/copese/wp-content/uploads/sites/42/2020/01/pontosdecorte_pism3_2020.pdf')


#declaring
faz_tudo <- function(nometabela,ntabela){
  tabela1<-data.frame(nometabela[[ntabela]]) %>% 
    slice(-1:-3) %>% 
    set_names('campus', 'curso', 'erro','Grupo A', 'Grupo A1','Grupo B','Grupo B1','Grupo D',
              'Grupo D1','Grupo E','Grupo E1','Grupo C','Grupo F') %>% 
    select(campus,curso, `Grupo C`) %>% 
    separate(sep = '\\s',
             col = `Grupo C`,
             into = c('nota_max_c','nota_min_c'),
             extra = 'merge') 
    
  }


tratamento_1 <- function(x){
  x%>% 
    mutate(campus = case_when(curso == "ODONTOLOGIA (integral)" ~"GOVERNADOR VALADARES")) %>% 
    fill(campus, .direction = 'up') %>% 
    mutate(campus = case_when(is.na(campus) ~ "JUIZ DE FORA",
                              TRUE ~campus)) %>% 
    filter(!nota_min_c == "")
}



#opening --------------------------------------------------
tables18<- tabulizer::extract_tables(pdf_16_18)

tables19<-tabulizer::extract_tables(pdf_17_19)






#wrangling ---------------------

#tables 18
initial_table<- faz_tudo(tables18, 1) %>% 
  tratamento_1()
        
final_table<-faz_tudo(tables18, 2) %>% 
  bind_rows(faz_tudo(tables18, 3)) %>% 
  filter(!nota_min_c == "") %>% 
  mutate(campus = "JUIZ DE FORA") %>% 
  bind_rows(initial_table)

#tables 19

initial_table19 <- faz_tudo(tables19,1) %>% 
  tratamento_1()


tabela <- data.frame(tables19[3])






final_table<-faz_tudo(2)

 %>% 
  bind_rows(faz_tudo(3)) %>% 
  filter(!nota_min_c == "") %>% 
  mutate(campus = "JUIZ DE FORA") %>% 
  bind_rows(initial_table)

















#links -------------------------
#notas de corte
#  https://www2.ufjf.br/noticias/2020/10/30/confira-pontos-de-corte-das-edicoes-passadas-do-pism/

#distribuicao das notas de cada modulo
# https://www2.ufjf.br/copese/wp-content/uploads/sites/42/2018/04/faixas_pontuacao_pism1_pism2_2019.pdf

















