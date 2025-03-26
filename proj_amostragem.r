library(readxl)
library(dplyr)

set.seed(1)

#importando CAD
dataset = read_excel("CAD.xlsx")

#importando RESP
dataset2 = read_excel("RESP.xlsx")

#boxplots
boxplot(espvi00 ~ regiao, data=dataset, xlab="Regiões", ylab="EspVi 2000")

regioes = c("Norte", "Norde", "Centr", "Sudes", "Sul")

N = count(dataset)

#estimar tamanho da amostra idh
tam_amostra_idh = function() {
  tamanho = vector()
  #erro maximo admitido
  D = 0.0001 / 3.8416
  for (x in regioes){
    s2_h = var(subset(dataset, regiao==x)$idh00)
    N_h = count(subset(dataset, regiao==x))
    tamanho = append(tamanho, ceiling(1 / ((D/s2_h) + (1/N_h))) )
  }
  return(tamanho)
}

#estimar tamanho da amostra espvi
tam_amostra_espvi = function() {
  tamanho = vector()
  #erro maximo admitido
  D = 0.25 / 3.8416
  for (x in regioes){
    s2_h = var(subset(dataset, regiao==x)$espvi00)
    N_h = count(subset(dataset, regiao==x))
    tamanho = append(tamanho, ceiling(1 / ((D/s2_h) + (1/N_h))) )
  }
  return(tamanho)
}

#tamanho da amostra final, usando idh ja que tam_amostra_idh >
#tam_amostra_espvi
n = 0
for(x in tam_amostra_idh()) {n = n + x}

#tamanho do estrato
tam_estrato = function(){
  tam = vector()
  for (x in regioes){
    N_h = count(subset(dataset, regiao==x))
    tam = append(tam, round(n * (N_h/N), digits=0))
  }
  return (tam)
}

#amostra por estrato
amostra_norte = subset(dataset2, regiao=="Norte") %>% sample_n(tam_estrato()[[1]])

#retirei 1 elemento por conta de arredondamento, o valor
#exato era 219,56..., se não retirar a amostra tem tamanho n = 679

amostra_nordeste = subset(dataset2, regiao=="Norde")%>% sample_n(tam_estrato()[[2]] - 1)

amostra_co = subset(dataset2, regiao=="Centr") %>% sample_n(tam_estrato()[[3]])

amostra_sudes = subset(dataset2, regiao=="Sudes") %>% sample_n(tam_estrato()[[4]])

amostra_sul = subset(dataset2, regiao=="Sul") %>% sample_n(tam_estrato()[[5]])

#amostra com todas as regioes
amostra_final = rbind(amostra_norte, amostra_nordeste, amostra_co, amostra_sudes, amostra_sul)

#criando os estimadores

#estimador media idh
est_media_idh = function(){
  return(amostra_final$idh10 %>% mean())
}

#estimador media espvi
est_media_espvi = function(){
  return(amostra_final$espvi10 %>% mean())
}

#est variancia idh
est_var_idh = function(){
  s2 = 0
  for (x in regioes){
    n_h = count(subset(amostra_final, regiao == x))
    N_h = count(subset(dataset, regiao==x))
    W_h = N_h / N
    f_h = n_h/N_h
    s2_h = var(subset(amostra_final, regiao==x)$idh10)
    s2 = s2 + (W_h**2 * (1-f_h) * s2_h/n_h)
    }
  return(s2)
}

#est variancia espvi
est_var_espvi = function(){
  s2 = 0
  for (x in regioes){
    n_h = count(subset(amostra_final, regiao == x))
    N_h = count(subset(dataset, regiao==x))
    W_h = N_h / N
    f_h = n_h/N_h
    s2_h = var(subset(amostra_final, regiao==x)$espvi10)
    s2 = s2 + (W_h**2 * (1-f_h) * s2_h/n_h)
  }
  return(s2) 
}

#intervalo de confiança idh
int_conf_idh = function(){
  inf = est_media_idh() - 1.96 * sqrt(est_var_idh())
  sup = est_media_idh() + 1.96 * sqrt(est_var_idh())
  return(c(inf, sup))
  }

#intervalo de confiança espvi
int_conf_espvi = function(){
  inf = est_media_espvi() - 1.96 * sqrt(est_var_espvi())
  sup = est_media_espvi() + 1.96 * sqrt(est_var_espvi())
  return(c(inf, sup))
}

#valor populacional idh10
dataset2$idh10 %>% mean()

#valor populacional espvi10
dataset2$espvi10 %>% mean()
