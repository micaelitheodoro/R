#? Comentários com "?" são comentários normais
#! Comentários com "!" são códigos errados
#* Comentários com "*" são para correções
# Apenas "#" são códigos comentados (ignorados)
#TODO é algo para fazermos juntos

#? Vamos adicionar a biblioteca dplyr 
library(dplyr)

#? E outras bibliotecas que serão úteis
library(lubridate)
library(stringr)
library(ggplot2)
#? Vamos começar com os dados de pokemon
#? https://www.kaggle.com/datasets/igorcoelho24/pokemon-all-generations/versions/1?resource=download
dados <- read.csv("Dados/Pokemon_full.csv")
head(dados) #? vê as primeiras linhas de dados


#? A biblioteca dplyr possui o operador "pipe"
#? dado por  %>%
#? Ele "pega" tudo que está à esquerda dele e coloca como primeiro elemento
#? da função à direita.
#? Também é possível usar o operador "."
#? para especificar onde ele deve substituir.


#? Exemplo: contar o número de linhas de dados

nrow(dados)
dados %>% nrow()
dados %>% nrow(.)

#? Algumas funções da biblioteca dplyr

#? A função filter seleciona linhas com base em um teste
filter(dados, type == "grass")

#? podemos usar o seguinte comando também
#dados %>% filter(type == "grass")


#Filtrar tipo fogo ou água
filter(dados, type == "fire"|type =="water")

#dados %>% filter(type == "grass")
#TODO Vamos filtrar todos os pokemons do tipo fogo ou água

#TODO Vamos filtrar todos os pokemons que tem  "fly"
filter(dados,secundary.type == "flying")
#TODO Vamos filtrar todos os pokemons que tem  "bee" ou "saur"
#filter(dados,names == "bee"|names=="saur")
texto_filtrado <- dados[str_detect(dados$name, "bee|saur"),]

# Imprimir o resultado
texto_filtrado
dados%>% filter(grepl("bee",name)|grepl("saur",name)) 
mutate(dados,height2=2*height)

dados<-dados%>%
  mutate(
    height2=2*height,
    speed2=2*speed)
#? A função pull devolve um vetor
dados
dados %>% pull(name)
dados%>%
  group_by(type,secundary.type)%>%
  summarise(
    media_altura=mean(height),
    media_peso=mean(weight),
    N=n()
  )%>%arrange(media_altura,media_peso)
dados%>%
  group_by(type)%>%
  mutate(
    media_altura=mean(height)
  )->df
write.csv(df,"df.csv")
dados%>%
  group_by(type)%>%
  mutate(
    media_altura=mean(height)
  )%>%
  filter(height>media_altura)%>%
select(-media_altura)->df
write.csv(df,"df.csv")



#? A função select seleciona colunas
dados %>% select(c(1, 2, 3)) #? pelo número
dados %>% select(name, type, height) #? pelo nome

#TODO achar todas as combinações existentes de type e secondary.type

#? Outras possibilidades
dados %>% names
dados %>%
    select(starts_with("h")) %>% head #? starts_with, ends_with, contains

dados %>% select(-name) %>% head #? negativo exclui as colunas

#? A função mutate modifica ou cria uma coluna com base em outras

dados %>% 
    mutate(
        height2 = 2*height
    )

#? A função arrange organiza o data frame com base em colunas

dados %>%
    arrange(name) %>%
        head()

dados %>%
    arrange(name) %>%
        tail()

dados %>%
    arrange(desc(name)) %>%
        tail()

#? Vamos fazer algumas contas!!

dados %>%
    summarise(
        media_altura = mean(height),
        media_peso = mean(weight)
    )

#? Podemos fazer isso por grupos
dados %>%
    group_by(type) %>%
        summarise(
            media_altura = mean(height),
            media_peso = mean(weight),
            N = n()
        ) %>%
            arrange(media_altura)

#TODO Filtrar os pokemons que tem o peso acima da média do seu type

#TODO criar uma coluna com a transformação Z-score POR type utilizando TODAS
#TODO as variáveis quantitativas

#Tarefa para 27/04/2023
dados %>%
  group_by(type) %>%
  mutate(scale(height))%>%
View
#Usando a função zscore direto
df<-dados %>% 
  group_by(type) %>% 
  mutate(
    Zscore1 = 
      (height - mean(height)) / sd(height),
  Zscore2 = 
  (weight - mean(weight)) / sd(weight)) %>%   
  View

ggplot(df)+
  geom_density(aes(x = zscore1, color = type))+
  theme_bw()

#? Renomear colunas
dados %>%
    group_by(type) %>%
        summarise(
            media_altura = mean(height),
            media_peso = mean(weight),
            N = n()
        ) %>%
            arrange(media_altura) %>%
                rename("Número de pokemons" = N)

#? Movê-las
dados %>%
    group_by(type) %>%
        summarise(
            media_altura = mean(height),
            media_peso = mean(weight),
            N = n()
        ) %>%
            arrange(media_altura) %>%
                rename("Número de pokemons" = N) %>%
                relocate("Número de pokemons")
dados %>%
    group_by(type) %>%
        summarise(
            media_altura = mean(height),
            media_peso = mean(weight),
            N = n()
        ) %>%
            arrange(media_altura) %>%
                rename("Número de pokemons" = N) %>%
                relocate("Número de pokemons", .after = type)


#? rowwise

#? A função mutate e outras do pacote dplyr trabalham diretamente com as colunas
#? como se fossem operações de vetor

dados %>%
    mutate(
        name2 = paste(name, " - NOVO")
    ) %>% head

#? Na prática, o dplyr faz isso:
paste(dados$name, "- NOVO")

#? o que significa que a função PRECISA aceitar um vetor

#? Imagine que você queira criar uma função que testa se o valor de uma coluna
#? na observação i é maior ou menor que um dado valor e executa uma certa ação.

f <- function(x){
    if(x <= 15){ #? no caso, o valor é 300
        return("Executei essa ação")
    }else{

        return("Executei Aquela ação")
    }
}

#! O código abaixo não funciona
dados %>%
    mutate(
        nova_var = f(height)
    ) %>%
        select(height, nova_var) %>% head(30)

#* O código abaixo funciona
#TODO

#? ifelse e case_when

dados %>%
    mutate(
        tamanho = ifelse(
            height < 15,
            "baixinho",
            "altão"
        )
    ) %>% head

dados %>%
    mutate(
        tamanho = case_when(
            height < 5 ~ "baixinho",
            height < 10 ~ "pequeno",
            height < 15 ~ "médio",
            TRUE ~ "altão"
        )
    ) %>% head

#? Alguns perrengues da vida

#! O código abaixo não funciona
dados %>%
    mutate(
        tamanho = case_when(
            height < 5 ~ "baixinho",
            height < 10 ~ "pequeno",
            height < 15 ~ NA,
            TRUE ~ "altão"
        )
    ) %>% head

#* O código abaixo conserta isso
#TODO


#? Vamos falar de JOIN

df_means <- dados %>%
    group_by(type) %>%
    summarise(
        media_h = mean(height),
        media_w = mean(weight)
    )
df_means

#? vamos excluir os grupos que começam com "g"
#TODO

#? vamos adicionar um grupo que não existe

novo_grupo <- data.frame(
    type = "Vozes da minha cabeça",
    media_h = 1000,
    media_w = 400.82
)

#TODO adicionar o grupo

#? full_join
#TODO

#? inner_join
#TODO

#? left_join
#TODO

#? right_join
#TODO



#? vamos adicionar um grupo que JÁ existe

novo_grupo <- data.frame(
    type = "bug",
    media_h = 10,
    media_w = 800
)

#TODO adicionar o grupo

#? left_join
#TODO

#? right_join
#TODO


#?#####################################################################
#? TIDYR
#?#####################################################################
#Tarefa para 25/05/2023
library(tidyr)
library(readr)#não sei se tinha necessidade, mas o meu não foi sem isso
#? baixado de https://livro.curso-r.com/

dados <- readr::read_rds("Dados/imdb.rds")
orçamento<-dados$orcamento
receita<-dados$receita
receita_eua<-dados$receita_eua
#cbind(orçamento,receita,receita_eua)

indice_correlação<-cor(cbind(orçamento,receita,receita_eua),use="pairwise.complete.obs")

print(indice_correlação)

#Outra forma de fazer a tarefa de correlação
dados%>%
  select_if(is.numeric)%>%
  filter(complete.cases(.))%>%
  cor()

dados <- readr::read_rds("Dados/imdb.rds")
View(dados)
head(dados)
names(dados)

df <- dados %>% 
  select(titulo, orcamento, receita, receita_eua)
df
#? Pivoteamento

#? pivot_wider

#? pivot_longer


#? Emissões de ar
#? https://www.kaggle.com/datasets/ashishraut64/global-methane-emissions


#? Netflix
#? https://www.kaggle.com/datasets/victorsoeiro/netflix-tv-shows-and-movies