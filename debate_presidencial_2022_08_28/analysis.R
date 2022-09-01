
# 0. Pacotes e dados -------------------------------------------------- 
# utilizarei esse pacote de lexicon em pt-br: devtools::install_github("sillasgonzaga/lexiconPT")
library(tidyverse)
library(scales)
library(tidytext)
library(showtext)
library(ggtext)
library(wordcloud)
library(reshape2)
library(lexiconPT)

#ler df que veio do script get_data.R
df_respostas <- 
  read_delim('data/dados_debate_presidencial_2022_08_28.csv',
             delim = ';')

## 1. Quem é mais mentiroso? ------------------------------------------
df_respostas %>% 
  filter(checado == 'sim') %>% 
  group_by(pessoa, selo) %>% 
  summarise(n = n()) %>% 
  spread(key = 'selo', value = 'n') %>% 
  mutate_all(.funs = ~ ifelse(is.na(.x), 0, .x)) %>% 
  mutate(total = falso + `não é bem assim` + verdadeiro,
         pct_falso = percent(falso/total) )


## 2. Quem falou mais? -----------------------------------------------

### 2.1. Considerando stopwords:
contagem_palavras <- 
  df_respostas %>%
  filter(!pessoa %in% c("MEDIADORES", "JORNALISTA", "MODERADORES",
                        "INTERVALO")) %>% 
  select(pessoa, paragrafo) %>% 
  unnest_tokens(word, paragrafo) %>% 
  group_by(pessoa) %>% 
  count(sort = T) %>% 
  ungroup()

contagem_palavras

### 2.1. Desconsiderando stopwords:
stopwords_df <- 
  tidytext::get_stopwords(language = 'pt') %>% 
  bind_rows(tibble(word = c('é','pra')))

contagem_palavras_sem_stop <- 
  df_respostas %>%
  filter(!pessoa %in% c("MEDIADORES", "JORNALISTA", "MODERADORES",
                        "INTERVALO")) %>% 
  select(pessoa, paragrafo) %>% 
  unnest_tokens(word, paragrafo) %>% 
  anti_join(stopwords_df) %>% 
  group_by(pessoa) %>% 
  count(sort = T) %>% 
  ungroup()

stopwords_df

### 2.3 juntando os dois e fazendo um plot

font_add_google(name = 'Poppins', family = 'poppins')
showtext_auto()

left_join(contagem_palavras,
          contagem_palavras_sem_stop,
          by='pessoa') %>%
  setNames(c("Candidato", 
             "<p style='color:#999999'>Considerando <i>stopwords</i>* </p>",
             "<p style='color:#0D5DB6'>Desconsiderando <i>stopwords</i>* </p>")) %>% 
  gather(key = 'tipo', value = 'Palavras', -Candidato) %>% 
  group_by(tipo) %>% 
  arrange(desc(Palavras)) %>% 
  mutate(posicao = 1:n(),
         lab1 = paste(posicao, '-', Candidato),
         lab2 = paste(tools::toTitleCase(tolower(Candidato)), '-', posicao)) %>%
  ungroup() %>% 
  arrange(lab1) %>% 
  mutate(lab1 = factor(lab1, rev(unique(lab1))),
         tipo = factor(tipo,
                       levels = c("<p style='color:#999999'>Considerando <i>stopwords</i>* </p>",
                                  "<p style='color:#0D5DB6'>Desconsiderando <i>stopwords</i>* </p>"))) %>% 
  ggplot(aes(lab1, Palavras)) +
  geom_col(aes(fill = tipo), show.legend = F) +
  scale_fill_manual(values = c("grey60", "#0D5DB6")) +
  geom_text(aes(label = lab2), hjust = 1.05, col = 'white',
            size = 8) +
  facet_wrap(~tipo, scales = 'free') +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        text = element_text(family = 'poppins', size = 24),
        axis.text.x = element_text(colour = "grey60"),
        axis.title.x = element_text(size = 20),
        plot.caption = element_markdown(lineheight = 1),
        strip.text = element_markdown()) +
  labs(caption = "Dados extraídos do portal  **aosfatos.org**",
       y = 'Quantidade de palavras no debate') 

ggsave('plots/plot_palavras_debate.png')  

## 3. O que cada um falou? -------------------------------------------

### wordcloud de termos + frequentes por candidato

simplifica_nomes_candidatos <- function(nome){
  case_when(str_detect(nome, 'LULA') ~ 'LULA',
            str_detect(nome, 'BOLSONARO') ~ 'BOLSONARO',
            str_detect(nome, 'SIMONE') ~ 'SIMONE',
            str_detect(nome, 'SORAYA') ~ 'SORAYA',
            str_detect(nome, 'CIRO') ~ 'CIRO',
            str_detect(nome, "D'ÁVILA") ~ "D'ÁVILA")
}

df_wordcloud <- 
  df_respostas %>%
  filter(!pessoa %in% c("MEDIADORES", "JORNALISTA", "MODERADORES",
                        "INTERVALO")) %>% 
  mutate(pessoa = simplifica_nomes_candidatos(pessoa)) %>% 
  select(pessoa, paragrafo) %>% 
  unnest_tokens(word, paragrafo) %>% 
  anti_join(stopwords_df) %>% 
  group_by(pessoa, word) %>% 
  count(sort = T)

### salvando wordcloud em arquivo
png(filename="plots/wordcloud_candidatos.png", 
    width = 1200, height = 1200,
    res = 650, family='poppins')

df_wordcloud %>% 
  acast(word ~ pessoa, value.var = "n", fill = 0) %>% 
  comparison.cloud(max.words=Inf,random.order=FALSE,
                   family='poppins')
dev.off()


## 4. Análise de sentimentos ---------------------------------------------

### nao vou usar nada mt complexo, apenas um dicionario de polaridade para unigram
oplexicon <- 
  lexiconPT::oplexicon_v3.0 %>% 
  select(term, polarity) %>% 
  setNames(c('word', 'polarity_op'))

df_polaridade <- 
  df_respostas %>%
  filter(!pessoa %in% c("MEDIADORES", "JORNALISTA", "MODERADORES",
                        "INTERVALO")) %>% 
  mutate(pessoa = simplifica_nomes_candidatos(pessoa)) %>% 
  select(pessoa, paragrafo) %>% 
  unnest_tokens(word, paragrafo) %>% 
  anti_join(stopwords_df) %>% 
  left_join(oplexicon)


classifica_polaridade <- function(score){
  case_when(score == -1 ~ 'Negativo',
            score == 0 ~ 'Neutro',
            score == 1 ~ 'Positivo')
}

df_polaridade %>% 
  mutate(polarity_op = classifica_polaridade(polarity_op)) %>% 
  group_by(pessoa) %>% 
  count(polarity_op) %>% 
  filter(!is.na(polarity_op)) %>% 
  mutate(pct = n/sum(n)) %>% 
  ungroup() %>% 
  arrange(polarity_op, pct) %>% 
  mutate(pessoa = factor(pessoa, unique(pessoa)),
         polarity_op = factor(polarity_op, rev(unique(polarity_op)))) %>% 
  ggplot(aes(pessoa, pct, fill=polarity_op)) +
  geom_col() +
  scale_fill_manual('Sentimento',
                    values = c('#f5d745', 'grey85', '#0D5DB6')) +
  coord_flip() +
  theme_minimal() +
  scale_y_continuous(labels = percent_format(decimal.mark = ',')) +
  labs(x = 'Candidato', 
       y = 'Representatividade das palavras no discurso') +
  theme(legend.position = 'top',
        text = element_text(family = 'poppins', size = 24),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(reverse=T))

ggsave('plots/plot_polaridade_palavras.png', width = 5)

## 5. Topic Modeling -----------------------------------------------------
## ideia: usar lda para clusterizar os topicos/candidatos 
## status: a fazer
