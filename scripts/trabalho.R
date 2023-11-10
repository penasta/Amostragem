# Pacotes ----

source("scripts/source/packages.R")

# Carregando os dados ----

dail <- read_excel("banco/Banco_Grupo3.xlsx")
guilherme <- read_excel("banco/Banco_Grupo3_Estante 3.xlsx",
                        col_types = c("text", "text", "numeric",
                                      "numeric", "text", "skip", "text",
                                      "skip", "skip", "skip", "skip", "numeric"))
bruno <- read_excel("banco/trabalho_amostragem_grupo3_bruno.xlsx")

# ETL ----

# Dail (Prateleira 1) ----

colnames(dail)[5] <- "Descrição_tipo_avaria"
dail <- head(dail,25)
dail <- dail |>
  mutate(Descrição_tipo_avaria = case_when(
    Descrição_tipo_avaria == "Sem avaria" ~ NA,
    Descrição_tipo_avaria == "Uso de lápis" ~ "Riscos",
    Descrição_tipo_avaria == "Oxidação" ~ "Oxidação/Costura"),
    Descrição_avaria = ifelse(Avaria == 1,"Avaria","Sem avaria"),
    Tipo_avaria = case_when(
      Descrição_tipo_avaria == NA ~ NA,
      Descrição_tipo_avaria == "Riscos" ~ 3,
      Descrição_tipo_avaria == "Oxidação/Costura" ~ 2)
    )
dail <- dail[,c(1,2,3,4,6,7,5)]
dail[25,1] <- "Classe 2"
dail$Prateleira <- 1

# Giulia (Prateleira 2) ----

# Guilherme (Prateleira 3) ----

colnames(guilherme)[5] <- "Descrição_tipo_avaria"
guilherme <- guilherme |>
  select(!c(`Endereço 2`,`número sorteado`)) |>
  mutate(Descrição_avaria = ifelse(Avaria == 1,"Avaria","Sem avaria"),
         Descrição_tipo_avaria = case_when(
           Descrição_tipo_avaria == "Sem avaria" ~ NA,
           Descrição_tipo_avaria == "uso de lápis" ~ "Riscos",
           Descrição_tipo_avaria == "oxidação" ~ "Oxidação/Costura",
           Descrição_tipo_avaria == "costura" ~ "Oxidação/Costura",
           Descrição_tipo_avaria == "costura aparente" ~ "Oxidação/Costura",
           Descrição_tipo_avaria == "capa" ~ "Capa"),
         Tipo_avaria = case_when(
           Descrição_tipo_avaria == NA ~ NA,
           Descrição_tipo_avaria == "Riscos" ~ 3,
           Descrição_tipo_avaria == "Oxidação/Costura" ~ 2,
           Descrição_tipo_avaria == "Capa" ~ 1)
         )
guilherme <- guilherme[,c(1,2,3,4,6,7,5)]

guilherme$Prateleira <- 3


# Bruno (Prateleira 4) ----

colnames(bruno) <- c("Classe","Endereço","Exemplar","Avaria",
                     "Descrição_avaria","Tipo_avaria","Descrição_tipo_avaria")

bruno$Prateleira <- 4

# Consolidação ----

df = rbind(dail,guilherme,bruno)
df$Classe <- factor(df$Classe)
df$Avaria <- factor(df$Avaria)
df$Descrição_avaria <- factor(df$Descrição_avaria)
df$Tipo_avaria <- factor(df$Tipo_avaria)
df$Descrição_tipo_avaria <- factor(df$Descrição_tipo_avaria)
df$Prateleira <- factor(df$Prateleira)

df <- df |>
  mutate(Descrição_avaria = case_when(
    Descrição_avaria == "Sem Avaria" ~ "Sem avaria",
    Descrição_avaria == "Sem avaria" ~ "Sem avaria",
    Descrição_avaria == "Avaria" ~ "Avaria"))

# Exportando ----

write.xlsx(df,"banco/grupo3.xlsx")

