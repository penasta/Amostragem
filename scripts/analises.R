# Pacotes ----
source("scripts/source/packages.R")

# Dados ----
df <- read_excel("banco/grupo3.xlsx",
                     col_types = c("skip",
                                   "text", "text", "text","text",
                                   "text","text", "text", "text"))

# ETL ----
colnames(df)
df$Classe <- factor(df$Classe)
df$Avaria <- as.numeric(df$Avaria)
df$Descrição_avaria <- factor(df$Descrição_avaria)
df$Tipo_avaria <- factor(df$Tipo_avaria)
df$Descrição_tipo_avaria <- factor(df$Descrição_tipo_avaria)
df$Prateleira <- factor(df$Prateleira)

# Análises ----
p_load(samplingbook)

# N = População de livros na Classe 2 - Religião - na BCE.
Sprop(y=df$Avaria)
Sprop(y=df$Avaria,N=197+2*1500)
