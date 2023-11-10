# 1.0 Pacotes ----
source("scripts/source/packages.R")

# 1.1 Dados ----
df <- read_excel("banco/grupo3.xlsx",
                     col_types = c("skip",
                                   "text", "text", "text","text",
                                   "text","text", "text", "text"))

# 1.2 ETL ----
colnames(df)
df$Classe <- factor(df$Classe)
df$Avaria <- as.numeric(df$Avaria)
df$Descrição_avaria <- factor(df$Descrição_avaria)
df$Tipo_avaria <- factor(df$Tipo_avaria)
df$Descrição_tipo_avaria <- factor(df$Descrição_tipo_avaria)
df$Prateleira <- factor(df$Prateleira)

# 2 Análises ----
# 2.0 Exploratória ----
# 2.0.1 Tabela completa em LaTeX ----
p_load(xtable)
xtable(df)

# 2.0.2 Tabela de contingência do tipo de avaria pela prateleira - LaTeX ----
table(df$Descrição_tipo_avaria,df$Prateleira)

# 2.0.3 Gráfico: Tipo de avaria pela prateleira ----
df$Descrição_tipo_avaria <- as.character(df$Descrição_tipo_avaria)
df %>%
  select(Descrição_tipo_avaria, Prateleira) %>%
  mutate(Descrição_tipo_avaria = ifelse(is.na(Descrição_tipo_avaria), "Sem avaria", Descrição_tipo_avaria)) %>%
  group_by(Descrição_tipo_avaria, Prateleira) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = freq %>% percent(),
    porcentagens = str_c(freq_relativa, "%") %>% str_replace("\\.", ","),
    legendas = str_squish(str_c(freq, " (", porcentagens, ")"))
    ) %>%
  ggplot() +
  aes(
    x = fct_reorder(Descrição_tipo_avaria, freq, .desc = T),
    y = freq,
    fill = Prateleira,
    label = legendas
    ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 2) +
  scale_fill_manual(values = cores_unb)+
  labs(x = "Tipo de avaria", y = "Frequência") +
  theme_minimal()
#ggsave("resultados/grafico1.pdf", width = 158, height = 93, units = "mm")


# 2.0.4 Proporção avaria ----

contagem2 <- df %>%
  mutate(Descrição_avaria = ifelse(Descrição_avaria == "Sem avaria", "Sem avaria","Com avaria")) %>%
  group_by(Descrição_avaria) %>%
  summarise(Freq = n()) %>%
  mutate(Prop = round(100 * (Freq / sum(Freq)), 2)) %>%
  arrange(desc(Descrição_avaria)) %>%
  mutate(posicao = cumsum(Prop) - 0.5 * Prop,
         ymax = cumsum(Prop),
         ymin = c(0, head(ymax, n=-1)))

ggplot(contagem2) +
  aes(
    x = factor(""),
    y = Prop,
    fill = factor(Descrição_avaria)
  ) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = cores_unb,name = "") +
  theme_void() +
  geom_text(
    aes(x = 1.8, y = posicao, label = paste0(Prop, "%")),
    color = "black"
  )
#ggsave("resultados/grafico2.pdf", width = 158, height = 93, units = "mm")


# 2.0.5 Proporção tipo de avaria ----
contagem <- df %>%
  mutate(Descrição_tipo_avaria = ifelse(is.na(Descrição_tipo_avaria), "Sem avaria", Descrição_tipo_avaria)) %>%
  group_by(Descrição_tipo_avaria) %>%
  summarise(Freq = n()) %>%
  mutate(Prop = round(100 * (Freq / sum(Freq)), 2)) %>%
  arrange(desc(Descrição_tipo_avaria)) %>%
  mutate(posicao = cumsum(Prop) - 0.5 * Prop,
         ymax = cumsum(Prop),
         ymin = c(0, head(ymax, n=-1)))

ggplot(contagem) +
  aes(
    x = factor(""),
    y = Prop,
    fill = factor(Descrição_tipo_avaria)
  ) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = cores_unb,name = "Descrição tipo avaria") +
  theme_void() +
  geom_text(
    aes(x = 1.8, y = posicao, label = paste0(Prop, "%")),
    color = "black"
  )
#ggsave("resultados/grafico3.pdf", width = 158, height = 93, units = "mm")

# 2.0.6 Proporção tipo de avaria - tirando "sem avaria" ----
contagem3 <- df %>%
  na.omit() %>%
  group_by(Descrição_tipo_avaria) %>%
  summarise(Freq = n()) %>%
  mutate(Prop = round(100 * (Freq / sum(Freq)), 2)) %>%
  arrange(desc(Descrição_tipo_avaria)) %>%
  mutate(posicao = cumsum(Prop) - 0.5 * Prop,
         ymax = cumsum(Prop),
         ymin = c(0, head(ymax, n=-1)))

ggplot(contagem3) +
  aes(
    x = factor(""),
    y = Prop,
    fill = factor(Descrição_tipo_avaria)
  ) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = cores_unb,name = "Descrição tipo avaria") +
  theme_void() +
  geom_text(
    aes(x = 1.8, y = posicao, label = paste0(Prop, "%")),
    color = "black"
  )
#ggsave("resultados/grafico4.pdf", width = 158, height = 93, units = "mm")


# 2.1 Proporção estimada na população, com intervalo de confiança; estatística pontual e erro padrão. ----
p_load(samplingbook)
Sprop(y=df$Avaria)

# 2.1.1 Mesmo, porém "chutando" um valor para N ----
# N = População de livros na Classe 2 - Religião - na BCE.
Sprop(y=df$Avaria,N=197+2*1500)

# 2.2.1 Verificando se a avaria pode ser explicada pelo tipo da avaria + qual prateleira o livro foi coletado
summary(aov(Avaria ~ Tipo_avaria + Prateleira,data=df)) # Não significativo

# 2.2.2 Verificando se a avaria pode ser explicada por qual prateleira o livro foi coletado ----
df %>%
  select(Descrição_avaria, Prateleira) %>%
  group_by(Descrição_avaria, Prateleira) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = freq %>% percent(),
    porcentagens = str_c(freq_relativa, "%") %>% str_replace("\\.", ","),
    legendas = str_squish(str_c(freq, " (", porcentagens, ")"))
  ) %>%
  ggplot() +
  aes(
    x = fct_reorder(factor(Prateleira), freq, .desc = T),
    y = freq,
    fill = Descrição_avaria,
    label = legendas
  ) +
  labs(fill='')  +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 2) +
  scale_fill_manual(values = cores_unb)+
  labs(x = "Prateleira", y = "Frequência") +
  theme_minimal()
#ggsave("resultados/grafico5.pdf", width = 158, height = 93, units = "mm")


anova = aov(Avaria ~ Prateleira,data=df)
summary(anova) # O teste anova indica que a prateleira em que o livro foi encontrado pode explicar o tipo de avaria.
xtable(summary(anova))
# Pressupostos do teste

# 2.2.2.1 Normalidade dos resíduos ----
shapiro.test(anova$residuals) # Não são normais
qqnorm(anova$residuals)
qqline(anova$residuals)

ggplot(anova, aes(sample=.resid)) +
  stat_qq(colour=cores_unb[1], size = 2) +
  stat_qq_line(size = 0.8, colour = cores_unb[2]) +
  labs(x="Quantis da Normal", y="Quantis amostrais", title = "Normalidade") 
#ggsave("resultados/grafico6.pdf", width = 158, height = 93, units = "mm")



# 2.2.2.2 Independência ----
plot(anova$residuals)
plot(anova$residuals~anova$fitted.values)
# Não aparentam ser independentes

ggplot(anova, aes(x=c(1:length(anova$residuals)),y=.resid)) +
  geom_point(colour=cores_unb[1], size=3) +
  geom_hline(yintercept=0,colour=cores_unb[2]) +
  labs(x="", y="Resíduos", title = "Independência") 
#ggsave("resultados/grafico7.pdf", width = 158, height = 93, units = "mm")

# 2.2.2.3 Homocedasticidade ----
pacman::p_load(car)
leveneTest(y=df$Avaria,group=df$Prateleira)
# Variâncias homogêneas.

residuos <- anova$residuals
valores_ajustados <- anova$fitted.values

ggplot(data = data.frame(fitted.values = valores_ajustados, residuos = residuos),
       mapping = aes(x = valores_ajustados, y = residuos)) +
  geom_point(colour=cores_unb[1], size=3) +
  scale_x_continuous() +
  labs(x="Valor Ajustado",
       y="Resíduos", title = "Homocedasticidade") 
#ggsave("resultados/grafico8.pdf", width = 158, height = 93, units = "mm")


# 2.2.3 Teste não paramétrico - Kruskall-Wallis ----
kruskal.test(Avaria ~ Prateleira,data=df) # Pelo teste não paramétrico de Kruskall-Wallis, concluímos que existem diferenças entre a quantidade de avarias nas prateleiras.

# 2.2.4 Verificando se o tipo de avaria é homogêneo entre as prateleiras
p_load(stats)
chisq.test(df$Tipo_avaria,df$Prateleira
#           , simulate.p.value = TRUE,B=10000
           )
# O teste qui-quadrado indica que O tipo de avaria é independente da prateleira (H0 não rejeitado).

# 2.2.5 Diagrama de Sankey: Proporção de livros avariados/não avariados por cada prateleira ----

p_load(ggalluvial)
prop <- df |>
  select(Descrição_avaria,Prateleira) |>
  count(Descrição_avaria, Prateleira) |>
  mutate(proptot = prop.table(n),
         Descrição_avaria = ifelse(Descrição_avaria == "Avaria","Com avaria","Sem avaria"))

ggplot(as.data.frame(prop),
       aes(y = proptot, axis1 = factor(Descrição_avaria), axis2 = factor(Prateleira))) +
  geom_alluvium(aes(fill = factor(Descrição_avaria)), width = 1/12,alpha=.8,show.legend = FALSE) +
  geom_stratum(width = 1/12, fill = cores_unb[4], colour = cores_unb[3],alpha=1) +
  geom_label(stat = "stratum", infer.label = TRUE) +
  scale_x_discrete(limits = c("Avaria", "Prateleira"),
                   expand = c(.05, .05),
                   labels = c("Avaria", "Prateleira")) +
  scale_fill_manual(values = cores_unb) +
  scale_y_continuous(labels = NULL,
                     name = NULL,
                     breaks = NULL) +
  theme_minimal()
#ggsave("resultados/grafico9.pdf", width = 158, height = 93, units = "mm")


# 2.2.6 Diagrama de Sankey: Tipo de avaria por cada prateleira ----

prop2 <- df |>
  select(Descrição_tipo_avaria,Prateleira) |>
  mutate(Descrição_tipo_avaria = ifelse(is.na(Descrição_tipo_avaria), "Sem avaria", Descrição_tipo_avaria)) %>%
  count(Descrição_tipo_avaria, Prateleira) |>
  mutate(proptot = prop.table(n))

ggplot(as.data.frame(prop2),
       aes(y = proptot, axis1 = factor(Descrição_tipo_avaria), axis2 = factor(Prateleira))) +
  geom_alluvium(aes(fill = factor(Descrição_tipo_avaria)), width = 1/12,alpha=.8,show.legend = FALSE) +
  geom_stratum(width = 1/12, fill = cores_unb[4], colour = cores_unb[3],alpha=1) +
  geom_label(stat = "stratum", infer.label = TRUE) +
  scale_x_discrete(limits = c("Avaria", "Prateleira"),
                   expand = c(.05, .05),
                   labels = c("Avaria", "Prateleira")) +
  scale_fill_manual(values = rev(cores_unb)) +
  scale_y_continuous(labels = NULL,
                     name = NULL,
                     breaks = NULL) +
  theme_minimal()
# ficou feio esse, não pretendo usar.