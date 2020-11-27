# Limpiar lista
rm(list=ls())

# Directorio
setwd("xxx")

# Paquetes
packages <- c("tm", "tidyverse", "tidytext", "lubridate", "zoo", "scales", "tokenizers", "ggplot2", "hrbrthemes", "viridis", "dplyr")

# Instalar paquetes no instalados
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Cargar paquetes
lapply(packages, library, character.only = TRUE)

# Cargar texto
texto <- read_file("path/texto.txt")

# Convertir en palabras individuales
texto_words <- tokenize_words(texto)

# Pasar a data frame
texto <- data.frame(matrix(unlist(texto_words), ncol=length(texto_words), byrow=T))
names(texto) <- c("texto")

# En vector characters
texto <- data.frame(lapply(texto, as.character), stringsAsFactors=FALSE)

# Léxico
download.file("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/lexico_afinn.en.es.csv",
              "lexico_afinn.en.es.csv")
afinn <- read.csv("lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% 
  tbl_df()

# Análisis positivo/negativo
texto_afinn <- 
  texto %>%
  unnest_tokens(input = "texto", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa"))

# Gráfico proporción
p <- texto_afinn %>%
  count(Tipo) %>%
  mutate(Proporcion = n / sum(n)) %>%
  ggplot() +
  aes(factor(1), Proporcion, fill = Tipo) +
  geom_col() +
  scale_y_continuous(labels = percent_format()) +
  theme(legend.position = "right")
tiff("texto1.tiff", units="in", width=3, height=3.5, res=300)
p + labs(x = "Texto 1")
dev.off()

# BoxPlot
tiff("texto2.tiff", units="in", width=3, height=3.5, res=300)
ggplot(texto_afinn, aes(x = factor(1), y = Puntuacion)) +
  geom_boxplot(width = 0.4, fill = "white") +
  geom_jitter(aes(color = Tipo, shape = Tipo), 
              width = 0.1, size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + 
  labs(x = NULL)   # Remove x axis label
dev.off()
