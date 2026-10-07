library(shiny)
library(readxl)
library(dplyr)
library(tidyverse)
library(zoo)
library(ggplot2)
library(plotly)
library(scales)
library(lubridate)
library(rsconnect)
library(magick)
library(grid)
library(showtext)
library(shadowtext)

Sys.setlocale("LC_TIME", "es_ES.UTF-8")

contribuciones <- contribuciones_imacecnsa %>%
  filter(date >= max(date) - years(1))
imacec <- imacecnsa %>%
  filter(date >= max(date) - years(1))

imacec <- imacec %>%
  filter(code == "imacec") %>%
  mutate(
    code = factor(code, levels = "imacec", labels = "Imacec"),
    date = as.Date(date)
  )
contribuciones <- contribuciones %>% filter(code != "imacec") %>%
  mutate(
    code = factor(code, levels = codigos_imacec, labels = names(codigos_imacec)),
    date = as.Date(date)
  )

grafico_para_foto <- contribuciones %>%
  ggplot(aes(x = date, y = value)) +
  geom_col(aes(fill = code), linewidth = 0.01) +
  labs(
    x = NULL, y = NULL
    ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none"
    ) +
  scale_fill_manual(
    values = c(brewer.pal(length(unique(contribuciones$code)), "Set1"), "black")
  ) +
  scale_y_continuous(
    labels = label_number(big.mark = ".", decimal.mark = ",")
  ) +
  geom_line(
      data = imacec,
      aes(text = tooltip, color = code, group = code), linewidth = 1
    ) +
  scale_color_manual(
    values = c("Imacec" = "black")
  ) +
  geom_hline(yintercept = 0, alpha = 0.5)

# Guardar imagen

ggsave(
  filename = "grafico_imacec.jpg",
  plot = grafico_para_foto,
  width = 4.7,
  height = 4,
  dpi = 300
)
