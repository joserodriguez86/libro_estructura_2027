# Librerías y bases de datos-----------
pacman::p_load(tidyverse, ggsci)

eph <- readRDS("fuentes/eph_clase.rds")

theme_set(theme_minimal())


#IPC
ipc <- read.csv("https://infra.datos.gob.ar/catalog/sspm/dataset/145/distribution/145.1/download/indice-precios-al-consumidor-nivel-general-base-diciembre-2016-trimestral.csv")

ipc <- ipc %>%
  select(indice_tiempo, ipc_ng_nacional)

ipc$indice_tiempo <- ymd(ipc$indice_tiempo)

ipc <- ipc %>%
  mutate(
    ipc_actual = (ipc_ng_nacional / ipc_ng_nacional[which(ipc$indice_tiempo == ymd("2026-01-01"))] * 100),
    anio = year(indice_tiempo),
    trimestre = quarter(indice_tiempo),  # sin with_label
    trimestre = case_when(
      trimestre == 1 ~ "I",
      trimestre == 2 ~ "II",
      trimestre == 3 ~ "III",
      trimestre == 4 ~ "IV"
    )
  )


# Preprocesamiento-----

#Joins
eph <- eph %>%
  mutate(semestre = case_when(TRIMESTRE <= 2 ~ 1,
                              TRIMESTRE > 2 ~ 2),
         trim = case_when(TRIMESTRE == 1 ~ "I",
                          TRIMESTRE == 2 ~ "II",
                          TRIMESTRE == 3 ~ "III",
                          TRIMESTRE == 4 ~ "IV"),
         ano2 = str_sub(as.character(ANO4), 3, 4),
         ano_trim = paste(as.character(ANO4), as.character(trim), sep = "-"),
         ano_sem = paste(as.character(ANO4), as.character(semestre), sep = "-"),
         horas = PP3E_TOT + PP3F_TOT,
         horas = ifelse(horas <= 0 | horas > 168, NA_real_, horas))

ipc <- ipc %>%
  mutate(ano_trim = paste(as.character(anio), as.character(trimestre), sep = "-")) %>%
  select(ano_trim, ipc_actual)

eph <- eph %>%
  left_join(ipc, by = "ano_trim")

eph2010 <- eph %>%
  filter(ANO4 >= 2010)

eph2016 <- eph %>%
  filter(ANO4 >= 2016)

#ingresos

eph2016 <- eph2016 %>%
  group_by(ano_trim, CODUSU, NRO_HOGAR) %>%
  mutate(miembros = n(),
         ocupados = sum(ESTADO == 1) / sum(CH06 >= 14)) %>%
  ungroup() %>%
  mutate(ipcf2 = ITF / miembros,
         ipcf2024 = (ipcf2 / ipc_actual)*100,
         iti2024 = (P47T / ipc_actual)*100,
         p21_2024 = (P21 / ipc_actual)*100,
         ingresos_lab1 = ifelse(P21 == -9, 0, P21),
         ingresos_lab2 = ifelse(TOT_P12 == -9, 0, TOT_P12),
         ing_lab = ingresos_lab1 + ingresos_lab2,
         ing_horario = ing_lab / (horas * 4.3),
         ing_lab2024 = (ing_lab / ipc_actual)*100,
         ing_horario2024 = (ing_horario / ipc_actual)*100)


# Clase: stocks ------------

eph2010 %>%
  filter(CH06 >= 18 & !is.na(cobhe), cobhe < 9, TRIMESTRE == 1) %>%
  group_by(ANO4, cobhe_f) %>%
  tally(PONDERA) %>%
  group_by(ANO4) %>%
  mutate(prop = n/sum(n)) %>%
  ggplot(mapping = aes(x = ANO4, y = prop, fill = cobhe_f)) +
  geom_col() +
  geom_text(aes(label = if_else(prop >= 0.04,
                                scales::percent(prop, accuracy = 0.1, suffix = ""),
                                "")),
            position = position_stack(.5), size = 2.7) +
  scale_fill_d3(labels = function(x) str_wrap(x, width = 20)) +
  labs(caption = "Fuente: Elaboración propia en base a EPH-INDEC 2010-2026 (primeros trimestres).",
       fill = "Clase Social",
       title = "Las clases sociales en los últimos 16 años",
       subtitle = "Argentina urbana 2010-2026") +
  theme(legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 10),
        legend.key.height=unit(1, "cm"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 9),
        plot.caption = element_text(size = 10, hjust = 1),
        strip.text = element_text(face = "bold")) +
  scale_x_continuous(breaks = c(2010:2026)) +
  scale_y_continuous(labels= scales::percent_format(accuracy = 1L), breaks=seq(0, 1, 0.1))

ggsave("salidas/para jornadas/estructura_clases.png", width = 8, height = 5, dpi = 300)


# Clase: ingresos ---------
#sacar outliers en ingresos

identify_outliers <- function(data) {
  Q1 <- quantile(data, 0.25, na.rm = TRUE)
  Q3 <- quantile(data, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 2 * IQR
  upper_bound <- Q3 + 3.5 * IQR
  return(data > upper_bound)
}

eph_clean <- eph2016 %>%
  filter(CH06 >= 18, ESTADO == 1, !is.na(cobhe_f)) %>%
  group_by(ano_trim) %>%
  mutate(is_outlier = identify_outliers(ing_lab2024)) %>%
  ungroup() %>%
  filter(!is_outlier) %>%
  select(-is_outlier)


tabla <- eph_clean %>%
  group_by(ano_trim, cobhe_f) %>%
  summarise(ingresos= weighted.mean(ing_lab2024, PONDII, na.rm = T))

tabla_prom <- eph_clean %>%
  group_by(ano_trim) %>%
  summarise(ingresos = weighted.mean(ing_lab2024, PONDII, na.rm = T))

tabla %>%
  ggplot(aes(x = as.character(ano_trim), y = ingresos)) +
  geom_line(aes(group=cobhe_f, color=cobhe_f), linetype = 1, linewidth = .8, alpha = .6) +
  geom_point(aes(group=cobhe_f, color=cobhe_f), size = 1.5, alpha = .6) +
  geom_line(data = tabla_prom, linetype = "dotted", linewidth = .8, color = "black", group = 1) +
  labs(
    title = "Evolución de los ingresos laborales según clase social",
       subtitle = "Argentina urbana, 2016-2026. Población ocupada mayor a 18 años",
    caption = "Fuente: elaboración propia en base a EPH-INDEC.") +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        legend.key.height=unit(1, "cm"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        plot.caption = element_text(size = 9, hjust = 1),
        panel.grid = element_line(size = .2),
        strip.text = element_text(face = "bold"),
        legend.background = element_blank()) +
  scale_color_d3(labels = function(x) str_wrap(x, width = 20)) +
  scale_y_continuous(breaks = seq(0, 2600000, by = 100000), limits = c(0, 2600000),
                     labels = scales::label_number(scale = 1, accuracy = 1))

ggsave("salidas/para jornadas/ingresos_clases.png", width = 8, height = 5, dpi = 300)
