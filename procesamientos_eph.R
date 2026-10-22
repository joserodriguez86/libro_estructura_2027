# Librerías y bases de datos

eph <- readRDS("fuentes/eph_clase.rds")

pacman::p_load(tidyverse, janitor, openxlsx, Hmisc, sf, ggthemes, effectsize)

#IPC
ipc <- read.csv("https://infra.datos.gob.ar/catalog/sspm/dataset/145/distribution/145.1/download/indice-precios-al-consumidor-nivel-general-base-diciembre-2016-trimestral.csv")


# Preprocesamiento ----------------

ipc <- ipc %>%
  select(indice_tiempo, ipc_ng_nacional)

ipc$indice_tiempo <- ymd(ipc$indice_tiempo)

ipc <- ipc %>%
  mutate(
    ipc_actual = (ipc_ng_nacional / ipc_ng_nacional[which(ipc$indice_tiempo == ymd("2025-04-01"))] * 100), #PONER ACÁ EL TRIMESTRE DESDE DONDE SE DEFLACTA
    anio = year(indice_tiempo),
    trimestre = quarter(indice_tiempo),  # sin with_label
    trimestre = case_when(
      trimestre == 1 ~ "I",
      trimestre == 2 ~ "II",
      trimestre == 3 ~ "III",
      trimestre == 4 ~ "IV"
    )
  )

#variables de fecha
eph <- eph %>%
  mutate(semestre = case_when(TRIMESTRE <= 2 ~ 1,
                              TRIMESTRE > 2 ~ 2),
         trim = case_when(TRIMESTRE == 1 ~ "I",
                          TRIMESTRE == 2 ~ "II",
                          TRIMESTRE == 3 ~ "III",
                          TRIMESTRE == 4 ~ "IV"),
         ano2 = str_sub(as.character(ANO4), 3, 4),
         ano_trim = paste(as.character(ANO4), as.character(trim), sep = "-"),
         ano_sem = paste(as.character(ANO4), as.character(semestre), sep = "-"))

ipc <- ipc %>%
  mutate(ano_trim = paste(as.character(anio), as.character(trimestre), sep = "-")) %>%
  select(ano_trim, ipc_actual)

eph <- eph %>%
  left_join(ipc, by = "ano_trim")

#Variables construidas
eph <- eph %>%
  filter(CH04 > 0) %>%
  mutate(sexo = factor(CH04, labels = c("Varón", "Mujer")),
         region_f = factor(REGION, labels = c("GBA", "NOA", "NEA", "Cuyo", "Pampeana", "Patagonia")),
         vivienda = case_when(II7 == 1 | II7 == 2 ~ 1,
                              TRUE ~ 0),
         ambiental = case_when(IV12_1 == 1 | IV12_2 == 1 | IV12_3 == 1 ~ 1,
                               TRUE ~ 0),
         calmat = case_when(IV3 == 1 & IV4 == 1 & IV5 == 1 ~ 1,
                            IV3 == 1 & IV4 == 1 & IV5 == 2 ~ 2,
                            IV3 == 1 & (IV4 >= 2 & IV4 <= 5) ~ 2,
                            IV3 == 1 & (IV4 >= 6 & IV4 <= 7) ~ 3,
                            IV3 == 2 & IV4 == 1 & IV5 == 1 ~ 2,
                            IV3 == 2 & IV4 == 1 & IV5 == 2 ~ 2,
                            IV3 == 2 & (IV4 >= 2 & IV4 <= 5) ~ 3,
                            IV3 == 2 & (IV4 >= 6 & IV4 <= 7) ~ 3,
                            IV3 == 3 & IV4 == 1 & IV5 == 1 ~ 3,
                            IV3 == 3 & IV4 == 1 & IV5 == 2 ~ 3,
                            IV3 == 3 & (IV4 >= 2 & IV4 <= 5) ~ 3,
                            IV3 == 3 & (IV4 >= 6 & IV4 <= 7) ~ 3,
                            TRUE ~ NA_real_ ),
         calmat_dic = car::recode(calmat, "1:2=1; 3=0"),
         incalserv = case_when(IV7 == 1 & IV11 == 1 ~ 1,
                               TRUE ~ 0),
         educ_superior = case_when((CH12 == 6 | CH12 == 7) & CH13 == 1 ~ 1,
                                   CH12 == 8 ~ 1,
                                   TRUE ~ 0),
         educ_secundaria = case_when((CH12 == 4 | CH12 == 5) & CH13 == 1 ~ 1,
                                     (CH12 >= 6 & CH12 <= 8) ~ 1,
                                     TRUE ~ 0),
         IX_MEN10 = ifelse(ANO4 == 2003 & is.na(IX_MEN10), 0, IX_MEN10),
         no_lab = ifelse(T_VI < 0, 0, T_VI),
         ipcf = ifelse(ANO4 == 2020, ITF/IX_TOT, IPCF),
         horas = PP3E_TOT + PP3F_TOT,
         horas = ifelse(horas <= 0 | horas > 168, NA_real_, horas)) %>%
  group_by(ANO4, TRIMESTRE, CODUSU, NRO_HOGAR) %>%
  mutate(ing_nolab_tot = sum(no_lab),
         menores0_14 = ifelse(CH06 < 14, 1, 0),
         menores0_14 = sum(menores0_14)) %>%
  ungroup()

#ingresos
eph <- eph %>%
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

eph$cobhe_f <- factor(eph$cobhe,
                      labels = c('Propietarios y directivos >5',
                                 'Propietarios y directivos <=5',
                                 'Cuenta propia profesionales / calificados',
                                 'Trabajadores no manuales >5',
                                 'Trabajadores manuales >5',
                                 'Trabajadores no manuales <=5',
                                 'Trabajadores manuales <=5',
                                 'Cuenta propia no calificados',
                                 'Inactivos'))

# Saco a los inactivos del análisis
# eph <- eph %>%
#   filter(cobhe != 9)





#Preparación de mapa--------------
argentina <- st_read("C:/Users/josed/OneDrive/Otros/proyectos_R/mapas/Codgeo_Pais_x_dpto_con_datos/argentina_n.shp")
argentina <- argentina %>%
  filter(!link %in% c("94028"))

provincias <- st_read("C:/Users/josed/OneDrive/Otros/proyectos_R/mapas/Codgeo_Pais_x_prov_datos/provincias_n.shp")

partidos <- c("06028", "06035", "06091", "06134", "06252", "06260", "06270", "06274", "06364", "06371",
              "06408", "06410", "06412", "06427", "06434", "06490", "06515", "06525", "06539", "06560",
              "06568", "06638", "06648", "06658", "06749", "06756", "06760", "06778", "06805", "06840",
              "06861")

argentina$link <- as.character(argentina$link)
argentina$codpcia <- as.numeric(argentina$codpcia)

argentina <- argentina %>%
  mutate(REGION = case_when(codpcia == 2 | (link %in% partidos) ~ 1,
                            codpcia == 10 | codpcia == 38 | codpcia == 46 | codpcia == 66 |
                              codpcia == 90 | codpcia == 86 ~ 40,
                            codpcia == 18 | codpcia == 22 | codpcia == 54 | codpcia == 34 ~ 41,
                            codpcia == 50 | codpcia == 70 | codpcia == 74 ~ 42,
                            (codpcia == 6 & !(link %in% partidos)) | codpcia == 30 | codpcia == 14
                            | codpcia == 82 | codpcia == 42 ~ 43,
                            codpcia == 58 | codpcia == 62 | codpcia == 26 | codpcia == 78 |
                              codpcia == 94 ~ 44,
                            TRUE ~ NA_real_))

options(OutDec= ",")

# 1) Estructura de clases ----------------
## Stocks con inactivos-----------------
### Dominancia--------------------
tabla <- eph %>%
  filter(dominancia == 1 & CH06 >= 18) %>%
  group_by(ANO4, cobhe_f) %>%
  tally(PONDERA) %>%
  pivot_wider(id_cols = cobhe_f, names_from = ANO4, values_from = n,
              names_vary = "slowest")

suma <- tabla %>%
  summarise(cobhe_f = "N",
            across(where(is.numeric), sum, na.rm = TRUE))


cuadro <- tabla %>%
  adorn_totals("row") %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1, affix_sign = F) %>%
  rbind(suma) %>%
  as.data.frame()

write.xlsx(cuadro, "salidas/procesamientos_eph_2022.xlsx", sheetName = "estructura_año_hog", rowNames = F, append = T)



### Individuos--------------
tabla <- eph %>%
  filter(CH06 >= 18 & !is.na(cobhe)) %>%
  group_by(ANO4, cobhe_f) %>%
  tally(PONDERA) %>%
  pivot_wider(id_cols = cobhe_f, names_from = ANO4, values_from = n,
              names_vary = "slowest")

suma <- tabla %>%
  summarise(cobhe_f = "N",
            across(where(is.numeric), sum, na.rm = TRUE))

cuadro <- tabla %>%
  adorn_totals("row") %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1, affix_sign = F) %>%
  rbind(suma) %>%
  as.data.frame()

write.xlsx(cuadro, "salidas/procesamientos_eph_2022.xlsx", sheetName = "estructura_año_ind", rowNames = F, append = T)


## Stocks sin inactivos-----------------
### Dominancia--------------------
tabla <- eph %>%
  filter(dominancia == 1 & CH06 >= 18 & cobhe != 9) %>%
  group_by(ANO4, cobhe_f) %>%
  tally(PONDERA) %>%
  pivot_wider(id_cols = cobhe_f, names_from = ANO4, values_from = n,
              names_vary = "slowest")

suma <- tabla %>%
  summarise(cobhe_f = "N",
            across(where(is.numeric), sum, na.rm = TRUE))

cuadro <- tabla %>%
  adorn_totals("row") %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1, affix_sign = F) %>%
  rbind(suma) %>%
  as.data.frame()

write.xlsx(cuadro, "salidas/procesamientos_eph_2022.xlsx", sheetName = "estructura_año_hog_PEA", rowNames = F, append = T)

### Individuos--------------
tabla <- eph %>%
  filter(CH06 >= 18 & !is.na(cobhe) & cobhe != 9) %>%
  group_by(ANO4, cobhe_f) %>%
  tally(PONDERA) %>%
  pivot_wider(id_cols = cobhe_f, names_from = ANO4, values_from = n,
              names_vary = "slowest")

suma <- tabla %>%
  summarise(cobhe_f = "N",
            across(where(is.numeric), sum, na.rm = TRUE))

cuadro <- tabla %>%
  adorn_totals("row") %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1, affix_sign = F) %>%
  rbind(suma) %>%
  as.data.frame()

write.xlsx(cuadro, "salidas/procesamientos_eph_2022.xlsx", sheetName = "estructura_año_ind_PEA", row.names = F, append = T)

## Género ---------------
### Dominancia --------------
tabla_varon <- eph %>%
  filter(dominancia == 1 & CH04 == 1 & CH06 >= 18) %>%
  group_by(ANO4, cobhe_f) %>%
  tally(PONDERA) %>%
  spread(ANO4, value = n)

tabla_mujer <- eph %>%
  filter(dominancia == 1 & CH04 == 2 & CH06 >= 18) %>%
  group_by(ANO4, cobhe_f) %>%
  tally(PONDERA) %>%
  spread(ANO4, value = n)

tabla <- tabla_varon %>%
  left_join(tabla_mujer, by = "cobhe_f", suffix = c("-varón", "-mujer"))

sum_tab <- c("N", colSums(tabla[,-1]))

cuadro <- tabla %>%
  adorn_totals("row") %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1, affix_sign = F) %>%
  rbind(sum_tab) %>%
  as.data.frame()

write.xlsx(cuadro, "salidas/procesamientos_eph_2022.xlsx", sheetName = "estructura_sexo_dom", row.names = F, append = T)

### Individuos ----------------
tabla_varon <- eph %>%
  filter(CH04 == 1 & CH06 >= 18 & !is.na(cobhe)) %>%
  group_by(ANO4, cobhe_f) %>%
  tally(PONDERA) %>%
  spread(ANO4, value = n)

tabla_mujer <- eph %>%
  filter(CH04 == 2 & CH06 >= 18 & !is.na(cobhe)) %>%
  group_by(ANO4, cobhe_f) %>%
  tally(PONDERA) %>%
  spread(ANO4, value = n)

tabla <- tabla_varon %>%
  left_join(tabla_mujer, by = "cobhe_f", suffix = c("-varón", "-mujer"))

sum_tab <- c("N", colSums(tabla[,-1]))

cuadro <- tabla %>%
  adorn_totals("row") %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1, affix_sign = F) %>%
  rbind(sum_tab) %>%
  as.data.frame()

write.xlsx(cuadro, "salidas/procesamientos_eph_2022.xlsx", sheetName = "estructura_sexo_ind", row.names = F, append = T)


## Edad -----------------
tabla <- eph %>%
  filter(CH06 >= 18 & !is.na(cobhe)) %>%
  group_by(ANO4, cobhe_f) %>%
  dplyr::summarise(edad = round(weighted.mean(CH06, PONDERA), digits = 2),
            sd = round(sqrt(wtd.var(CH06, PONDERA)), digits = 2),
            CI = 1.96 * (sd / sqrt(n()))) %>%
  pivot_wider(id_cols = cobhe_f, names_from = ANO4, values_from = c("edad", "sd", "CI"),
              names_vary = "slowest")

sum_tab <- eph %>%
  filter(CH06 >= 18) %>%
  group_by(ANO4) %>%
  dplyr::summarise(edad = round(weighted.mean(CH06, PONDERA), digits = 2),
            sd = round(sqrt(wtd.var(CH06, PONDERA)), digits = 2),
            CI = 1.96 * (sd / sqrt(n()))) %>%
  pivot_wider(names_from = ANO4, values_from = c("edad", "sd", "CI"),
              names_vary = "slowest") %>%
  add_column(cobhe_f = "Promedio total", .before = "edad_2003")

sum_tab1 <- eph %>%
  filter(CH06 >= 18 & cobhe != 9) %>%
  group_by(ANO4) %>%
  dplyr::summarise(edad = round(weighted.mean(CH06, PONDERA), digits = 2),
            sd = round(sqrt(wtd.var(CH06, PONDERA)), digits = 2),
            CI = 1.96 * (sd / sqrt(n()))) %>%
  pivot_wider(names_from = ANO4, values_from = c("edad", "sd", "CI"),
              names_vary = "slowest") %>%
  add_column(cobhe_f = "Promedio PEA", .before = "edad_2003")

cuadro <- as.data.frame(rbind(tabla, sum_tab, sum_tab1))

write.xlsx(cuadro, "salidas/procesamientos_eph_2022.xlsx", sheetName = "estructura_edad", row.names = F, append = T)


# Región ------------------
## Dominancia -----------------
tabla <- eph %>%
  filter(dominancia == 1 & CH06 >= 18) %>%
  group_by(ANO4, region_f, cobhe_f) %>%
  tally(PONDERA)%>%
  pivot_wider(id_cols = cobhe_f, names_from = c("ANO4", "region_f"), values_from = n,
              names_vary = "slowest")

sum_tab <- c("N", colSums(tabla[,-1]))

cuadro <- tabla %>%
  adorn_totals("row") %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1, affix_sign = F) %>%
  rbind(sum_tab) %>%
  as.data.frame()

write.xlsx(cuadro, "salidas/procesamientos_eph_2022.xlsx", sheetName = "estructura_region_hog", row.names = F, append = T)


### Individuos -----------------
tabla <- eph %>%
  filter(CH06 >= 18 & !is.na(cobhe)) %>%
  group_by(ANO4, region_f, cobhe_f) %>%
  tally(PONDERA)%>%
  pivot_wider(id_cols = cobhe_f, names_from = c("ANO4", "region_f"), values_from = n,
              names_vary = "slowest")

sum_tab <- c("N", colSums(tabla[,-1]))

cuadro <- tabla %>%
  adorn_totals("row") %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1, affix_sign = F) %>%
  rbind(sum_tab) %>%
  as.data.frame()

write.xlsx(cuadro, "salidas/procesamientos_eph_2022.xlsx", sheetName = "estructura_region_ind", row.names = F, append = T)


## Mapa --------------------
base_long_2003 <- eph %>%
  filter(ANO4 == 2003 & dominancia == 1 & CH06 >= 18) %>%
  group_by(REGION, cobhe_f) %>%
  tally(wt=PONDERA) %>%
  group_by(REGION) %>%
  mutate(percent = n/sum(n))

mapa_2003 <- merge(argentina, base_long_2003, by="REGION", all.x = TRUE)

ggplot() +
  geom_sf(data = mapa_2003, aes(group = REGION, fill = percent), color = NA) +
  geom_sf(data = provincias, fill = NA, color = NA) +
  theme_map() +
  labs(fill = "% Clase \nsocial") +
  theme(legend.title = element_text(size = 9, face = "bold", colour = "white"),
        legend.text = element_text(size = 8, colour = "white"),
        legend.position = c(1.15,.5),
        legend.justification = "center",
        legend.key = element_rect(fill = "#2D2D2D"),
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 9, hjust = 1),
        panel.background = element_rect(fill = "#2D2D2D"),
        strip.text = element_text(face = "bold", size = 7.5),
        legend.background = element_blank(),
        legend.box.background = element_rect(fill = "#2D2D2D", colour = "black")) +
  scale_fill_viridis_c(direction = -1, labels= scales::percent_format(accuracy = 1L),
                       guide = "colourbar") +
  facet_wrap(~cobhe_f, nrow = 2, labeller = labeller(cobhe_f = label_wrap_gen(15)))

ggsave(filename = "salidas/clase_region_2003.png", dpi = 400, type = "cairo", width = 8, height = 6)

base_long_2022 <- eph %>%
  filter(ANO4 == 2022 & dominancia == 1 & CH06 >= 18) %>%
  group_by(REGION, cobhe_f) %>%
  tally(wt=PONDERA) %>%
  group_by(REGION) %>%
  mutate(percent = n/sum(n))

mapa_2022 <- merge(argentina, base_long_2022, by="REGION", all.x = TRUE)

ggplot() +
  geom_sf(data = mapa_2022, aes(group = REGION, fill = percent), color = NA) +
  geom_sf(data = provincias, fill = NA, color = NA) +
  theme_map() +
  labs(fill = "% Clase \nsocial") +
  theme(legend.title = element_text(size = 9, face = "bold", colour = "white"),
        legend.text = element_text(size = 8, colour = "white"),
        legend.position = c(1.15,.5),
        legend.justification = "center",
        legend.key = element_rect(fill = "#2D2D2D"),
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 9, hjust = 1),
        panel.background = element_rect(fill = "#2D2D2D"),
        strip.text = element_text(face = "bold", size = 7.5),
        legend.background = element_blank(),
        legend.box.background = element_rect(fill = "#2D2D2D", colour = "black")) +
  scale_fill_viridis_c(direction = -1, labels= scales::percent_format(accuracy = 1L),
                       guide = "colourbar") +
  facet_wrap(~cobhe_f, nrow = 2, labeller = labeller(cobhe_f = label_wrap_gen(15)))

ggsave(filename = "salidas/clase_region_2022.png", dpi = 400, type = "cairo", width = 8, height = 6)


# 2) Vivienda y hábitat-----------------
## Propiedad de la vivienda------------------------
tabla <- eph %>%
  filter(dominancia == 1 & CH06 >= 18) %>%
  group_by(ANO4, cobhe_f) %>%
  summarise(propiedad = round(weighted.mean(vivienda, PONDERA)*100, digits = 2)) %>%
  spread(ANO4, value = propiedad)

sum_tab1 <- eph %>%
  filter(dominancia == 1 & CH06 >= 18) %>%
  group_by(ANO4) %>%
  summarise(propiedad = round(weighted.mean(vivienda, PONDERA)*100, digits = 2)) %>%
  spread(ANO4, value = propiedad) %>%
  add_column(cobhe_f = "Promedio total", .before = "2003")

sum_tab2 <- eph %>%
  filter(dominancia == 1 & CH06 >= 18 & vivienda == 1) %>%
  group_by(ANO4) %>%
  tally(PONDERA) %>%
  spread(ANO4, value = n) %>%
  add_column(cobhe_f = "N", .before = "2003")

sum_tab3 <- eph %>%
  filter(dominancia == 1 & CH06 >= 18 & cobhe != 9) %>%
  group_by(ANO4) %>%
  summarise(propiedad = round(weighted.mean(vivienda, PONDERA)*100, digits = 2)) %>%
  spread(ANO4, value = propiedad) %>%
  add_column(cobhe_f = "Promedio PEA", .before = "2003")

sum_tab4 <- eph %>%
  filter(dominancia == 1 & CH06 >= 18 & vivienda == 1 & cobhe != 9) %>%
  group_by(ANO4) %>%
  tally(PONDERA) %>%
  spread(ANO4, value = n) %>%
  add_column(cobhe_f = "N PEA", .before = "2003")

cuadro <- as.data.frame(rbind(tabla, sum_tab1, sum_tab2, sum_tab3, sum_tab4))

write.xlsx(cuadro, "salidas/procesamientos_eph_2022.xlsx", sheetName = "vivienda_prop", row.names = F, append = T)

## Tamaño del hogar------------------
tabla <- eph %>%
  filter(dominancia == 1 & CH06 >= 18) %>%
  group_by(ANO4, cobhe_f) %>%
  dplyr::summarise(miembros = round(weighted.mean(IX_TOT, PONDERA), digits = 2),
            sd = round(sqrt(wtd.var(IX_TOT, PONDERA)), digits = 2),
            CI = 1.96 * (sd / sqrt(n()))) %>%
  pivot_wider(id_cols = cobhe_f, names_from = ANO4, values_from = c("miembros", "sd", "CI"),
              names_vary = "slowest")

sum_tab1 <- eph %>%
  filter(dominancia == 1 & CH06 >= 18) %>%
  group_by(ANO4) %>%
  dplyr::summarise(miembros = round(weighted.mean(IX_TOT, PONDERA), digits = 2),
            sd = round(sqrt(wtd.var(IX_TOT, PONDERA)), digits = 2),
            CI = 1.96 * (sd / sqrt(n()))) %>%
  pivot_wider(names_from = ANO4, values_from = c("miembros", "sd", "CI"),
              names_vary = "slowest") %>%
  add_column(cobhe_f = "Promedio total", .before = "miembros_2003")


cuadro <- as.data.frame(rbind(tabla, sum_tab1))


write.xlsx(cuadro, "salidas/procesamientos_eph_2022.xlsx", sheetName = "vivienda_miembros", row.names = F, append = T)

## Menores de 14 años---------------------

tabla <- eph %>%
  filter(dominancia == 1 & CH06 >= 18) %>%
  group_by(ANO4, cobhe_f) %>%
  dplyr::summarise(menores = round(weighted.mean(menores0_14, PONDERA, na.rm = T), digits = 2),
                   sd = round(sqrt(wtd.var(menores0_14, PONDERA)), digits = 2),
                   CI = 1.96 * (sd / sqrt(n()))) %>%
  pivot_wider(id_cols = cobhe_f, names_from = ANO4, values_from = c("menores", "sd", "CI"),
              names_vary = "slowest")

sum_tab1 <- eph %>%
  filter(dominancia == 1 & CH06 >= 18) %>%
  group_by(ANO4) %>%
  dplyr::summarise(menores = round(weighted.mean(menores0_14, PONDERA, na.rm = T), digits = 2),
                   sd = round(sqrt(wtd.var(menores0_14, PONDERA)), digits = 2),
                   CI = 1.96 * (sd / sqrt(n()))) %>%
  pivot_wider(names_from = ANO4, values_from = c("menores", "sd", "CI"),
              names_vary = "slowest") %>%
  add_column(cobhe_f = "Promedio total", .before = "menores_2003")


cuadro <- as.data.frame(rbind(tabla, sum_tab1))

write.xlsx(cuadro, "salidas/procesamientos_eph_2022.xlsx", sheetName = "vivienda_menores", row.names = F, append = T)

## Hacinamiento---------------
tabla <- eph %>%
  filter(dominancia == 1 & CH06 >= 18) %>%
  mutate(hacinamiento = case_when(IX_TOT/II2 > 3 ~ 1,
                                  IX_TOT/II2 <= 3 ~ 0)) %>%
  group_by(ANO4, cobhe_f) %>%
  summarise(hacinamiento_crit = round(weighted.mean(hacinamiento, PONDERA)*100, digits = 2)) %>%
  spread(ANO4, value = hacinamiento_crit)

sum_tab1 <- eph %>%
  filter(dominancia == 1 & CH06 >= 18) %>%
  mutate(hacinamiento = case_when(IX_TOT/II1 > 3 ~ 1,
                                  IX_TOT/II1 <= 3 ~ 0)) %>%
  group_by(ANO4) %>%
  summarise(hacinamiento_crit = round(weighted.mean(hacinamiento, PONDERA)*100, digits = 2)) %>%
  spread(ANO4, value = hacinamiento_crit) %>%
  add_column(cobhe_f = "Promedio total", .before = "2003")

sum_tab2 <- eph %>%
  filter(dominancia == 1 & CH06 >= 18) %>%
  mutate(hacinamiento = case_when(IX_TOT/II1 > 3 ~ 1,
                                  IX_TOT/II1 <= 3 ~ 0)) %>%
  filter(hacinamiento == 1) %>%
  group_by(ANO4) %>%
  tally(PONDERA) %>%
  spread(ANO4, value = n) %>%
  add_column(cobhe_f = "N", .before = "2003")

sum_tab3 <- eph %>%
  filter(dominancia == 1 & CH06 >= 18 & cobhe != 9) %>%
  mutate(hacinamiento = case_when(IX_TOT/II1 > 3 ~ 1,
                                  IX_TOT/II1 <= 3 ~ 0)) %>%
  group_by(ANO4) %>%
  summarise(hacinamiento_crit = round(weighted.mean(hacinamiento, PONDERA)*100, digits = 2)) %>%
  spread(ANO4, value = hacinamiento_crit) %>%
  add_column(cobhe_f = "Promedio PEA", .before = "2003")

sum_tab4 <- eph %>%
  filter(dominancia == 1 & CH06 >= 18 & cobhe != 9) %>%
  mutate(hacinamiento = case_when(IX_TOT/II1 > 3 ~ 1,
                                  IX_TOT/II1 <= 3 ~ 0)) %>%
  filter(hacinamiento == 1) %>%
  group_by(ANO4) %>%
  tally(PONDERA) %>%
  spread(ANO4, value = n) %>%
  add_column(cobhe_f = "N PEA", .before = "2003")

cuadro <- as.data.frame(rbind(tabla, sum_tab1, sum_tab2, sum_tab3, sum_tab4))

write.xlsx(cuadro, "salidas/procesamientos_eph_2022.xlsx", sheetName = "vivienda_hacinamiento", row.names = F, append = T)


## Hábitat desfavorable-----------------
tabla <- eph2020 %>%
  filter(dominancia == 1 & CH06 >= 18) %>%
  group_by(ANO4, cobhe_f) %>%
  summarise(habitat = round(weighted.mean(ambiental, PONDERA)*100, digits = 2)) %>%
  spread(ANO4, value = habitat)

sum_tab1 <- eph2020 %>%
  filter(dominancia == 1 & CH06 >= 18) %>%
  group_by(ANO4) %>%
  summarise(habitat = round(weighted.mean(ambiental, PONDERA)*100, digits = 2)) %>%
  spread(ANO4, value = habitat) %>%
  add_column(cobhe_f = "Promedio total", .before = "2003")

sum_tab2 <- eph2020 %>%
  filter(dominancia == 1 & CH06 >= 18 & ambiental == 1) %>%
  group_by(ANO4) %>%
  tally(PONDERA) %>%
  spread(ANO4, value = n) %>%
  add_column(cobhe_f = "N", .before = "2003")

sum_tab3 <- eph2020 %>%
  filter(dominancia == 1 & CH06 >= 18 & cobhe != 9) %>%
  group_by(ANO4) %>%
  summarise(habitat = round(weighted.mean(ambiental, PONDERA)*100, digits = 2)) %>%
  spread(ANO4, value = habitat) %>%
  add_column(cobhe_f = "Promedio PEA", .before = "2003")

sum_tab4 <- eph2020 %>%
  filter(dominancia == 1 & CH06 >= 18 & ambiental == 1 & cobhe != 9) %>%
  group_by(ANO4) %>%
  tally(PONDERA) %>%
  spread(ANO4, value = n) %>%
  add_column(cobhe_f = "N PEA", .before = "2003")

cuadro <- as.data.frame(rbind(tabla, sum_tab1, sum_tab2, sum_tab3, sum_tab4))

write.xlsx(cuadro, "salidas/procesamientos_eph_2022.xlsx", sheetName = "vivienda_habitat", row.names = F, append = T)


## CALMAT ----------------
tabla <- eph %>%
  filter(dominancia == 1 & !is.na(calmat) & CH06 >= 18) %>%
  group_by(ANO4, cobhe_f) %>%
  summarise(calmat = round(weighted.mean(calmat_dic, PONDERA)*100, digits = 2)) %>%
  spread(ANO4, value = calmat)

sum_tab1 <- eph %>%
  filter(dominancia == 1 & !is.na(calmat) & CH06 >= 18) %>%
  group_by(ANO4) %>%
  summarise(calmat = round(weighted.mean(calmat_dic, PONDERA)*100, digits = 2)) %>%
  spread(ANO4, value = calmat) %>%
  add_column(cobhe_f = "Promedio total", .before = "2003")

sum_tab2 <- eph %>%
  filter(dominancia == 1 & CH06 >= 18 & calmat_dic == 1) %>%
  group_by(ANO4) %>%
  tally(PONDERA) %>%
  spread(ANO4, value = n) %>%
  add_column(cobhe_f = "N", .before = "2003")

sum_tab3 <- eph %>%
  filter(dominancia == 1 & !is.na(calmat) & CH06 >= 18 & cobhe != 9) %>%
  group_by(ANO4) %>%
  summarise(calmat = round(weighted.mean(calmat_dic, PONDERA)*100, digits = 2)) %>%
  spread(ANO4, value = calmat) %>%
  add_column(cobhe_f = "Promedio PEA", .before = "2003")

sum_tab4 <- eph %>%
  filter(dominancia == 1 & CH06 >= 18 & calmat_dic == 1 & cobhe != 9) %>%
  group_by(ANO4) %>%
  tally(PONDERA) %>%
  spread(ANO4, value = n) %>%
  add_column(cobhe_f = "N PEA", .before = "2003")

cuadro <- as.data.frame(rbind(tabla, sum_tab1, sum_tab2, sum_tab3, sum_tab4))

write.xlsx(cuadro, "salidas/procesamientos_eph_2022.xlsx", sheetName = "vivienda_calmat", row.names = F, append = T)


## INCALSERV-------------
tabla <- eph %>%
  filter(dominancia == 1 & !is.na(incalserv) & CH06 >= 18) %>%
  group_by(ANO4, cobhe_f) %>%
  summarise(incalserv = round(weighted.mean(incalserv, PONDERA)*100, digits = 2)) %>%
  spread(ANO4, value = incalserv)

sum_tab1 <- eph %>%
  filter(dominancia == 1 & !is.na(incalserv) & CH06 >= 18) %>%
  group_by(ANO4) %>%
  summarise(incalserv = round(weighted.mean(incalserv, PONDERA)*100, digits = 2)) %>%
  spread(ANO4, value = incalserv) %>%
  add_column(cobhe_f = "Promedio total", .before = "2003")

sum_tab2 <- eph %>%
  filter(dominancia == 1 & CH06 >= 18 & incalserv == 1) %>%
  group_by(ANO4) %>%
  tally(PONDERA) %>%
  spread(ANO4, value = n) %>%
  add_column(cobhe_f = "N", .before = "2003")

sum_tab3 <- eph %>%
  filter(dominancia == 1 & !is.na(incalserv) & CH06 >= 18 & cobhe != 9) %>%
  group_by(ANO4) %>%
  summarise(incalserv = round(weighted.mean(incalserv, PONDERA)*100, digits = 2)) %>%
  spread(ANO4, value = incalserv) %>%
  add_column(cobhe_f = "Promedio PEA", .before = "2003")

sum_tab4 <- eph %>%
  filter(dominancia == 1 & CH06 >= 18 & incalserv == 1 & cobhe != 9) %>%
  group_by(ANO4) %>%
  tally(PONDERA) %>%
  spread(ANO4, value = n) %>%
  add_column(cobhe_f = "N PEA", .before = "2003")


cuadro <- as.data.frame(rbind(tabla, sum_tab1, sum_tab2, sum_tab3, sum_tab4))

write.xlsx(cuadro, "salidas/procesamientos_eph_2022.xlsx", sheetName = "vivienda_incalserv", row.names = F, append = T)


# 3) Educación ----------------
## Superior -------------------
tabla <- eph %>%
  filter(!is.na(cobhe_f) & CH06 >= 30) %>%
  group_by(ANO4, cobhe_f) %>%
  summarise(educ_superior = round(weighted.mean(educ_superior, PONDERA)*100, digits = 2)) %>%
  spread(ANO4, value = educ_superior)

sum_tab1 <- eph %>%
  filter(!is.na(cobhe_f) & CH06 >= 30) %>%
  group_by(ANO4) %>%
  summarise(educ_superior = round(weighted.mean(educ_superior, PONDERA)*100, digits = 2)) %>%
  spread(ANO4, value = educ_superior) %>%
  add_column(cobhe_f = "Promedio total", .before = "2003")

sum_tab2 <- eph %>%
  filter(!is.na(cobhe) & CH06 >= 30 & educ_superior == 1) %>%
  group_by(ANO4) %>%
  tally(PONDERA) %>%
  spread(ANO4, value = n) %>%
  add_column(cobhe_f = "N", .before = "2003")

sum_tab3 <- eph %>%
  filter(!is.na(cobhe_f) & CH06 >= 30 & cobhe != 9) %>%
  group_by(ANO4) %>%
  summarise(educ_superior = round(weighted.mean(educ_superior, PONDERA)*100, digits = 2)) %>%
  spread(ANO4, value = educ_superior) %>%
  add_column(cobhe_f = "Promedio PEA", .before = "2003")

sum_tab4 <- eph %>%
  filter(!is.na(cobhe) & CH06 >= 30 & educ_superior == 1 & cobhe != 9) %>%
  group_by(ANO4) %>%
  tally(PONDERA) %>%
  spread(ANO4, value = n) %>%
  add_column(cobhe_f = "N PEA", .before = "2003")

cuadro <- as.data.frame(rbind(tabla, sum_tab1, sum_tab2, sum_tab3, sum_tab4))

write.xlsx(cuadro, "salidas/procesamientos_eph_2022.xlsx", sheetName = "educacion_superior", row.names = F, append = T)


## Secundario -------------------
tabla <- eph %>%
  filter(!is.na(cobhe_f) & CH06 >= 30) %>%
  group_by(ANO4, cobhe_f) %>%
  summarise(educ_secundaria = round(weighted.mean(educ_secundaria, PONDERA)*100, digits = 2)) %>%
  spread(ANO4, value = educ_secundaria)

sum_tab1 <- eph %>%
  filter(!is.na(cobhe_f) & CH06 >= 30) %>%
  group_by(ANO4) %>%
  summarise(educ_secundaria = round(weighted.mean(educ_secundaria, PONDERA)*100, digits = 2)) %>%
  spread(ANO4, value = educ_secundaria) %>%
  add_column(cobhe_f = "Promedio total", .before = "2003")

sum_tab2 <- eph %>%
  filter(!is.na(cobhe) & CH06 >= 30 & educ_secundaria == 1) %>%
  group_by(ANO4) %>%
  tally(PONDERA) %>%
  spread(ANO4, value = n) %>%
  add_column(cobhe_f = "N", .before = "2003")

sum_tab3 <- eph %>%
  filter(!is.na(cobhe_f) & CH06 >= 30 & cobhe != 9) %>%
  group_by(ANO4) %>%
  summarise(educ_secundaria = round(weighted.mean(educ_secundaria, PONDERA)*100, digits = 2)) %>%
  spread(ANO4, value = educ_secundaria) %>%
  add_column(cobhe_f = "Promedio PEA", .before = "2003")

sum_tab4 <- eph %>%
  filter(!is.na(cobhe) & CH06 >= 30 & educ_secundaria == 1 & cobhe != 9) %>%
  group_by(ANO4) %>%
  tally(PONDERA) %>%
  spread(ANO4, value = n) %>%
  add_column(cobhe_f = "N PEA", .before = "2003")

cuadro <- as.data.frame(rbind(tabla, sum_tab1, sum_tab2, sum_tab3, sum_tab4))

write.xlsx(cuadro, "salidas/procesamientos_eph_2022.xlsx", sheetName = "educacion_secundaria", row.names = F, append = T)


# 4) Ingresos ------------------

## Evolución ingresos (ITF) ------------
tabla <- eph2020 %>%
  filter(dominancia == 1 & !is.na(cobhe) & CH06 >= 18) %>%
  group_by(ANO4, cobhe_f) %>%
  dplyr::summarise(ITF = round(weighted.mean(itf_ppa, PONDIH_emp, na.rm = T)),
            sd = round(sqrt(wtd.var(itf_ppa, PONDIH_emp)), digits = 2),
            CI = 1.96 * (sd / sqrt(n()))) %>%
  pivot_wider(names_from = ANO4, values_from = c("ITF", "sd", "CI"),
                          names_vary = "slowest")

sum_tab1 <- eph2020 %>%
  filter(dominancia == 1 & !is.na(cobhe) & CH06 >= 18) %>%
  group_by(ANO4) %>%
  dplyr::summarise(ITF = round(weighted.mean(itf_ppa, PONDIH_emp, na.rm = T)),
                   sd = round(sqrt(wtd.var(itf_ppa, PONDIH_emp)), digits = 2),
                   CI = 1.96 * (sd / sqrt(n()))) %>%
  pivot_wider(names_from = ANO4, values_from = c("ITF", "sd", "CI"),
              names_vary = "slowest") %>%
  add_column(cobhe_f = "Promedio total", .before = "ITF_2003")

sum_tab2 <- eph2020 %>%
  filter(dominancia == 1 & !is.na(cobhe) & CH06 >= 18) %>%
  group_by(ANO4) %>%
  tally(PONDERA) %>%
  spread(ANO4, value = n) %>%
  add_column(cobhe_f = "N", .before = "2003")

sum_tab3 <- eph2020 %>%
  filter(dominancia == 1 & !is.na(cobhe) & CH06 >= 18 & cobhe != 9) %>%
  group_by(ANO4) %>%
  dplyr::summarise(ITF = round(weighted.mean(itf_ppa, PONDIH_emp, na.rm = T)),
                   sd = round(sqrt(wtd.var(itf_ppa, PONDIH_emp)), digits = 2),
                   CI = 1.96 * (sd / sqrt(n()))) %>%
  pivot_wider(names_from = ANO4, values_from = c("ITF", "sd", "CI"),
              names_vary = "slowest") %>%
  add_column(cobhe_f = "Promedio total", .before = "ITF_2003")

sum_tab4 <- eph2020 %>%
  filter(dominancia == 1 & !is.na(cobhe) & CH06 >= 18 & cobhe != 9) %>%
  group_by(ANO4) %>%
  tally(PONDERA) %>%
  spread(ANO4, value = n) %>%
  add_column(cobhe_f = "N PEA", .before = "2003")

cuadro <- as.data.frame(rbind(tabla, sum_tab1, sum_tab3))

write.xlsx(cuadro, "salidas/procesamientos_eph_2022.xlsx", sheetName = "ITF_ppa", row.names = F, append = T)

### ETA2----------------
eta_anio <- data.frame()

for(i in unique(sort(eph2020$ANO4))) {
  select <- eph2020 %>%
    filter(ANO4==i & dominancia == 1 & !is.na(cobhe) & CH06 >= 18)
  modelo <- anova(lm(itf_ppa ~ cobhe_f, data = select, weights = PONDIH_emp))
  eta <- eta_squared(modelo)
  f <- as.data.frame(modelo$`F value`[1])
  p <- as.data.frame(modelo$`Pr(>F)`[1])
  eta_fp <- cbind(eta, f, p)
  eta_anio <- rbind(eta_anio, eta_fp)
}

eta_anio <- eta_anio %>%
  add_column("Año" = unique(sort(eph2020$ANO4)), .before = "Parameter")

write.xlsx(eta_anio, "salidas/procesamientos_eph_2022.xlsx", sheetName = "eta_itf", row.names = F, append = T)

## Evolución ingresos (IPCF) ------------
tabla <- eph2020 %>%
  filter(dominancia == 1 & !is.na(cobhe) & CH06 >= 18) %>%
  group_by(ANO4, cobhe_f) %>%
  dplyr::summarise(IPCF = round(weighted.mean(ipcf_ppa, PONDIH_emp, na.rm = T)),
                   sd = round(sqrt(wtd.var(ipcf_ppa, PONDIH_emp)), digits = 2),
                   CI = 1.96 * (sd / sqrt(n()))) %>%
  pivot_wider(names_from = ANO4, values_from = c("IPCF", "sd", "CI"),
              names_vary = "slowest")

sum_tab1 <- eph2020 %>%
  filter(dominancia == 1 & !is.na(cobhe) & CH06 >= 18) %>%
  group_by(ANO4) %>%
  dplyr::summarise(IPCF = round(weighted.mean(ipcf_ppa, PONDIH_emp, na.rm = T)),
                   sd = round(sqrt(wtd.var(ipcf_ppa, PONDIH_emp)), digits = 2),
                   CI = 1.96 * (sd / sqrt(n()))) %>%
  pivot_wider(names_from = ANO4, values_from = c("IPCF", "sd", "CI"),
              names_vary = "slowest") %>%
  add_column(cobhe_f = "Promedio total", .before = "IPCF_2003")

sum_tab2 <- eph2020 %>%
  filter(dominancia == 1 & !is.na(cobhe) & CH06 >= 18) %>%
  group_by(ANO4) %>%
  tally(PONDERA) %>%
  spread(ANO4, value = n) %>%
  add_column(cobhe_f = "N", .before = "2003")

sum_tab3 <- eph2020 %>%
  filter(dominancia == 1 & !is.na(cobhe) & CH06 >= 18 & cobhe != 9) %>%
  group_by(ANO4) %>%
  dplyr::summarise(IPCF = round(weighted.mean(ipcf_ppa, PONDIH_emp, na.rm = T)),
                   sd = round(sqrt(wtd.var(ipcf_ppa, PONDIH_emp)), digits = 2),
                   CI = 1.96 * (sd / sqrt(n()))) %>%
  pivot_wider(names_from = ANO4, values_from = c("IPCF", "sd", "CI"),
              names_vary = "slowest") %>%
  add_column(cobhe_f = "Promedio total", .before = "IPCF_2003")

sum_tab4 <- eph2020 %>%
  filter(dominancia == 1 & !is.na(cobhe) & CH06 >= 18 & cobhe != 9) %>%
  group_by(ANO4) %>%
  tally(PONDERA) %>%
  spread(ANO4, value = n) %>%
  add_column(cobhe_f = "N PEA", .before = "2003")

cuadro <- as.data.frame(rbind(tabla, sum_tab1, sum_tab3))

write.xlsx(cuadro, "salidas/procesamientos_eph_2022.xlsx", sheetName = "IPCF_ppa", row.names = F, append = T)

### ETA2----------------
eta_anio <- data.frame()

for(i in unique(sort(eph2020$ANO4))) {
  select <- eph2020 %>%
    filter(ANO4==i & dominancia == 1 & !is.na(cobhe) & CH06 >= 18)
  modelo <- anova(lm(ipcf_ppa ~ cobhe_f, data = select, weights = PONDIH_emp))
  eta <- eta_squared(modelo)
  f <- as.data.frame(modelo$`F value`[1])
  p <- as.data.frame(modelo$`Pr(>F)`[1])
  eta_fp <- cbind(eta, f, p)
  eta_anio <- rbind(eta_anio, eta_fp)
}

eta_anio <- eta_anio %>%
  add_column("Año" = unique(sort(eph2020$ANO4)), .before = "Parameter")

write.xlsx(eta_anio, "salidas/procesamientos_eph_2022.xlsx", sheetName = "eta_ipcf", row.names = F, append = T)


## Evolución ingresos laborales ------------
tabla <- eph2020 %>%
  filter(dominancia == 1 & !is.na(cobhe) & CH06 >= 18) %>%
  group_by(ANO4, cobhe_f) %>%
  dplyr::summarise(ing_lab = round(weighted.mean(ing_lab_ppa, PONDIH_emp, na.rm = T)),
                   sd = round(sqrt(wtd.var(ing_lab_ppa, PONDIH_emp)), digits = 2),
                   CI = 1.96 * (sd / sqrt(n()))) %>%
  pivot_wider(names_from = ANO4, values_from = c("ing_lab", "sd", "CI"),
              names_vary = "slowest")

sum_tab1 <- eph2020 %>%
  filter(dominancia == 1 & !is.na(cobhe) & CH06 >= 18) %>%
  group_by(ANO4) %>%
  dplyr::summarise(ing_lab = round(weighted.mean(ing_lab_ppa, PONDIH_emp, na.rm = T)),
                   sd = round(sqrt(wtd.var(ing_lab_ppa, PONDIH_emp)), digits = 2),
                   CI = 1.96 * (sd / sqrt(n()))) %>%
  pivot_wider(names_from = ANO4, values_from = c("ing_lab", "sd", "CI"),
              names_vary = "slowest") %>%
  add_column(cobhe_f = "Promedio total", .before = "ing_lab_2003")

sum_tab2 <- eph2020 %>%
  filter(dominancia == 1 & !is.na(cobhe) & CH06 >= 18) %>%
  group_by(ANO4) %>%
  tally(PONDERA) %>%
  spread(ANO4, value = n) %>%
  add_column(cobhe_f = "N", .before = "2003")

sum_tab3 <- eph2020 %>%
  filter(dominancia == 1 & !is.na(cobhe) & CH06 >= 18 & cobhe != 9) %>%
  group_by(ANO4) %>%
  dplyr::summarise(ing_lab = round(weighted.mean(ing_lab_ppa, PONDIH_emp, na.rm = T)),
                   sd = round(sqrt(wtd.var(ing_lab_ppa, PONDIH_emp)), digits = 2),
                   CI = 1.96 * (sd / sqrt(n()))) %>%
  pivot_wider(names_from = ANO4, values_from = c("ing_lab", "sd", "CI"),
              names_vary = "slowest") %>%
  add_column(cobhe_f = "Promedio total", .before = "ing_lab_2003")

sum_tab4 <- eph2020 %>%
  filter(dominancia == 1 & !is.na(cobhe) & CH06 >= 18 & cobhe != 9) %>%
  group_by(ANO4) %>%
  tally(PONDERA) %>%
  spread(ANO4, value = n) %>%
  add_column(cobhe_f = "N PEA", .before = "2003")

cuadro <- as.data.frame(rbind(tabla, sum_tab1, sum_tab3))

write.xlsx(cuadro, "salidas/procesamientos_eph_2022.xlsx", sheetName = "ing_lab_ppa", row.names = F, append = T)

### ETA2----------------
eta_anio <- data.frame()

for(i in unique(sort(eph2020$ANO4))) {
  select <- eph2020 %>%
    filter(ANO4==i & dominancia == 1 & !is.na(cobhe) & CH06 >= 18)
  modelo <- anova(lm(ing_lab_ppa ~ cobhe_f, data = select, weights = PONDIH_emp))
  eta <- eta_squared(modelo)
  f <- as.data.frame(modelo$`F value`[1])
  p <- as.data.frame(modelo$`Pr(>F)`[1])
  eta_fp <- cbind(eta, f, p)
  eta_anio <- rbind(eta_anio, eta_fp)
}

eta_anio <- eta_anio %>%
  add_column("Año" = unique(sort(eph2020$ANO4)), .before = "Parameter")

write.xlsx(eta_anio, "salidas/procesamientos_eph_2022.xlsx", sheetName = "eta_ing_lab", row.names = F, append = T)







## Ingresos no laborales --------------
### con inactivos --------------------
tabla <- eph %>%
  filter(dominancia == 1 & !is.na(cobhe) & CH06 >= 18) %>%
  group_by(ANO4, cobhe_f) %>%
  summarise(no_lab = round(weighted.mean(ing_nolab_tot, PONDIH_emp) /
                              weighted.mean(ITF, PONDIH_emp)*100, digits = 2)) %>%
  spread(ANO4, value = no_lab)

sum_tab1 <- eph %>%
  filter(dominancia == 1 & !is.na(cobhe) & CH06 >= 18) %>%
  group_by(ANO4) %>%
  summarise(no_lab = round(weighted.mean(ing_nolab_tot, PONDIH_emp) /
                             weighted.mean(ITF, PONDIH_emp)*100, digits = 2)) %>%
  spread(ANO4, value = no_lab) %>%
  add_column(cobhe_f = "Total", .before = "2003")

sum_tab1n <- eph %>%
  filter(dominancia == 1 & !is.na(cobhe) & CH06 >= 18) %>%
  group_by(ANO4) %>%
  tally(PONDERA) %>%
  spread(ANO4, value = n) %>%
  add_column(cobhe_f = "N", .before = "2003")

# sum_tab2 <- eph %>%
#   filter(dominancia == 1 & !is.na(cobhe) & CH06 >= 18 & cobhe != 9) %>%
#   group_by(ANO4) %>%
#   summarise(no_lab = round(weighted.mean(ing_nolab_tot, PONDIH_emp) /
#                              weighted.mean(ITF, PONDIH_emp)*100, digits = 2)) %>%
#   spread(ANO4, value = no_lab) %>%
#   add_column(cobhe_f = "Total sin inactivos", .before = "2003")
#
# sum_tab2n <- eph %>%
#   filter(dominancia == 1 & !is.na(cobhe) & CH06 >= 18 & cobhe != 9) %>%
#   group_by(ANO4) %>%
#   tally(PONDERA) %>%
#   spread(ANO4, value = n) %>%
#   add_column(cobhe_f = "N sin inactivos", .before = "2003")

cuadro <- as.data.frame(rbind(tabla, sum_tab1, sum_tab1n))

write.xlsx(cuadro, "salidas/procesamientos_eph_2022.xlsx", sheetName = "no_lab", row.names = F, append = T)


# Índice de Theil -----------------------------
## Theil general ------------------
eph <- eph %>%
  mutate(ing_lab_bis= ifelse(ing_lab_tot <= 0, 0.000001, ing_lab_tot)) %>%
  mutate(itf_bis= ifelse(ITF <= 0, 0.000001, ITF))

theil_evol <- eph %>%
  filter(dominancia == 1 & !is.na(cobhe) & CH06 >= 18) %>%
  group_by(ANO4) %>%
  summarise(theil_t = dineq::theil.wtd(itf_bis, weights = PONDIH_emp),
            theil_lab = dineq::theil.wtd(ing_lab_bis, weights = PONDIH_emp),
            diferencia = (theil_lab - theil_t)) %>%
  rename(Año = ANO4)


### Theil entre clases ---------------------
#### ITF -----------------
descomp_theil <- data.frame()

for(i in unique(sort(eph$ANO4))) {
  select <- eph %>%
    filter(ANO4==i & dominancia == 1 & !is.na(cobhe) & CH06 >= 18)
  theil <- IC2::decompGEI(select$itf_bis, select$cobhe_f,
                          w = select$PONDIH_emp, alpha = 1, ELMO = FALSE)
  theil <- unlist(theil$decomp)
  descomp_theil <- rbind(descomp_theil, theil)
}

colnames(descomp_theil) <- c("intra_tot", "inter_tot")

descomp_theil <- descomp_theil %>%
  add_column("Año" = unique(sort(eph$ANO4)), .before = "intra_tot")

descomp_theil[,-1] <- round(prop.table(as.matrix(descomp_theil[-1]), margin = 1)*100, digits=2)

#### Ingresos laborales ----------------
descomp_theil_lab <- data.frame()

for(i in unique(sort(eph$ANO4))) {
  select <- eph %>%
    filter(ANO4==i & dominancia == 1 & !is.na(cobhe) & CH06 >= 18)
  theil_lab <- IC2::decompGEI(select$ing_lab_bis, select$cobhe_f,
                              w = select$PONDIH_emp, alpha = 1, ELMO = FALSE)
  theil_lab <- unlist(theil_lab$decomp)
  descomp_theil_lab <- rbind(descomp_theil_lab, theil_lab)
}

colnames(descomp_theil_lab) <- c("intra_lab", "inter_lab")

descomp_theil_lab <- descomp_theil_lab %>%
  add_column("Año" = unique(sort(eph$ANO4)), .before = "intra_lab")

descomp_theil_lab[,-1] <- round(prop.table(as.matrix(descomp_theil_lab[-1]), margin = 1)*100, digits=2)

#pegado
theil_evol <- theil_evol %>%
  left_join(descomp_theil, by = "Año") %>%
  left_join(descomp_theil_lab, by = "Año") %>%
  as.data.frame()


write.xlsx(theil_evol, "salidas/procesamientos_eph_2022.xlsx", sheetName = "theil", row.names = F, append = T)


