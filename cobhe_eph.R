
# Librerías y base

eph <- readRDS("fuentes/eph.rds")

pacman::p_load(tidyverse)

# COBHE para EPH ------------------
#Uno los códigos y agrego ceros
eph <- eph %>%
  mutate(rama4_ocup = ifelse(ANO4 >= 2011 & ANO4 <= 2015, PP04B_CAES, PP04B_COD),
         rama4_desocup = ifelse(ANO4 >= 2011 & ANO4 <= 2015, PP11B_CAES, PP11B_COD)) %>%
  mutate(rama4_ocup = ifelse(as.numeric(rama4_ocup) < 100, as.numeric(rama4_ocup) * 100, rama4_ocup),
         rama4_ocup = str_pad(rama4_ocup, 4, pad = "0"),
         rama4_desocup = ifelse(as.numeric(rama4_desocup) < 100, as.numeric(rama4_desocup) * 100, rama4_desocup),
         rama4_desocup = str_pad(rama4_desocup, 4, pad = "0"))

#Convierto todo a CAES Mercosur 1.0
eph <- eph %>%
  mutate(caes_ocup = ifelse(ANO4 < 2011,
                            case_when(rama4_ocup=="1501" ~ "1001",
                                      rama4_ocup=="1502" ~ "1002",
                                      rama4_ocup=="1503" ~ "1003",
                                      rama4_ocup=="1504" ~ "1009",
                                      rama4_ocup=="1505" ~ "1100",
                                      rama4_ocup=="1600" ~ "1200",
                                      rama4_ocup=="1901" ~ "1501",
                                      rama4_ocup=="1902" ~ "1502",
                                      rama4_ocup=="2000" ~ "1600",
                                      rama4_ocup=="2100" ~ "1700",
                                      rama4_ocup=="2301" ~ "1901",
                                      rama4_ocup=="2302" ~ "1902",
                                      rama4_ocup=="2401" ~ "2001",
                                      rama4_ocup=="2402" ~ "2100",
                                      rama4_ocup=="2403" ~ "2002",
                                      rama4_ocup=="2501" ~ "2201",
                                      rama4_ocup=="2502" ~ "2202",
                                      rama4_ocup=="2601" ~ "2301",
                                      rama4_ocup=="2609" ~ "2309",
                                      rama4_ocup=="3000" ~ "2602",
                                      rama4_ocup=="3400" ~ "2900",
                                      rama4_ocup=="3601" ~ "3100",
                                      rama4_ocup=="3609" ~ "3200",
                                      rama4_ocup=="4001" ~ "3501",
                                      rama4_ocup=="4002" ~ "3502",
                                      rama4_ocup=="4100" ~ "3600",
                                      rama4_ocup=="4500" ~ "4000",
                                      rama4_ocup=="5001" ~ "4501",
                                      rama4_ocup=="5002" ~ "4502",
                                      rama4_ocup=="5003" ~ "4503",
                                      rama4_ocup=="5004" ~ "4504",
                                      rama4_ocup=="5005" ~ "4806",
                                      rama4_ocup=="5301" ~ "4801",
                                      rama4_ocup=="5302" ~ "4802",
                                      rama4_ocup=="5303" ~ "4803",
                                      rama4_ocup=="5304" ~ "4804",
                                      rama4_ocup=="5305" ~ "4805",
                                      rama4_ocup=="5306" ~ "4807",
                                      rama4_ocup=="5307" ~ "4808",
                                      rama4_ocup=="5308" ~ "4809",
                                      rama4_ocup=="5309" ~ "4811",
                                      rama4_ocup=="5310" ~ "4810",
                                      rama4_ocup=="5501" ~ "5500",
                                      rama4_ocup=="5502" ~ "5602",
                                      rama4_ocup=="5503" ~ "5601",
                                      rama4_ocup=="6001" ~ "4901",
                                      rama4_ocup=="6002" ~ "4902",
                                      rama4_ocup=="6003" ~ "4904",
                                      rama4_ocup=="6004" ~ "4903",
                                      rama4_ocup=="6100" ~ "5000",
                                      rama4_ocup=="6200" ~ "5100",
                                      rama4_ocup=="6303" ~ "7900",
                                      rama4_ocup=="6401" ~ "5300",
                                      rama4_ocup=="6402" ~ "6100",
                                      rama4_ocup=="6500" ~ "6400",
                                      rama4_ocup=="7000" ~ "6800",
                                      rama4_ocup=="7103" ~ "7701",
                                      rama4_ocup=="7202" ~ "9501",
                                      rama4_ocup=="7300" ~ "7200",
                                      rama4_ocup=="7403" ~ "7301",
                                      rama4_ocup=="7404" ~ "7800",
                                      rama4_ocup=="7405" ~ "8000",
                                      rama4_ocup=="7406" ~ "8101",
                                      rama4_ocup=="7501" ~ "8401",
                                      rama4_ocup=="8501" ~ "8600",
                                      rama4_ocup=="8502" ~ "7500",
                                      rama4_ocup=="9101" ~ "9402",
                                      rama4_ocup=="9102" ~ "9401",
                                      rama4_ocup=="9109" ~ "9409",
                                      rama4_ocup=="9203" ~ "9100",
                                      rama4_ocup=="9301" ~ "9601",
                                      rama4_ocup=="9302" ~ "9602",
                                      rama4_ocup=="9303" ~ "9603",
                                      rama4_ocup=="9309" ~ "9609",
                                      rama4_ocup=="9900" ~ "9900",
                                      rama4_ocup== NA ~ "9800",
                                      rama4_ocup==" 6702" ~ "6600",
                                      rama4_ocup=="0101" ~ "0101",
                                      rama4_ocup=="0102" ~ "0102",
                                      rama4_ocup=="0103" ~ "0103",
                                      rama4_ocup=="0104" ~ "0104",
                                      rama4_ocup=="0105" ~ "0105",
                                      rama4_ocup=="0200" ~ "0200",
                                      rama4_ocup=="0500" ~ "0300",
                                      rama4_ocup=="1000" ~ "0500",
                                      rama4_ocup=="1100" ~ "0600",
                                      rama4_ocup=="1200" ~ "0700",
                                      rama4_ocup=="1300" ~ "0700",
                                      rama4_ocup=="1400" ~ "0800",
                                      rama4_ocup=="1700" ~ "1300",
                                      rama4_ocup=="1800" ~ "1400",
                                      rama4_ocup=="2200" ~ "5800",
                                      rama4_ocup=="2303" ~ "2009",
                                      rama4_ocup=="2409" ~ "2009",
                                      rama4_ocup=="2700" ~ "2400",
                                      rama4_ocup=="2800" ~ "2500",
                                      rama4_ocup=="2900" ~ "2701",
                                      rama4_ocup=="3100" ~ "2709",
                                      rama4_ocup=="3200" ~ "2601",
                                      rama4_ocup=="3300" ~ "2604",
                                      rama4_ocup=="3501" ~ "3001",
                                      rama4_ocup=="3502" ~ "3002",
                                      rama4_ocup=="3503" ~ "3003",
                                      rama4_ocup=="3509" ~ "3009",
                                      rama4_ocup=="3700" ~ "3800",
                                      rama4_ocup=="5311" ~ "9503",
                                      rama4_ocup=="6009" ~ "4905",
                                      rama4_ocup=="6301" ~ "5201",
                                      rama4_ocup=="6302" ~ "5202",
                                      rama4_ocup=="6600" ~ "6500",
                                      rama4_ocup=="6701" ~ "6600",
                                      rama4_ocup=="7101" ~ "7702",
                                      rama4_ocup=="7102" ~ "7702",
                                      rama4_ocup=="7201" ~ "6200",
                                      rama4_ocup=="7401" ~ "6900",
                                      rama4_ocup=="7402" ~ "7100",
                                      rama4_ocup=="7409" ~ "7400",
                                      rama4_ocup=="7502" ~ "8402",
                                      rama4_ocup=="8001" ~ "8501",
                                      rama4_ocup=="8009" ~ "8509",
                                      rama4_ocup=="8503" ~ "8700",
                                      rama4_ocup=="9000" ~ "3700",
                                      rama4_ocup=="9201" ~ "5900",
                                      rama4_ocup=="9202" ~ "6300",
                                      rama4_ocup=="9204" ~ "9302",
                                      rama4_ocup=="9500" ~ "9700"),
                            NA),
         caes_desocup = ifelse(ANO4 < 2011,
                               case_when(rama4_desocup=="1501" ~ "1001",
                                         rama4_desocup=="1502" ~ "1002",
                                         rama4_desocup=="1503" ~ "1003",
                                         rama4_desocup=="1504" ~ "1009",
                                         rama4_desocup=="1505" ~ "1100",
                                         rama4_desocup=="1600" ~ "1200",
                                         rama4_desocup=="1901" ~ "1501",
                                         rama4_desocup=="1902" ~ "1502",
                                         rama4_desocup=="2000" ~ "1600",
                                         rama4_desocup=="2100" ~ "1700",
                                         rama4_desocup=="2301" ~ "1901",
                                         rama4_desocup=="2302" ~ "1902",
                                         rama4_desocup=="2401" ~ "2001",
                                         rama4_desocup=="2402" ~ "2100",
                                         rama4_desocup=="2403" ~ "2002",
                                         rama4_desocup=="2501" ~ "2201",
                                         rama4_desocup=="2502" ~ "2202",
                                         rama4_desocup=="2601" ~ "2301",
                                         rama4_desocup=="2609" ~ "2309",
                                         rama4_desocup=="3000" ~ "2602",
                                         rama4_desocup=="3400" ~ "2900",
                                         rama4_desocup=="3601" ~ "3100",
                                         rama4_desocup=="3609" ~ "3200",
                                         rama4_desocup=="4001" ~ "3501",
                                         rama4_desocup=="4002" ~ "3502",
                                         rama4_desocup=="4100" ~ "3600",
                                         rama4_desocup=="4500" ~ "4000",
                                         rama4_desocup=="5001" ~ "4501",
                                         rama4_desocup=="5002" ~ "4502",
                                         rama4_desocup=="5003" ~ "4503",
                                         rama4_desocup=="5004" ~ "4504",
                                         rama4_desocup=="5005" ~ "4806",
                                         rama4_desocup=="5301" ~ "4801",
                                         rama4_desocup=="5302" ~ "4802",
                                         rama4_desocup=="5303" ~ "4803",
                                         rama4_desocup=="5304" ~ "4804",
                                         rama4_desocup=="5305" ~ "4805",
                                         rama4_desocup=="5306" ~ "4807",
                                         rama4_desocup=="5307" ~ "4808",
                                         rama4_desocup=="5308" ~ "4809",
                                         rama4_desocup=="5309" ~ "4811",
                                         rama4_desocup=="5310" ~ "4810",
                                         rama4_desocup=="5501" ~ "5500",
                                         rama4_desocup=="5502" ~ "5602",
                                         rama4_desocup=="5503" ~ "5601",
                                         rama4_desocup=="6001" ~ "4901",
                                         rama4_desocup=="6002" ~ "4902",
                                         rama4_desocup=="6003" ~ "4904",
                                         rama4_desocup=="6004" ~ "4903",
                                         rama4_desocup=="6100" ~ "5000",
                                         rama4_desocup=="6200" ~ "5100",
                                         rama4_desocup=="6303" ~ "7900",
                                         rama4_desocup=="6401" ~ "5300",
                                         rama4_desocup=="6402" ~ "6100",
                                         rama4_desocup=="6500" ~ "6400",
                                         rama4_desocup=="7000" ~ "6800",
                                         rama4_desocup=="7103" ~ "7701",
                                         rama4_desocup=="7202" ~ "9501",
                                         rama4_desocup=="7300" ~ "7200",
                                         rama4_desocup=="7403" ~ "7301",
                                         rama4_desocup=="7404" ~ "7800",
                                         rama4_desocup=="7405" ~ "8000",
                                         rama4_desocup=="7406" ~ "8101",
                                         rama4_desocup=="7501" ~ "8401",
                                         rama4_desocup=="8501" ~ "8600",
                                         rama4_desocup=="8502" ~ "7500",
                                         rama4_desocup=="9101" ~ "9402",
                                         rama4_desocup=="9102" ~ "9401",
                                         rama4_desocup=="9109" ~ "9409",
                                         rama4_desocup=="9203" ~ "9100",
                                         rama4_desocup=="9301" ~ "9601",
                                         rama4_desocup=="9302" ~ "9602",
                                         rama4_desocup=="9303" ~ "9603",
                                         rama4_desocup=="9309" ~ "9609",
                                         rama4_desocup=="9900" ~ "9900",
                                         rama4_desocup== NA ~ "9800",
                                         rama4_desocup==" 6702" ~ "6600",
                                         rama4_desocup=="0101" ~ "0101",
                                         rama4_desocup=="0102" ~ "0102",
                                         rama4_desocup=="0103" ~ "0103",
                                         rama4_desocup=="0104" ~ "0104",
                                         rama4_desocup=="0105" ~ "0105",
                                         rama4_desocup=="0200" ~ "0200",
                                         rama4_desocup=="0500" ~ "0300",
                                         rama4_desocup=="1000" ~ "0500",
                                         rama4_desocup=="1100" ~ "0600",
                                         rama4_desocup=="1200" ~ "0700",
                                         rama4_desocup=="1300" ~ "0700",
                                         rama4_desocup=="1400" ~ "0800",
                                         rama4_desocup=="1700" ~ "1300",
                                         rama4_desocup=="1800" ~ "1400",
                                         rama4_desocup=="2200" ~ "5800",
                                         rama4_desocup=="2303" ~ "2009",
                                         rama4_desocup=="2409" ~ "2009",
                                         rama4_desocup=="2700" ~ "2400",
                                         rama4_desocup=="2800" ~ "2500",
                                         rama4_desocup=="2900" ~ "2701",
                                         rama4_desocup=="3100" ~ "2709",
                                         rama4_desocup=="3200" ~ "2601",
                                         rama4_desocup=="3300" ~ "2604",
                                         rama4_desocup=="3501" ~ "3001",
                                         rama4_desocup=="3502" ~ "3002",
                                         rama4_desocup=="3503" ~ "3003",
                                         rama4_desocup=="3509" ~ "3009",
                                         rama4_desocup=="3700" ~ "3800",
                                         rama4_desocup=="5311" ~ "9503",
                                         rama4_desocup=="6009" ~ "4905",
                                         rama4_desocup=="6301" ~ "5201",
                                         rama4_desocup=="6302" ~ "5202",
                                         rama4_desocup=="6600" ~ "6500",
                                         rama4_desocup=="6701" ~ "6600",
                                         rama4_desocup=="7101" ~ "7702",
                                         rama4_desocup=="7102" ~ "7702",
                                         rama4_desocup=="7201" ~ "6200",
                                         rama4_desocup=="7401" ~ "6900",
                                         rama4_desocup=="7402" ~ "7100",
                                         rama4_desocup=="7409" ~ "7400",
                                         rama4_desocup=="7502" ~ "8402",
                                         rama4_desocup=="8001" ~ "8501",
                                         rama4_desocup=="8009" ~ "8509",
                                         rama4_desocup=="8503" ~ "8700",
                                         rama4_desocup=="9000" ~ "3700",
                                         rama4_desocup=="9201" ~ "5900",
                                         rama4_desocup=="9202" ~ "6300",
                                         rama4_desocup=="9204" ~ "9302",
                                         rama4_desocup=="9500" ~ "9700"),
                               NA),
         caes2_ocup = as.numeric(str_sub(caes_ocup, 1, 2)),
         caes2_desocup = as.numeric(str_sub(caes_desocup, 1, 2)))


##Separo dígitos del CNO-----
eph$cno <- ifelse(is.na(eph$PP04D_COD), eph$PP11D_COD, eph$PP04D_COD)

eph$cno12 <- ifelse(nchar(eph$cno) > 4, str_sub(eph$cno, 1, 2), str_sub(eph$cno, 1, 1))
eph$cno3 <- ifelse(nchar(eph$cno) > 4, str_sub(eph$cno, 3, 3), str_sub(eph$cno, 2, 2))
eph$cno4 <- ifelse(nchar(eph$cno) > 4, str_sub(eph$cno, 4, 4), str_sub(eph$cno, 3, 3))
eph$cno5 <- ifelse(nchar(eph$cno) > 4, str_sub(eph$cno, 5, 5), str_sub(eph$cno, 4, 4))

eph$cno12 <- as.numeric(eph$cno12)
eph$cno3 <- as.numeric(eph$cno3)
eph$cno4 <- as.numeric(eph$cno4)
eph$cno5 <- as.numeric(eph$cno5)

##Categoría ocupacional
eph <- eph %>%
  mutate(categoria = case_when(CAT_OCUP == 1 & cno3 == 0 ~ 1,
                               CAT_OCUP == 2 | cno3 == 1 ~ 2,
                               (CAT_OCUP == 3 | CAT_OCUP == 4) | (cno3 == 2 | cno3 == 3) ~ 3,
                               (CAT_OCUP == 3 | CAT_OCUP == 4) & (cno3 == 0) ~ 3,
                               CAT_OCUP == 1 & cno3 > 1 ~ 1,
                               CAT_OCUP == 9 & cno3 == 0 ~ 1,
                               CAT_OCUP == 9 & cno3 > 1 ~ 3,
                               cno3 == 9 & CAT_OCUP > 2 ~ 3))

## Tamaño del establecimiento------
eph <- eph %>%
  mutate(tamano = case_when((PP04C > 0 & PP04C <= 5) | (PP04C == 99 & PP04C99 == 1) ~ 1,
                            (PP04C > 5 & PP04C < 99) | (PP04C == 99 & (PP04C99 == 2 | PP04C99 == 3)) ~ 2,
                            ((PP04C == 0 & PP04C99 == 0) | (PP04C == 99 & PP04C99 == 9)) & PP04A == 1 ~ 2,
                            PP04A == 1 ~ 2,
                            TRUE ~ NA_real_))

eph <- eph %>%
  mutate(tamano_desocup = case_when((PP11C > 0 & PP11C <= 5) | (PP11C == 99 & PP11C99 == 1) ~ 1,
                                    (PP11C > 5 & PP11C < 99) | (PP11C == 99 & (PP11C99 == 2 | PP11C99 == 3)) ~ 2,
                                    ((PP11C == 0 & PP11C99 == 0) | (PP11C == 99 & PP11C99 == 9)) & PP11A == 1 ~ 2,
                                    PP11A == 1 ~ 2,
                                    TRUE ~ NA_real_))

eph$tamano <- ifelse(is.na(eph$tamano), eph$tamano_desocup, eph$tamano)


## Clasificador --------------

eph <- eph %>%
  mutate(cobhe = case_when(#Clase I: propietarios > 5 y directivos, gerentes, funcionarios de dirección
                          cno12 >= 0 & cno12 <= 4 ~ 1,
                          cno12 == 6 | cno12 == 7 ~ 1,

                          #Clase II: propietarios < 5 y directivos, gerentes, funcionarios de dirección
                          cno12 == 5 ~ 2,

                          #Clase III: cuenta propias profesionales/calificados
                          (cno12 == 32 | cno12 == 35 | cno12 == 51 | cno12 == 52 | cno12 == 53 |
                             cno12 == 54 | cno12 == 57 | cno12 == 58 | cno12 == 60 |
                             cno12 == 61 | cno12 == 62 | cno12 == 63 | cno12 == 64 |
                             cno12 == 65 | cno12 == 70 | cno12 == 71 | cno12 == 72 |
                             cno12 == 80 | cno12 == 82) & categoria == 2 & cno5 < 3 ~ 3,

                          (cno12 == 10 | cno12 == 11 | cno12 == 20 | cno12 == 30 | cno12 == 31 | cno12 == 40 |
                             cno12 == 41 | cno12 == 42 | cno12 == 43 | cno12 == 44 | cno12 == 45 |
                             cno12 == 46 | cno12 == 47 | cno12 == 50 | cno12 == 81 | cno12 == 90 |
                             cno12 == 91 | cno12 == 92) & categoria == 2 & cno5 <= 3 ~ 3,

                          cno12 == 34 & categoria == 2 & cno5 <= 2 ~ 3,
                          cno12 == 34 & categoria == 2 & cno5 > 2 & cno4 > 1 ~ 3,
                          cno12 == 35 & categoria == 2 & cno5 > 2 & cno4 > 1 ~ 3,

                          #Clase IV: trabajadores no manuales > 5
                          (cno12 >= 10 & cno12 <= 20) & categoria == 3 & tamano == 2 ~ 4,
                          cno12 == 30 & categoria == 3 & tamano == 2 ~ 4,
                          (cno12 == 31 | cno12 == 32) & categoria == 3 & tamano == 2 & cno5 <= 3 ~ 4,
                          cno12 == 35 & categoria == 3 & tamano == 2 & cno5 <= 3 ~ 4,
                          cno12 == 36 & categoria == 3 & tamano == 2 & cno5 <= 2 ~ 4,
                          (cno12 >= 40 & cno12 <= 43) & categoria == 3 & tamano == 2 ~ 4,
                          cno12 == 44 & categoria == 3 & tamano == 2 & cno5 <= 2 ~ 4,
                          (cno12 == 45 | cno12 == 46) & categoria == 3 & tamano == 2 ~ 4,
                          (cno12 >= 47 & cno12 <= 49) & categoria == 3 & tamano == 2 & cno5 <= 2 ~ 4,
                          (cno12 == 50 | cno12 == 52) & categoria == 3 & tamano == 2 ~ 4,
                          cno12 == 54 & categoria == 3 & tamano == 2 & cno5 <= 3 ~ 4,
                          cno12 == 58 & categoria == 3 & tamano == 2 & cno5 <= 2 ~ 4,
                          (cno12 >= 60 & cno12 <= 63) & categoria == 3 & tamano == 2 & cno5 <= 2 & cno4 != 2 ~ 4,
                          (cno12 >= 70 & cno12 <= 72) & categoria == 3 & tamano == 2 & cno5 <= 2 & cno4 != 2 ~ 4,
                          cno12 == 80 & categoria == 3 & tamano == 2 & cno5 <= 2 & cno4 != 2 ~ 4,
                          cno12 == 81 & categoria == 3 & tamano == 2 ~ 4,
                          cno12 == 91 & categoria == 3 & tamano == 2 ~ 4,
                          cno12 == 92 & categoria == 3 & tamano == 2 & cno5 == 1 ~ 4,

                          #Clase V: trabajadores manuales > 5
                          (cno12 == 31 | cno12 == 32) & categoria == 3 & tamano == 2 & cno5 == 4 ~ 5,
                          cno12 == 34 & categoria == 3 & tamano == 2 ~ 5,
                          cno12 == 35 & categoria == 3 & tamano == 2 & cno5 == 4 ~ 5,
                          cno12 == 36 & categoria == 3 & tamano == 2 & cno5 > 2 ~ 5,
                          cno12 == 44 & categoria == 3 & tamano == 2 & cno5 > 2 ~ 5,
                          (cno12 >= 47 & cno12 <= 49) & categoria == 3 & tamano == 2 & cno5 > 2 ~ 5,
                          cno12 == 51 & categoria == 3 & tamano == 2 ~ 5,
                          cno12 == 53 & categoria == 3 & tamano == 2 ~ 5,
                          cno12 == 54 & categoria == 3 & tamano == 2 & cno5 == 4 ~ 5,
                          (cno12 == 56 | cno12 == 57)  & categoria == 3 & tamano == 2 ~ 5,
                          cno12 == 58 & categoria == 3 & tamano == 2 & cno5 > 2 ~ 5,
                          (cno12 >= 60 & cno12 <= 63) & categoria == 3 & tamano == 2 & cno5 <= 2 & cno4 == 2 ~ 5,
                          (cno12 >= 60 & cno12 <= 63) & categoria == 3 & tamano == 2 & cno5 > 2 ~ 5,
                          (cno12 == 64 | cno12 == 65)  & categoria == 3 & tamano == 2 ~ 5,
                          (cno12 >= 70 & cno12 <= 72) & categoria == 3 & tamano == 2 & cno5 <= 2 & cno4 == 2 ~ 5,
                          (cno12 >= 70 & cno12 <= 72) & categoria == 3 & tamano == 2 & cno5 > 2 ~ 5,
                          cno12 == 80 & categoria == 3 & tamano == 2 & cno5 <= 2 & cno4 == 2 ~ 5,
                          cno12 == 80 & categoria == 3 & tamano == 2 & cno5 > 2 ~ 5,
                          (cno12 == 82 | cno12 == 90) & categoria == 3 & tamano == 2  ~ 5,
                          cno12 == 92 & categoria == 3 & tamano == 2 & cno5 > 1 ~ 5,

                          #Clase VI: trabajadores no manuales < 5
                          (cno12 >= 10 & cno12 <= 20) & categoria == 3 & tamano == 1 ~ 6,
                          cno12 == 30 & categoria == 3 & tamano == 1 ~ 6,
                          (cno12 == 31 | cno12 == 32) & categoria == 3 & tamano == 1 & cno5 <= 3 ~ 6,
                          cno12 == 35 & categoria == 3 & tamano == 1 & cno5 <= 3 ~ 6,
                          cno12 == 36 & categoria == 3 & tamano == 1 & cno5 <= 2 ~ 6,
                          (cno12 >= 40 & cno12 <= 43) & categoria == 3 & tamano == 1 ~ 6,
                          cno12 == 44 & categoria == 3 & tamano == 1 & cno5 <= 2 ~ 6,
                          (cno12 == 45 | cno12 == 46) & categoria == 3 & tamano == 1 ~ 6,
                          (cno12 >= 47 & cno12 <= 49) & categoria == 3 & tamano == 1 & cno5 <= 2 ~ 6,
                          (cno12 == 50 | cno12 == 52) & categoria == 3 & tamano == 1 ~ 6,
                          cno12 == 54 & categoria == 3 & tamano == 1 & cno5 <= 3 ~ 6,
                          cno12 == 58 & categoria == 3 & tamano == 1 & cno5 <= 2 ~ 6,
                          (cno12 >= 60 & cno12 <= 63) & categoria == 3 & tamano == 1 & cno5 <= 2 & cno4 != 2 ~ 6,
                          (cno12 >= 70 & cno12 <= 72) & categoria == 3 & tamano == 1 & cno5 <= 2 & cno4 != 2 ~ 6,
                          cno12 == 80 & categoria == 3 & tamano == 1 & cno5 <= 2 & cno4 != 2 ~ 6,
                          cno12 == 81 & categoria == 3 & tamano == 1 ~ 6,
                          cno12 == 91 & categoria == 3 & tamano == 1 ~ 6,
                          cno12 == 92 & categoria == 3 & tamano == 1 & cno5 == 1 ~ 6,

                          #Clase VII: trabajadores manuales < 5
                          (cno12 == 31 | cno12 == 32) & categoria == 3 & tamano == 1 & cno5 == 4 ~ 7,
                          cno12 == 34 & categoria == 3 & tamano == 1 ~ 7,
                          cno12 == 35 & categoria == 3 & tamano == 1 & cno5 == 4 ~ 7,
                          cno12 == 36 & categoria == 3 & tamano == 1 & cno5 > 2 ~ 7,
                          cno12 == 44 & categoria == 3 & tamano == 1 & cno5 > 2 ~ 7,
                          (cno12 >= 47 & cno12 <= 49) & categoria == 3 & tamano == 1 & cno5 > 2 ~ 7,
                          cno12 == 51 & categoria == 3 & tamano == 1 ~ 7,
                          cno12 == 53 & categoria == 3 & tamano == 1 ~ 7,
                          cno12 == 54 & categoria == 3 & tamano == 1 & cno5 == 4 ~ 7,
                          (cno12 == 56 | cno12 == 57)  & categoria == 3 & tamano == 1 ~ 7,
                          cno12 == 58 & categoria == 3 & tamano == 1 & cno5 > 2 ~ 7,
                          (cno12 >= 60 & cno12 <= 63) & categoria == 3 & tamano == 1 & cno5 <= 2 & cno4 == 2 ~ 7,
                          (cno12 >= 60 & cno12 <= 63) & categoria == 3 & tamano == 1 & cno5 > 2 ~ 7,
                          (cno12 == 64 | cno12 == 65)  & categoria == 3 & tamano == 1 ~ 7,
                          (cno12 >= 70 & cno12 <= 72) & categoria == 3 & tamano == 1 & cno5 <= 2 & cno4 == 2 ~ 7,
                          (cno12 >= 70 & cno12 <= 72) & categoria == 3 & tamano == 1 & cno5 > 2 ~ 7,
                          cno12 == 80 & categoria == 3 & tamano == 1 & cno5 <= 2 & cno4 == 2 ~ 7,
                          cno12 == 80 & categoria == 3 & tamano == 1 & cno5 > 2 ~ 7,
                          (cno12 == 82 | cno12 == 90) & categoria == 3 & tamano == 1  ~ 7,
                          cno12 == 92 & categoria == 3 & tamano == 1 & cno5 > 1 ~ 7,
                          cno12 == 55 ~ 7,
                          is.na(tamano) & PP04B1 == 1 ~ 7,

                          #Clase VIII: Cuenta propia semi-calificados y no calificados
                          (cno12 == 10 | cno12 == 32 | cno12 == 51 | cno12 == 52 | cno12 == 53 |
                             cno12 == 54 | cno12 == 57 | cno12 == 58 | cno12 == 60 |
                             cno12 == 61 | cno12 == 62 | cno12 == 63 | cno12 == 64 |
                             cno12 == 65 | cno12 == 70 | cno12 == 71 | cno12 == 72 |
                             cno12 == 80 | cno12 == 82) & categoria == 2 & (cno5 == 3 | cno5 == 4) ~ 8,

                          (cno12 == 11 | cno12 == 20 | cno12 == 30 | cno12 == 31 | cno12 == 40 |
                             cno12 == 41 | cno12 == 42 | cno12 == 43 | cno12 == 44 | cno12 == 45 |
                             cno12 == 46 | cno12 == 47 | cno12 == 50 | cno12 == 81 | cno12 == 90 |
                             cno12 == 91 | cno12 == 92) & categoria == 2 & (cno5 == 4) ~ 8,

                          cno12 == 34 & categoria == 2 & cno5 > 2 & cno4 == 1 ~ 8,
                          cno12 == 35 & categoria == 2 & cno5 > 2 & cno4 == 1 ~ 8,
                          cno12 == 36 & categoria == 2 ~ 8,
                          cno12 == 56 & categoria == 2 ~ 8,
                          cno12 == 33 ~ 8,
                          categoria == 2 & cno5 == 4 ~ 8,

                          #Clase IX: Inactivos
                          ESTADO == 3 | ESTADO == 4 ~ 9
                          ))

eph$cobhe_f <- factor(eph$cobhe,
       labels = c('Propietarios y directivos >5',
                  'Propietarios y directivos <5',
                  'Cuenta propia profesionales / calificados',
                  'Trabajadores no manuales >5',
                  'Trabajadores manuales >5',
                  'Trabajadores no manuales <5',
                  'Trabajadores manuales <5',
                  'Cuenta propia no calificados',
                  'Inactivos'))

## Pruebas-----------
# perdidos <- eph %>%
#   filter(is.na(cobhe) & !is.na(cno) & cno5 < 4 & !is.na(tamano)) %>%
#   select(cno12, cno3, cno4, cno5, categoria, tamano, PP04D_COD, PP11D_COD, PP04C, PP04C99) %>%
#   arrange(categoria, cno12, cno5) %>%
#   view()

## Clase según dominancia de sexo ---------
eph_hog <- eph %>%
 filter(CH03 <= 2) %>%
 group_by(CODUSU, NRO_HOGAR, ANO4) %>%
 mutate(clase_dom = min(cobhe, na.rm = TRUE))

# seleccion <- eph_hog %>%
#  select(CODUSU, NRO_HOGAR, CH03, ANO4, cobhe, clase_dom, cno, categoria, tamano) %>%
#  arrange(CODUSU, NRO_HOGAR, cobhe, clase_dom)

eph_hog <- eph_hog %>%
 mutate(dom = case_when(cobhe == clase_dom ~ 1,
                        cobhe > clase_dom ~ 0)) %>%
 group_by(CODUSU, NRO_HOGAR, ANO4) %>%
 mutate(dom_sum = sum(dom, na.rm = TRUE))

# seleccion <- eph_hog %>%
#   select(CODUSU, NRO_HOGAR, CH03, ANO4, cobhe, clase_dom, dom, dom_sum) %>%
#   arrange(CODUSU, NRO_HOGAR, cobhe, clase_dom) %>%
#   view()

eph_hog <- eph_hog %>%
  mutate(dominancia = case_when(dom == 1 & dom_sum == 1 ~ 1,
                                dom == 0 & dom_sum == 1 ~ 0,
                                dom == 1 & dom_sum == 2 & CH03 == 1 ~ 1,
                                dom == 1 & dom_sum == 2 & CH03 >= 2 ~ 0))

eph_hog <- eph_hog %>%
  select(ANO4, CODUSU, NRO_HOGAR, COMPONENTE, CH03, dominancia)



eph <- eph %>%
  left_join(eph_hog, by = c("ANO4", "CODUSU", "NRO_HOGAR", "COMPONENTE", "CH03"),
            relationship = "many-to-one")

rm(eph_hog, seleccion, perdidos)

## Guardo la base--------
saveRDS(eph, "fuentes/eph_clase.rds")
