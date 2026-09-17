# Librerías -------
pacman::p_load(tidyverse, eph)

# Carga de EPH ------
eph_individual <- get_microdata(year = 2003:2026,
                                type = "individual",
                                trimester = 2)

eph_hogar <- get_microdata(year = 2003:2026,
                           type = "hogar",
                           trimester = 2)


eph_hogar <- eph_hogar %>%
  select(!c("PONDERA", "PONDIH", "REGION", "MAS_500", "AGLOMERADO", "ITF",
            "DECIFR",  "IDECIFR", "RDECIFR", "GDECIFR", "PDECIFR", "ADECIFR",
            "IPCF", "DECCFR", "IDECCFR", "RDECCFR", "GDECCFR", "PDECCFR",
            "ADECCFR"))


eph <- eph_individual %>%
  left_join(eph_hogar, by = c("ANO4", "TRIMESTRE", "CODUSU", "NRO_HOGAR"))


remove(eph_hogar, eph_individual)


# Arreglos varios ------
## Correción de ponderador (sólo cuando trabajo con bases pegadas anteriores a 2016)----------

eph$PONDIH_emp <- ifelse(is.na(eph$PONDIH), eph$PONDERA, eph$PONDIH)
eph$PONDII_emp <- ifelse(is.na(eph$PONDII), eph$PONDERA, eph$PONDII)
eph$PONDIIO_emp <- ifelse(is.na(eph$PONDIIO), eph$PONDERA, eph$PONDIIO)

## Corrección variables ocupación -----
eph$PP04D_COD[eph$PP04D_COD==""] <- NA
eph$PP11D_COD[eph$PP11D_COD==""] <- NA

eph$PP04D_COD <- str_pad(eph$PP04D_COD, 5, pad = "0")
eph$PP11D_COD <- str_pad(eph$PP11D_COD, 5, pad = "0")


#Selección de variable-----------
eph <- eph %>%
  select(CODUSU, NRO_HOGAR, COMPONENTE, ANO4,
         TRIMESTRE, REGION, PONDERA, PONDIH,PONDII, PONDIIO, CH03, CH04,
         CH06, CH08, CH12, CH13, ESTADO, CAT_OCUP, CAT_INAC,
         PP02H, PP02I, PP3E_TOT, PP3F_TOT, INTENSI,
         PP04A, PP04B_COD, PP04B1, PP04C, PP04C99, PP04D_COD,
         PP07H, PP07I, PP10A, PP10E, PP11A, PP11B_COD,
         PP11C, PP11C99, PP11D_COD, P21, TOT_P12, P47T, T_VI, ITF, IPCF, PONDIH_emp, PONDII_emp,
         PONDIIO_emp, IX_TOT, IX_MEN10, II1, II2, II7,
         IV3, IV4, IV5, IV7, IV11, IV12_1, IV12_2, IV12_3)

saveRDS(eph, file = "fuentes/eph.rds")
