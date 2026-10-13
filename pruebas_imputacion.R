tabla <- eph %>%
  filter(ESTADO == 2) %>%
  count(caes_seccion_cod)

selec1 <- eph %>%
  filter(categoria == 3) %>%
  group_by(ANO4) %>%
  count(tamano) %>%
  mutate(prop = n / sum(n))

selec2 <- base_imputacion %>%
  filter(categoria == 3) %>%
  group_by(ANO4) %>%
  count(tamano) %>%
  mutate(prop = n / sum(n))

selec1 <- selec1 %>%
  left_join(selec2, by = c("ANO4", "tamano"), suffix = c("_original", "_imputado")) %>%
  mutate(diferencia = prop_imputado - prop_original)

set.seed(971986)

base_imputacion <- hotdeck(data = eph,
                            variable = "tamano",
                            domain_var = "caes2")
