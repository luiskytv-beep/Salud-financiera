library(FactoMineR)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(car)  
library(multcomp)
library(stats)
library(survey)
library(purrr)
library(WeightIt)
library(doParallel)
library(osqp)
library(ranger)
library(cobalt)
library(beepr)
library(SuperLearner)
library(foreach)
library(broom)
library(writexl)
library(ebal)
library(readr)
library(lmtest)
library(modelsummary)
library(PCAmixdata)
#====
#Carga de datos y definicion de variables a usar
# 📦 Cargar datos (asumiendo que tu dataset se llama 'df' y ya está cargado)
df <- read.csv("C:/Users/LLUUI/Escritorio/Tesis/Datos/ensafi_2023_bd_csv/Merged_ENSAFI_ColumnasFinal.csv", stringsAsFactors = FALSE)
diccionario <- c(
  "P1_1" = "Cuartos_dormir",
  "P1_2" = "Total_cuartos",
  "P1_3" = "Total_baños",
  "P1_4_01" = "Refrigerador",
  "P1_4_02" = "Lavadora",
  "P1_4_03" = "Microondas",
  "P1_4_04" = "Automovil",
  "P1_4_05" = "Computadora",
  "P1_4_06" = "Telefono_fijo",
  "P1_4_07" = "Celular",
  "P1_4_08" = "Internet",
  "P1_4_09" = "TV_paga",
  "P1_4_10" = "Streaming",
  "P1_5" = "Vivienda_tipo",
  "P2_1" = "Personas_viven",
  "P5_3" = "Estado_civil",
  "P5_5" = "Identidad_indigena", #SI=0 y NO=1
  "P5_7" = "Tiene_hijos",#SI=0 y NO=1
  "P5_11" = "Recibe_apoyo", #SI=0 y NO=1
  "P6_1_1" = "Ahorro_prestamos",
  "P6_1_2" = "Ahorro_bienes",
  "P6_1_3" = "Ahorro_caja_trabajo",
  "P6_1_4" = "Ahorro_familiares",
  "P6_1_5" = "Ahorro_tanda",
  "P6_1_6" = "Ahorro_casa",
  "P6_2_01" = "Cuenta_nomina",
  "P6_2_02" = "Cuenta_pension",
  "P6_2_03" = "Cuenta_gobierno",
  "P6_2_04" = "Cuenta_ahorro",
  "P6_2_05" = "Cuenta_cheques",
  "P6_2_06" = "Cuenta_cetes",
  "P6_2_07" = "Cuenta_inversion",
  "P6_2_08" = "Cuenta_fintech",
  "P6_2_09" = "Seguro_privado",
  "P6_2_10" = "Cuenta_AFORE",
  "P6_5_1" = "Deuda_caja_trabajo",
  "P6_5_2" = "Deuda_empeno",
  "P6_5_3" = "Deuda_amigos",
  "P6_5_4" = "Deuda_familia",
  "P6_6_1" = "Tarjeta_dept",
  "P6_6_2" = "Tarjeta_banco",
  "P6_6_3" = "Credito_nomina",
  "P6_6_8" = "Credito_online")
df <- df %>%
  rename_with(~ diccionario[.x], .cols = names(diccionario))
#====
# definicion de grupo de variables
# INSTRUMENTOS FORMALES

# ÍNDICE DE SALUD FINANCIERA (ISF)
vars_isf <- c(
  # 1. Ingresos/Gastos
  "P4_10_1", "P4_10_2", "P4_10_4",
  # 2. Ahorro/Resiliencia
  "P7_6_1", "P6_3", "P7_6_3", "P7_6_4",
  # 3. Endeudamiento
  "P6_7", "P6_8", "P6_10_7",
  # 4. Planificación
  "P7_1", "P7_2_1", "P7_2_2", "P7_2_3", "P7_2_4",
  # 5. Percepción/Bienestar
  "P8_4", "P8_1_2", "P8_1_3", "P8_2_1",
  "P8_3_01", "P8_3_04"
)
# CONTROLES
# ==========================================
# Vector SOCIODEMOGRÁFICO (todo en conjunto)
# + quitar categorías base para evitar colinealidad
# ==========================================

vars_sociodemo <- c(
  # Individuo (core)
  "SEXO", "EDAD", "EDUC",
  
  # Hogar / composición
  "P2_1","DEPEN_SUM", "DEP_ECO",
  
  # SES / bienes
  "NIV_BIENES",
  
  # Condición social
  "P5_5", "P5_7", "P5_11",
  
  # Región (dummies)
  "N", 
  #"CN", 
  "C", "S",
  
  # Vivienda (dummies)
  #"dueño_vivienda", 
  "renta_vivienda", "familiar_presta",
  "intestada_litigio", "otra_situacion_viv",
  
  # Estado civil (dummies)
  "soltero", "union_libre", "separado", "divorciado", "viudo"#"casado"
)
vars_sociodemo <- ifelse(
  vars_sociodemo %in% names(diccionario),
  unname(diccionario[vars_sociodemo]),
  vars_sociodemo
)
# Categorías base para evitar colinealidad (si hay intercepto)
persona_base <- c("CN", "casado", "dueño_vivienda")
# =========================
# rasgos conductuales
# =========================
# 7.6 Estrategias ante un imprevisto (AFRONTAMIENTO / RESILIENCIA FINANCIERA) (si y no)
#vars_resiliencia_imprevisto <- c("P7_6_2","P7_6_5","P7_6_6","P7_6_7","P7_6_8")
# 7.7 Estrés financiero observado / presión financiera (DISTRESS FINANCIERO) (invertir)
vars_distress_financiero <- c("P7_7_1","P7_7_2","P7_7_3","P7_7_4","P7_7_5","P7_7_6")
# 7.8 Bienestar financiero subjetivo / tranquilidad financiera (BIENESTAR SUBJETIVO)(invertir)
vars_bienestar_financiero_subj <- c("P7_8_1","P7_8_2","P7_8_3","P7_8_4","P7_8_5","P7_8_6")
# 7.9 Confianza / Autoeficacia financiera (invertir)
vars_autoeficacia <- c("P7_9_1", "P7_9_2", "P7_9_3")
# 7.10 Rasgos conductuales (Tarjeta 6)(invertir todas)
# A) Control percibido (locus de control interno)
vars_control <- c("P7_10_01")
# B) Orientación a la acción / persistencia (incluye procrastinación y hábitos)
vars_accion <- c("P7_10_02", "P7_10_07", "P7_10_08")
# C) Orientación al futuro
vars_futuro<- c("P7_10_03", "P7_10_04")
# D) Impulsividad / reactividad emocional
vars_impulsividad <- c("P7_10_05", "P7_10_06")
# E) Optimismo (vs pesimismo)
vars_optimismo <- c("P7_10_09", "P7_10_10", "P7_10_11", "P7_10_12")
# 7.11 Preferencias "Gasto vs Ahorro"(invertir)
vars_gasto_vs_ahorro <- c("P7_11_1", "P7_11_2", "P7_11_3", "P7_11_4")

# (Opcional) Un vector maestro con todo lo psicológico
vars_psico_todo <- c(
 # vars_resiliencia_imprevisto, 
  vars_distress_financiero,
  vars_bienestar_financiero_subj,
  vars_autoeficacia,
  vars_control, vars_accion, vars_futuro, 
  vars_impulsividad, vars_optimismo,
  vars_gasto_vs_ahorro
)
REGIONES <- c("N", "CN", "C", "S")
#ENTIDADES <- c(
#  "AGS", "BC", "BCS", "CAM", "COA", "COL", "CHP", "CHH", "CDMX", "DGO", 
#  "GTO", "GRO", "HGO", "JAL", "MEX", "MICH", "MOR", "NAY", "NL", 
#  "OAX", "PUE", "QRO", "QR", "SLP", "SIN", "SON", "TAB", "TAMPS", 
#  "TLAX","VER", "YUC", "ZACS")
dum_p1_5 <- c(
  "dueño_vivienda",        # 1. Vive la persona dueña
  "renta_vivienda",        # 2. Se paga renta
  "familiar_presta",       # 3. Es de familiar o la prestan
  "intestada_litigio",     # 4. Intestada o en litigio
  "otra_situacion"
)
dum_p5_3 <- c(
  "union_libre",       # 1. Unión libre
  "separado",          # 2. Separado(a)
  "divorciado",        # 3. Divorciado(a)
  "viudo",             # 4. Viudo(a)
  "casado",            # 5. Casado(a)
  "soltero"            # 6. Soltero(a)
)
bienes <- c("Cuartos_dormir", "Total_cuartos", 
            "Total_baños", "Refrigerador", "Lavadora", 
            "Microondas", "Automovil", "Computadora",
            "Telefono_fijo", "Celular", "TV_paga", "Streaming","Internet")
df <- df %>%
  mutate(
    anios_base = case_when(
      NIV == 0 ~ 0,   # Ninguno
      NIV == 1 ~ 0,   # Preescolar
      NIV == 2 ~ 3,   # Primaria
      NIV == 3 ~ 6,   # Secundaria
      NIV == 4 ~ 9,   # Normal básica
      NIV == 5 ~ 6,   # Técnicos con secundaria
      NIV == 6 ~ 9,   # Preparatoria
      NIV == 7 ~ 9,   # Técnicos con prepa
      NIV == 8 ~ 12,  # Licenciatura
      NIV == 9 ~ 16,  # Especialidad
      NIV == 10 ~ 17, # Maestría
      NIV == 11 ~ 19  # Doctorado
    ),
    EDUC = anios_base + GRA
  )
#vars_controls <- ifelse(vars_controls %in% names(diccionario),
#                      diccionario[vars_controls],
#                      vars_controls)
#====
#Construir las variables a como deberia ser
# Reemplazar NA en P6_7 por 2
df$P6_7[is.na(df$P6_7)] <- 2
# Reemplazar NA en P6_8 por 5
df$P6_8[is.na(df$P6_8)] <- 5
# Reemplazar NA en P6_10_7 por 2
df$P6_10_7[is.na(df$P6_10_7)] <- 2
# Reemplazar NA en P6_10_7 por 2
df$P6_3[is.na(df$P6_3)] <- 2

df <- df %>%
  dplyr::filter(!if_any(all_of(c("P4_10_1","P4_10_2","P4_10_4", "P6_8")), ~ .x == 9))

for (v in vars_isf) {
  cat("\n====================\n")
  cat("Variable:", v, "\n")
  tab <- table(df[[v]], useNA = "ifany")
  print(tab)
}

# ============================================================
# Re-codificar binarias ENSAFI: 1=Sí, 2=No  --->  1=Sí, 0=No
# Detecta solo columnas con valores {1,2} o {1,2,'b'} (ignorando NA)
# 'b' se deja igual; NA se deja NA; cualquier otra columna no se toca
# ============================================================

# Helper: limpia valores a texto (para detectar 'b') y a numérico (para recodificar 1/2)
binary_vars <- names(df)[sapply(df, function(x) {
  # a texto (preserva 'b' aunque venga como factor)
  x_chr <- tolower(trimws(as.character(x)))
  vals <- unique(na.omit(x_chr))
  # aceptar exactamente {1,2} o {1,2,b} (en cualquier orden)
  all(vals %in% c("1","2","b")) && any(vals %in% c("1","2"))
})]

df[binary_vars] <- lapply(df[binary_vars], function(x) {
  x_chr <- tolower(trimws(as.character(x)))
  
  # recodifica solo cuando sea "1" o "2"
  out <- x_chr
  out[x_chr == "1"] <- "1"
  out[x_chr == "2"] <- "0"
  # 'b' queda como 'b'; NA queda NA automáticamente
  
  # Si NO hay 'b' en la columna, lo regresamos a numérico (0/1)
  # Si SÍ hay 'b', lo dejamos como character para no perder 'b'
  if (!any(out %in% "b", na.rm = TRUE)) {
    suppressWarnings(as.numeric(out))
  } else {
    out
  }
})

# (Opcional) ver qué columnas fueron recodificadas
#print(df[binary_vars])

df <- df %>%
  mutate(
    P7_12_1 = ifelse(P7_12_1 == 1, 0, ifelse(P7_12_1 == 0, 1, P7_12_1)),
    P7_12_2 = ifelse(P7_12_2 == 1, 0, ifelse(P7_12_2 == 0, 1, P7_12_2)),
    P7_12_3 = ifelse(P7_12_3 == 1, 0, ifelse(P7_12_3 == 0, 1, P7_12_3))
  )

# 📌 Crear variables dummy por región
for (i in 1:4) {
  df[[paste0("REG_", i)]] <- ifelse(df$REGION == i, 1, 0)
}

# 📌 Renombrar columnas dummy con nombres de regiones
colnames(df)[tail(seq_along(df), 4)] <- REGIONES
# Filtrar solo las variables existentes en los datos
existing_vars_isf <- vars_isf[vars_isf %in% names(df)]
# Dummies para P1_5
df <- df %>%
  mutate(
    dueño_vivienda       = ifelse(Vivienda_tipo == 1, 1, 0),
    renta_vivienda       = ifelse(Vivienda_tipo == 2, 1, 0),
    familiar_presta      = ifelse(Vivienda_tipo == 3, 1, 0),
    intestada_litigio    = ifelse(Vivienda_tipo == 4, 1, 0),
    otra_situacion_viv   = ifelse(Vivienda_tipo == 5, 1, 0)
  )
# Dummies para P5_3
df <- df %>%
  mutate(
    union_libre     = ifelse(Estado_civil == 1, 1, 0),
    separado         = ifelse(Estado_civil == 2, 1, 0),
    divorciado       = ifelse(Estado_civil == 3, 1, 0),
    viudo            = ifelse(Estado_civil == 4, 1, 0),
    casado           = ifelse(Estado_civil == 5, 1, 0),
    soltero          = ifelse(Estado_civil == 6, 1, 0)
  )

# Variables a voltear
vars_flip <- c(
  "P7_7_1","P7_7_2","P7_7_3","P7_7_4",
  "P7_8_1","P7_8_2","P7_8_3","P7_8_4","P7_8_5","P7_8_6",
  "P7_9_1","P7_9_2","P7_9_3",
  "P7_10_01","P7_10_02",
  "P7_10_09","P7_10_10","P7_10_11"
)

# Función: voltea escala 1..K => (K+1) - x
flip_scale <- function(x) {
  x0 <- suppressWarnings(as.numeric(as.character(x)))
  K  <- suppressWarnings(max(x0[x0 %in% c(1,2,3,4,5)], na.rm = TRUE))  # detecta 4 o 5
  if (!is.finite(K) || !(K %in% c(4,5))) return(x)  # si no detecta 4/5, no toca
  
  out <- x0
  sel <- out %in% 1:K
  out[sel] <- (K + 1) - out[sel]
}

# Aplicar a df
df[vars_flip] <- lapply(df[vars_flip], flip_scale)

# =========================
# MCA por "rasgo" + MCA global (sin all_of)
# =========================

# 1) Lista de bloques (nombre -> vector de variables)
mca_blocks <- list(
  AUTOEFICACIA = vars_autoeficacia,
  CONTROL      = vars_control,
  ACCION       = vars_accion,
  FUTURO       = vars_futuro,
  IMPULSIVIDAD = vars_impulsividad,
  OPTIMISMO    = vars_optimismo,
  GASTO_AHORRO = vars_gasto_vs_ahorro,
  DISTRESS     = vars_distress_financiero,
  BIENESTAR    = vars_bienestar_financiero_subj,
  PSICO_TODO   = vars_psico_todo
)

# 2) Correr MCA por bloque y pegar índices (Dim 1) a df
idx_df <- imap_dfc(mca_blocks, function(vs, nm){
  X <- df %>% dplyr::select(all_of(vs)) %>% mutate(across(everything(), as.factor))
  res <- MCA(X, ncp = 5, graph = FALSE)
  setNames(data.frame(res$ind$coord[,1]), paste0("IDX_MCA_", nm))
})

df <- bind_cols(df, idx_df)

# 3) MCA de bienes -> índice
res_bienes <- MCA(df %>% dplyr::select(all_of(bienes)) %>% mutate(across(everything(), as.factor)),
                  graph = FALSE)
df$indice_bienes_mca <- res_bienes$ind$coord[,1]

df_isf <- df[,vars_isf] %>% na.omit()
df_isf[] <- lapply(df_isf, as.factor)
MCA_result <- MCA(df_isf, ncp=1, graph=FALSE)
df$ISF <- MCA_result$ind$coord[,1]

# Índices psicológicos por rasgo (los que ya generaste con MCA)
psico_rasgos <- c(
  "IDX_MCA_AUTOEFICACIA",
  "IDX_MCA_CONTROL",
  "IDX_MCA_ACCION",
  "IDX_MCA_FUTURO",
  "IDX_MCA_IMPULSIVIDAD",
  "IDX_MCA_OPTIMISMO",
  "IDX_MCA_GASTO_AHORRO",
#  "IDX_MCA_RESILIENCIA",
  "IDX_MCA_DISTRESS",
  "IDX_MCA_BIENESTAR"
)

f_socio <- as.formula(paste("ISF ~", paste(vars_sociodemo, collapse = " + "))) 
f_full <- as.formula(paste("ISF ~", paste(c(vars_sociodemo, "IDX_MCA_PSICO_TODO"), collapse = " + "))) 
f_rasgos <- as.formula(paste("ISF ~", paste(c(vars_sociodemo, psico_rasgos), collapse = " + "))) 
# 1) Declarar diseño muestral
options(survey.lonely.psu = "adjust");  # evita problemas si algún estrato tiene 1 PSU

des <- svydesign(
  ids     = ~UPM_x,
  strata  = ~EST_DIS_x,
  weights = ~FAC_ELE,
  data    = df,
  nest    = TRUE
)

# 2) Modelos (mismos fórmulas que ya traes)
m_socio_svy  <- svyglm(f_socio,  design = des)
m_full_svy   <- svyglm(f_full,   design = des)
m_rasgos_svy <- svyglm(f_rasgos, design = des)

# 3) Resúmenes
summary(m_socio_svy)
summary(m_full_svy)
summary(m_rasgos_svy)

# 4) Comparaciones tipo "anova" (Wald tests)
regTermTest(m_full_svy,  ~ IDX_MCA_PSICO_TODO)

# Para probar bloque completo de rasgos:
regTermTest(m_rasgos_svy, ~ IDX_MCA_AUTOEFICACIA + IDX_MCA_CONTROL + IDX_MCA_ACCION +
              IDX_MCA_FUTURO + IDX_MCA_IMPULSIVIDAD + IDX_MCA_OPTIMISMO +
              IDX_MCA_GASTO_AHORRO + IDX_MCA_DISTRESS + IDX_MCA_BIENESTAR)


# 2.1 Sensibilidad: eliminar FUTURO + ACCIÓN del modelo de rasgos

psico_rasgos_sens21 <- setdiff(
  psico_rasgos,
  c("IDX_MCA_FUTURO", "IDX_MCA_ACCION")
)

f_rasgos_sens21 <- as.formula(
  paste("ISF ~", paste(c(vars_sociodemo, psico_rasgos_sens21), collapse = " + ")))

m_rasgos_sens21_svy <- svyglm(f_rasgos_sens21, design = des)

summary(m_rasgos_sens21_svy)

# 2.2 Sensibilidad: eliminar CONTROL + OPTIMISMO del modelo de rasgos

psico_rasgos_sens22 <- setdiff(
  psico_rasgos,
  c("IDX_MCA_CONTROL", "IDX_MCA_OPTIMISMO")
)

f_rasgos_sens22 <- as.formula(
  paste("ISF ~", paste(c(vars_sociodemo, psico_rasgos_sens22), collapse = " + ")))

m_rasgos_sens22_svy <- svyglm(f_rasgos_sens22, design = des)

summary(m_rasgos_sens22_svy)

# 2.3 Sensibilidad: eliminar OPTIMISMO + DISTRESS + BIENESTAR del modelo de rasgos

psico_rasgos_sens23 <- setdiff(
  psico_rasgos,
  c("IDX_MCA_OPTIMISMO", "IDX_MCA_DISTRESS", "IDX_MCA_BIENESTAR")
)

f_rasgos_sens23 <- as.formula(
  paste("ISF ~", paste(c(vars_sociodemo, psico_rasgos_sens23), collapse = " + ")))

m_rasgos_sens23_svy <- svyglm(f_rasgos_sens23, design = des)

summary(m_rasgos_sens23_svy)



# Lista de modelos (ajusta nombres si los tuyos cambian)
mods <- list(
  "Sociodemográfico" = m_socio_svy,
  "+ Psicológico global" = m_full_svy,
  "+ Rasgos (desagregado)" = m_rasgos_svy,
  "Sin FUTURO+ACCION" = m_rasgos_sens21_svy,
  "Sin CONTROL+OPTIMISMO"=m_rasgos_sens22_svy,
  "Sin OPTIMISMO+DISTRESS+BIENESTAR"=m_rasgos_sens23_svy
  )

# Tabla en pantalla (markdown)
modelsummary(
  mods,
  statistic = "({std.error})",
  stars = TRUE,
  output = "markdown"
)#TABLA PRINCIPAL


# ============================================================
# HETEROGENEIDAD (Opción C): Barrido de interacciones + FDR(BH)
# Requiere: des (svydesign), f_rasgos (modelo base con TODOS los rasgos),
#           psico_rasgos (vector con todos los índices), vars_sociodemo ya en f_rasgos
# ============================================================
# 0) Ajuste para PSU solitaria (recomendado en ENSAFI)
options(survey.lonely.psu = "adjust")

# 1) Moderadores pertinentes
# - REGION: usamos dummies N, C, S (CN es la base)
mods <- list(
  SEXO   = "SEXO",
  EDAD   = "EDAD",
  EDUC   = "EDUC",
  REGION = c("N", "C", "S")
)

# 2) Helper: arma términos de interacción rasgo × moderador
make_int_terms <- function(rasgo, mod_vars) {
  paste0(rasgo, ":", mod_vars)
}

# 3) Helper: prueba una interacción (o bloque) con regTermTest
test_interaction <- function(des, base_formula, rasgo, mod_name, mod_vars) {
  int_terms <- paste0(rasgo, ":", mod_vars)
  
f_int <- update(base_formula, paste(". ~ . +", paste(int_terms, collapse = " + ")))  
m_int <- svyglm(f_int, design = des)
  
w <- regTermTest(m_int, as.formula(paste("~", paste(int_terms, collapse = " + "))))
  
data.frame(
    moderador = mod_name,
    rasgo     = rasgo,
    n_terms   = length(int_terms),
    terms     = paste(int_terms, collapse = " + "),  # <- NUEVO
    p_raw     = unname(w$p),
    stringsAsFactors = FALSE
  )
}


# 4) Barrido completo: todos los rasgos × todos los moderadores
res_list <- list()
k <- 1

for (rasgo in psico_rasgos) {
  for (mod_name in names(mods)) {
    mod_vars <- mods[[mod_name]];
    res_list[[k]] <- test_interaction(des, f_rasgos, rasgo, mod_name, mod_vars)
    k <- k + 1
  }
}

res <- do.call(rbind, res_list)

# Ajuste BH
res$q_bh <- p.adjust(res$p_raw, method = "BH")
res <- res[order(res$q_bh, res$p_raw), ]

# elige criterio: q<0.05 (o 0.10) y opcional top K
alpha <- 0.05
topK  <- 10   # pon NA si quieres "todas" las que pasen

res_keep <- subset(res, q_bh < alpha)

if (!is.na(topK) && nrow(res_keep) > topK) {
  res_keep <- res_keep[1:topK, ]
}
terms_final <- unique(unlist(strsplit(res_keep$terms, " \+ ")))

print(res_keep[, c("rasgo","moderador","q_bh","p_raw","terms")])
cat("\n# términos finales:\n")
print(terms_final)

f_het_final <- update(f_rasgos, paste(". ~ . +", paste(terms_final, collapse = " + ")))  
m_het_final_svy <- svyglm(f_het_final, design = des)
summary(m_het_final_svy)#TABLA CON LAS INTERACCIONES

# test del bloque completo (todas las interacciones seleccionadas)
regTermTest(m_het_final_svy, as.formula(paste("~", paste(terms_final, collapse=" + "))))


# =========================
# 3) Helpers (una sola vez)
# =========================
find_coef_name <- function(model, a, b){
  cn <- names(coef(model))
  cand1 <- paste0(a, ":", b)
  cand2 <- paste0(b, ":", a)
  if (cand1 %in% cn) return(cand1)
  if (cand2 %in% cn) return(cand2)
  stop("No encontré el coeficiente: ", cand1, " ni ", cand2)
}

marginal_effect_cont <- function(model, rasgo, mod, mod_value){
  b <- coef(model); V <- vcov(model)
  intn <- find_coef_name(model, rasgo, mod)
  est <- b[rasgo] + b[intn] * mod_value
  var <- V[rasgo, rasgo] + (mod_value^2)*V[intn, intn] + 2*mod_value*V[rasgo, intn]
  se  <- sqrt(var)
  c(est = unname(est), se = unname(se))
}

effect_by_region <- function(model, rasgo, dummies = c("N","C","S"), base = "CN"){  b <- coef(model); V <- vcov(model)  out <- data.frame(Region = base, Beta = unname(b[rasgo]), SE = sqrt(V[rasgo, rasgo]))  for (d in dummies){    intn <- find_coef_name(model, rasgo, d)    est  <- b[rasgo] + b[intn]    var  <- V[rasgo, rasgo] + V[intn, intn] + 2*V[rasgo, intn]    out <- rbind(out, data.frame(Region = d, Beta = unname(est), SE = sqrt(var)))  }  out}

# =========================
# 4) Marginales AUTOMÁTICOS para continuos (EDUC/EDAD si están en terms_final)
mods_cont <- c("EDUC","EDAD")
terms_cont <- terms_final[grepl(":(EDUC|EDAD)$", terms_final)]
marg_cont <- NULL
if (length(terms_cont) > 0) {
  # cuantiles ponderados necesarios
  if ("EDUC" %in% sub("^.*:", "", terms_cont)) {
    q_ed <- coef(svyquantile(~EDUC, design = des, quantiles = c(0.25, 0.75), na.rm = TRUE))
  }
  if ("EDAD" %in% sub("^.*:", "", terms_cont)) {
    q_age <- coef(svyquantile(~EDAD, design = des, quantiles = c(0.25, 0.75), na.rm = TRUE))
  }
  
  rows <- list(); k <- 1
  for (trm in terms_cont) {
    rasgo <- sub(":.*$