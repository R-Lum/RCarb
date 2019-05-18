DR_conv_factors <- data.frame(
  REFERENCE = c("Carb2007","Adamiec_Aitken_1998", "Guerin_et_al_2011", "Liritzis_et_al_2013"),
  UB = c(0.146, 0.146, 0.1457,0.1459),
  UB_X = c(0.001, NA, NA, 0.0004),
  TB = c(0.027, 0.0273, 0.0277, 0.0275),
  TB_X = c(0.001, NA, NA, 0.0009),
  KB = c(0.786, 0.782, 0.7982, 0.8011),
  KB_X = c(0.008, NA, NA, 0.0073),
  UG = c(0.113, 0.113, 0.1116, 0.1118),
  UG_X = c(0.002, NA, NA, 0.0002),
  TG = c(0.048, 0.0476, 0.0479, 0.0481),
  TG_X = c(0.002, NA, NA, 0.0002),
  KG = c(0.245, 0.243, 0.2491, 0.2498),
  KG_X = c(0.005, NA, NA, 0.0048),
  stringsAsFactors = FALSE
)



##old conversion factors
#handles.UB <- 0.146; handles.UB_X <- 0.001 ##TODO: check whether his is really needed
#handles.TB <- 0.027; handles.TB_X <- 0.001 ##TODO: check whether his is really needed
#handles.KB <- 0.786; handles.KB_X <- 0.008 ##TODO: check whether his is really needed
# handles.UG <- 0.113; handles.UG_X <- 0.002 ##TODO: check whether his is really needed
#handles.TG <- 0.048; handles.TG_X <- 0.002 ##TODO: check whether his is really needed
#handles.KG <- 0.245; handles.KG_X <- 0.005 ##TODO: check whether his is really needed
