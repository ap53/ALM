### Funciones para BorradorAlarmas.R  v0.93

noExposureID <- c("Cash and Banks",
                  "Accounts Payable",
                  "Sales - Pending Settlement",
                  "Accounts Receivable",
                  "Purchases - Pending Settlement",
                  "Other Receivables ",
                  "Other Payables",
                  "Div or Coup-Pending Settlement",
                  "Redemption Debt",
                  "Contingent Liabilities")



librerias_base <- function(){
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(lubridate))
  suppressPackageStartupMessages(library(stringr))
  suppressPackageStartupMessages(library(tidyr))
  suppressPackageStartupMessages(library(data.table))
  suppressPackageStartupMessages(library(pryr))
  suppressPackageStartupMessages(library(stringi))
  # suppressPackageStartupMessages(library(readr))
  suppressPackageStartupMessages(library(lazyeval))
}

# improved list of objects
.ls.objects <- function (env = .GlobalEnv, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, env = env)))
  names <- ls(env = env, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    capture.output(print(object.size(x), units = "auto")) })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

# shorthand
lsos <- function(..., n=20) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

corrida_alarmas <- function(filename, fecha_inicial = NULL, fecha_final = fecha_inicial){
  # # Calls to cambiar_fecha_base are ignored during a corrida_alarmas,
  # # fecha_base is controlled by a for loop
  # #     ==> reset fecha_base to "pnl_last_day" on exit.
  on.exit(fecha_base <<- alarm_env$max_fecha_datos)
  
  if (is.null(fecha_inicial)) {
    fecha_inicial <- (DATOS %>% filter(IxDia == 0) %>% select(Fecha))[[1]]
  } else {
    # Not really setting fecha_base, just taking advantage of date validation
    fecha_inicial <- cambiar_fecha_base(fecha_inicial)
  }
  
  
  
  nombre_backup <- paste0(path, 'Backups_Alarmas//Alarmas ', str_replace_all(Sys.time(), ':', ''), '.csv')
  
  # Al no haber sido evaluada fecha_final hasta ahora, el default del argumento 
  # hace que, si no había sido especificada en el llamado, fecha_final YA TIENE el
  # mismo valor que fecha_inicial.
  if (fecha_final != fecha_inicial) {
    # Not really setting fecha_base, just taking advantage of date validation
    fecha_final <- cambiar_fecha_base(fecha_final)
  }
  
  
  
  by <- ifelse(fecha_inicial > fecha_final, -1, 1)
  
  alarm_env$init_cant_alarmas()
  
  if (str_detect(filename, '\\.R$')) {
    fechas <- seq(fecha_inicial, fecha_final, by = by)
    for (ix in seq_along(fechas)) {
      fecha_base <<- fechas[ix]
      if(!wday(fecha_base) %in% c(1, 7)) {
        ######################################################
        source(filename, print.eval = TRUE, encoding = 'utf-8')
        ######################################################
      }
    }
  } else if (str_detect(filename, '\\.yaml$')) {
    print('Todavía no está...')
  } else stop("Sólo se pueden procesar .R o .yaml")
  
  print(alarm_env$ver_cuantas_corrieron())
  
  # Save id_alarma to file
  id_alarma <- alarm_env$id_alarma
  cat(id_alarma,file = "id_alarmas_file.txt",sep = "\n")
  
}

carga_datos <- function(flias, fecha_truncado = NULL){
  librerias_base()
  
  if(!dir.exists(file.path(path, 'Backups_Alarmas')))
    dir.create(file.path(path, 'Backups_Alarmas'))
  
  # Dataframes defined in this procedure are read-only and will be used interactively
  # I need them in the .GlobalEnv
  datos_long <<- leer_datos_long() # Reads the RDS file
  dias <<- datos_long %>% filter(source == 'pnl') %>% select(date) %>% unique %>% arrange(desc(date)) %>% mutate(IxDia = row_number() - 1)
  bulk <<- leer_bulk() # Reads the RDS file
  pnl <<- extraer_pnl() # Extracts pnl from datos_long    ¿DO I NEED THIS?
  # prc <<- leer_prc()
  familias <<- leer_familias()
  
  if (!is.null(fecha_truncado)) truncar_fechas_recientes(fecha_truncado)
  
  df <- generar_df(pnl)
  
  # Define fecha_base as global so it can be used interactively
  if (exists('usarYY') && usarYY){
    fecha_base <<- (datos_long %>% filter(source == 'pnl') %>% summarize(max(date)))[[1]]
  } else {
    fecha_base <<- (df %>% filter(IxDia == 0) %>% select(Fecha))[[1]]
  }
  alarm_env$max_fecha_datos <- fecha_base
  
  # Build data series to be used in alarm definitions
  bulk2 <- familias %>% tbl_df %>% 
    filter(Descripcion %in% familias_importantes) %>% 
    select(Familia = Descripcion, CarteraNom) %>% 
    inner_join(bulk %>% 
                 select(Periodo, CarteraNom, Valuacion, Cantidad, 
                        AT12, TipoNombre, EspecieNombre) %>% 
                 mutate(Margen = Valuacion, Exposure = abs(Valuacion)), 
               by = 'CarteraNom') %>% 
    gather(variable, value, Margen, Cantidad, Exposure) %>% 
    mutate(borrar = 
             ifelse (
               (variable == 'Margen' & 
                  ((AT12 != 'Cash Margin Accounts') | 
                     (TipoNombre != 'Cash and Banks') | 
                     (!str_detect(EspecieNombre, ' Margin ')))
               ) |
                 variable == 'Exposure' & (TipoNombre %in% noExposureID), 1, 0) ) %>% 
    select(date = Periodo, ticker = Familia, variable, value, 
           borrar, AT12, TipoNombre, EspecieNombre) %>% 
    filter(borrar != 1) %>% 
    tbl_df
  
  bulk3 <- bulk2 %>% select(date, ticker, variable, value) %>% 
    group_by(date, ticker, variable) %>% summarise(value = sum(value)) %>% 
    mutate(source = 'bulk') %>% ungroup
  
  datos_long <<- rbind(datos_long, bulk3)
  #   bulk3 %>% filter(date == fecha_base) %>% arrange(variable, ticker, date)
  #   DATOS %>% filter(Fecha == fecha_base) %>% select(starts_with('Cantidad BRE'))
  #   
  #   bulk3 %>% filter(date == fecha_base) %>% filter(variable == 'Margen') %>% arrange(variable, ticker, date)
  #   DATOS %>% filter(Fecha == fecha_base) %>% select(starts_with('Margen BREAKOUTS'))
  #   
  #   bulk3 %>% filter(date == fecha_base) %>% filter(variable == 'Exposure') %>% arrange(variable, ticker, date)
  #   DATOS %>% filter(Fecha == fecha_base) %>% select(starts_with('Exposure BREA'))
  
  # rm(bulk2, bulk3)
  
  df <- generar_margen(df, flias)
  df <- generar_cantidad(df, flias)
  # df <- generar_patrimonio_final(df, flias)
  df <- generar_exposure(df, flias)
  # df <- generar_resultado(df, flias)
  df <- generar_objetos_pnl(df, flias)
  
  diccionario <<- armar_diccionario()
  
  return(df)
}

truncar_fechas_recientes <- function(fecha_truncado){
  if (class(fecha_truncado) == 'character') fecha_truncado <- dmy(fecha_truncado)
  
  if (class(fecha_truncado) != 'Date') stop(paste(fecha_truncado, 'no es una fecha válida'))
  if (fecha_truncado <= dmy(1/1/2012)) stop(paste(fecha_truncado, 'es demasiado temprano'))
  
  bulk <- bulk %>% 
    filter(Periodo <= fecha_truncado) 
  
  pnl <- pnl %>% 
    filter(Fecha <= fecha_truncado) 
}


leer_bulk <- function(){
  bulk <- as.data.frame(readRDS(paste0(path, "bulk.RDS")))
  bulk <- bulk %>% 
    select(CarteraNom, Periodo, Especie, EspecieNombre, Codex1, Codex2, Tipo, 
           TipoNombre, Cantidad, CantPorLote, Precio, MonCodigoPrecio, TCambioPrecio, 
           Valuacion, Exposure, LongExposure, ShortExposure, Resultado, 
           PatrimonioCartera, Cartera, AT13, AT12, Estructura, AT2, AT11, Margin)
}

leer_datos_long <- function(){
  datos0 <- as.data.frame(readRDS(paste0(path, "data_long.RDS")))
}

extraer_pnl <- function(){
  # pnl <- as.data.frame(readRDS(paste0(path, "metricas.RDS")))
  pnl0 <- datos_long %>% filter(source == 'pnl') %>% select(-source)
  pnl <- pnl0 %>% spread(variable, value, fill = NA_real_) %>% 
    select(Fecha = date, NombreFamilia = ticker, Familia, PatFamilia, RentDiaria, Suscripciones, 
           Rescates, PorcRentDiaria, PorcRentAcumFamilia, PatFamiliaFinal, 
           Exposicion, Margen_Cuentas, Patrimonio_Bulk, Resultado_Bulk, 
           Margen_Assets, Max_Expo_Activo, Asset_Over_Pat_Limit, Bonds_Expo, 
           Long_Calls, Short_Calls, Long_Puts, Short_Puts, Short_Futures, 
           Long_Futures, Margen_Over_Equity_Ratio, Leverage_Ratio, 
           Dif_Margenes_Ratio, Cobertura_Calls, Cobertura_Puts, 
           Cobertura_Calls_Puts, Cobertura_Calls_Futuros, Calls_when_Long_Futures, 
           Profit_Factor, Patrimonio_Futuros_Adm, Vol_vs_Alfa, nav)
}

# leer_prc <- function(){
#   prc0 <- datos_long %>% filter(source == 'bloomberg') %>% select(-source)
#   prc <- prc0 %>% spread(variable, value, fill = NA_real_) %>% 
#     rename(Fecha = date, NombreFamilia = ticker, CLOSE = PX_LAST)
# }

limitar_ultima_fecha <- function(last_day){
  stopifnot(class(last_day) == 'Date')
}

leer_familias <- function(){
  familias <- as.data.frame(readRDS(paste0(path, "familias.RDS")))
}

limpiar_datos <- function(){
  
}

generar_df <- function(X){
  datos <- X %>% distinct(Fecha) %>% select(Fecha) %>% 
    arrange(desc(Fecha)) %>% 
    mutate(IxDia = row_number() - 1) %>% tbl_df
  
  return(datos)
}

cambiar_fecha_base <- function(fecha = NULL){
  # The default is to calculate alarms as seen from the most recent day in the data.
  # This date is stored in a global variable, "fecha_base"
  #
  # To ensure that we don't run the alarms with an old "fecha_base", this variable is
  # explicitely reset both at the end of carga_datos and at the beginning of corrida_alarmas.
  #
  # fecha_base can be changed, EVEN IN THE MIDDLE of a 'corrida_alarmas' file by calling: 
  # cambiar_fecha_base(new_date), with a dmy format. Example: 
  # fecha_base <- cambiar_fecha_base('22/8/2015')
  # 
  # After being changed, fecha_base can be easily reset to its original value by calling: 
  # cambiar_fecha_base(), with no argument.
  
  if(as.character(sys.calls()[[1]])[1] == 'corrida_alarmas' && length(sys.calls()) > 2) {
    warning('Las solicitudes de cambio de fecha base se ignoran durante las corridas de alarmas...')
    return(fecha_base)
  }
  
  if (is.null(fecha)) {
    return((DATOS %>% filter(IxDia == 0) %>% select(Fecha))[[1]])
  } else {
    if (class(fecha) == 'character') {
      
      try(ff <- suppressWarnings(dmy(fecha)))
      suppressWarnings(
        if (is.na(ff)) {
          stop(paste(fecha, 'no es una fecha válida'))
        } else {
          fecha <- ff
        }
      )
    }
    
    if (class(fecha) != 'Date') stop(paste(fecha, 'no es una fecha válida'))
    
    suppressWarnings(
      if (length(fecha) > 1) stop('Sólo se puede pasar una fecha')
    )
    
    if (fecha <= (DATOS %>% arrange(Fecha) %>% slice(1L) %>% select(Fecha))[[1]])
      stop(paste(fecha, 'es demasiado temprano'))
    
    if (wday(fecha) %in% c(1, 7)) stop(paste(fecha, 'es fin de semana'))
    
    ff <- DATOS %>% filter(Fecha == fecha) %>% select(Fecha)
    if (ff %>% nrow == 0 ) stop(paste(fecha, 'no figura en datos'))
    return(ff[[1]])
  }
}


# pnl %>% distinct(Fecha) %>% select(Fecha) %>% 
#   filter(Fecha <= base_day) %>% 
#   arrange(desc(Fecha)) %>% 
#   mutate(IxDia = row_number()) %>% tbl_df


generar_margen <- function(datos_local, flias){
  nombre_serie <- 'Margen'
  
  for (familia_target in flias) {
    
    carteras <- bulk %>% 
      inner_join(familias %>% 
                   filter(Descripcion == familia_target) %>% 
                   select(Familia = Descripcion, CarteraNom), 
                 by = 'CarteraNom') %>% 
      distinct(CarteraNom) %>% select(CarteraNom) %>% 
      unname %>% unlist
    
    margen <- bulk %>%
      filter(CarteraNom %in% carteras) %>%
      ### Filtro específico de margen
      filter(
        AT12 == 'Cash Margin Accounts' &
          TipoNombre == 'Cash and Banks' &
          str_detect(EspecieNombre, ' Margin ')) %>%
      ###
      group_by(Periodo) %>%
      summarise(Margen = sum(Valuacion)) %>% 
      select(Periodo, Margen)
    
    names(margen) <- c('Fecha', paste(nombre_serie, str_trim(familia_target)))
    
    datos_local <- datos_local %>% left_join(margen, by = 'Fecha')
  }
  return(datos_local)
}


generar_cantidad <- function(datos_local, flias){
  nombre_serie <- 'Cantidad'
  
  for (familia_target in flias) {
    
    carteras <- bulk %>% 
      inner_join(familias %>% 
                   filter(Descripcion == familia_target) %>% 
                   select(Familia = Descripcion, CarteraNom), 
                 by = 'CarteraNom') %>% 
      distinct(CarteraNom) %>% select(CarteraNom) %>% 
      unname %>% unlist
    
    cantidad <- bulk %>%
      filter(CarteraNom %in% carteras) %>%
      ### Filtro específico de cantidad
      ###
      group_by(Periodo) %>%
      summarise(Cantidad = sum(Cantidad)) %>% 
      select(Periodo, Cantidad)
    
    names(cantidad) <- c('Fecha', paste(nombre_serie, str_trim(familia_target)))
    
    datos_local <- datos_local %>% left_join(cantidad, by = 'Fecha')
  }
  return(datos_local)
}


generar_patrimonio_final <- function(datos_local, flias){
  nombre_serie <- 'PatrFinal'
  
  for (familia_target in flias) {
    patrimonio <- 
      pnl %>% 
      filter(NombreFamilia == familia_target) %>% 
      select(Fecha, PatFamiliaFinal)
    
    names(patrimonio) <- c('Fecha', paste(nombre_serie, str_trim(familia_target)))
    
    datos_local <- datos_local %>% left_join(patrimonio, by = 'Fecha')
  }
  return(datos_local)
}



generar_objetos_pnl <- function(datos_local, flias){
  # Could do it in one step...
  serie <- pnl %>% tbl_df %>% 
    filter(NombreFamilia %in% flias) %>% 
    select(-c(Familia, PorcRentAcumFamilia))
  
  serie <- serie %>% gather(columna, valor, -Fecha, -NombreFamilia) %>%
    unite(Dato_Familia, columna, NombreFamilia, sep = ' ') %>%
    spread(Dato_Familia, valor)
  
  datos_local <- datos_local %>% left_join(serie, by = 'Fecha')
  
  return(datos_local)
}


generar_objetos_X <- function(X, datos_local, flias){
  # Could do it in one step...
  if("Familia" %in% names(X)) X$Familia <- NULL
  if("PorcRentAcumFamilia" %in% names(X)) X$PorcRentAcumFamilia <- NULL
  
  serie <- X %>% tbl_df
  
  if(deparse(substitute(X)) == 'pnl') 
    serie <- serie %>% filter(NombreFamilia %in% flias)
  
  serie <- serie %>% gather(columna, valor, -Fecha, -NombreFamilia) %>%
    unite(Dato_Familia, columna, NombreFamilia, sep = ' ') %>%
    spread(Dato_Familia, valor)
  
  datos_local <- datos_local %>% left_join(serie, by = 'Fecha')
  
  return(datos_local)
}

generar_exposure <- function(datos_local, flias){
  nombre_serie <- 'Exposure'
  
  for (familia_target in flias) {
    
    carteras <- bulk %>% 
      inner_join(familias %>% 
                   filter(Descripcion == familia_target) %>% 
                   select(Familia = Descripcion, CarteraNom), 
                 by = 'CarteraNom') %>% 
      distinct(CarteraNom) %>% select(CarteraNom) %>% 
      unname %>% unlist
    
    exposure <- bulk %>%
      filter(CarteraNom %in% carteras) %>%
      ### Filtro específico de exposure
      filter(!(TipoNombre %in% noExposureID)) %>%
      ###
      group_by(Periodo) %>%
      summarise(Exposure = sum(abs(Valuacion))) %>% 
      select(Periodo, Exposure)
    
    names(exposure) <- c('Fecha', paste(nombre_serie, str_trim(familia_target)))
    
    datos_local <- datos_local %>% left_join(exposure, by = 'Fecha')
  }
  return(datos_local)
}

generar_resultado <- function(datos_local, flias){
  nombre_serie <- 'Resultado'
  
  for (familia_target in flias) {
    resultado <- 
      pnl %>% 
      filter(NombreFamilia == familia_target) %>% 
      select(Fecha, RentDiaria)
    
    names(resultado) <- c('Fecha', paste(nombre_serie, str_trim(familia_target)))
    
    datos_local <- datos_local %>% left_join(resultado)
  }
  return(datos_local)
}

# DICO CHANGE
armar_diccionario <- function() {
  datos_long %>% 
    select(variable, source) %>% unique %>% 
    arrange(source, variable)
}
# diccionario <- armar_diccionario()

diccionario_terminos <- function(mostrar_llaves = FALSE){
  if (exists('usarYY') && usarYY){
    terminos <- (diccionario %>% select(variable))[[1]]
    
    if (mostrar_llaves) return(terminos) else return(terminos[which(!str_detect(terminos, '_Ovr_'))])
  } else {
    terminos <- colnames(DATOS)[-c(1, 2)] %>% str_replace(' .*$', '') %>% unique()
    if (mostrar_llaves) return(terminos) else return(terminos[which(!str_detect(terminos, '_Ovr_'))])
  }
}

verificar_diccionario <- function(nombre){
  as.list(diccionario %>% filter(variable == nombre))
}


leer_yaml <- function(archivo){
  print(paste('Esto va a leer', archivo, 'y generar los llamados a correr_alarma() que siguen'))
}

correr_alarma <- function(expr, importancia = 5, flias_1, flias_2 = flias_1, 
                          flias_3 = flias_1, flias_4 = flias_2, tipo = '', 
                          mensaje_corto = '', mensaje = '', 
                          # parametros usados cuando se llama desde la funcion 'crossover'
                          paso_crossover = 0){
  
  tipo <- str_replace_all(tipo, ',', ' ')
  mensaje_corto <- str_replace_all(mensaje_corto, ',', ' ')
  mensaje <- str_replace_all(mensaje, ',', ' ')
  
  # call_tree_hadley(substitute(expr))
  # reemplazo <- call_tree_alarma(substitute(expr))
  expr_0 <- deparse(substitute(expr))
  expr_q <- paste0(str_trim(deparse(substitute(expr))), sep = ' ', collapse = '')
  
  # CAMBIAR ESTO!!
  flias_1 <- str_trim(flias_1)
  flias_2 <- str_trim(flias_2)
  flias_3 <- str_trim(flias_3)
  flias_4 <- str_trim(flias_4)
  
  probar <- 0
  if (probar == 1) {
    lista1 <- as.list(substitute(expr))
    
    lista1[[2]][[1]] <- as.character(lista1[[2]][[1]])
    lista1[[2]] <- as.call(c(quote(mifunc1), as.list(lista1[[2]])))
    lista1[[2]][[length(lista1[[2]]) + 1]] <- 'aaa'
    names(lista1[[2]])[length(lista1[[2]])] <- 'm'
    lista1[[2]][[length(lista1[[2]]) + 1]] <- 100
    names(lista1[[2]])[length(lista1[[2]])] <- 'z'
    
    eval(lista1[[2]])
    eval(as.call(lista1))
  }
  
  res <- list(expr = expr_q)
  
  res$fecha_base <- fecha_base
  
  flias <- c(flias_1, flias_2, flias_3, flias_4)
  info_flias <- flias
  if (flias_4 == flias_2) info_flias <- info_flias[-4]
  if (flias_3 == flias_1) info_flias <- info_flias[-3]
  if (flias_2 == flias_1) info_flias <- info_flias[-2]
  
  res$flias <- paste0(info_flias, collapse = ' | ')
  
  # Primero evalúo las llaves
  llaves <- str_match_all(expr_q, '\\{.*?\\}')[[1]]
  
  for (llave in llaves) {
    lista_result_llave <- evaluar_llave(llave, flias, primer_flia = 1)
    expr_q <- stri_replace_all_fixed(expr_q, llave, lista_result_llave[[1]])
    
    # Update DATOS in global environment
    if (!(exists('usarYY') && usarYY)) {
      DATOS <<- lista_result_llave[[2]]
    }
  }
  
  # terminos_parciales <- unlist(str_split(expr_q, '>[=]?|==|!=|<[=]?'))
  # simbolo_comparacion <- str_extract(expr_q, ' *(>[=]?|==|!=|<[=]?) *')
  
  # Luego las expresiones sueltas restantes
  llamadas <- 
    str_match_all(expr_q, '[a-zA-Z0-9_]*\\(.*?\\)')[[1]]
  
  for (ix in 1:length(llamadas)) {
    llamada <- llamadas[ix, ]
    valor <- try(a <- evaluar_termino(llamada, flias[ix]))
    
    #
    
    if (inherits(valor, 'try-error') || inherits(valor, 'error')) {
      mensaje <- str_split(as.character(valor[[1]]), '//')[[1]]
      res$mensaje <- mensaje[1]
      res$es_pnl <- str_detect(mensaje[2], 'pnl')
      # break()
    } else {
      # For data-checking ONLY!!! 
      # post_proceso = 'serie' is the only argument value that is allowed to
      # return a vector with length > 1: if this occurs, stop computing and
      # dump the full serie to screen so the analyst is able to verify the data
      if (length(valor) > 1) stop(paste0(valor, collapse = '\n'))
      
      valor[[1]] <- ifelse(is.nan(valor[[1]]), NA_real_, valor[[1]])
      
      valor <- ifelse(abs(valor) < 1, 
                      round(valor, 3), 
                      ifelse(abs(valor) < 1000, 
                             round(valor, 2), 
                             round(valor, 0)))
      
      expr_q <- stri_replace_all_fixed(expr_q, llamada, valor)
    }
  }
  
  
  if (is.null(res$mensaje) && !is.na(expr_q)) {
    res$eval <- expr_q
    
    terminos_parciales <- unlist(str_split(expr_q, '>[=]?|==|!=|<[=]?'))
    simbolo_comparacion <- str_extract(expr_q, ' *(>[=]?|==|!=|<[=]?) *')
    result_parciales <- sapply(X = terminos_parciales, FUN = function(y) eval(parse(text = y)))
    
    result_parciales <- ifelse(abs(result_parciales) < 1, 
                               round(result_parciales, 3), 
                               ifelse(abs(result_parciales) < 1000, 
                                      round(result_parciales, 2), 
                                      round(result_parciales, 0)))
    
    
    resultados_parciales <- paste(result_parciales, collapse = simbolo_comparacion)
    res$result_parciales <- resultados_parciales
    
    
    # Do I want to show partials on screen? Not if they don't add anything... 
    res$misma_formula <- (str_trim(resultados_parciales) == str_trim(expr_q))
    
    
    res$alarma <- eval(parse(text = expr_q))
  } else {
    # "eval" | "result_parciales" | "misma_formula"
    res$eval <- "ERROR AL CORRER LA ALARMA"
    res$result_parciales <- "NA"
    res$misma_formula <- TRUE
    res$alarma <- NA
  }
  
  # Retoque de campos para JIRA (emprolijar)
  if (is.na(res$alarma)) {
    res$alarma <- TRUE
    if (is.null(res$es_pnl) || is.na(res$es_pnl)) {
      res$importancia <- importancia
    } else {
      # On 'lack of data' errors, pnl is serious, other cases not so serious...
      res$importancia <- ifelse(res$es_pnl, -5, -1)
      # Remove res$es_pnl so it doesn't break preparar_output later on
      res$es_pnl <- NULL
    }
    res$tipo <- tipo
    res$mensaje_corto <- 'ERROR AL CORRER LA ALARMA'
    if (is.null(res$mensaje)) res$mensaje <- 'ERROR AL CORRER LA ALARMA'
  } else {
    res$importancia <- sign(res$alarma) * importancia
    res$tipo <- tipo
    res$mensaje_corto <- if (res$alarma) {
      mensaje_corto
      
      
    } else {
      iconv('No se disparó' , from = "", to = "UTF-8")
    }
    res$mensaje <- if (res$alarma) {
      str_replace_all(mensaje, '\n +', '\n')
    } else {''}
    
  }
  
  # rm(familia_target_, envir=globalenv())
  
  if (paso_crossover == 0) { # correr_alarma fue llamada directamente
    salida_pantalla <- preparar_output(res, archivo_salida, silencioso = FALSE)
    cat(salida_pantalla, '\n\n')
  } else { # correr_alarma fue llamada a través de crossover
    return(res)
  }
}


# Shorter call!
c_a <- correr_alarma

crossover <- function(..., permanente = 0){
  chk_hoy <- correr_alarma(..., paso_crossover = 1)
  
  chk_ayer <- correr_alarma(..., paso_crossover = 2)
  
  hay_error <- 0
  if (str_detect(chk_hoy$mensaje_corto, 'ERROR') || str_detect(chk_ayer$mensaje_corto, 'ERROR')) {
    hay_error <- 1
  } 
  
  if (  !chk_hoy$alarma & !chk_ayer$alarma){
    # No se disparó ni ayer ni hoy, mando resultados de hoy
    if (!hay_error) chk_hoy$mensaje_corto <- 'No hay crossover (alarma apagada)'
    
  } else if (chk_hoy$alarma & !chk_ayer$alarma){
    # No se disparó ayer pero hoy sí: se prendió la alarma, hay crossover (+1)
    if (!hay_error) chk_hoy$mensaje_corto <- paste0('Se prendió la alarma: "', chk_hoy$mensaje_corto, '"')
    chk_hoy$result_parciales <- paste(chk_ayer$result_parciales, '|', chk_hoy$result_parciales)
    
  } else if (!chk_hoy$alarma & chk_ayer$alarma){
    # Se disparó ayer pero hoy no: se apagó la alarma, hay crossover (-1)
    # Apago la alarma y aviso que no hay crossover
    if (!hay_error) {
      chk_hoy$alarma <- TRUE
      chk_hoy$importancia <- chk_ayer$importancia
      chk_hoy$mensaje_corto <- paste0('Se apagó la alarma: "', chk_ayer$mensaje_corto, '"')
      chk_hoy$mensaje <- paste0('Se apagó la alarma: "', chk_ayer$mensaje, '"')
    }
    chk_hoy$result_parciales <- paste(chk_ayer$result_parciales, '|', chk_hoy$result_parciales)
    
  } else if ( chk_hoy$alarma &  chk_ayer$alarma){
    # Se disparó ambas veces: no hay crossover, pero si piden 'permanente' lo informo igual
    if (!hay_error) {
      chk_hoy$mensaje_corto <- paste0(chk_hoy$mensaje_corto, ': no es crossover')
      if (!permanente) {
        # Apago la alarma
        chk_hoy$alarma <- FALSE
        chk_hoy$importancia <- 0
        chk_hoy$mensaje_corto <- paste0(chk_hoy$mensaje_corto, ': no se disparó.')
      }
    }
  }
  
  salida_pantalla <- preparar_output(chk_hoy, archivo_salida)
  cat(salida_pantalla, '\n\n')
}

evaluar_llave <- function(expr, flias, primer_flia){
  expr_q <- str_sub(expr, 2, str_length(expr) - 1)[[1]]
  
  llamadas <- str_match_all(expr_q, '[a-zA-Z_]*\\(.*?\\)')[[1]]
  
  # Make composite name
  llamada_1 <- str_replace(llamadas[1, ], '\\(.*', '')
  llamada_2 <- str_replace(llamadas[2, ], '\\(.*', '')
  nombre_compuesto <- paste(llamada_1, 'Ovr', llamada_2, sep = '_')
  
  if (exists('usarYY') && usarYY) {
    ticker_ <- paste(flias[1], '|', flias[2])
    subserie_1 <- datos_long %>% filter(ticker == ticker_, variable == nombre_compuesto) 
    
    ya_existe <- nrow(subserie_1) > 0
  } else {
    nombre_compuesto_familia <- paste(nombre_compuesto, flias[1], sep = ' ')
    ya_existe <- nombre_compuesto_familia %in% colnames(DATOS)
  }
  
  if (!ya_existe){
    subserie <- list(primera = vector('numeric', 0L),
                     segunda = vector('numeric', 0L) )
    
    if (exists('usarYY') && usarYY) {
      nombre_1 <- paste(llamada_1, flias[1], sep = ' ')
      ticker_ <- flias[1]
      subserie_1 <- datos_long %>% filter(ticker == ticker_, variable == llamada_1) 
      # Hold on to the source, it'll be useful in the error reporting if there is not enough data.
      source_1 <- (subserie_1 %>% select(source) %>% slice(1))[[1]]
      # Remake 'pnl' out of possible 'local pnl'
      if (str_detect(source_1, 'pnl') && (source_1 != 'pnl')) source_1 <- 'pnl'
      
      nombre_2 <- paste(llamada_2, flias[1], sep = ' ')
      ticker_ <- flias[2]
      subserie_2 <- datos_long %>% filter(ticker == ticker_, variable == llamada_2) 
      # I won't look at source_2, there is not much I can do if it's different from source_1...
      
      serie <- subserie_1 %>% 
        inner_join(subserie_2, by = 'date') %>% 
        rename(ticker = ticker.x, variable = variable.x, value = value.x, source = source.x) %>% 
        mutate(ticker = paste(flias[1], '|', flias[2]),
               variable = nombre_compuesto,
               value = value / value.y,
               source = paste('local', source_1)) %>% 
        select(-ends_with('.y'))
      
      datos_long <<- rbind(datos_long, serie)
      dic_local <- diccionario
      dic_local <- rbind(dic_local, 
                         data_frame(variable = nombre_compuesto, 
                                    source = source_1)
      )
      diccionario <<- dic_local
    } else {
      nombre_1 <- paste(llamada_1, flias[1], sep = ' ')
      ix_columna_datos <- which(colnames(DATOS) == nombre_1)
      subserie_1 <- DATOS[, ix_columna_datos][[1]]
      
      nombre_2 <- paste(llamada_2, flias[1], sep = ' ')
      ix_columna_datos <- which(colnames(DATOS) == nombre_2)
      subserie_2 <- DATOS[, ix_columna_datos][[1]]
      
      serie <- (subserie_1 / subserie_2)
      
      DATOS[, length(colnames(DATOS)) + 1] <- serie
      colnames(DATOS)[length(colnames(DATOS))] <- nombre_compuesto_familia 
    }
  }
  
  if (exists('usarYY') && usarYY) {
    return(nombre_compuesto)
  } else {
    return(list(nombre_compuesto, DATOS))
  }
  
}

#### Chanchadas a reemplazar ####
evaluar_termino <- function(f, familia_target){
  # assign("familia_target_", familia_target, envir=globalenv())
  
  # f1 <- deparse(substitute(f))
  f1 <- f  #quote(f)
  term <- str_extract(f1, '^.*?\\(') %>% str_sub(1, str_length(.) - 1)
  argumentos <- str_sub(f1, str_length(term) + 2, str_length(f1) - 1)
  
  # DICO CHANGE
  existe <- verificar_diccionario(term)
  if (length(existe[[1]]) == 0) {
    stop(paste(term, 'no figura en el diccionario de términos válidos'), call. = FALSE)
  } else {
    origen_termino <- existe[[2]]
  }
  nuevo_termino_string <- paste0('armar_lista_args(', argumentos, ')')
  lista_argumentos <- eval(parse(text = nuevo_termino_string))
  lista_argumentos <- c(quote(term), lista_argumentos)
  
  arg <- tryCatch({
    do.call(calcular_termino, lista_argumentos)
  }, error = function(err) {
    err$message <- paste0(err$message, '//', origen_termino)
    err
  })
  a <- 1  # Estas dos últimas líneas son para debugging, para poder poner un breakpoint
  arg     # confiable dentro de esta rutina. Más adelante se pueden sacar ambas...
}

armar_lista_args <- function(...){
  list(...)
}

calcular_termino <- function(serie, duracion = 0, start = NULL, post_proceso = c('percentil'), 
                      p = NULL, ver_serie = FALSE){
  
  # browser()  #####################################################
  
  ticker_ <- get('familia_target', envir = parent.frame())
  
  # serie_ <- lazyeval::as.lazy(serie) 
  
  if (str_detect(serie, '_Ovr_')) {
    ticker_ <- paste(ticker_, '|', ticker_)
  }
  
  # Si estoy en el segundo paso de un crossover(), tengo que mirar el día anterior
  xovr_slide <- ifelse(get('paso_crossover', parent.frame(2)) == 2, 1, 0)
  
  un_solo_dia <- (duracion == 0)
  if (is.null(start)) start <- ifelse(un_solo_dia, 0, 1) + xovr_slide
  
  fecha_start <- (dias %>% filter(IxDia == start) %>% select(date))[[1]]
  
  datos_uno <- datos_long %>% filter(ticker == ticker_, variable == serie) %>% 
    filter(date <= fecha_start) %>% 
    spread(variable, value, fill = NA) %>% 
    filter(complete.cases(.)) %>% 
    arrange(desc(date)) %>% 
    slice(1:duracion) %>% 
    tbl_df
  
  if(ver_serie) {
    # This is just for debugging
    print(datos_uno, n = nrow(datos_uno))
  }
  
  if (nrow(datos_uno) == 0) {
    if (post_proceso %in% c('ultimo', 'primero')) {
      return(ND)
    } else {
      stop('Faltan datos para ', ticker_, '(todos)', call. = FALSE)
    }
  } 
  
  if ( (datos_uno %>% select(date) %>% slice(1L))[[1]] < fecha_base) {
    if (un_solo_dia) {
      if (post_proceso %in% c('ultimo', 'primero')) {
        return(ND)
      } else {
        stop('Faltan datos para ', ticker_, ' (start)', call. = FALSE)
      }
    } else {
      # There are no more NAs, can safely return values
      if (post_proceso =='ultimo') {
        return( (datos_uno %>% select(serie) %>% slice(1L))[[1]] )
      } else if (post_proceso == 'primero') {
        return( (datos_uno %>% select(serie) %>% slice(n()))[[1]])
      } else {
        stop('Faltan datos para ', ticker_, ' (start)', call. = FALSE)
      }
    }
    
  }
  
  # 'start' records exists
  if (un_solo_dia) { # Si es un valor puntual, ya lo puedo devolver...
    resultado <- (datos_uno %>% select(serie) %>% slice(1L))[[1]]
    return( ifelse(is.na(resultado), ND, resultado) )
    
  } else if (nrow(datos_uno) < duracion) { # Si es más de un día, me fijo si tengo suficientes
    if (!post_proceso %in% c('ultimo', 'primero')) {                   # registros...
      stop('Faltan datos para ', ticker_, '(cantidad)', call. = FALSE)
    }
  }

  # If I got here, it means that:
  #    1) duracion > 1
  #    2) Data is OK
  # Just post-process and return
  v_serie <- (datos_uno %>% select(4))[[1]]
  
  if (post_proceso == 'percentil') {
    return(quantile(v_serie, p))
  } else if (post_proceso == 'media') {
    return(mean(v_serie))
  } else if (post_proceso == 'suma'){ 
    return(sum(v_serie))
  } else if (post_proceso == 'max'){ 
    return(max(v_serie))
  } else if (post_proceso == 'min'){ 
    return(min(v_serie))
  } else if (post_proceso == 'signo_suma'){ 
    return(sign(sum(v_serie)))
  } else if (post_proceso == '%pos') {
    return(sum(v_serie > 0) / length(v_serie))
  } else if (post_proceso == '%neg') {
    return(sum(v_serie < 0) / length(v_serie))
  } else if (post_proceso == 'ultimo') {
    return(dato_uno)
  } else if (post_proceso == 'primero') {
    return(dato_uno)
  } else
    stop('post_proceso: ', ticker_, ' desconocido.', call. = FALSE)
}



preparar_output <- function(res, archivo = NULL, silencioso = FALSE) {
  dic_alarmas <- 
    data_frame(importancia = 0L:5L, 
               Prioridad = c('Trivial', 'Trivial', 'Menor', 'Mayor', 'Crítica', 'Bloqueadora'))
  
  alarm_env$contar_alarma(res$importancia)
  
  res$id_alarma <- alarm_env$get_next_alarm_id()
  res$mensaje <- str_replace(res$mensaje, '\n', '')
  res$mensaje <- str_replace(res$mensaje, '\r', '')
  res$mensaje <- str_replace(res$mensaje, 'Error: Falta', 'Falta')
  
  # In corrida_alarmas we can save last id used at the end, in all other cases save it here.
  if(as.character(sys.calls()[[1]])[1] != 'corrida_alarmas') {
    cat(res$id_alarma, file="id_alarmas_file.txt", sep = "\n")
  }
  
  
  
  
  s0 <- as.data.frame(res, stringsAsFactors = FALSE) %>% 
    mutate(fecha_corrida = Sys.Date())
  
  s0 <- s0 %>% left_join(dic_alarmas, by = 'importancia') %>% 
    mutate(Prioridad = ifelse(!is.na(Prioridad), Prioridad, as.character(importancia)))
  
  s0 <- s0 %>% rename(fecha_alarma = fecha_base, cartera_precio = flias,
                      nombre = tipo, resumen = mensaje_corto)
  
  # names(s0) <- c('expr', 'fecha_alarma', 'cartera_precio', 'eval', 'result_parciales',
  #                'misma_formula', 'alarma', 'importancia', 'nombre', 'resumen',
  #                'mensaje', 'id_alarma', 'fecha_corrida', 'Prioridad')
  
  s_jira <- s0 %>% mutate(proyecto = 'ALM', 
                          fecha_corrida = format(fecha_corrida, '%d/%m/%Y'),
                          fecha_alarma  = format(fecha_alarma,  '%d/%m/%Y')) %>% 
    select(id_alarma, fecha_corrida, fecha_alarma, nombre, cartera_precio, 
           resumen, importancia, Prioridad, 'Project Key' = proyecto, free_text = mensaje, 
           'Software indicador de Señal' = result_parciales)
  
  s_long <- s0 %>% gather(dato, valor, -c(id_alarma, fecha_alarma, fecha_corrida)) %>% 
    select(id_alarma, fecha_alarma, fecha_corrida, dato, valor) %>% 
    filter(dato != 'misma_formula')
  
  if (archivo != '') {
    if(!str_detect(archivo, '\\.csv')) sep = '\t' else sep = ', '
    
    if (res$alarma) {
      archivo_jira <- str_replace(archivo, '\\.', '_jira.')
      suppressWarnings(
        write.table(s_jira, file = archivo_jira, 
                    append = TRUE, quote = FALSE, sep = sep,
                    row.names = FALSE, col.names = (!file.exists(archivo_jira)),
                    fileEncoding = 'UTF-8')
      )
      
      if(as.character(sys.calls()[[1]])[1] == 'corrida_alarmas') {
        nombre_backup = sys.frames()[[1]]$nombre_backup
        
        try({
          suppressWarnings(
            write.table(s_jira, file = nombre_backup, 
                        append = TRUE, quote = FALSE, sep = sep,
                        row.names = FALSE, col.names = (!file.exists(nombre_backup)),
                        fileEncoding = 'UTF-8')
          )
          
        })
      }
    }
    
    suppressWarnings(
      write.table(s_long, file = archivo, 
                  append = TRUE, quote = FALSE,  sep = sep,
                  row.names = FALSE, col.names = (!file.exists(archivo)))
    )
  }
  
  if (silencioso){
    return('')
  } else {
    # Don't show fecha_base if it is the same as 'last_day'
    if (res$fecha_base == alarm_env$max_fecha_datos)
      res$fecha_base <- NULL
    else
      res$fecha_base <- paste('fecha_base =', format(fecha_base, "%d/%m/%Y"))
    
    if (res$misma_formula)
      res$eval <- NULL
    
    res$misma_formula <- NULL
    
    res$mensaje <- str_replace_all(res$mensaje, '\n*$', '')
    salida <- paste(res, collapse = "\n")
    
    return(salida)
  }
}

ayuda_debug_llaves <- function(){
  DATOS <- DATOS %>% mutate(`Exposure_Ovr_PatFamiliaFinal BREAKOUTS+DONCHIAN` = 
                              `Exposure BREAKOUTS+DONCHIAN` /
                              `PatFamiliaFinal BREAKOUTS+DONCHIAN`) 
  
  DATOS <- DATOS %>% select(Fecha, IxDia, `PatFamiliaFinal BREAKOUTS+DONCHIAN`, 
                            `Exposure BREAKOUTS+DONCHIAN`,
                            `Exposure_Ovr_PatFamiliaFinal BREAKOUTS+DONCHIAN`)
  
  DATOS <- DATOS %>% select(-`Exposure_Ovr_PatFamiliaFinal BREAKOUTS+DONCHIAN`)
  
  DATOS %>% select(`Exposure_Ovr_PatFamiliaFinal BREAKOUTS+DONCHIAN`)
}

call_tree_hadley <- function(x, width = getOption("width")) 
{
  if (is.expression(x) || is.list(x)) {
    trees <- vapply(x, tree_hadley, character(1), width = width)
    out <- str_c(trees, collapse = "\n\n")
  }
  else {
    out <- tree_hadley(x, width = width)
  }
  cat(out, "\n")
}

tree_hadley <- function(x, level = 1, width = getOption("width"), branch = "\\- ") 
{
  indent <- str_c(str_dup("  ", level - 1), branch)
  # browser()
  if (is.atomic(x) && length(x) == 1) {
    label <- paste0(" ", deparse(x)[1])
    children <- NULL
  }
  else if (is.name(x)) {
    x <- as.character(x)
    if (x == "") {
      label <- "`MISSING"
    }
    else {
      label <- paste0("`", as.character(x))
    }
    children <- NULL
  }
  else if (is.call(x)) {
    # browser()
    label <- "()"
    children <- vapply(as.list(x), tree_hadley, character(1), level = level + 
                         1, width = width - 3)
  }
  else if (is.pairlist(x)) {
    label <- "[]"
    branches <- paste("\\", format(names(x)), "=")
    children <- character(length(x))
    for (i in seq_along(x)) {
      children[i] <- tree_hadley(x[[i]], level = level + 1, width = width - 
                                   3, branch = branches[i])
    }
  }
  else {
    if (inherits(x, "srcref")) {
      label <- "<srcref>"
    }
    else {
      label <- paste0("<", typeof(x), ">")
    }
    children <- NULL
  }
  #label <- str_trunc(label, width - 3)
  if (is.null(children)) {
    paste0(indent, label)
  }
  else {
    paste0(indent, label, "\n", paste0(children, collapse = "\n"))
  }
}

call_tree_alarma <- function(x) 
{
  if (is.expression(x) || is.list(x)) {
    reemplazo <- vapply(x, tree_alarma, character(1), width = width)
    out <- str_c(trees, collapse = "\n\n")
  }
  else {
    reemplazo <- tree_alarma(x)
  }
  if (exists('ver_arbol') && (ver_arbol != 0)){
    cat(reemplazo, "\n")
  }
  return(reemplazo)
}

tree_alarma <- function(x, level = 1, branch = "\\- ") 
{
  indent <- str_c(str_dup("  ", level - 1), branch)
  # browser()
  salidas <- ''
  valor <- ''
  
  if (is.atomic(x) && length(x) == 1) {
    label <- paste0(" ", deparse(x)[1])
    valor <- tree_eval_parcial(x)
    children <- NULL
  }
  else if (is.name(x)) {
    x <- as.character(x)
    if (x == "") {
      label <- "`MISSING"
      valor <- tree_eval_parcial(NA)
    }
    else {
      label <- paste0("`", as.character(x))
      valor <- tree_eval_parcial(x)
    }
    children <- NULL
  }
  else if (is.call(x)) {
    # browser()
    label <- "()"
    if (!exists(as.character(x[[1]]))) { # If it's an existing function, just run it, but if it's not...
      nombre_serie_correcto <- verificar_diccionario(as.character(x[[1]]))
      if (is.na(nombre_serie_correcto)) {
        stop(paste(as.character(x[[1]]), 'no existe en el diccionario.'))
      } else {
        
        x[[1]] <- as.character(x[[1]])
        x <- as.call(c(quote(terminoXX), as.list(x)))
        
        # eval(x)
        # eval(as.call(x))
        
        # x[[length(x) + 1]] <- 'aaa'
        # names(x)[length(x)] <- 'm'
        # x[[length(x) + 1]] <- 100
        # names(x)[length(x)] <- 'z'
      }
    }
    valor <- tree_eval_parcial(x)
    children <- vapply(as.list(x), tree_alarma, character(1), level = level + 1)
  }
  if (is.null(children)) {
    texto <- paste0(indent, label)
    salidas <- paste0(valor, salidas, collapse = '&&&')
  }
  else {
    texto <- paste0(indent, label, "\n", paste0(children, collapse = "\n"))
    salidas <- paste0(valor, salidas, collapse = '¿¿¿')
  }
}

tree_eval_parcial <- function(x){
  
  # strings <- 'a + { c / {d + e} - {f - g}  } '
  # pattern <- '\\{[^{}]*\\}'
  # str_extract(strings, pattern)
  
  intento <- try(eval(substitute(x)))
  if (!inherits(intento, "try-error")) {
    print(paste('Anduvo eval:   ', as.character(substitute(x)), '     :', as.character(intento)))
    return(paste0(as.character(intento), collapse = '###'))
  } else {
    print(paste('Falló eval:   ', as.character(substitute(x))))
    return(paste0(as.character(substitute(x)), collapse = '$$$'))
  }
}


asignar_familias <- function(){
  preparar_vectores_flias()
  if (length(flias_izq) == 0){
    print('**** ERROR: falta la primer familia')
  }
  
  
}


verif_uso_patron <- function(patron) {
  lista <- list()
  lista$f <- familias %>% select(Descripcion, CarteraNom) %>% 
    filter(str_detect(Descripcion, patron) | str_detect(CarteraNom, patron)) %>% 
    arrange(Descripcion)
  
  
  lista$b <- bulk %>% select(Especie, EspecieNombre) %>% 
    filter(str_detect(Especie, patron) | str_detect(EspecieNombre, patron)) %>% 
    arrange(EspecieNombre)
  
  rbindlist(lista) %>% rename(Nombre = CarteraNom) %>% tbl_df
}

lista_Unicodes <- function(){
  v <- c('á', 'Á', 'é', 'É', 'í', 'Í', 'ó', 'Ó', 'ú', 'Ú', 'ñ', 'Ñ')
  V <- sapply(v, function(ch) as.integer(charToRaw(ch)))
  cat(v)
  cat(V)
  sprintf("%s   -->   Unicode u%s", v, V)
}

see <- function(ticker_, variable_ = '', slice_ = 1:10) {
  d <- datos_long
  if (ticker_ != '')
    d <- d %>% filter(ticker == ticker_)
  
  if (variable_ != '')
    d <- d %>% filter(variable == variable_)
  
  d %>% arrange(desc(date)) %>% slice(slice_)
}

init_alarm_env <- function(){
  alarm_env <- new.env(parent = emptyenv())
  
  
  alarm_env$get_next_alarm_id <- function(){
    alarm_env$id_alarma <- alarm_env$id_alarma + 1
    return(alarm_env$id_alarma)
  }
  
  alarm_env$reset_alarm_id <- function(valor = 0L){
    alarm_env$id_alarma <- valor
    return(alarm_env$id_alarma)
  }
  alarm_env$show_envs <- function(){
    print(parent.frame())
    environment()
  }
  
  # Read last id_alarma used from file
  if (file.exists('id_alarmas_file.txt')) {
    alarm_env$id_alarma <- as.numeric(readLines('id_alarmas_file.txt'))
  } else {
    alarm_env$id_alarma <- 0L
    cat(0, file="id_alarmas_file.txt", sep = "\n")
  }
  
  
  alarm_env$cant_alarmas <- numeric(6)
  
  alarm_env$init_cant_alarmas <- function() {
    alarm_env$cant_alarmas <- numeric(6)
  }
  
  alarm_env$contar_alarma <- function(importancia) {
    importancia <- trunc(importancia)
    alarm_env$cant_alarmas[importancia + 1] <- 
      alarm_env$cant_alarmas[importancia + 1] + 1
    aaa <- 1
  }
  
  alarm_env$ver_cuantas_corrieron <- function() {
    msg <- paste('Alarmas corrida:', sum(alarm_env$cant_alarmas), 
                 '[', paste(alarm_env$cant_alarmas, collapse = ', '), ']')
  }

  if (exists('datos_long')) {
    # This is the "correct" value for max_fecha_datos
    # Normally I wouldn't need to set it here, as cargar_datos() will set it after this...
    # But I sometimes re_source the code without re-running cargar_datos()...
    alarm_env$max_fecha_datos <- (datos_long %>% filter(source == 'pnl') %>%
                                     summarize(max(date)))[[1]]
  } else {
    # In this case, just a place holder. (¿Needed?)
    alarm_env$max_fecha_datos <- Sys.Date()
  }
  
  fecha_base <<- alarm_env$max_fecha_datos
  
  return(alarm_env)
}


alarm_env <- init_alarm_env()
