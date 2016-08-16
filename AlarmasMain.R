### AlarmasMain v0.2

### 1) Load and prepare data: ---------------------
# This step should only be run when changes to production files
# are believed to have ocurred (probably once a day, maybe twice), or
# whenever R is started

# Parametros generales
path <- "..\\data2\\"
archivo_salida <- "salida_Antoine.csv"
usarYY <- TRUE

tipo_output <- 'screen'   # Es la actual, tira las 5 o 6 líneas a pantalla
# tipo_output <- 'csv'      # Proximamente, te aviso cuando lo puedas usar
# tipo_output <- 'bdd'      # Proximamente

if (exists('usarYY')) {
  source('BorradorAlarmasFunc_Precios.R', encoding = 'UTF-8')
} else {
  source('BorradorAlarmasFunciones.R', encoding = 'UTF-8')
}

familias_importantes <- c('VIX', 'MACROMODEL', 'Alfa Total', 'TOTAL T+ L','DONCHIAN',
                          'BREAKOUTS', 'BREAKOUTS+DONCHIAN', 'FUTUROS ADMINISTRADOS', 
                          'VIX + STRANGLES', 'PRIMA MERCADO DESARROLLADOS','VIX','STRANGLES','STRANGLES EN VIX')
DATOS <- carga_datos(familias_importantes)


### 2) Alarm checking: ---------------------

# filename must be either a .yaml alarm spec or a .R file with explicit alarms
# Run as often as needed, maybe changing file for different purposes 
# corrida_alarmas(filename = ".//Alarmas para correr//Alarmas Giselle.R", fecha_final = '27/6/2016')
corrida_alarmas(filename = ".//Alarmas para correr//Prueba cambia fecha_base.R", 
                fecha_inicial = '16/2/2016', fecha_final = '1/1/2016')
system.time({
  corrida_alarmas(filename = "Alarmas_Version1.R", '2/8/16', '2/5/16')
}
)
corrida_alarmas(filename = "Prueba_Alarmas_data_BBG.R", '2/8/16', '2/5/16')
corrida_alarmas(filename = "Alarma_precios_pincha.R", '19/1/16', '18/1/16')
corrida_alarmas(filename = "Prueba_Alarmas_data_BBG.R")

corrida_alarmas(filename = "Alarmas_Version1.R", '28/7/16')
corrida_alarmas(filename = "Alarmas_Version1browser.R", '28/7/16')

