# Strategy
# 
# parsear_expr
#   if atomic
#     eval(expr)
#   else if name


oper_comparacion <- c('>=', '>', '==', '!=', '<=', '<')
# str_detect('=!', '>[=]?|==|!=|<[=]?')

c_a2 <- function(alarma){
  # browser()
  expr <- expr_find(alarma)
  fmla <- as.formula(paste("~ ", paste(expr_text(alarma))))
  browser()
  
  if (exists('expr') && (class(expr) == 'call') && (length(expr) > 1) && 
      as.character(expr[[1]]) %in% oper_comparacion ) {
    val <- arbol(fmla)
  } else {
    stop('Alarma inválida')
  }
  return(val)
}



arbol <- function(x, level = 1) {
  if (is.atomic(x) && length(x) == 1){
    label <- deparse(x)
    val <- eval(x)
  } else if (is.name(x)) {
    label <- as.character(x)
    val <- eval(x)
  } else if (is.call(x)) 
    if (level %in% c(2, 3)){
      browser()
    }
  return(val)
}

# debugonce(c_a2)
c_a2(abs(b - 3) + (a > 0) - (a - 1) == (b-1))

a <- 1
b <- 2
