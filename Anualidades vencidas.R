#Funciones para anualidad vencida
#Fernando Zuriel Martinez Barrera
#Matricula 2350468h


#Formula para calcular el valor futuro con formula de anualidad vencida

ValorFuturo=function(A,r,t){
  valor=A*((1+r)^t-1)/r
  texto=paste0("Valor futuro:",valor)
  return(texto)
}

#Formula para calcular la anualidad con formula de anualidad vencida

Anualidad=function(VF,r,t){
  valor=(VF*r)/((1+r)^t-1)
  texto=paste0("Anualidad:",valor)
  return(texto)
}

#Formula para calcular el numero de pagos con la formula de anualidad vencida

N_pagos=function(A,VF,r){
  valor=(log(((VF*r)/A)+1))/log(1+r)
  texto=paste0("Numero de pagos:",valor)
  return(texto)
}

#Formula para calcular la tasa del periodo con la formula de anualidad vencida


Tasa=function(VF,A,t){
  aprox=function(r){
    VF-A*((1+r)^t-1)/r
  }
  tasa_aprox=uniroot(aprox,inter = c(0.0000001, 1))$root
  texto=paste0("Valor aproximado de la tasa:",tasa_aprox)
  return(texto)
}

#Formula para calcular valor actual con la formula de anualidad vencida

Valor_actual=function(A,r,t){
  valor=A*(1-(1+r)^(-t))/r
  texto=paste0("Valor actual:",valor)
  return(texto)
}

#Formula para calcular anualidad conociendo valor actual...etc

Anual=function(VA,r,t){
  valor=(VA*r)/(1-(1+r)^-t)
  texto=paste0("Valor de la anualidad conociendo el valor actual:",valor)
  return(texto)
}

#Formula para calcular N.pagos conociendo valor actual...etc

Num_p=function(VA,A,r){
  valor=(-log(1-(VA*r)/A))/log(1+r)
  texto=paste0("Valor del numero de periodos conociendo el valor actual:",valor)
  return(texto)
}

#Formula para calcular la tasa del periodo con valor actual

Tasa_v=function(VA,A,t){
  aproximado=function(r){
    VA-A*((1-(1+r)^(-t))/r)
  }
  t_aprox=uniroot(aproximado,inter = c(0.0000001, 1))$root
  texto=paste0("Valor aproximado de la tasa:",t_aprox)
  return(texto)
}











