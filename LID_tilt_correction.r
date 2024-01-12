# Creazione di una nuova colonna all'interno del nostro dataset "FLY_" con i valori del tilt sottratti di 5 ovvero il suo zero e poi corretti i valori negativi in positivi
df$nuova_colonna <- df$colonna_da_modificare - 5
df$nuova_colonna[df$nuova_colonna < 0] <- -df$nuova_colonna[df$nuova_colonna < 0]
