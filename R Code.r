#Dieser Code gibt genau den 8017er heraus, der um 18% abweicht von allen anderen abweicht (Tableau)
#Würden andere Daten abweichen, würden dieser Code diese auch finden
#DMG Mori Predictive Maintenance

df <- Testdaten_TGO
werkzeug_unique <- unique(df$Werkzeug)
werkzeug <- df$Werkzeug 
startzeit_unique <- unique(df$Startzeit)
startzeit <- df$Startzeit
aAveragesProWerkzeug <- array(dim=c(length(werkzeug_unique),length(colnames(df))))
aAverages <- array(dim=c(length(startzeit_unique), length(colnames(df))))

#Variable, um darin den durchschnittlichen Abstand zu speichern
aAverageDistances <- array(dim=c(length(werkzeug_unique),length(colnames(df))))


dfProzessschritt <- list()
dfERG <- as.data.frame

#Schleife über die 165 Prozessschritte
for (i in 1:length(startzeit_unique)){
  
  dfProzessschritt[[i]] <- as.data.frame(df[df$Startzeit==startzeit_unique[i] & df$Manueller.Trigger.Kanal1..0.I.==1, ] )
  
}

#Berechnung der Durchschnitte aller Spalten pro Prozessschritte (Werkzeuge) und Ablage in Zwei-dimensionales Array
i <- 0
j <- 0
colnames(aAverages) <- colnames(df)
for(i in 1:length(startzeit_unique)){
  
  for(j in 1:length(colnames(df))){
    
    aAverages[i,j] <- mean(dfProzessschritt[[i]][,colnames(df)[j]])
    
  }
  aAverages[i,2] <- startzeit_unique[i]
}

#Berechnung der Durchschnitte aller Spalten pro Werkzeuge (Werkzeuge zusammengefasst) und Ablage in Zwei-dimensionales Array
i <- 0
j <- 0
colnames(aAveragesProWerkzeug) <- colnames(df)
for(i in 1:length(werkzeug_unique)){
  for(j in 1:length(colnames(df))){
    
    aAveragesProWerkzeug[i,j] <- mean(df[df$Werkzeug==werkzeug_unique[i], ][,colnames(df)[j]])
    
  }
}

#Berechnung der Abstände zum Durchschnitt, um den durchschnittlichen Abstand zu ermitteln
#und so später Ausreißer erkennen zu können -- neue Methode
dfAverages <- as.data.frame(aAverages)
dfAveragesProWerkzeug <- as.data.frame(aAveragesProWerkzeug)






#hier werden alle Differenzen ermittelnt
i <- 0
j <- 0
k <- 0
aAbweichungen <- array(dim=c(length(dfAverages[,1]),length(4:12)))
colnames(aAbweichungen) <- c("Werkzeug", colnames(df)[4:11])


for(i in 1:length(dfAveragesProWerkzeug[,1])){
  for(j in 4:11){
    for(k in 1:length(dfAverages[,1])){
      if(dfAverages[k,30] == dfAveragesProWerkzeug[i,30]){
        
        #der Spalte Werkzeuge die richtigen Werkzeuge zuweisen...
        aAbweichungen[k,1] <- dfAveragesProWerkzeug[i,30]
        
        #hier die Differenzen für alle Spalten berechnen
        
        aAbweichungen[k,j-2] <- as.numeric(aAveragesProWerkzeug[i,j]) - as.numeric(aAverages[k,j])
        
      }
      
      
    }
  }
}
#hier werden die Abweichungen in Betrag umgewandelt
aAbweichungen <- abs(aAbweichungen)

#anzahl der Abweichungen jeweils __ nur Arraybildung
aAnzahlAbweichungen <- array(dim=c(length(dfAveragesProWerkzeug[,1]),2), 0)
colnames(aAnzahlAbweichungen) <- c("Werkzeug", "Anzahl der Additionen")


#nun müssen die Abweichungen zum Gesamtdurchschnitt aufsummiert werden
aSummierteAbweichungen <- array(dim=c(length(dfAveragesProWerkzeug[,1]),length(4:12)))
colnames(aSummierteAbweichungen) <- c("Werkzeug", colnames(df)[4:11])
dfAbweichungen <- as.data.frame(aAbweichungen)
i <- 0
j <- 0

#hier sind die summierten Abweichungen zum Gesamtdurchschnitt pro Werkzeugtyp und Messungstyp
for(i in 1:length(aSummierteAbweichungen[,1])){
  for(j in 2:9){
    
    
    aSummierteAbweichungen[i,j] <- sum(dfAbweichungen[dfAbweichungen$Werkzeug==werkzeug_unique[i], ][,j])
    aSummierteAbweichungen[i,1] <- werkzeug_unique[i]
  }
}

#hier sind die Anzahl der Abweichungen
#Das benötigen wir, damit wir mit der oben genannten summierten Abweichung einen
#Mittelwert der Abweichung bestimmen können (Mittelwert = Summe/Anzahl)
i <- 0
j <- 0
for(i in 1:length(aAnzahlAbweichungen[,1])){
  
  aAnzahlAbweichungen[i,1] <- werkzeug_unique[i]
  aAnzahlAbweichungen[i,2] <- length(dfAbweichungen[dfAbweichungen$Werkzeug==werkzeug_unique[i],][,1])
  
}



#Mittelwerte der Abweichungen
aDurchschnittlicheAbweichungen <- array(dim=c(length(dfAveragesProWerkzeug[,1]),length(4:12)))
colnames(aDurchschnittlicheAbweichungen) <- c("Werkzeug", colnames(df)[4:11])
i <- 0
j <- 0

for(j in 2:9){
  for(i in 1:length(aAnzahlAbweichungen[,1])){
    
    aDurchschnittlicheAbweichungen[i,1]  <- werkzeug_unique[i]
    aDurchschnittlicheAbweichungen[i,j] <- aSummierteAbweichungen[i,j]/aAnzahlAbweichungen[i,2]
    
    
  }
}


#nun müssen die durchschnittliche Abweichung pro Werkzeugstyp und Messungstyp mit jeweiligen Abweichungen verglichen werden
#d.h. prozentuale Abweichung
aProzentualeAbweichung <- array(dim=c(length(dfAverages[,1]),length(4:12)))
colnames(aProzentualeAbweichung) <- c("Werkzeug", colnames(df)[4:11])
i <- 0
j <- 0
k <- 0

for(i in 1:length(aDurchschnittlicheAbweichungen[,1])){
  for(k in 1:length(aProzentualeAbweichung[,1])){
    for(j in 2:length(aProzentualeAbweichung[1,])){
      
      
      if(aAbweichungen[k,1] == aDurchschnittlicheAbweichungen[i,1]){
        aProzentualeAbweichung[k,1] <- aAbweichungen[k,1]
        
        erg <- aAbweichungen[k,j]/aDurchschnittlicheAbweichungen[i,j]
        
        if(erg < -3 | erg > 3 ){
          aProzentualeAbweichung[k,j] <- erg
          aProzentualeAbweichung[k,1] <- aAbweichungen[k,1]
        }
        
      }
      
    }
    
  }
}



#ergebnis ist bei 152 in dem 8017-er (einziges Ergebnis)

cols <- c('startzeit', 'Werkzeug')
for(i in 4 :11){
  cols <- c(cols, colnames(df)[i])
}

dfERG <- as.data.frame(setNames(replicate(10, character(0)), cols),stringsAsFactors = FALSE)

i <- 0
j <- 0
x <- 0
for(i in 1:length(dfProzessschritt)){
  for(j in 2:9){
    cStartzeit <- as.character(dfAverages$Startzeit[i])
    
    if( is.na(aProzentualeAbweichung[i,j])==FALSE){
      x <- x + 1
      dfERG[x,'startzeit'] <- cStartzeit
      dfERG[x, 'Werkzeug'] <- as.vector(dfAverages$Werkzeug[i])
      dfERG[x, colnames(aProzentualeAbweichung)[j]] <- abs(aProzentualeAbweichung[i,j])
    }
  }
}

dfERG

#nur zum Testen:
#aAbweichungen[106,2]

#aDurchschnittlicheAbweichungen[4,2]

#aSummierteAbweichungen[8,9]

#aAnzahlAbweichungen[8,2]

#aDurchschnittlicheAbweichungen[8,9]

#aAbweichungen[163,9]