erste_hndr <-  rev(sort(table(nrw17$name)))[1:100] # Erste TOP 100
    zuf_usr <-   sample(erste_hndr ,1, replace=FALSE) # Waehlen wir zuefale
    anz_von_verws <- sum(grepl( paste0("@",names(zuf_usr),separate= ""), nrw17$inhalt[anz_von_at])) # Anzahl von Referenzen ,  die, auf zufaellig gewaehlte person, gemacht war.
    anz_von_verws
