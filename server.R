# Biblioteki
library(shiny)
library(ggplot2)
library(plotly)
library(data.table)
library(dplyr)
library(officer)



shinyServer(function(input, output, session) {
  
  # Tabela do przetrzymywania danych
  data_values <- reactiveValues(daneFinal = NULL, daneKlasy = NULL)
  
  # Ładowanie danych
  observeEvent(input$fileInPath, {
    
    try({
      
      # Dodatkowe kolumny
      dane <- readxl::read_xlsx(path = input$fileInPath$datapath, col_names = TRUE)
      dane[is.na(dane)] <- 0
      dane$razem <- rowSums(dane[, 3:37])
      dane$srednia <- round(dane$razem / 35, 4)
      dane$razemZamkniete <- rowSums(dane[, 3:30])
      dane$sredniaZamkniete <- round(dane$razemZamkniete / 35, 4)
      dane$razemOtwarte <- rowSums(dane[, 31:37])
      dane$sredniaOtwarte <- round(dane$razemOtwarte / 35, 4)
      
      # Update
      data_values$daneFinal <- dane
    })
  })
  
  # Update select input
  observeEvent(data_values$daneFinal, {
    req(data_values$daneFinal)
    
    daneKlasy <- data.frame(data_values$daneFinal)
    daneKlasy <- daneKlasy[tolower(daneKlasy$uczeń) != 'max', ]
    daneKlasy <- daneKlasy[tolower(daneKlasy$uczeń) != 'razem', ]
    daneKlasy <- unique(daneKlasy$klasa)
    
    updateSelectInput(session, "wyborKlasa", choices = daneKlasy, selected = NULL)
  })
  
  # Filtrowanie danych
  daneFiltr <- reactive({
    req(data_values$daneFinal, input$wyborKlasa)
    
    daneFinal <- data_values$daneFinal
    daneKlasy <- input$wyborKlasa
    
    daneFinal %>% filter(klasa %in% daneKlasy)
  })
  
  # Tabela z wynikami
  output$tabela <- renderTable({
    daneTabela <- data.frame(daneFiltr())
    daneTabela <- daneTabela  %>% select('uczeń', 'klasa', 'razem', 
                                         'srednia', 'razemZamkniete',
                                         'sredniaZamkniete',
                                         'razemOtwarte',
                                         'sredniaOtwarte')
    daneTabela <- daneTabela[tolower(daneTabela$uczeń) != 'max', ]
    colnames(daneTabela) <- c('Uczeń', 'Klasa', 'Suma punktów', 
                              'Średnia wszystkich zadań', 
                              'Suma punktów z zadań zamkniętych',
                              'Średnia z zadań zamkniętych', 
                              'Suma punktów z zadań otwartych',
                              'Średnia z zadań otwartych')
    
    # Zaokrąglanie
    numeric_cols <- c('Suma punktów', 'Średnia wszystkich zadań',
                      'Suma punktów z zadań zamkniętych', 'Średnia z zadań zamkniętych',
                      'Suma punktów z zadań otwartych', 'Średnia z zadań otwartych')
    
    for (col in numeric_cols) {
      daneTabela[[col]] <- format(daneTabela[[col]], nsmall = 0)
    }
    
    return(daneTabela)
    
  }, include.rownames = FALSE)
  
  
  # Średnie
  
  output$srednie <- renderPlotly({
    
    dane <- data.frame(daneFiltr())
    
    #Tabela średnich dla klas
    sredniaSzkola <- c('Szkoła',round(mean(dane$razem),2),round(mean(dane$razemZamkniete),2), 
                       paste0(round(mean(dane$razemZamkniete)/50*100,2),'%'),
                       round(mean(dane$razemOtwarte),2), round(max(dane$razem),2), round(min(dane$razem),2))
    
    srednie <- cbind(dane %>%
                       group_by(klasa) %>%
                       summarise(srednia = round(mean(razem),2)),
                     dane %>%
                       group_by(klasa) %>%
                       summarise(sredniaZamkniete = round(mean(razemZamkniete),2)),
                     dane %>%
                       group_by(klasa) %>%
                       summarise(sredniaOtwarte = round(mean(razemOtwarte),2)),
                     dane %>%
                       group_by(klasa) %>%
                       summarise(maxKlasa = round(max(razem),2)),
                     dane %>%
                       group_by(klasa) %>%
                       summarise(minKlasa = round(min(razem),2)),
                     dane %>%
                       group_by(klasa) %>%
                       summarise(sredniaProc = paste0(round(mean(razem)/50*100,2),'%'))
    )
    
    srednie <- cbind(srednie$klasa, srednie$srednia, srednie$sredniaProc, srednie$sredniaZamkniete, srednie$sredniaOtwarte,
                     srednie$maxKlasa, srednie$minKlasa)
    
    colnames(srednie) <- c('Klasa', 'Średnia', 'Średnia Procentowa', 'Średnia Zamknięte',
                           'Średnia Otwarte', 'Wynik Maksymalny', 'Wynik Minimalny')
    
    srednie <- rbind(srednie, sredniaSzkola)
    
    srednie <- data.frame(srednie)
    
    #wykres
    srednieWykres <- ggplot(srednie, aes(x = Klasa, y = Średnia, fill = Klasa)) +
      geom_col() +
      labs(x = "Klasa", y = "Średnia", fill = "Klasa")
  })
  
  
  # Zdawalność
  
  output$zdawalnosc <- renderTable({
    
    dane <- data.frame(daneFiltr())
    
    #zdawalność
    daneZdane <- dane[dane$razem>=14,]
    
    zdawalnosc <- cbind(daneZdane %>%
                          group_by(klasa) %>%
                          count(),
                        dane %>%
                          group_by(klasa) %>%
                          count()
    )
    
    zdawalnosc <- cbind(zdawalnosc[1], zdawalnosc[4], zdawalnosc[2])
    
    zdawalnosc[4] <- round(zdawalnosc[3]/zdawalnosc[2] * 100, 2)
    
    zdawalnosc <- rbind(zdawalnosc, c('Szkoła', 
                                      colSums(zdawalnosc[,2:3]), 
                                      round(colSums(zdawalnosc[3])/colSums(zdawalnosc[2]) * 100,2))
    )
    
    colnames(zdawalnosc) <- c('Klasa', 'Liczba uczniów', 'Liczba zdających uczniów', 'Procent zdających uczniów')
    
    return(zdawalnosc)
    
  })
  
  # Zadania
  
  output$zadania <- renderPlotly({
    
    dane <- data.frame(daneFiltr())
    
    #wykres zadań
    srednieZadania <- data.frame(factor(colnames(dane[,3:37])), round(colMeans(dane[,3:37]),4))
    
    colnames(srednieZadania) <- c('Zadanie', 'Średnia')
    zadanie_order <- colnames(dane[,3:37])
    srednieZadania$Zadanie <- factor(srednieZadania$Zadanie, levels = zadanie_order)
    
    srednieZadaniaWykres <- ggplot(srednieZadania, aes(x = Zadanie, y = Średnia, fill = Zadanie)) +
      geom_col() +
      geom_text(aes(label = round(Średnia, 2)), size = 2, nudge_y = 0.02) +
      geom_hline(yintercept = round(mean(srednieZadania$Średnia), 2))
    
    return(ggplotly(srednieZadaniaWykres))
    
  })
  
  
  # Łatwość zadania
  
  output$latwosc <- renderPlotly({
    
    dane <- data.frame(daneFiltr())
    
    zadanie_order <- colnames(dane[,3:37])
    
    #łatwość zadań
    latwoscZadania <- colSums(dane[,3:30]) / nrow(dane)
    latwoscZadania <- c(latwoscZadania, colSums(dane[,31:36]) / nrow(dane)*2)
    latwoscZadania <- c(latwoscZadania, colSums(dane[,33, drop = FALSE]) / nrow(dane)*5)
    latwoscZadania <- data.frame(zadanie_order,latwoscZadania)
    colnames(latwoscZadania) <- c('Zadanie', 'Łatwość')
    latwoscZadania$Zadanie <- factor(latwoscZadania$Zadanie, levels = zadanie_order)
    
    latwoscZadaniaWykres <- (ggplot(latwoscZadania, aes(x = Zadanie, y = Łatwość, fill = Zadanie))
                             + geom_col()
                             + geom_text(aes(label = round(Łatwość, 2)), size = 2, nudge_y=0.02)
    )
    
    return(ggplotly(latwoscZadaniaWykres))
    
  })
  
  
  # Tabela z surowymi danymi
  
  output$tabelaSurowe <- renderTable({
    daneTabela <- data.frame(daneFiltr())
    
    return(daneTabela)
    
  }, include.rownames = FALSE)
  
  
  # Przycisk
  
  output$downloadPresentation <- downloadHandler(filename = function() {
    "Analiza_matur.pptx"
  },
  content = function(file) {
    
    dane <- data.frame(daneFiltr())
    
    klasy <- unique(dane$klasa)
    
    #Tabela średnich dla klas
    sredniaSzkola <- c('Szkoła',round(mean(dane$razem),2),round(mean(dane$razemZamkniete),2), 
                       paste0(round(mean(dane$razemZamkniete)/50*100,2),'%'),
                       round(mean(dane$razemOtwarte),2), round(max(dane$razem),2), round(min(dane$razem),2))
    
    srednie <- cbind(dane %>%
                       group_by(klasa) %>%
                       summarise(srednia = round(mean(razem),2)),
                     dane %>%
                       group_by(klasa) %>%
                       summarise(sredniaZamkniete = round(mean(razemZamkniete),2)),
                     dane %>%
                       group_by(klasa) %>%
                       summarise(sredniaOtwarte = round(mean(razemOtwarte),2)),
                     dane %>%
                       group_by(klasa) %>%
                       summarise(maxKlasa = round(max(razem),2)),
                     dane %>%
                       group_by(klasa) %>%
                       summarise(minKlasa = round(min(razem),2)),
                     dane %>%
                       group_by(klasa) %>%
                       summarise(sredniaProc = paste0(round(mean(razem)/50*100,2),'%'))
    )
    
    srednie <- cbind(srednie$klasa, srednie$srednia, srednie$sredniaProc, srednie$sredniaZamkniete, srednie$sredniaOtwarte,
                     srednie$maxKlasa, srednie$minKlasa)
    
    colnames(srednie) <- c('Klasa', 'Średnia', 'Średnia Procentowa', 'Średnia Zamknięte',
                           'Średnia Otwarte', 'Wynik Maksymalny', 'Wynik Minimalny')
    
    srednie <- rbind(srednie, sredniaSzkola)
    
    srednie <- data.frame(srednie)
    
    #zdawalność
    
    daneZdane <- dane[dane$razem>=14,]
    
    zdawalnosc <- cbind(daneZdane %>%
                          group_by(klasa) %>%
                          count(),
                        dane %>%
                          group_by(klasa) %>%
                          count()
    )
    
    zdawalnosc <- cbind(zdawalnosc[1], zdawalnosc[4], zdawalnosc[2])
    
    zdawalnosc[4] <- round(zdawalnosc[3]/zdawalnosc[2] * 100, 2)
    
    zdawalnosc <- rbind(zdawalnosc, c('Szkoła', 
                                      colSums(zdawalnosc[,2:3]), 
                                      round(colSums(zdawalnosc[3])/colSums(zdawalnosc[2]) * 100,2))
    )
    
    colnames(zdawalnosc) <- cbind('Klasa', 'Liczba uczniów', 'Liczba zdających uczniów', 'Procent zdających uczniów')
    
    zdawalnosc <- data.frame(zdawalnosc)
    
    #wykres
    srednieWykres <- ggplot(srednie, aes(x = Klasa, y = Średnia, fill = Klasa)) +
      geom_col() +
      labs(x = "Klasa", y = "Średnia", fill = "Klasa")
    
    #print(srednieWykres)
    
    #wykres zadań
    srednieZadania <- data.frame(factor(colnames(dane[,3:37])), round(colMeans(dane[,3:37]),4))
    # srednieZadaniaT <- data.frame(t(srednieZadania[2]))
    
    colnames(srednieZadania) <- c('Zadanie', 'Średnia')
    zadanie_order <- colnames(dane[,3:37])
    srednieZadania$Zadanie <- factor(srednieZadania$Zadanie, levels = zadanie_order)
    
    srednieZadaniaWykres <- ggplot(srednieZadania, aes(x = Zadanie, y = Średnia, fill = Zadanie)) +
      geom_col() +
      geom_text(aes(label = round(Średnia, 2)), size = 2, nudge_y=0.02) +
      geom_hline(yintercept = round(mean(srednieZadania$Średnia),2))
    
    
    #łatwość zadań
    latwoscZadania <- colSums(dane[,3:30]) / nrow(dane)
    latwoscZadania <- c(latwoscZadania, colSums(dane[,31:36]) / nrow(dane)*2)
    latwoscZadania <- c(latwoscZadania, colSums(dane[,33, drop = FALSE]) / nrow(dane)*5)
    latwoscZadania <- data.frame(zadanie_order,latwoscZadania)
    colnames(latwoscZadania) <- c('Zadanie', 'Łatwość')
    latwoscZadania$Zadanie <- factor(latwoscZadania$Zadanie, levels = zadanie_order)
    
    latwoscZadaniaWykres <- (ggplot(latwoscZadania, aes(x = Zadanie, y = Łatwość, fill = Zadanie))
                             + geom_col()
                             + geom_text(aes(label = round(Łatwość, 2)), size = 2, nudge_y=0.02)
    )
    
    
    # inicjalizacja dokumentu
    my_pres <- read_pptx()
    
    #Slajd Tytułowy
    my_pres <- add_slide( my_pres ,layout = "Title and Content", master = "Office Theme")
    my_pres <- ph_with(my_pres, value = paste("Analiza wyników próbnego egzaminu maturalnego z matematyki", format(Sys.Date(), "%Y-%m-%d")),
                       location = ph_location_type(type = "title"))
    
    #Slajd ze zdawalnością
    my_pres <- add_slide(my_pres,layout = "Two Content", master = "Office Theme")
    my_pres <- ph_with(my_pres, value = "Zdawalność",      location = ph_location_type(type = "title"))
    my_pres <- ph_with(my_pres, value = zdawalnosc, location=ph_location_type(type="body") )
    
    #Slajd ze średnimi klas
    my_pres <- add_slide(my_pres,layout = "Two Content", master = "Office Theme")
    my_pres <- ph_with(my_pres, value = "Średnie wyniki klas i szkoły",      location = ph_location_type(type = "title"))
    my_pres <- ph_with(my_pres, value = srednie, location=ph_location_type(type="body") )
    
    #Slajd ze średnimi klas wykres
    my_pres <- add_slide(my_pres,layout = "Two Content", master = "Office Theme")
    my_pres <- ph_with(my_pres, value = "Średnie wyniki klas i szkoły",      location = ph_location_type(type = "title"))
    my_pres <- ph_with(my_pres, value = srednieWykres, location=ph_location_type(type="body") )
    
    #Slajd z średnimi zadań, tabela
    # my_pres <- add_slide(my_pres,layout = "Title and Content", master = "Office Theme")
    # my_pres <- ph_with(my_pres, value = "Średnie wyniki zadań",      location = ph_location_type(type = "title"))
    # my_pres <- ph_with( my_pres, value = srednieZadaniaT,  location=ph_location_type(type="body") )
    
    #Slajd z średnimi zadań, wykres
    my_pres <- add_slide(my_pres,layout = "Title and Content", master = "Office Theme")
    my_pres <- ph_with(my_pres, value = "Średnie wyniki zadań",      location = ph_location_type(type = "title"))
    my_pres <- ph_with( my_pres, value = srednieZadaniaWykres,  location=ph_location_type(type="body") )
    
    #Slajd z łatwością zadań, wykres
    my_pres <- add_slide(my_pres,layout = "Title and Content", master = "Office Theme")
    my_pres <- ph_with(my_pres, value = "Łatwość zadań",      location = ph_location_type(type = "title"))
    my_pres <- ph_with( my_pres, value = latwoscZadaniaWykres,  location=ph_location_type(type="body") )
    
    #Slajd z łatwością zadań, tabela
    # my_pres <- add_slide(my_pres,layout = "Title and Content", master = "Office Theme")
    # my_pres <- ph_with(my_pres, value = "Łatwość zadań",      location = ph_location_type(type = "title"))
    # my_pres <- ph_with( my_pres, value = latwoscZadania,  location=ph_location_type(type="body") )
    
    for (i in klasy){
      
      dane_temp <- dane[dane$klasa==i,]
      
      srednieZadaniaTemp <- data.frame(factor(colnames(dane_temp[,3:37])), round(colMeans(dane_temp[,3:37]),4))
      srednieZadaniaTTemp <- data.frame(t(srednieZadania[2]))
      
      colnames(srednieZadaniaTemp) <- c('Zadanie', 'Średnia')
      srednieZadaniaTemp$Zadanie <- factor(srednieZadaniaTemp$Zadanie, levels = zadanie_order)
      
      srednieZadaniaWykresTemp <- (ggplot(srednieZadaniaTemp, aes(x = Zadanie, y = Średnia, fill = Zadanie))
                                   + geom_col()
                                   + geom_text(aes(label = round(Średnia, 2)), size = 2, nudge_y=0.02)
                                   + geom_hline(yintercept = round(mean(srednieZadaniaTemp$Średnia),2))
      )
      
      latwoscZadaniaTemp <- colSums(dane_temp[,3:30]) / nrow(dane_temp)
      latwoscZadaniaTemp <- c(latwoscZadaniaTemp, colSums(dane_temp[,31:36]) / nrow(dane_temp)*2)
      latwoscZadaniaTemp <- c(latwoscZadaniaTemp, colSums(dane_temp[,33, drop = FALSE]) / nrow(dane_temp)*5)
      latwoscZadaniaTemp <- data.frame(zadanie_order,latwoscZadaniaTemp)
      colnames(latwoscZadaniaTemp) <- c('Zadanie', 'Łatwość')
      latwoscZadaniaTemp$Zadanie <- factor(latwoscZadaniaTemp$Zadanie, levels = zadanie_order)
      
      latwoscZadaniaWykresTemp <- (ggplot(latwoscZadaniaTemp, aes(x = Zadanie, y = Łatwość, fill = Zadanie))
                                   + geom_col()
                                   + geom_text(aes(label = round(Łatwość, 2)), size = 2, nudge_y=0.02)
      )
      
      #dodawanie slajdów
      # my_pres <- add_slide(my_pres,layout = "Title and Content", master = "Office Theme")
      # my_pres <- ph_with(my_pres, value = paste0(c("Średnie wyniki zadań", i)),      location = ph_location_type(type = "title"))
      # my_pres <- ph_with( my_pres, value = srednieZadaniaTTemp,  location=ph_location_type(type="body") )
      
      my_pres <- add_slide(my_pres,layout = "Title and Content", master = "Office Theme")
      my_pres <- ph_with(my_pres, value = paste0(c("Średnie wyniki zadań", i)),      location = ph_location_type(type = "title"))
      my_pres <- ph_with( my_pres, value = srednieZadaniaWykresTemp,  location=ph_location_type(type="body") )
      
      my_pres <- add_slide(my_pres,layout = "Title and Content", master = "Office Theme")
      my_pres <- ph_with(my_pres, value = paste0(c("Łatwość zadań", i)),      location = ph_location_type(type = "title"))
      my_pres <- ph_with( my_pres, value = latwoscZadaniaWykresTemp,  location=ph_location_type(type="body") )
    }
    
    ### Wydruk dokumentu
    print(my_pres, target = file)    
  })
  
})
