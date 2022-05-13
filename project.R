library(ggplot2)
library(shiny)
library(DT)
library(DBI)
library(googleVis)
library(RSQLite)
library(plotly)

ui <- fluidPage(
  
  titlePanel("Umieralnosc na podstawie GUS/EUROSTAT"),
  
  mainPanel(
    
    tabsetPanel(type = "tabs",
                
                tabPanel("Pobieranie danych",
                         HTML(paste0("<h3>Pobierz dane ze strony GUS a nastepnie zapisz je do katalogu data/:<h3>")),
                         actionButton(inputId = "open_page_GUS", label = "Otworz strone GUS w przegladarce"),
                         
                         
                         HTML(paste0("<h3>Pobierz dane ze strony EUROSTAT a nastepnie zapisz je do katalogu data/:<h3>")),
                         actionButton(inputId = "open_page_EUROSTAT", label = "Otworz strone EUROSTAT w przegladarce"),
                         HTML(paste0("<h3>Utworz baze danych EUROSTAT i GUS:<h3>")),
                         actionButton(inputId = "create_db", label = "Utworz baze danych"),
                         textOutput("selected_var")
                         
                ),
                
                tabPanel("SQL",
                         textInput("sqlQuery",
                                   label = "Wpisz zapytanie SQL:",
                                   value = "select time, geo, value from zgony_eurostat where time like '2000%'"
                         ),
                         actionButton(inputId = "execute_SQL", label = "Wczytaj"),
                         DT::dataTableOutput("table1"),
                         downloadButton("pobierz_dane", label = "Pobierz wyniki zapytania SQL"),
                         plotlyOutput("plot")
                ),
                
                tabPanel("Mapa GUS",
                         column(6, HTML(paste0("<p>Wizualizacja bezposrednia:<p>")),
                                htmlOutput("pl_bzp"),
                                textInput("zakres_dat1",
                                          label = "Wpisz szukany zakres dat (YYYY.MM.DD-YYYY.MM.DD):",
                                          value = "2020.01.01-2021.12.01"
                                ),
                                actionButton(inputId = "wczytaj_daty1", label = "Wczytaj")),
                         
                         column(6, HTML(paste0("<p>Wizualizacja wzgledna:<p>")),
                                htmlOutput("pl_wzg"),
                                textInput("zakres_dat2",
                                          label = "Wpisz szukany zakres dat (YYYY.MM.DD-YYYY.MM.DD):",
                                          value = "2015.01.01-2019.12.31"
                                ),
                                actionButton(inputId = "wczytaj_daty2", label = "Wczytaj"))
                ),
                tabPanel("Mapa EU",
                         column(6, HTML(paste0("<p>Wizualizacja bezposrednia:<p>")),
                                htmlOutput("eu_bzp"),
                                textInput("zakres_dat3",
                                          label = "Wpisz szukany zakres dat (YYYY.MM.DD-YYYY.MM.DD):",
                                          value = "2020.01.01-2021.12.01"
                                ),
                                actionButton(inputId = "wczytaj_daty3", label = "Wczytaj")),
                         
                         column(6, HTML(paste0("<p>Wizualizacja wzgledna:<p>")),
                                htmlOutput("eu_wzg"),
                                textInput("zakres_dat4",
                                          label = "Wpisz szukany zakres dat (YYYY.MM.DD-YYYY.MM.DD):",
                                          value = "2015.01.01-2019.12.31"
                                ),
                                actionButton(inputId = "wczytaj_daty4", label = "Wczytaj"))
                ),
                tabPanel("Szeregi czasowe GUS",
                         column(6, HTML(paste0("<p>Wizualizacja bezposrednia:<p>")),
                                plotlyOutput("pl_bzp1",width = "650px", height = "500px"),
                                textInput("zakres_dat5",
                                          label = "Wpisz szukany zakres dat (YYYY.MM.DD-YYYY.MM.DD):",
                                          value = "2020.01.01-2021.12.01"
                                ),
                                actionButton(inputId = "wczytaj_daty5", label = "Wczytaj")),
                         
                         column(6, HTML(paste0("<p>Wizualizacja wzgledna:<p>")),
                                plotlyOutput("pl_wzg1",width = "650px", height = "500px"),
                                textInput("zakres_dat6",
                                          label = "Wpisz szukany zakres dat (YYYY.MM.DD-YYYY.MM.DD):",
                                          value = "2015.01.01-2019.12.31"
                                ),
                                actionButton(inputId = "wczytaj_daty6", label = "Wczytaj"))
                ),
                tabPanel("Szeregi czasowe UE",
                         column(6, HTML(paste0("<p>Wizualizacja bezposrednia:<p>")),
                                plotlyOutput("eu_bzp1",width = "650px", height = "500px"),
                                textInput("zakres_dat7",
                                          label = "Wpisz szukany zakres dat (YYYY.MM.DD-YYYY.MM.DD):",
                                          value = "2000.01.01-2021.12.31"
                                ),
                                actionButton(inputId = "wczytaj_daty7", label = "Wczytaj")),
                         
                         column(6, HTML(paste0("<p>Wizualizacja wzgledna:<p>")),
                                plotlyOutput("eu_wzg1", width = "800px", height = "500px"),
                                textInput("zakres_dat8",
                                          label = "Wpisz szukany zakres dat (YYYY.MM.DD-YYYY.MM.DD):",
                                          value = "2000.01.01-2021.12.31"
                                ),
                                actionButton(inputId = "wczytaj_daty8", label = "Wczytaj"))
                )
                
    )
  )
  
)


server <- function(input, output) {
  
  utworzenie_bd <- reactive({
    dataDir <- file.path(getwd(),"data")
    
    if (!file.exists(dataDir)){dir.create(dataDir,mode="0777")}
    
    res <- try({
      
      unzip(file.path(dataDir,"zgony_wg_tygodni.zip"),exdir=file.path(dataDir),setTimes=T)
      
      hd <- getwd()
      setwd(file.path(dataDir,"zgony_wg_tygodni"))
      try({
        lapply(dir(),function(f){
          file.rename(
            from=f,
            to = gsub(" ","_",gsub("\x88","l",f))
          )
        })
      })
      setwd(hd)
      
    })
    
    library("readxl")
    library("data.table")
    library("reshape")
    
    czytajDaneLiczboweZZakladki <- function(f,sheet,plec){
      
      d <- as.data.frame(read_excel(f,sheet=sheet))
      colnames(d)[1:3] <- c("Grupa_wiekowa","Region_id","Region")
      d <- d[-c(1:(grep("^Og",d$Grupa_wiekowa)[1]-1)),]
      
      tygodnie <- 1:(ncol(d)-3)
      tygodnie[nchar(tygodnie)<2] <- paste0("0",tygodnie[nchar(tygodnie)<2])
      colnames(d)[4:ncol(d)] <- tygodnie
      
      d <- reshape::melt(d,id.vars=c("Grupa_wiekowa","Region_id","Region"))
      colnames(d) <- c("Grupa_wiekowa","Region_id","Region","Tydzien","Liczba")
      d$Grupa_wiekowa[grep("Og",d$Grupa_wiekowa)] <- "0 - Inf"
      d$Grupa_wiekowa[grep("wi",d$Grupa_wiekowa)] <- "90 - Inf"
      d <- cbind("Plec"=plec,d)
      
      return(d)
      
    }
    
    hd <- getwd()
    setwd(file.path(dataDir,"zgony_wg_tygodni"))
    
    try({
      mainRet <- do.call("rbind",lapply(dir(),function(f){
        print(f)
        
        ogolem <- czytajDaneLiczboweZZakladki(f,1,"Ogolem")
        mezczyzni <- czytajDaneLiczboweZZakladki(f,2,"Mezczyzni")
        kobiety <- czytajDaneLiczboweZZakladki(f,3,"Kobiety")
        
        dane <- rbind(ogolem,mezczyzni,kobiety)
        
        tygodnie <- as.data.frame(read_excel(f,sheet=grep("tyg",tolower(excel_sheets(f)))))
        tygodnie <- do.call("rbind",lapply(split(tygodnie,tygodnie[,2]),function(x){
          return(data.frame(Tydzien=unique(x[,2]),Od=min(x[,1]),Do=max(x[,1])))
        }))
        tygodnie$Tydzien <- gsub("T|W","",unlist(lapply(strsplit(tygodnie$Tydzien,"-"),function(x){x[2]})))
        rownames(tygodnie) <- NULL
        
        dane <- merge(x=dane,y=tygodnie,by="Tydzien",all=T)
        dane <- dane[,-which(colnames(dane)=="Tydzien")]
        
        dane <- dane[c("Od","Do","Plec","Grupa_wiekowa","Region_id","Region","Liczba")]
        dane$Liczba <- as.integer(dane$Liczba)
        
        dane$Grupa_wiekowa[dane$Grupa_wiekowa=="0 - 4"] <- "00 - 04"
        dane$Grupa_wiekowa[dane$Grupa_wiekowa=="5 - 9"] <- "05 - 09"
        
        return(dane)
      }))
      
      write.table(mainRet,file="../GUS_dane_przetworzone_pelne.csv",sep=";",dec=",",row.names=F)
    })
    
    setwd(hd)
    
    d <- read.table(file="data/demo_r_mwk_ts_1_Data.csv",sep=",",dec=",",header=T,stringsAsFactors=F)
    
    d$Value <- as.integer(gsub(",|:","",d$Value))
    
    try({
      write.table(d,file="data/eurostat_dane_przetworzone.csv",sep=",",dec=",",row.names=F)
    })
    
    dbName <- file.path(dataDir,"zgony_gus_eurostat.db")
    
    con <- dbConnect(
      dbDriver("SQLite"),
      dbname = dbName
    )
    try({
      
      d <- read.table(file=file.path(dataDir,"GUS_dane_przetworzone_pelne.csv"),sep=";",dec=",",header=T)

      d$Region[which(d$Region=="Makroregion Województwo Mazowieckie")] <- "Mazowieckie"
      d$Region[which(d$Region=="Lódzkie")] <- "Lodzkie"
      # d[6][d[6] == "Makroregion Województwo Mazowieckie"] <- "Mazowieckie"
      # d[6][d[6] == "Lódzkie"] <- "Lodzkie"
      dbWriteTable(con, "zgony_gus", d, overwrite = TRUE, row.names = FALSE)
    })
    try({
      
      d <- read.table(file=file.path(dataDir,"eurostat_dane_przetworzone.csv"),sep=",",dec=",",header=T)
      dbWriteTable(con, "zgony_eurostat", d, overwrite = TRUE, row.names = FALSE)
    })
    
    dbDisconnect(con)
  })
  
  observeEvent(input$open_page_GUS,{
    browseURL("https://stat.gov.pl/obszary-tematyczne/ludnosc/ludnosc/")
  })  
  
  observeEvent(input$open_page_EUROSTAT,{
    browseURL("https://ec.europa.eu/eurostat/web/population-demography/demography-population-stock-balance/database?node_code=demomwk")
  })  
  
  observeEvent(input$create_db, {
    utworzenie_bd()
    output$selected_var <- renderText({
      "Utworzenie bazy danych powiodlo sie!"
    })
  })
  
  observeEvent(input$execute_SQL, {
    
    zapytanie_SQL <- eventReactive(input$sqlQuery,{
      
      conn <- dbConnect(RSQLite::SQLite(), "data/zgony_gus_eurostat.db")
      
      zapytanie <- dbGetQuery(
        conn,
        statement = input$sqlQuery)
      
      return(zapytanie)
    })
    
    output$table1 <- renderDataTable({
      baza <- zapytanie_SQL()
      baza
    },
    options = list(pageLength = 10, info = FALSE,
                   lengthMenu = list(c(seq(10,100,10)), c(seq(10,100,10))) ))
    
    output$pobierz_dane <- downloadHandler(
      
      filename = function() { 
        return(paste0(gsub("-","_",as.character(Sys.Date())),"_out.csv")) 
      },
      content = function(file) {
        write.table(
          zapytanie_SQL(),
          file,
          sep=",",dec=".", row.names=F, col.names=T)
      }
    )
    
    baza3 <- zapytanie_SQL()
    if (!is.null(baza3$TIME) & !is.null(baza3$Value) & ncol(baza3)==3) {
      output$plot <- renderPlotly({
        colnames(baza3)
        Czas <- baza3$TIME
        if (is.null(baza3$SEX)) {
          Zmienna <- baza3$GEO
        } else {
          Zmienna <- baza3$SEX
        }
        
        Zgony <- baza3$Value
        ggplotly(
          ggplot(baza3,aes(x=Czas,y=Zgony,col=Zmienna)) + geom_line())
      })
    } 
    if (!is.null(baza3$Od) & !is.null(baza3$Liczba) & ncol(baza3)==3) {
      output$plot <- renderPlotly({
        colnames(baza3)
        Czas <- baza3$Od
        if (is.null(baza3$Plec) & is.null(baza3$Grupa_wiekowa)) {
          Zmienna <- baza3$Region
        } else if (is.null(baza3$Grupa_wiekowa) & is.null(baza3$Region)) {
          Zmienna <- baza3$Plec
        } else {
          Zmienna <- baza3$Grupa_wiekowa
        }
        
        Zgony <- baza3$Liczba
        ggplotly(
          ggplot(baza3,aes(x=Czas,y=Zgony,col=Zmienna)) + geom_line())
      })
    }
    
    
  })
  
  
  
  obrobka_eurostat1 <- eventReactive(input$wczytaj_daty3,{
    
    conn <- dbConnect(RSQLite::SQLite(), "data/zgony_gus_eurostat.db")
    
    tmp1 <- input$zakres_dat3
    
    tmp1 <- strsplit(as.character(tmp1),"-")
    
    x <- gsub('\\.','-',tmp1[[1]][1])
    y <- gsub('\\.','-',tmp1[[1]][2])
    
    try({
      d <- dbGetQuery(conn, "select * from zgony_eurostat")
    })
    
    d$Value <- as.integer(gsub(",|:","",d$Value))
    
    d$TIME <- as.Date(as.character(as.Date(paste0(gsub("W",".",d$TIME), ".1"), "%Y.%U.%u")),format= "%Y-%m-%d")
    
    aggrEu <- d[d$SEX=="Total",c("TIME","GEO", "Value")]
    
    aggrEu <- aggrEu[(aggrEu$TIME> as.Date(x,format= "%Y-%m-%d") & aggrEu$TIME < as.Date(y,format= "%Y-%m-%d")),]
    
    aggrEu <- aggrEu[!apply(is.na(aggrEu), 1, all),]
    
    aggrEu <- aggregate(aggrEu$Value ~ aggrEu$GEO, aggrEu, sum)
    
    
    colnames(aggrEu)[1] <- "GEO"
    colnames(aggrEu)[2] <- "Value"
    
    aggrEu[1][aggrEu[1] == "Germany (until 1990 former territory of the FRG)"] <- "Germany"
    aggrEu[1][aggrEu[1] == "Czechia"] <- "Czech Republic"
    
    return(aggrEu)
    
  })
  
  obrobka_eurostat2 <- eventReactive(input$wczytaj_daty4,{
    
    conn <- dbConnect(RSQLite::SQLite(), "data/zgony_gus_eurostat.db")
    
    tmp1 <- input$zakres_dat4
    
    tmp1 <- strsplit(as.character(tmp1),"-")
    
    
    x <- gsub('\\.','-',tmp1[[1]][1])
    y <- gsub('\\.','-',tmp1[[1]][2])
    
    try({
      d <- dbGetQuery(conn, "select * from zgony_eurostat")
    })
    
    d$Value <- as.integer(gsub(",|:","",d$Value))
    
    d$TIME <- as.Date(as.character(as.Date(paste0(gsub("W",".",d$TIME), ".1"), "%Y.%U.%u")),format= "%Y-%m-%d")
    
    aggrEu <- d[d$SEX=="Total",c("TIME","GEO", "Value")]
    
    
    aggrEu <- aggrEu[(aggrEu$TIME> as.Date(x,format= "%Y-%m-%d") & aggrEu$TIME < as.Date(y,format= "%Y-%m-%d")),]
    
    aggrEu <- aggrEu[!apply(is.na(aggrEu), 1, all),]
    
    aggrEu <- aggregate(aggrEu$Value ~ aggrEu$GEO, aggrEu, sum)
    
    colnames(aggrEu)[1] <- "GEO"
    colnames(aggrEu)[2] <- "Value"
    
    aggrEu[1][aggrEu[1] == "Germany (until 1990 former territory of the FRG)"] <- "Germany"
    aggrEu[1][aggrEu[1] == "Czechia"] <- "Czech Republic"
    
    return(aggrEu)
    
  })  
  
  
  obrobka_gus1 <- eventReactive(input$wczytaj_daty1,{
    
    conn <- dbConnect(RSQLite::SQLite(), "data/zgony_gus_eurostat.db")
    
    tmp1 <- input$zakres_dat1
    
    tmp1 <- strsplit(as.character(tmp1),"-")
    
    try({
      d <- dbGetQuery(conn, "select od, region, liczba from zgony_gus where (region_id like 'PL__' or region_id = 'PL9') and plec = 'Ogolem' and grupa_wiekowa = '0 - Inf'")
    })
    
    aggrGUS <- d[(d$Od> as.Date(tmp1[[1]][1],format= "%Y.%m.%d") & d$Od < as.Date(tmp1[[1]][2],format= "%Y.%m.%d")),]
    
    aggrGUS <- aggregate(aggrGUS$Liczba ~ aggrGUS$Region, aggrGUS, sum)
    
    colnames(aggrGUS)[1] <- "Wojewodztwo"
    colnames(aggrGUS)[2] <- "Liczba"
    

    
    
    return(aggrGUS)
  })
  
  obrobka_gus2 <- eventReactive(input$wczytaj_daty2,{
    
    conn <- dbConnect(RSQLite::SQLite(), "data/zgony_gus_eurostat.db")
    
    tmp1 <- input$zakres_dat2
    
    tmp1 <- strsplit(as.character(tmp1),"-")
    
    try({
      d <- dbGetQuery(conn, "select od, region, liczba from zgony_gus where (region_id like 'PL__' or region_id = 'PL9') and plec = 'Ogolem' and grupa_wiekowa = '0 - Inf'")
    })
    
    aggrGUS <- d[(d$Od> as.Date(tmp1[[1]][1],format= "%Y.%m.%d") & d$Od < as.Date(tmp1[[1]][2],format= "%Y.%m.%d")),]
    
    aggrGUS <- aggregate(aggrGUS$Liczba ~ aggrGUS$Region, aggrGUS, sum)
    
    colnames(aggrGUS)[1] <- "Wojewodztwo"
    colnames(aggrGUS)[2] <- "Liczba"
    
    aggrGUS[1][aggrGUS[1] == "Makroregion Województwo Mazowieckie"] <- "Mazowieckie"
    aggrGUS[1][aggrGUS[1] == "Lódzkie"] <- "Lodzkie"
    
    return(aggrGUS)
  })
  
  
  output$pl_bzp <- renderGvis({
    
    gvisGeoChart(obrobka_gus1(), "Wojewodztwo", "Liczba",
                 options=list(region="PL",
                              displayMode="regions",
                              resolution="provinces"))
  })
  
  output$pl_wzg <- renderGvis({
    
    dane <- obrobka_gus2()
    
    srednia <- mean(dane$Liczba,na.rm=TRUE)
    
    dane$Liczba <- dane$Liczba/srednia
    
    gvisGeoChart(dane, "Wojewodztwo", "Liczba",
                 options=list(region="PL",
                              displayMode="regions",
                              resolution="provinces"))
  })
  
  
  output$eu_bzp <- renderGvis({
    
    gvisGeoChart(obrobka_eurostat1(), "GEO", "Value",
                 options=list(region="150",
                              displayMode="countries"
                 ))
  })
  
  output$eu_wzg <- renderGvis({
    
    dane <- obrobka_eurostat2()
    
    srednia <- mean(dane$Value, na.rm=TRUE)
    
    dane$Value <- dane$Value/srednia
    
    gvisGeoChart(dane, "GEO", "Value",
                 options=list(region="150",
                              displayMode="countries"
                 ))
  })
  
  
  
  obrobka_eurostat3 <- eventReactive(input$wczytaj_daty7,{
    
    conn <- dbConnect(RSQLite::SQLite(), "data/zgony_gus_eurostat.db")
    
    tmp1 <- input$zakres_dat7
    
    tmp1 <- strsplit(as.character(tmp1),"-")
    
    
    x <- gsub('\\.','-',tmp1[[1]][1])
    y <- gsub('\\.','-',tmp1[[1]][2])
    
    try({
      d <- dbGetQuery(conn, "select * from zgony_eurostat")
    })
    
    d$Value <- as.integer(gsub(",|:","",d$Value))
    
    d$TIME <- as.Date(as.character(as.Date(paste0(gsub("W",".",d$TIME), ".1"), "%Y.%U.%u")),format= "%Y-%m-%d")
    
    aggrEu <- d[d$SEX=="Total",c("TIME","GEO", "Value")]
    
    aggrEu[2][aggrEu[2] == "Germany (until 1990 former territory of the FRG)"] <- "Germany"
    
    aggrEu <- aggrEu[(aggrEu$TIME> as.Date(x,format= "%Y-%m-%d") & aggrEu$TIME < as.Date(y,format= "%Y-%m-%d")),]
    
    aggrEu <- aggrEu[!apply(is.na(aggrEu), 1, all),]
    
    
    return(aggrEu)
    
    
  })
  
  obrobka_eurostat4 <- eventReactive(input$wczytaj_daty8,{
    
    conn <- dbConnect(RSQLite::SQLite(), "data/zgony_gus_eurostat.db")
    
    tmp1 <- input$zakres_dat8
    
    tmp1 <- strsplit(as.character(tmp1),"-")
    
    
    x <- gsub('\\.','-',tmp1[[1]][1])
    y <- gsub('\\.','-',tmp1[[1]][2])
    
    try({
      d <- dbGetQuery(conn, "select * from zgony_eurostat")
    })
    
    d$Value <- as.integer(gsub(",|:","",d$Value))
    
    d$TIME <- as.Date(as.character(as.Date(paste0(gsub("W",".",d$TIME), ".1"), "%Y.%U.%u")),format= "%Y-%m-%d")
    
    aggrEu <- d[d$SEX=="Total",c("TIME","GEO", "Value")]
    
    aggrEu[2][aggrEu[2] == "Germany (until 1990 former territory of the FRG)"] <- "Germany"
    
    aggrEu <- aggrEu[(aggrEu$TIME> as.Date(x,format= "%Y-%m-%d") & aggrEu$TIME < as.Date(y,format= "%Y-%m-%d")),]
    
    aggrEu <- aggrEu[!apply(is.na(aggrEu), 1, all),]
    
    
    return(aggrEu)
    
    
  })
  
  
  obrobka_gus3 <- eventReactive(input$wczytaj_daty5,{
    
    conn <- dbConnect(RSQLite::SQLite(), "data/zgony_gus_eurostat.db")
    
    tmp1 <- input$zakres_dat5
    
    tmp1 <- strsplit(as.character(tmp1),"-")
    
    x <- gsub('\\.','-',tmp1[[1]][1])
    y <- gsub('\\.','-',tmp1[[1]][2])
    
    try({
      d <- dbGetQuery(conn, "select od,region,liczba from zgony_gus where region = 'Polska'")
    })
    
    aggrGUS <- d[c("Od","Region","Liczba")]
    
    aggrGUS <- aggrGUS[(aggrGUS$Od> x & aggrGUS$Od < y),]
    
    
    return(aggrGUS)
  })
  
  obrobka_gus4 <- eventReactive(input$wczytaj_daty6,{
    
    conn <- dbConnect(RSQLite::SQLite(), "data/zgony_gus_eurostat.db")
    
    tmp1 <- input$zakres_dat6
    
    tmp1 <- strsplit(as.character(tmp1),"-")
    
    x <- gsub('\\.','-',tmp1[[1]][1])
    y <- gsub('\\.','-',tmp1[[1]][2])
    
    try({
      d <- dbGetQuery(conn, "select od,region,liczba from zgony_gus where region = 'Polska'")
    })
    
    aggrGUS <- d[c("Od","Region","Liczba")]
    
    aggrGUS <- aggrGUS[(aggrGUS$Od> x & aggrGUS$Od < y),]
    
    
    return(aggrGUS)
  })
  
  
  output$pl_bzp1 <- renderPlotly({
    
    aggrPl <- obrobka_gus3()
    
    aggrPl$Od <- as.POSIXct(aggrPl$Od,tz="GMT")
    
    img <- (
      ggplot(aggrPl,aes(x=Od,y=Liczba,col=Region))
      +
        geom_line()
      
    )
    
    return(img)
    
  })
  
  output$pl_wzg1 <- renderPlotly({
    
    dane <- obrobka_gus4()
    
    srednia <- mean(dane$Liczba, na.rm=TRUE)
    
    dane$Liczba <- dane$Liczba/srednia
    
    dane$Od <- as.POSIXct(dane$Od,tz="GMT")
    
    img <- (
      ggplot(dane,aes(x=Od,y=Liczba,col=Region))
      +
        geom_line()
      
    )
    
    return(img)
  })
  
  
  output$eu_bzp1 <- renderPlotly({
    
    aggrEu <- obrobka_eurostat3()
    
    aggrEu$TIME <- as.POSIXct(aggrEu$TIME,tz="GMT")
    
    img <- (
      ggplot(aggrEu,aes(x=TIME,y=Value,col=GEO))
      + 
        geom_line()
    )
    
    return(img)
    
  })
  
  output$eu_wzg1 <- renderPlotly({
    
    dane <- obrobka_eurostat4()
    
    srednia <- mean(dane$Value, na.rm=TRUE)
    
    dane$Value <- dane$Value/srednia
    
    dane$TIME <- as.POSIXct(dane$TIME,tz="GMT")
    
    img <- (
      ggplot(dane,aes(x=TIME,y=Value,col=GEO))
      + 
        geom_line()
    )
    
    return(img)
    
  })
  
  
}
shinyApp(ui = ui, server = server)