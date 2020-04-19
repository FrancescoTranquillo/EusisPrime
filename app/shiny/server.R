# SERVER ----
server <- function(input, output, session) {
    session$onSessionEnded(stopApp)
    
    # ELABORAZIONE FILE DI INPUT ----
    table <- reactive({
        # req(input$file_upload)
        
        
        # df <- read_xlsx("/cloud/project/files/covid_marzo1920.xlsx")
        df <- read_xlsx(file.choose())
        # df <- read.csv2(
        #     input$file_upload$datapath,
        #     header = T,
        #     stringsAsFactors = F,
        #     row.names = NULL,
        #     dec = ","
        # )
        names(df)[names(df) == "CONTO...9"] <- "CONTO"
        df$DATA <- as.Date(df$DATA, "%d/%m/%Y")
        df$IMPORTO <- as.numeric(df$IMPORTO)
        df$ANNO <- factor(year(df$DATA))
        df$MESE <- factor(months(df$DATA))
        
        return(df)
    })
    
    # DYGRAPH
    output$dygraph <- renderDygraph({
        req(input$dropdown1)
        df_input <- table()
        
        df_dy <-
            df_input[which(df_input$DESCR.CONTO %in% input$dropdown1), c(6, 33)] %>%
            arrange(.$DATA) %>%
            mutate(cumsum = cumsum(.$IMPORTO))
        
        ts <- xts(df_dy[, 3], order.by = df_dy[, 1])
        
        
        dygraph(ts, main = "Importo cumulativo", ylab = "Ordinato") %>%
            dySeries("V1", label = "Importo totale(€)") %>%
            dyRangeSelector(height = 60, strokeColor = "black") %>%
            dyOptions(
                fillGraph = T,
                fillAlpha = 0.4,
                colors = "#c62828",
                strokeWidth = 3,
                axisLineColor = "grey",
                gridLineColor = "grey"
            ) %>%
            dyHighlight(highlightCircleSize = 5) %>%
            dyAxis("x", drawGrid = F)
    })
    output$column_bar <- renderHighchart({
        req(input$dropdown1)
        df <- table()
        totali <- df %>%
            select(
                DATA,
                ANNO,
                MESE,
                DESCR.FORNITORE,
                CONTO,
                DESCR.CONTO,
                "CENTRO DI PRELIEVO",
                DESCR.CDP,
                IMPORTO,
                DESCR.CONTRATTO
            ) %>%
            group_by(CONTO, DESCR.CONTO, ANNO) %>%
            summarize(., IMPORTO_TOT = sum(IMPORTO, na.rm = T))
        
        totali$IMPORTO_TOT <-
            round(totali$IMPORTO_TOT, digits = 0)
        totali %<>% arrange(-IMPORTO_TOT)
        
        totali <- na.omit(totali)
        totali <-
            totali[which(totali$DESCR.CONTO %in% input$dropdown1), ]
        anni <-
            unique(as.numeric(levels(totali$ANNO))[totali$ANNO])
        print(anni)
        color.function <-
            colorRampPalette(c("#CCCCCC" , "#c62828"))
        
        color.ramp <- color.function(n = length(x = anni))
        
        if (length(anni) == 1) {
            color.ramp <- "#c62828"
        }
        
        tab_colori <-
            data.frame("column" = anni, "col" = as.character(color.ramp))
        totali$COLORE <- tab_colori[totali$ANNO, 2]
        
        print(input$dropdown1)
        hchart(totali,
               "column",
               hcaes(
                   x = DESCR.CONTO,
                   y = IMPORTO_TOT,
                   group = ANNO
               ),
               color = color.ramp) %>%
            hc_plotOptions(column = list(dataLabels = list(
                enabled = TRUE,
                format = "€ {point.y:,.f}"
            )))
        
        
        
    })
    
    tab_selezione <- reactive({
        req(input$dropdown1)
        df <- table()
        totali <- df %>%
            select(
                DATA,
                ANNO,
                DESCR.FORNITORE,
                CONTO,
                DESCR.CONTO,
                "CENTRO DI PRELIEVO",
                DESCR.CDP,
                IMPORTO,
                DESCR.CONTRATTO
            ) %>%
            group_by(CONTO, DESCR.CONTO, ANNO) %>%
            summarize(., IMPORTO_TOT = sum(IMPORTO, na.rm = T))
        
        totali$IMPORTO_TOT <-
            round(totali$IMPORTO_TOT, digits = 0)
        totali %<>% arrange(-IMPORTO_TOT)
        
        totali <- na.omit(totali)
        totali <-
            totali[which(totali$DESCR.CONTO %in% input$dropdown1), ]
        anni <-
            unique(as.numeric(levels(totali$ANNO))[totali$ANNO])
        print(anni)
        color.function <-
            colorRampPalette(c("#CCCCCC" , "#c62828"))
        
        color.ramp <- color.function(n = length(x = anni))
        
        if (length(anni) == 1) {
            color.ramp <- "#c62828"
        }
        
        tab_colori <-
            data.frame("column" = anni, "col" = as.character(color.ramp))
        totali$ID <- tab_colori[totali$ANNO, 2]
        
        
        return(totali)
        
        
    })
    output$highchart1 <- renderHighchart({
        req(input$dropdown1)
        df_input <- table()
        
        df_dy <-
            df_input[which(df_input$DESCR.CONTO %in% input$dropdown1), c(6, 33)] %>%
            arrange(.$DATA) %>%
            mutate(cumsum = cumsum(.$IMPORTO))
        
        ts <- xts(df_dy[, 3], order.by = df_dy[, 1])
        hchart(
            ts,
            color = "#c62828",
            lineWidth = 2,
            type = "area",
            name = "Importo totale",
            chart = list(zoomType = "xy"),
            states = list(hover = list(lineWidth = 3)),
            tooltip = list(valueDecimals = 2),
            fillColor = list(
                linearGradient = list(
                    x1 = 0,
                    x2 = 0,
                    y1 = 0,
                    y2 = 1
                ),
                stops = list(c(0, '#c62828'),
                             c(1, "#ffff"))
            ),
            threshold = NULL
        )
        
    })
    # TABELLA GENERICA ----
    output$table <- DT::renderDataTable(server = F, {
        req(input$dropdown1)
        
        DT::datatable(tab_selezione(),
                      extensions = "Buttons",
                      options = options_tab)
    })
    
    # INFOBOXES ----
    output$totaleconto <- renderUI({
        req(input$conto)
        req(input$anno)
        req(input$mese)
        tab_mesi <- tibble("MESE"=c("GENNAIO", "FEBBRAIO", 'MARZO',
                                    'APRILE', 'MAGGIO', 'GIUGNO',
                                    'LUGLIO', 'AGOSTO', 'SETTEMBRE',
                                    'OTTOBRE', 'NOVEMBRE', 'DICEMBRE'), "MONTH"=c("JANUARY", "FEBRUARY", "MARCH","APRIL","MAY" ,"JUNE", "JULY",
                                                                                  "AUGUST", "SEPTEMBER", "OCTOBER", "NOVEMBER", "DECEMBER"))
        df <- table()
        totali <- df %>%
            select(
                DATA,
                ANNO,
                MESE,
                DESCR.FORNITORE,
                CONTO,
                DESCR.CONTO,
                "CENTRO DI PRELIEVO",
                DESCR.CDP,
                IMPORTO,
                DESCR.CONTRATTO
            ) %>%
            group_by(CONTO, DESCR.CONTO, ANNO, MESE) %>%
            summarize(., IMPORTO_TOT = sum(IMPORTO, na.rm = T))
        
        totali$IMPORTO_TOT <-
            round(totali$IMPORTO_TOT, digits = 0)
        totali %<>% arrange(-IMPORTO_TOT)
        
        totali <- na.omit(totali)
        
        #CONVERTO MESE IN INGLESE
        mese_convertito <- as.character(tab_mesi[which(tab_mesi$MESE %in% input$mese),2])
        totali <-
            totali[which(totali$DESCR.CONTO %in% input$conto & totali$ANNO %in% input$anno & toupper(totali$MESE) %in% mese_convertito), which(colnames(totali)%in%"IMPORTO_TOT") ]
        
        material_card(title = HTML(
            paste0(
                "<span style='font-weight:bold; color:#c62828", "Importo Totale", "</span>&nbsp;&nbsp;",
                "<span style='font-size:14px'>", totali, "</span>")
        )
        )
        
    })
    # DROPDOWN ----
    output$dropdown1 <- renderUI({
        # req(input$file_upload)
        df <- table()
        selectInput(
            inputId = "dropdown1",
            label = "Seleziona un conto economico",
            choices = unique(df$DESCR.CONTO),
            multiple = T
        )
        
    })
    
    output$anno <- renderUI({
        # req(input$file_upload)
        df <- table()
        selectInput(
            inputId = "anno",
            label = "Seleziona l'anno di riferimento",
            choices = unique(df$ANNO),
            multiple = T
        )
        
    })
    
    output$conto <- renderUI({
        # req(input$file_upload)
        df <- table()
        selectInput(
            inputId = "conto",
            label = "Seleziona un conto economico",
            choices = unique(df$DESCR.CONTO),
            multiple = T
        )
        
    })
    output$mese <- renderUI({
        # req(input$file_upload)
        df <- table()
        selectInput(
            inputId = "mese",
            label = "Seleziona il mese di interesse",
            choices = c("GENNAIO", "FEBBRAIO", 'MARZO',
                        'APRILE', 'MAGGIO', 'GIUGNO',
                        'LUGLIO', 'AGOSTO', 'SETTEMBRE',
                        'OTTOBRE', 'NOVEMBRE', 'DICEMBRE'),
            multiple = F
            
        )
    })
    
    
    
    
}