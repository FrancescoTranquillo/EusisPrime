# UI ----
ui <- material_page(
    title = "ResoConti",
    
    nav_bar_fixed = TRUE,
    nav_bar_color = "red darken-3",
    
    # Place side-nav in the beginning of the UI
    material_side_nav(
        fixed = TRUE,
        image_source = "img/logo.png",
        # Place side-nav tabs within side-nav
        
        
        material_side_nav_tabs(
            side_nav_tabs = c("Panoramica" = "explore",
                              "Dettaglio" = "tab_report",
                              "Manutenzione" = "tab_manutenzioni"),
            icons = c("explore", "insert_chart","build"),
            color = "red-darken3"
        )
        
    ),
    # tab esplora ----
    
    material_side_nav_tab_content(
        side_nav_tab_id = "explore",
        
        material_row(
            material_column(
                offset = 1,
                width = 3,
                material_card(
                    depth = 4,
                    color = "#c62828",
                    # material_file_input(
                    #     input_id = "file_upload",
                    #     label = "carica export costi",
                    #     color = "#c62828"
                    # ),
                    br(),
                    uiOutput("dropdown1")
                    
                )
            ),
            
            material_column(
                offset = 0,
                width =  7,
                # dygraphOutput("dygraph")
                material_card(
                    depth = 5,
                    highchartOutput("column_bar"),
                    DT::dataTableOutput("table")
                    
                )
            )
        )
    ),
    # tab report ----
    material_side_nav_tab_content(
        side_nav_tab_id = "tab_report",
        
        material_row(
            material_column(width = 3,offset = 1,
                            
                            material_card(
                                title = "Crea filtro",
                                depth = 5,
                                uiOutput("anno"),
                                
                                uiOutput("conto"),
                                
                                uiOutput("mese")
                            )
                            
            ),
            material_column(width = 3,
                            
                            
                            uiOutput("totaleconto")
                            
                            
            )
            
        )
        
        
    ),
    # TAB MANUTENZIONI E APPARECCHI AL MANUTENTORE ----
    material_side_nav_tab_content(
        side_nav_tab_id = "tab_manutenzioni",
        
    )
)




