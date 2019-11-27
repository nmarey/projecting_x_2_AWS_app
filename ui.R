library(shiny)

navbarPage("Marey Projections!",
           tabPanel("Data",
                    sidebarLayout(
                      sidebarPanel(
                        numericInput("fg_player_id", "Fangraphs Player ID:", 9218, min = 1, max = 1000000),
                        tags$hr(style="border-color: black;"),
                        h4("Input Projections For Each Category Below"),
                        fluidRow(
                          column(6,numericInput("pa", "Plate Appearances:", 100, min = 1, max = 900 )),
                          column(6,numericInput("ab_per_2b", "AB/Doubles:", 50, min = 1, max = 900 ))
                        ),
                        fluidRow(
                          column(6,numericInput("ab_per_3b", "AB/Triple:", 50, min = 1, max = 900)),
                          column(6,numericInput("pa_per_hbp", "PA/HBP:", 50, min = 1, max = 900))
                        ),
                        fluidRow(
                          column(6,numericInput("pa_per_sf", "PA/SF:", 50, min = 1, max = 900)), 
                          column(6,numericInput("sba_per_TOB", "SBA/TOB:", 50, min = 1, max = 900))
                        ),
                        fluidRow(
                          column(6,numericInput("sb_per", "SB%:", .5, min = 0, max = 1)),
                          column(6,numericInput("bb_per", "BB%:", .2, min = 0, max = 1))
                        ),
                        fluidRow(
                          column(6,numericInput("ibb_per", "IBB%:", .05, min = 0, max = 1)),
                          column(6,numericInput("k_per", "K%:", 0.25, min = 0, max = 1))
                        ),
                        fluidRow(
                          column(6,numericInput("babip", "BABIP:", 50, min = 1, max = 900)),
                          column(6,numericInput("gb_per", "GB%:", 0.25, min = 0, max = 1))
                        ),
                        fluidRow(
                          column(6,numericInput("fb_per", "FB%:", .5, min = 0, max = 1)),
                          column(6,numericInput("hr_per_fb", "HR/FB:", 50, min = 1, max = 900))
                        ),
                        fluidRow(
                          column(6,numericInput("r_per_tob", "R/TOB:", 50, min = 1, max = 900)),
                          column(6,numericInput("rbi_per_bip", "RBI/BIP:", 50, min = 1, max = 900))
                        )
                        
                      ),
                      mainPanel(
                        h1("Projection", align = 'center'),
                        fluidRow(
                          column(width = 12,
                                 DT::dataTableOutput("proj"), style = "overflow-x:scroll;")
                        ),
                        br(),
                        h1("Historical Data", align = 'center'),
                        br(),
                        fluidRow(
                          column(width = 12,
                                 DT::dataTableOutput("table"),style = "overflow-x: scroll;"
                          )
                        ),
                        br(),
                        br(),
                        hr(),
                        print("~~~Fancy Disclaimer~~~~"),
                        br(),
                        print("Copyright Nicholas Marey")
                        
                        # div(style = 'overflow-x: scroll', DT::dataTableOutput("table"))
                      )
                    )
           )
)
