# infinite recursion, need to determine how to do iterative calculation

setwd("C:/Users/Nicholas/Documents/Rshiny Projecting x2.0/Projecting_X_2_App")

source("fangraph_functions.r")


function(input, output, session){

  output$table <- DT::renderDataTable({
    
    DT::datatable(hist_data(), options = list(
      pageLength = 25)
    )
    
  })
  
  
  hist_data <- reactive({
    df <- fangraphs_get_data(input$fg_player_id)
  })
  
  at_bat <- reactive({
    input$pa - walks() - hit_by_pitch() - sac_fly()
    
  })
  
  hits <- reactive({
    input$babip*ball_in_play()+home_runs()
  })
  
  singles <- reactive({
    hits()-doubles()-triples()-home_runs()
  })
  
  doubles <- reactive({
    ifelse(input$ab_per_2b > 0, at_bat()/input$ab_per_2b, 0)
  })
  
  triples <- reactive({
    ifelse(input$ab_per_3b>0, at_bat()/input$ab_per_3b, 0)
  })
  
  home_runs <- reactive({
    input$hr_per_fb*input$fb_per*(ball_in_play())
  })
  
  runs <- reactive({
    input$r_per_tob*(hits()+walks()+hit_by_pitch()-home_runs())+home_runs()
  })
  
  run_batted_in <- reactive({
    input$rbi_per_bip*(ball_in_play()-sac_fly())+(1.565*home_runs())+sac_fly()
  })
  
  walks <- reactive({
    input$bb_per*input$pa
  })
  
  int_walks <- reactive({
    input$ibb_per*input$pa
  })
  
  strikeouts <- reactive({
    input$k_per*input$pa
  })
  
  hit_by_pitch <- reactive({
    ifelse(input$pa_per_hbp > 0, input$pa/input$pa_per_hbp, 0)
    
  })
  
  sac_fly <- reactive({
    ifelse(input$pa_per_sf > 0 , input$pa/input$pa_per_sf, 0)
  })
  
  stolen_base <- reactive({
    input$sba_per_TOB*(singles()+doubles()+walks()+hit_by_pitch())*input$sb_per
  })
  
  caught_steal <- reactive({
    input$sba_per_TOB*(singles()+doubles()+walks()+hit_by_pitch())*(1-caught_steal())
  })
  
  bat_avg <- reactive({
    ifelse(hits()>0, hits()/at_bat(), 0)
  })
  
  on_base_percentage <- reactive({
    (hits()+walks()+hit_by_pitch())/(at_bat()+walks()+hit_by_pitch()+sac_fly())
  })
  
  slug_percentage <- reactive({
    (singles()+(2*doubles())+(3*triples())+(4*home_runs()))/at_bat()
  })
  
  on_base_plus_slug <- reactive({
    on_base_percentage()+slug_percentage()
  })
  
  iso_slug <- reactive({
    slug_percentage()-bat_avg()
  })
  
  weight_on_base <- reactive({
    (0.687*(walks()-int_walks())+0.718*hit_by_pitch()+0.881*singles()+1.256*doubles()+1.594*triples()+2.065*home_runs())/(at_bat()+walks()-O2+sac_fly()+hit_by_pitch())
  })
  
  ball_in_play <- reactive({
    at_bat()-strikeouts()-home_runs()+sac_fly()
  })
  
  line_drive_percentage <- reactive({
    1-input$gb_per-input$fb_per
  })
  
  output$proj <- DT::renderDataTable({
    
    df <- data.frame(AB = at_bat(), PA = input$pa, HITS = hits(), `1B` = singles(), `2B`=doubles(), 
                     `3B` = triples(), HR = home_runs(), R = runs(), RBI = run_batted_in(), BB = walks(),
                     IBB = int_walks(), SO = strikeouts(), HBP = hit_by_pitch(), SF = sac_fly(), SB = stolen_base(),
                     CS = caught_steal(), AVG = bat_avg(), OBP = on_base_percentage(), SLG = slug_percentage(),
                     OPS = on_base_plus_slug(), ISO = iso_slug(), wOBA = weight_on_base(), BIP = ball_in_play(),
                     `AB/2B` = input$ab_per_2b, `AB/3B` = input$ab_per_3b, `PA/HBP` = input$pa_per_hbp, 
                     `PA/SF` = input$pa_per_sf, `SBA/TOB` = input$sba_per_TOB, `SB%` = input$sb_per, 
                     `BB%` = input$bb_per, `IBB%` = input$ibb_per, `K%` = input$k_per, BABIP = input$babip,
                     `GB%` = input$gb_per, `LD%` = line_drive_percentage(), `FB%` = input$fb_per, 
                     `HR/FB`= input$hr_per_fb, `R/TOB` = input$r_per_tob, `RBI/BIP` = input$rbi_per_bip,
                     check.names = FALSE )
    
    DT::datatable(df)
    
  })
  
  
}