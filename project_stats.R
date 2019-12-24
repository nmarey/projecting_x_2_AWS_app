# TO DO
# Round stats
# sig digits of stats
# turn into function
# add action button to UI
# connect function to action button in UI

stats <- matrix(rep(NA, 39000), nrow = 1000, ncol = 39)
stats <- as.data.frame(stats)

# Two extra columns need to determine why

names(stats) <- c("AB", "PA", "Hits", "1B", "2B", "3B", "HR", "R", "RBI", "BB", "IBB", "SO", "HBP",
                  "SF", "SB", "CS", "AVG", "OBP", "SLG", "OPS", "ISO", "wOBA", "BIP", "AB/2B",
                  "AB/3B", "PA/HBP", "PA/SF", "SBA/TOB", "SB%", "BB%", "IBB%", "K%", "BABIP",
                  "GB%", "LD%", "FB%", "HR/FB", "R/TOB", "RBI/BIP")

# stats$PA <- input$pa
# stats$`AB/2B` <- input$ab_per_2b
# stats$`AB/3B` <- input$ab_per_3b 
# stats$`PA/HBP` <- input$pa_per_hbp 
# stats$`PA/SF` <- input$pa_per_sf 
# stats$`SBA/TOB` <- input$sba_per_TOB 
# stats$`SB%` <- input$sb_per 
# stats$`BB%` <- input$bb_per 
# stats$`IBB%` <- input$ibb_per 
# stats$`K%` <- input$k_per 
# stats$BABIP <- input$babip
# stats$`GB%` <- input$gb_per 
# stats$`FB%` <- input$fb_per 
# stats$`HR/FB` <- input$hr_per_fb 
# stats$`R/TOB` <- input$r_per_tob 
# stats$`RBI/BIP` <- input$rbi_per_bip 

stats$PA <- 680
stats$`AB/2B` <- 20
stats$`AB/3B` <- 590
stats$`PA/HBP` <- 100 
stats$`PA/SF` <- 100 
stats$`SBA/TOB` <- 100
stats$`SB%` <- 0.75 
stats$`BB%` <- 0.12 
stats$`IBB%` <- 0.01
stats$`K%` <- 0.24 
stats$BABIP <- 0.3
stats$`GB%` <- 0.4
stats$`FB%` <- 0.35
stats$`HR/FB` <- 0.2
stats$`R/TOB` <- 0.5
stats$`RBI/BIP` <- 0.5

for(i in 1:1000){
  if(i==1){
    
    stats$BIP[i] <- stats$PA[i] - (stats$PA[i] / 2)
    
    stats$BB[i] <- stats$`BB%`[i]*stats$PA[i]
    stats$IBB[i] <- stats$`IBB%`[i]*stats$PA[i]
    stats$SO[i] <- stats$`K%`[i]*stats$PA[i]
    
    stats$`LD%`[i] <- 1-stats$`GB%`[i]-stats$`FB%`[i]
    
    stats$SF[i] <- ifelse(stats$`PA/SF`[i]>0, stats$PA[i]/stats$`PA/SF`[i], 0)
    stats$HBP[i] <- ifelse(stats$`PA/HBP`[i]>0, stats$PA[i]/stats$`PA/HBP`[i], 0)
    
    stats$AB[i] <- stats$PA[i]-stats$BB[i]-stats$HBP[i] - stats$SF[i]
    
    stats$`2B`[i] <- ifelse(stats$`AB/2B`[i]>0, stats$AB[i]/stats$`AB/2B`[i], 0)
    stats$`3B`[i] <- ifelse(stats$`AB/3B`[i]>0, stats$AB[i]/stats$`AB/3B`[i], 0)
    stats$HR[i] <- stats$`HR/FB`[i]*stats$`FB%`[i]*stats$BIP[i]
    
    stats$Hits[i] <- stats$BABIP[i]*stats$BIP[i]+stats$HR[i]
    stats$`1B`[i] <- stats$Hits[i] - stats$`2B`[i] - stats$`3B`[i] - stats$HR[i]
    
    stats$AVG[i] <- ifelse(stats$Hits[i]>0, stats$Hits[i]/stats$AB[i], 0)
    
    stats$R[i] <- stats$`R/TOB`[i]*(stats$Hits[i] + stats$BB[i] + stats$HBP[i] - stats$HR[i]) + stats$HR[i]
    stats$RBI[i] <- stats$`RBI/BIP`[i]*(stats$BIP[i]-stats$SF[i]) + (1.565*stats$HR[i]) + stats$SF[i]
    stats$SB[i] <- stats$`SBA/TOB`[i]*(stats$`1B`[i] + stats$`2B`[i] + stats$BB[i] + stats$HBP[i])*stats$`SB%`[i]
    stats$CS[i] <- stats$`SBA/TOB`[i]*(stats$`1B`[i] + stats$`2B`[i] + stats$BB[i] + stats$HBP[i])*(1 - stats$`SB%`[i])
    
    stats$OBP[i] <- (stats$Hits[i]+stats$BB[i]+ stats$HBP[i])/(stats$AB[i]+stats$BB[i]+ stats$HBP[i] + stats$SF[i])
    stats$SLG[i] <- (stats$`1B`[i] + (2*stats$`2B`[i]) + (3*stats$`3B`[i]) + (4*stats$HR[i]))/stats$AB[i]
    stats$OPS[i] <- stats$OBP[i] + stats$SLG[i]
    stats$ISO[i] <- stats$SLG[i] - stats$AVG[i]
    stats$wOBA[i] <- (0.687*(stats$BB[i] - stats$IBB[i]) + 0.718*stats$HBP[i] + 0.881*stats$`1B`[i] + 1.256*stats$`2B`[i] + 1.594*stats$`3B`[i] + 2.065*stats$HR[i])/(stats$AB[i] + stats$BB[i] - stats$IBB[i] + stats$SF[i] + stats$HBP[i])
    
  } else {
    
    stats$BIP[i] = stats$AB[i-1] - stats$SO[i-1] - stats$HR[i-1] + stats$SF[i-1]
    
    stats$BB[i] <- stats$`BB%`[i-1]*stats$PA[i-1]
    stats$IBB[i] <- stats$`IBB%`[i-1]*stats$PA[i-1]
    stats$SO[i] <- stats$`K%`[i-1]*stats$PA[i-1]
    
    stats$`LD%`[i] <- 1-stats$`GB%`[i-1] - stats$`FB%`[i-1]
    
    stats$SF[i] <- ifelse(stats$`PA/SF`[i-1]>0, stats$PA[i-1]/stats$`PA/SF`[i-1], 0)
    stats$HBP[i] <- ifelse(stats$`PA/HBP`[i-1]>0, stats$PA[i-1]/stats$`PA/HBP`[i-1], 0)
    
    stats$AB[i] <- stats$PA[i-1]-stats$BB[i-1]-stats$HBP[i-1] - stats$SF[i-1]
    
    stats$`2B`[i] <- ifelse(stats$`AB/2B`[i-1]>0, stats$AB[i-1]/stats$`AB/2B`[i-1], 0)
    stats$`3B`[i] <- ifelse(stats$`AB/3B`[i-1]>0, stats$AB[i-1]/stats$`AB/3B`[i-1], 0)
    stats$HR[i] <- stats$`HR/FB`[i-1]*stats$`FB%`[i-1]*stats$BIP[i-1]
    
    stats$Hits[i] <- stats$BABIP[i-1]*stats$BIP[i-1]+stats$HR[i-1]
    stats$`1B`[i] <- stats$Hits[i-1] - stats$`2B`[i-1] - stats$`3B`[i-1] - stats$HR[i-1]
    
    stats$AVG[i] <- ifelse(stats$Hits[i-1]>0, stats$Hits[i-1]/stats$AB[i-1], 0)
    
    stats$R[i] <- stats$`R/TOB`[i-1]*(stats$Hits[i-1] + stats$BB[i-1] + stats$HBP[i-1] - stats$HR[i-1]) + stats$HR[i-1]
    stats$RBI[i] <- stats$`RBI/BIP`[i-1]*(stats$BIP[i-1]-stats$SF[i-1]) + (1.565*stats$HR[i-1]) + stats$SF[i-1]
    stats$SB[i] <- stats$`SBA/TOB`[i-1]*(stats$`1B`[i-1] + stats$`2B`[i-1] + stats$BB[i-1] + stats$HBP[i-1])*stats$`SB%`[i-1]
    stats$CS[i] <- stats$`SBA/TOB`[i-1]*(stats$`1B`[i-1] + stats$`2B`[i-1] + stats$BB[i-1] + stats$HBP[i-1])*(1 - stats$`SB%`[i-1])
    
    stats$OBP[i] <- (stats$Hits[i-1]+stats$BB[i-1]+ stats$HBP[i-1])/(stats$AB[i-1]+stats$BB[i-1]+ stats$HBP[i-1] + stats$SF[i-1])
    stats$SLG[i] <- (stats$`1B`[i-1] + (2*stats$`2B`[i-1]) + (3*stats$`3B`[i-1]) + (4*stats$HR[i-1]))/stats$AB[i-1]
    stats$OPS[i] <- stats$OBP[i-1] + stats$SLG[i-1]
    stats$ISO[i] <- stats$SLG[i-1] - stats$AVG[i-1]
    stats$wOBA[i] <- (0.687*(stats$BB[i-1] - stats$IBB[i-1]) + 0.718*stats$HBP[i-1] + 0.881*stats$`1B`[i-1] + 1.256*stats$`2B`[i-1] + 1.594*stats$`3B`[i-1] + 2.065*stats$HR[i-1])/(stats$AB[i-1] + stats$BB[i-1] - stats$IBB[i-1] + stats$SF[i-1] + stats$HBP[i-1])
    
  }
}

