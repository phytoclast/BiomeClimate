#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(ggplot2)
#calculate percentiles
library(plyr)


######
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

 rv <- reactiveValues(my_text = "") 
    #parameters
    lat <- c(0, 80)
    lon <- c(-150, -50) 
    elev <- c(-432, 8848) 
    temp <- c(-30, 35) 
    prec <- c(0, 9000) 
    biomnm <- ''
    econame <- ''
    Norm <- 1990
    
    output$Biome = renderUI({
      
      listBiome1 <- subset(Biomeclimate[order(Biomeclimate$BIOME),],
                           Latitude >= input$lat[1] &
                             Latitude <= input$lat[2] &
                             Longitude >= input$lon[1] &
                             Longitude <= input$lon[2] &
                             Elevation >= input$elev[1] &
                             Elevation <= input$elev[2] &
                             TT >= input$temp[1] &
                             TT <= input$temp[2]&
                             PP >= input$prec[1] &
                             PP <= input$prec[2]
      )
      
      listBiome <- unique(listBiome1[, c('biomname')])
      
      
      selectInput(inputId = "Biome",
                  label = "Select Biome:",
                  choices = listBiome,
                  selected = "")
    })
    
    
    output$ECO_NAME = renderUI({

      listEco1 <- subset(Biomeclimate,
                           Latitude >= input$lat[1] &
                           Latitude <= input$lat[2] &
                           Longitude >= input$lon[1] &
                           Longitude <= input$lon[2] &
                           Elevation >= input$elev[1] &
                           Elevation <= input$elev[2] &
                           TT >= input$temp[1] &
                           TT <= input$temp[2] &
                           PP >= input$prec[1] &
                           PP <= input$prec[2] &
                           grepl(input$Biome, biomname))
      listEco <- sort(unique(listEco1[, c('ECO_NAME')]))
      
      selectInput(inputId = "ECO_NAME", #name of input
                  label = "Select WWF Ecoregion:", #label displayed in ui
                  choices = unique(listEco), #calls list of available counties
                  selected = unique(listEco)[1])
    })
    output$elev = renderUI({
      
      sliderInput(inputId = 'elev',
                  label = 'Elevation range',
                  min= -500, max= 10000,
                  value= c(-432, 8848), step = 50,
                  dragRange = TRUE)
      
    })
    output$lat = renderUI({
      
      sliderInput(inputId = 'lat',
                  label = 'Latitude range',
                  min= -90, max= 90,
                  value= c(25, 50), step = 1,
                  dragRange = TRUE)
      
    })
    output$lon = renderUI({
      
      sliderInput(inputId = 'lon',
                  label = 'Longitude range',
                  min= -180, max= 180,
                  value= c(-150, -50), step = 2.5,
                  dragRange = TRUE)
      
    })

    output$temp = renderUI({
      
      sliderInput(inputId = 'temp',
                  label = 'Mean Annual Air Temperature range',
                  min= -30, max= 35,
                  value= c(-30, 35), step = 1,
                  dragRange = TRUE)
      
    })

    output$prec = renderUI({

      sliderInput(inputId = 'prec',
                  label = 'Mean Annual Precipitation range',
                  min= 0, max= 9000,
                  value= c(0, 9000), step = 50,
                  dragRange = TRUE)
      
    })
    
    
       output$climplot <- renderPlot({ 

    #parameters
    lat <- c(0, 80)
    lon <- c(-150, -50) 
    elev <- c(-432, 8848) 
    temp <- c(-30, 35) 
    prec <- c(0, 9000) 
    biomnm <- input$Biome
    econame <- input$ECO_NAME
    

    
    
    selectClim <- subset(Biomeclimate,
                         Latitude >= input$lat[1] &
                           Latitude <= input$lat[2] &
                           Longitude >= input$lon[1] &
                           Longitude <= input$lon[2] &
                           Elevation >= input$elev[1] &
                           Elevation <= input$elev[2] &
                           TT >= input$temp[1] &
                           TT <= input$temp[2] &
                           PP >= input$prec[1] &
                           PP <= input$prec[2] &
                           Norm == input$RadioNorm &
                           grepl(biomnm, biomname) &
                           grepl(econame, ECO_NAME))
    
    #Make Monthly Rows
    
    #Jan
    selectMonthly <- selectClim[,c("ECO_ID","ECO_NAME","BIOME","Latitude","Longitude","Elevation","t01","tl01","th01","p01","e01")]
    colnames(selectMonthly) <- c("ECO_ID","ECO_NAME","BIOME","Latitude","Longitude","Elevation","t","tl","th","p","e")
    selectMonthly$Month<- 1
    
    #Feb-Dec
    for (i in 1:11){
      
      selectMonthlyA <- selectClim[,c("ECO_ID","ECO_NAME","BIOME","Latitude","Longitude", "Elevation",
                                      colnames(selectClim)[which(colnames(selectClim)=='t01')+i],
                                      colnames(selectClim)[which(colnames(selectClim)=='tl01')+i],
                                      colnames(selectClim)[which(colnames(selectClim)=='th01')+i],
                                      colnames(selectClim)[which(colnames(selectClim)=='p01')+i],
                                      colnames(selectClim)[which(colnames(selectClim)=='e01')+i])]
      colnames(selectMonthlyA)<- c("ECO_ID","ECO_NAME","BIOME","Latitude","Longitude","Elevation","t","tl","th","p","e")
      selectMonthlyA$Month<- i+1
      selectMonthly <- rbind(selectMonthly, selectMonthlyA)
    }
    rm(selectMonthlyA)
    sumMonthly <- ddply(selectMonthly, "Month", summarise,
                        t25 = quantile(t, 0.25), t75 = quantile(t, 0.75), t = mean(t),
                        p25 = quantile(p, 0.25), p75 = quantile(p, 0.75), p = mean(p), 
                        e = mean(e), tl = mean(tl), th = mean(th))
    
    
    selectClim$PPETRatio <- selectClim$P/(selectClim$Deficit + selectClim$P - selectClim$Surplus +0.0001)
    selectClim$Mindex <- selectClim$PPETRatio/(selectClim$PPETRatio+1)
    T <- apply(selectClim[,c('t01', 't02', 't03', 't04', 't05', 't06', 't07', 't08', 't09', 't10', 't11', 't12')], MARGIN=c(2), na.rm=TRUE, FUN='mean')
    E <- apply(selectClim[,c('e01', 'e02', 'e03', 'e04', 'e05', 'e06', 'e07', 'e08', 'e09', 'e10', 'e11', 'e12')], MARGIN=c(2), na.rm=TRUE, FUN='mean')
    P <- apply(selectClim[,c('p01', 'p02', 'p03', 'p04', 'p05', 'p06', 'p07', 'p08', 'p09', 'p10', 'p11', 'p12')], MARGIN=c(2), na.rm=TRUE, FUN='mean')
    MAAT <- mean(T)
    
    Elev<-mean(selectClim$Elevation, na.rm=TRUE)
    
    Lat<-mean(selectClim$Latitude, na.rm=TRUE)
    Lon<-mean(selectClim$Longitude, na.rm=TRUE)
    PET<-sum(E, na.rm=TRUE)
    MAP<-sum(P, na.rm=TRUE)
    
    
    PPETRatio<-MAP/(PET+0.0001)
    Deficit <- mean(selectClim$Deficit, na.rm=TRUE)
    Surplus <- mean(selectClim$Surplus, na.rm=TRUE)
    SummerBioT<-mean(selectClim$Tg, na.rm=TRUE)
    peakAET <- mean(selectClim$pAET, na.rm=TRUE)
    
    Tw<-mean(selectClim$Tw, na.rm=TRUE)
    Twh<-mean(selectClim$Twh, na.rm=TRUE)
    Tc<-mean(selectClim$Tc, na.rm=TRUE)
    Tcl<-mean(selectClim$Tcl, na.rm=TRUE)
    Tclx <- mean(selectClim$Tclx, na.rm=TRUE)
    
    
    selectClim$SP1 <- round(ifelse(selectClim$PPETRatio < 0.5 & selectClim$Surplus < 25 & selectClim$pAET < 75, pmax(selectClim$Surplus/25, selectClim$pAET/75)  ,1),15)
    selectClim$SP2 <- round(ifelse(selectClim$SP1 >= 1, ifelse(selectClim$pAET < 75 & (selectClim$Deficit >= 150 | selectClim$PPETRatio < 1), pmax(selectClim$pAET/75, 150/(selectClim$Deficit+150)),1),0),15)
    selectClim$SP3 <- round(ifelse(selectClim$SP2 >= 1, ifelse(selectClim$Deficit >= 150 | selectClim$PPETRatio < 1, pmax(150/(selectClim$Deficit+150)),1),0),15)
    selectClim$SP4 <- round(ifelse(selectClim$SP3 >= 1, pmin(1-selectClim$Deficit/150),0),15)
    selectClim$SPindex <- selectClim$SP1 + selectClim$SP2 + selectClim$SP3 + selectClim$SP4 + 1 #Seasonal precipitation index
    selectClim$Cindex <- pmin(selectClim$Tclx+15, selectClim$Tc) #Cold index
    
    #Key to climate type_____________________________________________________
    
    
    Seasonalilty <- ifelse(Deficit < 150 & PPETRatio>=1, "Isopluvial",
                           ifelse(Surplus < 25 & PPETRatio < 0.5 & peakAET < 75, "Isoxeric",
                                  ifelse(peakAET < 75,"Xerothermic","Pluviothermic")))
    
    
    
    
    
    
    
    MRegime <- ifelse(PPETRatio>=2,"Perhumid",
                      ifelse(PPETRatio>=1.414,"Moist-Humid",
                             ifelse(PPETRatio>=1,"Dry-Humid",
                                    ifelse(PPETRatio>=0.707,"Moist-Subhumid",
                                           ifelse(PPETRatio>=0.5,"Dry-Subhumid",
                                                  ifelse(PPETRatio>=0.25,"Semiarid",
                                                         ifelse(PPETRatio>=0.125,"Arid","Perarid"
                                                         )))))))
    
    
    BioTemperatureC <- 
      ifelse(Tc >= 20 & Tclx >=5,"Meso-Tropical",
             ifelse(Tc >= 15 & Tclx >=0,"Cryo-Tropical",
                    ifelse(Tc >= 10 & Tclx >=-5,"Thermo-Sutropical",
                           ifelse(Tc >= 5 & Tclx >=-10,"Meso-Subtropical",
                                  ifelse(Tc >= 0 & Tclx >=-15,"Cryo-Subtropical",
                                         ifelse(Tc >= -5 & Tclx >=-20,"Thermo-Temperate",
                                                ifelse(Tc >= -10 & Tclx >=-25,"Meso-Temperate",
                                                       ifelse(Tc >= -25 & Tclx >=-40,"Cryo-Temperate","Polar"
                                                       ))))))))
    
    BioTemperatureW <- ifelse(SummerBioT >= 24,"Hot (Lowland)",
                              ifelse(SummerBioT >= 18,"Warm (Premontane)",
                                     ifelse(SummerBioT >= 15,"Warm-Mild (Lower-Montane)",
                                            ifelse(SummerBioT >= 12,"Cool-Mild (Upper-Montane)",
                                                   ifelse(SummerBioT >= 6,"Cool (Subalpine)","Cold (Alpine)"
                                                   )))))
    Climatetext<-paste(BioTemperatureW," ",BioTemperatureC,", ",MRegime," ",Seasonalilty, sep="" )
    
    #assemble supplemental summary
    
    my_text1 <- paste("Lat: ",round(Lat,digits=2),"  Lon:", round(Lon,digits=2),"  Elev: ",round(Elev,digits=0)," m")
    metric <- paste("Lat: ",round(Lat,digits=2),"°;  Lon: ", round(Lon,digits=2),"°;  Elev: ",round(Elev,digits=0)," m","\n",
                      "MAAT: ",round(MAAT,digits=1),"°C;  ","MAP: ", round(MAP,0)," mm  ","\n",
                      "Warm Month: ", round(Tw,1),"°C; High: ",round(Twh,1),"°C; ", "Cold Month: ", round(Tc,1),"°C; Low: ",round(Tcl,1),"°C","\n",
                      "Growing Season Temperature: ",round(SummerBioT,digits=1),"°C; Annual Extreme Low: ", round(Tclx,1),"°C","\n",
                      "P/PET: ", round(PPETRatio,2),"; Surplus: ", round(Surplus,0)," mm; Deficit: ", round(Deficit,0)," mm; Peak AET: ", round(peakAET,0), " mm","\n", Climatetext,sep="")
#, "SPindex: ",round(SPindex,2),"; Cindex: ",round(Cindex,2),"\n"
    retro <- paste("Lat: ",round(Lat,digits=2),"°;  Lon: ", round(Lon,digits=2),"°;  Elev: ",round(Elev/0.3048,digits=0)," ft","\n",
                      "Annual Temperature: ",round(MAAT*1.8+32,digits=0),"°F;  ","Annual Precipitation: ", round(MAP/25.4,0)," in  ","\n",
                      "Warm Month: ", round(Tw*1.8+32,0),"°F; High: ",round(Twh*1.8+32,0),"°F; ", "Cold Month: ", round(Tc*1.8+32,0),"°F; Low: ",round(Tcl*1.8+32,0),"°F","\n",
                      "Growing Season Temperature: ",round(SummerBioT*1.8+32,digits=0),"°F; Annual Extreme Low: ", round(Tclx*1.8+32,0),"°F","\n",
                      "P/PET: ", round(PPETRatio,2),"; Surplus: ", round(Surplus/25.4,0)," in; Deficit: ", round(Deficit/25.4,0)," in; Peak AET: ", round(peakAET/25.4,0), " in","\n", Climatetext,sep="")
    my_text2 <- if(input$RadioUnits == 'USC'){retro} else {metric}
    rv$my_text2 <- my_text2
    #aggregate graph
    climplot <- ggplot(sumMonthly, aes(x=Month)) + 
      geom_bar(stat="identity",aes(fill="Precipitation", y=p/5), alpha = 0.85,  color="blue") +
      geom_bar(stat="identity", aes(fill='PET', y=e/5), alpha = 0.60,  color="red" ) +
      geom_line(stat="identity",  aes(color= "Temperature", y=t), alpha = 1) +
      geom_point(aes(shape='Mean', y=t), color="red") +
      geom_point(aes(shape='Low', y=tl), color="red") +
      geom_point(aes(shape='High', y=th), color="red") +
      geom_errorbar(aes(ymin=p25/5, ymax=p75/5), width=.2,position=position_dodge(-0.9), color="blue") +
      geom_errorbar(aes(ymin=t25, ymax=t75), width=.2,position=position_dodge(0.9), color="red") +
      
      scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12), labels=c('01','02','03','04','05','06','07','08','09','10','11','12'))+
      scale_y_continuous(name= "Temperature",
                         breaks=c(-20,-15,-10,-5,0,5,10,15,20,25,30,35,40,45), labels=c('-20 (-4)', '-15 (  5)', '-10 (14)', '-5 (23)', '0 (32)', '5 (41)', '10 (50)', '15 (59)', '20 (68)', '25 (77)', '30 (86)', '35 (95)', '40 (104)', '°C (°F)'),
                         sec.axis = sec_axis(trans = ~.*1,
                                             name = "Precipitation",
                                             breaks=c(0,5,10,15,20,25,30,35,40,45),
                                             labels = c('0', '25   (1)', '50   (2)', '75   (3)', '100 (4)', '125 (5)', '150 (6)', '175 (7)', '200 (8)', 'mm (in)')))+
      
      
      #coord_cartesian(xlim = c(1,12), ylim = c(-20, 43))+
      
      theme(legend.position="bottom") +
      scale_fill_manual("Legend", values = c("Precipitation" = "cyan", "PET" = "yellow"))+
      scale_color_manual("",values = c("Temperature" = "red", "Mean" = "red", "Low" = "red", "High"="red","Growth"="darkgreen"))+
      scale_shape_manual("",values = c("Mean" = 19, "Low" = 6, "High"=2))+
      coord_fixed(ratio = 1/9,xlim = c(1,12), ylim = c(-20, 43))+
      labs(title = paste("Climate of ",selectClim[1,]$ECO_NAME, sep=""))# ,  subtitle = my_text1)
#Supplemental Graph1----
    a1=data.frame(x=c(-40,-40,0,0), y=c(0,6,6,0))
    a2=data.frame(x=c(-40,-40,0,0), y=c(6,12,12,6))
    a3=data.frame(x=c(-40,-40,0,0), y=c(12,30,30,12))
    a4=data.frame(x=c(0,0,6,0), y=c(0,6,6,0))
    a5=data.frame(x=c(0,0,18,6), y=c(6,18,18,6))
    a6=data.frame(x=c(0,0,15,15), y=c(18,30,30,18))
    a7=data.frame(x=c(15,15,30,18), y=c(18,30,30,18))
    
    
 climplot2 <-  ggplot() +
      geom_polygon(data=a1, mapping=aes(x=x, y=y, fill='alpine'),alpha = 0.3)+
      geom_polygon(data=a2, mapping=aes(x=x, y=y, fill='boreal'),alpha = 0.3)+
      geom_polygon(data=a3, mapping=aes(x=x, y=y, fill='temperate'),alpha = 0.3)+
      geom_polygon(data=a4, mapping=aes(x=x, y=y, fill='andean'),alpha = 0.3)+
      geom_polygon(data=a5, mapping=aes(x=x, y=y, fill='oceanic'),alpha = 0.3)+
      geom_polygon(data=a6, mapping=aes(x=x, y=y, fill='subtropical'),alpha = 0.3)+
      geom_polygon(data=a7, mapping=aes(x=x, y=y, fill='tropical'),alpha = 0.3)+
      geom_point(data=selectClim, mapping=aes(x=Cindex, y=Tg), color = 'black', size=0.1)+
      geom_density2d(data=selectClim, mapping=aes(x=Cindex, y=Tg),color = 'black',alpha = 0.25)+
      scale_fill_manual("Legend", values = c("alpine" = "pink",
                                             "boreal" = "darkgreen",
                                             "temperate" = "greenyellow",
                                             "andean" = "violet",
                                             "oceanic" = "darkcyan",
                                             "subtropical" = "orange",
                                             "tropical" = "darkred"
      ))+
      scale_x_continuous(name= "Coldest Month (Annual Extreme Minimum)", 
                         breaks=c(-40, -35, -30, -25, -20,-15, -10,-5, 0,5, 10,15, 20,25,30),
                         labels=c('-40 (-55)', '-35 (-50)','-30 (-45)', '-25 (-40)','-20 (-35)','-15 (-30)','-10 (-25)',
                                  '-5 (-20)','0 (-15)','5 (-10)','10 (-5)','15 (0)','20 (5)','25 (10)','30 (15)'))+
      scale_y_continuous(name= "Growing Season", breaks=c(0,6,12,18,24,30))+
      coord_fixed(ratio = 1/1,xlim = c(-40,30), ylim = c(0, 30))+
   labs(title = paste("Climate of ",selectClim[1,]$ECO_NAME, sep=""))+
   theme(legend.position='right') 
 #Supplemental Graph2----
 bs1=data.frame(y=c(1,1,2,2), x=c(0,0.3333,0.3333,0))
 bs2=data.frame(y=c(2,2,3,3), x=c(0,1,1,0))
 bs3=data.frame(y=c(3,3,4,4), x=c(0,1,1,0))
 bs4=data.frame(y=c(4,4,5,5), x=c(0.5,1,1,0.5))
 
 bm1=data.frame(y=c(1,1,4,4), x=c(0,0.1111,0.1111,0))
 bm2=data.frame(y=c(1,1,4,4), x=c(0.1111,0.2,0.2,0.1111))
 bm3=data.frame(y=c(1,1,4,4), x=c(0.2,0.3333,0.333,0.2))
 bm4=data.frame(y=c(2,2,4,4), x=c(0.3333,0.5,0.5,0.3333))
 bm5=data.frame(y=c(2,2,5,5), x=c(0.5,0.6667,0.6667,0.5))
 bm6=data.frame(y=c(2,2,5,5), x=c(0.6667,1,1,0.6667))
 
 climplot3 <- ggplot() +
   geom_polygon(data=bs1, mapping=aes(x=x, y=y, fill='isoxeric'),alpha = 0.2)+
   geom_polygon(data=bs2, mapping=aes(x=x, y=y, fill='xerothermic'),alpha = 0.2)+
   geom_polygon(data=bs3, mapping=aes(x=x, y=y, fill='pluviothermic'),alpha = 0.2)+
   geom_polygon(data=bs4, mapping=aes(x=x, y=y, fill='isopluvial'),alpha = 0.2)+
   geom_polygon(data=bm1, mapping=aes(x=x, y=y, fill='perarid'),alpha = 0.2)+
   geom_polygon(data=bm2, mapping=aes(x=x, y=y, fill='arid'),alpha = 0.2)+
   geom_polygon(data=bm3, mapping=aes(x=x, y=y, fill='semiarid'),alpha = 0.2)+
   geom_polygon(data=bm4, mapping=aes(x=x, y=y, fill='subhumid'),alpha = 0.2)+
   geom_polygon(data=bm5, mapping=aes(x=x, y=y, fill='humid'),alpha = 0.2)+
   geom_polygon(data=bm6, mapping=aes(x=x, y=y, fill='perhumid'),alpha = 0.2)+
   geom_point(data=selectClim, mapping=aes(y=SPindex, x=Mindex), color = 'black', size=0.1)+
   geom_density2d(data=selectClim, mapping=aes(y=SPindex, x=Mindex),color = 'black',alpha = 0.25)+
   scale_fill_manual("Legend", values = c("isoxeric" = "red",
                                          "xerothermic" = "blue",
                                          "pluviothermic" = "yellow",
                                          "isopluvial" = "green",
                                          "perarid" = "red",
                                          "arid" = "orange",
                                          "semiarid" = "yellow",
                                          "subhumid" = "green",
                                          "humid" = "blue",
                                          "perhumid" = "purple"
   ))+
   scale_y_continuous(name= "Seasonality", breaks=c(1, 2,3,4),
                      labels=c('Isoxeric', 'Xerothermic', 'Pluviothermic','Isopluvial'))+
   scale_x_continuous(name= "P/PET Ratio", breaks=c(0, 0.1111, 0.2,0.3333,0.5,0.6667),
                      labels=c('perarid', 'arid', 'semiarid','subhumid','humid','perhumid'))+
   coord_fixed(ratio = 1/9, ylim = c(1,5), xlim = c(0, 1))+
   labs(title = paste("Climate of ",selectClim[1,]$ECO_NAME, sep=""))+
   theme(legend.position='none', axis.text.x = element_text(angle = 0, vjust = 0, hjust = -0.5), axis.text.y = element_text(vjust = -2)) 

  #moisture x temperature
 bw1=data.frame(y=c(0,0,6,6), x=c(-4,2,2,-4))
 bw2=data.frame(y=c(6,6,12,12), x=c(-4,2,2,-4))
 bw3=data.frame(y=c(12,12,18,18), x=c(-4,2,2,-4))
 bw4=data.frame(y=c(18,18,24,24), x=c(-4,2,2,-4))
 bw5=data.frame(y=c(24,24,30,30), x=c(-4,2,2,-4))
 
 bmm1=data.frame(y=c(0,0,30,30), x=c(-4,-3,-3,-4))
 bmm2=data.frame(y=c(0,0,30,30), x=c(-3,-2,-2,-3))
 bmm3=data.frame(y=c(0,0,30,30), x=c(-2,-1,-1,-2))
 bmm4=data.frame(y=c(0,0,30,30), x=c(-1,0,0,-1))
 bmm5=data.frame(y=c(0,0,30,30), x=c(0,1,1,0))
 bmm6=data.frame(y=c(0,0,30,30), x=c(1,2,2,1))
 

climplot4 <- ggplot() +
   geom_polygon(data=bw1, mapping=aes(x=x, y=y, fill='alpine'),alpha = 0.2)+
   geom_polygon(data=bw2, mapping=aes(x=x, y=y, fill='cool'),alpha = 0.2)+
   geom_polygon(data=bw3, mapping=aes(x=x, y=y, fill='mild'),alpha = 0.2)+
   geom_polygon(data=bw4, mapping=aes(x=x, y=y, fill='warm'),alpha = 0.2)+
   geom_polygon(data=bw5, mapping=aes(x=x, y=y, fill='hot'),alpha = 0.2)+
   geom_polygon(data=bmm1, mapping=aes(x=x, y=y, fill='perarid'),alpha = 0.1)+
   geom_polygon(data=bmm2, mapping=aes(x=x, y=y, fill='arid'),alpha = 0.1)+
   geom_polygon(data=bmm3, mapping=aes(x=x, y=y, fill='semiarid'),alpha = 0.1)+
   geom_polygon(data=bmm4, mapping=aes(x=x, y=y, fill='subhumid'),alpha = 0.1)+
   geom_polygon(data=bmm5, mapping=aes(x=x, y=y, fill='humid'),alpha = 0.1)+
   geom_polygon(data=bmm6, mapping=aes(x=x, y=y, fill='perhumid'),alpha = 0.1)+
  geom_point(data=selectClim, mapping=aes(x=log2(PPETRatio), y=Tg), color = 'black', size=0.1)+
  geom_density2d(data=selectClim, mapping=aes(x=log2(PPETRatio), y=Tg),color = 'black',alpha = 0.25)+
  scale_fill_manual("Legend", values = c("alpine" = "cyan",
                                          "cool" = "green",
                                          "mild" = "yellow",
                                          "warm" = "orange",
                                          "hot" = "red",
                                          "perarid" = "red",
                                          "arid" = "orange",
                                          "semiarid" = "yellow",
                                          "subhumid" = "green",
                                          "humid" = "cyan",
                                          "perhumid" = "blue"
   ))+
   scale_y_reverse(name= "Growing Season", breaks=c(6,12,18,24,30),
                   labels=c('alpine/arctic 6', 'cool 12', 'mild 18','warm 24','hot 30'))+
   scale_x_continuous(name= "P/PET Ratio", breaks=c(-4, -3, -2,-1,0,1),
                      labels=c('perarid', 'arid', 'semiarid','subhumid','humid','perhumid'))+
   coord_fixed(ratio = 1/5,ylim = c(0,30), xlim = c(-4, 2))+
   
  labs(title = paste("Climate of ",selectClim[1,]$ECO_NAME, sep=""))+
  theme(legend.position='none', axis.text.x = element_text(angle = 90, vjust = 2.5, hjust = 1), 
         axis.text.y = element_text(vjust = 0)) 
 #----
    
if(input$RadioGraphtype == 1){climplot} 
else if(input$RadioGraphtype == 2) {climplot2}
else if(input$RadioGraphtype == 3) {climplot3}
else{climplot4}

    

    

    })

    
  output$Climtext = renderText({ 
    rv$my_text2
    })

})
