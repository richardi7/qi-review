# libraries 

library(dplyr)
library(ggplot2)
library(spccharter)

## SPC function ----

spc <- function(
  #function arguments
  data #data from spcharter
  ,type
  ,improvement.direction
  ,target
  ,rate.multiplier=NA
) {

# repeat for each facet ----
  facet_list <- unique(data$.fac_grp)
  for (i in seq_along(facet_list)) {
    facet_data<-  subset(data, data$.fac_grp==facet_list[[i]])
    # Variables ----  
    xl <- nrow(facet_data)
    xn <- nrow(facet_data) - 1
    improvementdirection <- case_when(improvement.direction == "Decrease" ~ -1, TRUE ~ 1)
    target <- if(!(is.null(target))) as.numeric(target) else rep(NA,xl)
    target = if(dataaspercentages == TRUE) {target/100} else {target}
    
    if (type %in% c("p","P","u","U")){facet_data$measure=facet_data$.numerator/facet_data$.denominator*rate_multiplier}
    else {facet_data$measure=facet_data$.numerator}
    
    facet_data$movingrange = abs(facet_data$measure - c(NA,facet_data$measure[1:xn]))
    facet_data$movingrangeaverage = mean(abs(facet_data$measure - c(NA,facet_data$measure[1:xn])),na.rm = TRUE)
    facet_data$outsidelimits = case_when(facet_data$measure > facet_data$ucl ~ 1
                                         ,facet_data$measure < facet_data$lcl ~ 1
                                         ,TRUE ~ 0
    )
    facet_data$relativetomean = case_when(facet_data$measure > facet_data$centre ~ 1
                                          ,facet_data$measure < facet_data$centre ~ -1
                                          ,TRUE ~ 0
    )
    facet_data$target = target
    facet_data$upper2sigma = facet_data$uwl 
    facet_data$lower2sigma = facet_data$lwl
    
    facet_data <- facet_data %>%
      mutate(closetolimits = case_when(measure > upper2sigma & measure <= ucl ~ 1
                                       ,measure < lower2sigma & measure >= lcl ~ -1
                                       
      )
      ) %>%
      mutate(sevenpointtrend = case_when(relativetomean == lag(relativetomean,1)
                                         & relativetomean == lag(relativetomean,2)
                                         & relativetomean == lag(relativetomean,3)
                                         & relativetomean == lag(relativetomean,4)
                                         & relativetomean == lag(relativetomean,5)
                                         & relativetomean == lag(relativetomean,6)
                                         ~ 1
                                         ,TRUE ~ 0
      )
      ) %>%
      mutate(partoftrend = case_when(sevenpointtrend == 1
                                     | lead(sevenpointtrend,1) == 1
                                     | lead(sevenpointtrend,2) == 1
                                     | lead(sevenpointtrend,3) == 1
                                     | lead(sevenpointtrend,4) == 1
                                     | lead(sevenpointtrend,5) == 1
                                     | lead(sevenpointtrend,6) == 1
                                     ~ 1
                                     ,TRUE ~ 0
      )
      ) %>%
      mutate(sixpointgrowth = case_when(measure > lag(measure,1)
                                        & lag(measure,1) > lag(measure,2)
                                        & lag(measure,2) > lag(measure,3)
                                        & lag(measure,3) > lag(measure,4)
                                        & lag(measure,4) > lag(measure,5)
                                        & lag(measure,5) > lag(measure,6)
                                        ~ 1
                                        ,measure < lag(measure,1)
                                        & lag(measure,1) < lag(measure,2)
                                        & lag(measure,2) < lag(measure,3)
                                        & lag(measure,3) < lag(measure,4)
                                        & lag(measure,4) < lag(measure,5)
                                        & lag(measure,5) < lag(measure,6)
                                        ~ -1
                                        ,TRUE ~ 0
      )
      ) %>%
      mutate(partofgrowth = case_when(abs(sixpointgrowth) == 1
                                      | abs(lead(sixpointgrowth,1)) == 1
                                      | abs(lead(sixpointgrowth,2)) == 1
                                      | abs(lead(sixpointgrowth,3)) == 1
                                      | abs(lead(sixpointgrowth,4)) == 1
                                      | abs(lead(sixpointgrowth,5)) == 1
                                      #| lead(sevenpointgrowth,6) == 1
                                      ~ 1
                                      ,TRUE ~ 0
      )
      ) %>%
      mutate(twointhree = case_when(closetolimits == 1
                                    & (
                                      lead(closetolimits,1) == 1
                                      | lead(closetolimits,2) == 1
                                    ) ~ 1
                                    ,closetolimits == -1
                                    & (
                                      lead(closetolimits,1) == -1
                                      | lead(closetolimits,2) == -1
                                    )
                                    ~ 1
                                    ,closetolimits == 1
                                    & (
                                      lag(closetolimits,1) == 1
                                      | lag(closetolimits,2) == 1
                                    ) ~ 1
                                    ,closetolimits == -1
                                    & (
                                      lag(closetolimits,1) == -1
                                      | lag(closetolimits,2) == -1
                                    )
                                    ~ 1
      )
      ) %>%
      mutate(partoftwointhree = case_when(twointhree == 1 && lag(twointhree,1) == 1
                                          && (
                                            lead(closetolimits,1) == 1
                                            || lead(closetolimits,2) == 2
                                          ) ~ 1
      )
      ) %>%
      mutate(specialcauseflag = case_when(abs(outsidelimits) == 1
                                          | abs(partoftrend) == 1
                                          | abs(sevenpointtrend) == 1
                                          | abs(sixpointgrowth) == 1
                                          | abs(partofgrowth) == 1
                                          | twointhree == 1
                                          ~ 1
      )
      ) %>%
      mutate(specialcauseconcern = case_when(specialcauseflag == 1
                                             & relativetomean == (improvementdirection * -1)
                                             ~ measure
      )
      ) %>%
      mutate(specialcauseimprovement = case_when(specialcauseflag == 1
                                                 & relativetomean == improvementdirection
                                                 ~ measure
      )
      ) %>%
      mutate(specialcauseeither = case_when(specialcauseflag == 1
                                            & improvement.direction %in% c("both","Both","BOTH")
                                            ~ measure
      )
      ) %>%
      mutate(commoncause = case_when(specialcauseflag != 1
                                     ~ measure
      )
      )
    
    if (i==1){facet_out<-facet_data} else {facet_out<-rbind(facet_out,facet_data)}
    
  } #end facet loop
} #spc function
  
#input and variables  ----
  
  dataset <- read.csv("sampledata2.csv")
  datasetNames <- names(read.csv("sampledata2.csv"))
  dataset$Date<- as.Date(dataset$Date,"%d/%m/%Y")

  
  my_run_length=12
  my_initial=8
  
  #spccharter  ----  
  
  c<-spccharter(dataset, numerator = GPReferrals, datecol = Date, initial_rows = my_initial, run_length= my_run_length,
             by = region, plot_type = 'c', direction = "both")
  c_c <- plot(c$spcchart)
  data_u <- c_c$data
  head(data_u)
  
  #add rules  ----    
  
  dataaspercentages=FALSE
  target=100


spc(
   data= data_u   #data
    ,type="i"
    ,improvement.direction="both"
    , target=100
  ) 

#plot charts  ---- 

  point.size=2
  # Create Plot ----
  plot <- ggplot(facet_out,aes(x=Date,y=measure)) +
    theme_classic() +
    geom_line(color="darkgrey",size=point.size/2.666666) +
    geom_point(color="darkgrey",size=point.size) +
    geom_line(aes(y=ucl),linetype = "dashed",size=point.size/2.666666,color="darkgrey") +
    geom_line(aes(y=lcl),linetype = "dashed",size=point.size/2.666666,color="darkgrey") +
    geom_line(aes(y=as.numeric(target)),linetype = "dashed",size=point.size/2.666666,color="purple") +
    geom_line(aes(y=centre)) +
    #geom_line(aes(y=lower2sigma)) +
    geom_point(aes(x=Date,y=specialcauseimprovement),color="skyblue",size=point.size) +
    geom_point(aes(x=Date,y=specialcauseconcern),color="orange",size=point.size) +
    geom_point(aes(x=Date,y=specialcauseeither),color="orange",size=point.size) +
    facet_wrap(~.fac_grp)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  plot

  