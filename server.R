#One visualization to rule them all...

#Todo:
#Change y-axis in visualization if ROI calculation box is checked?
#Explain that the visualisation shows a maximum of 1000 data points

#Clear memory and load packages
rm(list=ls())

library(shiny)
library(mvtnorm)
library(dplyr)
library(ggvis)
library(MASS)

#Define server logic
shinyServer(function(input, output, session) {
  
  ################################
  #PROBABILITY OF SUCCESSFUL HIRE#
  ################################
  
  ##############################################
  #Reactive expressions for important parameters
  
  #Reactive expression to get value for predictive validity
  pred_val <- reactive({
    r = input$pred_val
    r
  })
  
  #Reactive expression to calculate assessment cut-off
  #Depends on input$n_a and input$n_s
  cut_x <- reactive({
    #Conditions to calculate cut-off
    conditions <- !is.na(input$n_s) & !is.na(input$n_a) & input$n_s > 0 & input$n_a > 0 & input$n_s < input$n_a
    
    #proportion of candidates we wish to hire (selection ratio)
    if(conditions) {
      sel_ratio <- input$n_s / input$n_a
      #Quantile function of the normal curve, giving the z-score corresponding to given selection ratio
      c <- qnorm(sel_ratio, lower.tail=FALSE) 
    } else c <- -4
    
    c
  })
  
  #Reactive expression to calculate cut-off for successful job performance
  #Depends on input$baserate
  cut_y <- reactive({
    #Conditions to calculate cut-off
    conditions <- !is.na(input$n_s) & !is.na(input$n_a) & input$n_s > 0 & input$n_a > 0 & input$n_s < input$n_a & input$baserate > 0 & input$baserate < 100
    
    #Transform baserate ratio to z-score
    if(conditions) {
      #Quantile function of the normal curve, giving the z-score corresponding to given selection ratio
      c <- qnorm(input$baserate / 100, lower.tail=FALSE) 
    } else c <- -4
    
    c
    
  })
  
  #####################
  #Reactive text output
  
  #Generate title
  #Depends on input$met
  output$title <- renderText({
    p <- paste("Predictive validity of", pred_val())
  })
  
  #Generate text showing probability of hiring high performers
  #Depends on input$n_a, cut_x() & pred_val()
  output$prob <- renderText({
    
    if(!is.na(input$n_a) & input$n_a > 2) {
      
      if(cut_x() > -4) {
        
        #Create correlation matrix
        sigma <- matrix(c(1, pred_val(), pred_val(), 1), ncol=2, nrow=2)
        
        
        #Calculate probabilities
        top.left <- pmvnorm(mean=c(0,0), corr=sigma, lower=c(-Inf, cut_y()), upper=c(cut_x(), Inf))
        top.right <- pmvnorm(mean=c(0,0), corr=sigma, lower=c(cut_x(), cut_y()), upper=c(Inf, Inf))
        bottom.right <- pmvnorm(mean=c(0,0), corr=sigma, lower=c(cut_x(),-Inf), upper=c(Inf, cut_y()))
        bottom.left <- pmvnorm(mean=c(0,0), corr=sigma, lower=c(-Inf,-Inf), upper=c(cut_x(), cut_y()))
        
        #Precision
        p <- top.right / (top.right + bottom.right)
        #Calculate percentage
        p <- round(p * 100, 1)
        
        #Calculate percentile for cut-off
        x.perc <- round((input$n_s / input$n_a) * 100, 1)
        
        paste0("The probability of a successful hire is ", p, 
               "%. This follows from a predictive validity of ", pred_val(), 
               ", a base rate of ", round(input$baserate, 2), 
               "% successful incumbents and selecting the top ", x.perc,
               "% from the assessment (cut-off z = ", round(cut_x(), 2), ").")
        
      } else paste("The probability cannot be calculated. Please make sure that the number of applicants is larger than the number of positions to fill.")
      
      
    } else paste("The probability cannot be calculated with less than 3 applicants.")  
    
  })
  
  ###############
  #Visualization
  
  #Reactive expression to generate dataset
  #depends on input$n_a, cut() & pred_Val()
  data <- reactive({
    #Conditions for accurate calculations
    conditions <- !is.na(input$n_a) & input$n_a > 2 & !is.na(input$n_s) & input$n_s > 0 & input$n_s < input$n_a & input$baserate > 0 & input$baserate < 100
    
    if(conditions) {
      
      r <- pred_val() #Predictive validity with selection input
      
      #Method 1:
      #Generate uncorrelated data
      #set.seed(29583)
      #if(input$n_a < 300) {df <- data.frame(z1 = rnorm(input$n_a), z2 = rnorm(input$n_a))} else df <- data.frame(z1 = rnorm(300), z2 = rnorm(300))
      
      #Transform y with Cholesky decomposition, to the given correlation coefficient
      #df$x <- df$z1
      #df$y <- r * df$z1 + sqrt(1 - r^2) * df$z2#Selection input
      
      #Method 2:
      sigma <- matrix(c(1, r, r, 1), 2, 2)
      set.seed(29583)
      if(input$n_a < 1000) {
        df <- MASS::mvrnorm(input$n_a, mu = c(0, 0), Sigma = sigma, empirical=TRUE)
      } else if(input$n_a >= 1000) {
        df <- MASS::mvrnorm(1000, mu = c(0, 0), Sigma = sigma, empirical=TRUE)
      } 
      
      df <- data.frame(df)
      names(df) <- c("x", "y")
      
      #Categorize results and assign colour code
      i <- df$x > cut_x() & df$y > cut_y() #upper right
      j <- df$x > cut_x() & df$y <= cut_y() #lower right
      k <- df$x <= cut_x() & df$y > cut_y() #upper left
      l <- df$x <= cut_x() & df$y <= cut_y() #lower left
      
      df$cat[i] <- "#2CA02C" #"Selected high performers"
      df$cat[j] <- "#D62728" #"Selected low performers"
      df$cat[k] <- "#FF7F0E"  #"Not selected high performers"
      df$cat[l] <- "#1F77B4" #"Not selected low performers"
      
    } else df <- data.frame(x=-4:4, y=rep(0,9), cat=rep("white", 9))
    
    df
  })
  
  #Generate plot
  #Depends on data() & cut_x()
  reactive({
    conditions <- !is.na(input$n_s) & !is.na(input$n_a) & input$n_s > 0 & input$n_a > 2 & input$n_s < input$n_a & input$baserate > 0 & input$baserate < 100
    if(conditions) {
      data %>%
        ggvis(x = ~x, y = ~y) %>%
        layer_points(fill := ~cat, opacity := 0.7, size := 100) %>%
        layer_paths(data = data.frame(x = c(cut_x(), cut_x()), y = c(-4, 4)), stroke := "grey") %>%
        layer_paths(data = data.frame(x = c(-4, 4), y = c(cut_y(), cut_y())), stroke := "grey") %>%
        layer_model_predictions(model = "lm", se = FALSE, stroke := "black", strokeWidth := 2) %>%
        layer_text(text := ~text, data = data.frame(x = c(cut_x() + 0.2, cut_x() + 0.2, -4, -4), 
                                                    y = c(4 - 0.2, -4, 4 - 0.2, -4), 
                                                    text = c("Selected high performers", 
                                                             "Selected low performers", 
                                                             "Not selected high performers", 
                                                             "Not selected low performers"))) %>%
        scale_numeric("x", domain = c(-4, 4)) %>%
        add_axis("x", title = "Total Assessment Score (z)", values = -4:4) %>%
        scale_numeric("y", domain = c(-4, 4)) %>%
        add_axis("y", title = "Job Performance (z)", grid = TRUE, values = -4:4) %>%
        set_options(width = 500, height = 500)
      
    } else data %>% 
      ggvis(x = ~x, y = ~y) %>%
      layer_points(fill := ~cat, opacity := 0.7, size := 100) %>%
      layer_paths(data = data.frame(x = c(0, 0), y = c(-4, 4)), stroke := "grey") %>%
      layer_paths(data = data.frame(x = c(-4, 4), y = c(0, 0)), stroke := "grey") %>%
      layer_text(text := ~text, data = data.frame(x = c(0.2, 0.2, -4, -4), 
                                                  y = c(4 - 0.2, -4, 4 - 0.2, -4), 
                                                  text = c("Selected high performers", 
                                                           "Selected low performers", 
                                                           "Not selected high performers", 
                                                           "Not selected low performers"))) %>%
      scale_numeric("x", domain = c(-4, 4)) %>%
      add_axis("x", title = "Total Assessment Score (z)", values = -4:4) %>%
      scale_numeric("y", domain = c(-4, 4)) %>%
      add_axis("y", title = "Job Performance (z)", grid = TRUE, values = -4:4) %>%
      set_options(width = 500, height = 500)
    
    
    
  }) %>% bind_shiny("my_plot", controls_id = "pred_val")
  
  
  
  ##############################
  #OPTIONAL UTILITY CALCULATION#
  ##############################
  
  #Utility calculation 
  #Depends on input$n_a, input$n_s, input$salary, input$years, input$cost
  output$utility <- renderText({
    conditions <- !is.na(input$n_s) & !is.na(input$n_a) & !is.na(input$salary) & !is.na(input$years) & !is.na(input$cost) & 
      input$n_s > 0 & input$n_a > 2 & input$n_s < input$n_a & input$salary > 0 & input$years > 0 & input$cost > 0
    
    if(conditions) {
      r <- pred_val()
      
      #sr: selection ratio
      sr <- pnorm(cut_x(), lower.tail = FALSE)
      
      #SDy: standard deviation of utility for the role
      SDy <- input$salary * 0.4 #general rule from Schmidt, Hunter & Pearlman (1982) (page 368 Mabon 2006)
      
      #phi: ordinate (density) in normal distribution at cut-off
      phi <- dnorm(cut_x()) 
      
      #zs: mean of test score in the selected group
      zs <- phi / sr
      
      #Utility
      U <- input$n_s * input$years * r * SDy * zs - input$n_a * input$cost
      
      #Error of estimate
      err <- sqrt(1 - r^2) * SDy * input$years 
      
      
      #function to make spaces in large numbers
      readable <- function(x) {
        return(prettyNum(round(x, 0), big.mark=",", scientific=FALSE))
      }
      
      p <- paste("The return on investment will be approximately ", readable(U), " EUR over ", input$years,  " years 
              (95 % confidence interval: ", readable(U-1.96*err), " - ", readable(U+1.96*err), " EUR). 
              The calculation is based on ", input$n_s, " new hires, expected to deliver ", readable(input$years*r*zs*SDy), 
                 " EUR more than the average employee each. 
              The total cost of assessing all applicants (", prettyNum(input$n_a*input$cost, big.mark=",", scientific=F), " EUR) is then subtracted
              from the total gain in productivity.", sep="")
    } else p <- paste("Please enter correct values in all of the fields.")
    
    
  })
  
})

