#UI

#For comparisons of results:
#http://www.hr-software.net/cgi/TheoreticalExpectancy.cgi
#For more info about calculations:
#http://dx.doi.org/10.1108/eb029052


rm(list=ls())
library(ggvis)
library(shiny)

shinyUI(
  fluidPage(
    
    #Enable MathJax
    tags$head( tags$script(src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML-full", 
                           type = 'text/javascript'),
               tags$script("MathJax.Hub.Config({tex2jax: {inlineMath: [['$$','$$'], ['\\\\(','\\\\)']]}});", 
                           type='text/x-mathjax-config'),
               includeScript("google_analytics.js")
    ),
    
    #App title
    titlePanel(HTML("Selection methods: Precision and Return On Investment")),
    br(),
    
    #Sidebar with controls
    fluidRow(
      column(
        width = 4,
        
        wellPanel(
          
          h5("Please enter:"),
          
          sliderInput(inputId = "pred_val", 
                      label = "Predictive validity (r):", 
                      min = -1, 
                      max = 1, 
                      value = 0.5,
                      step = 0.05),
          
          br(),
          sliderInput("baserate",
                      label=HTML("Base rate (% successful applicants):"),
                      min=1,
                      max=99,
                      value=50,
                      step=1),
          br(),
          numericInput("n_s",
                       label=HTML("Number of positions to fill:"),
                       value = 10,
                       max = 1000,
                       min = 1),
          br(),
          numericInput("n_a",
                       label=HTML("Number of applicants:"),
                       value = 50, 
                       min = 3,
                       max = 10000),
          br(),
          checkboxInput("calc_roi", h5("Calculate Return On Investment")),
          conditionalPanel(
            condition = "input.calc_roi == true",
            br(),
            numericInput("salary", 
                         label = "Yearly salary (EUR):",
                         value = 40000,
                         min = 10000,
                         max = 1000000),
            numericInput("years",
                         label = "Expected tenure (years):",
                         value = 3,
                         min = 1,
                         max = 20),
            numericInput("cost",
                         label = "Est. cost per assessment (EUR):",
                         value = 150,
                         min = 1,
                         max = 5000)
          ),
          #submitButton(),
          br(),
          p(HTML("<font color=\"gray\" size=\"2\">The app is written in 
                 <a href=\"http://cran.r-project.org/\" target=\"_blank\">R</a> using 
                 <a href=\"http://shiny.rstudio.com/\" target=\"_blank\">Shiny</a>. 
                 Created by
                 <a href=\"https://www.linkedin.com/pub/morgan-str%C3%B6m/21/844/556\" target=\"_blank\">
                 Morgan Str&ouml;m</a>. Get in touch! 
                 <a href=\"mailto:morgan.e.strom@gmail.com\">morgan.e.strom@gmail.com</a></font>"))
          
        )
      ), 
      
      #Main panel with plot and text
      column(
        width = 7,
        fluidRow(
          #Title
          h4(textOutput("title")),
          br(),
          #Text about probability of good hire
          wellPanel(
            textOutput("prob"),
            #Display utility calculation if box is checked
            conditionalPanel(
              condition = "input.calc_roi == true",
              br(),
              textOutput("utility")
            )
          )  
        ),
        br(),
        
        fluidRow(
          #Display ggvis plot
          ggvisOutput("my_plot"),
          br()
        )
      )
    ),
    
    #Display some instructions and a description of probability calculations
    fluidRow(
      column(
        width = 10,
        offset = 1,
        br(),
        br(), 
        HTML("For guidance in choosing values of predictive validity for a range of selection methods, 
             please turn to articles by e.g. Schmidt and Hunter (1998) [1], Dilchert and Ones (2009) [2] and 
             Kuncel, Klieger, Connelly and Ones (2013) [3]. To set the base rate, make your judgement based on 
             the complexity of the job and the average performance in the applicant pool."),
        br(),
        br(),
        strong("Precision"),
        p(),
        HTML("The probability of a successful hire is here defined as the precision, 
             or <a href='https://en.wikipedia.org/wiki/Positive_and_negative_predictive_values' target='_blank'>positive predictive value</a>, 
             of the selection method:"),
        br(),
        br(),
        HTML("$$ \\mathrm{Precision} = P ( A \\mid B ) = \\frac{P ( A , B )}{P ( B )} 
             \\approx \\frac{ \\mathrm{ \\color{green}{\\sum Green} } }{ \\mathrm{ \\color{green}{\\sum Green} } + \\mathrm{ \\color{red}{\\sum Red} } }$$"),
        br(),
        HTML("Where \\(A\\) is the event that a given candidate will perform well on the job (i.e. ends up above the horizontal line of the graph) and 
             \\(B\\) is the event that a given candidate performs well on the assessment (i.e ends up to the right of the vertical line of the graph).
             The conditional probability \\(P(A \\mid B)\\) can be calculated using properties of the 
             <a href='https://en.wikipedia.org/wiki/Multivariate_normal_distribution#Bivariate_case'  target='_blank'>bivariate normal distribution function</a>. 
             If you want to check the results, you should get similar results by dividing the number of green points with the 
             total number of green and red points from the simulation above."),
        br(),
        br(), 
        HTML("The formula used here is equivalent to the one presented by Taylor and Russell in 
             their 1939 paper [4], where they calculated tables to show the practical effectiveness of using tests in selection."),
        br()
        )
        ),
    
    #Conditional panel with description of ROI calculation
    fluidRow(
      column(
        width = 10,
        offset = 1,
        conditionalPanel(
          condition = "input.calc_roi == true",
          br(),
          strong("Return On Investment"),
          p(),
          HTML("The Return On Investment (ROI) can be approximated using a simple linear regression model 
               (see [5], pages 15-22 for a closer explanation). 
               By multiplying the individual utility by the number of selectees and the expected tenure, 
               then subtracting the total cost of the assessments, the total utility is estimated by:"),
          br(),
          br(),
          HTML("$$\\Delta U = t N_s r_{xy} SD_y \\bar z_x - N_a C_a$$"),
          br(),
          HTML("This model is often referred to as the Brogden-Cronbach-Gleser model of utility [6] and it can also be expressed:"),
          br(),
          br(),
          HTML("$$\\Delta U = t N_a r_{xy} SD_y \\phi - N_a C_a$$"),
          br(),
          HTML("<ul>
               <li> \\(\\Delta U\\): the total utility of using the selection method.</li>
               <li> \\(t\\): the number of years each selectee is expected to stay in the position.</li>
               <li> \\(N_a\\): the number of applicants.</li>
               <li> \\(N_s\\): the number of positions to fill (also the number of selectees).</li>
               <li> \\(r_{xy}\\): the validity coefficient (Pearson correlation) between assessment score and job performance.</li>
               <li> \\(SD_y\\): the standard deviation of individual utility (rule of thumb: 40 % of the yearly salary).</li>
               <li> \\(\\bar z_x\\): the average assessment score \\(x\\) of the selectees [7]. Assuming \\(X \\sim N(0, 1)\\), 
                    this can be calculated by \\(\\bar z_x = \\phi / p\\)</li>
               <li> \\(\\phi\\), in some places denoted \\(\\lambda (p)\\): the ordinate of the normal probability distribution associated with \\(p\\).</li>
               <li> \\(p\\): the selection ratio \\(N_s / N_a\\).</li>
               <li> \\(C_a\\): the assessment cost (per applicant).</li>
               </ul>"),
          br(),
          HTML("To estimate the confidence intervals for the utility calculation, the following formula for standard errors was used:"),
          br(),
          br(),
          HTML("$$SE_e = t SD_y \\sqrt{1 - r^2}$$"),
          br(),
          HTML("Click <a href=\"roi_test.html\" target=\"_blank\">here</a> to view the R code underlying the calculations 
               and Monte Carlo simulations of the estimates."),
          br()
          )
          )
      
        ),
    
    #Note about assumptions in the formulas
    fluidRow(
      column(
        width = 10,
        offset = 1,
        br(),
        strong("Assumptions"),
        p(),
        HTML("The visualisation and the utility estimations are based on the assumption of a bivariate normal probability distribution. 
             That is, both the individual attributes measured in the selection process and the construct of job performance are assumed to be normally
             distributed in the population. This assumption is warranted by extensive research on measurements of
             general mental ability (GMA), personality, as well as the broad concept of job performance [8]."),
        br(),
        br(),
        HTML("This doesn't mean, however, that normality is
             observed in all applied settings. Contextual factors, such as the organisation's 
             standards for job performance and features of the applicant group, can influence the 
             observed distributions. It has been shown that the utility may be overestimated in 
             settings with low performing applicants and low organizational standards for job performance, 
             and underestimated in settings with high performing applicants and high organizational standards 
             for job performance. The utility calculations are, however, quite robust to departures from normality [9]."),
        br()
        
        )
        ),
    
    #References
    fluidRow(
      column(
        width = 8,
        offset = 1,
        
        br(),
        strong("References"),
        p(),
        p(HTML("[1] Schmidt, F. L. & Hunter, J. E. (1998). The validity and utility of selection methods in personnel psychology: 
               Practical and theoretical implications from 85 years of research findings. Psychological Bulletin, 124(2), 262-274.")),
        p(HTML("[2] Dilchert, S. & Ones, D. S. (2009). Assessment Center dimensions: Individual differences correlates and meta-analytic incremental validity.
    International Journal of Selection and Assessment, 17(3), 254-270.")),
        p(HTML("[3] Kuncel, N. R., Klieger, D. M., Connelly, B. S., & Ones, D. S. (2013). Mechanical Versus Clinical Data Combination in Selection and Admissions Decisions: 
          A Meta-Analysis. Journal of Applied Psychology.")),
        p(HTML("[4] Taylor, H. C. & Russell, J. T. (1939). The relationship of validity coefficients to the practical effectiveness of tests in selection: Discussion and tables. 
          Journal of Applied Psychology, 23, 565-578.")),
        p(HTML("[5] Russell, C. J. (2011). Evidence-based human resources management. San Diego, CA: Cognella/University Readers. 
          Downloaded from  <a href=\"http://www.ou.edu/russell/4153/CH3.pdf\" target=\"_blank\">http://www.ou.edu/russell/4153/CH3.pdf</a>")),
        p(HTML("[6] Schmidt, F. L., Hunter, J. E., McKenzie, R. C. & Muldrow, T. W. (1979). Impact of valid selection procedures on work-force productivity.
    Journal of Applied Psychology, 64(6), 609-626.")),
        p(HTML("[7] Naylor, J. C. & Shine, L. C. (1965). A table for determining the increase in mean criterion score obtained by using a selection device. 
               Journal of Industrial Psychology, 3, 33-42.")),
        p(HTML("[8] Beck, J. W., Beatty, A. S., & Sackett, P. S. (2013). On the distribution of job performance: the role of measurement characteristics 
          in observed departures from normality. Personnel Psychology.")),
        p(HTML("[9] Anderson, S. M., & Muchinsky, P. M. (1991). An Examination of the Robustness of the General Utility Function. 
          Educational and Psychological Measurement, 51, 49-65.")),
        br()
        
        )  
    )
      )
    )

