library(shiny)
library(ggplot2) # ggplot
library(nortest) # Anderson - Darling test for normality
options(warn = -1) # Hide warning messages
ui <- navbarPage(title = "Random Data Generator",
    navbarMenu("Normal Distribution",  
    tabPanel(title = "Histogram",
    plotOutput("norm"),
    actionButton("renorm", "Resample"),
    br(),
    sliderInput(inputId = "bin", 
    label = "Choose Number of Bins", 
             value = 20, min = 1, max = 30),
    h5("Wikipedia: In probability theory, the normal (or Gaussian) distribution"),
    h5("is a very common continuous probability distribution."),
    h5("A random variable with a Gaussian distribution is also called"),
    h5("a normal deviate or bell curve."),
    h5("Measures of Central Tendency and Spread of a Random Sample"),
    verbatimTextOutput("gstats"),
    h5("Tests for normality: If p > 0.05, do not reject the null hypothesis (H0) of normality."),
    verbatimTextOutput("shapiro")),
                            
    tabPanel(title = "Boxplot",
    plotOutput("box"),
    br(),
    h5("Wikipedia: A box plot or boxplot is a non-parametric method for"),
    h5("graphically depicting groups of numerical data through their quartiles."),
    h5("Box plots have lines extending vertically from the boxes (whiskers)"),
    h5("indicating variability outside the upper and lower quartiles,"),
    h5("hence the terms box-and-whisker plot and box-and-whisker diagram."),
    h5("Outliers, defined as any values beyound 1.5 times the interquartile"),
    h5("range are plotted as individual points."),
    br(),
    h5("Tukey's Five-Number Summary Statistics"),
    br(),
    # Output: Verbatim text for data summary ----
    verbatimTextOutput("summary")
    )
    ),
                 
    navbarMenu("Other Distributions",
    tabPanel(title = "Uniform",
    plotOutput("unif"),
    actionButton("reunif", "Resample"),
    br(),
    sliderInput(inputId = "binu", 
        label = "Choose Number of Bins", 
        value = 20, min = 1, max = 30),
    br(),
    h5("Wikipedia: In probability theory and statistics, the continuous"),
    h5("uniform distribution or rectangular distribution is a"),
    h5("family of symmetric probability distributions such that"),
    h5("for each member of the family, all intervals of the same"),
    h5("length on the distribution's support are equally probable.")
    ),
                            
    tabPanel(title = "Chi Squared",
    plotOutput("chisq"),
    actionButton("rechisq", "Resample"),
    br(),
    br(),
    sliderInput(inputId = "binch", 
        label = "Choose Number of Bins", 
        value = 20, min = 1, max = 30),
    br(),
    sliderInput(inputId = "degfree", 
        label = "Choose Degrees of Freedom then Resample", 
        value = 2, min = 1, max = 30),
    br(),
    h5("Wikipedia: In probability theory and statistics, the"),
    h5("chi-squared distribution [also chi-square or χ2-distribution]"),
    h5("with k degrees of freedom is the distribution of a sum of"),
    h5("the squares of k independent standard normal random variables.")       
    ) ),
                 
   navbarMenu("Mathematicians",  
    tabPanel(title = "JCF Gauss",
    br(),
    h4("Johann Carl Friedrich Gauss, German, 1977 - 1855"),
    tabPanel("",tags$img(src = "Gauss.jpg", width = "459px",height = "550px")),
    h5("He is ranked among history's most influential mathematicians"),
    h5("for significant contributions to many fields, including number theory,"),
    h5("algebra, statistics, analysis, differential geometry, geodesy, geophysics,"),
    h5("mechanics, electrostatics, magnetic fields, astronomy, matrix theory, and optics."),
    h5("His name is synonymous with the 'Normal' distribution even though Abraham de Moivre"),
    h5("first described it in 1733. When only eight, he purportedly figured out how to add up"),
    h5("all the numbers from 1 to 100 with the simple equation: 50 x 101 = 5050.")
    ),
   
   tabPanel(title = "JW Tukey",
    h4("John Wilder Tukey, American, 1915 – 2000"),
    br(),
    tabPanel("",tags$img(src = "John_Tukey.jpg", width = "417px",height = "500px")),
    h5("Considered a great data analyst, he coined the terms 'bits'"),
    h5("and 'software'. Founding chairman of the Princeton statistics"),
    h5("department in 1965, he is best known for development of the FFT"),
    h5("'Fast Fourier Transform' algorithm and exploratory data techniques,"),
    h5("including the box plot in 1977.")
    ),

    tabPanel(title = "K Pearson",
    h4("Karl Pearson, English, 1857 – 1936"),
    br(),
    tabPanel("",tags$img(src = "Pearson.jpg", width = "417px",height = "500px")),
    h5("He founded the world's first university statistics department at University"),
    h5("College London in 1911 and became the protégé of Charles Darwin's cousin"),
    h5("Francis Galton, the creator of regression analysis."),
    h5("Among the many statisical tools for which he is known are Principal"),
    h5("Component Analysis, Pearson's Corrleation Coefficient 'r', Pearson's"),
    h5("Chi Squared test,Phi Coefficient, and last but not least: Histogram.")
    )
))

server <- function(input, output) {
    rv <- reactiveValues(
        norm = rnorm(1000), 
        unif = runif(1000),
        chisq = rchisq(1000, 2))
    
 observeEvent(input$renorm, { rv$norm <- rnorm(1000) })
 observeEvent(input$reunif, { rv$unif <- runif(1000) })
 observeEvent(input$rechisq, { rv$chisq <- rchisq(1000,input$degfree) })
    
    output$norm <- renderPlot({
        ggplot(as.data.frame(rv$norm), aes(rv$norm)) + 
            geom_histogram(aes(y=..density..), bins=input$bin,col = "blue",fill = "lightblue") +
            geom_density() +
            geom_vline(aes(xintercept=mean(rv$norm)), color="red", cex=2,
                       linetype="dashed")+
            ggtitle(paste("Histogram: n = 1000, Red Line = Mean, Bins = ",bins=input$bin)) +
            labs(x = "Normal 'Gaussian' Distribution", y = "Frequency")+ 
            theme_bw()+
            theme(plot.title = element_text(size = 20, face = "bold"))
        
    })
    
    # Generate normal stats of the dataset
    output$gstats <- renderPrint({
        vr <- var(rv$norm)
        options(digits = 2)
        cat("Mean(x-bar) =",mean(rv$norm),"; Variance(s squared) =",vr,"; Standard Deviation(s) =",sqrt(vr))
    })
    
    output$box <- renderPlot({
        ggplot(as.data.frame(rv$norm),aes(rv$norm,rv$norm)) +
            geom_boxplot(outlier.color = "blue") +
            geom_hline(aes(yintercept=median(rv$norm)), color="red", cex=2,
                       linetype= 1)+
            ggtitle("Boxplot: n=1000, Red Line = Median, Blue Circles = Outliers") +
            labs(x="Normal Distribution", y = "Random Data") +
            theme_bw() +
            theme(axis.ticks = element_blank(),
                  axis.text.x = element_blank()) +
            theme(plot.title = element_text(size = 20, face = "bold"))
    }) 
    
    # Generate summary stats of the dataset 
    output$summary <- renderPrint({
        Tukey <- summary(rv$norm)
        options(digits = 3)
        cat("Min =",Tukey[1],"; Q1 =",Tukey[2],
            "; Q2 (Median) =",Tukey[3],"; Q3 =",Tukey[5],
            "; Max = ",Tukey[6],
            "; IQR = Q3 - Q1 =", IQR(rv$norm))
    })
    
    output$shapiro <- renderPrint({
        shap <- shapiro.test(rv$norm)
        shap<- unlist(shap)
        shap <- as.numeric(shap)
        ad <- ad.test(rv$norm)
        ad2 <- as.numeric(ad)
        cat("Shapiro-Wilk test: P value = ",signif(shap[2],4), "   Anderson-Darling test: P value = ", signif(ad2[2],4))
    })
    
    output$unif <- renderPlot({
        ggplot(as.data.frame(rv$unif), aes(rv$unif)) + 
            geom_histogram(bins = input$binu, col = "blue",fill = "lightblue") +
            ggtitle(paste("Histogram: n = 1000, Red Line = Equally Probable Frequency, Bins =",input$binu)) +
            labs(x = "Uniform Distribution", y = "Frequency")+ 
            geom_hline(aes(yintercept= 1000/input$binu), color="red", cex=2,
                       linetype="dashed")+
            theme_bw()+
            theme(plot.title = element_text(size = 20, face = "bold"))  
    })
    
    output$chisq <- renderPlot({
        ggplot(as.data.frame(rv$chisq), aes(rv$chisq)) + 
            geom_histogram(bins = input$binch, col = "blue",fill = "lightblue") +
            ggtitle(paste("Histogram: n = 1000, Bins =  ",input$binch, ", Degrees of Freedom =",input$degfree)) +
            labs(x = "Chi Squared Distribution ", y = "Probability") + 
            theme_bw()+
            theme(plot.title = element_text(size = 20, face = "bold"))
    })
}

shinyApp(server = server, ui = ui)
