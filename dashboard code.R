# Load required libraries
library(shiny)
library(plotly)
library(readr)
library(dplyr)


#load the data set
setwd("~/assignment2")
diabetes<-read.csv("C:\\Users\\ASUS\\Documents\\assignment2\\diabetes.csv")


# Define UI for application
ui <- fluidPage(
  
  # Include custom CSS to fix the tab set Panel
  tags$head(
    tags$style(HTML("
      .navbar-static-top { 
        position: relative; 
        width: 100%; 
        z-index: 1000; 
      }
      body, .shiny-body {
        background-image:url('https://www.inglobetechnologies.com/wp-content/uploads/2022/07/neuroscience-vr-elearninginside-e1539715238489-915x610.jpg.webp'); /* Specify the path to your image */
        background-size: cover;
        background-repeat: no-repeat;
        background-attachment: fixed; /* Ensures the background image stays fixed while scrolling */
        opacity: 0.9;
      }
      .main-title {
        position:fix;
        text-align: center;
        font-weight: bold;
        color:#00FFFF;
        font-style: italic;
        italic-bold-underline ;
        font-family:sans-serif;
        font-size: 90px; 
      }
      .parag {
        background-color:#B0C4DE; /* Light gray background color for the introduction */
        padding:30px;
        border-radius: 20px;
        width: 150%;
        font-weight: bold;
        text-align: justify;
        font-size: 20px;
        

      }
       .plot-row {
        height: 100%;
      }
      .plot-tile {
        padding-bottom: 1cm;
      }

      .overview-topic {
        display: flex;
        justify-content: space-between;
        align-items: center;
        
      }
      .overview-intro {
        flex: 3;
        text-decoration: underline;

      }
      .content {
        background-color:#ADD8E6; /* Light gray background color for the introduction */
        padding: 30px;
        border-radius: 20px;
        width: 150%;
        font-weight: bold;
        text-align: justify;
        font-size: 22px;
      }
      .team{
        text-align:center;
        background-color:#AFEEEE; /* Light gray background color for the introduction */
        padding: 20px;
        border-radius: 10px;
        width: 100%;
        font-weight: bold;
        font-size:20px;
         font-style: italic;

      }
     
    "))
    
  ),
  
  titlePanel(h1("DIABETES DYNAMIC", class = "main-title")),
  
  #create the  tab set Panel
  navbarPage("",
             tabPanel(h4("Introduction"),
                      column(8, div(class = "parag",
                                    h1("Hello and Welcome to the Student Dashboard", align = "center",tags$hr()),
                                    p("We've stumbled upon a treasure trove in the form of a CSV file named diabetes.csv nestled within GitHub's digital corridors. CSV, shorthand for
                                               comma-separated values, is the trusty vessel for tabular data, neatly organizing information in rows and columnsGiven the nature of the columns, it is plausible that this dataset is used for medical research purposes or for creating data visualizations about diabetes. Utilizing datasets like this can aid in understanding trends, patterns, and correlations within
                                               medical data, which can further research efforts aimed at managing or treating diabetes.We used this data set for regression analysis and some explore diabetes trends with interactive visualizations"),
                                    p("We employed the R programming language, utilizing the plotly package, to construct the dashboard.")
                      )),
             ),
             tabPanel(h4("Visualization"), 
                      class = "background",
                      h2(style = "color:	darkred;-webkit-text-stroke: 1px black;","Explore Diabetes Trends with Interactive Visualizations",tags$hr()),
                      fluidRow(
                        style = "height: 40vh;",
                        class = "plot-row",
                        column(width = 4,
                               div(class = "plot-tile",
                                   plotlyOutput("plot1", height = "100%", width = "100%"))
                        ),
                        column(width = 4,
                               div(class = "plot-tile",
                                   plotlyOutput("plot2", height = "100%", width = "100%"))
                        ),
                        column(width = 4,
                               div(class = "plot-tile",
                                   plotlyOutput("plot3", height = "100%", width = "100%"))
                        )
                      ),
                      fluidRow(
                        style = "height: 40vh;",
                        class = "plot-row",
                        column(width = 4,
                               div(class = "plot-tile",
                                   plotlyOutput("plot4", height = "100%", width = "100%"))
                        ),
                        column(width = 4,
                               div(class = "plot-tile",
                                   plotlyOutput("plot5", height = "100%", width = "100%"))
                        ),
                        column(width = 4,
                               div(class = "plot-tile",
                                   plotlyOutput("plot6", height = "100%", width = "100%"))
                        )
                      )
                      
             ),
             tabPanel(h4("Regression Analysis"),
                      class = "background",
                      h2(style = "color: darkred;-webkit-text-stroke: 1px black;","Logistic Regression Analysis",tags$hr()),
                      
                      # Add your content for regression analysis here
                      fluidRow(
                        column(width = 6,
                               plotlyOutput("regression_plot1", height = "500px")
                        ),
                        column(width = 6,
                               verbatimTextOutput("regression_summary1")
                        )
                      ),
                      tags$hr(),
                      fluidRow(
                        column(width = 6,
                               plotlyOutput("regression_plot2", height = "500px")
                        ),
                        column(width = 6,
                               verbatimTextOutput("regression_summary2")
                        )
                      ),
                      tags$hr(),
                      fluidRow(
                        column(width = 6,
                               plotlyOutput("regression_plot3", height = "500px")
                        ),
                        column(width = 6,
                               verbatimTextOutput  ("regression_summary3")
                        )
                      ), tags$hr(),
                      
                      
                      tags$hr()
             ), tabPanel(h4("Overview"),
                         fluidRow(
                           div(class = "overview-topic",
                               div(class = "overview-intro",
                                   column(8, div(class = "content",
                                                 h1("Overview of the Diabetic Dynamic Dashboard",tags$hr(), align = "center"),
                                                 p("The Diabetic Dynamic Dashboard offers a comprehensive overview of key metrics and trends 
                                            related to diabetes management. Through interactive visualizations and real-time
                                            data updates, users can monitor various aspects such as blood glucose levels,
                                            medication adherence, dietary habits, and physical activity. The dashboard provides 
                                            insights into patterns, fluctuations, and correlations within the data, empowering
                                            healthcare professionals and patients to make informed decisions for effective diabetes 
                                            management. With its dynamic features and user-friendly interface, the dashboard facilitates
                                            personalized care, enhances communication between patients and healthcare providers, and ultimately 
                                            contributes to better health outcomes for individuals living with diabetes."),
                                                 
                                   )),
                               ),
                               column(3, div(class = "team",
                                             h3("Team Members in Our Group",tags$hr()),
                                             p("1) Y.M.L.Kavindya - D/ADC/23/0042"),
                                             p("2) L.A.P.Thasanya - D/ADC/23/0048"),
                                             p("3) L.D.S.L.Renuja - D/ADC/23/0007")
                                             
                               ))
                           )
                         )
             )
  )
)


# Define server logic required to draw the plots
server <- function(input, output){
  
  # Scatter plot for Age vs. Glucose with color by class variable
  output$plot2 <- renderPlotly({
    plot_ly(data = diabetes, x = ~Age..yr., y = ~PlGluConc_2H, type = "scatter", mode = "markers",
            color = ~factor(Class.variable), colors = c("#FF4500", "#00FF00"),
            marker = list(size = 10, opacity = 0.7)) %>%
      layout(title = "Cholesterol over Ages",
             xaxis = list(title = "Age..yr."),
             yaxis = list(title = "PlGluConc_2H"),
             legend = list(title = "Diabetes Outcome", labels = c("No", "Yes")),
             paper_bgcolor = "rgba(255, 255, 255, 0.5)", # Adjust background color with opacity
             plot_bgcolor = "rgba(255, 255, 255, 1.0)") # Adjust plot area background color with opacity
  })
  
  
  # 3D scatter plot for Glucose, Insulin, and BMI with custom colors
  output$plot1 <- renderPlotly({
    plot_ly(data = diabetes, x = ~PlGluConc_2H, y = ~Insulin_2H_muU_ml, z = ~BMI_kg_m2, 
            type = "scatter3d", mode = "markers", 
            marker = list(size = 5, opacity = 0.7, color = ifelse(diabetes$Outcome == "Diabetic", "red", "blue"))) %>%
      layout(title = "3D Scatter Plot",
             scene = list(xaxis = list(title = "PlGluConc_2H"),
                          yaxis = list(title = "Insulin_2H_muU_ml"),
                          zaxis = list(title = "BMI_kg_m2")),
             paper_bgcolor = "rgba(255, 255, 255, 0.5)", # Adjust background color with opacity
             plot_bgcolor = "rgba(255, 255, 255, 1.0)") # Adjust plot area background color with opacity
  })
  
  # Histogram for Blood Pressure Distribution with custom colors
  output$plot3 <- renderPlotly({
    plot_ly(data = diabetes, x = ~DBP_mmHg, type = "histogram",
            marker = list(color = "magenta")) %>%
      layout(title = "Blood Pressure Distribution",
             xaxis = list(title = "Blood Pressure"),
             yaxis = list(title = "Frequency"),
             paper_bgcolor = "rgba(255, 255, 255, 0.5)", # Adjust background color with opacity
             plot_bgcolor = "rgba(255, 255, 255, 1.0)") # Adjust plot area background color with opacity
  })
  
  # Bar chart for Age distribution with custom colors
  output$plot4 <- renderPlotly({
    counts <- table(diabetes$Age..yr.)
    
    # Define custom colors for the bars
    custom_colors <- c("#1f77b4", "#ff7f0e", "#00FF00", "#d62728", "#9467bd", "#FFD700", "#e377c2", "#9400D3", "#8B0000", "#17becf")
    
    plot_ly(x = names(counts), y = counts, type = "bar", marker = list(color = custom_colors)) %>%
      layout(title = "Age Distribution",
             xaxis = list(title = "Age"),
             yaxis = list(title = "Frequency"),
             paper_bgcolor = "rgba(255, 255, 255, 0.5)", # Adjust background color with opacity
             plot_bgcolor = "rgba(255, 255, 255, 1.0)") # Adjust plot area background color with opacity
  })
  # Define common color palette
  common_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
  
  # Render bar plot
  output$plot5 <- renderPlotly({
    # Calculate counts of each variable
    counts <- table(diabetes$Class.variable)
    
    # Create bar plot using Plotly with common colors
    plot_ly(x = names(counts), y = counts, type = "bar", marker = list(color = common_colors)) %>%
      layout(title = "Class.variable Distribution",
             xaxis = list(title = "Class.variable"),
             yaxis = list(title = "Count"))
  })
  
  # Render 2D scatter plot
  output$plot6 <- renderPlotly({
    # Create 2D scatter plot using Plotly with common colors
    plot_ly(data = diabetes, x = ~PlGluConc_2H, y = ~Insulin_2H_muU_ml, type = "scatter", mode = "markers", marker = list(color = common_colors[4])) %>%
      layout(title = "2D Scatter Plot",
             xaxis = list(title = "PlGluConc_2H"),
             yaxis = list(title = "Insulin_2H_muU_ml"))
  })
  
  
  
  # Model 1 - Linear Regression with Insulin_2H_muU_ml and TSF_thickness_mm
    output$regression_plot1 <- renderPlotly({
      ggplot(diabetes, aes(x = Insulin_2H_muU_ml, y = TSF_thickness_mm )) + 
        geom_point() + 
        geom_smooth(method = "lm", color = "red")
    })
    
  
  output$regression_summary1  <- renderPrint({
    lm1 <- lm(Insulin_2H_muU_ml ~  TSF_thickness_mm , data = diabetes)
    summary(lm1)
    
  })
  # Model 2 - Linear Regression with TSF_thickness_mm and DPF
  # Model 2: 
  output$regression_plot2 <- renderPlotly({
    ggplot(diabetes, aes(x = TSF_thickness_mm   , y = DPF )) + 
      geom_point() + 
      geom_smooth(method = "lm", color = "red")
  })
    output$regression_summary2<- renderPrint({
      lm2 <- lm(TSF_thickness_mm    ~  DPF, data = diabetes)
      summary(lm2)
    })
  # Model 3 - Linear Regression with Temperature and Rain
  output$regression_plot3<- renderPlotly({
    # Fit logistic regression model
    
    model_glm <- glm(  Class.variable ~ DPF  , data = diabetes, family = "binomial")
    
    diabetes %>%
      mutate(chd = ifelse(Class.variable == "1", 1, 0)) %>%
      ggplot(aes(PlGluConc_2H, chd)) +
      geom_point(alpha = .25) +
      geom_smooth(method = "glm",method.args = list(family = "binomial")) +
      ggtitle("Logistic regression model fit") +
      xlab("Cholesterol") +
      ylab("Probability of CHD")
})
  # Render summary of the logistic regression model
  output$regression_summary3 <- renderPrint({
    # Fit logistic regression model
    model_glm <- glm(Class.variable ~ DPF, data = diabetes, family = "binomial")
    
    # Print summary
    summary(model_glm)
  })
  
  
  
  
}
# Run the application
shinyApp(ui = ui, server = server)