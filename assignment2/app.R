#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Assignment 2 - EigenFaces"),
    h3('Steps for creating Eigenfaces:'),
    h5('1. Prepare the data with each row representing an image.'),
    h5('2. Subtract the mean image from the data.'),
    h5('3. Calculate the eigenvectors and eigenvalues of the covariance matrix.'),
    h5('4. Find the optimal transformation matrix by selecting the principal components (eigenvectors with largest eigenvalues).'),
    h5('5. Project the centered data into the subspace.'),
    h5('6. New faces can then be projected into the linear subspace and the nearest neighbour to a set of projected training images is found.'),
    
    DT::dataTableOutput("mytable")
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    df <- read.csv("face_data.csv")
    
    output$mytable = DT::renderDataTable({
        DT::datatable(head(df), options = list(columnDefs = list(list(targets = 5))))
    }) 
    
    plt_img <- function(x){ image(x, col=grey(seq(0, 1, length=256)))}
    
    par(mfrow=c(1,2))
    par(mar=c(0.1,0.1,0.1,0.1))
    # Display first image vector
    b <- matrix(as.numeric(df[1, ]), nrow=64, byrow=T)
    plt_img(b)
    
    c <- t(apply(matrix(as.numeric(df[1, ]), nrow=64, byrow=T), 2, rev))
    plt_img(c)

}

# Run the application 
shinyApp(ui = ui, server = server)
