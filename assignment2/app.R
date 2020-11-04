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
library(RSpectra)
library('plot.matrix')
library('psych')
library(ggplot2)
library(shinycssloaders)


# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("The one-stop PCA Center"),
    
    sidebarLayout(
    sidebarPanel(
        fileInput('datafile', 'Choose CSV file',
                  accept=c('csv', 'comma-separated-values','.csv')),
        
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(), 
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        
        sliderInput("averageFaceSlider", "Pick an average face to choose from:",
                    min = 1, max = 40, value = 1
        )
    ),
    mainPanel(
    #     # Application title
        titlePanel("Assignment 2 - EigenFaces"),
        h3('Steps for creating Eigenfaces:'),
        h5('1. Prepare the data with each row representing an image.'),
        h5('2. Subtract the mean image from the data.'),
        h5('3. Calculate the eigenvectors and eigenvalues of the covariance matrix.'),
        h5('4. Find the optimal transformation matrix by selecting the principal components (eigenvectors with largest eigenvalues).'),
        h5('5. Project the centered data into the subspace.'),
        h5('6. New faces can then be projected into the linear subspace and the nearest neighbour to a set of projected training images is found.'),
        
        plotOutput("images") %>% withSpinner(color="#0dc5c1"),
        plotOutput("averageFaces") %>% withSpinner(color="#0dc5c1")
    ))
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # to allow for a large dataset of up to 30mb to be uploaded to the server
    options(shiny.maxRequestSize=30*1024^2)
    
    dataframe<-reactive({
        if (is.null(input$datafile))
            return(NULL)                
        data<-read.csv(input$datafile$datapath)
    })

    # if(!is.null(df_face)){
    plt_img <- function(x){ image(x, col=grey(seq(0, 1, length=256)))}
        
    output$images <- renderPlot({
        par(mfrow=c(1,2))
        par(mar=c(0.1,0.1,0.1,0.1))
        
        image <- matrix(as.numeric(dataframe()[1, ]), nrow=64, byrow=T)
        plt_img(image)
        
        rotate_image <- t(apply(matrix(as.numeric(dataframe()[1, ]), nrow=64, byrow=T), 2, rev))
        plt_img(rotate_image)
    })
    

    # getting training data!
    # newdata<-NULL
    # # Rotate every image and save to a new file for easy display in R
    # for(i in 1:nrow(df_face_face))
    # {
    #     # Rotated Image 90 degree
    #     c <- as.numeric((apply(matrix(as.numeric(dataframe()[i, ]), nrow=64, byrow=T), 2, rev)))
    #     # Vector containing the image
    #     newdata <- rbind(newdata,c)
    # }
    # 
    # df_train=as.data.frame(newdata)
    # df_train_as_matrix <- data.matrix(df_train)

    # Step 2: computing the mean average of the faces:
    output$averageFaces <- renderPlot({
            par(mfrow=c(1,1))
            par(mar=c(0.1,0.1,0.1,0.1))

            AV1=colMeans(data.matrix(dataframe()[(10*(input$averageFaceSlider-1))+1:10*(input$averageFaceSlider-1)+10,]))
            face_1 <- t(apply(matrix(AV1,nrow=64,byrow=T), 2, rev))
            plt_img(face_1)
            
            # 1:10
            # 11:20
            # 21:30
            # ...
            # 381:390
            # 391:400
    })
#     
#     # getting the average face of the training set
#     df_train <- read.csv("training_faces.csv")
#     D <- data.matrix(df_train)
#     
#     average_face=colMeans(df_train)
#     AVF=matrix(average_face, nrow=1, byrow=T)
#     plt_img(matrix(average_face,nrow=64,byrow=T))
#     
#     #-------------------------------------------------------------------------------
#     # Perform PCA on the data
#     
#     # Step 1: scale data
#     # Scale as follows: mean equal to 0, stdv equal to 1
#     D <- scale(D)
#     
#     # Step 2: calculate covariance matrix
#     A <- cov(D)
#     A_ <- t(D) %*% D / (nrow(D)-1)
#     
#     # Step 3: calculate eigenvalues and eigenvectors
#     
#     # Calculate the largest 40 eigenvalues and corresponding eigenvectors
#     eigs <- eigs(A, 40, which = "LM")
#     # Eigenvalues
#     eigenvalues <- eigs$values
#     # Eigenvectors (also called loadings or "rotation" in R prcomp function: i.e. prcomp(A)$rotation)
#     eigenvectors <- eigs$vectors
#     
#     par(mfrow=c(1,1))
#     par(mar=c(2.5,2.5,2.5,2.5))
#     y=eigenvalues[1:40]
#     # First 40 eigenvalues dominate
#     plot(1:40, y, type="o", log = "y", main="Magnitude of the 40 biggest eigenvalues", xlab="Eigenvalue #", ylab="Magnitude")
#     
#     # New variables (the principal components, also called scores, and called x in R prcomp function: i.e. prcomp(A)$x)
#     D_new <- D %*% eigenvectors
#     
#     # Plot of first 6 eigenfaces
#     
#     par(mfrow=c(3,2))
#     par(mar=c(0.2,0.2,0.2,0.2))
#     for (i in 1:6){
#         plt_img(matrix(as.numeric(eigenvectors[, i]),nrow=64,byrow=T))
#     }
#     
#     par(mfrow=c(2,2))
#     par(mar=c(2,2,2,2))
#     
#     # projection of 1st photo into eigen space
#     # reduce the dimension from 4096 to 40
#     PF1 <- data.matrix(df_train[1,]) %*% eigenvectors
#     barplot(PF1,main="projection coefficients in eigen space", col="blue",ylim = c(-40,10))
#     legend("topright", legend = "1st photo")
#     
#     # projection of 11th photo into eigen space
#     # reduce the dimension from 4096 to 40
#     PF2 <- data.matrix(df_train[2,]) %*% eigenvectors
#     barplot(PF2,main="projection coefficients in eigen space", col="blue",ylim = c(-40,10))
#     legend("topright", legend = "2nd photo")
#     
#     # projection of 1st photo into eigen space
#     # reduce the dimension from 4096 to 40
#     PF3 <- data.matrix(df_train[3,]) %*% eigenvectors
#     barplot(PF3,main="projection coefficients in eigen space", col="blue",ylim = c(-40,10))
#     legend("topright", legend = "3rd photo")
#     
#     # projection of 11th photo into eigen space
#     # reduce the dimension from 4096 to 40
#     PF4 <- data.matrix(df_train[4,]) %*% eigenvectors
#     barplot(PF4,main="projection coefficients in eigen space", col="blue",ylim = c(-40,10))
#     legend("topright", legend = "4th photo")
#     
#     par(mfrow=c(2,2))
#     par(mar=c(2,2,2,2))
#     
#     # projection of 1st photo into eigen space
#     # reduce the dimension from 4096 to 40
#     PF1 <- data.matrix(df_train[1,]) %*% eigenvectors
#     barplot(PF1,main="projection coefficients in eigen space", col="blue",ylim = c(-40,10))
#     legend("topright", legend = "1st photo")
#     
#     # projection of 11th photo into eigen space
#     # reduce the dimension from 4096 to 40
#     PF2 <- data.matrix(df_train[11,]) %*% eigenvectors
#     barplot(PF2,main="projection coefficients in eigen space", col="red",ylim = c(-40,10))
#     legend("topright", legend = "11th photo")
#     
#     # projection of 1st photo into eigen space
#     # reduce the dimension from 4096 to 40
#     PF3 <- data.matrix(df_train[21,]) %*% eigenvectors
#     barplot(PF3,main="projection coefficients in eigen space", col="green",ylim = c(-40,10))
#     legend("topright", legend = "21st photo")
#     
#     # projection of 11th photo into eigen space
#     # reduce the dimension from 4096 to 40
#     PF4 <- data.matrix(df_train[31,]) %*% eigenvectors
#     barplot(PF4,main="projection coefficients in eigen space", col="grey",ylim = c(-40,10))
#     legend("topright", legend = "31st photo")
#     
#     # Every face has different projection on eigenvector space.
#     # We can use these new fewer values for a classification task.
#     par(mfrow=c(2,2))
#     par(mar=c(1,1,1,1))
#     
#     # 1st person 1st photo
#     plt_img(matrix(as.numeric(df_train[1, ]), nrow=64, byrow=T))
#     
#     # 1st person project into eigen space and reconstruct
#     PF1 <- data.matrix(df_train[1,]) %*% eigenvectors
#     RE1 <- PF1 %*% t(eigenvectors)
#     plt_img(matrix(as.numeric(RE1),nrow=64,byrow=T))
#     
#     # 2nd person 1st photo
#     plt_img(matrix(as.numeric(df_train[11, ]), nrow=64, byrow=T))
#     
#     # 2nd persoon project into eigen space and reconstruct
#     PF2 <- data.matrix(df_train[11,]) %*% eigenvectors
#     RE2 <- PF2 %*% t(eigenvectors)
#     plt_img(matrix(as.numeric(RE2),nrow=64,byrow=T))
#     
#     par(mfrow=c(2,2))
#     par(mar=c(1,1,1,1))
#     
#     # 1st person 1st photo
#     plt_img(matrix(as.numeric(df_train[1, ]), nrow=64, byrow=T))
#     
#     # average face
#     average_face=colMeans(df_train)
#     AVF=matrix(average_face,nrow=1,byrow=T)
#     plt_img(matrix(average_face,nrow=64,byrow=T))
#     
#     # project into eigen space and back
#     PF1 <- data.matrix(df_train[1,]) %*% eigenvectors
#     RE1 <- PF1 %*% t(eigenvectors)
#     plt_img(matrix(as.numeric(RE1),nrow=64,byrow=T))
#     
#     # add the average face
#     RE1AVF=RE1+AVF
#     plt_img(matrix(as.numeric(RE1AVF),nrow=64,byrow=T))
#     
#     par(mfrow=c(2,2))
#     par(mar=c(1,1,1,1))
#     
#     # 3rd person 31st photo
#     plt_img(matrix(as.numeric(df_train[31, ]), nrow=64, byrow=T))
#     
#     # average face
#     average_face=colMeans(df_train)
#     AVF=matrix(average_face,nrow=1,byrow=T)
#     plt_img(matrix(average_face,nrow=64,byrow=T))
#     
#     # project into eigen space and back
#     PF1 <- data.matrix(df_train[31,]) %*% eigenvectors
#     RE1 <- PF1 %*% t(eigenvectors)
#     plt_img(matrix(as.numeric(RE1),nrow=64,byrow=T))
#     
#     # add the average face
#     RE1AVF=RE1+AVF
#     plt_img(matrix(as.numeric(RE1AVF),nrow=64,byrow=T))
#     
#     # New photo under test, say, 142nd photo
#     # Transform onto eigen space to find the coefficients
#     PF1 <- data.matrix(df_train[142,]) %*% eigenvectors
#     
#     # Transform all the traning photos onto eigen space and get the coefficients
#     PFall <- data.matrix(df_train) %*% eigenvectors
#     
#     # Find the simple difference and multiplied by itself to avoid negative value
#     test <- matrix(rep(1,400),nrow=400,byrow=T)
#     test_PF1 <- test %*% PF1
#     Diff <- PFall-test_PF1
#     y <- (rowSums(Diff)*rowSums(Diff))
#     
#     # Find the minimum number to match the photo in the files
#     x=c(1:400)
#     newdf=data.frame(cbind(x,y))
#     
#     the_number = newdf$x[newdf$y == min(newdf$y)]
#     
#     par(mfrow=c(1,1))
#     par(mar=c(1,1,1,1))
#     barplot(y,main = "Similarity Plot: 0 = Most Similar")
#     
#     #minimum euclidean distance:
#     plt_img(matrix(as.numeric(df_train[the_number,]), nrow=64, byrow=T))
#     
}

# Run the application
shinyApp(ui = ui, server = server)
