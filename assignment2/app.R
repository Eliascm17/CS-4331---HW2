library(shiny)
library(DT)
library(RSpectra)
library('plot.matrix')
library('psych')
library(ggplot2)
library(shinycssloaders)
library(knitr)
library(shinydashboard)

body <- dashboardBody(fluidRow(
    tabBox(
        title = NULL,
        width = 12,
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1",
        height = "250px",
        tabPanel(
            "Data Uploader",
            fileInput(
                'datafile',
                'Choose CSV file',
                accept = c('csv', 'comma-separated-values', '.csv')
            ),
            h5('*This program assumes that the data is in the shape of 4096x40', style =
                   "color:red"),
        ),
        tabPanel(
            "EigenFaces",
            titlePanel("Assignment 2 - EigenFaces"),
            h3('Steps for creating Eigenfaces:'),
            h5('1. Prepare the data with each row representing an image.'),
            h5('2. Subtract the mean image from the data.'),
            h5(
                '3. Calculate the eigenvectors and eigenvalues of the covariance matrix.'
            ),
            h5(
                '4. Find the optimal transformation matrix by selecting the principal components (eigenvectors with largest eigenvalues).'
            ),
            h5('5. Project the centered data into the subspace.'),
            h5(
                '6. New faces can then be projected into the linear subspace and the nearest neighbour to a set of projected training images is found.'
            ),
            
            h3('A displaying of the first vector image:'),
            plotOutput("images") %>% withSpinner(color = "#0dc5c1"),
            
            
            h3('Taking the average mean of the faces and displaying them:'),
            plotOutput("averageFacePicker") %>% withSpinner(color = "#0dc5c1"),
            
            
            h3('The average face image from all images'),
            plotOutput("averageFace") %>% withSpinner(color = "#0dc5c1"),
            
            h3('Magnitude of eigenvalues'),
            plotOutput("magnitude") %>% withSpinner(color = "#0dc5c1"),
            
            h3('Plotting of all of the eigenvectors'),
            plotOutput("plotOfEigenvectors") %>% withSpinner(color = "#0dc5c1"),
            
            h3('Projection coefficients in eigen space in photos'),
            plotOutput("projectionGraph") %>% withSpinner(color = "#0dc5c1"),
            
            h3("Reconstruction of the photo from the eigenvector space"),
            plotOutput("reconstruction") %>% withSpinner(color = "#0dc5c1"),
            
            h3("Add the average face"),
            
            h5("original"),
            plotOutput("originalFace") %>% withSpinner(color = "#0dc5c1"),
            
            h5("average face"),
            plotOutput("averageFaceeeeeeee") %>% withSpinner(color = "#0dc5c1"),
            
            h5("reconstructed from eigen space"),
            plotOutput("reconstructedFace") %>% withSpinner(color = "#0dc5c1"),
            
            h5("reconstructed + average face"),
            plotOutput("reconstructedAverageFace") %>% withSpinner(color = "#0dc5c1"),
            
            h3("Simple Classifier based on the Euclidean distance"),
            plotOutput("euclideanClassifier") %>% withSpinner(color = "#0dc5c1"),
            
            h3("Eigen Values Heatmap"),
            plotOutput("heatmap1") %>% withSpinner(color = "#0dc5c1"),
            
            h3("Eigen Vectors Heatmap"),
            plotOutput("heatmap2") %>% withSpinner(color = "#0dc5c1"),
            
        )
    )
), )

shinyApp(
    ui = dashboardPage(
        dashboardHeader(title = "Assignment 2"),
        dashboardSidebar(disable = TRUE),
        body
    ),
    server = function(input, output) {
        # output$NAMEHERE <- renderPlot({
        #     if (!is.null(input$datafile)){
        #        # ...
        #     }
        # })
        
        # to allow for a large dataset of up to 30mb to be uploaded to the server
        options(shiny.maxRequestSize = 30 * 1024 ^ 2)
        
        dataframe <- reactive({
            if (is.null(input$datafile))
                return(NULL)
            data <- read.csv(input$datafile$datapath)
        })
        
        
        plt_img <-
            function(x) {
                image(x, col = grey(seq(0, 1, length = 256)))
            }
        
        #first images rendered underneath the instructions.
        output$images <- renderPlot({
            if (!is.null(input$datafile)) {
                par(mfrow = c(1, 4))
                # par(mar=c(0.1,0.1,0.1,0.1))
                
                image <-
                    matrix(as.numeric(dataframe()[1, ]),
                           nrow = 64,
                           byrow = T)
                plt_img(image)
                
                rotate_image <-
                    t(apply(matrix(
                        as.numeric(dataframe()[1, ]),
                        nrow = 64,
                        byrow = T
                    ), 2, rev))
                plt_img(rotate_image)
            }
        })
        
        output$averageFacePicker <- renderPlot({
            if (!is.null(input$datafile)) {
                par(mfrow = c(1, 3))
                # par(mar=c(0.1,0.1,0.1,0.1))
                
                AV1 = colMeans(data.matrix(dataframe()[11:20,]))
                face_1 <-
                    t(apply(matrix(
                        AV1, nrow = 64, byrow = T
                    ), 2, rev))
                plt_img(face_1)
                
                AV2 = colMeans(data.matrix(dataframe()[21:30,]))
                face_1 <-
                    t(apply(matrix(
                        AV2, nrow = 64, byrow = T
                    ), 2, rev))
                plt_img(face_1)
                
                # 1:10
                # 11:20
                # 21:30
                # ...
                # 381:390
                # 391:400
            }
        })
        
        newdata <- NULL
        
        # Rotate every image and save to a new file for easy display in R
        average <- reactive({
            for (i in 1:nrow(dataframe()))
            {
                # Rotated Image 90 degree
                c <-
                    as.numeric((apply(
                        matrix(
                            as.numeric(dataframe()[i,]),
                            nrow = 64,
                            byrow = T
                        ), 2, rev
                    )))
                # Vector containing the image
                newdata <- rbind(newdata, c)
            }
            
            df <- as.data.frame(newdata)
        })
        
        output$averageFace <- renderPlot({
            if (!is.null(input$datafile)) {
                par(mfrow = c(1, 3))
                average_face = colMeans(average())
                AVF = matrix(average_face, nrow = 1, byrow = T)
                plt_img(matrix(average_face, nrow = 64, byrow = T))
            }
        })
        
        output$magnitude <- renderPlot({
            if (!is.null(input$datafile)) {
                dataForScale <- data.matrix(average())
                scaledData <- scale(dataForScale)
                rows <- nrow(dataForScale)
                
                covarianceMatrix <-
                    (rows - 1) ^ -1 * t(scaledData) %*% scaledData
                
                covariancePopulation <-
                    (rows) ^ -1 * t(scaledData) %*% scaledData
                
                eigs <- eigs(covarianceMatrix, 40, which = "LM")
                
                eigenvalues <- eigs$values
                
                eigenvectors <- eigs$vectors
                
                par(mfrow = c(1, 2))
                par(mar = c(2.5, 2.5, 2.5, 2.5))
                y = eigenvalues[1:40]
                # First 40 eigenvalues dominate
                plot(
                    1:40,
                    y,
                    type = "o",
                    log = "y",
                    main = "Magnitude of the 40 biggest eigenvalues",
                    xlab = "Eigenvalue #",
                    ylab = "Magnitude"
                )
                
            }
        })
        
        
        output$plotOfEigenvectors <- renderPlot({
            if (!is.null(input$datafile)) {
                dataForScale <- data.matrix(average())
                scaledData <- scale(dataForScale)
                rows <- nrow(dataForScale)
                
                covarianceMatrix <-
                    (rows - 1) ^ -1 * t(scaledData) %*% scaledData
                
                covariancePopulation <-
                    (rows) ^ -1 * t(scaledData) %*% scaledData
                
                eigs <- eigs(covarianceMatrix, 40, which = "LM")
                
                eigenvalues <- eigs$values
                
                eigenvectors <- eigs$vectors
                
                par(mfrow = c(3, 2))
                par(mar = c(0.2, 0.2, 0.2, 0.2))
                for (i in 1:6) {
                    plt_img(matrix(
                        as.numeric(eigenvectors[, i]),
                        nrow = 64,
                        byrow = T
                    ))
                }
            }
            
        })
        
        output$projectionGraph <- renderPlot({
            if (!is.null(input$datafile)) {
                par(mfrow = c(1, 3))
                # par(mar=c(2,2,2,2))
                
                dataForScale <- data.matrix(average())
                scaledData <- scale(dataForScale)
                rows <- nrow(dataForScale)
                
                covarianceMatrix <-
                    (rows - 1) ^ -1 * t(scaledData) %*% scaledData
                
                covariancePopulation <-
                    (rows) ^ -1 * t(scaledData) %*% scaledData
                
                eigs <- eigs(covarianceMatrix, 40, which = "LM")
                
                eigenvalues <- eigs$values
                
                eigenvectors <- eigs$vectors
                
                projection1 <-
                    data.matrix(average()[1, ]) %*% eigenvectors
                
                barplot(
                    projection1,
                    main = "",
                    col = "blue",
                    ylim = c(-40, 10)
                )
                legend("topright", legend = "Photo Projection")
                
            }
            
        })
        
        
        output$reconstruction <- renderPlot({
            if (!is.null(input$datafile)) {
                par(mfrow = c(1, 4))
                # par(mar = c(1, 1, 1, 1))
                
                dataForScale <- data.matrix(average())
                scaledData <- scale(dataForScale)
                rows <- nrow(dataForScale)
                
                covarianceMatrix <-
                    (rows - 1) ^ -1 * t(scaledData) %*% scaledData
                
                covariancePopulation <-
                    (rows) ^ -1 * t(scaledData) %*% scaledData
                
                eigs <- eigs(covarianceMatrix, 40, which = "LM")
                
                eigenvalues <- eigs$values
                
                eigenvectors <- eigs$vectors
                
                # 1st person 1st photo
                plt_img(matrix(
                    as.numeric(average()[1, ]),
                    nrow = 64,
                    byrow = T
                ))
                
                # 1st person project into eigen space and reconstruct
                PF1 <- data.matrix(average()[1,]) %*% eigenvectors
                RE1 <- PF1 %*% t(eigenvectors)
                plt_img(matrix(
                    as.numeric(RE1),
                    nrow = 64,
                    byrow = T
                ))
                
                # 2nd person 1st photo
                plt_img(matrix(
                    as.numeric(average()[11, ]),
                    nrow = 64,
                    byrow = T
                ))
                
                # 2nd persoon project into eigen space and reconstruct
                PF2 <- data.matrix(average()[11,]) %*% eigenvectors
                RE2 <- PF2 %*% t(eigenvectors)
                plt_img(matrix(
                    as.numeric(RE2),
                    nrow = 64,
                    byrow = T
                ))
            }
            
        })
        
        
        output$originalFace <- renderPlot({
            if (!is.null(input$datafile)) {
                par(mfrow = c(1, 3))
                plt_img(matrix(
                    as.numeric(average()[1, ]),
                    nrow = 64,
                    byrow = T
                ))
            }
        })
        
        output$averageFaceeeeeeee <- renderPlot({
            if (!is.null(input$datafile)) {
                par(mfrow = c(1, 3))
                average_face = colMeans(average())
                AVF = matrix(average_face, nrow = 1, byrow = T)
                plt_img(matrix(average_face, nrow = 64, byrow = T))
            }
        })
        
        output$reconstructedFace <- renderPlot({
            if (!is.null(input$datafile)) {
                dataForScale <- data.matrix(average())
                scaledData <- scale(dataForScale)
                rows <- nrow(dataForScale)
                
                covarianceMatrix <-
                    (rows - 1) ^ -1 * t(scaledData) %*% scaledData
                
                covariancePopulation <-
                    (rows) ^ -1 * t(scaledData) %*% scaledData
                
                eigs <- eigs(covarianceMatrix, 40, which = "LM")
                
                eigenvalues <- eigs$values
                
                eigenvectors <- eigs$vectors
                
                par(mfrow = c(1, 3))
                PF1 <- data.matrix(average()[1,]) %*% eigenvectors
                RE1 <- PF1 %*% t(eigenvectors)
                plt_img(matrix(
                    as.numeric(RE1),
                    nrow = 64,
                    byrow = T
                ))
                
            }
        })
        
        output$reconstructedAverageFace <- renderPlot({
            if (!is.null(input$datafile)) {
                par(mfrow = c(1, 3))
                
                dataForScale <- data.matrix(average())
                scaledData <- scale(dataForScale)
                rows <- nrow(dataForScale)
                
                covarianceMatrix <-
                    (rows - 1) ^ -1 * t(scaledData) %*% scaledData
                
                covariancePopulation <-
                    (rows) ^ -1 * t(scaledData) %*% scaledData
                
                eigs <- eigs(covarianceMatrix, 40, which = "LM")
                
                eigenvalues <- eigs$values
                
                eigenvectors <- eigs$vectors
                
                average_face = colMeans(average())
                AVF = matrix(average_face, nrow = 1, byrow = T)
                PF1 <- data.matrix(average()[1,]) %*% eigenvectors
                RE1 <- PF1 %*% t(eigenvectors)
                RE1AVF = RE1 + AVF
                
                plt_img(matrix(
                    as.numeric(RE1AVF),
                    nrow = 64,
                    byrow = T
                ))
            }
        })
        
        output$euclideanClassifier <- renderPlot({
            if (!is.null(input$datafile)) {
                dataForScale <- data.matrix(average())
                scaledData <- scale(dataForScale)
                rows <- nrow(dataForScale)
                
                covarianceMatrix <-
                    (rows - 1) ^ -1 * t(scaledData) %*% scaledData
                
                covariancePopulation <-
                    (rows) ^ -1 * t(scaledData) %*% scaledData
                
                eigs <- eigs(covarianceMatrix, 40, which = "LM")
                
                eigenvalues <- eigs$values
                
                eigenvectors <- eigs$vectors
                # New photo under test, say, 142nd photo
                # Transform onto eigen space to find the coefficients
                PF1 <- data.matrix(average()[142,]) %*% eigenvectors
                
                # Transform all the traning photos onto eigen space and get the coefficients
                PFall <- data.matrix(average()) %*% eigenvectors
                
                # Find the simple difference and multiplied by itself to avoid negative value
                test <- matrix(rep(1, 400), nrow = 400, byrow = T)
                test_PF1 <- test %*% PF1
                Diff <- PFall - test_PF1
                y <- (rowSums(Diff) * rowSums(Diff))
                
                # Find the minimum number to match the photo in the files
                x = c(1:400)
                newdf = data.frame(cbind(x, y))
                
                the_number = newdf$x[newdf$y == min(newdf$y)]
                
                par(mfrow = c(1, 1))
                par(mar = c(1, 1, 1, 1))
                barplot(y, main = "Similarity Plot: 0 = Most Similar")
                
            }
        })
        
        output$heatmap1 <- renderPlot({
            dataForScale <- data.matrix(average())
            scaledData <- scale(dataForScale)
            rows <- nrow(dataForScale)
            
            covarianceMatrix <-
                (rows - 1) ^ -1 * t(scaledData) %*% scaledData
            
            covariancePopulation <-
                (rows) ^ -1 * t(scaledData) %*% scaledData
            
            eigs <- eigs(covarianceMatrix, 40, which = "LM")
            
            eigenvalues <- eigs$values
            
            eigenvectors <- eigs$vectors
            
            if (!is.null(input$datafile)){
                matrix <- cbind(eigenvalues)
                plot(matrix,  main="Eigen Values heatmap", xlab="", ylab="")
            }
        })
        
        output$heatmap2 <- renderPlot({
            dataForScale <- data.matrix(average())
            scaledData <- scale(dataForScale)
            rows <- nrow(dataForScale)
            
            covarianceMatrix <-
                (rows - 1) ^ -1 * t(scaledData) %*% scaledData
            
            covariancePopulation <-
                (rows) ^ -1 * t(scaledData) %*% scaledData
            
            eigs <- eigs(covarianceMatrix, 40, which = "LM")
            
            eigenvalues <- eigs$values
            
            eigenvectors <- eigs$vectors
            
            if (!is.null(input$datafile)){
                matrix <- cbind(eigenvectors)
                plot(matrix,  main="Eigen Vectors heatmap", xlab="", ylab="")
            }
        })
        
    }
)
