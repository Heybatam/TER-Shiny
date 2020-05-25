list.of.packages <- c("shiny",
                      "ggplot2",
                      "DT",
                      "GGally",
                      "psych",
                      "Hmisc",
                      "mclust",
                      "kernlab",
                      "MASS",
                      "factoextra",
                      "apcluster",
                      "diceR")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load all of these
lapply(list.of.packages, require, character.only = TRUE)


server <- function(input, output, session) {
  ########################################################### DATA ###########################################################
  
  # read in the CSV
  data <- eventReactive(input$go, {
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    read.csv(inFile$datapath, header = (input$header == "Yes"),
             sep = input$sep, quote = input$quote, stringsAsFactors=FALSE)
    
    #return(data)
  })
  
  # tableplot
  output$tableplot <- renderPlot({
    data()
  })
  
  output$summary <- renderPrint({ 
    t(summary(data())) 
  })
  
  output$contents <- DT::renderDataTable({
    DT::datatable(data = data())
  })
  
  #tab visualization
  output$scatter <- renderPlot({
    data <- data()
    X <- data[,0:(ncol(data)-1)]
    y <- data[,ncol(data)]
    y_col <- palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                       "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    #pdf('dat_iris.pdf')
    pairs(X, lower.panel = NULL, col = y_col[y])
    par(xpd = T)
    legend(x = 0.1, y = 0.4, legend = as.character(unique(y)), fill = y_col)
  })
  
  output$histo <- renderPlot({
    data <- data()
    y_col <- palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                       "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    hist(data[,ncol(data)], col = y_col)
  })
  
  ########################################################### PCA ###########################################################
  
  output$bartlett <- renderPrint({
    the_data <- data()
    the_data_num <- na.omit(the_data[,sapply(the_data,is.numeric)])
    # exclude cols with zero variance
    the_data_num <- the_data_num[,!apply(the_data_num, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
    
    cortest.bartlett(cor(the_data_num), n = nrow(the_data_num))
  })  
  
  output$kmo <- renderPrint({
    the_data <- data()
    the_data_num <- the_data[,sapply(the_data,is.numeric)]
    # exclude cols with zero variance
    the_data_num <- the_data_num[,!apply(the_data_num, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
    
    # R <- cor(the_data_num)
    # KMO(R)
    
    # http://www.opensubscriber.com/message/r-help@stat.math.ethz.ch/7315408.html
    # KMO Kaiser-Meyer-Olkin Measure of Sampling Adequacy 
    kmo = function( data ){ 
      
      library(MASS) 
      X <- cor(as.matrix(data)) 
      iX <- ginv(X) 
      S2 <- diag(diag((iX^-1))) 
      AIS <- S2%*%iX%*%S2                      # anti-image covariance matrix 
      IS <- X+AIS-2*S2                         # image covariance matrix 
      Dai <- sqrt(diag(diag(AIS))) 
      IR <- ginv(Dai)%*%IS%*%ginv(Dai)         # image correlation matrix 
      AIR <- ginv(Dai)%*%AIS%*%ginv(Dai)       # anti-image correlation matrix 
      a <- apply((AIR - diag(diag(AIR)))^2, 2, sum) 
      AA <- sum(a) 
      b <- apply((X - diag(nrow(X)))^2, 2, sum) 
      BB <- sum(b) 
      MSA <- b/(b+a)                        # indiv. measures of sampling adequacy 
      
      AIR <- AIR-diag(nrow(AIR))+diag(MSA)  # Examine the anti-image of the 
      # correlation matrix. That is the 
      # negative of the partial correlations, 
      # partialling out all other variables. 
      
      kmo <- BB/(AA+BB)                     # overall KMO statistic 
      
      # Reporting the conclusion 
      if (kmo >= 0.00 && kmo < 0.50){ 
        test <- 'The KMO test yields a degree of common variance unacceptable for FA.' 
      } else if (kmo >= 0.50 && kmo < 0.60){ 
        test <- 'The KMO test yields a degree of common variance miserable.' 
      } else if (kmo >= 0.60 && kmo < 0.70){ 
        test <- 'The KMO test yields a degree of common variance mediocre.' 
      } else if (kmo >= 0.70 && kmo < 0.80){ 
        test <- 'The KMO test yields a degree of common variance middling.' 
      } else if (kmo >= 0.80 && kmo < 0.90){ 
        test <- 'The KMO test yields a degree of common variance meritorious.' 
      } else { 
        test <- 'The KMO test yields a degree of common variance marvelous.' 
      } 
      
      ans <- list(  overall = kmo, 
                    report = test, 
                    individual = MSA, 
                    AIS = AIS, 
                    AIR = AIR ) 
      return(ans) 
      
    }    # end of kmo() 
    kmo(na.omit(the_data_num))
    
  })
  
  output$col_pca <- renderUI ({
    data <- data()
    
    #Getting the data set with the appropriate name
    
    #only numerical cols
    data_num <- na.omit(data[, sapply(data,is.numeric)])
    
    #exclude col with zero variance
    data_num <- data_num[,!apply(data_num,MARGIN=2, function(x) max(x,na.rm=TRUE)== min(x,na.rm=TRUE))]
    
    colnames <- names(data_num)
    
    #Creating the checkboxes and select them all by default
    checkboxGroupInput("columns", "Choose columns",
                       choices = colnames,
                       selected = colnames)
  })
  
  pca_objects <- eventReactive(input$pcago, {
    
    # Selected columns
    columns <-  input$columns
    data <- na.omit(data())
    data_subset <- na.omit(data[,columns, drop = FALSE ])
    pca_output <- prcomp(na.omit((data_subset),
                                 center = (input$center == 'YES'),
                                 scale. = input$scale. == 'YES'))
    
    # data frame of PCs
    pcs_df <- cbind(data,pca_output$x)
    return(list(data=data, 
                data_subset=data_subset,
                pca_output=pca_output,
                pcs_df=pcs_df))
    
  })
  
  # choose a grouping variable
  output$the_grouping_variable <- renderUI({
    the_data <- data()
    
    
    # for grouping we want to see only cols where the number of unique values are less than 
    # 10% the number of observations
    grouping_cols <- sapply(seq(1, ncol(the_data)), function(i) length(unique(the_data[,i])) < nrow(the_data)/10 )
    
    the_data_group_cols <- the_data[, grouping_cols, drop = FALSE]
    # drop down selection
    selectInput(inputId = "the_grouping_variable", 
                label = "Grouping variable:",
                choices=c("None", names(the_data_group_cols)))
    
  })
  
  output$pcs_to_plot_x <- renderUI({
    pca_output <- pca_objects()$pca_output$x
    # drop down selection
    selectInput(inputId = "pcs_to_plot_x",
                label = "X axis:",
                choices= colnames(pca_output),
                selected = 'PC1')
  })
  
  output$pcs_to_plot_y <- renderUI({
    pca_output <- pca_objects()$pca_output$x
    
    # drop down selection
    selectInput(inputId = "pcs_to_plot_y",
                label = "Y axis:",
                choices = colnames(pca_output),
                selected = 'PC2')
  })
  
  
  output$plot2 <- renderPlot({
    pca_output <- pca_objects()$pca_output
    eig = (pca_output$sdev)^2
    variance <- eig*100/sum(eig)
    cumvar <- paste(round(cumsum(variance),1), "%")
    eig_df <- data.frame(eig = eig,
                         PCs = colnames(pca_output$x),
                         cumvar =  cumvar)
    ggplot(eig_df, aes(reorder(PCs, -eig), eig)) +
      geom_bar(stat = "identity", fill = "white", colour = "black") +
      geom_text(label = cumvar, size = 4,
                vjust=-0.4) +
      theme_bw(base_size = 14) +
      xlab("PC") +
      ylab("Variances") +
      ylim(0,(max(eig_df$eig) * 1.1))
  })
  
  
  
  # PC plot
  pca_biplot <- reactive({
    pcs_df <- pca_objects()$pcs_df
    pca_output <-  pca_objects()$pca_output
    
    var_expl_x <- round(100 * pca_output$sdev[as.numeric(gsub("[^0-9]", "", input$pcs_to_plot_x))]^2/sum(pca_output$sdev^2), 1)
    var_expl_y <- round(100 * pca_output$sdev[as.numeric(gsub("[^0-9]", "", input$pcs_to_plot_y))]^2/sum(pca_output$sdev^2), 1)
    labels <- rownames(pca_output$x)
    grouping <- input$the_grouping_variable
    
    if(grouping == 'None'){
      # plot without grouping variable
      pc_plot_no_groups  <- ggplot(pcs_df, 
                                   aes_string(input$pcs_to_plot_x, 
                                              input$pcs_to_plot_y
                                   )) +
        
        
        geom_text(aes(label = labels),  size = 5) +
        theme_bw(base_size = 14) +
        coord_equal() +
        xlab(paste0(input$pcs_to_plot_x, " (", var_expl_x, "% explained variance)")) +
        ylab(paste0(input$pcs_to_plot_y, " (", var_expl_y, "% explained variance)")) 
      # the plot
      pc_plot_no_groups
      
      
    } else {
      # plot with grouping variable
      
      pcs_df$fill_ <-  as.character(pcs_df[, grouping, drop = TRUE])
      pc_plot_groups  <- ggplot(pcs_df, aes_string(input$pcs_to_plot_x, 
                                                   input$pcs_to_plot_y, 
                                                   fill = 'fill_', 
                                                   colour = 'fill_'
      )) +
        stat_ellipse(geom = "polygon", alpha = 0.1) +
        
        geom_text(aes(label = labels),  size = 5) +
        theme_bw(base_size = 14) +
        scale_colour_discrete(guide = FALSE) +
        guides(fill = guide_legend(title = "groups")) +
        theme(legend.position="top") +
        coord_equal() +
        xlab(paste0(input$pcs_to_plot_x, " (", var_expl_x, "% explained variance)")) +
        ylab(paste0(input$pcs_to_plot_y, " (", var_expl_y, "% explained variance)")) 
      # the plot
      pc_plot_groups
    }
    
    
  })
  
  output$brush_info <- renderTable({
    # the brushing function
    brushedPoints(pca_objects()$pcs_df, input$plot_brush)
  })
  
  # for zooming
  output$z_plot1 <- renderPlot({
    pca_biplot() 
    
  })
  
  # zoom ranges
  zooming <- reactiveValues(x = NULL, y = NULL)
  
  observe({
    brush <- input$z_plot1Brush
    if (!is.null(brush)) {
      zooming$x <- c(brush$xmin, brush$xmax)
      zooming$y <- c(brush$ymin, brush$ymax)
    }
    else {
      zooming$x <- NULL
      zooming$y <- NULL
    }
  })
  
  
  # for zooming
  output$z_plot2 <- renderPlot({
    
    pca_biplot() + coord_cartesian(xlim = zooming$x, ylim = zooming$y)
    
    
  })
  
  output$brush_info_after_zoom <- renderTable({
    # the brushing function
    brushedPoints(pca_objects()$pcs_df, input$plot_brush_after_zoom)
  })
  
  output$pca_details <- renderPrint({
    # 
    print(pca_objects()$pca_output$rotation)
    summary(pca_objects()$pca_output)
    
  })
  
  ######################################################### KMEANS #######################################################
  
  output$km_pcs_x <- renderUI({
    pca_output <- pca_objects()$pca_output$x
    # drop down selection
    selectInput(inputId = "km_pcs_x",
                label = "X Variable:",
                choices= colnames(pca_output),
                selected = 'PC1')
  })
  
  output$km_pcs_y <- renderUI({
    pca_output <- pca_objects()$pca_output$x
    # drop down selection
    selectInput(inputId = "km_pcs_y",
                label = "Y Variable:",
                choices= colnames(pca_output),
                selected = 'PC2')
  })
  
  output$km_label <- renderUI ({
    
    data <- data()
    
    data_num <- na.omit(data[, sapply(data,is.numeric)])
    
    #exclude col with zero variance
    data_num <- data_num[,!apply(data_num,MARGIN=2, function(x) max(x,na.rm=TRUE)== min(x,na.rm=TRUE))]
    
    colnames <- names(data_num)
    
    #Creating the checkboxes and select them all by default
    checkboxGroupInput(inputId = "km_label", label = "Choose the labels :",
                       choices = colnames)
  })
  
  
  kmeans_plot <- reactive({
    pcs_df <- pca_objects()$pcs_df
    x <- pca_objects()$pca_output$x
    selectedData <- pcs_df[, c(input$km_pcs_x,input$km_pcs_y)]
    clusters <- kmeans(x = selectedData, centers= input$clusters, iter.max = input$iter, nstart = input$nstart, algorithm = input$kmalgo, trace = input$trace)
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    
    label = input$km_label
    
    km_plot <-
      
      # ggplot(as.data.frame(selectedData),
      #        aes_string(paste0("`", input$km_pcs_x, "`"),
      #                   paste0("`", input$km_pcs_y, "`")
      #        ))+
      # points(clusters$centers, pch = 1, cex = 4, lwd = 4)
      if(!is.null(label)) {
        plot(selectedData,
             col = clusters$cluster,
             pch = 10)+
          points(clusters$centers, pch = 6, cex = 3, lwd = 3)+
          points(selectedData, col=x[label], pch=11)
      }
    else{
      plot(selectedData,
           col = clusters$cluster,
           pch = 4)+
        points(clusters$centers, pch = 2, cex = 3, lwd = 3)
    }
    
    km_plot
  })
  
  
  output$plot_kmeans <- renderPlot({
    kmeans_plot()
    
  })
  
  ######################################################### GMM ##########################################################
  #Based on the mclust package
  
  output$gmm_pcs_x <- renderUI({
    pca_output <- pca_objects()$pca_output$x
    # drop down selection
    selectInput(inputId = "gmm_pcs_x",
                label = "X Variable:",
                choices= colnames(pca_output),
                selected = 'PC1')
  })
  
  output$gmm_pcs_y <- renderUI({
    pca_output <- pca_objects()$pca_output$x
    # drop down selection
    selectInput(inputId = "gmm_pcs_y",
                label = "Y Variable:",
                choices= colnames(pca_output),
                selected = 'PC2')
  })
  
  output$mod1_summary <- renderPrint({
    pcs_df <- pca_objects()$pcs_df
    df <- pcs_df[, c(input$gmm_pcs_x,input$gmm_pcs_y)]
    BIC <- mclustBIC(df)
    mod1 <- Mclust(df, x = BIC)
    t(summary(mod1))
  })
  
  output$mod1_plot <- reactive({
    pcs_df <- pca_objects()$pcs_df
    df <- pcs_df[, c(input$gmm_pcs_x,input$gmm_pcs_y)]
    BIC <- mclustBIC(df)
    mod1 <- Mclust(df, x = BIC)
    plot(mod1, what = "BIC")
  })
  
  output$mod4__dens_plot1 <- renderPlot({
    pcs_df <- pca_objects()$pcs_df
    #x <- pca_objects()$pca_output$x
    df <- pcs_df[, c(input$gmm_pcs_x,input$gmm_pcs_y)]
    mod4 <- densityMclust(df)
    plot(mod4, what = "density", type = "image")
  })
  
  output$mod4__dens_plot2 <- renderPlot({
    pcs_df <- pca_objects()$pcs_df
    #x <- pca_objects()$pca_output$x
    df <- pcs_df[, c(input$gmm_pcs_x,input$gmm_pcs_y)]
    mod4 <- densityMclust(df)
    plot(mod4, what = "density", type = "hdr")
  })
  
  output$mod4__dens_plot3 <- renderPlot({
    pcs_df <- pca_objects()$pcs_df
    #x <- pca_objects()$pca_output$x
    df <- pcs_df[, c(input$gmm_pcs_x,input$gmm_pcs_y)]
    mod4 <- densityMclust(df)
    plot(mod4, what = "density", type = "contour")
  })
  
  output$mod4__dens_plot4 <- renderPlot({
    pcs_df <- pca_objects()$pcs_df
    #x <- pca_objects()$pca_output$x
    df <- pcs_df[, c(input$gmm_pcs_x,input$gmm_pcs_y)]
    mod4 <- densityMclust(df)
    plot(mod4, what = "density", type = "persp")
  })
  
  output$specc_pcs_x <- renderUI({
    # drop down selection
    pca_output <- pca_objects()$pca_output$x
    selectInput(inputId = "specc_pcs_x",
                label = "X Variable:",
                choices= colnames(pca_output),
                selected = 'PC1')
  })
  
  output$specc_pcs_y <- renderUI({
    # drop down selection
    pca_output <- pca_objects()$pca_output$x
    selectInput(inputId = "specc_pcs_y",
                label = "Y Variable:",
                choices= colnames(pca_output),
                selected = 'PC2')
  })
  
  specc_plot <- reactive({
    # kernlab package
    data <- data()
    pcs_df <- pca_objects()$pcs_df
    df <- pca_objects()$pca_output$x[, c(input$specc_pcs_x,input$specc_pcs_y)]
    sc <- specc(df, centers=input$specc_centers,cex=3, lwd=3)
    plot(df, col=sc, pch=6)            # estimated classes (x)
    #points(df, col=data[,ncol(data)], pch=10) # true classes (<>)
  })
  
  # specc_plot <- reactive({
  #   data <- data()
  #   data <- as.data.frame(na.omit(data))
  #   data_num <- data[, sapply(data,is.numeric)]
  #   columns <- names(data_num)
  #   data_subset <- na.omit(data_num[,columns, drop = FALSE ])
  #   pca_output <- prcomp(na.omit((data_subset),center = TRUE,scale. = TRUE))
  #   pcs_df <- cbind(data_subset,pca_output$x)
  #   df <- pca_output$x
  #   df <- df[,c(input$specc_pcs_x,input$specc_pcs_y)]
  #   sc <- specc(df, centers=input$specc_centers)
  #   plot(df, col=sc, pch=6)  # estimated classes (x)
  #   #points(df, col=data[,ncol(data)], pch=11) # true classes (<>)
  # })
  # 
  output$specc <- renderPlot({
    specc_plot()
  })
  
  
  output$ap_pcs_x <- renderUI({
    # drop down selection
    pca_output <- pca_objects()$pca_output$x
    selectInput(inputId = "ap_pcs_x",
                label = "Y Variable:",
                choices= colnames(pca_output),
                selected = 'PC1')
  })
  
  output$ap_pcs_y <- renderUI({
    # drop down selection
    pca_output <- pca_objects()$pca_output$x
    selectInput(inputId = "ap_pcs_y",
                label = "Y Variable:",
                choices= colnames(pca_output),
                selected = 'PC2')
  })
  
  ap_plot <- reactive({
    data <- data()
    pcs_df <- pca_objects()$pcs_df
    df <- pca_objects()$pca_output$x[, c(input$ap_pcs_x,input$ap_pcs_y)]
    d.apclus <- apcluster(negDistMat(r=2), df)
    plot(d.apclus, df, main=max(unique(data[,(ncol(data))])))
  })
  
  output$ap_plot <- renderPlot({
    ap_plot()
  })
  
  output$ap_print <- renderPrint({
    data <- data()
    pcs_df <- pca_objects()$pcs_df
    df <- pca_objects()$pca_output$x[, c(input$ap_pcs_x,input$ap_pcs_y)]
    d.apclus <- apcluster(negDistMat(r=2), df, details=TRUE)
    show(d.apclus)
  })
  
  ap_hm <- reactive({
    data <- data()
    pcs_df <- pca_objects()$pcs_df
    df <- pca_objects()$pca_output$x[, c(input$ap_pcs_x,input$ap_pcs_y)]
    apres <- apcluster(negDistMat(r=2), df)
    heatmap(apres)
  })
  
  ap_plot2 <- reactive({
    data <- data()
    pcs_df <- pca_objects()$pcs_df
    df <- pca_objects()$pca_output$x[, c(input$ap_pcs_x,input$ap_pcs_y)]
    apres <- apcluster(negDistMat(r=2), df)
    apres <- apcluster(s=apres@sim, q=0.1)
    plot(apres, df)
  })
  
  output$ap_hm <- renderPlot({
    ap_hm()
  })
  
  output$ap_plot2 <- renderPlot({
    ap_plot2()
  })
  
}
