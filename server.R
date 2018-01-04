library(shiny)
library(DBI)
library(RMySQL)
library(igraph)

shinyServer(function(input, output) {

    mydb = dbConnect(MySQL(), user='root', password='', dbname='focusSNQ_test', host='127.0.0.1')
    
    network <- reactive({
        
        edges <- dbGetQuery(mydb, statement = paste("SELECT CONCAT(TRIM(Demographic.resident_fname),' ',TRIM(Demographic.resident_lname)) AS Source, 
                    TRIM(CONCAT(TRIM(Network.network_fname),' ',TRIM(Network.network_lname))) AS Target, 
                    SUM(Network.network_weight) AS Weight FROM Demographic, Network 
                    WHERE Demographic.survey_id = Network.survey_id AND Demographic.block_id =",input$block ,
                    "GROUP BY Source, Target HAVING Weight > 0;"))
        
        net <- graph_from_data_frame(d = edges, directed = T)
        
        inDeg <- igraph::degree(net, mode = "in")
        outDeg <- igraph::degree(net, mode = "out")
        Deg <- igraph::degree(net, mode = "total")
        between <- igraph::betweenness(net, directed = T, weights = NA)
        
        nodes <- cbind(as.data.frame(as_ids(V(net))), inDeg, outDeg, Deg, between)
        nodes <- nodes[order(-nodes$Deg),]
        
        net <- graph_from_data_frame(d = edges, vertices = nodes, directed = T)
        net_ft <- induced.subgraph(net, V(net)$Deg > input$filter)
        
        list(nodes = nodes, net_ft = net_ft)
    })

    output$table <- renderTable({
        
        nodes <- network()$nodes

        colnames(nodes)[1] <- "Resident"
        nodes[1:input$influencer,]
        
    })
    
    output$network <- renderPlot({
        
        net_ft <- network()$net_ft

        l <- layout_with_fr(net_ft)
        V(net_ft)$size <- V(net_ft)$inDeg * 3
        E(net_ft)$width <- E(net_ft)$Weight * .2
        plot(net_ft, layout = l, edge.arrow.size = .4, edge.curved = .1,
             vertex.label = V(net_ft)$name, vertex.label.cex = V(net_ft)$size * .075,
             vertex.size = V(net_ft)$size, edge.width = E(net_ft)$width)
    })
})
