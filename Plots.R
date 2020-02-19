getwd()
setwd("C:/Users/delsa/Documents/FirstYearPhDTalk")
library(igraph)
library(plot3D)
diet.edges <- data.frame(from = Rsim.model$params$PreyFrom[3:length(Rsim.model$params$PreyFrom)],
                         to = Rsim.model$params$PreyTo[3:length(Rsim.model$params$PreyTo)],
                         weigth = Rsim.model$params$PreyPreyWeight[3:length(Rsim.model$params$PreyPreyWeight)])
diet.edges <- subset(diet.edges, subset = diet.edges[,1]>=1)
diet.nodes <- data.frame(id = 1:22,
                         group = c("Grey Seal", "Fish Feeding Birds", "Juv Cod", "Ad Cod", "Juv Herring",
                                   "Ad Herring", "Juv Sprat", "Ad Sprat", "Juv Flounder", "Ad Flounder",
                                   "Saduria Entemon", "Mytilus Sp", "Macoma Balthica", 
                                   "Other Macrozoobenthos", "Meiobenthos", "Mysids", "Other Zooplankton",
                                   "Pseudocalanus Sp", "Acartia Sp", "Temora Sp", "Phytoplankton",
                                   "Detritus"),
                         type = c(rep(1, 20), 2, 3),
                         type.lebel = c(rep("Living", 20), "Producer", "Detritus"),
                         biomass = Rsim.model$params$B_BaseRef[2:23])
write.csv(diet.edges, "diet.edges.csv")
write.csv(diet.nodes, "diet.nodes.csv")
net <- graph_from_data_frame(d = diet.edges, vertices = diet.nodes, directed = TRUE)
cols <- c(rgb(70/255,130/255,180/255, alpha = 0.75), 
          rgb(34/255,139/255,34/255, alpha = 0.75),
          rgb(255/255,215/255,0/255, alpha = 0.75))
V(net)$color <- cols[V(net)$type]
plot(net, edge.arrow.size = 0.5, 
     vertex.label = NA,
     margin = c(-0.5,-0.5,-0.5,-0.5))
# Equilibrium yields
# Krill
by_fac <- 1/30
F1 <- seq(0, 1, by_fac)
F2 <- seq(0, 1, by_fac)
v <- 1
r1 <- 1
K <- 1
result_1i <- list()
result_1j <- list()
for (i in 1:length(F1)) {
  for (j in 1:length(F2)) {
    Y1_eq <- ((r1*K)*F1[i]*(1-F1[i]))/(1+v*(1-F2[j]))
    result_1j[[j]] <- Y1_eq
  }
  result_1i[[i]] <- result_1j
}
# Baleen whales
F1 <- seq(0, 1, by_fac)
F2 <- seq(0, 1, by_fac)
v <- 1
r2 <- 1
K <- 1
alpha <- 1
result_2i <- list()
result_2j <- list()
for (i in 1:length(F1)) {
  for (j in 1:length(F2)) {
    Y2_eq <- ((alpha*r2*K)*(1-F1[i])*F2[j]*(1-F2[j]))/(1+v*(1-F2[j]))
    result_2j[[j]] <- Y2_eq
  }
  result_2i[[i]] <- result_2j
}
# Plotting results
par(mfrow = c(1,2))
par(mar = c(0,2,0,0))
persp3D(z=matrix(unlist(result_1i), nrow = length(F1),  ncol = length(F1)), 
        theta = 310, phi = 20, 
        xlab = "\n\nWhale F2", 
        ylab = "\n\nKrill F1", 
        zlab = "\n\nKrill Y1", 
        ticktype = "detailed", nticks = 5, border = "black", 
        facets = FALSE, colvar = FALSE, cex.axis = 0.5, bty = "b2")
mtext("A", side = 3, line = -3, cex = 1.5)
persp3D(z=matrix(unlist(result_2i), nrow = length(F2),  ncol = length(F2)),
        theta = 310, phi = 20, 
        xlab = "\n\nWhale F2",
        ylab = "\n\nKrill F1",
        zlab = "\n\nWhale Y2",
        ticktype = "detailed", nticks = 5, border = "black", 
        facets = FALSE, colvar = FALSE, cex.axis = 0.5, bty = "b2")
mtext("B", side = 3, line = -3, cex = 1.5)
par(mar = c(3,3,1,2))
# Krill
by_fac <- 1/50
F1 <- seq(0, 1, by_fac)
F2 <- seq(0, 1, by_fac)
v <- 1
r1 <- 1
K <- 1
result_1i <- list()
result_1j <- list()
for (i in 1:length(F1)) {
  for (j in 1:length(F2)) {
    Y1_eq <- ((r1*K)*F1[i]*(1-F1[i]))/(1+v*(1-F2[j]))
    result_1j[[j]] <- Y1_eq
  }
  result_1i[[i]] <- result_1j
}
# Baleen whales
F1 <- seq(0, 1, by_fac)
F2 <- seq(0, 1, by_fac)
v <- 1
r2 <- 1
K <- 1
alpha <- 1
result_2i <- list()
result_2j <- list()
for (i in 1:length(F1)) {
  for (j in 1:length(F2)) {
    Y2_eq <- ((alpha*r2*K)*(1-F1[i])*F2[j]*(1-F2[j]))/(1+v*(1-F2[j]))
    result_2j[[j]] <- Y2_eq
  }
  result_2i[[i]] <- result_2j
}
krill <- matrix(unlist(result_1i), nrow = length(F1),  ncol = length(F1))
whales <- matrix(unlist(result_2i), nrow = length(F2),  ncol = length(F2))
fields::image.plot(x = seq(0, max(krill), length.out = nrow(krill)),
                   y = seq(0, max(whales), length.out = nrow(whales)),
                   z =(matrix(unlist(result_1i), nrow = length(F1),  ncol = length(F1))+matrix(unlist(result_2i), nrow = length(F2),  ncol = length(F2))), col = cm.colors(30), 
                   xlab = expression(paste("Krill Yield Y"[1])),
                   ylab = expression(paste("Whale Yield Y"[2])))
lines(unlist(result_1i), unlist(result_2i), col = rgb(0,0,0, alpha = 0.1))
lines(0:1,0:1)
axis(3, seq(0, 0.25, 0.05), labels = FALSE)
axis(4, seq(0, 0.15, 0.05), labels = FALSE)
text("1:1 Line", x = 0.175, y = 0.15)
points(x = 0.185, y = 0.07, pch = 13)
text("Sweet Spot", x = 0.21, y = 0.0775)
mtext(expression(paste("Y"[1],"+Y"[2])), side = 4, adj = -0.4, padj = -9.5, las = 2)
legend(x = 0.242, y = 0.184, pch = 13, xpd = NA, legend = "", bty = "n")
text("SS-MSY", x = 0.23, y = 0.165)