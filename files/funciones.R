# DIVECOFOR 1.0
# Copyright (C) 2023 [INIFAP]
# Maintainer: Ernesto Rubio (ernestorub@gmail.com)

instalar_faltantes <- function(paquete) {
  if (!require(paquete, character.only = TRUE)) {
    install.packages(paquete)
  }
}

paquetes_necesarios <- c("openxlsx", "shiny", "dplyr", "shinythemes", "shinyWidgets", "colourpicker")
lapply(paquetes_necesarios, instalar_faltantes)

library(shiny)
library(dplyr)
library(openxlsx)
library(shinythemes)
# library(forstr)
library(ggplot2)
library(shinyWidgets)
library(colourpicker)
##############################
### índices de Diversidad ####
##############################
rich <- function(x) {
   counts <- table(x)
   counts[counts > 0] <- 1
   return(sum(counts))
}

shannon <- function(x) {
  y <- table(x)
  xx <- y / sum(y)
  h <- (-1) * sum(xx * log(xx), na.rm = T)
  return(h)
}

simpson <- function(x) {
  y <- table(x)
  xx <- y / sum(y)
  xy <- sum(xx^2, na.rm = T)
  return(xy)
}

margalef <- function(x) {
  xx <- sum(table(unique(x)))
  xy <- sum(table(x))
  mg <- (xx - 1) / log(xy)
  return(mg)
}

menhinick <- function(x) {
  xx <- sum(table(unique(x)))
  xy <- sum(table(x))
  men <- xx / (sqrt(xy))
  return(men)
}

pielou <- function(x) {
  xx <- sum(table(unique(x)))
  y <- shannon(x) / log(xx)
  return(y)
}

diversidad <- function(x, y, z, data, a) {
  x <- deparse(substitute(x))
  y <- deparse(substitute(y))
  z <- deparse(substitute(z))

  data1 <- as.data.frame(cbind(x = as.character(data[[x]]), y = data[[y]], z = data[[z]]))
  data1$y <- as.numeric(as.character(data1$y))
  data1$z <- as.character(data1$z)
  xx <-
    data1 %>%
    dplyr::group_by(z) %>%
    dplyr::summarise(
      Margalef = margalef(x), 
      Shannon = shannon(x), 
      Simpson = simpson(x), 
      GINI = 1-simpson(x),
      Menhinick = menhinick(x), 
      Pielou = pielou(x), 
      q0 = rich(x), q1 = exp(shannon(x)), q2 = 1/simpson(x)
    )
  xx
}

##############################
### Variables de Estructura ##
##############################
Xha <- function(x, y) {
  xx <- sum(x, na.rm = T)
  yy <- ((xx) * 10000) / y
  return(yy)
}

Nha <- function(x, y) {
  xx <- length(x)
  Nha <- ((xx) * 10000) / y
  return(Nha)
}

g <- function(x) {
  xx <- 0.7854 * ((x / 100)^2)
  xx
}

Gha <- function(x, y) {
  xx <- sum(g(x), na.rm = T)
  yy <- ((xx) * 10000) / y
  return(yy)
}

dg <- function(x) {
  y <- g(x)
  k <- 0.0000785
  dg <- sqrt(sum(y, na.rm = T) / (k * length(x)))
  return(dg)
}

h_dom <- function(h, d, ps) {
  xx <- round(ps / 100)
  xy <- 0
  for (i in 1:length(d)) {
    z <- d[i]
    zx <- length(d[d >= z])
    xy[i] <- zx
  }
  yx <- ifelse(xy <= xx, "h100", "ndom")
  yy <- mean(h[which(yx=="h100")])
  return(yy)
}

d_dom <- function(d, ps) {
  xx <- round(ps / 100)
  xy <- 0
  for (i in 1:length(d)) {
    z <- d[i]
    zx <- length(d[d >= z])
    xy[i] <- zx
  }
  yx <- ifelse(xy <= xx, "dom_tree", "not_dom")
  yy <- mean(d[which(yx == "dom_tree")])
  return(yy)
}

# x, y ,z
estructura <- function(y, h, dc, z, data, a) {
  # x <- deparse(substitute(x))
  y <- deparse(substitute(y))
  h <- deparse(substitute(h))
  dc <- deparse(substitute(dc))
  z <- deparse(substitute(z))

  data1 <- as.data.frame(cbind(
    # x = as.character(data[[x]]), 
    y = data[[y]],
    h = data[[h]], dc = data[[dc]], z = data[[z]]
  ))
  
  data1$y <- as.numeric(as.character(data1$y))
  data1$h <- as.numeric(as.character(data1$h))
  data1$dc <- as.numeric(as.character(data1$dc))
  data1$z <- as.character(data1$z)
  xx <-
    data1 %>%
    dplyr::group_by(z) %>%
    dplyr::summarise(
      Nha = Nha(y, a), Gha = Gha(y, a),
      dm = mean(y, na.rm = T), d_sd = sd(y, na.rm = T),
      hm = mean(h, na.rm = T), h_sd = sd(h, na.rm = T),
      dg = dg(x = y), h_dom = h_dom(h = h, d = y, ps = a), d_dom = d_dom(d = y, ps = a),
      ACha = Xha(x = dc, y = a)
    )
  xx
}

IVI_index_table <- function(x, y, z, data, a) {
  x <- deparse(substitute(x))
  y <- deparse(substitute(y))
  z <- deparse(substitute(z))


  data1 <- as.data.frame(cbind(x = as.character(data[[x]]), y = data[[y]], z = data[[z]]))
  data1$y <- as.numeric(as.character(data1$y))
  data1$z <- as.character(data1$z)

  a <- a * length(unique(data1$z))
  xx <-
    data1 %>%
    dplyr::group_by(x) %>%
    dplyr::summarise(y = Nha(y, a))
  yy <-
    data1 %>%
    dplyr::group_by(x) %>%
    dplyr::summarise(y = Xha(y, a))

  nn <- aggregate(y ~ z + x, data1, length)
  # zz <- aggregate(z ~ x, nn, length)
  zz <-
    nn %>%
    group_by(x) %>%
    summarise(y = length(z))

  table1 <- cbind(xx, yy, zz, all = T)
  table2 <- table1[c(1, 2, 4, 6)]
  # table2
  colnames(table2) <- c("Especie", "Nha", "Xha", "Sitios")
  # table2
  Abundancia <- (table2$Nha / sum(table2$Nha)) * 100
  Dominancia <- (table2$Xha / sum(table2$Xha)) * 100
  Frecuencia <- (table2$Sitios / sum(table2$Sitios)) * 100
  IVI300 <- Abundancia + Dominancia + Frecuencia
  # IVI300
  IVI <- (IVI300 / sum(IVI300)) * 100
  # IVI
  table3 <- cbind(table2, Abundancia, Dominancia, Frecuencia, IVI)
  table3 <- as.data.frame(table3[order(-table3$IVI), ])
  table3
}
