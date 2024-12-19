#=============================================================
######################### Projet 2 ###########################
#=============================================================

# ============================================================
# Packages
# ============================================================

if (!require("ggplot2", character.only = TRUE)) install.packages("ggplot2")
if (!require("reshape2", character.only = TRUE)) install.packages("reshape2")
if (!require("patchwork")) install.packages("patchwork")

library(ggplot2)
library(reshape2)
library(patchwork)

# ============================================================
# Parametres
# ============================================================

# Définition de la salle (15 x 5 = 75 places)
n_rows <- 15 # nombre de rangées dans la salle
n_cols <- 5 # nombre de colonne

# Initialisation de la salle et calcul du bruit initial
salle_initiale <- matrix(NA, nrow = n_rows, ncol = n_cols)  # Matrice vide (doit être initialisée en amont)

set.seed(123)  # Pour reproduire les résultats

# Paramètres de la distribution gaussienne
# Les nombres d'étudiants dans les catégories A, B et C sont tirés selon 
# une distribution gaussienne centrée autour :
mean_value <- 20  # Moyenne de 20
sd_value <- 5     # Écart type de 5

# Générer des valeurs gaussiennes pour A, B, et C
n_A <- round(abs(rnorm(1, mean = mean_value, sd = sd_value)))  # A
n_B <- round(abs(rnorm(1, mean = mean_value, sd = sd_value)))  # B
n_C <- round(abs(rnorm(1, mean = mean_value, sd = sd_value)))  # C

# Ajuster n_D pour que la somme soit 75
# Détermine le nombre de places vides restantes dans la salle après avoir 
# répartis les étudiants dans les catégories A, B et C
n_D <- 75 - (n_A + n_B + n_C)

# S'assurer que le nombre total est bien égal à 75 (au cas où il y aurait un léger écart à cause du hasard)
if (n_D < 0) {
  # Si n_D devient négatif, ajuster les autres catégories pour que la somme fasse 75
  n_A <- n_A + abs(n_D)
  n_D <- 0
}

# Affichage des valeurs de A, B, C, et D
cat("Nombre d'étudiants de type A :", n_A, "\n")
cat("Nombre d'étudiants de type B :", n_B, "\n")
cat("Nombre d'étudiants de type C :", n_C, "\n")
cat("Nombre de places vides :", n_D, "\n")

# Vérification que la somme est bien 75
cat("Nombre d'étudiants dans la salle :", n_A + n_B + n_C, "\n")
cat("Nombre de places totales dans la salle :", n_A + n_B + n_C + n_D, "\n")

# Répartition des catégories
categories <- c(rep("A", n_A), rep("B", n_B), rep("C", n_C), rep("D", n_D))

# Répartir les catégories dans la salle
salle_initiale[] <- sample(categories, size = n_rows * n_cols, replace = FALSE)


# ============================================================
# Fonctions
# ============================================================

# Fonction pour calculer le bruit total dans la salle
calculer_bruit_total <- function(salle, bruits) {
  bruit_total <- 0
  
  for (i in 1:n_rows) {
    for (j in 1:n_cols) {
      # Trouver tous les voisins (adjacents + diagonaux)
      voisins <- c(
        if (i > 1) salle[i - 1, j],     # voisin au-dessus
        if (i < n_rows) salle[i + 1, j], # voisin en dessous
        if (j > 1) salle[i, j - 1],     # voisin à gauche
        if (j < n_cols) salle[i, j + 1], # voisin à droite
        if (i > 1 & j > 1) salle[i - 1, j - 1], # voisin en haut à gauche
        if (i > 1 & j < n_cols) salle[i - 1, j + 1], # voisin en haut à droite
        if (i < n_rows & j > 1) salle[i + 1, j - 1], # voisin en bas à gauche
        if (i < n_rows & j < n_cols) salle[i + 1, j + 1] # voisin en bas à droite
      )
      voisins <- voisins[!is.na(voisins)] # Exclure les cases hors matrice
      bruit_local <- sum(bruits[salle[i, j], voisins])
      bruit_total <- bruit_total + bruit_local
    }
  }
  
  return(bruit_total / 2) # Diviser par 2 car chaque paire est comptée deux fois
}

# Fonction pour calculer le bruit pour chaque cellule
calculer_bruit_par_cellule <- function(salle, bruits) {
  bruit_cellulaire <- matrix(0, nrow = n_rows, ncol = n_cols)
  
  for (i in 1:n_rows) {
    for (j in 1:n_cols) {
      voisins <- c(
        if (i > 1) salle[i - 1, j],     # voisin au-dessus
        if (i < n_rows) salle[i + 1, j], # voisin en dessous
        if (j > 1) salle[i, j - 1],     # voisin à gauche
        if (j < n_cols) salle[i, j + 1], # voisin à droite
        if (i > 1 & j > 1) salle[i - 1, j - 1], # voisin en haut à gauche
        if (i > 1 & j < n_cols) salle[i - 1, j + 1], # voisin en haut à droite
        if (i < n_rows & j > 1) salle[i + 1, j - 1], # voisin en bas à gauche
        if (i < n_rows & j < n_cols) salle[i + 1, j + 1] # voisin en bas à droite
      )
      voisins <- voisins[!is.na(voisins)] # Exclure les cases hors matrice
      bruit_cellulaire[i, j] <- sum(bruits[salle[i, j], voisins])
    }
  }
  
  return(bruit_cellulaire)
}

# Fonction Carte de bruits
generer_carte_bruit <- function(salle, bruits) {
  # Calculer le bruit total et par cellule
  bruit_total <- calculer_bruit_total(salle, bruits)
  bruit_par_cellule <- calculer_bruit_par_cellule(salle, bruits)
  
  # Transformer la matrice des niveaux de bruit en format long pour ggplot2
  bruit_df <- melt(bruit_par_cellule)
  colnames(bruit_df) <- c("Row", "Column", "Bruit")
  
  # Ajouter les lettres de la salle dans le data frame
  lettres_df <- melt(salle)
  colnames(lettres_df) <- c("Row", "Column", "Lettre")
  
  # Fusionner les données de bruit et de lettres
  bruit_df <- merge(bruit_df, lettres_df, by = c("Row", "Column"))
  
  # Créer le graphique avec ggplot2
  p <- ggplot(bruit_df, aes(x = Column, y = Row, fill = Lettre)) +
    geom_tile(color = "black") +  # Afficher les cases
    geom_text(aes(label = ifelse(Lettre == "D", "", Lettre)), color = "black", size = 4) +  # Ajouter les lettres sauf D
    scale_fill_manual(
      values = c("A" = "red", "B" = "orange", "C" = "lightgreen", "D" = "white"),
      name = "Légende :",
      labels = c("A" = "Très bavard", "B" = "Bavard", "C" = "Timide", "D" = "Place vide")
    ) +
    theme_minimal() +  # Style minimal
    labs(
      title = "Carte des niveaux de bruit dans la salle",
      x = "Colonnes",
      y = "Rangées"
    ) +
    theme(
      axis.text.y = element_text(angle = 90, hjust = 1),  # Orientation des étiquettes Y
      legend.position = "top"
    ) +
    # Ajouter une annotation pour afficher le bruit total plus haut
    annotate(
      "text",
      x = max(bruit_df$Column) / 2,  # Centre horizontalement
      y = max(bruit_df$Row) + 1.5,  # Décale verticalement pour éviter le chevauchement
      label = paste("Bruit total :", round(bruit_total, 2)),
      size = 6,
      fontface = "bold"
    )
  
  return(p)
}


# Fonction de transition basique
transition_basique = function(salle_old, beta, bruits) {
  # Créer une copie de la matrice pour éviter de modifier l'original
  salle_new = salle_old
  
  # Tirage aléatoire des 2 positions où permuter les personnes
  # Chaque position est tirée parmi les indices linéaires de la matrice
  coord = sample(1:(n_rows * n_cols), size = 2, replace = FALSE)
  
  # Convertir les positions linéaires en indices de matrice (row, col)
  pos1 = coord[1]
  pos2 = coord[2]
  row1 = (pos1 - 1) %/% n_cols + 1  # Ligne correspondante pour la 1ère position
  col1 = (pos1 - 1) %% n_cols + 1  # Colonne correspondante pour la 1ère position
  row2 = (pos2 - 1) %/% n_cols + 1  # Ligne correspondante pour la 2e position
  col2 = (pos2 - 1) %% n_cols + 1  # Colonne correspondante pour la 2e position
  
  # Échanger les deux valeurs dans la matrice
  temp = salle_new[row1, col1]
  salle_new[row1, col1] = salle_new[row2, col2]
  salle_new[row2, col2] = temp
  
  # Calculer la variation de bruit (delta) entre l'ancienne et la nouvelle configuration
  delta = calculer_bruit_total(salle_old, bruits) - calculer_bruit_total(salle_new, bruits)
  
  # Probabilité d'acceptation de la nouvelle configuration (règle du recuit simulé)
  alpha = exp(beta * delta)
  
  # Générer un nombre aléatoire entre 0 et 1
  u = runif(1)
  
  # Si u < alpha, accepter la nouvelle configuration
  if (u < alpha) {
    return(list(salle = salle_new, D = delta))  # Retourner la nouvelle matrice et le delta de bruit
  } 
  else {
    # Sinon, garder l'ancienne configuration
    return(list(salle = salle_old, D = 0))  # Retourner la matrice initiale et delta = 0
  }
}

# Fonction de transition avec une rotation de 180°
transition_rotation <- function(salle_old, beta, bruits) {
  # Créer une copie de la matrice pour éviter de modifier l'original
  salle_new <- salle_old
  
  # Tirage aléatoire des 2 positions où permuter les personnes
  coord <- sample(1:(n_rows * n_cols), size = 2, replace = FALSE)
  
  # Convertir les positions linéaires en indices de matrice (row, col)
  pos1 <- coord[1]
  pos2 <- coord[2]
  row1 <- (pos1 - 1) %/% n_cols + 1  # Ligne correspondante pour la 1ère position
  col1 <- (pos1 - 1) %% n_cols + 1  # Colonne correspondante pour la 1ère position
  row2 <- (pos2 - 1) %/% n_cols + 1  # Ligne correspondante pour la 2e position
  col2 <- (pos2 - 1) %% n_cols + 1  # Colonne correspondante pour la 2e position
  
  # Identifier les coins de la sous-matrice
  row_min <- min(row1, row2)
  row_max <- max(row1, row2)
  col_min <- min(col1, col2)
  col_max <- max(col1, col2)
  
  # Extraire la sous-matrice
  sous_matrice <- salle_new[row_min:row_max, col_min:col_max]
  sous_matrice <- as.matrix(sous_matrice, nrow = row_max - row_min + 1, ncol = col_max - col_min + 1)
  
  # Effectuer une rotation de 180° sur la sous-matrice
  sous_matrice <- sous_matrice[nrow(sous_matrice):1, ncol(sous_matrice):1]
  
  # Remettre la sous-matrice dans la salle
  salle_new[row_min:row_max, col_min:col_max] <- sous_matrice
  
  # Calculer la variation de bruit (delta) entre l'ancienne et la nouvelle configuration
  delta <- calculer_bruit_total(salle_old, bruits) - calculer_bruit_total(salle_new, bruits)
  
  # Probabilité d'acceptation de la nouvelle configuration (règle du recuit simulé)
  alpha <- exp(beta * delta)
  
  # Générer un nombre aléatoire entre 0 et 1
  u <- runif(1)
  
  # Si u < alpha, accepter la nouvelle configuration
  if (u < alpha) {
    return(list(salle = salle_new, D = delta))  # Retourner la nouvelle matrice et le delta de bruit
  } else {
    # Sinon, garder l'ancienne configuration
    return(list(salle = salle_old, D = 0))  # Retourner la matrice initiale et delta = 0
  }
}

transition_ciblee <- function(salle_old, beta, bruits) {
  # Créer une copie de la matrice pour éviter de modifier l'original
  salle_new <- salle_old
  
  # Tirer une position aléatoire
  coord <- sample(1:(n_rows * n_cols), size = 1)
  row1 <- (coord - 1) %/% n_cols + 1
  col1 <- (coord - 1) %% n_cols + 1
  
  # Étudiant à la position choisie
  type_courant <- salle_new[row1, col1]
  
  # Identifier les voisins
  voisins <- c(
    if (row1 > 1) salle_new[row1 - 1, col1],
    if (row1 < n_rows) salle_new[row1 + 1, col1],
    if (col1 > 1) salle_new[row1, col1 - 1],
    if (col1 < n_cols) salle_new[row1, col1 + 1],
    if (row1 > 1 & col1 > 1) salle_new[row1 - 1, col1 - 1],
    if (row1 > 1 & col1 < n_cols) salle_new[row1 - 1, col1 + 1],
    if (row1 < n_rows & col1 > 1) salle_new[row1 + 1, col1 - 1],
    if (row1 < n_rows & col1 < n_cols) salle_new[row1 + 1, col1 + 1]
  )
  voisins <- voisins[!is.na(voisins)]
  
  # Compter les fréquences des types dans le voisinage
  freqs <- table(voisins)
  
  # Trouver le type le moins fréquent
  type_moins_frequent <- names(freqs)[freqs == min(freqs)]
  type_cible <- sample(type_moins_frequent, 1)  # Choisir aléatoirement si égalité
  
  # Trouver une position contenant ce type ailleurs
  positions_cibles <- which(salle_new == type_cible, arr.ind = TRUE)
  if (nrow(positions_cibles) > 0) {
    cible <- positions_cibles[sample(1:nrow(positions_cibles), 1), ]
    row2 <- cible[1]
    col2 <- cible[2]
    
    # Échanger les deux étudiants
    temp <- salle_new[row1, col1]
    salle_new[row1, col1] <- salle_new[row2, col2]
    salle_new[row2, col2] <- temp
  }
  
  # Calculer la variation de bruit (delta)
  delta <- calculer_bruit_total(salle_old, bruits) - calculer_bruit_total(salle_new, bruits)
  
  # Probabilité d'acceptation
  alpha <- exp(beta * delta)
  u <- runif(1)
  
  if (u < alpha) {
    return(list(salle = salle_new, D = delta))
  } else {
    return(list(salle = salle_old, D = 0))
  }
}

# Optimisation du bruit avec l'algorithme du recuit
optimisation_bruit_recuit <- function(nb_iter, gamma, transition, bruits) {
  # Initialisation des variables pour suivre l'évolution du bruit
  bruit_evol <- rep(0, nb_iter)  # Vecteur pour stocker le bruit à chaque itération
  
  bruit_evol[1] <- calculer_bruit_total(salle_initiale, bruits)  # Calcul du bruit initial
  
  salle <- salle_initiale  # Matrice courante de la salle
  bruit_best <- bruit_evol[1]  # Meilleur bruit trouvé
  salle_best <- salle_initiale  # Configuration associée au meilleur bruit trouvé
  
  # Boucle d'optimisation par recuit simulé
  for (i in 2:nb_iter) {
    # Calcul du paramètre beta, qui diminue avec les itérations (dépendance logarithmique)
    beta <- gamma * log(i)
    
    # Effectuer une transition (permutation aléatoire dans la salle)
    result <- transition(salle, beta, bruits)
    
    # Mise à jour de la configuration courante
    salle <- result$salle
    bruit_evol[i] <- bruit_evol[i - 1] - result$D  # Mettre à jour le bruit total
    
    # Mise à jour du meilleur résultat si un bruit plus faible est trouvé
    if (bruit_evol[i] < bruit_best) {
      salle_best <- salle
      bruit_best <- bruit_evol[i]
    }
  }
  
  # Retourner les résultats : meilleure salle et le bruit associé
  return(list(salle = salle_best, bruit = bruit_best, bruit_evol = bruit_evol))
}

# Fonction pour visualiser la convergence avec plusieurs gammas
comparer_gamma <- function(gammas, nb_iter = 1000, transition, bruits) {
  # Créer une palette de couleurs, une couleur par gamma
  couleurs <- rainbow(length(gammas))
  
  plot(NULL, xlim = c(1, nb_iter), ylim = c(0, 1000),
       xlab = "Itérations", ylab = "Bruit Total",
       main = "Évolution du Bruit pour Différents Gamma")
  grid()
  
  # Boucle sur chaque gamma et tracer la courbe correspondante
  for (i in seq_along(gammas)) {
    gamma <- gammas[i]
    result <- optimisation_bruit_recuit(nb_iter, gamma, transition, bruits)
    lines(result$bruit_evol, type = "l", lwd = 2, col = couleurs[i], 
          label = paste("Gamma =", gamma))
  }
  
  # Ajouter la légende avec les couleurs correspondantes
  legend("topright", legend = paste("Gamma =", gammas), col = couleurs, lty = 1)
}

# Méthode dichotomique pour trouver le gamma optimal
trouver_gamma_optimal <- function(nb_iter = 500, tol = 1e-3, low, high, transition, bruits) {
  best_gamma <- NULL
  
  # Initialisation pour éviter de recalculer les mêmes valeurs
  cache <- list()
  
  # Fonction pour calculer le bruit avec mise en cache
  get_bruit <- function(gamma) {
    if (!is.null(cache[[as.character(gamma)]])) {
      return(cache[[as.character(gamma)]])
    }
    result <- optimisation_bruit_recuit(nb_iter, gamma, transition, bruits)$bruit
    cache[[as.character(gamma)]] <<- result
    return(result)
  }
  
  while ((high - low) > tol) {
    mid1 <- low + (high - low) / 3
    mid2 <- high - (high - low) / 3
    
    # Calculer les bruits correspondants
    bruit1 <- get_bruit(mid1)
    bruit2 <- get_bruit(mid2)
    
    # Affichage de l'état pour suivi
    cat(sprintf("Testing: low=%.3f, mid1=%.3f, mid2=%.3f, high=%.3f\n", low, mid1, mid2, high))
    cat(sprintf("Bruit1: %.3f, Bruit2: %.3f\n", bruit1, bruit2))
    
    # Mise à jour des bornes
    if (bruit1 < bruit2) {
      high <- mid2
      best_gamma <- mid1
    } else {
      low <- mid1
      best_gamma <- mid2
    }
  }
  
  return(best_gamma)
}

# Fonction qui change la configuration de la salle selon une transition et un bruit donnés
Evolution_bruit <- function(gamma_optimal, transition, bruits) {
  
  nb_iter = 10000
  
  # Exécuter l'optimisation par recuit
  result <- optimisation_bruit_recuit(nb_iter = nb_iter, gamma = gamma_optimal, transition, bruits)
  
  step <- 100
  
  # Déterminer le titre en fonction du nom de la transition
  transition_name <- gsub("_", " ", deparse(substitute(transition)))
  titre <- paste("Évolution du bruit au cours des itérations (", transition_name, ")", sep = "")
  
  # Initialiser le graphique pour la courbe d'évolution
  plot(
    1, 1,
    type = "n",  # Graphique vide
    xlab = "Nombre d'itérations",
    ylab = "Bruit total",
    main = titre,
    xlim = c(1, 10000),  # Ajuste selon nb_iter
    ylim = c(min(result$bruit_evol), max(result$bruit_evol)),  # Ajuste selon bruit_evol
    cex.main = 0.8  # Diminuer la taille de la police du titre
  )
  
  # Ajouter une grille
  grid(col = "gray", lty = "dotted")
  
  # Tracer l'évolution par lot
  for (i in seq(1, length(result$bruit_evol), by = step)) {
    lines(1:i, result$bruit_evol[1:i], col = "blue", lwd = 2)  # Ajouter une ligne jusqu'à i
    Sys.sleep(0.05)  # Pause pour visualiser
  }
  
  # Ajouter la légende finale
  legend(
    "topright",
    legend = c("Bruit total"),
    col = c("blue"),
    lwd = 2,
    bg = "white"
  )
  
  cat("Bruit initial :", result$bruit_evol[1], "\n")
  cat("Bruit final :", result$bruit_evol[nb_iter], "\n")
  cat("Réduction totale du bruit :", result$bruit_evol[1] - result$bruit_evol[nb_iter], "\n")
  
  
  # Combiner les graphes et ajouter une légende et un titre global
  final_plot <- ((generer_carte_bruit(salle_initiale, bruits) + 
                   theme(legend.position = "none", plot.title = element_blank())) | (generer_carte_bruit(result$salle, bruits) + 
                                                                                      theme(legend.position = "none", plot.title = element_blank()))) + 
    plot_layout(guides = "collect") &  # Collecter la légende pour les deux graphes
    theme(legend.position = "top")     # Positionner la légende en haut
  
  # Ajouter un titre global
  final_plot <- final_plot + 
    plot_annotation(title = "Comparaison des niveaux de bruit avant et après optimisation")
  final_plot # Afficher le résultat
  
}


# Fonction de sélection par tournoi
selection_tournoi <- function(population, bruits, taille_tournoi = 2) {
  candidats <- sample(1:length(population), taille_tournoi)
  scores <- sapply(candidats, function(i) calculer_bruit_total(population[[i]], bruits))
  return(population[[candidats[which.min(scores)]]])
}

# Fonction de croisement (crossover)
crossover <- function(parent1, parent2, n_A, n_B, n_C, n_D) {
  # Obtenir les dimensions des matrices (nombre de lignes et de colonnes)
  n_rows <- nrow(parent1)
  n_cols <- ncol(parent1)
  
  # Initialiser les enfants avec NA (matrices vides)
  enfant1 <- matrix(NA, nrow = n_rows, ncol = n_cols)
  enfant2 <- matrix(NA, nrow = n_rows, ncol = n_cols)
  
  # Initialiser les compteurs de lettres (A, B, C, D) dans un vecteur pour chaque enfant
  compteurs_enfant1 <- c(A = n_A, B = n_B, C = n_C, D = n_D)
  compteurs_enfant2 <- c(A = n_A, B = n_B, C = n_C, D = n_D)
  
  # Parcourir chaque case de la matrice pour identifier les cases communes
  for (i in 1:n_rows) {
    for (j in 1:n_cols) {
      if (parent1[i, j] == parent2[i, j]) {
        # Si les deux parents ont la même valeur, on l'assigne à la fois à enfant1 et enfant2
        lettre <- parent1[i, j]
        enfant1[i, j] <- lettre
        enfant2[i, j] <- lettre
        
        # Décrémenter le compteur de la lettre pour les deux enfants
        compteurs_enfant1[lettre] <- compteurs_enfant1[lettre] - 1
        compteurs_enfant2[lettre] <- compteurs_enfant2[lettre] - 1
      }
    }
  }
  
  # Parcourir chaque case encore pour gérer les différences entre les parents
  for (i in 1:n_rows) {
    for (j in 1:n_cols) {
      if (parent1[i, j] != parent2[i, j]) {
        
        lettre1 <- parent1[i, j]
        lettre2 <- parent2[i, j]
        
        # Choisir aléatoirement entre lettre1 et lettre2 si les deux sont disponibles pour l'enfant1
        if (compteurs_enfant1[lettre1] > 0 && compteurs_enfant1[lettre2] > 0) {
          # Choix aléatoire entre lettre1 et lettre2 pour l'enfant1
          if (runif(1) < 0.5) {
            enfant1[i, j] <- lettre1
            compteurs_enfant1[lettre1] <- compteurs_enfant1[lettre1] - 1
          } else {
            enfant1[i, j] <- lettre2
            compteurs_enfant1[lettre2] <- compteurs_enfant1[lettre2] - 1
          }
        } else if (compteurs_enfant1[lettre1] > 0) {
          # Si seule lettre1 est disponible, on la prend
          enfant1[i, j] <- lettre1
          compteurs_enfant1[lettre1] <- compteurs_enfant1[lettre1] - 1
        } else if (compteurs_enfant1[lettre2] > 0) {
          # Si seule lettre2 est disponible, on la prend
          enfant1[i, j] <- lettre2
          compteurs_enfant1[lettre2] <- compteurs_enfant1[lettre2] - 1
        } else {
          # Si aucune des deux lettres n'est disponible, on prend une lettre aléatoire parmi celles qui ont encore un compteur > 0
          lettres_disponibles <- names(compteurs_enfant1[compteurs_enfant1 > 0])
          lettre_choisie <- sample(lettres_disponibles, 1)
          enfant1[i, j] <- lettre_choisie
          compteurs_enfant1[lettre_choisie] <- compteurs_enfant1[lettre_choisie] - 1
        }
        
        # Choisir aléatoirement entre lettre1 et lettre2 si les deux sont disponibles pour l'enfant2
        if (compteurs_enfant2[lettre1] > 0 && compteurs_enfant2[lettre2] > 0) {
          # Choix aléatoire entre lettre1 et lettre2 pour l'enfant2
          if (runif(1) < 0.5) {
            enfant2[i, j] <- lettre1
            compteurs_enfant2[lettre1] <- compteurs_enfant2[lettre1] - 1
          } else {
            enfant2[i, j] <- lettre2
            compteurs_enfant2[lettre2] <- compteurs_enfant2[lettre2] - 1
          }
        } else if (compteurs_enfant2[lettre1] > 0) {
          # Si seule lettre1 est disponible pour l'enfant2, on la prend
          enfant2[i, j] <- lettre1
          compteurs_enfant2[lettre1] <- compteurs_enfant2[lettre1] - 1
        } else if (compteurs_enfant2[lettre2] > 0) {
          # Si seule lettre2 est disponible pour l'enfant2, on la prend
          enfant2[i, j] <- lettre2
          compteurs_enfant2[lettre2] <- compteurs_enfant2[lettre2] - 1
        } else {
          # Si aucune des deux lettres n'est disponible pour l'enfant2, on prend une lettre aléatoire parmi celles qui ont encore un compteur > 0
          lettres_disponibles <- names(compteurs_enfant2[compteurs_enfant2 > 0])
          lettre_choisie <- sample(lettres_disponibles, 1)
          enfant2[i, j] <- lettre_choisie
          compteurs_enfant2[lettre_choisie] <- compteurs_enfant2[lettre_choisie] - 1
        }
      }
    }
  }
  
  # Retourner les matrices des deux enfants
  return(list(enfant1 = enfant1, enfant2 = enfant2))
}

# Fonction de mutation
mutation <- function(individu) {
  
  # Calculer les dimensions de la matrice
  n_rows <- nrow(individu)
  n_cols <- ncol(individu)
  
  # Appliquer la mutation avec une faible probabilité
  if (runif(1) < 0.01) {  # Probabilité de mutation = 1%
    # Sélectionner deux positions aléatoires dans la matrice
    pos1 <- sample(1:(n_rows * n_cols), 1)
    pos2 <- sample(1:(n_rows * n_cols), 1)
    
    # Transformer les positions en indices (ligne, colonne)
    row1 <- ((pos1 - 1) %% n_rows) + 1
    col1 <- ((pos1 - 1) %/% n_rows) + 1
    row2 <- ((pos2 - 1) %% n_rows) + 1
    col2 <- ((pos2 - 1) %/% n_rows) + 1
    
    # Échanger les valeurs entre les deux positions
    temp <- individu[row1, col1]
    individu[row1, col1] <- individu[row2, col2]
    individu[row2, col2] <- temp
    
  }
  
  return(individu)
}

# Algorithme génétique
algorithme_genetique <- function(population_size, nb_generations, bruits, n_A, n_B, n_C, n_D) {
  population <- list()
  
  # Initialiser la population
  for (i in 1:population_size) {
    categories <- c(rep("A", n_A), rep("B", n_B), rep("C", n_C), rep("D", n_D))
    salle <- sample(categories, size = n_rows * n_cols, replace = FALSE)
    population[[i]] <- matrix(salle, nrow = n_rows, ncol = n_cols)
  }
  
  meilleur_individu <- NULL
  meilleur_bruit <- Inf
  
  # Boucle principale de l'algorithme génétique
  for (generation in 1:nb_generations) {
    parent1 <- selection_tournoi(population, bruits)
    parent2 <- selection_tournoi(population, bruits)
    
    # Effectuer un croisement
    enfants <- crossover(parent1, parent2, n_A, n_B, n_C, n_D)
    
    # Appliquer la mutation
    enfants <- lapply(enfants, mutation)
    
    # Calculer les scores de bruit pour les enfants
    scores_enfants <- sapply(enfants, function(e) calculer_bruit_total(e, bruits))
    
    # Sélectionner le meilleur enfant
    best_enfant_idx <- which.min(scores_enfants)
    meilleur_enfant <- enfants[[best_enfant_idx]]
    meilleur_score <- scores_enfants[best_enfant_idx]
    
    # Mettre à jour le meilleur individu si nécessaire
    if (meilleur_score < meilleur_bruit) {
      meilleur_individu <- meilleur_enfant
      meilleur_bruit <- meilleur_score
    }
    
    # Remplacer un individu dans la population
    population[[which.max(sapply(population, function(ind) calculer_bruit_total(ind, bruits)))]] <- meilleur_enfant
  }
  
  return(list(salle = meilleur_individu, bruit = meilleur_bruit))
}



# On a défini 3 matrices de bruits différentes pour modéliser une gamme variée de 
# perturbations possibles :

# ========================================================================================
#                                   Bruit 1
# ========================================================================================

# =========================== Bruit Initial =========================== #

# Matrice des niveaux de bruit caractérisée par des interactions modérées 
# entre les zones :

# 4 groupes d'éleves: 
#         - Très bavards : A
#         - Bavards : B
#         - Timides : C
#         - Places Vides 
bruits1 <- matrix(c(
  10, 6, 4, 0,
  6,  4, 2, 0,
  4,  2, 1, 0,
  0,  0, 0, 0
), nrow = 4, ncol = 4, byrow = TRUE)

# Nommer les colonnes et les lignes de la matrice
colnames(bruits1) <- rownames(bruits1) <- c("A", "B", "C", "D")

# Génération de la carte de bruit initiale
generer_carte_bruit(salle_initiale, bruits1)
g1_ini <- generer_carte_bruit(salle_initiale, bruits1) + 
  theme(legend.position = "none", plot.title = element_blank())

# ======================== Gamma Optimal (Basique) ======================== #

# Visualisation dynamique de la convergence pour différents gammas
gammas <- c(0.01, 0.1, 1, 10, 100)
comparer_gamma(gammas, nb_iter = 5000, transition_basique, bruits1)

# Sélection visuelle du gamma optimal autour de 1

# Méthode dichotomique pour trouver le gamma optimal
gamma_optimal <- trouver_gamma_optimal(
  nb_iter = 500, 
  tol = 1e-3, 
  low = 0.5, 
  high = 1.5, 
  transition_basique, 
  bruits1
)
cat("Gamma optimal :", gamma_optimal, "\n")

# ================= Algorithme du Recuit Simulé (Basique) ================= #

# Évolution de la réduction du bruit avec le gamma optimal
Evolution_bruit(gamma_optimal, transition_basique, bruits1)

# =================== Gamma Optimal (Rotation 180°) ==================== #

# Visualisation dynamique de la convergence pour différents gammas
gammas <- c(0.01, 0.1, 1, 10, 100)
comparer_gamma(gammas, nb_iter = 5000, transition_rotation, bruits1)

# Sélection visuelle du gamma optimal autour de 10

# Méthode dichotomique pour trouver le gamma optimal (rotation)
gamma_optimal <- trouver_gamma_optimal(
  nb_iter = 500, 
  tol = 1e-3, 
  low = 9, 
  high = 11, 
  transition_rotation, 
  bruits1
)
cat("Gamma optimal (rotation de 180°) :", gamma_optimal, "\n")

# ========= Algorithme du Recuit Simulé (Rotation 180°) ========= #

# Évolution de la réduction du bruit avec le gamma optimal (rotation)
Evolution_bruit(gamma_optimal, transition_rotation, bruits1)

# =================== Gamma Optimal (ciblée) ==================== #

# Visualisation dynamique de la convergence pour différents gammas
gammas <- c(0.01, 0.1, 1, 10, 100)
comparer_gamma(gammas, nb_iter = 5000, transition_ciblee, bruits1)

# Sélection visuelle du gamma optimal autour de 1

# Méthode dichotomique pour trouver le gamma optimal (rotation)
gamma_optimal <- trouver_gamma_optimal(
  nb_iter = 500, 
  tol = 1e-3, 
  low = 0.5, 
  high = 1.5, 
  transition_ciblee, 
  bruits1
)
cat("Gamma optimal (ciblee) :", gamma_optimal, "\n")

# ========= Algorithme du Recuit Simulé (ciblee) ========= #

# Évolution de la réduction du bruit avec le gamma optimal (ciblee)
Evolution_bruit(gamma_optimal, transition_ciblee, bruits1)

# =================== Algorithme Génétique =================== #
resultat_genetique <- algorithme_genetique(population_size = 20, nb_generations = 1000, bruits = bruits1, n_A = n_A, n_B = n_B, n_C = n_C, n_D = n_D)

# Affichage du meilleur individu trouvé
cat("Meilleur bruit trouvé avec l'algorithme génétique:", resultat_genetique$bruit, "\n")

# Visualiser les salles avant et après optimisation
g1_gene <- generer_carte_bruit(resultat_genetique$salle, bruits1) + 
  theme(legend.position = "none", plot.title = element_blank()) 

# Combiner les graphes et ajouter une légende et un titre global
final_plot <- (g1_ini | g1_gene) + 
  plot_layout(guides = "collect") &  # Collecter la légende pour les deux graphes
  theme(legend.position = "top")     # Positionner la légende en haut

# Ajouter un titre global
final_plot <- final_plot + 
  plot_annotation(title = "Comparaison des niveaux de bruit avant et après optimisation")
final_plot # Afficher le résultat

# ========================================================================================
#                                   Bruit 2
# ========================================================================================

# =========================== Bruit Initial =========================== #

# Matrice des niveaux de bruit où les interactions entre toutes les zones 
# sont équilibrées sans dominance particulière
# => les étudiants d'une même catégorie côte à côte génèrent bcp de bruits

bruits2 <- matrix(c(
  10, 2, 2, 0,
  2, 10, 2, 0,
  2, 2, 10, 0,
  0, 0, 0, 0
), nrow = 4, ncol = 4, byrow = TRUE)

# Nommer les colonnes et les lignes de la matrice
colnames(bruits2) <- rownames(bruits2) <- c("A", "B", "C", "D")

# Génération de la carte de bruit initiale
generer_carte_bruit(salle_initiale, bruits2)
g2_ini <- generer_carte_bruit(salle_initiale, bruits2) + 
  theme(legend.position = "none", plot.title = element_blank()) 

# ======================== Gamma Optimal (Basique) ======================== #

# Visualisation dynamique de la convergence pour différents gammas
gammas <- c(0.01, 0.1, 1, 10, 100)
comparer_gamma(gammas, nb_iter = 5000, transition_basique, bruits2)

# Sélection visuelle du gamma optimal autour de 0.1

# Méthode dichotomique pour trouver le gamma optimal
gamma_optimal <- trouver_gamma_optimal(
  nb_iter = 500, 
  tol = 1e-3, 
  low = 0.05, 
  high = 0.15, 
  transition_basique, 
  bruits2
)
cat("Gamma optimal :", gamma_optimal, "\n")

# ================= Algorithme du Recuit Simulé (Basique) ================= #

# Évolution de la réduction du bruit avec le gamma optimal
Evolution_bruit(gamma_optimal, transition_basique, bruits2)

# =================== Gamma Optimal (Rotation 180°) ==================== #

# Visualisation dynamique de la convergence pour différents gammas
gammas <- c(0.01, 0.1, 1, 10, 100)
comparer_gamma(gammas, nb_iter = 5000, transition_rotation, bruits2)

# Sélection visuelle du gamma optimal autour de 100

# Méthode dichotomique pour trouver le gamma optimal (rotation)
gamma_optimal <- trouver_gamma_optimal(
  nb_iter = 500, 
  tol = 1e-3, 
  low = 90, 
  high = 110, 
  transition_rotation, 
  bruits2
)
cat("Gamma optimal (rotation de 180°) :", gamma_optimal, "\n")

# ========= Algorithme du Recuit Simulé (Rotation 180°) ========= #

# Évolution de la réduction du bruit avec le gamma optimal (rotation)
Evolution_bruit(gamma_optimal, transition_rotation, bruits2)

# =================== Gamma Optimal (ciblée) ==================== #

# Visualisation dynamique de la convergence pour différents gammas
gammas <- c(0.01, 0.1, 1, 10, 100)
comparer_gamma(gammas, nb_iter = 5000, transition_ciblee, bruits2)

# Sélection visuelle du gamma optimal autour de 0.1

# Méthode dichotomique pour trouver le gamma optimal (rotation)
gamma_optimal <- trouver_gamma_optimal(
  nb_iter = 500, 
  tol = 1e-3, 
  low = 0.05, 
  high = 0.15, 
  transition_ciblee, 
  bruits2
)
cat("Gamma optimal (ciblee) :", gamma_optimal, "\n")

# ========= Algorithme du Recuit Simulé (ciblee) ========= #

# Évolution de la réduction du bruit avec le gamma optimal (ciblee)
Evolution_bruit(gamma_optimal, transition_ciblee, bruits2)

# =================== Algorithme Génétique =================== #
resultat_genetique <- algorithme_genetique(population_size = 20, nb_generations = 1000, bruits = bruits2, n_A = n_A, n_B = n_B, n_C = n_C, n_D = n_D)

# Affichage du meilleur individu trouvé
cat("Meilleur bruit trouvé avec l'algorithme génétique:", resultat_genetique$bruit, "\n")

# Visualiser les salles avant et après optimisation
g2_gene <- generer_carte_bruit(resultat_genetique$salle, bruits2) + 
  theme(legend.position = "none", plot.title = element_blank()) 

# Combiner les graphes et ajouter une légende et un titre global
final_plot <- (g2_ini | g2_gene) + 
  plot_layout(guides = "collect") &  # Collecter la légende pour les deux graphes
  theme(legend.position = "top")     # Positionner la légende en haut

# Ajouter un titre global
final_plot <- final_plot + 
  plot_annotation(title = "Comparaison des niveaux de bruit avant et après optimisation")
final_plot # Afficher le résultat

# ========================================================================================
#                                   Bruit 3
# ========================================================================================

# =========================== Bruit Initial =========================== #

# Matrice des niveaux de bruit caractérisée par des interactions modérées 
# entre les zones
# => les étudiants d'une même catégorie ne se parlent pas du tout
#    ils ne parlent qu'avec les autes catégories
bruits3 <- matrix(c(
  0, 6, 4, 0,
  6, 0, 2, 0,
  4, 2, 0, 0,
  0, 0, 0, 0
), nrow = 4, ncol = 4, byrow = TRUE)

# Nommer les colonnes et les lignes de la matrice
colnames(bruits3) <- rownames(bruits3) <- c("A", "B", "C", "D")

# Génération de la carte de bruit initiale
generer_carte_bruit(salle_initiale, bruits3)
g3_ini <- generer_carte_bruit(salle_initiale, bruits3) + 
  theme(legend.position = "none", plot.title = element_blank()) 

# ======================== Gamma Optimal (Basique) ======================== #

# Visualisation dynamique de la convergence pour différents gammas
gammas <- c(0.01, 0.1, 1, 10, 100)
comparer_gamma(gammas, nb_iter = 5000, transition_basique, bruits3)

# Sélection visuelle du gamma optimal autour de 100

# Méthode dichotomique pour trouver le gamma optimal
gamma_optimal <- trouver_gamma_optimal(
  nb_iter = 500, 
  tol = 1e-3, 
  low = 90, 
  high = 110, 
  transition_basique, 
  bruits3
)
cat("Gamma optimal :", gamma_optimal, "\n")

# ================= Algorithme du Recuit Simulé (Basique) ================= #

# Évolution de la réduction du bruit avec le gamma optimal
Evolution_bruit(gamma_optimal, transition_basique, bruits3)

# =================== Gamma Optimal (Rotation 180°) ==================== #

# Visualisation dynamique de la convergence pour différents gammas
gammas <- c(0.01, 0.1, 1, 10, 100)
comparer_gamma(gammas, nb_iter = 5000, transition_rotation, bruits3)

# Sélection visuelle du gamma optimal autour de 1

# Méthode dichotomique pour trouver le gamma optimal (rotation)
gamma_optimal <- trouver_gamma_optimal(
  nb_iter = 500, 
  tol = 1e-3, 
  low = 0.5, 
  high = 1.5, 
  transition_rotation, 
  bruits3
)
cat("Gamma optimal (rotation de 180°) :", gamma_optimal, "\n")

# ========= Algorithme du Recuit Simulé (Rotation 180°) ========= #

# Évolution de la réduction du bruit avec le gamma optimal (rotation)
Evolution_bruit(gamma_optimal, transition_rotation, bruits3)

# =================== Gamma Optimal (ciblée) ==================== #

# Visualisation dynamique de la convergence pour différents gammas
gammas <- c(0.01, 0.1, 1, 10, 100)
comparer_gamma(gammas, nb_iter = 5000, transition_ciblee, bruits3)

# Sélection visuelle du gamma optimal autour de 1

# Méthode dichotomique pour trouver le gamma optimal (rotation)
gamma_optimal <- trouver_gamma_optimal(
  nb_iter = 500, 
  tol = 1e-3, 
  low = 0.5, 
  high = 1.5, 
  transition_ciblee, 
  bruits3
)
cat("Gamma optimal (ciblee) :", gamma_optimal, "\n")

# ========= Algorithme du Recuit Simulé (ciblee) ========= #

# Évolution de la réduction du bruit avec le gamma optimal (ciblee)
Evolution_bruit(gamma_optimal, transition_ciblee, bruits3)

# =================== Algorithme Génétique =================== #
resultat_genetique <- algorithme_genetique(population_size = 20, nb_generations = 1000, bruits = bruits3, n_A = n_A, n_B = n_B, n_C = n_C, n_D = n_D)

# Affichage du meilleur individu trouvé
cat("Meilleur bruit trouvé avec l'algorithme génétique:", resultat_genetique$bruit, "\n")

# Visualiser les salles avant et après optimisation
g3_gene <- generer_carte_bruit(resultat_genetique$salle, bruits3) + 
  theme(legend.position = "none", plot.title = element_blank()) 

# Combiner les graphes et ajouter une légende et un titre global
final_plot <- (g3_ini | g3_gene) + 
  plot_layout(guides = "collect") &  # Collecter la légende pour les deux graphes
  theme(legend.position = "top")     # Positionner la légende en haut

# Ajouter un titre global
final_plot <- final_plot + 
  plot_annotation(title = "Comparaison des niveaux de bruit avant et après optimisation")
final_plot # Afficher le résultat
