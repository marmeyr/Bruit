# Optimisation du Bruit dans une Salle de Classe

## Contexte
Ce projet a été réalisé durant ma deuxième année de master, dans le cadre de l'Unité d'Enseignement *Simulation et Copules*. Il vise à appliquer des méthodes stochastiques et heuristiques pour résoudre un problème d'optimisation en réorganisant les élèves dans une salle de classe afin de minimiser l'impact sonore.

J'ai mené ce projet en collaboration avec **Shuaib Mohun**, et il a fait l'objet d'une présentation orale dans le cadre de notre cursus.

## Objectif
L'objectif principal est de réduire le bruit total dans une salle de classe en réorganisant les élèves selon des critères définis, à l'aide d'algorithmes adaptés.

## Algorithmes Explorés
Pour atteindre cet objectif, nous avons implémenté et comparé plusieurs algorithmes d'optimisation :
1. **Recuit Simulé Basique** : On sélectionne un étudiant au hasard qu'on permute avec un autre étudiant.
2. **Recuit Simulé Ciblé** : On sélectionne un étudiant au hasard, on regarde la catégorie de ses voisins qui est le moins représenter. Puis on regarde tous les étudiants de cette catégorie dans la salle, on en sélectionne un au hasard qu'on va permuter avec notre étudiant de base.
3. **Recuit Simulé avec Rotation de 180°** : On fait une roation de 180°.
4. **Algorithme Génétique** : Une méthode évolutive basée sur la sélection, le croisement et la mutation.

## Contenu
Ce dépôt contient :
- Le code R complet pour chaque algorithme.
- Des fonctions pour visualiser les résultats et analyser la convergence.
- Les données nécessaires à la simulation.
- Des scripts permettant de comparer les différentes approches.

## Accès au Code
Vous pouvez consulter le code R utilisé dans ce projet directement sur ce dépôt GitHub.
