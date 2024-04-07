  !!!!!!!!!!!!!!!!!
  ! MODULE PACMAN !
  !!!!!!!!!!!!!!!!!

MODULE mod_pacman
    IMPLICIT NONE
    
    ! Type derive pour decrire un individu
    TYPE Individu
        ! categprie : vaut 0, 1 ou 2 selon que l’individu est le joueur, une friandise ou un fantome
        ! pos       : position
        ! old_pos   : position a l'instant precedent
        ! symbole   : symbole utilise pour representer l'individu
        INTEGER               :: categorie
        INTEGER, DIMENSION(2) :: pos
        INTEGER, DIMENSION(2) :: old_pos
        CHARACTER             :: symbol
    END TYPE Individu
  

    ! Type derive pour decrire le domaine
    TYPE Domaine
        ! ng        : taille de la grille carree
        ! nfri      : contient le nombre de friandises dans le domaine
        ! nfant     : contient le nombre de fantomes dans le domaine
        ! joueur    : individu unique representant le joueur
        ! friandise : un tableau de nfri individus representant les friandises
        ! fantome   : un tableau de nfant individus representant les fant ´ omes
        ! grille    : cases du domaine pour affichage a l'ecran
        INTEGER                                   :: ng
        INTEGER                                   :: nfri
        INTEGER                                   :: nfant
        TYPE(Individu)                            :: joueur
        TYPE(Individu), DIMENSION(:), ALLOCATABLE :: friandise
        TYPE(Individu), DIMENSION(:), ALLOCATABLE :: fantome
        TYPE(Individu), DIMENSION(32)             :: portail
        CHARACTER, DIMENSION(:,:), ALLOCATABLE    :: grille
    END TYPE Domaine
  
    CONTAINS
  
    ! Initaliser de façon aleatoire un individu (Par exemple une friandise)
    SUBROUTINE InitialiserPosition(unindividu, undomaine)
        TYPE(Individu), INTENT(INOUT) :: unindividu
        TYPE(Domaine), INTENT(INOUT)  :: undomaine
        INTEGER                       :: i1, i2
        REAL                          :: x, y
  
        ! Génération de nombres aléatoires
        CALL RANDOM_NUMBER(x)
        CALL RANDOM_NUMBER(y) 
    
        ! Générer des coordonnées aléatoires dans les limites du domaine
        i1 = FLOOR(x * (undomaine%ng - 1)) + 1 
        i2 = FLOOR(y * (undomaine%ng - 1)) + 1
    
        !S'assurer que la position générée est valide
        !On verifie que l'individu occupe une place libre , si ce n'est pas le cas alors on regenere d'autres coordonées de façon aleatoire
        !Génération de nombres aléatoires
        DO WHILE (undomaine%grille(i1, i2) /= ' ')                                     
            CALL RANDOM_NUMBER(x)
            CALL RANDOM_NUMBER(y) 
            i1 = FLOOR(x * (undomaine%ng - 1)) + 1
            i2 = FLOOR(y * (undomaine%ng - 1)) + 1
        END DO
    
        undomaine%grille(i1, i2) = unindividu%symbol
        unindividu%pos(1) = i1
        unindividu%pos(2) = i2
    END SUBROUTINE InitialiserPosition
  
    ! Affichage de la grille
    SUBROUTINE affichage(undomaine)
        TYPE(domaine), INTENT(INOUT)  :: undomaine
        INTEGER                       :: i, j
        PRINT*,"------------------------------------------------"
        DO i = 0, undomaine%ng + 1
            DO j = 0, undomaine%ng + 1 
            IF (i == 0 .AND. (j == 0 .OR. j == SIZE(undomaine%grille, 2))) THEN
                WRITE(*, '(1000A2)', ADVANCE='NO') "|"
            ELSE IF (i == SIZE(undomaine%grille, 1) .AND. (j == 1 .OR. j == SIZE(undomaine%grille, 2))) THEN
                WRITE(*, '(1000A2)', ADVANCE='NO') "|"
            ELSE
                WRITE(*, '(1000A2)', ADVANCE='NO') undomaine%grille(i, j)
            ENDIF
            END DO 
            PRINT*
        END DO 
        PRINT*,"------------------------------------------------------------------"
    END SUBROUTINE affichage
  
    ! Initialisation du domaine et creation du cadre
    SUBROUTINE cadre(undomaine) 
        TYPE(domaine), INTENT(INOUT)  :: undomaine
        INTEGER                       :: i, j
        DO i = 0 , SIZE(undomaine%grille,1)
            DO j = 0 , SIZE(undomaine%grille,2)
            undomaine%grille(i,j) = " "
            END DO 
        END DO 
        DO j = 0 , undomaine%ng + 1 
            undomaine%grille(0, j) = "-"            ! On rajoute le plafond en haut
            undomaine%grille(undomaine%ng+1,j) = "-"! On rajoute le sol en bas
        END DO
        DO i = 0, undomaine%ng + 1 
            undomaine%grille(i, 0) = "|"             ! On rajoute les murs à gauche
            undomaine%grille(i, undomaine%ng+1) = "|"! On rajoute les murs à droite
        END DO
    END SUBROUTINE cadre
    
    
    ! Sous-programme pour le déplacement du joueur
    SUBROUTINE deplacement_joueur(joueur, taille, dom, depl,touche)
        TYPE(domaine), INTENT(INOUT)      :: dom
        TYPE(Individu), INTENT(INOUT)     :: joueur
        INTEGER, INTENT(INOUT)            :: depl
        INTEGER, INTENT(IN)               :: taille
        CHARACTER ,INTENT(INOUT)          :: touche
        LOGICAL                           :: traverser_active = .TRUE.
        INTEGER                           :: retval, tsec = 0, tmicrosec = 150000, selec_get
        
        ! Deplacement du joueur
        retval = selec_get(touche, tsec, tmicrosec)
        ! Renvoie la valeur retval = 0 si aucune touche n’a été frappée, retval = 1 dans le cas contraire
    
        SELECT CASE (retval)
        CASE (0)
            PRINT*, "Aucune touche n'a ete frapee"
    
        CASE (1)
            dom%joueur%old_pos = dom%joueur%pos
    
            SELECT CASE (touche)
            ! Aller à droite
            CASE ('d', 'D')
                PRINT*, "Aller a droite"
                IF (dom%joueur%pos(2) < dom%ng .AND. dom%grille(dom%joueur%pos(1), dom%joueur%pos(2) + 1) /= '|'&
                .AND. dom%grille(dom%joueur%pos(1), dom%joueur%pos(2) + 1) /= '-' ) THEN
                    dom%joueur%old_pos = dom%joueur%pos ! Sauvegarde de la position precedente
                    dom%joueur%pos(2) = dom%joueur%pos(2) + 1
                    depl=depl+1  
                ENDIF
    
            ! Aller à gauche
            CASE ('q', 'Q')
                PRINT*, "Aller a gauche"
                IF (dom%joueur%pos(2) > 1 .AND. dom%grille(dom%joueur%pos(1), dom%joueur%pos(2) - 1) /= '|'&
                .AND. dom%grille(dom%joueur%pos(1), dom%joueur%pos(2) - 1) /= '-') THEN
                    dom%joueur%old_pos = dom%joueur%pos ! Sauvegarde de la position precedente
                    dom%joueur%pos(2) = dom%joueur%pos(2) - 1
                    depl=depl+1  
                ENDIF
    
            ! Aller en haut
            CASE ('z', 'Z')
                PRINT*, "Aller en haut"
                IF (dom%joueur%pos(1) > 1 .AND. dom%grille(dom%joueur%pos(1) - 1, dom%joueur%pos(2)) /= '-' &
                .AND. dom%grille(dom%joueur%pos(1) - 1, dom%joueur%pos(2)) /= '|') THEN
                    dom%joueur%old_pos = dom%joueur%pos ! Sauvegarde de la position precedente
                    dom%joueur%pos(1) = dom%joueur%pos(1) - 1
                    depl=depl+1  
                ENDIF
    
            ! Aller en bas
            CASE ('s', 'S')
                PRINT*, "Aller en bas"
                IF (dom%joueur%pos(1) < dom%ng .AND. dom%grille(dom%joueur%pos(1) + 1, dom%joueur%pos(2)) /= '-' &
                    .AND. dom%grille(dom%joueur%pos(1) + 1, dom%joueur%pos(2)) /= '|') THEN
                    dom%joueur%old_pos = dom%joueur%pos ! Sauvegarde de la position precedente
                    dom%joueur%pos(1) = dom%joueur%pos(1) + 1
                    depl=depl+1  
                ENDIF

            CASE (' ')
                PRINT*, "Arret du programme"
                STOP

            CASE DEFAULT
                PRINT*, "Erreur"

            END SELECT
            
            ! Vérification si la prochaine position contient le symbole "+"
            IF (.NOT. traverser_active) THEN
                IF (dom%grille(dom%joueur%pos(1), dom%joueur%pos(2)) == "+") THEN
                    PRINT*, "Impossible de traverser le symbole '+' !"
                    RETURN  ! Annuler le déplacement du joueur
                END IF
    
                ! Vérification si la prochaine position en haut contient le symbole "+"
                IF (dom%grille(dom%joueur%pos(1) - 1, dom%joueur%pos(2)) == "+") THEN
                    PRINT*, "Impossible de traverser le symbole '+' en haut !"
                    RETURN  ! Annuler le déplacement du joueur
                END IF
    
                ! Vérification si la prochaine position en bas contient le symbole "+"
                IF (dom%grille(dom%joueur%pos(1) + 1, dom%joueur%pos(2)) == "+") THEN
                    PRINT*, "Impossible de traverser le symbole '+' en bas !"
                    RETURN  ! Annuler le déplacement du joueur
                END IF
    
                ! Vérification si la prochaine position à gauche contient le symbole "+"
                IF (dom%grille(dom%joueur%pos(1), dom%joueur%pos(2) - 1) == "+") THEN
                    PRINT*, "Impossible de traverser le symbole '+' à gauche !"
                    RETURN  ! Annuler le déplacement du joueur
                END IF
    
                ! Vérification si la prochaine position à droite contient le symbole "+"
                IF (dom%grille(dom%joueur%pos(1), dom%joueur%pos(2) + 1) == "+") THEN
                    PRINT*, "Impossible de traverser le symbole '+' à droite !"
                    RETURN  ! Annuler le déplacement du joueur
                END IF
            END IF
    
        
            ! Mise à jour de la position actuelle du joueur
            PRINT*, "Position du joueur", dom%joueur%pos
            dom%grille(dom%joueur%old_pos(1), dom%joueur%old_pos(2)) = " "
            dom%grille(dom%joueur%pos(1), dom%joueur%pos(2)) = "@"

        END SELECT
  
    END SUBROUTINE deplacement_joueur
  
    ! Déplacement aléatoire des fantômes
    SUBROUTINE avance_aleatoire(ind, taille, joueur, dom, depl)
        TYPE(Individu), INTENT(INOUT)                                   :: ind
        INTEGER, INTENT(IN)                                             :: taille
        TYPE(Individu), INTENT(INOUT)                                   :: joueur
        TYPE(Domaine), INTENT(INOUT)                                    :: dom
        INTEGER, INTENT(INOUT)                                          :: depl 
        INTEGER                                                         :: friandises_restantes
        INTEGER                                                         :: i_rand, j_rand
        REAL                                                            :: prob_dir
          
        friandises_restantes=0

        ! Sauvegarde de la position actuelle du fantôme
        ind%old_pos = ind%pos
         
        ! Génération d'un nombre aléatoire pour déterminer la direction
        CALL RANDOM_NUMBER(prob_dir)
  
        ! Génération de coordonnées aléatoires dans les 8 directions possibles
        !Selon la probabilité on choisit une des directions 
        IF (prob_dir < 0.125) THEN
            i_rand = -1
            j_rand = -1
        ELSE IF (prob_dir < 0.25) THEN
            i_rand = -1
            j_rand = 0
        ELSE IF (prob_dir < 0.375) THEN
            i_rand = -1
            j_rand = 1
        ELSE IF (prob_dir < 0.5) THEN
            i_rand = 0
            j_rand = -1
        ELSE IF (prob_dir < 0.625) THEN
            i_rand = 0
            j_rand = 1
        ELSE IF (prob_dir < 0.75) THEN
            i_rand = 1
            j_rand = -1
        ELSE IF (prob_dir < 0.875) THEN
            i_rand = 1
            j_rand = 0
        ELSE
            i_rand = 1
            j_rand = 1
        END IF
  
        ! Vérifier que la nouvelle position est valide (pas de mur et pas de friandise)
        IF (ind%pos(1) + i_rand >= 1 .AND. ind%pos(1) + i_rand <= taille .AND. &
            ind%pos(2) + j_rand >= 1 .AND. ind%pos(2) + j_rand <= taille .AND. &
            dom%grille(ind%pos(1) + i_rand, ind%pos(2) + j_rand) /= "|" .AND. &
            dom%grille(ind%pos(1) + i_rand, ind%pos(2) + j_rand) /= "-" .AND. &
            dom%grille(ind%pos(1) + i_rand, ind%pos(2) + j_rand) /= "O" .AND. &
            dom%grille(ind%pos(1) + i_rand, ind%pos(2) + j_rand) /= "+") THEN
            ! Mise à jour de la position du fantôme
            ind%pos(1) = ind%pos(1) + i_rand
            ind%pos(2) = ind%pos(2) + j_rand
  
            ! Mettre à jour la grille avec la nouvelle position du fantôme
            dom%grille(ind%old_pos(1), ind%old_pos(2)) = " "
            dom%grille(ind%pos(1), ind%pos(2)) = ind%symbol

              
        END IF

    END SUBROUTINE avance_aleatoire
  
    ! Place les murs sur le domaine
    SUBROUTINE placer_murs(dom)
        TYPE(Domaine), INTENT(INOUT)        :: dom
        INTEGER                             :: i, j 
        dom%portail%symbol = "+"
          
        ! Motif avec des murs et un espace au centre
        dom%grille(1:3, 1:3) = "+" 
        dom%grille(2, 2) = " "
  
        ! Copier le motif aux autres coins de la grille
        dom%grille(1:3, dom%ng-2:dom%ng) = dom%grille(1:3, 1:3)
        dom%grille(dom%ng-2:dom%ng, 1:3) = dom%grille(1:3, 1:3)
        dom%grille(dom%ng-2:dom%ng, dom%ng-2:dom%ng) = dom%grille(1:3, 1:3)
          
        ! Croix au centre
        DO i = dom%ng/2 - 3, dom%ng/2 + 3
            dom%grille(i, dom%ng/2) = "|"
            dom%grille(dom%ng/2, i) = "-"
        END DO
          
  
        ! Ligne de tirets en haut
        dom%grille(2, 6:dom%ng-6) = "-"
        ! Ligne de tirets en bas
        dom%grille(dom%ng-1, 6:dom%ng-6) = "-"
  
    END SUBROUTINE placer_murs
    
    ! Interception des friandises par le joueur  
    SUBROUTINE Gobe_friandise(dom,friandises_restantes,friandises_croquees)
        TYPE(domaine), INTENT(INOUT)                                            :: dom
        INTEGER , INTENT(INOUT)                                                 :: friandises_restantes, friandises_croquees
        INTEGER                                                                 :: i
  
        DO i = 1, dom%nfri
            ! Condition vérifiant si la position précédente d'une friandise correspond à la position actuelle du joueur 
            IF ((dom%friandise(i)%old_pos(1) == dom%joueur%pos(1)) .AND. (dom%friandise(i)%old_pos(2) == dom%joueur%pos(2))) THEN
                friandises_croquees = friandises_croquees + 1
                ! Mettre à jour le nombre de friandises restantes
                friandises_restantes = friandises_restantes - 1
                ! Supprimer la friandise de la grille
                dom%grille(dom%friandise(i)%old_pos(1), dom%friandise(i)%old_pos(2)) = "."
                ! Supprimer la friandise du tableau
                dom%friandise(i) = dom%friandise(dom%nfri)
                ! Réduire le nombre total de friandises
                dom%nfri = dom%nfri - 1
                ! Ne pas oublier de sortir de la boucle une fois que la friandise est trouvée et traitée
                EXIT
            END IF
        END DO
      
        ! Affichage du nombre de friandises restantes et croquées
        PRINT*, "Nombre de friandises restantes :", friandises_restantes
        PRINT*, "Nombre de friandises croquees :", friandises_croquees
    END SUBROUTINE Gobe_friandise

    ! Création des portails 
    SUBROUTINE traverse(dom, joueur)
        TYPE(domaine), INTENT(INOUT)                :: dom 
        TYPE(individu), INTENT(INOUT)               :: joueur
        INTEGER                                     :: i , j
          
        DO i = 1, 3
            DO j = 1, 3
                ! Côté supérieur gauche
                IF (i == 2 .AND. j == 2) THEN
                ! Ne rien faire pour le centre (laissez-le vide)
                ELSE IF(dom%grille(i,j) == " ") THEN
                ! Remplacez les autres cases par le symbole "+"
                    dom%grille(i, j) = "+"
                END IF
                ! Côté supérieur droit
                IF (i == 2 .AND. j == 2) THEN
                ! Ne rien faire pour le centre (laissez-le vide) 
                ELSE IF(dom%grille(i, dom%ng-j+1) == " ") THEN 
                    dom%grille(i, dom%ng-j+1) = "+"
                END IF
                ! Côté inférieur gauche
                IF (i == 2 .AND. j == 2) THEN
                ! Ne rien faire pour le centre (laissez-le vide)
                ELSE IF(dom%grille(dom%ng-i+1,j) == " ") THEN 
                    dom%grille(dom%ng-i+1, j) = "+"
                END IF
                ! Côté inférieur droit
                IF (i == 2 .AND. j == 2) THEN
                ! Ne rien faire pour le centre (laissez-le vide)
                ELSE IF(dom%grille(dom%ng-i+1, dom%ng-j+1) == " ") THEN 
                      dom%grille(dom%ng-i+1, dom%ng-j+1) = "+"
                END IF
            END DO
        END DO
          
    END SUBROUTINE traverse
    
    ! Gère la téléportation du joueur d'un coin de la grille à l'autre, en fonction de la touche entrée
    SUBROUTINE teleportation(dom,touche) 
        TYPE(domaine),INTENT(INOUT)                 :: dom 
        CHARACTER,INTENT(INOUT)                     :: touche
                  
        !Detecter dans quel carré se situe le joueur :
        IF(dom%joueur%pos(1)==2 .AND. dom%joueur%pos(2)==2) THEN     
            !Adapter la teleportation du joueur en fonction de la touche 
            !Le joueur est sur le carré en haut à gauche 
            IF(touche == 'd' .OR. touche == 'D' .OR. touche == 'q' .OR. touche == 'Q') THEN 
                dom%joueur%pos(1) = 2
                dom%joueur%pos(2) = dom%ng-2
                dom%grille(2,2)=" "
            ELSE IF(touche == 's' .OR. touche == 'S' .OR. touche == 'z'.OR. touche == 'Z'  ) THEN 
                dom%joueur%pos(1) = dom%ng - 1
                dom%joueur%pos(2) = 2
                dom%grille(2,2)=" "
            END IF
        ELSE IF(dom%joueur%pos(1)==2 .AND. dom%joueur%pos(2)==dom%ng-1) THEN 
            !Le joueur est sur le carré en haut à droite
            IF(touche == 'd' .OR. touche == 'D' .OR. touche == 'q' .OR. touche == 'Q') THEN
                dom%joueur%pos(1) = 2
                dom%joueur%pos(2) = 2
                dom%grille(2,dom%ng-1)=" " 
            ELSE IF(touche == 's' .OR. touche == 'S' .OR. touche == 'z'.OR. touche == 'Z'  ) THEN 
                dom%joueur%pos(1) = dom%ng-2
                dom%joueur%pos(2) = dom%ng-2
                dom%grille(2,dom%ng-1)=" "
            END IF
         ELSE IF (dom%joueur%pos(1) == dom%ng - 1 .AND. dom%joueur%pos(2) == 2) THEN
            ! Le joueur est sur le carré en bas à gauche
            IF (touche == 'd' .OR. touche == 'D' .OR. touche == 'q' .OR. touche == 'Q') THEN
                dom%joueur%pos(1) = dom%ng-2
                dom%joueur%pos(2) = dom%ng-2
                dom%grille(dom%ng-1,2)=" "
            ELSE IF (touche == 's' .OR. touche == 'S' .OR. touche == 'z' .OR. touche == 'Z') THEN
                dom%joueur%pos(1) = 2
                dom%joueur%pos(2) = 2
                dom%grille(dom%ng,2)=" "
            END IF
        ELSE IF (dom%joueur%pos(1) == dom%ng - 1 .AND. dom%joueur%pos(2) == dom%ng - 1) THEN
            ! Le joueur est sur le carré en bas à droite
            IF (touche == 'd' .OR. touche == 'D' .OR. touche == 'q' .OR. touche == 'Q') THEN
                dom%joueur%pos(1) =dom%ng-2
                dom%joueur%pos(2) = 2
                dom%grille(dom%ng-1,dom%ng-1)=" "
            ELSE IF (touche == 's' .OR. touche == 'S' .OR. touche == 'z' .OR. touche == 'Z') THEN
                dom%joueur%pos(1) = 2
                dom%joueur%pos(2) = dom%ng-1
                dom%grille(dom%ng-1,dom%ng-1)=" "
            END IF
        END IF
                  
    END SUBROUTINE
  
END MODULE mod_pacman
  

