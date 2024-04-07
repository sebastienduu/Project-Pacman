  !!!!!!!!!!!!!!!!!!!!!!!
  ! PROGRAMME PRINCIPAL !
  !!!!!!!!!!!!!!!!!!!!!!!
  

  
PROGRAM pacman
    USE mod_pacman
    IMPLICIT NONE
    INTEGER                                   :: i,ok,temps
    INTEGER                                   :: deplacements , friandises_croquees , friandises_restantes 
    TYPE(domaine)                             :: dom 
    CHARACTER                                 :: touche
  
    ! Initialisation de l'aleatoire
    CALL RANDOM_SEED
  
    ! Lecture de la taille ng du domaine dom 
    !PRINT*,"Quelle taille la grille ? "
    !READ*,dom%ng
  
    ! Lecture du nombre de friandises nfri
    !PRINT*,"Quelle est le nombre de friandises ?"
    !READ*, dom%nfri
  
    ! Lecture du nombre de fantomes nfant
    !PRINT*,"Quelle est le nombre de fantomes ?"
    !READ*, dom%nfant
    
    ! Ouverture du fichier param.dat
    OPEN(10,FILE="param.dat")
    READ(10,*)dom%ng,dom%nfri,dom%nfant
    CLOSE(10)
    
    ! Ouverture du fichier traj.res
    OPEN (UNIT = 12,FILE = "traj.res")
    friandises_croquees = 0
    friandises_restantes = dom%nfri
  
    ! Allocation du tableau dynamique dom%grille, dom%friandise, dom%fantome
    ALLOCATE(dom%grille(0:dom%ng+1,0:dom%ng+1),dom%friandise(dom%nfri),dom%fantome(dom%nfant),STAT=ok)
    IF (ok/=0) STOP "Probleme Allocation"
  
    ! Choix des symboles pour joueur, friandise et fantômes
    dom%joueur%symbol = "@"
    dom%friandise(1:dom%nfri)%symbol = "O"
    dom%fantome(1:dom%nfant)%symbol  = "X"

  
    ! Pose du cadre dans la grille du domaine
    CALL cadre(dom)
    CALL placer_murs(dom)
  
    ! Inilisation de la position joueur
    CALL InitialiserPosition(dom%joueur,dom)
  
    ! Initialisation des positions de chaque friandise
    DO i = 1 , dom%nfri 
        CALL InitialiserPosition(dom%friandise(i),dom)
        dom%friandise(i)%old_pos(1)=dom%friandise(i)%pos(1)
        dom%friandise(i)%old_pos(2)=dom%friandise(i)%pos(2)
    END DO
  
    !Initialisation des positions de chaque fantome
    DO i = 1 , dom%nfant 
      CALL InitialiserPosition(dom%fantome(i),dom)
      dom%fantome(i)%old_pos(1)=dom%fantome(i)%pos(1)
      dom%fantome(i)%old_pos(2)=dom%fantome(i)%pos(2)
    END DO

    deplacements = 0
   
    ! Deplacement du joueur
    DO
        ! Écriture des coordonnées du joueur et fantômes dans le fichier trajectoire
        WRITE(UNIT = 12, FMT = *) "temps:", temps, "        xjoueur:", dom%joueur%pos(2), "      yjoueur:", dom%joueur%pos(1), &
        "       xfantome:", dom%fantome%pos(2), "      yfantome:", dom%fantome%pos(1)
        temps = temps + 1

        !CALL SYSTEM("stty -cbreak")
        CALL deplacement_joueur(dom%joueur,dom%ng,dom,deplacements,touche)
        CALL traverse(dom,dom%joueur)
        CALL teleportation(dom,touche)
        
        ! Déplacement aléatoire des fantômes
        DO i = 1, dom%nfant
            CALL avance_aleatoire(dom%fantome(i), dom%ng, dom%joueur, dom, deplacements)
        
            ! Vérification du croisement
            IF (dom%joueur%pos(1) == dom%fantome(i)%old_pos(1) .AND. dom%joueur%pos(2) == dom%fantome(i)%old_pos(2)) THEN
              PRINT*, "Le joueur a croise un fantome !"
              PRINT*, "Nombre de deplacements effectues :", deplacements
              STOP
            END IF
        END DO
  
        CALL Gobe_friandise(dom,friandises_restantes,friandises_croquees)
        
        CALL affichage(dom)
       
        
        !Arret du programme lorsque le nombre de friandise restante est de 0
        IF (dom%nfri == 0) THEN
            PRINT*, "Nombre de deplacements :", deplacements
            PRINT*, "Victoire !"
            EXIT
        END IF
    END DO
    
    !CALL SYSTEM("stty -cbreak")

    ! Fermeture du fichier traj.res
    CLOSE(UNIT = 12)
  
    ! Désallocation des tableaux
    DEALLOCATE(dom%grille, dom%friandise, dom%fantome)
  
END PROGRAM 
  
  
  
  
  
  
  
  
  