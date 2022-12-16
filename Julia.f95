!*******************************************************************************
!
! Année :2020/2021
!
! Auteur: Laboureur Guillaume
!
!*******************************************************************************

program fourth


complex :: c , zzero = (-2.01,2.01) , complox = (0,1)
INTEGER :: i,j,k,m,t
complex, dimension(27) :: zi
complex, dimension(401,401) :: Matrice_zero
INTEGER, dimension(401,401) :: Matrice_k


do k = 1,27
    zi(k) =  5
end do
do i = 1,401
    do j = 1,401
        Matrice_k(i,j) = 28
    end do
end do 



!---------------------------------------------------
! Création de la matrice zero contenant tous les z0
!---------------------------------------------------

do i = 1, 401
    zzero = zzero - (0,0.01)
        do j = 1,401
        zzero = zzero + (0.01,0) 
            
        Matrice_zero(i,j) = zzero                      

    end do
    zzero = zzero - 401* (0.01,0)       
end do

open(unit=1,FILE='bonus_10.txt')

do t=1,20
    c = EXP(complox*t*0.15)
    
    do i = 1, 401   
        do j= 1,401

            do k = 1, 26  
                zi(1) = Matrice_zero(i,j)
                If (k /= 1) Then 
                    do m = 1,(k-1)
                        If ( CABS(zi(m)) <= 2) Then 

                            zi(m+1) = zi(m)**2 + c

                        end If
                    end do
                end If                             
            end do 

            
            do k=1,27
                If (zi(k) == 5) Then
                    
                    If (Matrice_k(i,j)>k) Then
                        Matrice_k(i,j) = k-1
                    end if

                end if 
            end do

            
            do k = 1,26
                zi(k) = 5
            end do
            

        end do
    end do 



    write(1,*)  Matrice_k

    do i = 1,401
            do j = 1,401
                Matrice_k(i,j) = 28
            end do
    end do


end do


close(1)


! Programme Matlab: 
!
! matrice=fscanf(fopen('bonus_10.txt','r'), '%f',[401,8020]);
! X = [-2,2]
! Y = [-2,2]
! for i=0:19
!   
!     for k = 1: 401
!         for j = 1:401
!             matrice_2(k,j) = 0  ;                    
!         end 
!     end 
!    
!     for k = 1+401*i: 401+400*i
!         for j = 1:401
!             matrice_2(j,k-401*i) =  matrice(j,k);                    
!         end 
!     end 
!     
!     subplot(4,5,i+1)  
!     imagesc(X,Y,matrice_2)
!     title(i*0.15)
!    
! end
! suptitle('Ensembles de Julia pour c = exp(iwt) pour différentes valeurs de wt')




end program  fourth




















