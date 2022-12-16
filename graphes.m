
 matrice=fscanf(fopen('bonus_10.txt','r'), '%f',[401,8020]);
 X = [-2,2]
 Y = [-2,2]
 for i=0:19
   
     for k = 1: 401
         for j = 1:401
             matrice_2(k,j) = 0  ;                    
         end 
     end 
    
     for k = 1+401*i: 401+400*i
         for j = 1:401
             matrice_2(j,k-401*i) =  matrice(j,k);                    
         end 
     end 
     
     subplot(4,5,i+1)  
     imagesc(X,Y,matrice_2)
     title(i*0.15)
    
 end