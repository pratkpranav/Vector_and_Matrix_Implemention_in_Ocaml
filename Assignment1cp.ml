exception InvalidInput
exception UnequalVectorSize
exception UnequalMatrixShape
exception IncompatibleMatrixShape
exception SingularMatrix


type vector = float list ;;
open List;;
let rec vdim (v:vector): int = length v ;;

let rec mkzerov (n:int): vector =
  if n < 0 then
    raise (InvalidInput)
  else

  let rec aux cpt acc =
      if cpt < 1 then acc
      else aux (cpt-1) ( 0.0 ::acc)
  in aux n []   ;;



  let rec iszerov (v:vector): bool = 
  if v = [] then false
else
      let rec find mylist =  match mylist with 
  	| [] -> true 
  	| x :: ve ->
  	begin
  	if x =0.0 then find ve
  	else false
  	end
  	 in find v ;;


let rec addv (v1:vector) (v2:vector): vector =
  if ( vdim v1 ) != (vdim v2 ) then 
    raise (UnequalVectorSize) 
  else

let fi = v1 in 
	let se = v2 in 
		let rec listadd a b =                                                         
			match a with                                                                  
			  | [] -> b
			  | g :: m -> (match b with
   			  | h :: n -> (g +. h) :: (listadd m n)
    			 | [] -> a
    		)
    	 in listadd fi se ;; 


let rec scalarmultv (c:float) (v:vector): vector =  
let fi = v in 
		let rec listadd a b =                                                         
			match a with                                                                  
			  | [] ->  a
			  | c :: m -> (c *. b) :: (listadd m b)
    							 
    	 in listadd fi c ;; 


let rec dotprodv (v1:vector) (v2:vector): float =  
   if ( vdim v1 ) != (vdim v2 ) then 
    raise (UnequalVectorSize) 
  else
let fi = v1 in 
	let se = v2 in 
		let rec listadd a b =                                                         
			match a with                 
                                                  
			  | [] -> 0.0
			  | g :: m -> (match b with
   			  | h :: n -> (g *. h) +.  (listadd m n)
    			 | [] -> 0.0
    		)
    	 in listadd fi se ;;     	 


let find_x (v: vector) (a:int)  =

(let rec find_element myList k  = match myList with
        [] ->  0.0

        | ff_sec :: mac_sec -> 
        if  k = 0 then ff_sec
        else find_element mac_sec (k-1)


        in find_element v a 
      );;


let rec crossprodv (v1:vector) (v2:vector): vector = 
  if ( vdim v1 ) != (vdim v2 ) then 
    raise (UnequalVectorSize) 
  else
  [ ((find_x v1 1) *. (find_x v2 2 )) -. ((find_x v1 2) *. (find_x v2 1 )) ; ((find_x v1 2) *. (find_x v2 0 )) -. ((find_x v1 0) *. (find_x v2 2 )) ;
   ((find_x v1 0) *. (find_x v2 1 )) -. ((find_x v1 1) *. (find_x v2 0 )) ]

 type matrix =  float list list  ;;


let rec mdim (m:matrix): int*int = ( length m , let r = m in
  			match r with [] -> 0 
  			| x :: r -> length x ) ;;

let sizem (m:matrix): int = (let r = m in
  			match r with [] -> 0 
  			| x :: r -> length x);;


let rec mkzerom (m_:int) (n_:int): matrix = 
if (m_ < 0)  then 
  raise (InvalidInput)
else if (n_ < 0)  then 
  raise (InvalidInput)
  else
let rec au cp ac = 
	if cp < 1 then ac
	else au (cp - 1 ) (  let rec aux cpt acc =
     	 if cpt < 1 then acc
     	else aux (cpt-1) ( 0.0 :: acc)
  		in aux n_ []  :: ac)
	in au m_ []  ;;


let rec iszerom (m:matrix): bool = 
  if m = [] then false
else
      let rec find mylist =  match mylist with 
  	| [] -> true 
  	| x :: ve ->
  	begin
  		if x = [] then false
		else
      	(let rec find mylist =  match mylist with 
  		| [] -> true 
  		| x1 :: ve1 ->
  		begin
  		if x1 = 0.0 then find ve1
  		else false
  		end
  		 in find x)


  	end
  	 in find m ;;
let rec mkunitm (m_:int): matrix  =
if m_< 0 then 
  raise (InvalidInput)
else
let rec au cp ac = 
	if cp < 1 then ac
	else au (cp - 1 ) (  let rec aux cpt acc =
     	 if cpt < 1 then acc
     	else aux (cpt-1) ( 1.0 :: acc)
  		in aux m_ []  :: ac)
	in au m_ []  ;;



let rec isunitm (m:matrix): bool = 
 if m = [] then false
else
      let rec find mylist =  match mylist with 
  	| [] -> true 
  	| x :: ve ->
  	begin
  		if x = [] then false
		else
      	(let rec find mylist =  match mylist with 
  		| [] -> true 
  		| x1 :: ve1 ->
  		begin
  		if x1 = 1.0 then find ve1
  		else false
  		end
  		 in find x)


  	end
  	 in find m ;;

let rec addm (m1:matrix) (m2:matrix): matrix =

  if (mdim m1) != ( mdim m2 ) then 
  raise (UnequalMatrixShape)
else
let fi = m1 in 
	let se = m2 in 
		let rec listadd a b =                                                         
			match a with                                                                  
			  | [] -> b
			  | g :: m -> (match b with
   			  | h :: n -> (
   						let rec listadd a b =                                                         
						match a with                                                                  
						  | [] -> b
						  | g :: m -> (match b with
   						  | h :: n -> (g +. h) :: (listadd m n)
    					  | [] -> a
    					)
    					 in listadd g h) :: (listadd m n)
    			 		| [] -> a
    					)
    					 in listadd fi se 
    	;; 



let rec scalarmultm (c:float) (m:matrix): matrix =
 	let fi = m in 
		let rec listaddv a b =                                                         
			


			match a with                                                                  
			| [] ->  a
			| mn :: p -> 

			(


			let rec listadd a b =                                                         
			match a with                                                                  
			| [] ->  a
			| g :: q -> (g *. b) :: (listadd q b)
    		in listadd mn b
			


			)

			 :: (listaddv p b)




    		in listaddv fi c 


    		;; 



let rec transm (m:matrix): matrix = 


rev
(
let rec find_column m k = 

match k with 

| 0 -> []
| _ ->


(

(let rec transpose myList k = match myList with
| [] -> []
| ff :: mac -> 
	(
		let rec find_element myList k  = match myList with
		[] ->  0.0

		| ff_sec :: mac_sec -> 
		if  k = 0 then ff_sec
		else find_element mac_sec (k-1)


		in find_element ff k 
	)


	  :: (transpose mac k)

in transpose m (k-1)) 

) :: find_column m (k - 1) 

in find_column m ( let r = m in
  			match r with [] -> 0 
  			| x :: r -> length x )

)


;;
let find_fl_x (v: float list) (a:int)  =

(let rec find_element myList k  = match myList with
        [] ->  0.0

        | ff_sec :: mac_sec -> 
        if  k = 0 then ff_sec
        else find_element mac_sec (k-1)


        in find_element v a 
      );;






let rec multmp (m2:matrix) (p:int) (q: float list ) (r: float) (s:int) = (


    match m2 with [] -> r
    |  pp :: xx ->
    
      multmp xx p q (r +. (find_fl_x q s) *. (find_fl_x pp p) ) (s + 1)

      


    )


(*)
let rec multmnp (m1:matrix) (m2:matrix): matrix = 
  if (List.length (hd m1)) != (List.length m2) then 
      raise (IncompatibleMatrixShape)
    else

  let fi = m1 in 
      let g = 0 in
    let rec listaddv a b =  match a with                                                                  
      | [] ->  a
      | mn :: p -> 
      (let c = 0 in
let rec listadd a b k = match a with                                                                  
      | [] ->  0
      | x :: q ->(    ) :: (listadd q b (k + 1))
        in listadd mn b c
      ):: (listaddv p b)
      in listaddv fi g 

      ;; 
*)

let rec multm (m1:matrix) (m2:matrix): matrix = 
  if (List.length (hd m1)) != (List.length m2) then 
      raise (IncompatibleMatrixShape)
    else

  let fi = m1 in 
      
    let rec listaddv m b =  match m with                                                                  
      | [] ->  []
      | mn :: p -> 
      (
let rec listadd a b k = match a with                                                                  
      | [] ->  a
      | x :: q ->(
           (multmp m2 k mn 0.0 0)   ) :: (listadd q b (k + 1))
        in listadd mn b 0
      ):: (listaddv p b)
      in listaddv fi 0 

      ;; 

let find_x_y (m: matrix) (a:int) (b:int) =


  (let r = m in
  let rec final_find r a b = 
 match r with                                                                  
      | [] ->  -1.0
      | x :: q ->

      begin
      if a = 0 then 


      
        (let rec find_element myList k  = match myList with
        [] ->  0.0

        | ff_sec :: mac_sec -> 
        if  k = 0 then ff_sec
        else find_element mac_sec (k-1)


        in find_element x b 
      )


    else final_find q (a-1) b 
	end
	in final_find r a b);;


let multdiagonalm (m:matrix)  = 

	(
		let c = (sizem m) in
	let p = 1.0 in
		(let rec find_multdiag a p = 
			match a with 
			0 -> p
			| _  -> (find_multdiag (a-1) p *. (find_x_y m (a-1) (a-1)))

		in find_multdiag c p 


		));;

let det (mnp:matrix) b = 	let fi = mnp in 


let rec listaddv m b c =                                                         
			match m with                                                                  
			| [] ->  m
			| mn :: p -> 
		begin
			if c >= b + 1 then

			let x =  ((find_x_y mnp (c) b)  /.  (find_x_y mnp (b) (b)))  in 

			let rec listadd lis c =
                                                        
			match lis with                                                                  
			| [] ->  lis
			| g :: q -> 
				( ( g -. ((find_x_y mnp b c ) *. x))) :: (listadd q (c+1))
			
    		in listadd mn 0
			 :: (listaddv p b (c+1))
		else
			mn :: (listaddv p b (c+1))
			end
			in listaddv fi b 0
		;; 
	


let rec detm (m:matrix): float = (


multdiagonalm	(let rec iter ma k = 

	if k = (length m) then  ma 
	else 
					iter (det ma k) ( k+ 1)

in iter m  0)

)




let dets (mnp:matrix) b =   let fi = mnp in 


let rec listaddv m b c =                                                         
      match m with                                                                  
      | [] ->  m
      | mn :: p -> 
    begin
      if c >= b + 1 then

      let x =  ((find_x_y mnp (c) (b + (List.length mnp)))  /.  (find_x_y mnp (b) (b + List.length mnp)))  in 

      let rec listadd lis c =
                                                        
      match lis with                                                                  
      | [] ->  lis
      | g :: q -> 
        ( ( g -. ((find_x_y mnp b c ) *. x))) :: (listadd q (c+1))
      
        in listadd mn 0
       :: (listaddv p b (c+1))
    else
      mn :: (listaddv p b (c+1))
      end
      in listaddv fi b 0
    ;; 
  


let rec add3 k d len = 
  if k = len then
    []
  else if k = d then
    1.0::add3 (k+1) d len
  else 
    0.0::add3 (k+1) d len;;


let convert_to (m:matrix )   = 
  let rec makepair (m:matrix) k  = 
    match m with 
    [] -> []
      | mn :: p -> 
      (
        let rec makpar (m1) k  = 
          match m1 with 
            [] -> (add3 0  k (List.length mn) )
            | rr :: pp -> rr :: (makpar pp k) 

            in makpar mn k
      ) :: (makepair p  (k+1))
    in makepair m 0;;



let final_inverse_find  m = (


let rec iter ma k = 

  if k = (length m) then  ma 
  else 
          iter (dets ma (k)) ( k+ 1)

in iter m  0



);;

let rec rev_matrix m = (


rev (let rec listadd m =
  match m with [] -> m 
  | c :: d-> rev c :: (listadd d)

in listadd m )

);;



let final_inverse  m = (


rev_matrix (let rec iter ma k = 

  if k = (length m) then final_inverse_find  (rev_matrix ma)
  else 
          iter (det ma k) ( k+ 1)

in iter (convert_to m)  0)



);;





let rec find_in (c:float) (m:matrix) (k:int) : matrix =
  let fi = m in 
    let rec listaddv a b =                                                         
      match a with                                                                  
      | [] ->  a
      | mn :: p -> 
    (let rec listadd listi a k  =  



                                                        
      match listi with                                                                  
      | [] ->  listi
      | g :: q -> 

      if a >= k then (g) :: (listadd q (a+1) k)
    else
      (listadd q (a+1) k)






        in listadd mn 0 (List.length m)
      


      )

       :: (listaddv p b)




        in listaddv fi c 


        ;; 



let rec invm (m:matrix): matrix = 

  (

    if (detm m) = 0.0 then 
    raise (SingularMatrix)
  else
    let r = ( find_in 0.0 (final_inverse m ) (List.length m)) in
      let rec finally m1 k = 
        match m1 with [] -> m1 
         | li :: st -> 

          (

            let rec fin li n = 
              match li with [] -> li 
              | g :: a -> (g /. (find_x_y (final_inverse m ) n n )):: (fin a (n))

            in fin li k
          )  :: (finally st (k+1))

         in finally r 0 



  );;


