let rec get i =function
                []->failwith "indice invalide"
              | x::list ->if i=1 then x else get (i-1) list;;

let rec getd i =function
                [] ->failwith "indice invalide"
              | x::[]->failwith "indice invalide"
              | x::y::list ->if i=1 then (x,y) else getd (i-1) (y::list)
                                                        ;;
let med list=  let l=List.sort ( - ) list in
               let length=List.length l in
               if length mod 2 =0 then
                 let (a,b) = getd (length/2) l in (float a+. float b)/.2.

               else
               float ( get (( length-1)/2 +1) l) ;;
