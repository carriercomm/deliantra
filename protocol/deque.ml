module A = BatDynArray


type 'a t = {
  negative : 'a A.t;
  positive : 'a A.t;
}


let create () = {
  negative = A.create ();
  positive = A.create ();
}


let ensure_index arr index make =
  if index < 0 then
    while A.length arr.negative <= (~-index) do
      A.add arr.negative (make ())
    done
  else
    while A.length arr.positive <= (~+index) do
      A.add arr.positive (make ())
    done


let get arr index =
  if index < 0 then
    A.get arr.negative (~-index)
  else
    A.get arr.positive (~+index)


let set arr index value =
  if index < 0 then
    A.set arr.negative (~-index) value
  else
    A.set arr.positive (~+index) value


let has arr index =
  if index < 0 then
    A.length arr.negative > (~-index)
  else
    A.length arr.positive > (~+index)


let length arr =
  A.length arr.negative + A.length arr.positive


let fold_left f x arr =
  let x = A.fold_left f x arr.negative in
  A.fold_left f x arr.positive


let iter f arr =
  A.iter f arr.negative;
  A.iter f arr.positive;
;;


let iteri f arr =
  A.iteri (fun i e ->
    f (~-i) e
  ) arr.negative;
  A.iteri (fun i e ->
    f (~+i) e
  ) arr.positive;
;;
