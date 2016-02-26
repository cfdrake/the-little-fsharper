module Chapter02

// 1
type ShishKebab =
  | Skewer
  | Onion of ShishKebab
  | Lamb of ShishKebab
  | Tomato of ShishKebab

// 15
let rec only_onions (s) =
  match s with
  | Skewer -> true
  | Onion(x) -> only_onions x
  | Lamb(_) -> false
  | Tomato(_) -> false

(only_onions : ShishKebab -> bool)

// 63
let rec is_vegetarian (s) = 
  match s with
  | Skewer -> true
  | Onion(x) -> is_vegetarian x
  | Lamb(_) -> false
  | Tomato(x) -> is_vegetarian x

(is_vegetarian : ShishKebab -> bool)
