module Chapter02

// 1
type ShishKebab =
  | Skewer
  | Onion of ShishKebab
  | Lamb of ShishKebab
  | Tomato of ShishKebab

// 15
let rec onlyOnions (s) =
  match s with
  | Skewer -> true
  | Onion(x) -> onlyOnions x
  | Lamb(_) -> false
  | Tomato(_) -> false

(onlyOnions : ShishKebab -> bool)

// 63
let rec isVegetarian (s) = 
  match s with
  | Skewer -> true
  | Onion(x) -> isVegetarian x
  | Lamb(_) -> false
  | Tomato(x) -> isVegetarian x

(isVegetarian : ShishKebab -> bool)
