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
