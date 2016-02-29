module Chapter05

// 1
type Pizza<'a> =
  | Bottom
  | Topping of ('a * Pizza<'a>)

// 2
type Fish = Anchovy | Lox | Tuna

// 11
type FishPizza = Pizza<Fish>

let rec removeAnchovy p = 
  match p with
  | Bottom -> Bottom
  | Topping(Anchovy, p) -> removeAnchovy(p)
  | Topping(t, p) -> Topping(t, removeAnchovy(p))

(removeAnchovy : FishPizza -> FishPizza) 

// 21
let rec removeTuna p =
  match p with
  | Bottom -> Bottom
  | Topping(Tuna, p) -> removeTuna(p)
  | Topping(t, p) -> Topping(t, removeTuna(p))

// 28
// ...
