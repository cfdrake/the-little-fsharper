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
let rec removeFish = function
  | (_, Bottom) -> Bottom
  | (Anchovy, Topping(Anchovy, p)) -> removeFish(Anchovy, p)
  | (Anchovy, Topping(t, p)) -> Topping(t, removeFish(Anchovy, p))
  | (Lox, Topping(Lox, p)) -> removeFish(Lox, p)
  | (Lox, Topping(t, p)) -> Topping(t, removeFish(Lox, p))
  | (Tuna, Topping(Tuna, p)) -> removeFish(Tuna, p)
  | (Tuna, Topping(t, p)) -> Topping(t, removeFish(Tuna, p))

(removeFish : (Fish * FishPizza) -> FishPizza)

// 38
let eqFish = function
  | (Anchovy, Anchovy) -> true
  | (Lox, Lox) -> true
  | (Tuna, Tuna) -> true
  | (_, _) -> false

(eqFish : (Fish * Fish) -> bool)

// 40
let rec removeFish'(f: Fish, p: FishPizza) =
  match p with
  | Bottom -> Bottom
  | Topping(t, p) -> if eqFish(f, t) then removeFish'(f, p) else Topping(t, removeFish'(f, p))

// 57
let rec removeInt(x: int, p: Pizza<int>) =
  match p with
  | Bottom -> Bottom
  | Topping(y, p) -> if x = y then removeInt(x, p) else Topping(y, removeInt(x, p))
