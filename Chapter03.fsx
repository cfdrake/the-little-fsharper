module Chapter03

// 1
type Pizza =
  | Crust
  | Cheese of Pizza
  | Onion of Pizza
  | Anchovy of Pizza
  | Sausage of Pizza

// 7
let rec removeAnchovy (p) =
  match p with
  | Crust -> Crust
  | Cheese(x) -> Cheese(removeAnchovy(x))
  | Onion(x) -> Onion(removeAnchovy(x))
  | Anchovy(x) -> removeAnchovy(x)
  | Sausage(x) -> Sausage(removeAnchovy(x))

(removeAnchovy : Pizza -> Pizza)

// 38
// Time to experiment with non-match syntax...
// function a -> b | c -> d allows single argument matching.
let rec topAnchovyWithCheese = function
  | Crust -> Crust
  | Cheese(x) -> Cheese(topAnchovyWithCheese(x))
  | Onion(x) -> Onion(topAnchovyWithCheese(x))
  | Anchovy(x) -> Cheese(Anchovy(topAnchovyWithCheese(x)))
  | Sausage(x) -> Sausage(topAnchovyWithCheese(x))

(topAnchovyWithCheese : Pizza -> Pizza)

// 51
let rec substAnchovyByCheese = function
  | Crust -> Crust
  | Cheese(x) -> Cheese(substAnchovyByCheese(x))
  | Onion(x) -> Onion(substAnchovyByCheese(x))
  | Anchovy(x) -> Cheese(substAnchovyByCheese(x))
  | Sausage(x) -> Sausage(substAnchovyByCheese(x))

(substAnchovyByCheese : Pizza -> Pizza)
