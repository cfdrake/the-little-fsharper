module Chapter04

// 4
type Meza = Shrimp | Calamari | Escargots | Hummus

// 5
type Main = Steak | Ravioli | Chicken | Eggplant
type Salad = Green | Cucumber | Greek
type Dessert = Sundae | Mousse | Torte

// 25
let addASteak = function
  | Shrimp -> (Shrimp, Steak)
  | Calamari -> (Calamari, Steak)
  | Escargots -> (Escargots, Steak)
  | Hummus -> (Hummus, Steak)

(addASteak : Meza -> (Meza * Main))

let addASteak' (m) = (m, Steak)

// 42
let eqMain (m, m') =
  match (m, m') with
  | (Steak, Steak) -> true
  | (Steak, Ravioli) -> false
  | (Steak, Chicken) -> false
  | (Steak, Eggplant) -> false
  | (Ravioli, Steak) -> false
  | (Ravioli, Ravioli) -> true
  | (Ravioli, Chicken) -> false
  | (Ravioli, Eggplant) -> false
  | (Chicken, Steak) -> false
  | (Chicken, Ravioli) -> false
  | (Chicken, Chicken) -> true
  | (Chicken, Eggplant) -> false
  | (Eggplant, Steak) -> false
  | (Eggplant, Ravioli) -> false
  | (Eggplant, Chicken) -> false
  | (Eggplant, Eggplant) -> true

(eqMain : (Main * Main) -> bool)

let eqMain' (m, m') =
  match (m, m') with
  | (Steak, Steak) -> true
  | (Ravioli, Ravioli) -> true
  | (Chicken, Chicken) -> true
  | (Eggplant, Eggplant) -> true
  | (_, _) -> false

// 54
let hasSteak (meza:Meza, meal:Main, dessert:Dessert) =
  match (meza, meal, dessert) with
  | (_, Steak, _) -> true
  | (_, _, _) -> false

// 67
let addASteak''(x:Meza) = (x, Steak)
