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

// 64
type Shish<'a> =
  | Bottom of 'a
  | Onion of Shish<'a>
  | Lamb of Shish<'a>
  | Tomato of Shish<'a>

// 67
type Rod = Dagger | Fork | Sword
type Plate = GoldPlate | SilverPlate | BrassPlate

// 73
let rec isVeggie (s) =
  match s with
  | Bottom(b) -> true
  | Onion(x) -> isVeggie(x)
  | Lamb(_) -> false
  | Tomato(x) -> isVeggie(x)

(isVeggie : Shish<'a> -> bool)

// 98
let rec whatBottom (s) =
  match s with
  | Bottom(b) -> b
  | Onion(x) -> whatBottom(x)
  | Lamb(x) -> whatBottom(x)
  | Tomato(x) -> whatBottom(x)

(whatBottom : Shish<'a> -> 'a)
