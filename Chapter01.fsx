module Chapter01

// 16
type Seasoning = Salt | Pepper

// 21
type Num = Zero | OneMoreThan of Num

// 32
type OpenFacedSandwich<'a> =
  | Bread of 'a
  | Slice of OpenFacedSandwich<'a>
