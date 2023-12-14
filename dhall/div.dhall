-- divyx y x is  floor(x / y) and if y == 0 then it's anything
-- div y x Natural is floor(x / y) provided that y != 0 and y is a literal
-- let lessThanEqual = https://prelude.dhall-lang.org/v23.0.0/Natural/lessThanEqual sha256:1a5caa2b80a42b9f58fff58e47ac0d9a9946d0b2d36c54034b8ddfe3cb0f3c99
let lessThan =
      https://prelude.dhall-lang.org/v23.0.0/Natural/lessThan
        sha256:3381b66749290769badf8855d8a3f4af62e8de52d1364d838a9d1e20c94fa70c

let divyx
    : Natural -> Natural -> Natural
    = \(y : Natural) ->
      \(x : Natural) ->
        let Acc = Natural

        let update
            : Acc -> Acc
            = \(acc : Acc) ->
                if lessThan x ((acc + 1) * y) then acc else acc + 1

        in  Natural/fold x Acc update 0

let test = assert : divyx 1 4 === 4

let test = assert : divyx 2 4 === 2

let test = assert : divyx 2 2 === 1

let test = assert : divyx 2 3 === 1

let test = assert : divyx 3 2 === 0

let test = assert : divyx 3 0 === 0

let test = assert : divyx 0 2 === 2

let test = assert : divyx 0 0 === 0

let Void
    : Type
    = forall (x : Type) -> x

let Unit
    : Type
    = forall (x : Type) -> x -> x

let unit
    : Unit
    = \(x : Type) -> \(y : x) -> y

let NonzeroNatural
    : Natural -> Type
    = \(y : Natural) -> if Natural/isZero y then Void else {}

let test = {=} : NonzeroNatural 1

let divide
    : Natural -> forall (y : Natural) -> NonzeroNatural y -> Natural
    = \(x : Natural) -> \(y : Natural) -> \(_ : NonzeroNatural y) -> divyx y x

let test = assert : divide 40 20 {=} === 2

let test = assert : divide 40 13 {=} === 3

let test = assert : divide 40 14 {=} === 2

let NN
    : Natural -> Type
    = \(y : Natural) -> Natural/isZero y === False

let test = assert : NN 1

let dividex
    : forall (y : Natural) -> NN y -> Natural -> Natural
    = \(y : Natural) -> \(assrt : NN y) -> \(x : Natural) -> divyx y x

let test = assert : dividex 1 (assert : NN 1) 3 === 3

let test = assert : dividex 3 (assert : NN 1) 1 === 0

in  divide
