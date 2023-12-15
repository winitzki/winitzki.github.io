-- (bitWidth n) is how many times we need to divide n by 2 to obtain 0. This is floor(1 + log2 n) except for n = 0.
let lessThan =
      https://prelude.dhall-lang.org/v23.0.0/Natural/lessThan
        sha256:3381b66749290769badf8855d8a3f4af62e8de52d1364d838a9d1e20c94fa70c

let bitWidth
    : Natural -> Natural
    = \(x : Natural) ->
        let Acc
            : Type
            = { current : Natural, count : Natural }

        let updateAcc
            : Acc -> Acc
            = \(acc : Acc) ->
                if    lessThan x acc.current
                then  acc
                else  { current = acc.current * 2, count = acc.count + 1 }

        let initAcc
            : Acc
            = { current = 1, count = 0 }

        let upperBound =
              if    lessThan x 1000000000000000000000000000000
              then  101
              else  if lessThan
                         x
                         1000000000000000000000000000000000000000000000000000000000000
              then  201
              else  if lessThan
                         x
                         1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
              then  401
              else  if lessThan
                         x
                         1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
              then  801
              else  x + 1

        let result = Natural/fold upperBound Acc updateAcc initAcc

        in  result.count

let test = assert : bitWidth 1 === 1

let test = assert : bitWidth 2 === 2

let test = assert : bitWidth 3 === 2

let test = assert : bitWidth 4 === 3

let test = assert : bitWidth 5 === 3

let test = assert : bitWidth 6 === 3

let test = assert : bitWidth 7 === 3

let test = assert : bitWidth 8 === 4

let test = assert : bitWidth 9 === 4

let test = assert : bitWidth 1023 === 10

let test = assert : bitWidth 1024 === 11

let test = assert : bitWidth 1025 === 11

let test = assert : bitWidth 0 === 0

in  bitWidth
