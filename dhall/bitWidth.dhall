-- (bitWidth n) is how many times we need to divide n by 2 to obtain 0. This is floor(1 + log2 n) except for n = 0.

let lessThanEqual = https://prelude.dhall-lang.org/v23.0.0/Natural/lessThanEqual
                    sha256:1a5caa2b80a42b9f58fff58e47ac0d9a9946d0b2d36c54034b8ddfe3cb0f3c99

let bitWidth: Natural -> Natural = \(x: Natural) ->
  let Acc: Type = {current: Natural, count: Natural}
  let updateAcc: Acc -> Acc = \(acc: Acc) ->
    if  lessThanEqual x acc.current then acc
     else { current = acc.current * 2, count = acc.count + 1 }

  let initAcc: Acc = {current = 1, count = 1}
  let result = Natural/fold (x + 1) Acc updateAcc initAcc
  in result.count

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

let test = assert : bitWidth 0 === 1    -- Special case.

in bitWidth
