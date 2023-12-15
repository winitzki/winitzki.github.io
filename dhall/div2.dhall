-- `divfast y x` is  floor(x / y) but `divfast 0 x = x`.
-- Time complexity is (log x)**2 and space complexity is log(x). But performance is much slower than `divyx` because of lists.

let lessThan =
      https://prelude.dhall-lang.org/v23.0.0/Natural/lessThan
        sha256:3381b66749290769badf8855d8a3f4af62e8de52d1364d838a9d1e20c94fa70c

let Natural/equal =
      https://prelude.dhall-lang.org/v23.0.0/Natural/equal
        sha256:7f108edfa35ddc7cebafb24dc073478e93a802e13b5bc3fd22f4768c9b066e60

let Optional/default =
      https://prelude.dhall-lang.org/v23.0.0/Optional/default
        sha256:5bd665b0d6605c374b3c4a7e2e2bd3b9c1e39323d41441149ed5e30d86e889ad

let List/drop =
      https://prelude.dhall-lang.org/v23.0.0/List/drop
        sha256:af983ba3ead494dd72beed05c0f3a17c36a4244adedf7ced502c6512196ed0cf

let divfast
    : Natural -> Natural -> Natural
    = let Acc = { up : Bool, steps : List Natural, result : Natural }

      in  \(y : Natural) ->
          \(x : Natural) ->
            let init
                : Acc
                = { up = True, steps = [ 1 ], result = 0 }

            let head
                : Acc -> Natural
                = \(acc : Acc) ->
                    Optional/default Natural 0 (List/head Natural acc.steps)

            let bumpUp
                : Acc -> Acc
                = \(acc : Acc) ->
                    acc // { steps = [ head acc * 2 ] # acc.steps }

            let bumpDown
                : Acc -> Acc
                = \(acc : Acc) ->
                    acc // { steps = List/drop 1 Natural acc.steps }

            let update
                : Acc -> Acc
                = \(acc : Acc) ->
                    let r = acc.result * y

                    in  if    acc.up
                        then  if    Natural/equal r x
                              then  acc
                              else  if lessThan r x
                              then  bumpUp acc // { result = head acc }
                              else  let a = bumpDown (acc // { up = False })

                                    in  a // { result = head a }
                        else  if Natural/equal r x
                        then  acc
                        else  if lessThan r x
                        then  let a = bumpDown acc

                              in  a // { result = acc.result + head a }
                        else  let a = bumpDown acc

                              in      a
                                  //  { result =
                                          Natural/subtract
                                            (head acc)
                                            (acc.result + head a)
                                      }

            in  if  lessThan y 2 then x else (Natural/fold x Acc update init).result

let test = assert : divfast 20 40 === 2
let test = assert : divfast 10 40 === 4
let test = assert : divfast 13 40 === 3
let test = assert : divfast 13 39 === 3
let test = assert : divfast 13 38 === 2
let test = assert : divfast 13 0 === 0
let test = assert : divfast 1 40 === 40
let test = assert : divfast 1 1 === 1
let test = assert : divfast 1 0 === 0
let test = assert : divfast 0 40 === 40
let test = assert : divfast 0 0 === 0
let test = assert : divfast 0 1 === 1

in  divfast
