let Natural/le =
      λ(a : Natural) → λ(b : Natural) → Natural/isZero (Natural/subtract b a)

let Natural/less =
      λ(a : Natural) →
      λ(b : Natural) →
        if Natural/le b a then False else True

let bits =
      λ(bits : Natural) →
        Natural/fold
          bits
          (List Natural)
          ( λ(l : List Natural) →
                l
              # [ merge
                    { Some = λ(i : Natural) → i * 2, None = 1 }
                    (List/last Natural l)
                ]
          )
          ([] : List Natural)

let quotient =
      λ(w : Natural) →
        let bits = bits w

        in  λ(n : Natural) →
            λ(m : Natural) →
              let T = { r : Natural, q : Natural } : Type

              let div =
                    List/fold
                      Natural
                      bits
                      T
                      ( λ(bit : Natural) →
                        λ(t : T) →
                          let m = m * bit

                          in  if    Natural/le m t.r
                              then  { r = Natural/subtract m t.r
                                    , q = t.q + bit
                                    }
                              else  t
                      )
                      { r = n, q = 0 }

              in  if Natural/less div.r m then div else { q = 0, r = n }

let max = 10

let powpowT = { l : Natural, n : Natural }

let powpow =
      Natural/fold
        max
        (List powpowT)
        ( λ(ts : List powpowT) →
            merge
              { Some =
                  λ(t : powpowT) → [ { l = t.l + t.l, n = t.n * t.n } ] # ts
              , None = [ { l = 1, n = 2 } ]
              }
              (List/head powpowT ts)
        )
        ([] : List powpowT)

let powpow = List/reverse powpowT powpow

let bitapprox =
      λ(n : Natural) →
        List/fold
          powpowT
          powpow
          Natural
          ( λ(e : powpowT) →
            λ(l : Natural) →
              if Natural/less n e.n then e.l else l
          )
          0

in { quotient = λ(n : Natural) → λ(m : Natural) → quotient (bitapprox n) n m, bits }
