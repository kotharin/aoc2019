namespace Day4

module Part1 =
    let isSixDigit i =
        (i > 99999) && (i < 1000000)

    let hasAdjacentSameDigitsAndNonDecreasing i =
        let s = i.ToString().ToCharArray()

        let adjacent, decreasing, _ =
            s
            |> Array.fold (fun adp n -> 
                let a,d,p = adp
                (a || (p = n)), (d && (p<=n)) ,n
                ) (false, true,'0')
        adjacent && decreasing

    let solution = 
        let range = [307237..769058]
        //let range = [223450]

        range
        |> Seq.filter isSixDigit
        |> Seq.filter hasAdjacentSameDigitsAndNonDecreasing
        |> Seq.length

module Part2 =
    let isSixDigit i =
        (i > 99999) && (i < 1000000)


    let rec adjacentEqualRec ln =
        match ln with
        | a::b::c::d::e::[f] ->
            (((a=b) && (b <> c) || ((a<>b) && (b = c) && (c <> d)) || ((b <> c) && (c = d) && (d <> e))|| ((c <> d) && (d = e) && (e <> f)) || ((d <> e) && (e = f)) ))
        | _ ->
            false

    let rec nonDecreasing ln =
        match ln with
        | a::[b] -> 
            a <= b
        | a::b::c -> 
            (a <= b) && (nonDecreasing (b::c))
        | _ -> 
            false

    let solution = 
        let range = [307237..769058]
        //let range = [677788]

        range
        |> Seq.filter isSixDigit
        |> Seq.map (fun n -> n.ToString().ToCharArray() |> List.ofArray)
        |> Seq.filter nonDecreasing
        |> Seq.filter adjacentEqualRec
        |> Seq.length