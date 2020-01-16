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

    let hasAdjacentSameDigitsAndNonDecreasing i =
        let s = i.ToString().ToCharArray()

        let adjacent, decreasing, _ =
            s
            |> Array.fold (fun adp n -> 
                let a,d,p = adp
                ((a || (p = n)) && (not a)), (d && (p<=n)) ,n
                ) (false, true,'0')
        adjacent && decreasing

    let solution = 
        let range = [307237..769058]
        //let range = [112233]

        range
        |> Seq.filter isSixDigit
        |> Seq.filter hasAdjacentSameDigitsAndNonDecreasing
        |> Seq.length