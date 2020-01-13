namespace Day2

module Part1 =

    type Operation = Add | Multiply | End | Unknown

    let operationFromIntOpcode opCode =
        match opCode with
        | 1 -> Operation.Add
        | 2 -> Operation.Multiply
        | 99 -> Operation.End
        | _ -> Operation.Unknown
    
    type IntCode = {
        operation: Operation
        input1: int
        input2: int
        outputPosition: int
    } with
        static member fromStringAtPosition (cmd:string[]) position =
            let operation = operationFromIntOpcode (cmd.[position] |> int)
            if ((operation = Add) || (operation = Multiply)) then
                {
                    operation = operation
                    input1 = (cmd.[position + 1] |> int)
                    input2 = (cmd.[position + 2] |> int)
                    outputPosition = (cmd.[position + 3] |> int)
                }
            else
                {
                    operation = operation
                    input1 = -1
                    input2 = -1
                    outputPosition = -1
                }

    let solve (input:string) (noun:int) (verb:int) =

        let inputCmds = input.Split [|','|]

        let updateNounAndVerb (commands:string[]) (noun:int) (verb:int) =
            commands.[1] <- noun.ToString()
            commands.[2] <- verb.ToString()
            commands

        let updateCommands (commands:string[]) (position:int) (command:int) =
            let left = Array.sub commands 0 position
            let right = Array.sub commands (position + 1) (commands.Length - (position + 1))
            Array.concat [|left; [|command.ToString()|]; right|]

        let addOrMultiply inp1 inp2 (operation:Operation) =
            if operation = Operation.Add then
                inp1 + inp2
            else
                inp1 * inp2

        let rec executeCommands (startPosition:int) (commands:string[]) =
            if (startPosition >= commands.Length) then
                commands
            else
                let intCode = IntCode.fromStringAtPosition commands startPosition
                match intCode.operation with
                | Add | Multiply -> 
                    let inp1 = (int)commands.[intCode.input1]
                    let inp2 = (int)commands.[intCode.input2]
                    addOrMultiply inp1 inp2 intCode.operation
                    |> updateCommands commands intCode.outputPosition
                    |> executeCommands (startPosition + 4)
                | Unknown ->
                    executeCommands (startPosition + 4) commands
                | End ->
                    commands

        let cmds = updateNounAndVerb inputCmds noun verb

        executeCommands 0 cmds

    let solution = 
        let input = "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,19,6,23,1,23,6,27,1,13,27,31,2,13,31,35,1,5,35,39,2,39,13,43,1,10,43,47,2,13,47,51,1,6,51,55,2,55,13,59,1,59,10,63,1,63,10,67,2,10,67,71,1,6,71,75,1,10,75,79,1,79,9,83,2,83,6,87,2,87,9,91,1,5,91,95,1,6,95,99,1,99,9,103,2,10,103,107,1,107,6,111,2,9,111,115,1,5,115,119,1,10,119,123,1,2,123,127,1,127,6,0,99,2,14,0,0"
        solve input 12 2

module Part2 =

    let solve input noun verb =
        let result = Part1.solve input noun verb
        result.[0]

    let solution =
        let expectedValue = 19690720 //12490719
        let input = "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,19,6,23,1,23,6,27,1,13,27,31,2,13,31,35,1,5,35,39,2,39,13,43,1,10,43,47,2,13,47,51,1,6,51,55,2,55,13,59,1,59,10,63,1,63,10,67,2,10,67,71,1,6,71,75,1,10,75,79,1,79,9,83,2,83,6,87,2,87,9,91,1,5,91,95,1,6,95,99,1,99,9,103,2,10,103,107,1,107,6,111,2,9,111,115,1,5,115,119,1,10,119,123,1,2,123,127,1,127,6,0,99,2,14,0,0"
        let mutable continueCalc = true
        let mutable noun = 0
        let mutable verb = 0
        while continueCalc do
            let result = solve input noun verb
            if (result = expectedValue.ToString()) then
                continueCalc <- false
            else
                if (verb < 99) then 
                    verb <- verb + 1
                else
                    if noun < 99 then
                        verb <- 0
                        noun <- noun + 1
                    else    
                        continueCalc <- false // end of the line

        noun, verb                


