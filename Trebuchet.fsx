#load "common.fsx"

[<Struct>]
type ParseOrigin =
    | Start
    | End

    static member slice count origin (v: string) =
        match origin with
        | Start -> v[count..]
        | End -> v[.. v.Length - count]

    static member firstFrom origin (v: string) =
        match origin with
        | Start -> v[0]
        | End -> v[v.Length - 1]

let private digitsSpelled =
    [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]

let parseDigit origin (line: string) =

    let substringsToParse =
        seq {
            for i in 0 .. line.Length do
                line |> ParseOrigin.slice i origin
        }

    let tryParseDigit char =
        if char <= '9' && char >= '0' then
            char - '0' |> int |> Some
        else
            None

    let tryParseSpelled (line: string) =
        match origin with
        | Start -> digitsSpelled |> List.tryFindIndex line.StartsWith |> Option.map ((+) 1)
        | End -> digitsSpelled |> List.tryFindIndex line.EndsWith |> Option.map ((+) 1)

    let tryParseDigit (line: string) =
        let potentialDigitChar = line |> ParseOrigin.firstFrom origin
        tryParseDigit potentialDigitChar |> Option.orElse (tryParseSpelled line)

    substringsToParse |> Seq.pick tryParseDigit

let parseCalibrationValue line =
    let firstDigit = parseDigit Start line
    let lastDigit = parseDigit End line
    firstDigit * 10 + lastDigit

let part2Answer = Common.problemInput 1 |> Seq.map parseCalibrationValue |> Seq.sum
