open System

// for calculating adjacent cells in the schematic
[<Struct>]
type HorizontalSegment =
    { Row: int
      StartIndex: int
      EndIndex: int }

type EnginePart =
    | PotentialPartNumber of HorizontalSegment * value: int
    | PotentialGear
    | Symbol
    | Period

module EngineSchematic =
    type private Cursor =
        | ParsingNumber of startIndex: int * currentValue: int ref
        | Empty

    let parse (lines: string array) =
        let schematic = Array2D.zeroCreate<EnginePart> lines.Length lines[0].Length

        let (|Digit|Gear|Symbol|Period|) c =
            match c with
            | '*' -> Gear
            | '.' -> Period
            | x when Char.IsAsciiDigit x -> Digit
            | _ -> Symbol

        let parseSimple row column =
            function
            | Gear -> schematic[row, column] <- PotentialGear
            | Period -> schematic[row, column] <- Period
            | Symbol -> schematic[row, column] <- Symbol
            >> fun _ -> Empty // reset the cursor

        // write the number to all the applicable cells in the schematic
        // called when the cursor is in a ParsingNumber state, and a non-digit is encountered
        let parsePartNumber (ParsingNumber(startIndex, currentValue)) row column =
            let partNumber =
                PotentialPartNumber(
                    { Row = row
                      StartIndex = startIndex
                      EndIndex = column - 1 },
                    currentValue.Value
                )

            for i in startIndex .. column - 1 do
                schematic[row, i] <- partNumber

        lines
        |> Seq.map Seq.indexed
        |> Seq.indexed
        |> Seq.iter (fun (row, line) ->
            line
            |> Seq.fold
                (fun cursor (column, c) ->
                    match cursor, c with
                    | ParsingNumber(_, currentValue), Digit ->
                        currentValue.Value <- currentValue.Value * 10 + (int (c - '0'))
                        cursor
                    | ParsingNumber(_, _), _ ->
                        parsePartNumber cursor row column
                        // parse the terminating character
                        parseSimple row column c
                    | Empty, Digit -> ParsingNumber(column, ref (int (c - '0')))
                    | Empty, _ -> parseSimple row column c)
                Empty
            |> function
                | Empty -> ()
                | ParsingNumber(_, _) as cursor -> parsePartNumber cursor row lines.[0].Length)

        schematic

let adjacentToSegment
    ({ Row = row
       StartIndex = startIndex
       EndIndex = endIndex })
    (matrix: 't[,])
    =
    seq {
        let adjacentToLeft = startIndex = 0
        let adjacentToRight = endIndex = Array2D.length2 matrix - 1
        let adjacentToTop = row = 0
        let adjacentToBottom = row = Array2D.length1 matrix - 1

        // top left corner
        if not adjacentToTop && not adjacentToLeft then
            yield matrix[row - 1, startIndex - 1]

        // chars on top
        if not adjacentToTop then

            for i in startIndex..endIndex do
                yield matrix[row - 1, i]

        // top right corner
        if not adjacentToTop && not adjacentToRight then
            yield matrix[row - 1, endIndex + 1]

        // left
        if not adjacentToLeft then
            yield matrix[row, startIndex - 1]

        // right
        if not adjacentToRight then
            yield matrix[row, endIndex + 1]

        // bottom left corner
        if not adjacentToBottom && not adjacentToLeft then
            yield matrix[row + 1, startIndex - 1]

        // chars under
        if not adjacentToBottom then

            for i in startIndex..endIndex do
                yield matrix[row + 1, i]

        // bottom right corner
        if not adjacentToBottom && not adjacentToRight then
            yield matrix[row + 1, endIndex + 1]
    }

let adjacentToPoint row index =
    adjacentToSegment
        { Row = row
          StartIndex = index
          EndIndex = index }

#load "Common.fsx"

let schematic = Common.problemInput 3 |> EngineSchematic.parse

let sumOfPartNumbers () =
    let isSymbol =
        function
        | PotentialGear
        | Symbol -> true
        | _ -> false

    seq {
        // if previous is a number, then ignore this one, as they are spanning the same segment
        let mutable wasPreviousNumber = false

        for i in 0 .. Array2D.length1 schematic - 1 do
            for j in 0 .. Array2D.length2 schematic - 1 do
                match schematic[i, j] with
                | PotentialPartNumber(coordinates, value) as part when
                    not wasPreviousNumber
                    && adjacentToSegment coordinates schematic |> Seq.exists isSymbol
                    ->
                    wasPreviousNumber <- true
                    value
                | PotentialPartNumber(_, _) -> wasPreviousNumber <- true
                | PotentialGear
                | Symbol
                | Period -> wasPreviousNumber <- false
    }
    |> Seq.sum

let sumOfGearRatios () =
    let exactlyTwoAdjacentNumberValues (adjacents: EnginePart seq) =
        adjacents
        |> Seq.fold
            (fun numbers part ->
                match numbers, part with
                | None, _ -> None // exceeded 2 adjacent numbers
                | Some foundNumbers, PotentialPartNumber(_, _) when foundNumbers |> List.contains part -> numbers
                | Some foundNumbers, PotentialPartNumber(_, _) -> Some(part :: foundNumbers)
                | Some foundNumbers, _ -> numbers)

            (Some [])
        |> Option.filter (Seq.length >> (=) 2)
        |> Option.map (
            Seq.map (function
                | PotentialPartNumber(_, value) -> value)
        )

    seq {
        for i in 0 .. Array2D.length1 schematic - 1 do
            for j in 0 .. Array2D.length2 schematic - 1 do
                match schematic[i, j] with
                | PotentialGear -> adjacentToPoint i j schematic |> exactlyTwoAdjacentNumberValues
                | _ -> None
    }
    |> Seq.choose (Option.map (Seq.reduce (*)))
    |> Seq.sum
