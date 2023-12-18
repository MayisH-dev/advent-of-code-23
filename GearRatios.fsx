open System

type EngineSchematic = EngineSchematic of char[,]


type PotentialPartNumber =
    | PotentialPartNumber of row: int * startIndex: int * endIndex: int

    static member allFromUnparsedSchematic(lines: string seq) =
        let parseLine rowNumber (line: string) =
            line :> seq<_>
            |> Seq.indexed
            |> Seq.fold
                (fun ((previouslyParsed, currentStartIndex) as state) (i, c) ->
                    match (Char.IsAsciiDigit c, currentStartIndex) with
                    | false, Some(start) -> PotentialPartNumber(rowNumber, start, i - 1) :: previouslyParsed, None
                    | true, None -> previouslyParsed, Some i
                    | false, None -> state
                    | true, Some _ -> state)
                ([], None)
            |> function
                | parsed, None -> parsed
                | parsed, Some startIndex -> PotentialPartNumber(rowNumber, startIndex, line.Length - 1) :: parsed

        lines |> Seq.indexed |> Seq.collect ((<||) parseLine)

    static member isAdjacentToSymbol (EngineSchematic matrix) (PotentialPartNumber(row, startIndex, endIndex)) =
        let adjacentItems =
            seq {
                let adjacentToLeft = startIndex = 0
                let adjacentToRight = endIndex = Array2D.length2 matrix - 1
                let adjacentToTop = row = 0
                let adjacentToBottom = row = Array2D.length1 matrix - 1

                // top left corner
                if not adjacentToTop && not adjacentToLeft then
                    matrix[row - 1, startIndex - 1]

                // chars on top
                if not adjacentToTop then

                    for i in startIndex..endIndex do
                        matrix[row - 1, i]

                // top right corner
                if not adjacentToTop && not adjacentToRight then
                    matrix[row - 1, endIndex + 1]

                // left
                if not adjacentToLeft then
                    matrix[row, startIndex - 1]

                // right
                if not adjacentToRight then
                    matrix[row, endIndex + 1]

                // bottom left corner
                if not adjacentToBottom && not adjacentToLeft then
                    matrix[row + 1, startIndex - 1]

                // chars under
                if not adjacentToBottom then

                    for i in startIndex..endIndex do
                        matrix[row + 1, i]

                // bottom right corner
                if not adjacentToBottom && not adjacentToRight then
                    matrix[row + 1, endIndex + 1]
            }

        adjacentItems |> Seq.exists (fun x -> not (x >= '0' && x <= '9') && x <> '.')

    static member parseValue (EngineSchematic matrix) (PotentialPartNumber(row, startIndex, endIndex)) =
        matrix[row, startIndex..endIndex] |> Int32.Parse

#load "Common.fsx"

let part1Answer () =
    let input = Common.problemInput 3

    let schematic =
        input |> Seq.map (fun x -> x.ToCharArray()) |> array2D |> EngineSchematic

    input
    |> PotentialPartNumber.allFromUnparsedSchematic
    |> Seq.filter (PotentialPartNumber.isAdjacentToSymbol schematic)
    |> Seq.map (PotentialPartNumber.parseValue schematic)
    |> Seq.sum
