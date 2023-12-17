open System
// Input example Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green

type Game =
    | Game of id: int * handfuls: Map<string, int> list

    static member parse(line: string) =
        let (gameId, handfuls) =
            let parts = line.Split(':')

            parts[0]["Game ".Length ..] |> int,
            parts.[1].Split(';')
            |> Seq.map (fun hand ->
                let cubeCounts = hand.Split(',', StringSplitOptions.TrimEntries)

                cubeCounts
                |> Seq.map (fun cubeCountEntry ->
                    let cubeCountParts = cubeCountEntry.Split(' ', StringSplitOptions.TrimEntries)
                    (cubeCountParts[1], cubeCountParts[0] |> int))
                |> Map.ofSeq)
            |> List.ofSeq

        Game(gameId, handfuls)

    static member getMinimumBag(Game(_, handfuls)) =
        let adjustBag (bag: Map<_, _>) (color, count) =
            let countInBag = bag.TryFind color |> Option.defaultValue 0

            if countInBag <= count then bag.Add(color, count) else bag


        handfuls |> Seq.collect Map.toSeq |> Seq.fold adjustBag Map.empty |> Bag


and Bag =
    | Bag of Map<string, int>

    static member isGamePossible (Bag bagContents) (Game(_, handfuls)) =
        let isDrawPossible color handCount =
            bagContents.TryFind color
            |> Option.map ((<=) handCount)
            |> Option.defaultValue false

        let isHandPossible = Map.forall isDrawPossible

        handfuls |> List.forall isHandPossible

    static member power(Bag contents) =
        contents |> Map.values |> Seq.reduce (*)

#load "Common.fsx"

let bag =
    seq {
        "red", 12
        "green", 13
        "blue", 14
    }
    |> Map.ofSeq
    |> Bag

let part1Answer () =
    Common.problemInput 2
    |> Seq.map Game.parse
    |> Seq.filter (Bag.isGamePossible bag)
    |> Seq.sumBy (fun (Game(id, _)) -> id)

let part2Answer () =
    Common.problemInput 2
    |> Seq.map (Game.parse >> Game.getMinimumBag >> Bag.power)
    |> Seq.sum
