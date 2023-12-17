open System.Net.Http
open System

let problemInput day =
    use client =
        let client =
            new HttpClient(BaseAddress = Uri $"https://adventofcode.com/2023/day/%d{day}/input")

        client.DefaultRequestHeaders.Add("Cookie", [ "<auth cookies here>" ])

        client

    task {
        let! response = client.GetAsync String.Empty
        let! content = response.Content.ReadAsStringAsync()
        return content.Split("\n", StringSplitOptions.RemoveEmptyEntries)
    }
    |> fun t -> t.Result
