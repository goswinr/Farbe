namespace Farbe


open System

module ParseFarbe =

    let parseHex(c:Char) =
        match c with
        | '0' -> 0
        | '1' -> 1
        | '2' -> 2
        | '3' -> 3
        | '4' -> 4
        | '5' -> 5
        | '6' -> 6
        | '7' -> 7
        | '8' -> 8
        | '9' -> 9
        | 'A' -> 10
        | 'B' -> 11
        | 'C' -> 12
        | 'D' -> 13
        | 'E' -> 14
        | 'F' -> 15

        | 'a' -> 10
        | 'b' -> 11
        | 'c' -> 12
        | 'd' -> 13
        | 'e' -> 14
        | 'f' -> 15
        | _ -> raise ( ArgumentException $"ParseFrabe: Invalid hex character '{c}'" )

    let parseHexPair(h1:Char, h2:Char) =
        parseHex(h1) * 16 + parseHex(h2)


    let parseHexColor(s:string) =
        if s.Length <> 7 then
            raise ( ArgumentException $"ParseFrabe: Invalid hex color '{s}'" )
        else
            let r = parseHexPair(s.[1], s.[2])
            let g = parseHexPair(s.[3], s.[4])
            let b = parseHexPair(s.[5], s.[6])
            r, g, b