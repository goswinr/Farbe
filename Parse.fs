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
        | _ -> raise ( ArgumentException $"ParseFarbe: Invalid hex character '{c}'" )

    let parseHexPair(h1:Char, h2:Char) =
        parseHex(h1) * 16 + parseHex(h2)

    /// <summary>
    /// Parses 'HEX' strings (with or without '#').
    /// </summary>
    /// <remarks>
    /// When encountering a 'HEXA' string, it will ignore the <c>alpha</c> value (without validation).
    /// </remarks>
    let looseParseHexColor(s: string) =
        match s[0] with
        | _ when s.Length = 6 || s.Length = 8 ->
            let r = parseHexPair(s.[0], s.[1])
            let g = parseHexPair(s.[2], s.[3])
            let b = parseHexPair(s.[4], s.[5])
            r, g, b
        | '#' when s.Length = 7 || s.Length = 9 ->
            let r = parseHexPair(s.[1], s.[2])
            let g = parseHexPair(s.[3], s.[4])
            let b = parseHexPair(s.[5], s.[6])
            r, g, b
        | _ ->
            raise ( ArgumentException $"ParseFarbe: Invalid hex color '{s}'" )

    /// <summary>
    /// Parses 'HEXA' strings (with or without '#').
    /// </summary>
    /// <remarks>
    /// When encountering a 'HEX' string, it will default <c>alpha</c> to <c>255</c>.
    /// </remarks>
    let looseParseHexaColor(s: string) =
        match s[0] with
        | _ when s.Length = 6 ->
            let r = parseHexPair(s.[0], s.[1])
            let g = parseHexPair(s.[2], s.[3])
            let b = parseHexPair(s.[4], s.[5])
            r, g, b, 255
        | _ when s.Length = 8 ->
            let r = parseHexPair(s.[0], s.[1])
            let g = parseHexPair(s.[2], s.[3])
            let b = parseHexPair(s.[4], s.[5])
            let a = parseHexPair(s.[6], s.[7])
            r, g, b, a
        | '#' when s.Length = 7 ->
            let r = parseHexPair(s.[1], s.[2])
            let g = parseHexPair(s.[3], s.[4])
            let b = parseHexPair(s.[5], s.[6])
            r, g, b, 255
        | '#' when s.Length = 9 ->
            let r = parseHexPair(s.[1], s.[2])
            let g = parseHexPair(s.[3], s.[4])
            let b = parseHexPair(s.[5], s.[6])
            let a = parseHexPair(s.[7], s.[8])
            r, g, b, a
        | _ ->
            raise ( ArgumentException $"ParseFarbe: Invalid hex color '{s}'" )
    
    let parseHexColor(s:string) =
        if s.Length <> 7 then
            raise ( ArgumentException $"ParseFarbe: Invalid hex color '{s}'")
        else 
            let r = parseHexPair(s.[1], s.[2])
            let g = parseHexPair(s.[3], s.[4])
            let b = parseHexPair(s.[5], s.[6])
            r, g, b        
    
    let parseHexaColor(s: string) =
        if s.Length <> 9 then
            raise ( ArgumentException $"ParseFarbe: Invalid hexa color '{s}'")
        else
            let r = parseHexPair(s.[1], s.[2])
            let g = parseHexPair(s.[3], s.[4])
            let b = parseHexPair(s.[5], s.[6])
            let a = parseHexPair(s.[7], s.[8])
            r,g,b,a
