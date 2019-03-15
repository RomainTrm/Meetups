module Tests

open System
open Xunit
open FsCheck
open FsCheck.Xunit


type BaseDamages = BaseDamages of int
type Damages = Damages of float

type LuckyNumber = ZERO | ONE | TWO | THREE | FOUR | FIVE
type Luck = Lucky | Unlucky

type Character = {
    BaseDamages: BaseDamages
    LuckyNumber: LuckyNumber
}

let createBaseDamages value =
    if value >= 1 && value <= 1337
    then Some (BaseDamages value)
    else None

let createCharacter baseDamages luckyNumber =
    match createBaseDamages baseDamages with
    | None -> None
    | Some damages -> Some {BaseDamages = damages; LuckyNumber = luckyNumber}

let attack character amILucky =
    let luckyMultiplier = (*) 1.33
    let normalMultiplier = id

    let (BaseDamages damages) = character.BaseDamages
    let inflictedDamages multiplier = damages |> float |> multiplier |> Damages
    match character.LuckyNumber |> amILucky with
    | Unlucky -> inflictedDamages normalMultiplier
    | Lucky -> inflictedDamages luckyMultiplier

[<Property>]
let ``Un perso doit avoir un base damage entre 1 et 1337`` baseDamages luckyNumber =
    (baseDamages >= 1 && baseDamages <= 1337) ==> lazy
    let expectedCharacter = { BaseDamages = (BaseDamages baseDamages); LuckyNumber = luckyNumber }
    (createCharacter baseDamages luckyNumber) = (Some expectedCharacter)

[<Property>]
let ``Un perso ne peut pas avoir de base damage hors de la plage 1 à 1337`` baseDamages luckyNumber =
    (baseDamages < 1 || baseDamages > 1337) ==> lazy
    (createCharacter baseDamages luckyNumber) = None

[<Property>]
let ``La vanilla attack lancée par un perso inflige ses base damages`` baseDamages luckyNumber =
    (baseDamages >= 1 && baseDamages <= 1337) ==> lazy
    let unlucky _ = Unlucky
    let damages = 
        (createCharacter baseDamages luckyNumber)
        |> Option.map (fun character -> attack character unlucky)
    let expected = Some (Damages (float baseDamages))    
    damages = expected    
    
[<Property>]
let ``La lucky attack lancée par un perso inflige ses base damage * 1.33`` baseDamages luckyNumber =
    (baseDamages >= 1 && baseDamages <= 1337) ==> lazy
    let lucky _ = Lucky
    let damages = 
        (createCharacter baseDamages luckyNumber)
        |> Option.map (fun character -> attack character lucky)
    let expected = Some (Damages ((float baseDamages) * 1.33))    
    damages = expected    