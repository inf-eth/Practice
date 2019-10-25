module Allergies

open System

// TODO: define the Allergen type
type Allergen = 
  | Eggs
  | Peanuts
  | Shellfish
  | Strawberries
  | Tomatoes
  | Chocolate
  | Pollen
  | Cats

let translateAllergen allergen =
  match allergen with
  | Allergen.Eggs -> 1
  | Allergen.Peanuts -> 2
  | Allergen.Shellfish -> 4
  | Allergen.Strawberries -> 8
  | Allergen.Tomatoes -> 16
  | Allergen.Chocolate -> 32
  | Allergen.Pollen -> 64
  | Allergen.Cats -> 128

let testAllergy code testAllergyCode =
  match code with
  | c when testAllergyCode = 0 -> None
  | c when c &&& 1 = testAllergyCode -> Some Allergen.Eggs
  | c when c &&& 2 = testAllergyCode -> Some Allergen.Peanuts
  | c when c &&& 4 = testAllergyCode -> Some Allergen.Shellfish
  | c when c &&& 8 = testAllergyCode -> Some Allergen.Strawberries
  | c when c &&& 16 = testAllergyCode -> Some Allergen.Tomatoes
  | c when c &&& 32 = testAllergyCode -> Some Allergen.Chocolate
  | c when c &&& 64 = testAllergyCode -> Some Allergen.Pollen
  | c when c &&& 128 = testAllergyCode -> Some Allergen.Cats
  | _ -> None

let getAllergy testAllergy =
  match testAllergy with
  | Some allergen -> [allergen]
  | None -> []

let allergicTo codedAllergies allergen =
  match testAllergy codedAllergies (translateAllergen allergen) with
  | Some allergen -> true
  | None -> false

let list codedAllergies = 
  [Eggs;Peanuts;Shellfish;Strawberries;Tomatoes;Chocolate;Pollen;Cats] |> List.map translateAllergen |> List.map (testAllergy codedAllergies) |> List.map getAllergy |> List.filter (fun x -> x<>[]) |> List.collect (fun allergenList -> allergenList)