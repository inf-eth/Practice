// This file was auto-generated based on version 1.0.0 of the canonical data.

module GradeSchoolTest

open FsUnit.Xunit
open Xunit

open GradeSchool

[<Fact>]
let ``Adding a student adds them to the sorted roster`` () =
    let school = studentsToSchool [("Aimee", 2)]
    roster school |> should equal ["Aimee"]

[<Fact>]
let ``Adding more student adds them to the sorted roster`` () =
    let school = studentsToSchool [("Blair", 2); ("James", 2); ("Paul", 2)]
    roster school |> should equal ["Blair"; "James"; "Paul"]

[<Fact>]
let ``Adding students to different grades adds them to the same sorted roster`` () =
    let school = studentsToSchool [("Chelsea", 3); ("Logan", 7)]
    roster school |> should equal ["Chelsea"; "Logan"]

[<Fact>]
let ``Roster returns an empty list if there are no students enrolled`` () =
    let school = studentsToSchool []
    roster school |> should be Empty

[<Fact>]
let ``Student names with grades are displayed in the same sorted roster`` () =
    let school = studentsToSchool [("Peter", 2); ("Anna", 1); ("Barb", 1); ("Zoe", 2); ("Alex", 2); ("Jim", 3); ("Charlie", 1)]
    roster school |> should equal ["Anna"; "Barb"; "Charlie"; "Alex"; "Peter"; "Zoe"; "Jim"]

[<Fact>]
let ``Grade returns the students in that grade in alphabetical order`` () =
    let school = studentsToSchool [("Franklin", 5); ("Bradley", 5); ("Jeff", 1)]
    grade 5 school |> should equal ["Bradley"; "Franklin"]

[<Fact>]
let ``Grade returns an empty list if there are no students in that grade`` () =
    let school = studentsToSchool []
    grade 1 school |> should be Empty

