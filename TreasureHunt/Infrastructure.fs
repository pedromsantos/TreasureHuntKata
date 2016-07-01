module Infrastructure
    open Microsoft.FSharp.Reflection

    let Construct<'T> (caseInfo: UnionCaseInfo) = FSharpValue.MakeUnion(caseInfo, [||]) :?> 'T
    let GetUnionCaseInfoAndInstance<'T> (caseInfo: UnionCaseInfo) = (caseInfo, Construct<'T> caseInfo)
    let AllCases<'T> = 
            FSharpType.GetUnionCases(typeof<'T>)
            |> Seq.map GetUnionCaseInfoAndInstance<'T>
