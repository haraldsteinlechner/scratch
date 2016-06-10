open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.Rendering.NanoVg
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.WinForms

open System.Numerics

open FSharp.Quotations


[<ReflectedDefinition>]
module Nat =
    type Head = 
        abstract member Check : bool
        abstract member Infer : (unit -> unit) -> unit
    type True() = 
        interface Head with
            member x.Check = true
            member x.Infer _ = ()
    type Rule<'a>([<ReflectedDefinition>]v : 'a) = 
        interface Head with
            member x.Check = true
            member x.Infer _ = ()
    type And<'a,'b>(r : Rule<'a>, l : Rule<'b>) = 
        interface Head with
            member x.Check = true
            member x.Infer _ = ()

    type Rule = Head * (unit -> unit) // check head and reduction


    [<ReflectedDefinition>]
    let (<==) (thing : 'a) (r : Head) = ()
    [<ReflectedDefinition>]
    let (==>) (r : Head) (thing : 'a)  = 
        ()

    let (&&&) l r = And(Rule l,Rule r)
    let True = True()
    let solve (binder : 'a -> 'b) = ()
    
    type N = interface end
    type Z() = interface N

    type Succ<'s when 's :> N> = Succ of 's interface N
    type Int<'a> = Int of 'a
    type Counting() =
        member x.Int (       ) =        True  ==> Int (Z ())
        member x.Int ( m : N ) = Rule (Int m) ==> Int (Succ m) 

    type Sum<'a,'b,'c when 'a :> N and 'b :> N and 'c :> N> = Sum of 'a * 'b * 'c
    type Addition() =

        member x.Sum (Z : Z, M : N) = 
            True ==> Sum(Z, M, M)

        member x.Sum (N : N, M : N, K : N) = 
            Rule ( Sum (N, M, K) ) ==>
                Sum ( Succ N, N, Succ K ) 

    type Prod<'a,'b,'c when 'a :> N and 'b :> N and 'c :> N> = Prod of 'a * 'b * 'c
    type Product() =
        
        member x.Prod(Z : Z, M : N) =
            True ==> Prod (Z, M, Z)

        member x.Prod(N : N, M : N, P : N, K : N) =
            Prod(N,M,K) &&& Sum (K,M,P) ==>
                Prod (Succ(N), M, P) 

    let test = solve ( fun (r : N) -> Sum(Z(), Z(), r) )

//    let rules = 
//         [ for m in typeof<Product>.GetMethods() do
//            yield FSharp.Quotations.Expr.TryGetReflectedDefinition m ]
//
//    let universe = [ Z :> obj ]
    
    let testE = 
        <@ fun (Z : Z, M : N) -> True ==> Sum(Z, M, M) @>
    let testE2 = 
        <@ fun (N : N, M : N, K : N)  -> Rule ( Sum (N, M, K) ) ==> Sum ( Succ N, N, Succ K )  @>
    open FSharp.Quotations.Patterns
    open FSharp.Quotations.DerivedPatterns

    let matchRule (e : Expr) =
        match e with
            | DerivedPatterns.SpecificCall <@ (==>) @> (a, types, [premise;conclusion]) -> 
                Some (types, premise, conclusion)
            | _ -> None

    let rec cleanup (e : Expr) =
        match e with
            | Patterns.Let(e,Patterns.Var binder,body) when e.Name = binder.Name && e.Type = binder.Type ->  cleanup body
            | _ -> e

    let k = 
        match testE2 with
            | Lambdas(vars,expr) ->
                match matchRule (cleanup expr) with
                    | None -> printfn "JD"
                    | Some (t,p,c) -> printfn "%A ==> %A" p c
            | Lambdas(vars,expr) -> printfn "missed: %A" expr
            | _ -> printfn "noope"




[<EntryPoint>]
let main argv = 

    let a = V4f V3f.III

    let cnt = 10000
    let rep = 100000
    let rand = System.Random()
    let r = [ for x in 0 .. cnt do yield float32 ( rand.NextDouble() )]
    
    let baseVecs = 
        [| for angle in r do yield M44f.RotationZ(angle) |]


    let sysVecs = 
        [| for angle in r do yield Matrix4x4.CreateRotationZ(float32 angle) |]

    let mutable baseResult = M44f.Identity
    for v in baseVecs do
        baseResult <- baseResult + v

    Log.startTimed "basevec"
    for i in 0 .. rep do
        baseResult <- M44f.Identity
        for v in baseVecs do
            baseResult <- baseResult + v
    Log.stop()


    let mutable sysResult = Matrix4x4.Identity
    for v in sysVecs do
        sysResult <- sysResult + v

    Log.startTimed "system"
    for i in 0 .. rep do
        sysResult <- Matrix4x4.Identity
        for v in sysVecs do
            sysResult <- sysResult + v
    Log.stop()


    Aardvark.Init()

    use app = new OpenGlApplication()
    let win = app.CreateSimpleRenderWindow()
    win.Text <- "Aardvark rocks \\o/"

    let quadGeometry =
        IndexedGeometry(
            Mode = IndexedGeometryMode.TriangleList,
            IndexArray = ([|0;1;2; 0;2;3|] :> Array),
            IndexedAttributes =
                SymDict.ofList [
                    DefaultSemantic.Positions, [| V3f.OOO; V3f.IOO; V3f.IIO; V3f.OIO |] :> Array
                    DefaultSemantic.Colors, [| C4b.Red; C4b.Green; C4b.Blue; C4b.Yellow |] :> Array
                ]
        )
       

    let initialView = CameraView.lookAt (V3d(6,6,6)) V3d.Zero V3d.OOI
    let view = initialView |> DefaultCameraController.control win.Mouse win.Keyboard win.Time
    let proj = win.Sizes |> Mod.map (fun s -> Frustum.perspective 60.0 0.1 100.0 (float s.X / float s.Y))


    let sg =
        quadGeometry 
            |> Sg.ofIndexedGeometry
            |> Sg.effect [
                DefaultSurfaces.trafo |> toEffect
                DefaultSurfaces.vertexColor |> toEffect
               ]
            |> Sg.viewTrafo (view |> Mod.map CameraView.viewTrafo)
            |> Sg.projTrafo (proj |> Mod.map Frustum.projTrafo)

    
    let task =
        app.Runtime.CompileRender(win.FramebufferSignature, sg)
            |> DefaultOverlays.withStatistics

    win.RenderTask <- task
    win.Run()
    0
