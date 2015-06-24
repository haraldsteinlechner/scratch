open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.Rendering.NanoVg
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.WinForms

open System.Numerics


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
