import Mouse
import Debug
import Char (toCode)
import History
import Keyboard
import Window
import Graphics.Collage (Form, LineStyle)
import Graphics.Input (input, button, Input)

type CanvasState =
  { drawings: [Drawing]
  , selectedDrawingIndex: Maybe Int
  , tempPoints: [(Float, Float)]
  , selectedTool: Tool
  , selectedLineStyle: LineStyle
  , selectedFill: Color
  , ragionStyle: RagionStyle
  , isDrawing: Bool
  , history: History.History [Drawing]
  }
canvasState : CanvasState
canvasState =
  { drawings = []
  , selectedDrawingIndex = Nothing
  , tempPoints = []
  , selectedTool = LinePen
  , selectedLineStyle = defaultLine
  , selectedFill = white
  , ragionStyle = PathOnly
  , isDrawing = False
  , history = History.infinite
  }

data RagionStyle = PathOnly | FillOnly | PathAndFill
data Tool = LinePen | OvalPen | PolygonPen | RectPen
data Action = Select Tool | Clear | NoAction | Click (Float, Float) | DoubleClick | Cancel | Undo | Redo
data Drawing =
  Rect (Float, Float) (Float, Float) LineStyle Color RagionStyle |
  Oval (Float, Float) (Float, Float) LineStyle Color RagionStyle |
  Polygon [(Float, Float)] LineStyle Color RagionStyle |
  Path [(Float, Float)] LineStyle

canvas_width = 800
canvas_height = 600
xoffsetS = lift (\w -> if w > canvas_width then (w-canvas_width) // 2 else 0) Window.width
yoffsetS = lift (\h -> if h > canvas_height then (h-canvas_height) // 2 else 0) Window.height

toolbox : Input Action
toolbox = input NoAction

lineBtn : Element
lineBtn = button toolbox.handle (Select LinePen) "Line"

ovalBtn : Element
ovalBtn = button toolbox.handle (Select OvalPen) "Oval"

rectBtn : Element
rectBtn = button toolbox.handle (Select RectPen) "Rectangle"

clearBtn : Element
clearBtn = button toolbox.handle (Clear) "Clear"

activeTool selectedTool =
  case selectedTool of
    LinePen -> [color red lineBtn, ovalBtn, rectBtn]
    OvalPen -> [lineBtn, color red ovalBtn, rectBtn]
    RectPen -> [lineBtn, ovalBtn, color red rectBtn]

draw: Drawing -> Form
draw drawing =
  case drawing of
    Path points lineStyle ->
      path points |> traced lineStyle
    Oval a b lineStyle fillColor ragionStyle ->
      let
        w = fst b - fst a
        h = snd b - snd a
        center = (fst a + w / 2, snd a + h / 2)
        outline = oval w h |> outlined lineStyle |> move center
        fill = oval w h |> filled red |> move center
      in
        case ragionStyle of
          PathOnly -> outline
          FillOnly -> fill
          PathAndFill -> group [fill, outline]
    Rect a b lineStyle fillColor ragionStyle ->
      let
        w = fst b - fst a
        h = snd b - snd a
        center = (fst a + w / 2, snd a + h / 2)
        outline = rect w h |> outlined lineStyle |> move center
        fill = rect w h |> filled red |> move center
      in
        case ragionStyle of
          PathOnly -> outline
          FillOnly -> fill
          PathAndFill -> group [fill, outline]

showHints canvas =
  case canvas.selectedTool of
    LinePen ->
      if not <| isEmpty canvas.tempPoints
        then plainText "Double click to end the path."
        else plainText ""
    _ -> plainText ""

createPath points canvas = Path points canvas.selectedLineStyle
createRect a b canvas = Rect a b canvas.selectedLineStyle canvas.selectedFill canvas.ragionStyle
createOval a b canvas = Oval a b canvas.selectedLineStyle canvas.selectedFill canvas.ragionStyle

sketchpad : CanvasState -> (Float, Float) -> Int -> Int -> Int -> Int -> Element
sketchpad canvas mpos w h xoffset yoffset =
  let _ = Debug.watch "canvas" canvas
      newDrawing =
        if canvas.isDrawing
          then
            case canvas.selectedTool of
              LinePen -> [createPath (canvas.tempPoints ++ [mpos]) canvas]
              OvalPen -> [createOval (head canvas.tempPoints) mpos canvas]
              RectPen -> [createRect (head canvas.tempPoints) mpos canvas]
          else []
  in
  container w h (topLeftAt (absolute <| xoffset - 100) (absolute yoffset)) <|
    flow down
    [ flow right
      [ flow down (activeTool canvas.selectedTool ++ [spacer 20 20, clearBtn])
      , collage canvas_width canvas_height
          ([rect canvas_width canvas_height |> filled lightGray
          , rect canvas_width canvas_height |> outlined (solid black)
          ] ++ (map draw (canvas.drawings ++ newDrawing)))
      , flow down
        [ plainText "Undo: Ctrl-Z"
        , plainText "Redo: Ctrl-R"
        , spacer 10 10
        , plainText "Line: L"
        , plainText "Oval: O"
        , plainText "Rect: R"
        , spacer 10 10
        , showHints canvas
        ]
      ]
    ]

processKey : Bool -> Bool -> [Keyboard.KeyCode] -> Action
processKey ctrl shift keys =
  if | any (\k->k==27) keys -> Cancel
     | ctrl && shift && any (\k->k==toCode 'Z') keys || ctrl && any (\k->k==toCode 'R') keys -> Redo
     | ctrl && any (\k->k==toCode 'Z') keys -> Undo
     | any (\k->k==toCode 'L') keys -> Select LinePen
     | any (\k->k==toCode 'O') keys -> Select OvalPen
     | any (\k->k==toCode 'R') keys -> Select RectPen
     | otherwise -> NoAction

clearCurrentDrawing canvas = {canvas | tempPoints <- [], isDrawing <- False}
clearPreviousDrawing canvas = {canvas | drawings <- [], history <- History.add [] canvas.history}

processAction : Action -> CanvasState -> CanvasState
processAction action canvas = 
  case Debug.watch "action" action of
    Select tool ->
      if tool /= canvas.selectedTool
        then {canvas | selectedTool <- tool, tempPoints <- [], isDrawing <- False}
        else canvas
    Cancel -> clearCurrentDrawing canvas
    Clear -> clearCurrentDrawing canvas |> clearPreviousDrawing
    NoAction -> canvas
    Click mpos -> 
      if canvas.isDrawing
        then
          if mpos /= last canvas.tempPoints
            then
              case canvas.selectedTool of
                LinePen -> {canvas | tempPoints <- canvas.tempPoints ++ [mpos]}
                OvalPen ->
                  let
                    newDrawing = createOval (head canvas.tempPoints) mpos canvas
                  in
                    {canvas | drawings <- canvas.drawings ++ [newDrawing],
                              tempPoints <- [], isDrawing <- False,
                              history <- History.add (canvas.drawings ++ [newDrawing]) canvas.history}
                RectPen ->
                  let
                    newDrawing = createRect (head canvas.tempPoints) mpos canvas
                  in
                    {canvas | drawings <- canvas.drawings ++ [newDrawing],
                              tempPoints <- [], isDrawing <- False,
                              history <- History.add (canvas.drawings ++ [newDrawing]) canvas.history}
            else
              canvas
        else
          {canvas | isDrawing <- True, tempPoints <- [mpos]}
    DoubleClick ->
      if canvas.isDrawing && canvas.selectedTool == LinePen
        then
          let
            newPath = createPath canvas.tempPoints canvas
          in
            {canvas | drawings <- canvas.drawings ++ [newPath], tempPoints <- [], 
                      isDrawing <- False, history <- History.add (canvas.drawings ++ [newPath]) canvas.history}
        else
          canvas
    Undo ->
      case History.undo canvas.history of
        Nothing -> canvas
        Just h ->
          case History.value h of
            Nothing -> { canvas | drawings <- [], history <- h}
            Just s -> { canvas | drawings <- s, history <- h}
    Redo ->
      case History.redo canvas.history of
        Nothing -> canvas
        Just h ->
          case History.value h of
            Nothing -> canvas
            Just s -> { canvas | drawings <- s, history <- h}

transformPosition: (Int, Int) -> Int -> Int -> (Float, Float)
transformPosition (x, y) xoffset yoffset = 
  let newx = x - canvas_width // 2 - xoffset |> toFloat
      newy = canvas_height // 2 - y + yoffset |> toFloat
  in (newx, newy)


inCanvas : Int -> Int -> (Int, Int) -> Bool
inCanvas xoffset yoffset (x, y) =
     x >= xoffset && x <= xoffset + canvas_width
  && y >= yoffset && y <= yoffset + canvas_height

filteredMouseClicks = 
  let
    inCanvasClicks = keepWhen (inCanvas <~ xoffsetS ~ yoffsetS ~ Mouse.position) () Mouse.clicks
    clickAction : (Int, Int) -> Int -> Int -> Action
    clickAction = (\(x, y) xoffset yoffset -> Click (transformPosition (x, y) xoffset yoffset))
  in clickAction <~ (sampleOn inCanvasClicks Mouse.position) ~ xoffsetS ~ yoffsetS

doubleClick : Signal ()
doubleClick =
  let
      calcInterval (t, _) (interval, lastT) = ((t-lastT), t)
      intervals: Signal (Time, Time)
      intervals = foldp calcInterval (10000.0, 0.0) (timestamp Mouse.clicks)
      shortIntervals: Signal (Time, Time)
      shortIntervals = keepIf (\(interval, _) -> interval < 200) (10000.0, 0.0) intervals
  in sampleOn shortIntervals (constant ())

doubleClickAction =
  let inCanvasDoubleClick = keepWhen (inCanvas <~ xoffsetS ~ yoffsetS ~ Mouse.position) () doubleClick
  in sampleOn inCanvasDoubleClick (constant DoubleClick)

allInput : Signal Action
allInput = merges [
  toolbox.signal
  , processKey <~ (merge Keyboard.meta Keyboard.ctrl) ~ Keyboard.shift ~ Keyboard.keysDown
  , doubleClickAction, filteredMouseClicks
  ]

main = sketchpad <~ foldp processAction canvasState allInput ~ 
  (transformPosition <~ Mouse.position ~ xoffsetS ~ yoffsetS) ~ Window.width ~ Window.height ~ xoffsetS ~ yoffsetS