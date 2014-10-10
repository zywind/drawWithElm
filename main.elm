import Mouse
import Debug
import Char
import Keyboard (KeyCode, keysDown)
import Graphics.Collage (Form)
import Graphics.Input (input, button, Input)

type CanvasState =
  { forms: [Form]
  , currentPath: [(Float, Float)]
  , freePoint: Maybe (Float, Float)
  , selectedTool: Tool
  , drawing: Bool
  }
canvasState : CanvasState
canvasState =
  { forms = []
  , currentPath = []
  , freePoint = Nothing
  , selectedTool = Nil
  , drawing = False
  }

data Tool = Line | Nil

toolbox : Input Tool
toolbox = input Nil

xoffset = 100
canvas_width = 600
canvas_height = 400

lineTool : Element
lineTool = button toolbox.handle Line "line"

--polygonTool : Element
--polygonTool = plainText "polygon" |> clickable toolbox.handle Polygon

transformPos: (Int, Int) -> Maybe (Float, Float)
transformPos (x, y) = 
  let newx = x - canvas_width // 2 - xoffset |> toFloat
      newy = canvas_height // 2 - y |> toFloat
  in
    if abs newx <= (canvas_width // 2 |> toFloat) && abs newy <= (canvas_height // 2 |> toFloat)
      then Just (newx, newy)
      else Nothing

drawPath: CanvasState -> Form
drawPath canvas =
  case canvas.freePoint of
    Just pos ->
      if canvas.drawing
        then path (canvas.currentPath ++ [pos]) |> traced (solid black)
        else path canvas.currentPath |> traced (solid black)
    Nothing -> path canvas.currentPath |> traced (solid black)

sketchpad : CanvasState -> Element
sketchpad canvas =
  let _=Debug.watch "canvas" canvas
  in
  flow outward [
    flow down [lineTool],
    container (canvas_width+xoffset) canvas_height (topLeftAt (absolute xoffset) (absolute 0)) <|
    collage canvas_width canvas_height
    ([rect canvas_width canvas_height |> filled lightGray
    , rect canvas_width canvas_height |> outlined (solid black)
    , drawPath canvas
    ] ++ canvas.forms)
  ]

processKey : [KeyCode] -> CanvasState -> CanvasState
processKey keys canvas =
  if | any (\k->k==27) keys -> 
        case canvas.currentPath of
          [] -> {canvas | drawing <- False}
          xs -> {canvas | drawing <- False
                        , forms <- canvas.forms ++ [path canvas.currentPath |> traced (solid black)]
                        , currentPath <- []}
     --| any (\k->k==76) keys -> {canvas | selectedTool <- Line} --L
     --| any (\k->k==78) keys -> {canvas | selectedTool <- Nil} --N
     | otherwise -> canvas

updateTool : Tool -> CanvasState -> CanvasState
updateTool tool canvas = {canvas | selectedTool <- tool}

processMousePos : (Int, Int) -> CanvasState -> CanvasState
processMousePos mpos canvas = {canvas | freePoint <- transformPos mpos}

processMousePress : Bool -> CanvasState -> CanvasState
processMousePress mpressed canvas =
  case canvas.freePoint of 
    Just pos ->
      if mpressed && (isEmpty canvas.currentPath || pos /= last canvas.currentPath)
        then if canvas.drawing
          then
            case canvas.selectedTool of
              Line -> {canvas | currentPath <- canvas.currentPath ++ [pos]}
              Nil -> canvas
          else
            case canvas.selectedTool of
              Line -> {canvas | 
                          drawing <- True, 
                          currentPath <- canvas.currentPath ++ [pos]}
              Nil -> canvas
        else canvas
    Nothing -> canvas

computeState : ([KeyCode], Tool, Bool, (Int, Int)) -> CanvasState -> CanvasState
computeState (keys, tool, mpressed, mpos) canvas =
  processKey keys canvas |>
  updateTool tool |>
  processMousePos mpos |>
  processMousePress mpressed

allInput = lift4 (,,,) keysDown toolbox.signal Mouse.isDown Mouse.position

main = sketchpad <~ foldp computeState canvasState allInput