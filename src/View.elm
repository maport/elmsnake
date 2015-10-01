module View (view, playerColour) where

import String
import Html exposing (Html)
import Svg
import Svg.Attributes as SVG
import List
import Dict
import Array
import Model as M exposing (Position, Model, Player, Players, Food, Wall, PlayerStatus(..))

  
colours : Array.Array String
colours =
  ["#517A48", "#E17D25", "#5F3960", "#469B9E", "#DB4706"] |> Array.fromList


playerColour: Int -> String
playerColour idx =
  Array.get (idx % (Array.length colours)) colours
  |> Maybe.withDefault ""


svgPoint: Position -> String
svgPoint {x,y} =
  (toString x) ++ "," ++ (toString y)


svgLine : List Position -> String
svgLine points =
  let
    seg idx pos =
      (if idx == 0 then "M" else "L") ++ (svgPoint pos)
  in
    List.indexedMap seg points
    |> String.concat


playerSvg : (Int, Player) -> Svg.Svg
playerSvg (id, {line, status}) = 
  Svg.path
    [ SVG.d (svgLine line)
    , SVG.stroke (playerColour id)
    , SVG.fill "none"
    , SVG.strokeWidth (if status == Ghost then "0.75" else "1")
    , SVG.strokeLinecap "round"
    , SVG.strokeLinejoin "round"
    , SVG.opacity (if status == Ghost then "0.5" else "1")
    ]
    []


wallSvg : Wall -> Svg.Svg
wallSvg {line} = 
  Svg.path
    [ SVG.d (svgLine line)
    , SVG.stroke "#3E606F"
    , SVG.fill "none"
    , SVG.strokeWidth "1"
    , SVG.strokeLinecap "square"
    ]
    []


foodSvg : Food -> Svg.Svg
foodSvg {line} = 
  let [{x,y},_] = line
  in Svg.circle
       [ SVG.cx (toString x)
       , SVG.cy (toString y)
       , SVG.r "0.4"
       , SVG.fill "none"
       , SVG.stroke "#FF7061"
       , SVG.strokeWidth "0.1"
       ]
       []


scoreSvg : Float -> Float -> Player -> Svg.Svg
scoreSvg x y {id, score} = 
  Svg.text'
    [ SVG.x (toString x)
    , SVG.y (toString y)
    , SVG.fontFamily """"Courier New", Courier, monospace"""
    , SVG.fontSize "1"
    , SVG.stroke "none"
    , SVG.fill (playerColour id)
    ]
    [ Svg.text (score |> toString |> String.padLeft 5 '0') ]


scoresSvg : Float -> Float -> List Player -> Svg.Svg
scoresSvg x y players = 
  Svg.g
    []
    (List.indexedMap (\i p -> scoreSvg (x+(toFloat i)*7) y p) players)


view : Signal.Address a -> Model -> Html
view address model =
  let hSize = model.halfSize
  in Svg.svg
       [ SVG.style "background-color:#3E606F"
       , SVG.width "100%"
       , SVG.height "100%"
       , SVG.viewBox ((toString -hSize.x) ++ " " ++ (toString -hSize.y) ++ " " ++ (toString (2*hSize.x)) ++ " " ++ (toString (2*hSize.y)))
       ]
       [ Svg.rect
           [ SVG.x (toString -hSize.x)
           , SVG.y (toString -hSize.y)
           , SVG.width (toString (2*hSize.x))
           , SVG.height (toString (2*hSize.y))
           , SVG.stroke "none"
           , SVG.fill "black"
           ]
           []
       , scoresSvg -17 -17 (Dict.values model.players) 
       , Svg.g
           []
           (
             (List.map foodSvg model.foods)
             ++ (List.map playerSvg (Dict.toList model.players))
             ++ (List.map wallSvg model.walls)
           )
       ]
