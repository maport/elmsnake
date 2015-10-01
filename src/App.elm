module App where

import Effects exposing (Effects)
import Time
import Json.Decode as Js exposing ( (:=) )
import Json.Encode as JsE
import Set
import StartApp
import Task
import List
import Dict
import AirConsole
import Model as M exposing (Position, Model, Player, Players, Food)
import Utils as U
import View


type Action
  = ConsoleMsg AirConsole.Message
  | Tick Float
  | ConsoleMsgSent (Maybe ())


init : (Model, Effects Action)
init =
  ( 
    { halfSize = { x=20, y=20 }
    , maxPlayers = 5
    , maxSnakeLength = 50
    , players = Dict.empty
    , walls =
      [ { line = [{x=-20, y=-20}, {x=20, y=-20}, {x=20, y=20}, {x=-20, y=20}, {x=-20, y=-20}] }
      , { line = [{x=-15, y=-15}, {x=7, y=-15}, {x=7, y=-5}] }
      , { line = [{x=7, y=-2}, {x=7, y=5}] }
      ]
    , foods = []
    , clockTime = Nothing
    , lastFoodSpawn = Nothing
    }

  , Effects.tick Tick

  )


movePlayers: Time.Time -> Model -> Players
movePlayers clockTime model =
  case model.clockTime of

    Nothing -> model.players

    Just oldClockTime ->

      let delta = Time.inSeconds (clockTime - oldClockTime)
          dist = 4 * delta

          movedPlayers = Dict.map (\id -> M.movePlayer dist) model.players 

          livePlayers = movedPlayers |> Dict.values |> List.filter M.playerIsLive

          checkPlayers id p players =
            let otherPlayer = M.hitEntity livePlayers p  
                wall = M.hitEntity model.walls p

            in case (otherPlayer, wall) of

                 (Just otherP, _) ->  
                   let players' = Dict.update p.id (Maybe.map (M.killPlayer model)) players 
                   in if M.playerIsLive p then
                        Dict.update otherP.id (Maybe.map (M.updateScore 10)) players'
                      else
                        players'

                 (_, Just w) ->
                   Dict.update p.id (Maybe.map (M.killPlayer model)) players

                 _ -> players

      in Dict.foldl checkPlayers movedPlayers movedPlayers


eatFoods: Float -> Players -> List Food -> (Players, List Food)
eatFoods maxLen players foods =
  let eatFood player =
        { player | length <- (min (player.length + 1) maxLen) }
        |> M.updateScore 5

      checkFoods id player (players, foods) =
        case M.hitEntity foods player of
          Nothing -> (players, foods)
          Just food -> ( Dict.insert id (eatFood player) players
                       , List.filter (not << ((==) food)) foods
                       )

  in Dict.foldl checkFoods (players, foods) (Dict.filter (\id -> M.playerIsLive) players)


maybeSpawnFood: Model -> Model
maybeSpawnFood model =
  let shouldSpawn = 
    case (List.length model.foods < 5, model.clockTime, model.lastFoodSpawn) of
      (True, Just clockTime, Just lastFoodSpawn) -> (Time.inSeconds (clockTime - lastFoodSpawn)) > 10
      (True, Just clockTime, Nothing) -> True
      _ -> False

  in if shouldSpawn then
       { model | foods <- (M.makeFood model) :: model.foods
               , lastFoodSpawn <- model.clockTime }
     else
       model


userDataDecoder : Js.Decoder Position
userDataDecoder =
  let toPos dir =
    case dir of
      "up" -> {x=0, y=-1}
      "right" -> {x=1, y=0}
      "down" -> {x=0, y=1}
      "left" -> {x=-1, y=0}

  in Js.map toPos ("key" := Js.string)


notifyNewPlayers : List Player -> Effects Action
notifyNewPlayers players =
    let sendMsg p = 
          AirConsole.sendMessage (Just p.id) (p.id |> View.playerColour |> JsE.string)
          |> Task.toMaybe
          |> Task.map ConsoleMsgSent

        -- Delay message send slightly (otherwise controller may still be initializing and not get it)
        delayedSend p =
          U.delayedTask (5 * Time.second) (sendMsg p)

    in List.map (delayedSend >> Effects.task) players
       |> Effects.batch


handleConsoleAction: AirConsole.Action -> (Model, Effects Action) -> (Model, Effects Action)
handleConsoleAction action (model, effects) =
  case action of

    {- We don't always seem to get AirConsole.DeviceChange messages when new users
       join, so we detect users joining or leaving by diffing the current users list
       (supplied by AirConsole.UserUpdate with every air console message)
       against the current players list. -}

    AirConsole.UserUpdate userIds ->
      let (staying, leaving) = Dict.partition (\k v -> Set.member k userIds) model.players

          arriving = Set.filter (\k -> not (Dict.member k model.players)) userIds 
                     |> Set.toList
                     |> List.take (model.maxPlayers - (staying |> Dict.keys |> List.length)) 
                     |> List.map (\id -> (id, M.makePlayer model id))
                     |> Dict.fromList

          players' = Dict.union staying arriving

          notifyEffects = notifyNewPlayers (Dict.values arriving)

      in ( { model | players <- players' }
         , Effects.batch [effects, notifyEffects]
         )

    AirConsole.Data userId json ->
      let player = Dict.get userId model.players

      in case player of
           Nothing -> (model, effects)

           Just p ->
            let dir' = Js.decodeValue userDataDecoder json |> Result.toMaybe |> Maybe.withDefault p.dir
                player' = M.changePlayerPendingDir dir' p |> M.makePlayerLive

            in ( { model | players <- Dict.insert userId player' model.players }
               , effects 
               )

    _ -> (model, effects)


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of

    ConsoleMsg msg ->
      List.foldl handleConsoleAction (model, Effects.none) (AirConsole.actionsFromMessage msg)

    Tick clockTime ->
      let players' = movePlayers clockTime model   
          (players'', foods') = eatFoods model.maxSnakeLength players' model.foods

      in ( { model | players <- players''
                   , foods <- foods'
                   , clockTime <- Just clockTime
           } |> maybeSpawnFood
         ,
           Effects.tick Tick
         )

    _ -> (model, Effects.none)


-- WIRING

app =
  StartApp.start
    { init = init
    , update = update
    , view = View.view
    , inputs = [Signal.map ConsoleMsg airConsoleIn]
    }
                          

main =
  app.html
      

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks


port airConsoleIn : AirConsole.InPortType


port airConsoleOut : AirConsole.OutPortType
port airConsoleOut =
  AirConsole.outSignal

