module Model where

import Time
import List
import Dict
import AirConsole
import Random
import Utils as U


type alias Position =
  { x: Float
  , y: Float }


type PlayerStatus
  = Ghost 
  | Live


type alias Entity a =
  { a |
      line: List Position
  }


type alias Wall =
  Entity {}


type alias Food =
  Entity {}


type alias Player =
  Entity
    { id: AirConsole.UserId
    , dir: Position 
    , pendingDir: Maybe Position
    , length: Float
    , status: PlayerStatus
    , score: Int }


type alias Players = Dict.Dict AirConsole.UserId Player


type alias Model =
  { halfSize: Position
  , maxPlayers: Int
  , maxSnakeLength: Float
  , players: Players
  , walls: List Wall
  , foods: List Food
  , clockTime: Maybe Time.Time
  , lastFoodSpawn: Maybe Time.Time
  }


randomSeed: Model -> Random.Seed
randomSeed model =
  model.clockTime
  |> Maybe.withDefault 0
  |> Time.inMilliseconds
  |> floor
  |> Random.initialSeed


makePlayer : Model -> AirConsole.UserId -> Player
makePlayer model id =
  let seed = randomSeed model

      halfArea = { x = model.halfSize.x / 2
                 , y = model.halfSize.y / 2
                 }

      ((p1, dir), seed') = Random.generate (Random.pair (randomPos halfArea) randomDir) seed

      p2 = movePosition dir 1.0 p1

  in { id = id
     , line = [p2, p1]
     , dir = dir
     , pendingDir = Nothing
     , length = 3.0
     , status = Ghost
     , score = 0
     }


updateScore : Int -> Player -> Player
updateScore delta player =
  case player.status of
    Live -> { player | score <- (max 0 (player.score + delta)) }
    _ -> player


killPlayer : Model -> Player -> Player
killPlayer model player =
  makePlayer model player.id
  |> \p -> { p | score <- (updateScore -20 player).score }


allowedDirChange: Position -> Position -> Bool
allowedDirChange dir dir' =
  if | dir.x == 0 && dir'.x /= 0 -> True
     | dir.y == 0 && dir'.y /= 0 -> True
     | otherwise -> False


changePlayerPendingDir: Position -> Player -> Player
changePlayerPendingDir dir player =
  if allowedDirChange player.dir dir
    then
      { player | pendingDir <- Just dir }
    else
      player


changePlayerDir: Position -> Player -> Player
changePlayerDir dir player =
  let line = player.line

      line' = List.head line
              |> Maybe.map (\head -> head :: line)
              |> Maybe.withDefault line

  in { player | dir <- dir
              , line <- line' }


segmentLength : Position -> Position -> Float
segmentLength pos1 pos2 =
  abs (
    if pos1.x == pos2.x
      then pos1.y - pos2.y
      else pos1.x - pos2.x
  )


randomDir: Random.Generator Position
randomDir =
  Random.customGenerator
    <| \seed ->

      let (whichDir, seed') = Random.generate (Random.int 0 3) seed

          dir = case whichDir of
            0 -> { x=0, y=-1 }
            1 -> { x=1, y=0 }
            2 -> { x=0, y=1 }
            3 -> { x=-1, y=0 }

      in (dir, seed')


randomPos: Position -> Random.Generator Position
randomPos halfSize =
  Random.customGenerator
  <| \seed ->

      let rangeX = Random.int -(ceiling halfSize.x) (floor halfSize.x)
          rangeY = Random.int -(ceiling halfSize.y) (floor halfSize.y)
          ((x, y), seed') = Random.generate (Random.pair rangeX rangeY) seed

      in ( { x = (toFloat x), y = (toFloat y) }
         , seed'
         )


segmentDir : Position -> Position -> Position
segmentDir pos1 pos2 =
  { x = U.sign (pos2.x - pos1.x)
  , y = U.sign (pos2.y - pos1.y)
  }


lineSegments: List Position -> List (Position, Position)
lineSegments line =
  case line of
    head::tail -> List.map2 (,) line tail
    _ -> []


isSegmentHit: Position -> ( Position, Position) -> Bool
isSegmentHit pos (s1, s2) =
  case (s1.x == s2.x, s1.y == s2.y) of

    (True, True) ->
      ((abs (pos.x - s1.x)) < 0.5) &&  ((abs (pos.y - s1.y)) < 0.5)

    (True, False) ->
      (pos.y >= (min s1.y s2.y)) && (pos.y <= (max s1.y s2.y)) && ((abs (pos.x - s1.x)) < 0.5)

    _ ->
      (pos.x >= (min s1.x s2.x)) && (pos.x <= (max s1.x s2.x)) && ((abs (pos.y - s1.y)) < 0.5)


collisionPoint: Player -> Maybe Position
collisionPoint player =
  List.head player.line
  |> Maybe.map (movePosition player.dir 0.5)


entityAtPos: List (Entity a) -> Position -> Maybe (Entity a)
entityAtPos entities pos =
  let collidesWith e = lineSegments e.line |> List.any (isSegmentHit pos)

  in List.filter collidesWith entities
     |> List.head


hitEntity: List (Entity a) -> Player -> Maybe (Entity a)
hitEntity entities player =
  let playerPos = collisionPoint player 

  in case playerPos of
       Nothing -> Nothing
       Just pos -> entityAtPos entities pos
  

movePosition : Position -> Float -> Position -> Position
movePosition dir dist pos =
  { x = pos.x + (dist * dir.x)
  , y = pos.y + (dist * dir.y) }


nextTurnPosition : Position -> Position -> Position
nextTurnPosition dir pos =
  let (movingAxis, updatePos) =
        if dir.x == 0
          then (.y, \f -> { pos | y <- toFloat (f pos.y) })
          else (.x, \f -> { pos | x <- toFloat (f pos.x) })

  in updatePos (if (movingAxis dir) > 0 then ceiling else floor)


shortenLine : Float -> List Position -> List Position 
shortenLine len line =
  case line of

    head :: next :: tail ->
      let segLen = segmentLength head next
          len' = len - segLen

      in if len' > 0
           then head :: (shortenLine len' (next :: tail))
           else [head, movePosition (segmentDir head next) len head]

    _ -> line


makePlayerLive: Player -> Player
makePlayerLive player =
  { player | status <- Live } 


playerIsLive: Player -> Bool
playerIsLive =
  .status >> ((==) Live)


movePlayer : Float -> Player -> Player
movePlayer dist player =
  case player.line of

    pos :: tail ->
      let maybeTurnPos = case player.pendingDir of
              Just pendingDir ->
                let turnPos = nextTurnPosition player.dir pos
                in if (segmentLength pos turnPos) <= dist 
                     then Just turnPos
                     else Nothing
              _ -> Nothing

          updateLine = shortenLine player.length

      in case maybeTurnPos of

           Nothing ->
             { player | line <- updateLine ((movePosition player.dir dist pos) :: tail) }

           Just turnPos ->
             { player | line <- updateLine (turnPos :: turnPos :: tail)
                      , dir <- player.pendingDir |> Maybe.withDefault player.dir
                      , pendingDir <- Nothing
             }

    _ -> player


makeFood: Model -> Food
makeFood m =
  let players = m.players |> Dict.values

      findEmptyPos seed = 
        let (pos, seed') = Random.generate (randomPos m.halfSize) seed
        in if (entityAtPos m.foods pos == Nothing)
              && (entityAtPos m.walls pos == Nothing)
              && (entityAtPos players pos == Nothing)
            then pos
            else findEmptyPos seed'

      emptyPos = findEmptyPos (randomSeed m)

  in { line = [emptyPos, emptyPos] }
