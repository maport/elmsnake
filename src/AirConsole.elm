module AirConsole
  ( Action(..), Message, UserId, InPortType, OutPortType
  , sendMessage, actionsFromMessage, outSignal) where


import Json.Decode as Js exposing ( (:=) )
import Json.Encode as JsE
import Set
import Task


type alias UserId =
  Int


type alias Message =
  Js.Value


type alias InPortType =
  Signal Message


type alias OutPortType =
  Signal (Maybe UserId, Js.Value)


mailbox : Signal.Mailbox (Maybe UserId, Js.Value)
mailbox = Signal.mailbox (Nothing, JsE.null)


outSignal = mailbox.signal


sendMessage : Maybe UserId -> Js.Value -> Task.Task x ()
sendMessage userId data =
  Signal.send mailbox.address (userId, data)  


type alias DeviceState =
  { uid: String
  , custom: Maybe String
  , nickname: Maybe String
  , slowConnection : Maybe Bool
  }


type Action
  = Ready String
  | DeviceChange UserId (Maybe DeviceState) 
  | Data UserId Js.Value 
  | UserUpdate (Set.Set UserId)


message : Js.Decoder Action
message =
  ("msg" := Js.string) `Js.andThen` body


usersFromMessage : Js.Decoder (List UserId)
usersFromMessage =
  ("user_ids" := (Js.list Js.int))


deviceState : Js.Decoder DeviceState
deviceState =
  Js.object4 DeviceState
    ("uid" := Js.string )
    (Js.maybe ("custom" := Js.string) )
    (Js.maybe ("nickname" := Js.string) )
    (Js.maybe ("slow_connection" := Js.bool) )


body : String -> Js.Decoder Action
body msg =
  case msg of
    "ready" ->
      Js.object1 Ready
        ("code" := Js.string)  
    "deviceChange" ->
      Js.object2 DeviceChange
        ("device_id" := Js.int)
        (Js.maybe ("user_data" := deviceState))
    "data" ->
      Js.object2 Data
        ("from" := Js.int)
        ("data" := Js.value)
    _ ->
      Js.fail ("unknown message: " ++ msg)
    

parseMessage : Js.Value -> Result String Action
parseMessage = Js.decodeValue message


parseUsersFromMessage : Js.Value -> Result String (Set.Set UserId)
parseUsersFromMessage msg =
  Js.decodeValue usersFromMessage msg
  |> Result.map Set.fromList


{-
  Parse the incoming message from airconsole and return a list of
  actions.
  Regardless of the incoming message type we also return an additional
  UserUpdate action listing the currently active users.
-}
actionsFromMessage : Message -> List Action
actionsFromMessage msg =
  let
    results =
        [ parseUsersFromMessage msg |> Result.map UserUpdate
        , parseMessage msg
        ]
    is_ok r = case r of
                Ok _ -> True
                _ -> False
  in
    List.filter is_ok results
    |> List.map (Result.toMaybe >> (Maybe.withDefault (Ready "?"))) 
