module StStephens exposing (main)

import Time
import Iso8601
import Dict

import Browser
import Html exposing (Html, header, div, text, h1, a, img, video, source)
import Html.Attributes exposing (class, href, attribute)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD exposing (Decoder, map3, field, string, list, andThen)

import Bootstrap.Navbar as Navbar
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row

-- Main

main : Program String Model Msg
main = Browser.element {
    init= init
    , view= view
    , update= update
    , subscriptions= subscriptions
  }


-- Model

type alias AllEntries = {
    english : List Entry
    , ipc : List Entry
    , phonics: List Entry
    , pe : List Entry
    , math : List Entry
  }


type alias Entry1 = {
      name : String
  }

type alias Entry2 = {
      name : String
      , asset : String
  }

type alias Entry = {
      name : String
      , asset : String
      , moment: Time.Posix
  }


type alias ApiEntries = Dict.Dict String (List Entry)


type Page =
  LandingP LandingState
  | TopicP
  | TimeP
  | ViewVideoP
  | FailureP


type LandingState =
    Failure
    | Loading
    | Success (Result Http.Error ApiEntries)


type alias Model = {
    someName : String
    , items : AllEntries
    , navbarState : Navbar.State
    , page : Page
    , currentEntry : Maybe Entry
    , jsonDebug: String
  }


initEntries : AllEntries
initEntries =
  { english= [], ipc= [], phonics= [], pe= [], math= [] }


init : String -> ( Model, Cmd Msg )
init aParam =
    let (initState, command) = Navbar.initialState NavbarMsg
    in
     ( Model aParam initEntries initState (LandingP Loading) Nothing ""
      , getItemList
    )


-- Update

type Msg =
  NavbarMsg Navbar.State
  | LandingAct
  | M1Act
  | M2Act
  | ViewVideoAct Entry
  | GotApiResult (Result Http.Error ApiEntries)
  | TestResult (Result Http.Error (List (List String)))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
      NavbarMsg state -> ({model | navbarState = state }, Cmd.none)
      LandingAct -> ({ model | page= (LandingP Loading) }, Cmd.none)
      M1Act -> ({ model | page= TimeP }, Cmd.none)
      M2Act -> let newModel= model in   -- updTopics
          ({ newModel | page= TopicP }, Cmd.none)
      ViewVideoAct entry -> ({ model | page= ViewVideoP, currentEntry = Just entry }, Cmd.none)
      GotApiResult results ->
        case results of
          Ok entries -> let oldItems= model.items in
                   ({ model | page= TopicP, items= apiContentToAllEntries entries }, Cmd.none)
                   -- LandingP (Success (Ok entries))
          Err error -> describeError error "HttpErr" model
      TestResult tResult ->
          case tResult of
              Ok stringList -> let flatString= List.foldr (++) "" (List.foldr List.append [] stringList)
                                in ({model | page= FailureP, jsonDebug= flatString }, Cmd.none)
              Err error -> describeError error "TestErr" model


describeError : Http.Error -> String -> Model -> (Model, Cmd Msg)
describeError error label model =
  case error of
    Http.BadUrl _ -> ({model | page= FailureP, jsonDebug= label ++ " badUrl" }, Cmd.none)
    Http.Timeout -> ({model | page= FailureP, jsonDebug= label ++ " timeout" }, Cmd.none)
    Http.NetworkError -> ({model | page= FailureP, jsonDebug= label ++ " netErr" }, Cmd.none)
    Http.BadStatus number -> ({model | page= FailureP, jsonDebug= label ++ " bad code" }, Cmd.none)
    Http.BadBody bodyMsg -> ({model | page= FailureP, jsonDebug= label ++ " bad body: " ++ bodyMsg }, Cmd.none)


-- Subscription
subscriptions : Model -> Sub Msg
subscriptions model =
  Navbar.subscriptions model.navbarState NavbarMsg


-- View

view : Model -> Html Msg
view model =
  div []
    [
      headSection model
    , bodySection model
    ]

{-
  [ class "navbar", attribute "collapseOnSelect" "", attribute "expand" "md", attribute "fixed" "bottom" ]
-}

headSection : Model -> Html Msg
headSection model =
  header [] [
    navBarSection model
  ]


navBarSection : Model -> Html Msg
navBarSection model =
    Navbar.config NavbarMsg
    -- |> Navbar.withAnimation
    -- |> Navbar.fixBottom
    |> Navbar.brand [ href "#", onClick LandingAct ] [ text "Myriam"]
    |> Navbar.items [
          Navbar.itemLink [ href "#", onClick M1Act ] [ text "Time"]
          , Navbar.itemLink [ href "#", onClick M2Act ] [ text "Topic"]
        ]
    |> Navbar.view model.navbarState


bodySection : Model -> Html Msg
bodySection model =
  div []
  [
    case model.page of
      LandingP pageContext -> landingBody model pageContext
      TimeP -> timeBody model
      TopicP -> topicBody model
      ViewVideoP -> videoDisplay model
      FailureP -> landingBody model Failure
  ]


bodyHeader : Model -> Html Msg
bodyHeader model=
  div [] [ text model.someName ]

-- Testing:
bodyHeader_T1 : Model -> Html Msg
bodyHeader_T1 model=
  div [] [
    div [class "base"] [
      div [class "header"] [
        div [class "left-aligned"] [
          div [] [ text model.someName ]
          , div [] [ text "Boards"]
          , div [] [ text "Search bar"]
        ]
        , div [class "logo"] [ text "Logo" ]
        , div [class "right-aligned"] [
            div [] [ text "Add"]
            , div [] [ text "Info"]
            , div [] [ text "Bell"]
            , div [] [ text "Gear"]
            , div [] [ text "Avatar"]
        ]
      ]
    ]
  ]



landingBody : Model -> LandingState -> Html Msg
landingBody model pageState=
  case pageState of
    Loading -> div [] [ text "..." ]
    Failure -> div [] [ text ("Initialisation failed : " ++ model.jsonDebug) ]
    Success content ->
      case content of
        Ok values -> div [] [ text ("success, " ++ (countEntries content) ++ " entries.") ]
        Err error -> div [] [ text "Failure!"]


countEntries : Result Http.Error ApiEntries -> String
countEntries maybeEntries =
  case maybeEntries of
    Ok entries -> let totalLength= countEntry (Dict.get "english" entries) + countEntry (Dict.get "ipc" entries)
                          + countEntry (Dict.get "phonics" entries) + countEntry (Dict.get "math" entries)
                  in String.fromInt totalLength
    Err _ -> "0 (err.)"


countEntry anEntry =
  case anEntry of
    Just x -> List.length x
    Nothing -> 0


timeBody : Model -> Html Msg
timeBody model=
  div []
    [ text "Time" ]


topicBody : Model -> Html Msg
topicBody model=
  div [] <|
      formatTopic "English" model.items.english
      ++ formatTopic "IPC" model.items.ipc
      ++ formatTopic "Math" model.items.math
      ++ formatTopic "Phonics" model.items.phonics
      ++ formatTopic "PE" model.items.pe


formatTopic : String -> List Entry -> List (Html Msg)
formatTopic label entries =
  [ h1 [] [ text label ]
    , Grid.container []
      <| layoutEntries entries
  ]

layoutEntries : List Entry -> List (Html Msg)
layoutEntries entries =
  let subGroups= groupByN 4 entries in
      List.map format3Entries subGroups


format3Entries entries =
  Grid.row [] <| List.map (\entry -> Grid.col [ Col.lg3, Col.sm6, Col.attrs [ class "entryCol" ]] [ formatEntry  entry ]) entries


formatEntry : Entry -> Html Msg
formatEntry entry =
  div [] [
    Grid.row [] [
      Grid.col [ Col.sm12 ] [
        a [ class "entry", onClick (ViewVideoAct entry)] [
             img [ class "entryImg", attribute "alt" entry.name
                , attribute "src" <| getImagePath entry ] []
          ]
      ]
    ]
    , Grid.row [Row.attrs [ class "entryCol" ]] [
       Grid.col [ Col.sm12 ] [
          text <| entry.name ++  ", " ++ (showMoment entry.moment)
        ]
      ]
  ]

videoDisplay : Model -> Html Msg
videoDisplay model =
  case model.currentEntry of
    (Just entry) ->
        div [] [
            video [ attribute "autoplay" "", attribute "controls" "", attribute "poster" <| getImagePath entry ]
            [ source [attribute "src" <| getVideoPath entry ] [] ]
        ]
    Nothing ->
      div [] [
      ]


cdnUrlPrefix : String
cdnUrlPrefix =  "http://chodov/video"

getImagePath : Entry -> String
getImagePath entry =
  cdnUrlPrefix ++ "/poster_" ++ entry.asset ++ ".png"

getVideoPath : Entry -> String
getVideoPath entry =
  cdnUrlPrefix ++ "/" ++  entry.asset ++ ".mp4"


groupByN length list =
  let (first, second) = splitAt length list in
    if second == [] then [ first ]
    else first :: (groupByN length second)

splitAt : Int -> List a -> (List a, List a)
splitAt n xs = (List.take n xs, List.drop n xs)


-- 2020-05-26T00:00:00Z
-- 2020-05-31 00:39:54.995559
getDateTime aString =
  let
    result= if (String.endsWith "Z" aString) then
              Iso8601.toTime aString
            else
              Iso8601.toTime ((String.left 19 (String.replace " " "T" aString)) ++ "Z")
  in
    case result of
      Result.Ok posixVal->  posixVal
      Result.Err deadEnd -> Time.millisToPosix 0


showMoment : Time.Posix -> String
showMoment aDT =
    (String.fromInt <| (Time.toYear Time.utc aDT) - 2000) ++ "." ++ (String.fromInt <| monthToNbr <| Time.toMonth Time.utc aDT) ++ "." ++ (String.fromInt <| Time.toDay Time.utc aDT)


monthToNbr : Time.Month -> Int
monthToNbr aMonth =
  case aMonth of
    Time.Jan -> 1
    Time.Feb -> 2
    Time.Mar -> 3
    Time.Apr -> 4
    Time.May -> 5
    Time.Jun -> 6
    Time.Jul -> 7
    Time.Aug -> 8
    Time.Sep -> 9
    Time.Oct -> 10
    Time.Nov -> 11
    Time.Dec -> 12


-- API access:
-- "http://chodov:7001/apistub"
-- "/apistub"
getItemList : Cmd Msg
getItemList =  Http.post
      { url = "http://chodov:7001/apistub"
        , body= Http.stringBody "application/x-www-form-urlencoded" "a=1"
        , expect = Http.expectJson
            GotApiResult decodeEntries
            -- TestResult testDecode
      }


-- Testing
testDecode : JD.Decoder (List (List String))
testDecode =
  JD.field "result" JD.string |> JD.andThen testItems

testItems : String -> JD.Decoder (List (List String))
testItems code=
  case code of
    "ok" -> JD.field "items" (JD.field "english" (JD.list (JD.list string)))
    "err" ->  JD.field "reason" string |> JD.andThen (\err -> JD.fail ("Err: " ++ err))
    _ -> JD.fail ("Unknown situation: " ++ code)


-- JSON decoding:
-- api sends "data": { "result" : <a code:string> [ "items": <an entry:list>:list | "reason": <msg:string> ]}

decodeEntries : JD.Decoder ApiEntries
decodeEntries =
  JD.field "result" JD.string |> JD.andThen decodeList

-- conditional decoding based on result field:
decodeList : String -> JD.Decoder ApiEntries
decodeList code=
  case code of
    "ok" -> field "items" (JD.dict (JD.list decodeStringArray))
    "err" ->  JD.field "reason" string |> JD.andThen (\err -> JD.fail ("api err: " ++ err))
    _ -> JD.fail ("Unknown situation: " ++ code)


-- phonics vs phse!!
apiContentToAllEntries : ApiEntries -> AllEntries
apiContentToAllEntries aDict =
  let english = getListOfEntries (Dict.get "english" aDict)
      ipc = getListOfEntries (Dict.get "ipc" aDict)
      phonics = getListOfEntries (Dict.get "phonics" aDict)
      math = getListOfEntries (Dict.get "math" aDict)
  in AllEntries english ipc phonics [] math


getListOfEntries : Maybe (List x) -> List x
getListOfEntries aValue =
  case aValue of
    Just array -> array
    Nothing -> []


decodeStringArray : JD.Decoder Entry
decodeStringArray =
  JD.list string |> JD.andThen (fromResult << decodeEntry)


fromResult : Result String a -> Decoder a
fromResult result =
  case result of
    Ok a -> JD.succeed a
    Err errorMessage -> JD.fail errorMessage


decodeEntry : List String -> Result String Entry
decodeEntry stringList =
  case stringList of
    (x::y::z::[]) -> Ok (Entry x y (getDateTime z))
    _ -> Err ("Looking for a list of 3 strings exactly: " ++ (List.foldr (\s accum -> if accum == "" then s else s ++ ", " ++ accum) "" stringList))

