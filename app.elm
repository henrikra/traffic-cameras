import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Maps
import Maps.Map as Map
import Maps.Geo



main =
  Html.program
    { init = init "cats"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL

type alias Lol = Maps.Model ()

type alias Model =
  { topic : String
  , gifUrl : String
  , cameraStations : List String
  , map : Lol
  }

init : String -> (Model, Cmd Msg)
init topic =
  ( { topic = topic
  , gifUrl = "waiting.gif"
  , cameraStations = []
  , map = Maps.defaultModel
      |> Maps.updateMap (Map.moveTo (Maps.Geo.latLng 64.865143 26.209900))
      |> Maps.updateMap (Map.setZoom 5)
      |> Maps.updateMap (Map.setHeight 600)
      |> Maps.updateMap (Map.setWidth 900)}
  , getRandomGif topic
  )



-- UPDATE


type Msg
  = MorePlease
  | NewGif (Result Http.Error (List CameraStation))
  | Change String
  | Left 
  | Right
  | MapMsg (Maps.Msg ())


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      (model, getRandomGif model.topic)

    NewGif (Ok newUrl) ->
    let lol = Debug.log "heppp" newUrl
    in
      (model, Cmd.none)

    NewGif (Err error) ->
      ({model | topic = Debug.log "heps" (toString error)}, Cmd.none)

    Change newContent ->
      ({model | topic = newContent}, Cmd.none)

    Left ->
    let lol = Debug.log "heippa" "lol"
    in
      (model, Cmd.none)
    Right -> 
      (model, Cmd.none)

    MapMsg mapMsg ->
      Maps.update mapMsg model.map
      |> Tuple.mapFirst (\map -> { model | map = map })
      |> Tuple.mapSecond (Cmd.map MapMsg)


-- VIEW

viewButton : String -> Html ()
viewButton name =
  button [ onClick () ] [ text name ]


view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Type topic here", onInput Change ] []
    , Html.map MapMsg <| Maps.view model.map
    , Html.map (\_ -> Left) (viewButton "Left")
    , Html.map (\_ -> Right) (viewButton "Right")
    -- , span [] [ text (toString model)]
    , button [ onClick MorePlease ] [ text "More Please!" ]
    , br [] []
    , img [src model.gifUrl] []
    ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- HTTP


getRandomGif : String -> Cmd Msg
getRandomGif topic =
  let
    url =
      "https://tie.digitraffic.fi/api/v1/metadata/camera-stations?lastUpdated=false"
  in
    Http.send NewGif (Http.get url decodeGifUrl)


type alias CameraStation =
  { id : String
    , lollero: String
    , coordinates : Maybe (List Float)
  }

geometryDecoder: Decode.Decoder (List Float)
geometryDecoder = Decode.field "coordinates" (Decode.list Decode.float)

personDecoder: Decode.Decoder CameraStation
personDecoder =
  Decode.map3 CameraStation
    (Decode.field "id" Decode.string)
    (Decode.field "type" Decode.string)
    (Decode.field "geometry" (Decode.maybe geometryDecoder))

decodeGifUrl : Decode.Decoder (List CameraStation)
decodeGifUrl =
  Decode.at ["features"] (Decode.list personDecoder)
