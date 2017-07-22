import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode



main =
  Html.program
    { init = init "cats"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { topic : String
  , gifUrl : String
  , cameraStations : List String
  }


init : String -> (Model, Cmd Msg)
init topic =
  ( Model topic "waiting.gif" []
  , getRandomGif topic
  )



-- UPDATE


type Msg
  = MorePlease
  | NewGif (Result Http.Error (List CameraStation))
  | Change String


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


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Type topic here", onInput Change ] []
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
    , geometry : Maybe Geometry
  }

type alias Geometry =
  { munnaatit : List Float
  }

geometryDecoder: Decode.Decoder Geometry
geometryDecoder =
  Decode.map Geometry
    (Decode.field "coordinates" (Decode.list Decode.float))

personDecoder: Decode.Decoder CameraStation
personDecoder =
  Decode.map3 CameraStation
    (Decode.field "id" Decode.string)
    (Decode.field "type" Decode.string)
    (Decode.field "geometry" (Decode.maybe geometryDecoder))

decodeGifUrl : Decode.Decoder (List CameraStation)
decodeGifUrl =
  Decode.at ["features"] (Decode.list personDecoder)
