module App exposing (..)

import Browser
import Html exposing (Html, button, div, h1, text)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode


-- Document Type
type alias Document =
    { docId : Int
    , title : String
    , content : String
    }


-- Decode the JSON response from the backend
documentDecoder : Decode.Decoder Document
documentDecoder =
    Decode.map3 Document
        (Decode.field "docId" Decode.int)
        (Decode.field "title" Decode.string)
        (Decode.field "content" Decode.string)


-- Fetch Documents
getDocuments : Cmd Msg
getDocuments =
    Http.get
        { url = "http://localhost:8080/documents"
        , expect = Http.expectJson GotDocuments (Decode.list documentDecoder)
        }


-- Messages (handling the HTTP response)
type Msg
    = LoadDocuments
    | GotDocuments (Result Http.Error (List Document))


-- Update Function
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadDocuments ->
            ( model, getDocuments )

        GotDocuments (Ok docs) ->
            ( { model | documents = docs }, Cmd.none )

        GotDocuments (Err _) ->
            ( model, Cmd.none )


-- Model
type alias Model =
    { documents : List Document }


-- View Function
view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Document Collaboration" ]
        , button [ onClick LoadDocuments ] [ text "Load Documents" ]
        , div [] (List.map viewDocument model.documents)
        ]


-- View a Document
viewDocument : Document -> Html msg
viewDocument doc =
    div []
        [ h1 [] [ text doc.title ]
        , div [] [ text doc.content ]
        ]


-- Initialize the Model
init : () -> ( Model, Cmd Msg )
init _ =
    ( { documents = [] }, Cmd.none )


-- Main Function
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
