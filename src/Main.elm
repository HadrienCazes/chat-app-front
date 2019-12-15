port module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, img, input, text)
import Html.Attributes exposing (id, placeholder, src, style)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


port receivedMessage : (Decode.Value -> msg) -> Sub msg


port sendUsername : String -> Cmd msg


type alias Model =
    { user : String
    , message : String
    , list_messages : List Message
    , list_users : List String
    , connected : Bool
    }



{--
type alias User =
    { name : String
    }
--}


type alias Message =
    { title : String
    , content : String
    , user : String
    }


type Action
    = NoOp (Result Http.Error ())
    | ReceivedMessage Decode.Value
    | Connect String
    | ValidateConnection
    | UserEdit String
    | MessageEdit String
    | ValidateMessage Message


decodeMessage : Decoder Message
decodeMessage =
    Decode.map3 Message
        (Decode.field "title" Decode.string)
        (Decode.field "message" Decode.string)
        (Decode.field "user" Decode.string)


encodeMessage : Message -> Encode.Value
encodeMessage m =
    Encode.object
        [ ( "title", Encode.string "New message !" )
        , ( "message", Encode.string m.content )
        , ( "username", Encode.string m.user )
        ]


init : () -> ( Model, Cmd Action )
init () =
    ( { user = ""
      , connected = False
      , list_messages = []
      , list_users = []
      , message = ""
      }
    , Cmd.none
    )


sendMessage : Message -> Cmd Action
sendMessage msg =
    Http.post
        { url = "http://localhost:4000/message" -- "https://cfa-chat-app.herokuapp.com/message"
        , body = Http.jsonBody (encodeMessage msg)
        , expect = Http.expectWhatever NoOp
        }


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        NoOp whatever ->
            ( model, Cmd.none )

        ReceivedMessage value ->
            let
                andThen result =
                    case result of
                        Ok val ->
                            val :: model.list_messages

                        Err r ->
                            model.list_messages
            in
            ( { model | list_messages = andThen (Decode.decodeValue decodeMessage value) }
            , Cmd.none
            )

        Connect usr ->
            ( { model
                | connected = True
                , user = usr
                , list_users = model.list_users ++ [ String.trim usr ]
              }
            , Cmd.none
            )

        ValidateConnection ->
            ( { model | list_users = model.list_users ++ [ model.user ], connected = True }, Cmd.batch [ sendUsername model.user, sendMessage { title = "New message !", content = model.user ++ "is connected!", user = "Console" } ] )

        UserEdit edit ->
            ( { model | user = edit }, Cmd.none )

        MessageEdit edit ->
            ( { model | message = edit }, Cmd.none )

        ValidateMessage msg ->
            ( { model | list_messages = model.list_messages ++ [ msg ], message = "" }, sendMessage msg )


viewMessage : Message -> Html Action
viewMessage msg =
    div []
        [ div [] [ text ("[" ++ msg.user ++ "] : " ++ msg.content) ]
        ]


view : Model -> Html Action
view model =
    if model.connected == False then
        home

    else
        div []
            [ h1 [] [ text ("Discutez ici au chaud " ++ model.user) ]
            , div [ style "padding" "12px" ] []
            , model.list_messages
                |> List.map viewMessage
                |> Html.div []
            , input [ placeholder "rentrez vos messages ici", onInput MessageEdit ] []
            , button [ onClick (ValidateMessage { title = "New message !", user = model.user, content = model.message }) ] [ text "Envoyer" ]
            ]


subscriptions : Model -> Sub Action
subscriptions model =
    Sub.batch
        [ receivedMessage ReceivedMessage ]


main : Program () Model Action
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


home =
    div []
        [ h1 [] [ text "Hello Elm-Chat !" ]
        , div [ style "padding" "12px" ] []
        , div [] [ text "Mettez un pseudo:" ]
        , input [ placeholder "ici", onInput UserEdit ] []
        , button [ onClick ValidateConnection ] [ text "confirm" ]
        ]
