module Main exposing (..)

import Html exposing (Html, text)
import Html.Events as E
import Html.Attributes as A
import Task


-- Main.elm


type alias Model =
    { value : Int, subModel : IntModel }


type Msg
    = ConvertToKiB Int
    | SubMsg (IntMsg Msg)


main : Program Never Model Msg
main =
    Html.program
        { init = Model 0 (IntModel "") ! []
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


view : Model -> Html Msg
view model =
    Html.div []
        [ text "Value:"
        , text <| toString model.value
        , Html.br [] []
        , Html.map SubMsg <| intView model.subModel
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ConvertToKiB value ->
            let
                newModel =
                    { model | value = value * 1024 }
            in
                newModel ! []

        SubMsg sub ->
            case sub of
                UpMsg m ->
                    update m model

                _ ->
                    let
                        ( subModel, subCmd ) =
                            intUpdate ConvertToKiB sub model.subModel
                    in
                        { model | subModel = subModel } ! [ Cmd.map SubMsg subCmd ]



-- IntField.elm
-- "component" source goes here


do : msg -> Cmd msg
do msg =
    Task.perform identity <| Task.succeed msg


type alias IntModel =
    { string : String }


type IntMsg msg
    = ValidateInput
    | Input String
    | UpMsg msg


intView : IntModel -> Html (IntMsg msg)
intView model =
    Html.div []
        [ Html.input [ E.onInput Input, A.value model.string ] []
        , Html.button [ E.onClick ValidateInput ] [ text "OK" ]
        ]


intUpdate : (Int -> msg) -> IntMsg msg -> IntModel -> ( IntModel, Cmd (IntMsg msg) )
intUpdate upMsg msg model =
    case msg of
        Input s ->
            { model | string = s } ! []

        ValidateInput ->
            case String.toInt model.string of
                Ok v ->
                    model ! [ do <| UpMsg <| upMsg v ]

                Err e ->
                    model ! []

        UpMsg _ ->
            model ! []
