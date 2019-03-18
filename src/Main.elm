module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Events as Event
import Http
import Json.Decode as Decode exposing (Decoder)
import Url exposing (Url)


type alias Model =
    { userId : UserUUID
    , userApiKey : UserApiKey
    , session : Session
    }


type alias LoggedInModel =
    { webhooks : WebhookStatus }


empty : Model
empty =
    { userId = UserUUID ""
    , userApiKey = UserApiKey ""
    , session = NotLoggedIn
    }


type Session
    = NotLoggedIn
    | AttemptingLogin
    | LoginFailure LoginFailureMessage
    | LoggedIn LoggedInModel


type WebhookStatus
    = Loading
    | Ready (List Webhook)


type alias LoginFailureMessage =
    String


type UserUUID
    = UserUUID String


type UserApiKey
    = UserApiKey String


type WebhookUUID
    = WebhookUUID String


type GroupUUID
    = GroupUUID String


type Label
    = Label String


type Type
    = TaskActivity TaskActivityOptions
    | GroupChatReceived GroupChatReceivedOptions
    | UserActivity UserActivityOptions


type alias TaskActivityOptions =
    { created : Bool
    , updated : Bool
    , deleted : Bool
    , scored : Bool
    }


type alias GroupChatReceivedOptions =
    { groupId : GroupUUID
    }


type alias UserActivityOptions =
    { petHatched : Bool
    , mountRaised : Bool
    , leveledUp : Bool
    }


type alias Webhook =
    { id : WebhookUUID
    , url : Url
    , enabled : Bool
    , label : Label
    , type_ : Type
    }


type Msg
    = UpdateUserId UserUUID
    | UpdateUserApiKey UserApiKey
    | Login
    | ReceiveLoginResult (Result String Session)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateUserId userId ->
            ( { model | userId = userId }, Cmd.none )

        UpdateUserApiKey apiKey ->
            ( { model | userApiKey = apiKey }, Cmd.none )

        Login ->
            let
                loginCmd =
                    requestUser model.userId model.userApiKey
            in
            ( { model | session = AttemptingLogin }, loginCmd )

        ReceiveLoginResult result ->
            case result of
                Err e ->
                    ( { model
                        | session =
                            LoginFailure e
                      }
                    , Cmd.none
                    )

                Ok session ->
                    ( { model | session = session }
                    , Cmd.none
                    )


view : Model -> Html Msg
view model =
    case model.session of
        LoggedIn loggedInModel ->
            webhookDashboard loggedInModel

        _ ->
            loginPage model


loginPage : Model -> Html Msg
loginPage model =
    let
        statusMsg =
            case model.session of
                AttemptingLogin ->
                    Html.div [] [ Html.text "Logging in..." ]

                LoginFailure msg ->
                    Html.div [] [ Html.text msg ]

                _ ->
                    Html.text ""
    in
    Html.div
        []
        [ Html.input
            [ Event.onInput (UpdateUserId << UserUUID) ]
            []
        , Html.br [] []
        , Html.input
            [ Event.onInput (UpdateUserApiKey << UserApiKey) ]
            []
        , Html.br [] []
        , Html.button
            [ Event.onClick Login ]
            [ Html.text "Login" ]
        , Html.br [] []
        , statusMsg
        ]


webhookDashboard : LoggedInModel -> Html Msg
webhookDashboard model =
    case model.webhooks of
        Loading ->
            Html.text "Loading webhooks."

        Ready webhooks ->
            Html.div [] <|
                List.map
                    (\webhook ->
                        Html.p [] [ Html.text (Debug.toString webhook) ]
                    )
                    webhooks


requestUser : UserUUID -> UserApiKey -> Cmd Msg
requestUser (UserUUID uuid) (UserApiKey apiKey) =
    let
        customExpectJson : (Result String a -> msg) -> Decoder a -> Http.Expect msg
        customExpectJson toMsg decoder =
            Http.expectStringResponse toMsg <|
                \response ->
                    case response of
                        Http.BadStatus_ metadata body ->
                            case Decode.decodeString decoder body of
                                Ok value ->
                                    Ok value

                                Err err ->
                                    Err ("Something went wrong while parsing the response from the server. Error message was: " ++ Decode.errorToString err)

                        Http.GoodStatus_ metadata body ->
                            case Decode.decodeString decoder body of
                                Ok value ->
                                    Ok value

                                Err err ->
                                    Err ("Something went wrong while parsing the response from the server. Error message was: " ++ Decode.errorToString err)

                        _ ->
                            Err "Something went wrong. Please try again later."
    in
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "x-api-user" uuid
            , Http.header "x-api-key" apiKey
            ]
        , url = "https://habitica.com/api/v3/user"
        , body = Http.emptyBody
        , expect = customExpectJson ReceiveLoginResult loginDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


loginDecoder : Decoder Session
loginDecoder =
    let
        handleLoginResult : Bool -> Decoder Session
        handleLoginResult success =
            if success then
                Decode.at
                    [ "data", "webhooks" ]
                    (Decode.map (LoggedIn << LoggedInModel << Ready) <|
                        Decode.list webhookDecoder
                    )

            else
                Decode.field "message" (Decode.map LoginFailure Decode.string)
    in
    Decode.field "success" Decode.bool
        |> Decode.andThen handleLoginResult


webhookDecoder : Decoder Webhook
webhookDecoder =
    let
        taskActivityOptionsDecoder : Decoder TaskActivityOptions
        taskActivityOptionsDecoder =
            Decode.map4 TaskActivityOptions
                (Decode.field "created" Decode.bool)
                (Decode.field "updated" Decode.bool)
                (Decode.field "deleted" Decode.bool)
                (Decode.field "scored" Decode.bool)

        groupChatReceivedOptionsDecoder : Decoder GroupChatReceivedOptions
        groupChatReceivedOptionsDecoder =
            Decode.map GroupChatReceivedOptions
                (Decode.field "groupId" (Decode.map GroupUUID Decode.string))

        userActivityOptionsDecoder : Decoder UserActivityOptions
        userActivityOptionsDecoder =
            Decode.map3 UserActivityOptions
                (Decode.field "petHatched" Decode.bool)
                (Decode.field "mountRaised" Decode.bool)
                (Decode.field "leveledUp" Decode.bool)

        decodeType : String -> Decoder Type
        decodeType typeStr =
            let
                decodeOptions =
                    case typeStr of
                        "taskActivity" ->
                            Decode.map TaskActivity taskActivityOptionsDecoder

                        "groupChatReceived" ->
                            Decode.map GroupChatReceived groupChatReceivedOptionsDecoder

                        "userActivity" ->
                            Decode.map UserActivity userActivityOptionsDecoder

                        _ ->
                            Decode.fail ("Invalid webhook type: " ++ typeStr)
            in
            Decode.field "options" decodeOptions

        decodeUrl : String -> Decoder Url
        decodeUrl urlStr =
            Url.fromString urlStr
                |> Maybe.andThen (Just << Decode.succeed)
                |> Maybe.withDefault (Decode.fail ("Invalid webhook URL: " ++ urlStr))
    in
    Decode.map5 Webhook
        (Decode.field "id" (Decode.map WebhookUUID Decode.string))
        (Decode.field "url" Decode.string |> Decode.andThen decodeUrl)
        (Decode.field "enabled" Decode.bool)
        (Decode.field "label" (Decode.map Label Decode.string))
        (Decode.field "type" Decode.string |> Decode.andThen decodeType)


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( empty, Cmd.none )
        , view = \model -> Browser.Document "Habitica Webhook Manager" [ view model ]
        , update = update
        , subscriptions = \_ -> Sub.none
        }
