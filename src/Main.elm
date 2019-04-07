module Main exposing (main)

import Browser
import Element exposing (Element)
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as Event
import Http
import Json.Decode as Decode exposing (Decoder)
import Url exposing (Url)
import Uuid exposing (Uuid)


type alias Model =
    { userId : UserUUID
    , userApiKey : UserApiKey
    , session : Session
    }


type alias LoggedInModel =
    { webhooks : WebhookStatus
    , editor : Maybe WebhookEditForm
    }


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


type
    UserUUID
    -- This one is a string instead of a Uuid type as
    -- the user has to input it directly, and attempting
    -- to login is validation enough here.
    = UserUUID String


type UserApiKey
    = UserApiKey String


type WebhookUUID
    = WebhookUUID Uuid


type GroupUUID
    = GroupUUID Uuid


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
    { groupId : GroupUUID }


type alias UserActivityOptions =
    { petHatched : Bool
    , mountRaised : Bool
    , leveledUp : Bool
    }


type alias Webhook =
    { id : WebhookUUID
    , url : Url
    , enabled : Bool
    , label : String
    , type_ : Type
    }


type EditType
    = EditTaskActivity
    | EditGroupChatReceived
    | EditUserActivity


type EditableTaskActivityOptions
    = EditableTaskActivityOptions TaskActivityOptions


type EditableGroupChatReceivedOptions
    = EditableGroupChatReceivedOptions { groupId : String }


type EditableUserActivityOptions
    = EditableUserActivityOptions UserActivityOptions


emptyEditableTaskActivity : EditableTaskActivityOptions
emptyEditableTaskActivity =
    EditableTaskActivityOptions
        { created = False
        , updated = False
        , deleted = False
        , scored = False
        }


emptyEditableGroupChatReceived : EditableGroupChatReceivedOptions
emptyEditableGroupChatReceived =
    EditableGroupChatReceivedOptions
        { groupId = "" }


emptyEditableUserActivity : EditableUserActivityOptions
emptyEditableUserActivity =
    EditableUserActivityOptions
        { petHatched = False
        , mountRaised = False
        , leveledUp = False
        }


type alias WebhookEditForm =
    { id : String
    , url : String
    , enabled : Bool
    , label : String
    , type_ : EditType
    , taskActivityOptions : EditableTaskActivityOptions
    , groupChatReceivedOptions : EditableGroupChatReceivedOptions
    , userActivityOptions : EditableUserActivityOptions
    }


type Msg
    = UpdateUserId UserUUID
    | UpdateUserApiKey UserApiKey
    | Login
    | ReceiveLoginResult (Result String Session)
    | Edit Webhook
    | EditorSetEnabled Bool
    | EditorSetLabel String
    | EditorSetId String
    | EditorSetUrl String
    | EditorSetType EditType
    | EditorSetOptCreated Bool
    | EditorSetOptUpdated Bool
    | EditorSetOptDeleted Bool
    | EditorSetOptScored Bool
    | EditorSetOptPetHatched Bool
    | EditorSetOptMountRaised Bool
    | EditorSetOptLeveledUp Bool
    | EditorSetOptGroupId String


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

        Edit webhook ->
            case model.session of
                LoggedIn loggedInModel ->
                    let
                        (WebhookUUID id) =
                            webhook.id

                        hookOpts =
                            typeToEditable webhook.type_
                    in
                    ( { model
                        | session =
                            LoggedIn
                                { loggedInModel
                                    | editor =
                                        Just <|
                                            { id = Uuid.toString id
                                            , url = Url.toString webhook.url
                                            , enabled = webhook.enabled
                                            , label = webhook.label
                                            , type_ = hookOpts.editType
                                            , taskActivityOptions = hookOpts.opts.taskActivityOptions
                                            , groupChatReceivedOptions = hookOpts.opts.groupChatReceivedOptions
                                            , userActivityOptions = hookOpts.opts.userActivityOptions
                                            }
                                }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        EditorSetEnabled enabled ->
            ( mapEditor (\form -> { form | enabled = enabled }) model, Cmd.none )

        EditorSetLabel label ->
            ( mapEditor (\form -> { form | label = label }) model, Cmd.none )

        EditorSetId id ->
            ( mapEditor (\form -> { form | id = id }) model, Cmd.none )

        EditorSetUrl url ->
            ( mapEditor (\form -> { form | url = url }) model, Cmd.none )

        EditorSetType type_ ->
            ( mapEditor (\form -> { form | type_ = type_ }) model, Cmd.none )

        EditorSetOptCreated created ->
            ( mapTaskActivityOptions (\opts -> { opts | created = created }) model, Cmd.none )

        EditorSetOptUpdated updated ->
            ( mapTaskActivityOptions (\opts -> { opts | updated = updated }) model, Cmd.none )

        EditorSetOptDeleted deleted ->
            ( mapTaskActivityOptions (\opts -> { opts | deleted = deleted }) model, Cmd.none )

        EditorSetOptScored scored ->
            ( mapTaskActivityOptions (\opts -> { opts | scored = scored }) model, Cmd.none )

        EditorSetOptPetHatched petHatched ->
            ( mapUserActivityOptions (\opts -> { opts | petHatched = petHatched }) model, Cmd.none )

        EditorSetOptMountRaised mountRaised ->
            ( mapUserActivityOptions (\opts -> { opts | mountRaised = mountRaised }) model, Cmd.none )

        EditorSetOptLeveledUp leveledUp ->
            ( mapUserActivityOptions (\opts -> { opts | leveledUp = leveledUp }) model, Cmd.none )

        EditorSetOptGroupId groupId ->
            ( mapGroupChatReceivedOptions (\opts -> { opts | groupId = groupId }) model, Cmd.none )


type alias EditorOpts =
    { taskActivityOptions : EditableTaskActivityOptions
    , groupChatReceivedOptions : EditableGroupChatReceivedOptions
    , userActivityOptions : EditableUserActivityOptions
    }


typeToEditable : Type -> { editType : EditType, opts : EditorOpts }
typeToEditable type_ =
    case type_ of
        TaskActivity opts ->
            { editType = EditTaskActivity
            , opts =
                { taskActivityOptions = EditableTaskActivityOptions opts
                , groupChatReceivedOptions = emptyEditableGroupChatReceived
                , userActivityOptions = emptyEditableUserActivity
                }
            }

        GroupChatReceived opts ->
            let
                (GroupUUID idUnwrapped) =
                    opts.groupId
            in
            { editType = EditGroupChatReceived
            , opts =
                { taskActivityOptions = emptyEditableTaskActivity
                , groupChatReceivedOptions =
                    EditableGroupChatReceivedOptions
                        { groupId = Uuid.toString idUnwrapped }
                , userActivityOptions = emptyEditableUserActivity
                }
            }

        UserActivity opts ->
            { editType = EditUserActivity
            , opts =
                { taskActivityOptions = emptyEditableTaskActivity
                , groupChatReceivedOptions = emptyEditableGroupChatReceived
                , userActivityOptions = EditableUserActivityOptions opts
                }
            }


mapEditor : (WebhookEditForm -> WebhookEditForm) -> Model -> Model
mapEditor f model =
    case model.session of
        LoggedIn loggedInModel ->
            case loggedInModel.editor of
                Nothing ->
                    model

                Just editForm ->
                    { model
                        | session =
                            LoggedIn <|
                                { loggedInModel
                                    | editor = Just (f editForm)
                                }
                    }

        _ ->
            model


mapTaskActivityOptions : (TaskActivityOptions -> TaskActivityOptions) -> Model -> Model
mapTaskActivityOptions f =
    mapEditor
        (\form ->
            let
                (EditableTaskActivityOptions opts) =
                    form.taskActivityOptions
            in
            { form | taskActivityOptions = EditableTaskActivityOptions <| f opts }
        )


mapGroupChatReceivedOptions : ({ groupId : String } -> { groupId : String }) -> Model -> Model
mapGroupChatReceivedOptions f =
    mapEditor
        (\form ->
            let
                (EditableGroupChatReceivedOptions opts) =
                    form.groupChatReceivedOptions
            in
            { form | groupChatReceivedOptions = EditableGroupChatReceivedOptions <| f opts }
        )


mapUserActivityOptions : (UserActivityOptions -> UserActivityOptions) -> Model -> Model
mapUserActivityOptions f =
    mapEditor
        (\form ->
            let
                (EditableUserActivityOptions opts) =
                    form.userActivityOptions
            in
            { form | userActivityOptions = EditableUserActivityOptions <| f opts }
        )


view : Model -> Html Msg
view model =
    Element.layout [] <|
        case model.session of
            LoggedIn loggedInModel ->
                webhookDashboard loggedInModel

            _ ->
                loginPage model


loginPage : Model -> Element Msg
loginPage model =
    let
        statusMsg =
            case model.session of
                AttemptingLogin ->
                    Element.text "Logging in..."

                LoginFailure msg ->
                    Element.text msg

                _ ->
                    Element.none

        (UserUUID userId) =
            model.userId

        (UserApiKey apiKey) =
            model.userApiKey
    in
    Element.column
        []
        [ Input.username
            [ Element.htmlAttribute (A.name "username") ]
            { onChange = UpdateUserId << UserUUID
            , text = userId
            , placeholder = Nothing
            , label = Input.labelLeft [] (Element.text "User ID")
            }
        , Input.currentPassword
            []
            { onChange = UpdateUserApiKey << UserApiKey
            , text = apiKey
            , placeholder = Nothing
            , label = Input.labelLeft [] (Element.text "API Key")
            , show = False
            }
        , Input.button
            []
            { onPress = Just Login
            , label = Element.text "Login"
            }
        , statusMsg
        ]


webhookDashboard : LoggedInModel -> Element Msg
webhookDashboard model =
    case model.webhooks of
        Loading ->
            Element.text "Loading webhooks."

        Ready webhooks ->
            case model.editor of
                Nothing ->
                    Element.column [] <|
                        List.map
                            (\webhook ->
                                Element.paragraph
                                    []
                                    [ Element.text (Debug.toString webhook)
                                    , Input.button
                                        []
                                        { onPress = Just (Edit webhook)
                                        , label = Element.text "Edit"
                                        }
                                    ]
                            )
                            webhooks

                Just editor ->
                    Element.column
                        []
                        [ Input.checkbox []
                            { onChange = EditorSetEnabled
                            , icon =
                                \bool ->
                                    if bool then
                                        Element.text "☑"

                                    else
                                        Element.text "☐"
                            , checked = editor.enabled
                            , label = Input.labelLeft [] (Element.text "Enabled")
                            }
                        , Input.text
                            []
                            { onChange = EditorSetLabel
                            , text = editor.label
                            , placeholder = Nothing
                            , label = Input.labelLeft [] (Element.text "Label")
                            }
                        , Input.text
                            []
                            { onChange = EditorSetId
                            , text = editor.id
                            , placeholder = Nothing
                            , label = Input.labelLeft [] (Element.text "UUID")
                            }
                        , Input.text
                            []
                            { onChange = EditorSetUrl
                            , text = editor.url
                            , placeholder = Nothing
                            , label = Input.labelLeft [] (Element.text "URL")
                            }
                        , Input.radioRow
                            []
                            { onChange = EditorSetType
                            , options =
                                [ Input.optionWith EditTaskActivity <|
                                    \state ->
                                        if state /= Input.Selected then
                                            Element.text "[ ] taskActivity"

                                        else
                                            Element.text <| "[x] taskActivity"
                                , Input.optionWith EditGroupChatReceived <|
                                    \state ->
                                        if state /= Input.Selected then
                                            Element.text "[ ] groupChatReceived"

                                        else
                                            Element.text <| "[x] groupChatReceived"
                                , Input.optionWith EditUserActivity <|
                                    \state ->
                                        if state /= Input.Selected then
                                            Element.text "[ ] userActivity"

                                        else
                                            Element.text <| "[x] userActivity"
                                ]
                            , selected = Just editor.type_
                            , label = Input.labelAbove [] (Element.text "Webhook Type")
                            }
                        , case editor.type_ of
                            EditTaskActivity ->
                                let
                                    (EditableTaskActivityOptions opts) =
                                        editor.taskActivityOptions
                                in
                                Element.column
                                    []
                                    [ checkbox EditorSetOptCreated opts.created "created"
                                    , checkbox EditorSetOptUpdated opts.updated "updated"
                                    , checkbox EditorSetOptDeleted opts.deleted "deleted"
                                    , checkbox EditorSetOptScored opts.scored "scored"
                                    ]

                            EditGroupChatReceived ->
                                let
                                    (EditableGroupChatReceivedOptions opts) =
                                        editor.groupChatReceivedOptions
                                in
                                Element.column
                                    []
                                    [ Input.text
                                        []
                                        { onChange = EditorSetOptGroupId
                                        , text = opts.groupId
                                        , placeholder = Nothing
                                        , label = Input.labelLeft [] (Element.text "Group ID")
                                        }
                                    ]

                            EditUserActivity ->
                                let
                                    (EditableUserActivityOptions opts) =
                                        editor.userActivityOptions
                                in
                                Element.column
                                    []
                                    [ checkbox EditorSetOptMountRaised opts.mountRaised "mountRaised"
                                    , checkbox EditorSetOptPetHatched opts.petHatched "petHatched"
                                    , checkbox EditorSetOptLeveledUp opts.leveledUp "leveledUp"
                                    ]
                        ]


checkbox : (Bool -> msg) -> Bool -> String -> Element msg
checkbox msg isChecked labelTxt =
    Input.checkbox []
        { onChange = msg
        , icon =
            \bool ->
                if bool then
                    Element.text "☑"

                else
                    Element.text "☐"
        , checked = isChecked
        , label = Input.labelLeft [] (Element.text labelTxt)
        }


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
                    (Decode.map (\webhook -> LoggedIn <| LoggedInModel (Ready webhook) Nothing) <|
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
                (Decode.field "groupId" (Decode.map GroupUUID Uuid.decoder))

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
        (Decode.field "id" (Decode.map WebhookUUID Uuid.decoder))
        (Decode.field "url" Decode.string |> Decode.andThen decodeUrl)
        (Decode.field "enabled" Decode.bool)
        (Decode.field "label" Decode.string)
        (Decode.field "type" Decode.string |> Decode.andThen decodeType)


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( empty, Cmd.none )
        , view = \model -> Browser.Document "Habitica Webhook Manager" [ view model ]
        , update = update
        , subscriptions = \_ -> Sub.none
        }
