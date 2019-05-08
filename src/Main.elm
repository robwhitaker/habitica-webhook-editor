module Main exposing (main)

import Browser
import Element exposing (Element)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as Event
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Task exposing (Task)
import Url exposing (Url)
import Uuid exposing (Uuid)


type alias Model =
    { userId : UserUUID
    , userApiKey : UserApiKey
    , session : Session
    }


type alias LoggedInModel =
    { webhooks : WebhookStatus
    , editor : Maybe Editor
    , confirm : Maybe Confirmation
    }


type alias Confirmation =
    { text : String
    , action : Msg
    }


type alias Editor =
    ( WebhookEditForm, List ValidationError )


empty : Model
empty =
    { userId = UserUUID ""
    , userApiKey = UserApiKey ""
    , session = NotLoggedIn
    }


type alias ValidationError =
    String


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
    , checklistScored : Bool
    }


type alias GroupChatReceivedOptions =
    { groupId : GroupUUID }


type alias UserActivityOptions =
    { petHatched : Bool
    , mountRaised : Bool
    , leveledUp : Bool
    }


type alias Webhook =
    { id : Maybe WebhookUUID
    , url : Url
    , label : String
    , enabled : Bool
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
        , checklistScored = False
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
    { id : Maybe String
    , url : String
    , enabled : Bool
    , label : String
    , type_ : EditType
    , taskActivityOptions : EditableTaskActivityOptions
    , groupChatReceivedOptions : EditableGroupChatReceivedOptions
    , userActivityOptions : EditableUserActivityOptions
    }


defaultWebhookEditForm : WebhookEditForm
defaultWebhookEditForm =
    { id = Nothing
    , url = ""
    , enabled = True
    , label = ""
    , type_ = EditTaskActivity
    , taskActivityOptions = emptyEditableTaskActivity
    , groupChatReceivedOptions = emptyEditableGroupChatReceived
    , userActivityOptions = emptyEditableUserActivity
    }


type Msg
    = UpdateUserId UserUUID
    | UpdateUserApiKey UserApiKey
    | Login
    | ReceiveLoginResult (Result String Session)
    | SavedWebhook (Result (Http.Response String) ())
    | DeletedWebhook (Result Http.Error ())
    | Edit (Maybe Webhook)
    | Delete WebhookUUID
    | ConfirmDelete Webhook
    | ConfirmCancel
    | EditorSetEnabled Bool
    | EditorSetLabel String
    | EditorSetId String
    | EditorSetUrl String
    | EditorSetType EditType
    | EditorSetOptCreated Bool
    | EditorSetOptUpdated Bool
    | EditorSetOptDeleted Bool
    | EditorSetOptScored Bool
    | EditorSetOptChecklistScored Bool
    | EditorSetOptPetHatched Bool
    | EditorSetOptMountRaised Bool
    | EditorSetOptLeveledUp Bool
    | EditorSetOptGroupId String
    | EditorSubmit
    | EditorCancel


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

        SavedWebhook result ->
            let
                -- TODO: this is just copied from above to reload the webhooks
                --       and probably shouldn't be duplicated like this
                loginCmd =
                    requestUser model.userId model.userApiKey
            in
            ( model, loginCmd )

        DeletedWebhook result ->
            let
                -- TODO: this is just copied from above to reload the webhooks
                --       and probably shouldn't be duplicated like this
                loginCmd =
                    requestUser model.userId model.userApiKey
            in
            ( model, loginCmd )

        Edit maybeWebhook ->
            case model.session of
                LoggedIn loggedInModel ->
                    case maybeWebhook of
                        Nothing ->
                            ( { model
                                | session =
                                    LoggedIn
                                        { loggedInModel | editor = Just ( defaultWebhookEditForm, [] ) }
                              }
                            , Cmd.none
                            )

                        Just webhook ->
                            let
                                webhookUuid =
                                    Maybe.map (\(WebhookUUID uuid) -> Uuid.toString uuid) webhook.id

                                hookOpts =
                                    typeToEditable webhook.type_
                            in
                            ( { model
                                | session =
                                    LoggedIn
                                        { loggedInModel
                                            | editor =
                                                Just <|
                                                    ( { id = webhookUuid
                                                      , url = Url.toString webhook.url
                                                      , enabled = webhook.enabled
                                                      , label = webhook.label
                                                      , type_ = hookOpts.editType
                                                      , taskActivityOptions = hookOpts.opts.taskActivityOptions
                                                      , groupChatReceivedOptions = hookOpts.opts.groupChatReceivedOptions
                                                      , userActivityOptions = hookOpts.opts.userActivityOptions
                                                      }
                                                    , []
                                                    )
                                        }
                              }
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )

        Delete uuid ->
            case model.session of
                LoggedIn loggedInModel ->
                    ( { model | session = LoggedIn { loggedInModel | webhooks = Loading } }
                    , deleteWebhook model.userId model.userApiKey uuid
                    )

                _ ->
                    ( model, Cmd.none )

        ConfirmDelete webhook ->
            case model.session of
                LoggedIn loggedInModel ->
                    ( { model
                        | session =
                            LoggedIn
                                { loggedInModel
                                    | confirm =
                                        webhook.id
                                            |> Maybe.andThen
                                                (\uuid ->
                                                    Just <|
                                                        Confirmation
                                                            "Are you sure you want to delete this webhook?"
                                                            (Delete uuid)
                                                )
                                }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ConfirmCancel ->
            case model.session of
                LoggedIn loggedInModel ->
                    ( { model | session = LoggedIn { loggedInModel | confirm = Nothing } }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        EditorSetEnabled enabled ->
            ( mapEditorFormFields (\form -> { form | enabled = enabled }) model, Cmd.none )

        EditorSetLabel label ->
            ( mapEditorFormFields (\form -> { form | label = label }) model, Cmd.none )

        EditorSetId id ->
            ( mapEditorFormFields (\form -> { form | id = Just id }) model, Cmd.none )

        EditorSetUrl url ->
            ( mapEditorFormFields (\form -> { form | url = url }) model, Cmd.none )

        EditorSetType type_ ->
            ( mapEditorFormFields (\form -> { form | type_ = type_ }) model, Cmd.none )

        EditorSetOptCreated created ->
            ( mapTaskActivityOptions (\opts -> { opts | created = created }) model, Cmd.none )

        EditorSetOptUpdated updated ->
            ( mapTaskActivityOptions (\opts -> { opts | updated = updated }) model, Cmd.none )

        EditorSetOptDeleted deleted ->
            ( mapTaskActivityOptions (\opts -> { opts | deleted = deleted }) model, Cmd.none )

        EditorSetOptScored scored ->
            ( mapTaskActivityOptions (\opts -> { opts | scored = scored }) model, Cmd.none )

        EditorSetOptChecklistScored checklistScored ->
            ( mapTaskActivityOptions (\opts -> { opts | checklistScored = checklistScored }) model, Cmd.none )

        EditorSetOptPetHatched petHatched ->
            ( mapUserActivityOptions (\opts -> { opts | petHatched = petHatched }) model, Cmd.none )

        EditorSetOptMountRaised mountRaised ->
            ( mapUserActivityOptions (\opts -> { opts | mountRaised = mountRaised }) model, Cmd.none )

        EditorSetOptLeveledUp leveledUp ->
            ( mapUserActivityOptions (\opts -> { opts | leveledUp = leveledUp }) model, Cmd.none )

        EditorSetOptGroupId groupId ->
            ( mapGroupChatReceivedOptions (\opts -> { opts | groupId = groupId }) model, Cmd.none )

        EditorSubmit ->
            case getEditor model of
                Nothing ->
                    ( model, Cmd.none )

                Just ( formFields, _ ) ->
                    case editorToWebhook formFields of
                        Err errors ->
                            ( mapEditorErrors (always errors) model, Cmd.none )

                        Ok webhook ->
                            let
                                newModel =
                                    { model
                                        | session =
                                            LoggedIn
                                                { webhooks = Loading
                                                , editor = Nothing
                                                , confirm = Nothing
                                                }
                                    }

                                saveCmd =
                                    saveWebhookWrapper model.userId model.userApiKey webhook
                                        |> Task.attempt SavedWebhook
                            in
                            ( newModel
                            , saveCmd
                            )

        EditorCancel ->
            case model.session of
                LoggedIn loggedInModel ->
                    ( { model | session = LoggedIn { loggedInModel | editor = Nothing } }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


editorToWebhook : WebhookEditForm -> Result (List ValidationError) Webhook
editorToWebhook form =
    let
        validatedUrl =
            form.url
                |> Url.fromString
                |> Maybe.map Ok
                |> Maybe.withDefault (Err "Invalid URL.")

        validatedHookId =
            case form.id of
                Nothing ->
                    Ok Nothing

                Just someId ->
                    someId
                        |> Uuid.fromString
                        |> Maybe.map (Ok << Just << WebhookUUID)
                        |> Maybe.withDefault (Err "Malformed UUID provided for field 'ID'.")
    in
    case form.type_ of
        EditTaskActivity ->
            let
                (EditableTaskActivityOptions opts) =
                    form.taskActivityOptions
            in
            Ok
                (\url hookId ->
                    { url = url
                    , id = hookId
                    , label = form.label
                    , enabled = form.enabled
                    , type_ = TaskActivity opts
                    }
                )
                |> andMapAccumErr validatedUrl
                |> andMapAccumErr validatedHookId

        EditGroupChatReceived ->
            let
                (EditableGroupChatReceivedOptions opts) =
                    form.groupChatReceivedOptions

                validatedGroupId =
                    opts.groupId
                        |> Uuid.fromString
                        |> Maybe.map (Ok << GroupUUID)
                        |> Maybe.withDefault (Err "Malformed UUID provided for field 'Group ID'.")
            in
            Ok
                (\url hookId groupId ->
                    { url = url
                    , id = hookId
                    , label = form.label
                    , enabled = form.enabled
                    , type_ = GroupChatReceived { groupId = groupId }
                    }
                )
                |> andMapAccumErr validatedUrl
                |> andMapAccumErr validatedHookId
                |> andMapAccumErr validatedGroupId

        EditUserActivity ->
            let
                (EditableUserActivityOptions opts) =
                    form.userActivityOptions
            in
            Ok
                (\url hookId ->
                    { url = url
                    , id = hookId
                    , label = form.label
                    , enabled = form.enabled
                    , type_ = UserActivity opts
                    }
                )
                |> andMapAccumErr validatedUrl
                |> andMapAccumErr validatedHookId


andMapAccumErr : Result x a -> Result (List x) (a -> b) -> Result (List x) b
andMapAccumErr result fnResult =
    case fnResult of
        Err errList ->
            case result of
                Ok _ ->
                    Err errList

                Err err ->
                    Err (err :: errList)

        Ok f ->
            case result of
                Ok val ->
                    Ok (f val)

                Err err ->
                    Err [ err ]


getEditor : Model -> Maybe Editor
getEditor model =
    case model.session of
        LoggedIn loggedInModel ->
            loggedInModel.editor

        _ ->
            Nothing


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


mapEditor : (Editor -> Editor) -> Model -> Model
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


mapEditorFormFields : (WebhookEditForm -> WebhookEditForm) -> Model -> Model
mapEditorFormFields f =
    mapEditor
        (\form ->
            let
                ( formFields, errors ) =
                    form
            in
            ( f formFields, errors )
        )


mapEditorErrors : (List ValidationError -> List ValidationError) -> Model -> Model
mapEditorErrors f =
    mapEditor
        (\form ->
            let
                ( formFields, errors ) =
                    form
            in
            ( formFields, f errors )
        )


mapTaskActivityOptions : (TaskActivityOptions -> TaskActivityOptions) -> Model -> Model
mapTaskActivityOptions f =
    mapEditorFormFields
        (\form ->
            let
                (EditableTaskActivityOptions opts) =
                    form.taskActivityOptions
            in
            { form | taskActivityOptions = EditableTaskActivityOptions <| f opts }
        )


mapGroupChatReceivedOptions : ({ groupId : String } -> { groupId : String }) -> Model -> Model
mapGroupChatReceivedOptions f =
    mapEditorFormFields
        (\form ->
            let
                (EditableGroupChatReceivedOptions opts) =
                    form.groupChatReceivedOptions
            in
            { form | groupChatReceivedOptions = EditableGroupChatReceivedOptions <| f opts }
        )


mapUserActivityOptions : (UserActivityOptions -> UserActivityOptions) -> Model -> Model
mapUserActivityOptions f =
    mapEditorFormFields
        (\form ->
            let
                (EditableUserActivityOptions opts) =
                    form.userActivityOptions
            in
            { form | userActivityOptions = EditableUserActivityOptions <| f opts }
        )


theme =
    { text =
        { normal = Element.rgb 1 1 1
        , error = Element.rgb255 180 0 0
        , faded = Element.rgb255 125 125 125
        , link = Element.rgb255 180 0 0
        , linkGlow = Element.rgb 1 1 1
        , enabled = Element.rgb 0.2 0.8 0.2
        }
    , button =
        { yes = Element.rgb255 90 0 0
        , yesText = Element.rgb 1 1 1
        , yesHover = Element.rgb255 70 0 0
        , yesHoverGlow = Element.rgb 0.5 0.5 0.5
        , no = Element.rgb 0.5 0.5 0.5
        , noText = Element.rgb 0 0 0
        , noHover = Element.rgb 0.3 0.3 0.3
        , noHoverGlow = Element.rgb 0.5 0.5 0.5
        }
    , input =
        { background = Element.rgb255 50 50 50
        , text = Element.rgb 1 1 1
        , placeholderText = Element.rgb 0.8 0.8 0.8
        }
    , page =
        { background = Element.rgb255 10 10 10 }
    , misc =
        { widget = Element.rgb255 20 20 20
        , widgetShadow = Element.rgba 0 0 0 0.4
        }
    }


view : Model -> Html Msg
view model =
    Element.layout
        [ BG.color theme.page.background
        , Font.color theme.text.normal
        ]
    <|
        case model.session of
            LoggedIn loggedInModel ->
                webhookDashboard loggedInModel

            _ ->
                loginPage model


contentColumn : List (Element Msg) -> Element Msg
contentColumn =
    Element.column
        [ Element.centerX
        , Element.centerY
        , Element.spacing 10
        ]


loginPage : Model -> Element Msg
loginPage model =
    let
        statusMsg =
            case model.session of
                AttemptingLogin ->
                    Element.text "Logging in..."

                LoginFailure msg ->
                    Element.el
                        [ Font.color theme.text.error
                        , Element.centerX
                        ]
                        (Element.text msg)

                _ ->
                    Element.none

        (UserUUID userId) =
            model.userId

        (UserApiKey apiKey) =
            model.userApiKey
    in
    contentColumn
        [ h1 "Habitica Webhook Editor"
        , Input.username
            [ Element.htmlAttribute (A.name "username")
            , BG.color theme.input.background
            , Font.color theme.input.text
            , Border.width 0
            ]
            { onChange = UpdateUserId << UserUUID
            , text = userId
            , placeholder = inputPlaceholder "User ID"
            , label = Input.labelHidden "User ID"
            }
        , Input.currentPassword
            [ Element.htmlAttribute (A.name "password")
            , BG.color theme.input.background
            , Font.color theme.input.text
            , Border.width 0
            ]
            { onChange = UpdateUserApiKey << UserApiKey
            , text = apiKey
            , placeholder = inputPlaceholder "API Key"
            , label = Input.labelHidden "API Key"
            , show = False
            }
        , yesButton
            [ Element.width Element.fill ]
            (Just Login)
            "Login"
        , Element.el
            [ Element.width Element.fill
            , Font.size 16
            , Font.center
            ]
            statusMsg
        ]


inputPlaceholder : String -> Maybe (Input.Placeholder Msg)
inputPlaceholder txt =
    Just <|
        Input.placeholder
            [ Font.center
            , Font.color theme.input.placeholderText
            ]
            (Element.text txt)


webhookDashboard : LoggedInModel -> Element Msg
webhookDashboard model =
    contentColumn <|
        case model.webhooks of
            Loading ->
                [ Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                    (Element.text "Loading webhooks.")
                ]

            Ready webhooks ->
                case ( model.confirm, model.editor ) of
                    ( Just confirm, _ ) ->
                        [ Element.text confirm.text
                        , yesButton [] (Just confirm.action) "Yes"
                        , noButton [] (Just ConfirmCancel) "Cancel"
                        ]

                    ( Nothing, Nothing ) ->
                        [ yesButton [ Element.centerX ] (Just (Edit Nothing)) "Create webhook"
                        ]
                            ++ List.map webhookView webhooks

                    ( Nothing, Just editor ) ->
                        let
                            ( fields, errors ) =
                                editor
                        in
                        [ Element.column [] <| List.map Element.text errors
                        , Input.checkbox []
                            { onChange = EditorSetEnabled
                            , icon =
                                \bool ->
                                    if bool then
                                        Element.text "☑"

                                    else
                                        Element.text "☐"
                            , checked = fields.enabled
                            , label = Input.labelLeft [] (Element.text "Enabled")
                            }
                        , Input.text
                            []
                            { onChange = EditorSetLabel
                            , text = fields.label
                            , placeholder = Nothing
                            , label = Input.labelLeft [] (Element.text "Label")
                            }
                        , Element.text
                            (Maybe.withDefault "New Webhook." fields.id)
                        , Input.text
                            []
                            { onChange = EditorSetUrl
                            , text = fields.url
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
                            , selected = Just fields.type_
                            , label = Input.labelAbove [] (Element.text "Webhook Type")
                            }
                        , case fields.type_ of
                            EditTaskActivity ->
                                let
                                    (EditableTaskActivityOptions opts) =
                                        fields.taskActivityOptions
                                in
                                Element.column
                                    []
                                    [ checkbox EditorSetOptCreated opts.created "created"
                                    , checkbox EditorSetOptUpdated opts.updated "updated"
                                    , checkbox EditorSetOptDeleted opts.deleted "deleted"
                                    , checkbox EditorSetOptScored opts.scored "scored"
                                    , checkbox EditorSetOptChecklistScored opts.checklistScored "checklistScored"
                                    ]

                            EditGroupChatReceived ->
                                let
                                    (EditableGroupChatReceivedOptions opts) =
                                        fields.groupChatReceivedOptions
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
                                        fields.userActivityOptions
                                in
                                Element.column
                                    []
                                    [ checkbox EditorSetOptMountRaised opts.mountRaised "mountRaised"
                                    , checkbox EditorSetOptPetHatched opts.petHatched "petHatched"
                                    , checkbox EditorSetOptLeveledUp opts.leveledUp "leveledUp"
                                    ]
                        , Element.row
                            [ Element.spacing 10
                            , Element.alignRight
                            ]
                            [ yesButton [] (Just EditorSubmit) "Submit"
                            , noButton [] (Just EditorCancel) "Cancel"
                            ]
                        ]


webhookView : Webhook -> Element Msg
webhookView webhook =
    let
        labelTxt =
            if String.trim webhook.label == "" then
                "Unlabeled webhook"

            else
                webhook.label

        idTxt =
            case webhook.id of
                Nothing ->
                    "No ID (if you see this, something went wrong)"

                Just (WebhookUUID uuid) ->
                    Uuid.toString uuid

        boolOpt txt enabled =
            Element.el
                (if enabled then
                    [ Font.color theme.text.enabled
                    , Font.bold
                    ]

                 else
                    [ Font.color theme.text.faded
                    , Font.italic
                    ]
                )
                (Element.text txt)

        optPara =
            List.intersperse (Element.text " ")
                >> Element.paragraph
                    [ Font.size 16 ]

        ( type_, optsElem ) =
            case webhook.type_ of
                TaskActivity opts ->
                    ( "taskActivity"
                    , optPara
                        [ boolOpt "created" opts.created
                        , boolOpt "updated" opts.updated
                        , boolOpt "deleted" opts.deleted
                        , boolOpt "scored" opts.scored
                        , boolOpt "checklistScored" opts.checklistScored
                        ]
                    )

                GroupChatReceived opts ->
                    let
                        (GroupUUID groupId) =
                            opts.groupId
                    in
                    ( "groupChatReceived"
                    , optPara
                        [ Element.text <| "groupId: " ++ Uuid.toString groupId ]
                    )

                UserActivity opts ->
                    ( "userActivity"
                    , optPara
                        [ boolOpt "mountRaised" opts.mountRaised
                        , boolOpt "petHatched" opts.petHatched
                        , boolOpt "leveledUp" opts.leveledUp
                        ]
                    )
    in
    Element.column
        ([ Border.rounded 3
         , Element.spacing 10
         , Element.padding 10
         , Element.width (Element.fill |> Element.maximum 800)
         ]
            ++ (if not webhook.enabled then
                    [ Border.dashed
                    , Border.color theme.misc.widget
                    , Border.width 1
                    ]

                else
                    [ BG.color theme.misc.widget
                    , Border.shadow
                        { offset = ( 3, 3 )
                        , size = 4
                        , blur = 5
                        , color = theme.misc.widgetShadow
                        }
                    ]
               )
        )
        [ Element.column
            [ Element.width Element.fill
            , Element.spacing 4
            ]
            [ Element.row
                [ Element.width Element.fill
                , Element.spacing 30
                ]
                [ Element.paragraph
                    (if not webhook.enabled then
                        [ Font.color theme.text.faded
                        , Font.italic
                        ]

                     else
                        []
                    )
                    [ h2 labelTxt ]
                , Element.row
                    [ Element.alignRight
                    , Element.alignTop
                    , Element.spacing 10
                    ]
                    [ linkButton [] (Just (Edit <| Just webhook)) "Edit"
                    , linkButton [] (Just (ConfirmDelete webhook)) "Delete"
                    ]
                ]
            , linkButton [] Nothing (Url.toString webhook.url)
            ]
        , Element.column
            [ Font.family [ Font.typeface "Courier New" ]
            , Font.size 18
            , Element.spacing 4
            ]
            [ Element.el
                []
                (Element.text type_)
            , Element.wrappedRow
                [ Element.paddingEach
                    { left = 30
                    , right = 0
                    , top = 0
                    , bottom = 0
                    }
                ]
                [ Element.text "[ "
                , optsElem
                , Element.text " ]"
                ]
            ]
        , Element.el
            [ Font.size 14
            , Font.color theme.text.faded
            , Element.width Element.fill
            , Font.alignRight
            ]
            (Element.text idTxt)
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


button : List (Element.Attribute Msg) -> Maybe Msg -> String -> Element Msg
button extraAttrs action text =
    Input.button
        ([ Element.padding 10
         , Border.rounded 3
         , Font.center
         ]
            ++ extraAttrs
        )
        { onPress = action
        , label = Element.text text
        }


yesButton : List (Element.Attribute Msg) -> Maybe Msg -> String -> Element Msg
yesButton extraAttrs =
    button <|
        [ BG.color theme.button.yes
        , Font.color theme.button.yesText
        , Element.mouseOver
            [ BG.color theme.button.yesHover
            , Border.glow theme.button.yesHoverGlow 2
            ]
        ]
            ++ extraAttrs


noButton : List (Element.Attribute Msg) -> Maybe Msg -> String -> Element Msg
noButton extraAttrs =
    button <|
        [ BG.color theme.button.no
        , Font.color theme.button.noText
        , Element.mouseOver
            [ BG.color theme.button.noHover
            , Border.glow theme.button.noHoverGlow 2
            ]
        ]
            ++ extraAttrs


linkButton : List (Element.Attribute Msg) -> Maybe Msg -> String -> Element Msg
linkButton extraAttrs action text =
    Input.button
        ([ Font.color theme.text.link
         , Font.size 18
         , Element.mouseOver
            [ Font.glow theme.text.linkGlow 10
            ]
         ]
            ++ extraAttrs
        )
        { onPress = action
        , label = Element.text text
        }


h : Int -> List (Element.Attribute Msg) -> String -> Element Msg
h n extraAttrs =
    \text ->
        Element.el
            (extraAttrs ++ [ Region.heading n ])
            (Element.text text)


h1 : String -> Element Msg
h1 =
    h 1 [ Element.centerX, Font.size 32 ]


h2 : String -> Element Msg
h2 =
    h 2
        [ Font.size 28
        , Font.family [ Font.typeface "Trebuchet MS" ]
        ]


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


requestUser : UserUUID -> UserApiKey -> Cmd Msg
requestUser (UserUUID uuid) (UserApiKey apiKey) =
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


deleteWebhook : UserUUID -> UserApiKey -> WebhookUUID -> Cmd Msg
deleteWebhook (UserUUID userUuid) (UserApiKey apiKey) (WebhookUUID uuid) =
    Http.request
        { method = "DELETE"
        , headers =
            [ Http.header "x-api-user" userUuid
            , Http.header "x-api-key" apiKey
            ]
        , url = "https://habitica.com/api/v3/user/webhook/" ++ Uuid.toString uuid
        , body = Http.emptyBody
        , expect = Http.expectWhatever DeletedWebhook
        , timeout = Nothing
        , tracker = Nothing
        }



{- This is a hack fix to address a Habitica API bug where changing a webhook's options
   doesn't take unless you are also changing the type of the webhook. This wrapper calls
   saveWebook twice, the first time switching the type with garbage options and the second
   changing it back with the options we want.
-}


saveWebhookWrapper : UserUUID -> UserApiKey -> Webhook -> Task (Http.Response String) ()
saveWebhookWrapper uuid apiKey webhook =
    let
        tempType =
            case webhook.type_ of
                UserActivity _ ->
                    TaskActivity
                        { created = False
                        , updated = False
                        , deleted = False
                        , scored = False
                        , checklistScored = False
                        }

                _ ->
                    UserActivity
                        { petHatched = False
                        , mountRaised = False
                        , leveledUp = False
                        }
    in
    case webhook.id of
        Nothing ->
            saveWebhook uuid apiKey webhook

        Just _ ->
            saveWebhook uuid apiKey { webhook | type_ = tempType }
                |> Task.andThen (\_ -> saveWebhook uuid apiKey webhook)


saveWebhook : UserUUID -> UserApiKey -> Webhook -> Task (Http.Response String) ()
saveWebhook (UserUUID uuid) (UserApiKey apiKey) webhook =
    let
        ( method, endpoint ) =
            case webhook.id of
                Nothing ->
                    ( "POST"
                    , "https://habitica.com/api/v3/user/webhook"
                    )

                Just (WebhookUUID webhookUuid) ->
                    ( "PUT"
                    , "https://habitica.com/api/v3/user/webhook/" ++ Uuid.toString webhookUuid
                    )

        encodedWebhook =
            webhookEncoder webhook
    in
    Http.task
        { method = method
        , headers =
            [ Http.header "x-api-user" uuid
            , Http.header "x-api-key" apiKey
            ]
        , url = endpoint
        , body = Http.jsonBody encodedWebhook
        , resolver =
            Http.stringResolver
                (\res ->
                    case res of
                        Http.GoodStatus_ _ _ ->
                            Ok ()

                        other ->
                            Err res
                )
        , timeout = Nothing
        }


loginDecoder : Decoder Session
loginDecoder =
    let
        handleLoginResult : Bool -> Decoder Session
        handleLoginResult success =
            if success then
                Decode.at
                    [ "data", "webhooks" ]
                    (Decode.map (\webhook -> LoggedIn <| LoggedInModel (Ready webhook) Nothing Nothing) <|
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
            Decode.map5 TaskActivityOptions
                (Decode.field "created" Decode.bool)
                (Decode.field "updated" Decode.bool)
                (Decode.field "deleted" Decode.bool)
                (Decode.field "scored" Decode.bool)
                (Decode.field "checklistScored" Decode.bool)

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
        (Decode.field "id" (Decode.map (Just << WebhookUUID) Uuid.decoder))
        (Decode.field "url" Decode.string |> Decode.andThen decodeUrl)
        (Decode.field "label" Decode.string)
        (Decode.field "enabled" Decode.bool)
        (Decode.field "type" Decode.string |> Decode.andThen decodeType)


webhookEncoder : Webhook -> Value
webhookEncoder webhook =
    Encode.object <|
        [ ( "url", Encode.string (Url.toString webhook.url) )
        , ( "label"
            -- This is a janky hack because Habitica treats
            -- empty string as "no label field provided" and therefore
            -- fails to clear the field.
          , if String.length webhook.label > 0 then
                Encode.string webhook.label

            else
                Encode.string " "
          )
        , ( "enabled", Encode.bool webhook.enabled )
        ]
            ++ (case webhook.type_ of
                    TaskActivity opts ->
                        [ ( "type", Encode.string "taskActivity" )
                        , ( "options"
                          , Encode.object
                                [ ( "created", Encode.bool opts.created )
                                , ( "updated", Encode.bool opts.updated )
                                , ( "deleted", Encode.bool opts.deleted )
                                , ( "scored", Encode.bool opts.scored )
                                , ( "checklistScored", Encode.bool opts.checklistScored )
                                ]
                          )
                        ]

                    GroupChatReceived opts ->
                        let
                            (GroupUUID groupUuid) =
                                opts.groupId
                        in
                        [ ( "type", Encode.string "groupChatReceived" )
                        , ( "options"
                          , Encode.object
                                [ ( "groupId", Uuid.encode groupUuid ) ]
                          )
                        ]

                    UserActivity opts ->
                        [ ( "type", Encode.string "userActivity" )
                        , ( "options"
                          , Encode.object
                                [ ( "petHatched", Encode.bool opts.petHatched )
                                , ( "mountRaised", Encode.bool opts.mountRaised )
                                , ( "leveledUp", Encode.bool opts.leveledUp )
                                ]
                          )
                        ]
               )


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( empty, Cmd.none )
        , view = \model -> Browser.Document "Habitica Webhook Manager" [ view model ]
        , update = update
        , subscriptions = \_ -> Sub.none
        }
