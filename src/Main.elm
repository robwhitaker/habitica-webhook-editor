module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import FontAwesome.Icon as FA
import FontAwesome.Solid as FA
import FontAwesome.Styles as FA
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as Event
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Nonempty as Nonempty exposing (Nonempty)
import Task exposing (Task)
import Time exposing (Posix)
import Url exposing (Url)
import Uuid exposing (Uuid)



-- Constants


appName : String
appName =
    "HabiticaWebhookEditor"


maintainerId : String
maintainerId =
    "cab16cfa-e951-4dc3-a468-1abadc1dd109"



-- Template text


secondsUntilRetry : Posix -> Posix -> Int
secondsUntilRetry retryAfter currentTime =
    ceiling <| toFloat (Time.posixToMillis retryAfter - Time.posixToMillis currentTime) / 1000


manualRetryRateLimit : Posix -> Posix -> String
manualRetryRateLimit retryAfter currentTime =
    let
        secondsLeft =
            String.fromInt (secondsUntilRetry retryAfter currentTime)
    in
    "You have been rate limited! Try again in " ++ secondsLeft ++ " seconds."


autoRetryRateLimit : Posix -> Posix -> String
autoRetryRateLimit retryAfter currentTime =
    let
        secondsLeft =
            String.fromInt (secondsUntilRetry retryAfter currentTime)
    in
    "You have been rate limited! Trying again in " ++ secondsLeft ++ " seconds."



-- Model


type alias Model =
    { userId : UserUUID
    , userApiKey : UserApiKey
    , currentTime : Posix
    , rateLimitRetryAfter : Posix
    , session : Session
    }


type alias UUIDStr =
    String


type alias GroupName =
    String


type alias GroupNameDict =
    Dict UUIDStr (Result String GroupName)


type alias LoggedInModel =
    { webhooks : WebhookStatus
    , editor : Maybe Editor
    , confirm : Maybe Confirmation
    , requestError : Maybe WebhookListError
    , partyId : Maybe GroupUUID
    , groupNames : GroupNameDict
    , delayedRequest : Maybe (Cmd Msg)
    }


type WebhookListError
    = ErrorSeen String
    | ErrorUnseen String


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
    , currentTime = Time.millisToPosix 1
    , rateLimitRetryAfter = Time.millisToPosix 0
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
    = Saving SaveAction
    | Reloading
    | FailedLoading String
    | Ready (List Webhook)


type SaveAction
    = SaveDelete
    | SaveCreate
    | SaveUpdate


type LoginFailureMessage
    = OneTimeLoginFailure String
    | RateLimitLoginFailure


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
    | QuestActivity QuestActivityOptions


type alias TaskActivityOptions =
    { created : Bool
    , updated : Bool
    , deleted : Bool
    , scored : Bool
    , checklistScored : Bool
    }


type alias GroupChatReceivedOptions =
    { groupId : Result String GroupUUID }


type alias UserActivityOptions =
    { petHatched : Bool
    , mountRaised : Bool
    , leveledUp : Bool
    }


type alias QuestActivityOptions =
    { questStarted : Bool
    , questFinished : Bool
    , questInvited : Bool
    }


type alias Webhook =
    { id : Maybe WebhookUUID
    , url : Result String Url
    , label : String
    , enabled : Bool
    , type_ : Type
    }


type EditType
    = EditTaskActivity
    | EditGroupChatReceived
    | EditUserActivity
    | EditQuestActivity


type EditableTaskActivityOptions
    = EditableTaskActivityOptions TaskActivityOptions


type EditableGroupChatReceivedOptions
    = EditableGroupChatReceivedOptions { groupId : String }


type EditableUserActivityOptions
    = EditableUserActivityOptions UserActivityOptions


type EditableQuestActivityOptions
    = EditableQuestActivityOptions QuestActivityOptions


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


emptyEditableQuestActivity : EditableQuestActivityOptions
emptyEditableQuestActivity =
    EditableQuestActivityOptions
        { questStarted = False
        , questFinished = False
        , questInvited = False
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
    , questActivityOptions : EditableQuestActivityOptions
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
    , questActivityOptions = emptyEditableQuestActivity
    }


type FailureResponse a
    = AnyFailure a
    | RateLimitExceededFailure Posix


type Msg
    = UpdateUserId UserUUID
    | UpdateUserApiKey UserApiKey
    | Login
    | ReloadWebhooks
    | ReceiveLoginResult (Result (FailureResponse String) Session)
    | ReloadedWebhooks (Result (FailureResponse String) (List Webhook))
    | SavedWebhook (Result (FailureResponse String) ())
    | DeletedWebhook (Result (FailureResponse String) ())
    | GotGroupName (Result String GroupUUID) (Result (FailureResponse String) (Result String GroupName))
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
    | EditorSetOptQuestStarted Bool
    | EditorSetOptQuestFinished Bool
    | EditorSetOptQuestInvited Bool
    | EditorSubmit
    | EditorCancel
    | SetCurrentTime Posix


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
                    requestUserLogin model.userId model.userApiKey
            in
            ( { model | session = AttemptingLogin }, loginCmd )

        ReceiveLoginResult result ->
            case result of
                Err err ->
                    case err of
                        RateLimitExceededFailure retryAfter ->
                            ( setRetryAfterTime retryAfter
                                { model
                                    | session =
                                        LoginFailure RateLimitLoginFailure
                                }
                            , Cmd.none
                            )

                        AnyFailure e ->
                            ( { model
                                | session =
                                    LoginFailure <| OneTimeLoginFailure e
                              }
                            , Cmd.none
                            )

                Ok session ->
                    ( { model | session = session }
                    , case session of
                        LoggedIn loggedInModel ->
                            case loggedInModel.webhooks of
                                Ready webhooks ->
                                    fetchGroupNames model.userId model.userApiKey webhooks loggedInModel.groupNames

                                _ ->
                                    Cmd.none

                        _ ->
                            Cmd.none
                    )

        ReloadWebhooks ->
            let
                cmd =
                    reloadWebhooks model.userId model.userApiKey
            in
            withLoggedInModel
                (\loggedInModel ->
                    ( { loggedInModel
                        | webhooks = Reloading
                        , delayedRequest = Just cmd
                      }
                    , cmd
                    )
                )
                model

        ReloadedWebhooks result ->
            case result of
                Err (RateLimitExceededFailure timeRemaining) ->
                    ( setRetryAfterTime timeRemaining model, Cmd.none )

                Err (AnyFailure err) ->
                    withLoggedInModel
                        (\loggedInModel ->
                            ( { loggedInModel
                                | webhooks = FailedLoading err
                                , delayedRequest = Nothing
                              }
                            , Cmd.none
                            )
                        )
                        model

                Ok webhooks ->
                    withLoggedInModel
                        (\loggedInModel ->
                            ( { loggedInModel
                                | webhooks = Ready webhooks
                                , delayedRequest = Nothing
                                , requestError =
                                    case loggedInModel.requestError of
                                        Nothing ->
                                            Nothing

                                        Just (ErrorSeen _) ->
                                            Nothing

                                        Just (ErrorUnseen txt) ->
                                            Just (ErrorSeen txt)
                              }
                            , fetchGroupNames model.userId model.userApiKey webhooks loggedInModel.groupNames
                            )
                        )
                        model

        SavedWebhook result ->
            let
                reloadCmd =
                    reloadWebhooks model.userId model.userApiKey
            in
            case result of
                Err (RateLimitExceededFailure timeRemaining) ->
                    ( setRetryAfterTime timeRemaining model, Cmd.none )

                Err (AnyFailure err) ->
                    withLoggedInModel
                        (\loggedInModel ->
                            -- TODO: It would make sense to do this before leaving the editor
                            --       so the user doesn't lose their form data.
                            ( { loggedInModel
                                | requestError =
                                    Just <|
                                        ErrorUnseen
                                            "There was an error saving your webhook. Please try again later."
                                , delayedRequest = Just reloadCmd
                              }
                            , reloadCmd
                            )
                        )
                        model

                Ok webhooks ->
                    withLoggedInModel
                        (\loggedInModel ->
                            ( { loggedInModel
                                | requestError = Nothing
                                , webhooks = Reloading
                                , delayedRequest = Just reloadCmd
                              }
                            , reloadCmd
                            )
                        )
                        model

        DeletedWebhook result ->
            let
                reloadCmd =
                    reloadWebhooks model.userId model.userApiKey
            in
            case result of
                Err (RateLimitExceededFailure timeRemaining) ->
                    ( setRetryAfterTime timeRemaining model, Cmd.none )

                Err (AnyFailure err) ->
                    withLoggedInModel
                        (\loggedInModel ->
                            ( { loggedInModel
                                | requestError =
                                    Just <|
                                        ErrorUnseen
                                            "There was an error deleting your webhook. Please try again later."
                                , delayedRequest = Just reloadCmd
                              }
                            , reloadCmd
                            )
                        )
                        model

                Ok webhooks ->
                    withLoggedInModel
                        (\loggedInModel ->
                            ( { loggedInModel
                                | requestError = Nothing
                                , webhooks = Reloading
                                , delayedRequest = Just reloadCmd
                              }
                            , reloadCmd
                            )
                        )
                        model

        GotGroupName groupId result_ ->
            let
                groupUuidStr =
                    unwrapGroupId groupId

                result =
                    case result_ of
                        Err (RateLimitExceededFailure _) ->
                            Err "Unable to fetch group name. Rate limit exceeded."

                        Err (AnyFailure errMsg) ->
                            Err errMsg

                        Ok val ->
                            val

                newModel =
                    mapLoggedInModel
                        (\loggedInModel ->
                            { loggedInModel
                                | groupNames = Dict.insert groupUuidStr result loggedInModel.groupNames
                            }
                        )
                        model
            in
            ( newModel, Cmd.none )

        Edit maybeWebhook ->
            let
                newModel =
                    mapLoggedInModel
                        (\loggedInModel ->
                            case maybeWebhook of
                                Nothing ->
                                    { loggedInModel | editor = Just ( defaultWebhookEditForm, [] ) }

                                Just webhook ->
                                    let
                                        webhookUuid =
                                            Maybe.map (\(WebhookUUID uuid) -> Uuid.toString uuid) webhook.id

                                        hookOpts =
                                            typeToEditable webhook.type_
                                    in
                                    { loggedInModel
                                        | editor =
                                            Just <|
                                                ( { id = webhookUuid
                                                  , url = unwrapUrl webhook.url
                                                  , enabled = webhook.enabled
                                                  , label = webhook.label
                                                  , type_ = hookOpts.editType
                                                  , taskActivityOptions = hookOpts.opts.taskActivityOptions
                                                  , groupChatReceivedOptions = hookOpts.opts.groupChatReceivedOptions
                                                  , userActivityOptions = hookOpts.opts.userActivityOptions
                                                  , questActivityOptions = hookOpts.opts.questActivityOptions
                                                  }
                                                , []
                                                )
                                    }
                        )
                        model
            in
            ( newModel, Cmd.none )

        Delete uuid ->
            let
                deleteCmd =
                    deleteWebhook model.userId model.userApiKey uuid
            in
            withLoggedInModel
                (\loggedInModel ->
                    ( { loggedInModel
                        | webhooks = Saving SaveDelete
                        , confirm = Nothing
                        , delayedRequest = Just deleteCmd
                      }
                    , deleteCmd
                    )
                )
                model

        ConfirmDelete webhook ->
            ( mapLoggedInModel
                (\loggedInModel ->
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
                        , requestError =
                            case loggedInModel.requestError of
                                Nothing ->
                                    Nothing

                                Just (ErrorSeen _) ->
                                    Nothing

                                Just (ErrorUnseen txt) ->
                                    Just (ErrorSeen txt)
                    }
                )
                model
            , Cmd.none
            )

        ConfirmCancel ->
            ( mapLoggedInModel
                (\m -> { m | confirm = Nothing })
                model
            , Cmd.none
            )

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

        EditorSetOptQuestStarted questStarted ->
            ( mapQuestActivityOptions (\opts -> { opts | questStarted = questStarted }) model, Cmd.none )

        EditorSetOptQuestFinished questFinished ->
            ( mapQuestActivityOptions (\opts -> { opts | questFinished = questFinished }) model, Cmd.none )

        EditorSetOptQuestInvited questInvited ->
            ( mapQuestActivityOptions (\opts -> { opts | questInvited = questInvited }) model, Cmd.none )

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
                                saveCmd =
                                    saveWebhook model.userId model.userApiKey webhook
                            in
                            withLoggedInModel
                                (\m ->
                                    ( { m
                                        | webhooks =
                                            case webhook.id of
                                                Nothing ->
                                                    Saving SaveCreate

                                                _ ->
                                                    Saving SaveUpdate
                                        , editor = Nothing
                                        , confirm = Nothing
                                        , delayedRequest = Just saveCmd
                                        , requestError = Nothing
                                      }
                                    , saveCmd
                                    )
                                )
                                model

        EditorCancel ->
            ( mapLoggedInModel
                (\m ->
                    { m
                        | editor = Nothing
                        , requestError =
                            case m.requestError of
                                Nothing ->
                                    Nothing

                                Just (ErrorSeen _) ->
                                    Nothing

                                Just (ErrorUnseen txt) ->
                                    Just (ErrorSeen txt)
                    }
                )
                model
            , Cmd.none
            )

        SetCurrentTime time ->
            let
                newModel =
                    { model | currentTime = time }

                delayedRequest =
                    case model.session of
                        LoggedIn loggedInModel ->
                            loggedInModel.delayedRequest

                        _ ->
                            Nothing
            in
            if Time.posixToMillis newModel.currentTime > Time.posixToMillis model.rateLimitRetryAfter then
                case delayedRequest of
                    Nothing ->
                        ( newModel, Cmd.none )

                    Just req ->
                        ( newModel, req )

            else
                ( newModel, Cmd.none )


fetchGroupNames : UserUUID -> UserApiKey -> List Webhook -> GroupNameDict -> Cmd Msg
fetchGroupNames userId apiKey webhooks namesById =
    webhooks
        |> List.filterMap
            (\webhook ->
                case webhook.type_ of
                    GroupChatReceived opts ->
                        Just opts.groupId

                    _ ->
                        Nothing
            )
        |> List.foldl
            (\groupId (( cmds, seenDict ) as acc) ->
                let
                    groupIdStr =
                        unwrapGroupId groupId
                in
                if Dict.member groupIdStr seenDict then
                    acc

                else
                    ( case groupId of
                        Ok validGroupUuid ->
                            getGroupName userId apiKey validGroupUuid :: cmds

                        Err _ ->
                            (Task.succeed
                                (GotGroupName groupId (Err <| AnyFailure "Invalid group UUID. Unable to fetch group."))
                                |> Task.perform identity
                            )
                                :: cmds
                    , Dict.insert groupIdStr (Ok "") seenDict
                    )
            )
            ( [], namesById )
        |> Tuple.first
        |> Cmd.batch


editorToWebhook : WebhookEditForm -> Result (List ValidationError) Webhook
editorToWebhook form =
    let
        validatedUrl =
            form.url
                |> Url.fromString
                |> Maybe.map (Ok << Ok)
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
                        |> Maybe.map (Ok << Ok << GroupUUID)
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

        EditQuestActivity ->
            let
                (EditableQuestActivityOptions opts) =
                    form.questActivityOptions
            in
            Ok
                (\url hookId ->
                    { url = url
                    , id = hookId
                    , label = form.label
                    , enabled = form.enabled
                    , type_ = QuestActivity opts
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
    , questActivityOptions : EditableQuestActivityOptions
    }


typeToEditable : Type -> { editType : EditType, opts : EditorOpts }
typeToEditable type_ =
    let
        defOpts =
            { taskActivityOptions = emptyEditableTaskActivity
            , groupChatReceivedOptions = emptyEditableGroupChatReceived
            , userActivityOptions = emptyEditableUserActivity
            , questActivityOptions = emptyEditableQuestActivity
            }
    in
    case type_ of
        TaskActivity opts ->
            { editType = EditTaskActivity
            , opts =
                { defOpts | taskActivityOptions = EditableTaskActivityOptions opts }
            }

        GroupChatReceived opts ->
            let
                idUnwrapped =
                    unwrapGroupId opts.groupId
            in
            { editType = EditGroupChatReceived
            , opts =
                { defOpts
                    | groupChatReceivedOptions =
                        EditableGroupChatReceivedOptions { groupId = idUnwrapped }
                }
            }

        UserActivity opts ->
            { editType = EditUserActivity
            , opts =
                { defOpts | userActivityOptions = EditableUserActivityOptions opts }
            }

        QuestActivity opts ->
            { editType = EditQuestActivity
            , opts =
                { defOpts | questActivityOptions = EditableQuestActivityOptions opts }
            }


withLoggedInModel : (LoggedInModel -> ( LoggedInModel, Cmd msg )) -> Model -> ( Model, Cmd msg )
withLoggedInModel f model =
    case model.session of
        LoggedIn loggedInModel ->
            let
                ( newLoggedInModel, cmds ) =
                    f loggedInModel
            in
            ( { model | session = LoggedIn newLoggedInModel }, cmds )

        _ ->
            ( model, Cmd.none )


mapLoggedInModel : (LoggedInModel -> LoggedInModel) -> Model -> Model
mapLoggedInModel f model =
    withLoggedInModel (\m -> ( f m, Cmd.none )) model |> Tuple.first


mapEditor : (Editor -> Editor) -> Model -> Model
mapEditor f =
    mapLoggedInModel <|
        \loggedInModel ->
            case loggedInModel.editor of
                Nothing ->
                    loggedInModel

                Just editForm ->
                    { loggedInModel
                        | editor = Just (f editForm)
                    }


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


mapQuestActivityOptions : (QuestActivityOptions -> QuestActivityOptions) -> Model -> Model
mapQuestActivityOptions f =
    mapEditorFormFields
        (\form ->
            let
                (EditableQuestActivityOptions opts) =
                    form.questActivityOptions
            in
            { form | questActivityOptions = EditableQuestActivityOptions <| f opts }
        )


theme =
    { text =
        { normal = Element.rgb 1 1 1
        , warning = Element.rgb255 255 255 0
        , error = Element.rgb255 180 0 0
        , faded = Element.rgb255 125 125 125
        , link = Element.rgb255 180 0 0
        , linkGlow = Element.rgb 1 1 1
        , enabled = Element.rgb 1 1 1
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
        , widgetError = Element.rgb255 25 10 10
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
                webhookDashboard model loggedInModel

            _ ->
                loginPage model


contentColumn : List (Element Msg) -> Element Msg
contentColumn body =
    Element.column
        [ Element.centerX
        , Element.centerY
        , Element.spacing 10
        , Element.paddingXY 0 15
        , Element.height Element.fill
        ]
        [ Element.column
            [ Element.centerX
            , Element.centerY
            , Element.spacing 10
            , Element.paddingXY 0 15
            ]
            (heading :: body)
        , footer
        ]


loginPage : Model -> Element Msg
loginPage model =
    let
        statusMsg =
            case model.session of
                AttemptingLogin ->
                    Element.text "Logging in..."

                LoginFailure (OneTimeLoginFailure msg) ->
                    Element.el
                        [ Font.color theme.text.error
                        , Element.centerX
                        ]
                        (Element.text msg)

                LoginFailure RateLimitLoginFailure ->
                    if Time.posixToMillis model.currentTime > Time.posixToMillis model.rateLimitRetryAfter then
                        Element.none

                    else
                        Element.el
                            [ Font.color theme.text.error
                            , Element.centerX
                            ]
                            (Element.text <| manualRetryRateLimit model.rateLimitRetryAfter model.currentTime)

                _ ->
                    Element.none

        (UserUUID userId) =
            model.userId

        (UserApiKey apiKey) =
            model.userApiKey
    in
    contentColumn
        [ textInput Input.username
            [ Element.htmlAttribute (A.name "username") ]
            { onChange = UpdateUserId << UserUUID
            , text = userId
            , placeholder = inputPlaceholder "User ID"
            , label = Input.labelHidden "User ID"
            }
        , textInput Input.currentPassword
            [ Element.htmlAttribute (A.name "password") ]
            { onChange = UpdateUserApiKey << UserApiKey
            , text = apiKey
            , placeholder = inputPlaceholder "API Key"
            , label = Input.labelHidden "API Key"
            , show = False
            }
        , let
            disabled =
                Time.posixToMillis model.rateLimitRetryAfter >= Time.posixToMillis model.currentTime
          in
          yesButton
            ([ Element.width Element.fill ]
                ++ (if disabled then
                        [ Border.dashed
                        , Border.color theme.misc.widget
                        , Border.width 1
                        , Font.color theme.text.faded
                        , Font.italic
                        , BG.color theme.page.background
                        , Element.mouseOver []
                        ]

                    else
                        []
                   )
            )
            (if disabled then
                Nothing

             else
                Just Login
            )
            "Login"
        , Element.el
            [ Element.width Element.fill
            , Font.size 16
            , Font.center
            ]
            statusMsg
        ]


workingOn : Model -> String -> List (Element Msg)
workingOn model job =
    [ Element.el
        [ Element.centerX
        , Element.centerY
        ]
        (Element.text job)
    ]
        ++ (if Time.posixToMillis model.rateLimitRetryAfter >= Time.posixToMillis model.currentTime then
                [ Element.el
                    [ Font.color theme.text.error
                    , Element.padding 10
                    , Font.size 18
                    , Element.centerX
                    , Element.centerY
                    ]
                    (Element.text <| autoRetryRateLimit model.rateLimitRetryAfter model.currentTime)
                ]

            else
                []
           )


confirmation : Confirmation -> List (Element Msg)
confirmation confirm =
    [ Element.text confirm.text
    , Element.row
        [ Element.alignRight
        , Element.width Element.fill
        , Element.spacing 10
        ]
        [ yesButton [ Element.width Element.fill ] (Just confirm.action) "Yes"
        , noButton [ Element.width Element.fill ] (Just ConfirmCancel) "Cancel"
        ]
    ]


webhookList : GroupNameDict -> List Webhook -> List (Element Msg)
webhookList groupNames webhooks =
    [ Element.row
        [ Element.centerX
        , Element.spacing 10
        ]
        [ yesButton [] (Just (Edit Nothing)) "Create webhook"
        , noButton [] (Just ReloadWebhooks) "Refresh"
        ]
    ]
        ++ List.map (webhookView groupNames) webhooks


webhookView : GroupNameDict -> Webhook -> Element Msg
webhookView groupNames webhook =
    let
        labelTxt =
            if webhook.label == "" then
                "Unlabeled Webhook"

            else
                webhook.label

        idTxt =
            case webhook.id of
                Nothing ->
                    "No ID (if you see this, something went wrong)"

                Just (WebhookUUID uuid) ->
                    Uuid.toString uuid

        listToSentence : Nonempty String -> List (Element Msg)
        listToSentence opts =
            let
                italic =
                    Element.el [ Font.italic ] << Element.text
            in
            case Nonempty.length opts of
                1 ->
                    [ italic (Nonempty.head opts) ]

                2 ->
                    [ italic (Nonempty.get 0 opts), Element.text " or ", italic (Nonempty.get 1 opts) ]

                _ ->
                    let
                        reversed =
                            Nonempty.reverse opts |> Nonempty.map (\t -> [ italic t ])

                        last =
                            Nonempty.head reversed

                        withEndingOr =
                            Nonempty.replaceHead (Element.text "or " :: last) reversed
                    in
                    Nonempty.toList withEndingOr
                        |> List.reverse
                        |> List.intersperse [ Element.text ", " ]
                        |> List.concat

        optsToSentence : List ( Bool, String ) -> Maybe (List (Element Msg))
        optsToSentence =
            List.filter Tuple.first
                >> List.map Tuple.second
                >> Nonempty.fromList
                >> Maybe.andThen (listToSentence >> Just)

        highlightStyle =
            [ BG.color theme.page.background
            , Element.paddingXY 5 10
            ]

        highlightBox =
            Element.el highlightStyle

        webhookUrlLink =
            linkButton [] Nothing (unwrapUrl webhook.url)

        optUI optPairs optView =
            let
                maybeOptPhrase =
                    optsToSentence optPairs
            in
            (\body_ -> Tuple.pair body_ (Ok ())) <|
                case maybeOptPhrase of
                    Nothing ->
                        [ Element.text "This webhook will never fire."
                        , highlightBox webhookUrlLink
                        ]

                    Just phrase ->
                        optView phrase

        ( body, hookTypeStatus ) =
            case webhook.type_ of
                TaskActivity opts ->
                    optUI
                        [ Tuple.pair opts.created "created"
                        , Tuple.pair opts.updated "updated"
                        , Tuple.pair opts.deleted "deleted"
                        , Tuple.pair opts.scored "scored"
                        , Tuple.pair opts.checklistScored "a task's checklist item is scored"
                        ]
                    <|
                        \phrase ->
                            [ Element.text "This webhook will send an event to"
                            , highlightBox webhookUrlLink
                            , Element.wrappedRow []
                                [ Element.text "when "
                                , Element.paragraph highlightStyle <|
                                    [ Element.text "a task is " ]
                                        ++ phrase
                                        ++ [ Element.text "." ]
                                ]
                            ]

                GroupChatReceived opts ->
                    let
                        uuid =
                            unwrapGroupId opts.groupId

                        ( groupText, maybeErr ) =
                            Dict.get uuid groupNames
                                |> Maybe.andThen
                                    (\groupNameResult ->
                                        case groupNameResult of
                                            Ok name ->
                                                Just <|
                                                    Tuple.pair
                                                        (Element.wrappedRow []
                                                            [ Element.text "when "
                                                            , Element.paragraph highlightStyle
                                                                [ Element.text "a message is sent to "
                                                                , Element.el [ Font.italic ] (Element.text name)
                                                                , Element.text "."
                                                                ]
                                                            ]
                                                        )
                                                        (Ok ())

                                            Err err ->
                                                Just <|
                                                    Tuple.pair
                                                        (Element.wrappedRow []
                                                            [ Element.text "when "
                                                            , Element.paragraph highlightStyle
                                                                [ Element.text "a message is sent to the group with ID "
                                                                , Element.el [ Font.italic ] (Element.text uuid)
                                                                , Element.text "."
                                                                ]
                                                            ]
                                                        )
                                                        (Err err)
                                    )
                                |> Maybe.withDefault
                                    (Tuple.pair
                                        (Element.paragraph highlightStyle
                                            [ Element.text "a message is sent to "
                                            , Element.el [ Font.italic ] (Element.text "(...loading group name...)")
                                            , Element.text "."
                                            ]
                                        )
                                        (Ok ())
                                    )
                    in
                    ( [ Element.text "This webhook will send an event to"
                      , highlightBox webhookUrlLink
                      , groupText
                      ]
                    , maybeErr
                    )

                UserActivity opts ->
                    optUI
                        [ Tuple.pair opts.mountRaised "raises a mount"
                        , Tuple.pair opts.petHatched "hatches a pet"
                        , Tuple.pair opts.leveledUp "levels up"
                        ]
                    <|
                        \phrase ->
                            [ Element.text "This webhook will send an event to"
                            , highlightBox webhookUrlLink
                            , Element.wrappedRow []
                                [ Element.text "when "
                                , Element.paragraph highlightStyle <|
                                    [ Element.text "the user " ]
                                        ++ phrase
                                        ++ [ Element.text "." ]
                                ]
                            ]

                QuestActivity opts ->
                    optUI
                        [ Tuple.pair opts.questStarted "a quest is started"
                        , Tuple.pair opts.questFinished "a quest is finished"
                        , Tuple.pair opts.questInvited "the user is invited to a quest"
                        ]
                    <|
                        \phrase ->
                            [ Element.text "This webhook will send an event to"
                            , highlightBox webhookUrlLink
                            , Element.wrappedRow []
                                [ Element.text "when "
                                , Element.paragraph highlightStyle <|
                                    phrase
                                        ++ [ Element.text "." ]
                                ]
                            ]

        webhookErrors =
            [ webhook.url
                |> Result.map (always ())
                |> Result.mapError (always "Invalid URL.")
            , hookTypeStatus
                |> Result.map (always ())
            ]
                |> List.filterMap
                    (\possibleError ->
                        case possibleError of
                            Err msg ->
                                Just msg

                            Ok _ ->
                                Nothing
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
                    , Font.color theme.text.faded
                    , Font.italic
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
            ++ (if List.isEmpty webhookErrors then
                    []

                else
                    [ Border.dashed
                    , Border.color theme.text.error
                    , Border.width 1
                    , BG.color theme.misc.widgetError
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
                [ Element.paragraph []
                    [ h2_ <|
                        Element.paragraph []
                            [ if List.isEmpty webhookErrors then
                                Element.none

                              else
                                errorIcon
                            , Element.text labelTxt
                            ]
                    ]
                , Element.row
                    [ Element.alignRight
                    , Element.alignTop
                    , Element.spacing 10
                    ]
                    [ linkButton [] (Just (Edit <| Just webhook)) "Edit"
                    , linkButton [] (Just (ConfirmDelete webhook)) "Delete"
                    ]
                ]
            ]
        , Element.column
            [ Font.family [ Font.typeface "Calibri", Font.sansSerif ]
            , Font.hairline
            , Font.size 18
            , Element.spacing 6
            , Element.width Element.fill
            ]
          <|
            [ if List.isEmpty webhookErrors then
                Element.none

              else
                Element.textColumn
                    ([ Element.width Element.fill ]
                        ++ highlightStyle
                        ++ [ Element.padding 20 ]
                    )
                    ([ Element.paragraph
                        [ Font.color theme.text.warning
                        , Font.size 22
                        , Element.paddingEach { top = 0, bottom = 15, right = 0, left = 0 }
                        ]
                        [ Element.text "There are errors with your webhook, which may cause it not to fire:" ]
                     ]
                        ++ List.map
                            (\err ->
                                Element.row
                                    [ Font.family [ Font.typeface "Courier New", Font.monospace ]
                                    , Element.paddingEach { top = 0, bottom = 0, right = 0, left = 30 }
                                    ]
                                    [ Element.el [ Font.heavy ] (Element.text "(!) ")
                                    , Element.paragraph [] [ Element.text err ]
                                    ]
                            )
                            webhookErrors
                    )
            ]
                ++ body
        , Element.el
            [ Font.size 14
            , Font.color theme.text.faded
            , Element.width Element.fill
            , Font.alignRight
            ]
            (Element.text idTxt)
        ]


iconElement : FA.Icon -> Element Msg
iconElement =
    Element.html << FA.viewIcon


errorIcon : Element Msg
errorIcon =
    Element.row
        [ Font.color theme.text.error
        , Element.paddingEach { top = 0, bottom = 0, left = 0, right = 10 }
        ]
        [ iconElement FA.exclamationCircle ]


editorFieldRow : String -> (String -> List (Element.Attribute Msg) -> Element Msg) -> Element Msg
editorFieldRow label fieldFn =
    Element.row
        [ Element.width Element.fill ]
        [ Element.el [ Element.width (Element.fillPortion 3) ]
            (Element.text label)
        , fieldFn label
            [ Element.width (Element.fillPortion 6) ]
        ]


hookTypeOption : String -> Input.OptionState -> Element Msg
hookTypeOption hookType optState =
    Element.el
        ([ Element.paddingXY 15 8
         ]
            ++ (if optState == Input.Selected then
                    [ Font.color theme.text.enabled
                    ]

                else
                    [ Font.color theme.text.faded
                    , BG.color theme.page.background
                    ]
               )
        )
        (Element.text hookType)


webhookTypeEditor : Maybe GroupUUID -> WebhookEditForm -> Element Msg
webhookTypeEditor partyId fields =
    Element.column
        [ Element.spacing 4
        , Element.width Element.fill
        ]
        [ Element.el
            [ Font.size 22 ]
            (Element.text "Webhook Type")
        , Element.column
            [ BG.color theme.misc.widget
            , Border.rounded 3
            , Element.clip
            , Element.spacing 10
            ]
            [ Input.radioRow
                []
                { onChange = EditorSetType
                , options =
                    [ Input.optionWith EditTaskActivity (hookTypeOption "taskActivity")
                    , Input.optionWith EditGroupChatReceived (hookTypeOption "groupChatReceived")
                    , Input.optionWith EditUserActivity (hookTypeOption "userActivity")
                    , Input.optionWith EditQuestActivity (hookTypeOption "questActivity")
                    ]
                , selected = Just fields.type_
                , label = Input.labelHidden "Webhook Type"
                }
            , Element.column
                [ Element.paddingXY 15 8
                , Element.width Element.fill
                , Font.size 18
                ]
              <|
                case fields.type_ of
                    EditTaskActivity ->
                        let
                            (EditableTaskActivityOptions opts) =
                                fields.taskActivityOptions
                        in
                        [ checkboxFieldRow EditorSetOptCreated opts.created "created"
                        , checkboxFieldRow EditorSetOptUpdated opts.updated "updated"
                        , checkboxFieldRow EditorSetOptDeleted opts.deleted "deleted"
                        , checkboxFieldRow EditorSetOptScored opts.scored "scored"
                        , checkboxFieldRow EditorSetOptChecklistScored opts.checklistScored "checklistScored"
                        ]

                    EditGroupChatReceived ->
                        let
                            (EditableGroupChatReceivedOptions opts) =
                                fields.groupChatReceivedOptions
                        in
                        [ editorFieldRow "Group ID" <|
                            \label extraAttrs ->
                                textInput Input.text
                                    extraAttrs
                                    { onChange = EditorSetOptGroupId
                                    , text = opts.groupId
                                    , placeholder = Nothing
                                    , label = Input.labelHidden label
                                    }
                        , case partyId of
                            Nothing ->
                                Element.none

                            Just (GroupUUID pId) ->
                                linkButton
                                    [ Element.alignRight
                                    , Font.size 12
                                    , Element.paddingEach { top = 10, right = 0, bottom = 0, left = 0 }
                                    ]
                                    (Just <| EditorSetOptGroupId (Uuid.toString pId))
                                    "Use Party ID"
                        ]

                    EditUserActivity ->
                        let
                            (EditableUserActivityOptions opts) =
                                fields.userActivityOptions
                        in
                        [ checkboxFieldRow EditorSetOptMountRaised opts.mountRaised "mountRaised"
                        , checkboxFieldRow EditorSetOptPetHatched opts.petHatched "petHatched"
                        , checkboxFieldRow EditorSetOptLeveledUp opts.leveledUp "leveledUp"
                        ]

                    EditQuestActivity ->
                        let
                            (EditableQuestActivityOptions opts) =
                                fields.questActivityOptions
                        in
                        [ checkboxFieldRow EditorSetOptQuestStarted opts.questStarted "questStarted"
                        , checkboxFieldRow EditorSetOptQuestFinished opts.questFinished "questFinished"
                        , checkboxFieldRow EditorSetOptQuestInvited opts.questInvited "questInvited"
                        ]
            ]
        ]


webhookEditor : Maybe GroupUUID -> Editor -> List (Element Msg)
webhookEditor partyId editor =
    let
        ( fields, errors ) =
            editor
    in
    [ Element.column [ Element.spacing 10 ] <|
        List.map
            (\err ->
                Element.paragraph
                    [ Font.color theme.text.error ]
                    [ errorIcon
                    , Element.text err
                    ]
            )
            errors
    , Element.column
        [ Element.spacing 20 ]
        [ Element.column
            [ Element.width Element.fill
            , Element.spacing 10
            ]
            [ checkboxFieldRow EditorSetEnabled fields.enabled "Enabled"
            , editorFieldRow "Label" <|
                \label extraAttrs ->
                    textInput Input.text
                        extraAttrs
                        { onChange = EditorSetLabel
                        , text = fields.label
                        , placeholder = Nothing
                        , label = Input.labelHidden label
                        }
            , editorFieldRow "URL" <|
                \label extraAttrs ->
                    textInput Input.text
                        extraAttrs
                        { onChange = EditorSetUrl
                        , text = fields.url
                        , placeholder = Nothing
                        , label = Input.labelHidden label
                        }
            ]
        , webhookTypeEditor partyId fields
        , Element.row
            [ Element.spacing 10
            , Element.alignRight
            ]
            [ yesButton [] (Just EditorSubmit) "Submit"
            , noButton [] (Just EditorCancel) "Cancel"
            ]
        ]
    ]


webhookDashboard : Model -> LoggedInModel -> Element Msg
webhookDashboard model loggedInModel =
    contentColumn <|
        case loggedInModel.webhooks of
            Reloading ->
                workingOn model "Fetching updated webhooks."

            Saving SaveDelete ->
                workingOn model "Deleting webhook."

            Saving SaveCreate ->
                workingOn model "Creating webhook."

            Saving SaveUpdate ->
                workingOn model "Updating webhook."

            FailedLoading err ->
                workingOn model "Failed to load webhooks."

            Ready webhooks ->
                case ( loggedInModel.confirm, loggedInModel.editor ) of
                    ( Just confirm, _ ) ->
                        confirmation confirm

                    ( Nothing, Nothing ) ->
                        let
                            showError err =
                                case err of
                                    ErrorSeen txt ->
                                        txt

                                    ErrorUnseen txt ->
                                        txt

                            errorBox =
                                case loggedInModel.requestError of
                                    Nothing ->
                                        Element.none

                                    Just err ->
                                        Element.el
                                            [ Font.color theme.text.error
                                            , Element.padding 10
                                            , Font.size 18
                                            , Element.centerX
                                            ]
                                            (Element.text <| showError err)
                        in
                        errorBox :: webhookList loggedInModel.groupNames webhooks

                    ( Nothing, Just editor ) ->
                        webhookEditor loggedInModel.partyId editor


heading : Element Msg
heading =
    Element.column
        [ Font.center
        , Element.centerX
        ]
        [ h1_ (Element.text "Habitica Webhook Editor")
        , Element.el
            [ Font.size 14
            , Font.color theme.text.faded
            , Element.alignRight
            , Element.moveRight 15
            ]
            (Element.text "v1.2.0")
        ]


footer : Element Msg
footer =
    Element.paragraph
        [ Font.size 12
        , Font.color theme.text.faded
        , Font.center
        , Element.centerX
        , Element.width (Element.fill |> Element.maximum 400)
        , Element.alignBottom
        ]
        [ Element.text "Created by "
        , Element.newTabLink
            [ Font.color theme.text.link ]
            { url = "https://habitica.com/profile/cab16cfa-e951-4dc3-a468-1abadc1dd109"
            , label = Element.text "rhitakorrr"
            }
        , Element.text ". Bug reports and pull requests may be submitted on this project's "
        , Element.newTabLink
            [ Font.color theme.text.link ]
            { url = "https://github.com/robwhitaker/habitica-webhook-editor"
            , label = Element.text "GitHub page"
            }
        , Element.text "."
        ]



-- Reusable styled elements


checkboxFieldRow : (Bool -> Msg) -> Bool -> String -> Element Msg
checkboxFieldRow msg checked label =
    editorFieldRow label <|
        \label_ extraAttrs ->
            Input.checkbox
                ([ Font.size 32 ] ++ extraAttrs)
                { onChange = msg
                , icon =
                    \bool ->
                        if bool then
                            Element.text "☑"

                        else
                            Element.text "☐"
                , checked = checked
                , label = Input.labelHidden label_
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


h_ : Int -> List (Element.Attribute Msg) -> Element Msg -> Element Msg
h_ n extraAttrs =
    Element.el
        (extraAttrs ++ [ Region.heading n ])


h : Int -> List (Element.Attribute Msg) -> String -> Element Msg
h n extraAttrs txt =
    h_ n extraAttrs (Element.text txt)


h1_ : Element Msg -> Element Msg
h1_ =
    h_ 1
        [ Font.size 34
        , Font.family [ Font.typeface "Trebuchet MS" ]
        , Element.centerX
        ]


h2_ : Element Msg -> Element Msg
h2_ =
    h_ 2
        [ Font.size 28
        , Font.family [ Font.typeface "Trebuchet MS" ]
        ]


h2 : String -> Element Msg
h2 =
    h2_ << Element.text


type alias TextInputFn msg opts =
    List (Element.Attribute msg)
    -> opts
    -> Element msg


textInput : TextInputFn Msg opts -> TextInputFn Msg opts
textInput input extraAttrs opts =
    input
        ([ BG.color theme.input.background
         , Font.color theme.input.text
         , Border.width 0
         ]
            ++ extraAttrs
        )
        opts


inputPlaceholder : String -> Maybe (Input.Placeholder Msg)
inputPlaceholder txt =
    Just <|
        Input.placeholder
            [ Font.center
            , Font.color theme.input.placeholderText
            ]
            (Element.text txt)


setRetryAfterTime : Posix -> Model -> Model
setRetryAfterTime retryAfter model =
    { model
        | rateLimitRetryAfter =
            Time.millisToPosix <| Time.posixToMillis model.currentTime + Time.posixToMillis retryAfter
    }


customResponseHandler : Decoder a -> (Http.Response String -> Result (FailureResponse String) a)
customResponseHandler decoder =
    \response ->
        case response of
            Http.BadStatus_ metadata body ->
                case Dict.get "retry-after" metadata.headers of
                    Just retryAfter ->
                        case String.toFloat retryAfter of
                            Just time ->
                                -- The 2 here is a magic number. It's just a little time padding since most of the time
                                -- the rate limit won't be done exactly after the retryAfter time. Generally, it needs
                                -- another second or so. Nothing will break if the request is made a little early, but
                                -- who wants to see the timer hit zero and then go back up after trying again?
                                Err <| RateLimitExceededFailure (Time.millisToPosix <| round <| (2 + time) * 1000)

                            Nothing ->
                                Err <| AnyFailure "Something went wrong while trying to parse the rate limit header, Retry-After."

                    Nothing ->
                        case Decode.decodeString decoder body of
                            Ok value ->
                                Ok value

                            Err err ->
                                Err <| AnyFailure ("Something went wrong while parsing the response from the server. Error message was: " ++ Decode.errorToString err)

            Http.GoodStatus_ metadata body ->
                case Decode.decodeString decoder body of
                    Ok value ->
                        Ok value

                    Err err ->
                        Err <| AnyFailure ("Something went wrong while parsing the response from the server. Error message was: " ++ Decode.errorToString err)

            _ ->
                Err <| AnyFailure "Something went wrong. Please try again later."


customExpectJson : (Result (FailureResponse String) a -> msg) -> Decoder a -> Http.Expect msg
customExpectJson toMsg =
    Http.expectStringResponse toMsg << customResponseHandler


mkHeaders : UserUUID -> UserApiKey -> List Http.Header
mkHeaders (UserUUID uuid) (UserApiKey apiKey) =
    [ Http.header "x-api-user" uuid
    , Http.header "x-api-key" apiKey
    , Http.header "x-client" (maintainerId ++ "-" ++ appName)
    ]


requestUser : Http.Expect Msg -> UserUUID -> UserApiKey -> Cmd Msg
requestUser expect userId apiKey =
    Http.request
        { method = "GET"
        , headers = mkHeaders userId apiKey
        , url = "https://habitica.com/api/v3/user"
        , body = Http.emptyBody
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }


requestUserLogin : UserUUID -> UserApiKey -> Cmd Msg
requestUserLogin =
    requestUser (customExpectJson ReceiveLoginResult loginDecoder)


reloadWebhooks : UserUUID -> UserApiKey -> Cmd Msg
reloadWebhooks =
    requestUser (customExpectJson ReloadedWebhooks (Decode.map Tuple.first userResponseDecoder))


deleteWebhook : UserUUID -> UserApiKey -> WebhookUUID -> Cmd Msg
deleteWebhook userId apiKey (WebhookUUID uuid) =
    Http.request
        { method = "DELETE"
        , headers = mkHeaders userId apiKey
        , url = "https://habitica.com/api/v3/user/webhook/" ++ Uuid.toString uuid
        , body = Http.emptyBody
        , expect = customExpectJson DeletedWebhook (Decode.succeed ())
        , timeout = Nothing
        , tracker = Nothing
        }


getGroupName : UserUUID -> UserApiKey -> GroupUUID -> Cmd Msg
getGroupName userId apiKey ((GroupUUID uuid) as groupId) =
    Http.request
        { method = "GET"
        , headers = mkHeaders userId apiKey
        , url = "https://habitica.com/api/v3/groups/" ++ Uuid.toString uuid
        , body = Http.emptyBody
        , expect = customExpectJson (GotGroupName (Ok groupId)) groupNameDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


saveWebhook : UserUUID -> UserApiKey -> Webhook -> Cmd Msg
saveWebhook userId apiKey webhook =
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
    Http.request
        { method = method
        , headers = mkHeaders userId apiKey
        , url = endpoint
        , body = Http.jsonBody encodedWebhook
        , expect = customExpectJson SavedWebhook (Decode.succeed ())
        , timeout = Nothing
        , tracker = Nothing
        }


userResponseDecoder : Decoder ( List Webhook, Maybe GroupUUID )
userResponseDecoder =
    Decode.map2
        Tuple.pair
        (Decode.at [ "data", "webhooks" ] (Decode.list webhookDecoder))
        (Decode.maybe
            (Decode.at [ "data", "party", "_id" ] (Decode.map GroupUUID Uuid.decoder))
        )


habiticaResponseDecoder : Decoder a -> Decoder (Result String a)
habiticaResponseDecoder decoder =
    let
        handleResult success =
            if success then
                Decode.map Ok decoder

            else
                Decode.map Err (Decode.field "message" Decode.string)
    in
    Decode.field "success" Decode.bool
        |> Decode.andThen handleResult


loginDecoder : Decoder Session
loginDecoder =
    let
        handleLoginResult : Result String ( List Webhook, Maybe GroupUUID ) -> Decoder Session
        handleLoginResult res =
            case res of
                Ok ( webhook, maybePartyId ) ->
                    LoggedInModel (Ready webhook) Nothing Nothing Nothing maybePartyId Dict.empty Nothing
                        |> LoggedIn
                        |> Decode.succeed

                Err msg ->
                    Decode.succeed (LoginFailure <| OneTimeLoginFailure msg)
    in
    habiticaResponseDecoder userResponseDecoder
        |> Decode.andThen handleLoginResult


groupNameDecoder : Decoder (Result String GroupName)
groupNameDecoder =
    habiticaResponseDecoder (Decode.at [ "data", "name" ] Decode.string)


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
                (Decode.field "groupId" <|
                    Decode.oneOf
                        [ Decode.map (Ok << GroupUUID) Uuid.decoder
                        , Decode.map Err Decode.string
                        ]
                )

        userActivityOptionsDecoder : Decoder UserActivityOptions
        userActivityOptionsDecoder =
            Decode.map3 UserActivityOptions
                (Decode.field "petHatched" Decode.bool)
                (Decode.field "mountRaised" Decode.bool)
                (Decode.field "leveledUp" Decode.bool)

        questActivityOptionsDecoder : Decoder QuestActivityOptions
        questActivityOptionsDecoder =
            Decode.map3 QuestActivityOptions
                (Decode.field "questStarted" Decode.bool)
                (Decode.field "questFinished" Decode.bool)
                (Decode.field "questInvited" Decode.bool)

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

                        "questActivity" ->
                            Decode.map QuestActivity questActivityOptionsDecoder

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
        (Decode.field "url" Decode.string
            |> Decode.andThen
                (\urlStr ->
                    Decode.oneOf
                        [ Decode.map Ok (decodeUrl urlStr)
                        , Decode.succeed (Err urlStr)
                        ]
                )
        )
        (Decode.field "label" Decode.string)
        (Decode.field "enabled" Decode.bool)
        (Decode.field "type" Decode.string |> Decode.andThen decodeType)


webhookEncoder : Webhook -> Value
webhookEncoder webhook =
    Encode.object <|
        [ ( "url", Encode.string (unwrapUrl webhook.url) )
        , ( "label", Encode.string webhook.label )
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
                        [ ( "type", Encode.string "groupChatReceived" )
                        , ( "options"
                          , Encode.object
                                [ ( "groupId"
                                  , case opts.groupId of
                                        Ok (GroupUUID validUuid) ->
                                            Uuid.encode validUuid

                                        Err strUuid ->
                                            Encode.string strUuid
                                  )
                                ]
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

                    QuestActivity opts ->
                        [ ( "type", Encode.string "questActivity" )
                        , ( "options"
                          , Encode.object
                                [ ( "questStarted", Encode.bool opts.questStarted )
                                , ( "questFinished", Encode.bool opts.questFinished )
                                , ( "questInvited", Encode.bool opts.questInvited )
                                ]
                          )
                        ]
               )



-- HELPER FUNCTIONS


unwrapResult : (a -> c) -> (b -> c) -> Result a b -> c
unwrapResult f g res =
    case res of
        Err a ->
            f a

        Ok b ->
            g b


unwrapUrl : Result String Url -> String
unwrapUrl =
    unwrapResult identity Url.toString


unwrapGroupId : Result String GroupUUID -> String
unwrapGroupId =
    unwrapResult
        identity
        (\(GroupUUID groupId) -> Uuid.toString groupId)


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( empty, Cmd.none )
        , view =
            \model ->
                Browser.Document
                    "Habitica Webhook Editor"
                    [ FA.css, view model ]
        , update = update
        , subscriptions = \_ -> Time.every 1000 SetCurrentTime
        }
