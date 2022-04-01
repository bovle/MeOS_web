module Main exposing (..)

import Animation
import Browser
import Browser.Dom exposing (Element, Viewport, getElement)
import Browser.Events exposing (onAnimationFrame, onAnimationFrameDelta)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (checked, class, classList, for, height, id, placeholder, style, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Html.Keyed as Keyed
import Html.Lazy
import Http exposing (Error(..), expectString, get)
import Http.Xml exposing (expectXml)
import Set exposing (Set)
import Task
import Time
import Xml.Decode
import XmlParser



--MODEL


type alias Model =
    { classes : Dict Int Class
    , results : List ResultTime
    , error : Maybe String
    , containerHeight : Float
    , resultsHeight : Float
    , transformSpeed : Float
    , delay : Float
    , showConfig : Bool
    , scrollSpeed : Float
    , meosIp : String
    }


type Status a
    = Loading
    | Loaded a
    | Failed


init : () -> ( Model, Cmd Msg )
init _ =
    ( { classes = Dict.empty
      , results = []
      , containerHeight = 500
      , resultsHeight = 500
      , transformSpeed = 0
      , delay = 0
      , showConfig = False
      , scrollSpeed = 50
      , error = Nothing
      , meosIp = "127.0.0.1:2009"
      }
    , getElement "container" |> Task.attempt GotContainerElement
      --getClasses
    )



--UPDATE


type Msg
    = CompleteClassesLoad (Result Http.Error (Dict Int Class))
    | CompleteResultsLoad (Result Http.Error (List ResultTime))
    | GotContainerElement (Result Browser.Dom.Error Element)
    | GotResultsElement (Result Browser.Dom.Error Element)
    | FetchResults Time.Posix
    | SetClassActive ( Int, Bool )
    | ToggleConfig
    | SetScrollSpeed String
    | SetMeosIp String
    | UpdateClick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchResults _ ->
            ( model, getResults model.meosIp )

        GotContainerElement (Ok element) ->
            ( { model | containerHeight = element.viewport.height - element.element.y - 10 }, getClasses model.meosIp )

        GotContainerElement (Err (Browser.Dom.NotFound error)) ->
            ( { model | error = Just ("Not Found element: " ++ error) }, getClasses model.meosIp )

        CompleteClassesLoad (Ok newClasses) ->
            ( { model | classes = Dict.union model.classes newClasses }, getElement "results" |> Task.attempt GotResultsElement )

        CompleteClassesLoad (Err error) ->
            ( { model | error = Just ("Error getting classes: " ++ stringFromHttpError error) }, getElement "results" |> Task.attempt GotResultsElement )

        CompleteResultsLoad (Ok res) ->
            ( { model | results = res, error = Nothing }, getElement "results" |> Task.attempt GotResultsElement )

        CompleteResultsLoad (Err error) ->
            ( { model | error = Just ("Error getting results: " ++ stringFromHttpError error) }, Cmd.none )

        SetClassActive ( key, val ) ->
            ( { model | classes = Dict.update key (Maybe.map (\old -> { old | active = val })) model.classes }
            , getElement "results" |> Task.attempt GotResultsElement
            )

        SetScrollSpeed speedStr ->
            ( updateModelScrollSpeed model.resultsHeight (Maybe.withDefault 0 (String.toFloat speedStr)) model, Cmd.none )

        SetMeosIp meosIpStr ->
            ( { model | meosIp = meosIpStr }, Cmd.none )

        UpdateClick ->
            ( {model | resultsHeight = 500}, Cmd.batch [ getClasses model.meosIp, getResults model.meosIp ] )

        ToggleConfig ->
            ( { model | showConfig = not model.showConfig }, Cmd.none )

        GotResultsElement (Ok element) ->
            ( updateModelScrollSpeed element.element.height model.scrollSpeed model
            , Cmd.none
            )

        GotResultsElement (Err (Browser.Dom.NotFound error)) ->
            ( { model | error = Just ("Not Found element: " ++ error) }, Cmd.none )

updateClasses: Dict Int Class -> Dict Int Class -> Dict Int Class
updateClasses oldClasses newClasses = 
    Dict.union oldClasses newClasses

updateModelScrollSpeed : Float -> Float -> Model -> Model
updateModelScrollSpeed newHeight speed model =
    let
        should_scroll =
            newHeight > (model.containerHeight * 2)

        should_update_speed =
            (model.resultsHeight /= newHeight || model.scrollSpeed /= speed) && should_scroll

        new_speed =
            newHeight / speed
    in
    { model
        | scrollSpeed = speed
        , resultsHeight = newHeight
        , transformSpeed =
            if should_update_speed then
                new_speed

            else if should_scroll then
                model.transformSpeed

            else
                0
        , delay =
            if should_update_speed && model.transformSpeed > 0 then
                model.delay + (model.transformSpeed - new_speed)

            else
                model.delay
    }


stringFromHttpError : Http.Error -> String
stringFromHttpError err =
    case err of
        Http.BadBody msg ->
            "Bad Body: " ++ msg

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network Error"

        Http.BadStatus errNum ->
            "Bad Status: " ++ String.fromInt errNum

        Http.BadUrl msg ->
            "Bad Url: " ++ msg


type alias ResultTime =
    { class : Int
    , status : Int
    , time : Int
    , name : String
    , team : String
    }


type alias Class =
    { key : Int
    , name : String
    , order : Int
    , active : Bool
    }


getResults : String -> Cmd Msg
getResults meosIp =
    get
        { url = "http://" ++ meosIp ++ "/meos?get=result"
        , expect = expectXml CompleteResultsLoad xmlResultsDecoder
        }


xmlResultsDecoder : Xml.Decode.Decoder (List ResultTime)
xmlResultsDecoder =
    Xml.Decode.path [ "results", "person" ] (Xml.Decode.list xmlSingleResultDecoder)


xmlSingleResultDecoder : Xml.Decode.Decoder ResultTime
xmlSingleResultDecoder =
    Xml.Decode.map5
        ResultTime
        (Xml.Decode.intAttr "cls")
        (Xml.Decode.intAttr "stat")
        (Xml.Decode.withDefault -1 (Xml.Decode.intAttr "rt"))
        (Xml.Decode.path [ "name" ] (Xml.Decode.single Xml.Decode.string))
        (Xml.Decode.path [ "org" ] (Xml.Decode.single Xml.Decode.string))


getClasses : String -> Cmd Msg
getClasses meosIp =
    get
        { url = "http://" ++ meosIp ++ "/meos?get=class"
        , expect = expectXml CompleteClassesLoad xmlClassesDecoder
        }


xmlClassesDecoder : Xml.Decode.Decoder (Dict Int Class)
xmlClassesDecoder =
    Xml.Decode.map4
        (List.map4 Class)
        (Xml.Decode.path [ "cls" ] (Xml.Decode.list (Xml.Decode.intAttr "id")))
        (Xml.Decode.path [ "cls" ] (Xml.Decode.list Xml.Decode.string))
        (Xml.Decode.path [ "cls" ] (Xml.Decode.list (Xml.Decode.intAttr "ord")))
        (Xml.Decode.path [ "cls" ] (Xml.Decode.list (Xml.Decode.succeed True)))
        |> Xml.Decode.map (List.map (\cls -> ( cls.key, cls )))
        |> Xml.Decode.map Dict.fromList



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ id "header" ] [ h2 [] [ text "Resultater" ], p [] [text (getActiveClassNames  model.classes)], button [ onClick ToggleConfig ] [ text "config" ] ]
        , div [ id "container", style "height" (String.fromFloat model.containerHeight ++ "px") ]
            [ div
                [ style "animation-duration" (String.fromFloat model.transformSpeed ++ "s")
                --, style "animation-delay" (String.fromFloat model.delay ++ "s")
                , id "results"
                , classList [ ( "auto-scroll", model.resultsHeight > model.containerHeight ) ]
                ]
                (if List.isEmpty model.results then
                    [ text "loading results" ]

                 else
                    resultsView model
                )
            ]
        , div [ classList [ ( "config", True ), ( "hide", not model.showConfig ) ] ]
            [ div []
                [ input [ placeholder "pixel pr. sekund scroll", type_ "text", id "speed", onInput SetScrollSpeed, value (String.fromFloat model.scrollSpeed) ] []
                , input [ placeholder "meos ip og port", type_ "text", id "meosIp", onInput SetMeosIp, value model.meosIp ] []
                , button [ onClick UpdateClick ] [ text "opdater" ]
                ]
            , br [] []
            , div [ class "class-config" ]
                (model.classes
                    |> Dict.values
                    |> List.sortBy (\cls -> cls.order)
                    |> List.map
                        (\cls ->
                            div []
                                [ input
                                    [ type_ "checkbox"
                                    , checked cls.active
                                    , onCheck (\chk -> SetClassActive ( cls.key, chk ))
                                    ]
                                    []
                                , text cls.name
                                ]
                        )
                )
            ]
        , div [ class "error" ]
            (case model.error of
                Nothing ->
                    []

                Just err ->
                    [ text err ]
            )
        ]

getActiveClassNames: Dict Int Class -> String
getActiveClassNames classes = 
    classes
     |> Dict.values
     |> List.filter (\cls -> cls.active)
     |> List.map (\cls -> cls.name)
     |> List.foldl (\res name -> res ++ ", " ++ name) ""
     |> String.dropRight 2

resultsView : Model -> List (Html Msg)
resultsView model =
    let
        classIds =
            model.results
                |> List.map (\res -> res.class)
                |> List.filter (\classId -> Dict.get classId model.classes |> Maybe.map (\class -> class.active) |> Maybe.withDefault False)
                |> Set.fromList
                |> Set.toList
    in
    if model.resultsHeight > model.containerHeight then
        [ resultListView classIds model ] ++ [ resultListView classIds model ]

    else
        [ resultListView classIds model ]


resultListView : List Int -> Model -> Html Msg
resultListView classIds model =
    Keyed.ul []
        (model.classes
            |> Dict.filter (\key _ -> List.member key classIds)
            |> Dict.values
            |> List.sortBy (\cls -> cls.order)
            |> List.map
                (\cls ->
                    keyedClassResultView cls.key cls.name (List.filter (\res -> res.class == cls.key) model.results)
                )
        )


keyedClassResultView : Int -> String -> List ResultTime -> ( String, Html msg )
keyedClassResultView classId className results =
    ( String.fromInt classId, Html.Lazy.lazy2 classResultView className results )


classResultView : String -> List ResultTime -> Html msg
classResultView className results =
    li [ class "class" ]
        [ h3 [] [ text className ]
        , Keyed.ol [ class "class_results" ]
            (results
                |> List.map keyedSingleResultView
            )
        ]


keyedSingleResultView : ResultTime -> ( String, Html msg )
keyedSingleResultView res =
    ( res.name, Html.Lazy.lazy singleResultView res )


singleResultView : ResultTime -> Html msg
singleResultView res =
    li []
        [ text
            (res.name
                ++ " | "
                ++ (if res.status == 1 then
                        timeView res.time

                    else
                        "not finished"
                   )
            )
        ]


timeView : Int -> String
timeView time =
    let
        hour =
            String.fromInt (time // 10 // 60 // 60)

        minuteStr =
            String.fromInt (modBy 60 (time // 10 // 60))

        minute = if String.length minuteStr == 1 then "0" ++ minuteStr else minuteStr

        secondStr =
            String.fromInt (modBy 60 (time // 10))

        second = if String.length secondStr == 1 then "0" ++ secondStr else secondStr
    in
    hour ++ ":" ++ minute ++ ":" ++ second



--SUBS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (2 * 60 * 1000) FetchResults
        ]



--MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
