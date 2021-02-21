port module Main exposing (Model, Msg(..), SunInfo, TimeInfo, clockface, computeTimeInfo, currentSekki, getSunInfo, init, main, sekki, subscriptions, sunInfoDecoder, timeToAngle, update, view)

import Basics.Extra
import Browser
import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (..)
import Collage.Text as Text exposing (Shape(..), Text, fromString)
import Color exposing (..)
import Date exposing (..)
import Dict exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (select)
import Http exposing (..)
import Iso8601 exposing (..)
import Json.Decode as D
import Set exposing (..)
import Task exposing (..)
import Time exposing (..)
import Time.Extra exposing (..)
import TimeZone exposing (zones)


port getGeoloc : () -> Cmd msg


port geoloc : (D.Value -> msg) -> Sub msg


type alias Model =
    { currentTime : Maybe Posix
    , zone : Maybe Zone
    , sunInfo : Maybe SunInfo
    , hourOffset : Int
    , dayOffset : Int
    , monthOffset : Int
    , latitude : Float
    , latitudeBuffer : String
    , longitude : Float
    , longitudeBuffer : String
    , sunInfoCache : Dict ( Float, Float, Int ) SunInfo
    , waitingForSunInfo : Bool
    }


type Msg
    = SetZone Zone
    | Tick Posix
    | GotSunInfo (Result Error SunInfo)
    | GotGeolocation Zone D.Value
    | IncHour
    | DecHour
    | IncDay
    | DecDay
    | IncMonth
    | DecMonth
    | SetLatitude String
    | SetLongitude String
    | ReloadSunInfo
    | Reset
    | NoOp


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions model =
    Sub.batch
        [ Time.every 200 Tick
        , geoloc (GotGeolocation (Maybe.withDefault utc model.zone))
        ]


init : () -> ( Model, Cmd Msg )
init flags =
    ( { currentTime = Nothing
      , sunInfo = Nothing
      , zone = Nothing
      , hourOffset = 0
      , dayOffset = 0
      , monthOffset = 0
      , latitude = 0 --47.5556
      , latitudeBuffer = "" --String.fromFloat 47.5556
      , longitude = 0 --3.2744
      , longitudeBuffer = "" --String.fromFloat 3.2744
      , sunInfoCache = Dict.empty
      , waitingForSunInfo = False
      }
    , Cmd.batch
        [ Task.perform SetZone Time.here
        ]
    )


update msg model =
    case msg of
        SetZone zone ->
            ( { model | zone = Just zone }
            , getGeoloc ()
            )

        Tick t ->
            let
                newTime =
                    correctedTime model t

                ( sunInfo, waitingForSunInfo, cmd ) =
                    case ( model.waitingForSunInfo, model.sunInfo ) of
                        ( False, Just si ) ->
                            let
                                dayLength =
                                    sunset_ - sunrise_

                                nightLength =
                                    (24 * 60 * 60 * 1000)
                                        - dayLength

                                dayHourLength =
                                    dayLength / 6

                                nightHourLength =
                                    nightLength / 6

                                sunrise_ =
                                    toFloat <| posixToMillis si.sunrise

                                sunset_ =
                                    toFloat <| posixToMillis si.sunset

                                temporalDayStart =
                                    sunrise_ - 3 * nightHourLength

                                temporalDayEnd =
                                    temporalDayStart + (24 * 60 * 60 * 1000)

                                newTime_ =
                                    toFloat <| posixToMillis newTime

                                zone_ =
                                    model.zone |> Maybe.withDefault utc
                            in
                            if newTime_ >= temporalDayStart && newTime_ <= temporalDayEnd then
                                ( Just si, False, Cmd.none )

                            else
                                let
                                    correctedDate =
                                        --Adjust date because temporal midnight and gregorian midnight are not the same
                                        if Date.day si.date == (Date.day <| Date.fromPosix zone_ newTime) then
                                            if newTime_ < temporalDayStart then
                                                Time.Extra.add Time.Extra.Hour -1 zone_ newTime

                                            else if newTime_ > temporalDayEnd then
                                                Time.Extra.add Time.Extra.Hour 1 zone_ newTime

                                            else
                                                newTime

                                        else
                                            newTime
                                in
                                case Dict.get ( model.latitude, model.longitude, Date.toRataDie <| Date.fromPosix zone_ correctedDate ) model.sunInfoCache of
                                    Just si_ ->
                                        ( Just si_, False, Cmd.none )

                                    Nothing ->
                                        ( model.sunInfo, True, getSunInfo model.latitude model.longitude (Date.fromPosix zone_ correctedDate) )

                        _ ->
                            ( model.sunInfo, model.waitingForSunInfo, Cmd.none )
            in
            ( { model
                | currentTime = Just newTime
                , waitingForSunInfo = waitingForSunInfo
                , sunInfo = sunInfo
              }
            , cmd
            )

        GotSunInfo res ->
            case res of
                Ok si ->
                    ( { model
                        | sunInfo = Just si
                        , sunInfoCache = Dict.insert ( model.latitude, model.longitude, Date.toRataDie si.date ) si model.sunInfoCache
                        , waitingForSunInfo = False
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        GotGeolocation zone val ->
            case D.decodeValue geolocDecoder val of
                Ok ( lat, lng ) ->
                    let
                        newModel =
                            { model
                                | latitude = lat
                                , latitudeBuffer = String.fromFloat lat
                                , longitude = lng
                                , longitudeBuffer = String.fromFloat lng
                            }
                    in
                    ( newModel
                    , Maybe.map (Date.fromPosix zone) newModel.currentTime
                        |> Maybe.map (getSunInfo newModel.latitude newModel.longitude)
                        |> Maybe.withDefault Cmd.none
                    )

                Err e ->
                    let
                        tokyoLat =
                            35.689722

                        tokyoLng =
                            139.692222

                        newModel =
                            { model
                                | latitude = tokyoLat
                                , latitudeBuffer = String.fromFloat tokyoLat
                                , longitude = tokyoLng
                                , longitudeBuffer = String.fromFloat tokyoLng
                            }
                    in
                    ( newModel
                    , Maybe.map (Date.fromPosix zone) newModel.currentTime
                        |> Maybe.map (getSunInfo newModel.latitude newModel.longitude)
                        |> Maybe.withDefault Cmd.none
                    )

        IncHour ->
            ( { model | hourOffset = model.hourOffset + 1 }
            , Cmd.none
            )

        DecHour ->
            ( { model | hourOffset = model.hourOffset - 1 }
            , Cmd.none
            )

        IncDay ->
            ( { model
                | dayOffset = model.dayOffset + 1
              }
            , Cmd.none
            )

        DecDay ->
            ( { model
                | dayOffset = model.dayOffset - 1
              }
            , Cmd.none
            )

        IncMonth ->
            ( { model
                | monthOffset = model.monthOffset + 1
              }
            , Cmd.none
            )

        DecMonth ->
            ( { model
                | monthOffset = model.monthOffset - 1
              }
            , Cmd.none
            )

        SetLatitude lat ->
            ( { model
                | latitude = String.toFloat lat |> Maybe.withDefault model.latitude
                , latitudeBuffer = lat
              }
            , Cmd.none
            )

        SetLongitude lng ->
            ( { model
                | longitude = String.toFloat lng |> Maybe.withDefault model.longitude
                , longitudeBuffer = lng
              }
            , Cmd.none
            )

        ReloadSunInfo ->
            case ( model.currentTime, model.zone ) of
                ( Just time, Just zone ) ->
                    ( model, getSunInfo model.latitude model.longitude (Date.fromPosix zone time) )

                _ ->
                    ( model, Cmd.none )

        Reset ->
            case ( model.currentTime, model.zone ) of
                ( Just currentTime, Just zone ) ->
                    let
                        currentTime_ =
                            correctedTime
                                { model
                                    | hourOffset = 0
                                    , dayOffset = 0
                                    , monthOffset = 0
                                }
                                currentTime

                        today =
                            Date.fromPosix zone currentTime_
                    in
                    ( { model
                        | hourOffset = 0
                        , dayOffset = 0
                        , monthOffset = 0
                        , currentTime = Just currentTime_
                      }
                    , getSunInfo model.latitude model.longitude today
                    )

                _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


zeroTime =
    millisToPosix 0



-------------------------------------------------------------------------------


view model =
    { title = ""
    , body =
        [ layout []
            (row
                [ Element.height fill ]
                [ column
                    [ Element.width fill
                    , Element.height fill
                    , spacing 15
                    ]
                    [ clockface model
                    , timeInfoView model
                    ]
                , controlPanelView model
                ]
            )
        ]
    }


timeInfoView model =
    case ( model.zone, model.currentTime, model.sunInfo ) of
        ( Just zone, Just time, Just { sunrise, sunset } ) ->
            let
                hour t =
                    String.padLeft 2 '0' (String.fromInt (Time.toHour zone t))

                minute t =
                    String.padLeft 2 '0' (String.fromInt (Time.toMinute zone t))

                second t =
                    String.padLeft 2 '0' (String.fromInt (Time.toSecond zone t))

                day t =
                    String.padLeft 2 '0' (String.fromInt (Time.toDay zone t))

                ti =
                    computeTimeInfo zone time sunrise sunset 0

                milliToStr v =
                    let
                        hours =
                            round v // 1000 // 60 // 60

                        minutes =
                            modBy (1000 * 60 * 60) (round v)
                                |> (\x -> x // 1000 // 60)
                    in
                    String.padLeft 2 '0' (String.fromInt hours)
                        ++ ":"
                        ++ String.padLeft 2 '0' (String.fromInt minutes)
            in
            column
                [ spacing 15
                , padding 20
                ]
                [ row
                    [ spacing 10 ]
                    [ text "Current time:"
                    , Element.text <| hour time ++ ":" ++ minute time ++ ":" ++ second time
                    ]
                , row
                    [ spacing 10 ]
                    [ text "Day hour length:"
                    , Element.text <|
                        milliToStr ti.dayHourLength
                    ]
                , row
                    [ spacing 10 ]
                    [ text "Night hour length:"
                    , Element.text <|
                        milliToStr ti.nightHourLength
                    ]
                , row
                    [ spacing 10 ]
                    [ text "Sunrise:"
                    , Element.text <| hour sunrise ++ ":" ++ minute sunrise
                    ]
                , row
                    [ spacing 10 ]
                    [ text "Sunset:"
                    , Element.text <| hour sunset ++ ":" ++ minute sunset
                    ]
                , row
                    [ spacing 10 ]
                    [ text "temporalHour: "
                    , Element.text <| String.fromFloat ti.temporalTime.temporalHour
                    ]
                , row
                    [ spacing 10 ]
                    [ text "temporalMinute: "
                    , Element.text <| String.fromFloat ti.temporalTime.temporalMinute
                    ]
                , row
                    [ spacing 10 ]
                    [ text "temporalSecond: "
                    , Element.text <| String.fromFloat ti.temporalTime.temporalSecond
                    ]

                --, row
                --    [ spacing 10 ]
                --    [ text "Hour hand pos (pos on trig circle in radians):"
                --    , Element.text <| String.fromFloat ti.hourHandPos
                --    ]
                --, row
                --    [ spacing 10 ]
                --    [ text "Minute hand pos (pos on trig circle in radians):"
                --    , Element.text <| String.fromFloat ti.minuteHandPos
                --    ]
                --, row
                --    [ spacing 10 ]
                --    [ text "Minute hand pos (pos on trig circle in radians):"
                --    , Element.text <| String.fromFloat ti.minuteHandPos
                --    ]
                --, row
                --    [ spacing 10 ]
                --    [ text <|
                --        "Is day:"
                --            ++ (let
                --                    alphaDiff a b =
                --                        if a > b then
                --                            a - b
                --                        else
                --                            b - a
                --                    d1 =
                --                        alphaDiff ti.sunrisePos ti.sunsetPos
                --                    d2 =
                --                        alphaDiff ti.sunrisePos ti.hourHandPos + alphaDiff ti.sunsetPos ti.hourHandPos
                --                in
                --                if
                --                    if ti.dayHourLength > ti.nightHourLength then
                --                        d2 > d1
                --                    else
                --                        d2 <= d1
                --                then
                --                    " yes"
                --                else
                --                    " no"
                --               )
                --    ]
                ]

        _ ->
            Element.none


controlPanelView model =
    let
        buttonStyle =
            [ Font.center
            , Background.color (Element.rgb255 220 220 220)
            , paddingXY 5 5
            , Border.rounded 5
            ]

        offsetView o =
            (if o >= 0 then
                "(+ "

             else
                "(- "
            )
                ++ String.fromInt o
                ++ ")"
    in
    case model.sunInfo of
        Just _ ->
            column
                [ padding 20
                , alignTop
                , spacing 15
                ]
                [ row
                    [ spacing 15 ]
                    [ el [ Element.width (px 75) ] (text "Hours:")
                    , Input.button
                        buttonStyle
                        { onPress = Just IncHour
                        , label = text "+"
                        }
                    , Input.button
                        buttonStyle
                        { onPress = Just DecHour
                        , label = text "-"
                        }
                    , text <| offsetView model.hourOffset
                    ]
                , row
                    [ spacing 15 ]
                    [ el [ Element.width (px 75) ] (text "Days:")
                    , Input.button
                        buttonStyle
                        { onPress = Just IncDay
                        , label = text "+"
                        }
                    , Input.button
                        buttonStyle
                        { onPress = Just DecDay
                        , label = text "-"
                        }
                    , text <| offsetView model.dayOffset
                    ]
                , row
                    [ spacing 15 ]
                    [ el [ Element.width (px 75) ] (text "Months:")
                    , Input.button
                        buttonStyle
                        { onPress = Just IncMonth
                        , label = text "+"
                        }
                    , Input.button
                        buttonStyle
                        { onPress = Just DecMonth
                        , label = text "-"
                        }
                    , text <| offsetView model.monthOffset
                    ]
                , column
                    [ spacing 15 ]
                    [ Input.text
                        [ Element.width (px 130)
                        , Element.height (px 30)
                        , padding 5
                        ]
                        { onChange = SetLatitude
                        , text =
                            model.latitudeBuffer
                        , placeholder = Nothing
                        , label =
                            Input.labelLeft [ centerY, Element.width (px 100) ] (text "Latitude: ")
                        }
                    , Input.text
                        [ Element.width (px 130)
                        , Element.height (px 30)
                        , padding 5
                        ]
                        { onChange = SetLongitude
                        , text =
                            model.longitudeBuffer
                        , placeholder = Nothing
                        , label = Input.labelLeft [ centerY, Element.width (px 100) ] (text "Longitude: ")
                        }
                    ]
                , row
                    [ spacing 15 ]
                    [ Input.button
                        buttonStyle
                        { onPress = Just ReloadSunInfo
                        , label = text "Reload sun info"
                        }
                    , Input.button
                        buttonStyle
                        { onPress = Just Reset
                        , label = text "Reset"
                        }
                    ]
                ]

        _ ->
            Element.none


clockface model =
    case ( model.zone, model.currentTime, model.sunInfo ) of
        ( Just zone, Just time, Just { sunrise, sunset } ) ->
            let
                today =
                    Date.fromPosix zone time

                ti =
                    computeTimeInfo zone time sunrise sunset 0

                outerRim =
                    circle 115
                        |> styled
                            ( uniform lightGrey
                            , solid 1 (uniform darkCharcoal)
                            )

                innerRim =
                    circle 68
                        |> styled
                            ( uniform lightGrey
                            , solid 1 (uniform darkCharcoal)
                            )

                middleRim =
                    circle 85
                        |> styled
                            ( uniform white
                            , solid 1 (uniform darkCharcoal)
                            )

                daySymbols =
                    [ ( "卯", "六" )
                    , ( "辰", "五" )
                    , ( "巳", "四" )
                    , ( "午", "九" )
                    , ( "未", "八" )
                    , ( "申", "七" )
                    ]
                        |> List.foldl
                            (\d ( done, acc ) ->
                                ( ( d, acc ) :: done
                                , acc - ti.dayHourArc
                                )
                            )
                            ( [], ti.sunrisePos + 2 * pi )
                        |> Tuple.first
                        |> List.map (\( s, angle ) -> ( s, angle - ti.dayHourArc / 2 ))

                nightSymbols =
                    [ ( "酉", "六" )
                    , ( "戌", "五" )
                    , ( "亥", "四" )
                    , ( "子", "九" )
                    , ( "丑", "八" )
                    , ( "寅", "七" )
                    ]
                        |> List.foldl
                            (\d ( done, acc ) ->
                                ( ( d, acc ) :: done
                                , acc - ti.nightHourArc
                                )
                            )
                            ( [], ti.sunsetPos + 2 * pi )
                        |> Tuple.first
                        |> List.map (\( s, angle ) -> ( s, angle - ti.nightHourArc / 2 ))

                delims hourArc symbols =
                    List.map
                        (\( _, angle ) ->
                            let
                                ( x1, y1 ) =
                                    fromPolar ( 68, angle - hourArc / 2 )

                                ( x2, y2 ) =
                                    fromPolar ( 115, angle - hourArc / 2 )
                            in
                            ( ( x1, y1 ), ( x2, y2 ) )
                        )
                        symbols
                        |> List.map (\( a, b ) -> segment a b)
                        |> List.map (traced (solid 1 (uniform black)))

                symbolView isDay ( ( sign, number ), angle ) =
                    let
                        ( x, y ) =
                            fromPolar ( 97.5, angle )

                        sView =
                            Text.fromString sign
                                |> (if isDay then
                                        Text.color yellow

                                    else
                                        Text.color blue
                                   )
                                |> Text.size 20
                                |> rendered
                                |> shiftX x
                                |> shiftY y
                                |> Collage.rotate (angle - pi / 2)

                        ( u, v ) =
                            fromPolar ( 75.5, angle )

                        nView =
                            Text.fromString number
                                |> Text.color black
                                |> Text.size 14
                                |> rendered
                                |> shiftX u
                                |> shiftY v
                                |> Collage.rotate (angle - pi / 2)
                    in
                    [ sView, nView ]

                hand clr linePx len angle =
                    segment ( 0, 0 ) (fromPolar ( len, angle ))
                        |> traced (solid linePx (uniform clr))

                sekkiView =
                    currentSekki today
                        |> Text.fromString
                        |> Text.color black
                        |> Text.size 22
                        |> rendered
                        |> shiftY 30

                dateView =
                    (String.padLeft 2
                        '0'
                        (String.fromInt <| Date.monthNumber today)
                        ++ "月"
                        ++ String.padLeft 2
                            '0'
                            (String.fromInt <| Date.day today)
                        ++ "日"
                    )
                        |> Text.fromString
                        |> Text.color black
                        |> Text.size 20
                        |> rendered
                        |> shiftY -30
            in
            (stack <|
                [ circle 4
                    |> filled (uniform darkRed)
                , hand darkRed 2 65 ti.hourHandPos
                , hand darkRed 2 82 ti.minuteHandPos
                , hand darkBlue 2 112 ti.secondHandPos
                , sekkiView
                , dateView
                ]
                    ++ List.concatMap (symbolView True) daySymbols
                    ++ List.concatMap (symbolView False) nightSymbols
                    ++ delims ti.dayHourArc daySymbols
                    ++ delims ti.nightHourArc nightSymbols
                    ++ [ innerRim
                       , middleRim
                       , outerRim
                       ]
            )
                |> svg
                |> (\x ->
                        el
                            [ padding 20
                            , Element.width (px 270)
                            , Element.height (px 270)
                            ]
                            (Element.html x)
                   )

        _ ->
            row
                [ padding 20
                , spacing 7
                ]
                [ text "loading..."
                , el
                    [ Background.image "loadingS.webp"
                    , Element.width (px 25)
                    , Element.height (px 25)
                    ]
                    Element.none
                ]



-------------------------------------------------------------------------------


correctedTime model now =
    case model.zone of
        Just zone ->
            Time.Extra.add Time.Extra.Month model.monthOffset zone now
                |> Time.Extra.add Time.Extra.Day model.dayOffset zone
                |> Time.Extra.add Time.Extra.Hour model.hourOffset zone

        _ ->
            now


timeToAngle : Int -> Int -> Int -> Float
timeToAngle hour min sec =
    (((toFloat (modBy 12 hour) * 60 * 60)
        + (toFloat min * 60)
        + toFloat sec
     )
        / 43200
    )
        * (2 * pi)
        |> (\n -> n - (pi / 2))
        |> (\n -> (2 * pi) - n)


currentSekki : Date -> String
currentSekki today =
    let
        inRange rd =
            let
                start =
                    fromRataDie rd

                end =
                    Date.add Days 15 start
            in
            Date.isBetween start end today
    in
    Dict.toList (sekki (Date.year today))
        |> List.filter (Tuple.first >> inRange)
        |> List.head
        |> Maybe.map Tuple.second
        |> Maybe.withDefault "冬至"


sekki : Int -> Dict Int String
sekki year =
    [ ( ( Jan, 6 ), "小寒" )
    , ( ( Jan, 21 ), "大寒" )
    , ( ( Feb, 4 ), "立春" )
    , ( ( Feb, 19 ), "雨水" )
    , ( ( Mar, 5 ), "啓蟄" )
    , ( ( Mar, 20 ), "春分" )
    , ( ( Apr, 4 ), "清明" )
    , ( ( Apr, 20 ), "穀雨" )
    , ( ( May, 5 ), "立夏" )
    , ( ( May, 21 ), "小満" )
    , ( ( Jun, 5 ), "芒種" )
    , ( ( Jun, 21 ), "夏至" )
    , ( ( Jul, 7 ), "小暑" )
    , ( ( Jul, 22 ), "大暑" )
    , ( ( Aug, 7 ), "立秋" )
    , ( ( Aug, 23 ), "処暑" )
    , ( ( Sep, 7 ), "白露" )
    , ( ( Sep, 23 ), "秋分" )
    , ( ( Oct, 8 ), "寒露" )
    , ( ( Oct, 23 ), "霜降" )
    , ( ( Nov, 7 ), "立冬" )
    , ( ( Nov, 22 ), "小雪" )
    , ( ( Dec, 7 ), "大雪" )
    , ( ( Dec, 21 ), "冬至" )
    ]
        |> List.map
            (\( ( m, d ), s ) ->
                ( fromCalendarDate year m d
                    |> toRataDie
                , s
                )
            )
        |> Dict.fromList


type alias TimeInfo =
    { dayLength : Float -- duration in milliseconds
    , nightLength : Float -- duration in milliseconds
    , dayHourLength : Float -- duration in milliseconds
    , dayHourArc : Float -- angle in radians
    , nightHourLength : Float -- duration in milliseconds
    , nightHourArc : Float -- angle in radians
    , sunrisePos : Float -- pos on trig circle in radians
    , sunsetPos : Float -- pos on trig circle in radians
    , hourHandPos : Float -- pos on trig circle in radians
    , minuteHandPos : Float -- pos on trig circle in radians
    , secondHandPos : Float -- pos on trig circle in radians
    , temporalTime :
        { temporalHour : Float
        , temporalMinute : Float
        , temporalSecond : Float
        }
    }


computeTimeInfo : Zone -> Posix -> Posix -> Posix -> Int -> TimeInfo
computeTimeInfo zone time sunrise sunset dayStartOffset =
    let
        dayLength =
            toFloat <|
                (2 * dayStartOffset)
                    + (posixToMillis sunset
                        - posixToMillis sunrise
                      )

        nightLength =
            (24 * 60 * 60 * 1000)
                - dayLength

        dayHourLength =
            dayLength / 6

        dayHourArc =
            dayHourLength / (24 * 60 * 60 * 1000) * (2 * pi)

        nightHourLength =
            nightLength / 6

        nightHourArc =
            nightHourLength / (24 * 60 * 60 * 1000) * (2 * pi)

        temporalTime =
            let
                time_ =
                    toFloat <| posixToMillis time

                sunrise_ =
                    toFloat <| posixToMillis sunrise

                sunset_ =
                    toFloat <| posixToMillis sunset

                temporalMidnight =
                    sunrise_ - 3 * nightHourLength

                dayMinLength =
                    dayHourLength / 60

                daySecLength =
                    dayMinLength / 60

                nightMinLength =
                    nightHourLength / 60

                nightSecLength =
                    nightMinLength / 60
            in
            if time_ >= temporalMidnight && time_ < sunrise_ then
                --before dawn
                let
                    temporalHour =
                        toFloat <| Basics.floor <| (time_ - temporalMidnight) / nightHourLength

                    temporalMinute =
                        toFloat <| Basics.floor <| (time_ - (temporalMidnight + (temporalHour * nightHourLength))) / nightMinLength

                    temporalSecond =
                        toFloat <| Basics.floor <| (time_ - (temporalMidnight + (temporalHour * nightHourLength) + (temporalMinute * nightMinLength))) / nightSecLength
                in
                { temporalHour = temporalHour
                , temporalMinute = temporalMinute
                , temporalSecond = temporalSecond
                }

            else if time_ >= sunrise_ && time_ < sunset_ then
                --before sunset
                let
                    temporalHour =
                        toFloat <| Basics.floor <| (time_ - sunrise_) / dayHourLength

                    temporalMinute =
                        toFloat <| Basics.floor <| (time_ - (sunrise_ + (temporalHour * dayHourLength))) / dayMinLength

                    temporalSecond =
                        toFloat <| Basics.floor <| (time_ - (sunrise_ + (temporalHour * dayHourLength) + (temporalMinute * dayMinLength))) / daySecLength
                in
                { temporalHour = 3 + temporalHour
                , temporalMinute = temporalMinute
                , temporalSecond = temporalSecond
                }

            else
                --after sunset
                let
                    temporalHour =
                        toFloat <| Basics.floor <| (time_ - sunset_) / nightHourLength

                    temporalMinute =
                        toFloat <| Basics.floor <| (time_ - (sunset_ + (temporalHour * nightHourLength))) / nightMinLength

                    temporalSecond =
                        toFloat <| Basics.floor <| (time_ - (sunset_ + (temporalHour * nightHourLength) + (temporalMinute * nightMinLength))) / nightSecLength
                in
                { temporalHour = 9 + temporalHour
                , temporalMinute = temporalMinute
                , temporalSecond = temporalSecond
                }

        sunrisePos =
            (3 * pi / 2)
                + (3 * dayHourArc)
                |> normalize

        sunsetPos =
            (pi / 2)
                + (3 * nightHourArc)
                |> normalize

        hourHandPos =
            sunrisePos
                + (toFloat <| posixToMillis sunrise - posixToMillis time)
                / (24 * 60 * 60 * 1000)
                * (2 * pi)
                |> normalize

        isDay =
            let
                alphaDiff a b =
                    if a > b then
                        a - b

                    else
                        b - a

                d1 =
                    --shortest interval between sunrise and sunset
                    alphaDiff sunrisePos sunsetPos

                d2 =
                    alphaDiff sunrisePos hourHandPos + alphaDiff sunsetPos hourHandPos
            in
            if dayHourLength > nightHourLength then
                d2 > d1

            else
                d2 <= d1

        minuteHandPos =
            (pi / 2) - (temporalTime.temporalMinute / 60) * 2 * pi

        secondHandPos =
            (pi / 2) - (temporalTime.temporalSecond / 60) * 2 * pi
    in
    { dayLength = dayLength
    , nightLength = nightLength
    , dayHourLength = dayHourLength
    , dayHourArc = dayHourArc
    , nightHourLength = nightHourLength
    , nightHourArc = nightHourArc
    , sunrisePos = sunrisePos
    , sunsetPos = sunsetPos
    , hourHandPos = hourHandPos
    , minuteHandPos = minuteHandPos
    , secondHandPos = secondHandPos
    , temporalTime = temporalTime
    }


normalize alpha =
    let
        a_ =
            Basics.Extra.fractionalModBy (2 * pi) alpha
    in
    if a_ < 0 then
        2 * pi + a_

    else
        a_



--temporalTimeToDate sunrise temporalTime =
--    modBy 12 temporalTime.temporalHour
--    |>
-------------------------------------------------------------------------------


type alias SunInfo =
    { sunrise : Posix
    , sunset : Posix
    , date : Date
    }


sunInfoDecoder date =
    D.field "results"
        (D.map2 (\a b -> SunInfo a b date)
            (D.field "sunrise"
                (D.string
                    |> D.map Iso8601.toTime
                    |> D.map (Result.withDefault (Time.millisToPosix 0))
                )
            )
            (D.field "sunset"
                (D.string
                    |> D.map Iso8601.toTime
                    |> D.map (Result.withDefault (Time.millisToPosix 0))
                )
            )
        )


getSunInfo latitude longitude date =
    let
        lat =
            String.fromFloat latitude

        lng =
            String.fromFloat longitude

        dateStr =
            (String.fromInt <| Date.year date)
                ++ "-"
                ++ (String.fromInt <| Date.monthNumber date)
                ++ "-"
                ++ (String.fromInt <| Date.day date)
    in
    Http.get
        { url =
            "https://api.sunrise-sunset.org/json?lat="
                ++ lat
                ++ "&lng="
                ++ lng
                ++ "&date="
                ++ dateStr
                ++ "&formatted=0\n"
        , expect = Http.expectJson GotSunInfo (sunInfoDecoder date)
        }


geolocDecoder =
    D.oneOf
        [ D.map2 Tuple.pair
            (D.at [ "coords", "latitude" ] D.float)
            (D.at [ "coords", "longitude" ] D.float)
        ]
