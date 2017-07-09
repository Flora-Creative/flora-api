module AppAPI exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


type alias Floraapps =
    { floraappsAppName : String
    , floraappsImages : List (String)
    , floraappsVideoLinks : List (String)
    , floraappsItunesUrl : String
    , floraappsAppDescription : String
    , floraappsBackgroundColor : String
    , floraappsForegroundColor : String
    , floraappsAuIdentifier : String
    , floraappsAppIcon : String
    , floraappsShortName : String
    , id : Int
    }

decodeFloraapps : Decoder Floraapps
decodeFloraapps =
    decode Floraapps
        |> required "floraappsAppName" string
        |> required "floraappsImages" (list string)
        |> required "floraappsVideoLinks" (list string)
        |> required "floraappsItunesUrl" string
        |> required "floraappsAppDescription" string
        |> required "floraappsBackgroundColor" string
        |> required "floraappsForegroundColor" string
        |> required "floraappsAuIdentifier" string
        |> required "floraappsAppIcon" string
        |> required "floraappsShortName" string
        |> required "id" int

encodeFloraapps : Floraapps -> Json.Encode.Value
encodeFloraapps x =
    Json.Encode.object
        [ ( "floraappsAppName", Json.Encode.string x.floraappsAppName )
        , ( "floraappsImages", (Json.Encode.list << List.map Json.Encode.string) x.floraappsImages )
        , ( "floraappsVideoLinks", (Json.Encode.list << List.map Json.Encode.string) x.floraappsVideoLinks )
        , ( "floraappsItunesUrl", Json.Encode.string x.floraappsItunesUrl )
        , ( "floraappsAppDescription", Json.Encode.string x.floraappsAppDescription )
        , ( "floraappsBackgroundColor", Json.Encode.string x.floraappsBackgroundColor )
        , ( "floraappsForegroundColor", Json.Encode.string x.floraappsForegroundColor )
        , ( "floraappsAuIdentifier", Json.Encode.string x.floraappsAuIdentifier )
        , ( "floraappsAppIcon", Json.Encode.string x.floraappsAppIcon )
        , ( "floraappsShortName", Json.Encode.string x.floraappsShortName )
        , ( "id", Json.Encode.int x.id )
        ]

get : String -> Http.Request (List (Floraapps))
get urlBase =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ urlBase
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeFloraapps)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getByName : String -> String -> Http.Request (Floraapps)
getByName urlBase capture_name =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ urlBase
                , capture_name |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeFloraapps
        , timeout =
            Nothing
        , withCredentials =
            False
        }