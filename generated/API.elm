module API exposing (..)

import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import String


type alias IOSApp =
    { appName : String
    , images : List String
    , videoLinks : List String
    , itunesUrl : String
    , appDescription : String
    , backgroundColor : String
    , foregroundColor : String
    , auIdentifier : String
    , appIcon : String
    , shortName : String
    }


decodeIOSApp : Decoder IOSApp
decodeIOSApp =
    decode IOSApp
        |> required "appName" string
        |> required "images" (list string)
        |> required "videoLinks" (list string)
        |> required "itunesUrl" string
        |> required "appDescription" string
        |> required "backgroundColor" string
        |> required "foregroundColor" string
        |> required "auIdentifier" string
        |> required "appIcon" string
        |> required "shortName" string


get : String -> Http.Request (List IOSApp)
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
            Http.expectJson (list decodeIOSApp)
        , timeout =
            Nothing
        , withCredentials =
            False
        }


getByName : String -> String -> Http.Request IOSApp
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
            Http.expectJson decodeIOSApp
        , timeout =
            Nothing
        , withCredentials =
            False
        }


type alias ContactForm =
    { origin : String
    , name : String
    , email : String
    , subject : String
    , message : String
    , leaveMeBlank : Maybe String
    }


encodeContactForm : ContactForm -> Json.Encode.Value
encodeContactForm x =
    Json.Encode.object
        [ ( "origin", Json.Encode.string x.origin )
        , ( "name", Json.Encode.string x.name )
        , ( "email", Json.Encode.string x.email )
        , ( "subject", Json.Encode.string x.subject )
        , ( "message", Json.Encode.string x.message )
        , ( "leaveMeBlank", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.leaveMeBlank )
        ]


postContact : String -> ContactForm -> Http.Request ContactForm
postContact urlBase body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ urlBase
                , "contact"
                ]
        , body =
            Http.jsonBody (encodeContactForm body)
        , expect =
            Http.expectJson decodeContactForm
        , timeout =
            Nothing
        , withCredentials =
            False
        }
