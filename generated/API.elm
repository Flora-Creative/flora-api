module API exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


type alias ContactForm =
    { name : String
    , email : String
    , subject : String
    , message : String
    }

decodeContactForm : Decoder ContactForm
decodeContactForm =
    decode ContactForm
        |> required "name" string
        |> required "email" string
        |> required "subject" string
        |> required "message" string

type alias IOSApp =
    { appName : String
    , images : List (String)
    , videoLinks : List (String)
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

get : String -> Http.Request (List (IOSApp))
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

getByName : String -> String -> Http.Request (IOSApp)
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