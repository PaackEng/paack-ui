module UI.Analytics exposing
    ( Analytics
    , Property, encode
    )

{-| Analytics serialization.


# Analytics

@docs Analytics


# Serialization

@docs Property, encode

-}

import UI.Internal.Analytics as Internal


{-| Analytics event data.
-}
type alias Analytics =
    Internal.Analytics


{-| A list of properties to use with`Json.Encode.object`.
-}
type alias Property =
    Internal.Property


{-| Encodes Analytics into a list of properties.

Use it with `Json.Encode.object` to describe your event:

        import UI.Analytics
        import Json.Encode as Encode

        encodeEvent : Analytics -> Value
        encodeEvent event =
            ( "name", Encode.string "EventName" )
                :: UI.Analytics.encode event
                |> Encode.object

-}
encode : Analytics -> List Property
encode =
    Internal.encode
