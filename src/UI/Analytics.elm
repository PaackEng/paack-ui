module UI.Analytics exposing (Analytics, Property, encode)

import UI.Internal.Analytics as Internal


type alias Analytics =
    Internal.Analytics


type alias Property =
    Internal.Property


encode : Analytics -> List Property
encode =
    Internal.encode
