module Form.State exposing
    ( Model
    , Msg(..)
    , initModel
    , update
    )


type Msg
    = OnPlaceholderStoryTyped String
    | OnCheckboxToggled Bool


type alias Model =
    { placeholderStory : String
    , checkboxStory : Bool
    }


initModel : Model
initModel =
    { placeholderStory = ""
    , checkboxStory = False
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnPlaceholderStoryTyped val ->
            { model | placeholderStory = val }

        OnCheckboxToggled bool ->
            { model | checkboxStory = bool }
