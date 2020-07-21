module PluginOptions exposing
    ( PluginOptions
    , defaultWithMenu
    , defaultWithoutMenu
    )


type alias PluginOptions =
    { note : String
    , code : String
    , hasMenu : Bool
    }


defaultWithMenu : PluginOptions
defaultWithMenu =
    { note = ""
    , code = ""
    , hasMenu = True
    }


defaultWithoutMenu : PluginOptions
defaultWithoutMenu =
    { note = ""
    , code = ""
    , hasMenu = False
    }
