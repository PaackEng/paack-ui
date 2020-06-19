module Badges exposing (stories)

import UI.Badge as Badge
import UIExplorer exposing (storiesOf)
import Utils exposing (story)


stories cfg =
    storiesOf
        "Badges"
        [ oneBadge cfg Badge.light "light"
        , oneBadge cfg Badge.dark "dark"
        , oneBadge cfg Badge.primary "primary"
        , oneBadge cfg Badge.warning "warning"
        , oneBadge cfg Badge.danger "danger"
        , oneBadge cfg Badge.success "success"
        ]


oneBadge cfg constructor str =
    story
        ( "Badge " ++ str
        , Badge.renderElement cfg <| constructor "123"
        , { note =
                """
```elm
Badge.""" ++ str ++ """ "123"
  |> Badge.renderElement renderCfg

```
"""
          }
        )
