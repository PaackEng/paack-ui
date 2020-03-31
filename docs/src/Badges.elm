module Badges exposing (stories)

import UI.Badge as Badge
import UIExplorer exposing (storiesOf)
import Utils exposing (storyList)


stories =
    storiesOf
        "Badges"
        [ storyList
            ( "Badges"
            , [ Badge.light "ETA 2.5L"
              , Badge.dark "ETA 2.5L"
              , Badge.primary "ETA 2.5L"
              , Badge.success "ETA 2.5L"
              , Badge.warning "ETA 2.5L"
              , Badge.error "ETA 2.5L"
              ]
            , { note =
                    """
```elm
Badge.light "ETA 2.5L"


Badge.dark "ETA 2.5L"


Badge.primary "ETA 2.5L"


Badge.success "ETA 2.5L"


Badge.warning "ETA 2.5L"


Badge.error "ETA 2.5L"

```
"""
              }
            )
        ]
