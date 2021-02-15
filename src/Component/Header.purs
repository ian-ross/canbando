module Component.Header (header) where

import Prelude hiding (div)

import Halogen (ComponentHTML)
import Halogen.HTML (AttrName(..), ClassName(..), a, button, div, header_, nav, span, text)
import Halogen.HTML.Properties (attr, class_, classes, href)

import Component.Avatar (avatar)

header :: forall action cs m. ComponentHTML action cs m
header =
  header_
  [ nav
    [classes $ map ClassName
     ["navbar", "navbar-expand-md", "navbar-light", "fixed-top", "bg-light"]
    ]
    [ div [class_ (ClassName "container-fluid")]
      [ a [class_ (ClassName "navbar-brand"), href "#"]
        [text "Project Whatnot"]
      , navbarToggler
      , div [class_ $ ClassName "d-flex"]
        [ a [ classes [ClassName "nav-link", ClassName "active"]
            , attr (AttrName "aria-current") "page"
            , href "#" ]
          [ text "Boards" ]
        , avatar "IR"
        ]
      ]
    ]
  ]


navbarToggler :: forall action cs m. ComponentHTML action cs m
navbarToggler =
  button [ class_ (ClassName "navbar-toggler")
         , attr (AttrName "data-bs-toggle") "collapse"
         , attr (AttrName "data-bs-target") "#navbarCollapse"
         , attr (AttrName "aria-controls") "navbarCollapse"
         , attr (AttrName "aria-expanded") "false"
         , attr (AttrName "aria-label") "Toggle navigation"]
  [span [class_ (ClassName "navbar-toggler-icon")] []]
