module Component.Header (header) where

import Prelude hiding (div)

import CSS as CSS
import Component.Avatar (avatar)
import Halogen (ComponentHTML)
import Halogen.HTML (AttrName(..), a, button, div, header_, nav, span, text)
import Halogen.HTML.Properties (attr, class_, classes, href)
import Halogen.HTML.Properties.ARIA as ARIA

header :: forall action cs m. ComponentHTML action cs m
header =
  header_
  [ nav
    [classes [ CSS.navbar, CSS.navbarExpandMd, CSS.navbarLight
             , CSS.fixedTop, CSS.bgLight ]
    ]
    [ div [class_ CSS.containerFluid]
      [ a [class_ CSS.navbarBrand, href "#"]
        [text "Project Whatnot"]
      , navbarToggler
      , div [class_ CSS.dFlex]
        [ a [ classes [CSS.navLink, CSS.active]
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
  button [ class_ CSS.navbarToggler
         , attr (AttrName "data-bs-toggle") "collapse"
         , attr (AttrName "data-bs-target") "#navbarCollapse"
         , ARIA.controls "navbarCollapse"
         , ARIA.expanded "false"
         , ARIA.label "Toggle navigation"]
  [span [class_ CSS.navbarTogglerIcon] []]
