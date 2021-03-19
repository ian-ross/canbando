module Canbando.Component.Header (header) where

import Prelude hiding (div)

import Canbando.CSS as CSS
import Canbando.Component.Avatar (avatar)
import Canbando.Component.Icon (icon)
import Canbando.Component.Modal (modalButton)
import Data.Maybe (Maybe(..))
import Halogen (ComponentHTML)
import Halogen.HTML (AttrName(..), a, button, div, div_, header_, nav, span, text)
import Halogen.HTML.Properties (attr, class_, classes, href)
import Halogen.HTML.Properties.ARIA as ARIA


header :: forall action cs m. Maybe String -> ComponentHTML action cs m
header name =
  header_
  [ nav
    [classes [ CSS.navbar, CSS.navbarExpandMd, CSS.navbarLight
             , CSS.fixedTop, CSS.bgLight ]
    ]
    [ div [class_ CSS.containerFluid]
      [ div [classes [CSS.w100, CSS.dFlex]] $
        case name of
          Nothing ->
            [ a [ classes [CSS.navbarBrand]
                , href "/"]
              [text "Canbando!"]
            ]
          Just n ->
            [ a [ classes [CSS.navLink, CSS.linkSecondary]
                , href "/"]
              [icon "bi-arrow-left-circle"]
            , div [ classes [CSS.navbarBrand] ]
              [text n] ]
        <> [ navbarToggler ]
        <> [ div [classes [CSS.msAuto]]
             (case name of
                 Nothing -> []
                 Just _ -> [modalButton "boardDetailsModal" "Board details..."]
              <> [ avatar "IR" ])]
      ]
    ]
  ]

  --     , div [class_ CSS.dFlex]
  --       [ a [ classes [CSS.navLink, CSS.active]
  --           , attr (AttrName "aria-current") "page"
  --           , href "/" ]
  --         [ text "Boards" ]
  --       , avatar "IR"
  --       ]
  --     ]
  --   ]
  -- ]


navbarToggler :: forall action cs m. ComponentHTML action cs m
navbarToggler =
  button [ class_ CSS.navbarToggler
         , attr (AttrName "data-bs-toggle") "collapse"
         , attr (AttrName "data-bs-target") "#navbarCollapse"
         , ARIA.controls "navbarCollapse"
         , ARIA.expanded "false"
         , ARIA.label "Toggle navigation"]
  [span [class_ CSS.navbarTogglerIcon] []]
