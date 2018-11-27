module Component.UserSettings where

import Prelude hiding (div)

import Control.Monad.State.Class (class MonadState)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Globals (app')
import Halogen as H
import Halogen.HTML (HTML, div_)
-- import Halogen.HTML (HTML, br_, button, div, div_, form, input, label, p, span, table, tbody_, td, td_, text, textarea, tr_)
-- import Halogen.HTML.Events (onSubmit, onValueChange, onChecked, onClick)
-- import Halogen.HTML.Events as HE
-- import Halogen.HTML.Properties (autofocus, ButtonType(..), InputType(..), autocomplete, checked, for, id_, name, required, rows, title, type_, value)
import Model (UserSettings)
-- import Util (_curQuerystring, _loc, _lookupQueryStringValue, attr, class_)
-- import Web.Event.Event (Event, preventDefault)

data UQuery a
  = UEditField EditField a

type UState =
  { us :: UserSettings
  , edit_us :: UserSettings
  }

data EditField
  -- = Ea String
  -- | Eb String

-- | The bookmark component definition.
usetting :: UserSettings -> H.Component HTML UQuery Unit Unit Aff
usetting u' =
  H.component
    { initialState: const (mkState u')
    , render
    , eval
    , receiver: const Nothing
    }
  where
  app = app' unit

  mkState u =
    { us: u
    , edit_us: u
    }

  render :: UState -> H.ComponentHTML UQuery
  render s@{ us, edit_us } =
    div_ []

  eval :: UQuery ~> H.ComponentDSL UState UQuery Unit Aff
  eval (UEditField f next) = do
    -- modifyEdit $ case f of
    --   Ea e -> _ { url = e }
    --   Eb e -> _ { title = e }
    pure next
    where
      modifyEdit :: forall m. MonadState UState m => (UserSettings -> UserSettings) -> m Unit
      modifyEdit g = H.modify_ \s -> s { edit_us = g s.edit_us  }
