module Expression.DeepLink.View exposing (toDisplay)

import Expression.DeepLink exposing (DeepLink(..))
import Expression.HardLink as HardLink exposing (HardLink)
import Scope.View
import Shared


toDisplay : Shared.Model -> DeepLink -> String
toDisplay s deeplink =
    case deeplink of
        Null ->
            "Empty"

        Link hl dl ->
            HardLink.toString hl ++ " → " ++ toDisplay s dl

        EndPoint scope name ->
            Scope.View.toDisplay s scope ++ " → " ++ name
