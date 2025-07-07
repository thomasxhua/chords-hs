module Music.Terz
    ( _T,_t,_p,_g,_P,_G
    , _Tp,_Tg,_TP,_TG
    , _tG,_tP,_tg,_tp
    ) where

import Music.Triad

_T,_t,_p,_g,_P,_G,_Tp,_Tg,_TP,_TG,_tG,_tP,_tg,_tp :: Triad -> Triad
_T = major
_t = minor
_p = minor . relative
_g = minor . leittonw
_P = major . relative
_G = major . leittonw
_Tp = _p . _T
_Tg = _g . _T
_TP = _P . _T
_TG = _G . _T
_tG = _G . _t
_tP = _P . _t
_tg = _g . _t
_tp = _p . _t

