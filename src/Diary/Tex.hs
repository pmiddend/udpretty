{-# LANGUAGE OverloadedStrings #-}

module Diary.Tex where

import Data.Text (Text)
import Text.LaTeX.Base (protectText, raw)
import Text.LaTeX.Base.Class (LaTeXC, comm0, liftL, liftL2)
import Text.LaTeX.Base.Render (render)
import Text.LaTeX.Base.Syntax
  ( LaTeX(TeXComm, TeXEnv, TeXRaw)
  , Measure
  , TeXArg(FixArg, OptArg)
  )
import Text.LaTeX.Base.Types (PackageName)

realNewline :: LaTeXC l => l
realNewline = raw "\n"

figure :: LaTeXC l => l -> l
figure = liftL $ TeXEnv "figure" [OptArg $ TeXRaw "H"]

sansSerifFont :: LaTeXC l => l
sansSerifFont = raw "\\renewcommand{\\familydefault}{\\sfdefault}"

captionSetupNoLabel :: LaTeXC l => l
captionSetupNoLabel = raw "\\captionsetup{labelformat=empty}"

subfigure :: LaTeXC l => Measure -> l -> l
subfigure m =
  liftL $ TeXEnv "subfigure" [OptArg (TeXRaw "t"), FixArg (TeXRaw (render m))]

declareUnicodeCharacter :: LaTeXC l => l -> l -> l
declareUnicodeCharacter =
  liftL2 $ \p q -> TeXComm "DeclareUnicodeCharacter" [FixArg p, FixArg q]

textcomp :: PackageName
textcomp = "textcomp"

subcaption :: PackageName
subcaption = "subcaption"

textdegree :: LaTeXC l => l
textdegree = comm0 "textdegree"

texteuro :: LaTeXC l => l
texteuro = comm0 "texteuro"

heart :: LaTeXC l => l
heart = raw "\\ensuremath\\heartsuit"

protected :: LaTeXC l => Text -> l
protected = raw . protectText
