{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

data Panda = Panda
    { inFormat :: InputFormats
    , outFormat :: OutputFormats
    , inText :: Textarea
    --, outText :: Maybe Textarea
    }
    deriving Show

data InputFormats = Markdown | ReStructuredText | HTML | Textile | LaTeX | DocBook
    deriving (Show, Eq, Enum, Bounded)

data OutputFormats = ToPreview | ToHTML | ToHTML5 | ToMarkdown | ToReStructuredText | ToLaTeX | ToConTeXt | ToDocBook | ToGroffMan | ToRTF | ToOpenDocumentXML | ToTexinfo | ToTextile | ToOrgmode | ToAsciidoc | ToMediaWiki
    deriving (Show, Eq, Enum, Bounded)
pandocForm :: Html -> MForm App App (FormResult Panda, Widget)
pandocForm extra = do
    -- type signature can not be omitted
    --let formats = [("Markdown", Markdown), ("LaTeX", LaTeX)] :: [(Text, PandaFormat)]
    let formats = map (pack . show &&& id ) $ [minBound..maxBound]
    let toformats = map (pack . (drop 2 . show) &&& id ) $ [minBound..maxBound]
    (inFormatRes, inFormatView) <- mreq (selectFieldList formats) "inFormat" Nothing
    (outFormatRes, outFormatView) <- mreq (selectFieldList toformats) "outFormat" Nothing
    (inRes, inView) <- mreq textareaField "inText" {fsId = Just "edit"} Nothing
    --(outRes, outView) <- mopt textareaField "outText" {fsId = Just "View"} Nothing
    let pandocRes = Panda <$> inFormatRes <*> outFormatRes <*> inRes
    let widget = do
        [whamlet|
<div>#{extra}
<p>
    From ^{fvInput inFormatView}
    To ^{fvInput outFormatView}
    <input type="submit" value="Convert">
<div .editor>
    ^{fvInput inView}
|]
    return (pandocRes, widget)

getHomeR :: Handler RepHtml
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost pandocForm
    defaultLayout $ do
        let output = Right "" :: Either Html String
        $(widgetFile "homepage")

postHomeR :: Handler RepHtml
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost pandocForm
    case result of 
        FormSuccess pandoc -> do
            let input = unTextarea $ inText pandoc
            let inputFormat = inFormat pandoc
            let outputFormat = outFormat pandoc
            -- filter out '\r', thus '\r\n' become '\n'
            let inputString = tabFilter 0 $ unpack input
            let output = processDoc inputFormat outputFormat inputString
            defaultLayout $ do
                let out = output
                $(widgetFile "homepage")
        _ -> defaultLayout $ do
            let output = Right "" :: Either Html String
            $(widgetFile "homepage")

processDoc :: InputFormats -> OutputFormats -> String -> Either Html String
processDoc inputFormat outputFormat inStr = do
    let pd = case inputFormat of
            Markdown -> readMarkdown defaultParserState inStr
            ReStructuredText -> readRST defaultParserState inStr
            HTML -> readHtml defaultParserState inStr
            Textile -> readTextile defaultParserState inStr
            LaTeX -> readLaTeX defaultParserState inStr
            DocBook -> readDocBook defaultParserState inStr
    let output = case outputFormat of
            ToPreview -> Left $ writeHtml defaultWriterOptions pd
            ToMarkdown -> Right $ writeMarkdown defaultWriterOptions pd
            ToHTML -> Right $ writeHtmlString defaultWriterOptions pd
            ToHTML5 -> Right $ writeHtmlString defaultWriterOptions {writerHtml5 = True} pd
            ToReStructuredText -> Right $ writeRST defaultWriterOptions pd
            ToLaTeX -> Right $ writeLaTeX defaultWriterOptions pd
            ToConTeXt -> Right $ writeConTeXt defaultWriterOptions pd
            ToDocBook -> Right $ writeDocbook defaultWriterOptions pd
            ToGroffMan -> Right $ writeMan defaultWriterOptions pd
            ToRTF -> Right $ writeRTF defaultWriterOptions pd
            ToOpenDocumentXML -> Right $ writeOpenDocument defaultWriterOptions pd
            ToTexinfo -> Right $ writeTexinfo defaultWriterOptions pd
            ToTextile -> Right $ writeTextile defaultWriterOptions pd
            ToOrgmode -> Right $ writeOrg defaultWriterOptions pd
            ToAsciidoc -> Right $ writeAsciiDoc defaultWriterOptions pd
            ToMediaWiki -> Right $ writeMediaWiki defaultWriterOptions pd
    output

