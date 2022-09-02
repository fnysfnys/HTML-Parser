import System.IO

-- list of valid tag types given in the spec
validTypes = ["html", "head", "body", "title", "h1", "h2", "h3", "p", "ul", "li", "a", "div", "br", "hr"]

-- token used to represent unrecognised tag.
-- allows for better readability of code.
invalidToken = ("INVALID", False)

---LEXING---

-- extracts a list of tags from a string of HTML. 
-- boolean value indicates a tag is being read.
-- takes all values inbetween and including a '<' and a '>'
-- if a '<' is found whilst the bool is True the tag is invalid.
-- if a '>' is found whilst the bool is False the tag is invalid.
getTags :: String -> Bool -> [String] -> String -> [String]
getTags [] False tags currentTag = reverse tags
getTags (c:cs) False tags currentTag | c == '<' = getTags cs True tags (c:currentTag)
                                     | c == '>' = ["INVALID"]
                                     | otherwise = getTags cs False tags []

getTags (c:cs) True tags currentTag | c == '>' = getTags cs False ((reverse (c:currentTag)):tags) []
                                    | c == '<' = ["INVALID"]
                                    | otherwise = getTags cs True tags (c:currentTag)


-- extracts the type from a tag following the '<' or "</":
-- ...div class="foo"> -> div
-- ...html> -> html                                   
extractType :: String -> String -> String                                    
extractType [] extractedType = "INVALID"
extractType (' ':cs) extractedType = reverse extractedType
extractType ('>':cs) extractedType = reverse extractedType  
extractType (c:cs) extractedType = extractType cs (c:extractedType)

-- converts a tag to a token of the form (Type, openTag)
-- </html> -> ("html", False)
-- <div class="foo"> -> ("div", True)
tokenise :: String -> (String, Bool)
tokenise ('<' : '/' : cs) | extractedType `elem` ([x | x <- validTypes, x /= "br" && x/= "hr"]) = (extractedType, False)
                          | otherwise = invalidToken
                            where extractedType = extractType cs []

tokenise ('<' : cs) | extractedType `elem` validTypes = (extractedType, True)
                    | otherwise = invalidToken
                      where extractedType = extractType cs []

tokenise(c:cs) = invalidToken

---PARSING---

push :: [(String, Bool)] -> [(String, Bool)] -> String
push (token:tokens) stack | tokenType == "hr" = parseTokens tokens stack --continue
                          | tokenType == "br" = parseTokens tokens stack --continue
                          | tokenType == "div" && (("p", True) `elem` stack) = "INVALID: A <div> tag cannot be nested inside a <p> tag."
                          | tokenType == "p" && (("p", True) `elem` stack) = "INVALID: A <p> tag cannot be nested inside a <p> tag."
                          | tokenType == "title" && not ((("head", True) `elem` stack)) = "INVALID: The <title> tag must be included in the section between the head tags only, and not in the body section between the body tags."
                          | otherwise = parseTokens tokens (token:stack) --pushes head of token list to the stack
                            where tokenType = fst token


-- pops the top from the stack, compares its type with the head of the tokens list
pop :: [(String, Bool)] -> [(String, Bool)] -> String                            
pop (token:tokens) (top:stack) | tokenType == topOfStackType = parseTokens tokens stack
                               | otherwise = "Invalid nesting of </" ++ tokenType ++ ">"
                                 where tokenType = fst token
                                       topOfStackType = fst top


-- pushes open tags and pops close tags.                                       
parseTokens :: [(String, Bool)] -> [(String, Bool)] -> String
parseTokens [] [] = "This is valid HTML"
parseTokens [] (top:stack) = "no closing tag for <" ++ fst top ++ ">"

parseTokens (token:tokens) stack | openTag = push (token:tokens) stack
                                 | otherwise = pop (token:tokens) stack
                                   where openTag = snd token


htmlAroundAll :: [(String, Bool)] -> Bool
htmlAroundAll tokens | (((fst (head tokens)) == "html") && ((fst (last tokens)) == "html")) = True
                     | otherwise = False


onlyOneHtml :: [(String, Bool)] -> Bool
onlyOneHtml tokens = [x | x <- tokens, x == ("html", True) || x == ("html", False)] == [("html", True), ("html", False)]


headFollowedByBody :: [(String, Bool)] -> Bool
headFollowedByBody tokens = [x | x <- tokens, x == ("head", True) || x == ("head", False) || x == ("body", True) || x == ("body", False)] == [("head",True),("head",False),("body",True),("body",False)]
    

main = do
    theFile <- openFile "file.html" ReadMode
    contents <- hGetContents theFile
    let tags = getTags contents False [] []
    let tokens = map tokenise tags
    if(tags==["INVALID"]) then (print "INVALID: Invalid use of angle brackets. Check for incorrect placement of '<' or '>'.")
    else if (invalidToken `elem` tokens) then (print "INVALID: HTML file contains a non recognised tag.")
    else if (htmlAroundAll tokens == False) then (print "INVALID: Entire document must be wrapped in html tags.")
    else if (onlyOneHtml tokens == False) then (print "INVALID: There can not be more than one <html>...</html> section.")
    else if (headFollowedByBody tokens == False) then (print "INVALID: must be a single <head>...</head> section followed by a single <body>...</body> section in that order.")
    else (putStrLn (parseTokens tokens []))