module Happy.Backend.RAD.CodeGen where
  import Happy.Backend.RAD.Tools (XGrammar(..), NameSet(..), showItem, showProd, lhs, showRecognitionPoint, recognitionPoints, rhsAfterDot)
  import qualified Happy.Backend.RAD.Tools as RADTools
  import Happy.Grammar
  import Happy.Tabular
  import Happy.Tabular.LALR
  import Happy.Backend.RAD.StateGen
  import Control.Monad
  import Data.List
  import Data.Maybe
  import Data.Text (pack, unpack, replace)
  import GHC.Arr ((!), indices)

  data ParserType = Normal | Monad | MonadLexer deriving (Eq, Show)

  data GenOptions = GenOptions {
    ptype :: ParserType,

    wrapperType :: String, -- e.g. "Parser"
    errorTokenType :: String, -- e.g. "ErrorToken"

    showTypes :: Bool,
    comments :: Bool,

    rank2Types :: Bool, -- when True, all functions (including goto functions) which use or enclose a higher-rank-function are annotated with an explicit type.
    forallMatch :: String, -- the text which determines which types count as rank-2-types.

    rulesTupleBased :: Bool, -- actually, this doesn't produce good nor fast code. maybe drop this?

    optimize :: Bool -- inline all rule functions and eta-expand all applications of local "g" functions
  } deriving Show

  mlex opts = ptype opts == MonadLexer

  raw = flip (.) _raw

  dotleft (Lr0 rule dot) = Lr0 rule (dot-1)

  hasNT state = elem (raw radType state) [Type1, Type2]

  replaceDollar a char = maybe a ($ char) (mapDollarDollar a)

  -------------------- GENCODE --------------------
  -- Generate the full code
  genCode :: GenOptions -> XGrammar -> [RADState] -> ActionTable -> GotoTable -> [Int] -> IO String
  genCode opts x states action goto unused_rules = do
    return $ newlines 3 [nowarn, languageFeatures, header', entryPoints', definitions', rules', parseNTs', parseTerminals', states', actions', footer'] where
      nowarn = "{-# OPTIONS_GHC -w #-}"

      languageFeatures
        | rank2Types opts = newline $ map extension ["RankNTypes", "ScopedTypeVariables"]
        | otherwise = "" where
          extension str = "{-# LANGUAGE " ++ str ++ " #-}"

      g = (RADTools.g x)
      common = (RADTools.common x)
      header' = fromMaybe "" (hd x)
      entryPoints' = newlines 2 $ map (entryPoint opts x states) (starts g)
      definitions' = definitions opts x

      rules' = newlines 2 $ map (genRule opts x) $ filter (not . flip elem unused_rules) [0..prods]
      parseNTs' = newlines 2 $ catMaybes $ map (genParseNT opts g states) (non_terminals g)
      parseTerminals' = newlines 2 $ map (genParseTerminal opts x) (delete errorTok (terminals g))

      states' = newlines 2 $ map (genState opts x) states

      actions' = newlines 2 $ map (genAction opts x) [0..prods]
      prods = length (productions g) - 1
      footer' = fromMaybe "" (tl x)


  -------------------- ENTRYPOINT --------------------
  entryPoint :: GenOptions -> XGrammar -> [RADState] -> (String, Name, Name, Bool) -> String
  entryPoint opts x@XGrammar { RADTools.g = g, RADTools.common = common_ } states (name, lhs, rhs, isPartial) = newline [typedecl, definition] where
    typedecl
      | showTypes opts = fromMaybe "" $ fmap (((name ++ " :: ") ++) . correctP) (symboltype opts x rhs)
      | otherwise = ""

    correctP = if mlex opts then p else parser

    definition = case ptype opts of
      Normal -> common ++ paren (checkEof ++ "const")
      Monad -> common ++ paren (checkEof ++ "const . " ++ returnP)
      MonadLexer -> common ++ paren (checkEof ++ "const . " ++ returnP) ++ " Nothing"

    checkEof
      | isPartial = ""
      | otherwise = "parse" ++ show (eof_term g) ++ " . "

    common = name ++ " = rule" ++ show prod ++ " "

    -- Rule LHS -> RHS
    prod = fromJust $ find matches [0 .. length (productions g) - 1] where
      matches i = matches' (lookupProdNo g i)
      matches' (Production lhs' rhs' _ _) = lhs' == lhs && rhs' == [rhs]

    p a = p' ++ " " ++ a
    parser a = wrapperType opts ++ " " ++ a
    (_, _, p', _, returnP) = monad common_


  -------------------- DEFINITIONS --------------------
  -- Generate definitions such as wrappers, the parser type or more required functions and types
  definitions :: GenOptions -> XGrammar -> String
  definitions opts x@XGrammar { RADTools.g = g, RADTools.common = common } = case ptype opts of
    Normal -> newlines 2 [parserDecl, errorToken]
    Monad -> newlines 2 [parserDecl, errorToken, wrapThen]
    MonadLexer -> newlines 2 [parserDecl, errorToken, wrapThen, repeatTok, wrapLexer, wrapError]
    where

      -- type Parser r = [Token] -> P r
      parserDecl = case ptype opts of
        Normal -> "type " ++ parser "r" ++ " = [" ++ tokenT ++ "] -> r"
        Monad -> "type " ++ parser "r" ++ " = [" ++ tokenT ++ "] -> " ++ p "r"
        MonadLexer -> "type " ++ parser "r" ++ " = Maybe " ++ paren tokenT ++ " -> " ++ p "r"

      -- data ErrorToken = ErrorToken
      errorToken = "data " ++ errorTokenT ++ " = " ++ errorTokenT

      -- thenWrapP :: P a -> (a -> Parser b) -> Parser b
      -- thenWrapP a f ts = (thenP) a (flip f ts)
      wrapThen = newline [typedecl, definition] where
        name = "thenWrapP"
        typedecl = name ++ " :: " ++ p "a" ++ " -> (a -> " ++ parser "b" ++ ") -> " ++ parser "b"
        definition = name ++ " a f ts = " ++ paren thenP ++ " a (flip f ts)"

      -- repeatTok :: Token -> Parser a -> Parser a
      -- repeatTok tok p _ = p (Just tok)
      repeatTok = newline [typedecl, definition] where
        name = "repeatTok"
        typedecl = name ++ " :: " ++ tokenT ++ " -> " ++ parser "a" ++ " -> " ++ parser "a"
        definition = name ++ " tok p _ = p (Just tok)"

      -- lexerWrapper :: (Token -> Parser a) -> Parser a
      -- lexerWrapper cont Nothing = lexer (\tok -> cont tok Nothing)
      -- lexerWrapper cont (Just tok) = cont tok Nothing
      wrapLexer = newline [typedecl, line1, line2] where
        name = "lexerWrapper"
        typedecl = name ++ " :: " ++ paren (tokenT ++ " -> " ++ parser "a") ++ " -> " ++ parser "a"
        line1 = name ++ " cont Nothing = " ++ lexer' ++ " (\\t -> cont t Nothing)"
        line2 = name ++ " cont (Just tok) = cont tok Nothing"

      -- happyErrorWrapper :: Token -> Parser a
      -- happyErrorWrapper _ _ = happyError, or
      -- happyErrorWrapper t _ = happyError t
      wrapError = newline [typedecl, definition] where
        name = "happyErrorWrapper"
        typedecl = name ++ " :: " ++ tokenT ++ " -> " ++ parser "a"
        definition
          | errorHasTokenInput = name ++ " = const . " ++ happyError
          | otherwise = name ++ " _ _ = " ++ happyError

      p a = p' ++ " " ++ a
      parser a = wrapperType opts ++ " " ++ a
      (_, _, p', thenP, _) = monad common
      tokenT = token_type common
      errorTokenT = errorTokenType opts
      (Just (lexer', _)) = lexer common
      happyError = fromMaybe "happyError" (error_handler common)
      -- When an %error directive is present, the type of happyError is `Token -> P a`, else it is `P a`
      errorHasTokenInput = not (error_handler common == Nothing)


  -------------------- GENSTATE -------------------
  -- Generate the code for a single state.
  genState :: GenOptions -> XGrammar -> RADState -> String
  genState opts x@XGrammar { RADTools.g = g, RADTools.common = common } state
    | isTrivialAccept = newline [comment, trivialTypedecl, trivialAcceptHeader]
    | isTrivialAnnounce = newline [comment, trivialTypedecl, trivialAnnounceHeader]
    | otherwise = newline [comment, typedecl, header, shifts'', announces'', fails'', accepts'', defaultAction'', gotos''] where

    hasNoActions = (null $ shifts' state) && (null $ accepts' state) && (null $ announces' state) && (null $ fails' state) && length (artCore state) == 1
    hasNoGotos = (null $ gotos' state)
    isTrivialAccept = hasNoActions && hasNoGotos && (defaultAction' state == Accept')
    isTrivialAnnounce = isAlwaysAnnounce && hasNoGotos
    isAlwaysAnnounce = case defaultAction' state of
      Announce' _ -> hasNoActions
      _ -> False

    hasRank2Goto = any ((hasRank2Type opts x) . fst) (gotos' state)
    hasRank2TypeSignature = any (hasRank2Item) (artCore state)
    hasRank2Item item = any (hasRank2Type opts x) (rhsAfterDot g item)

    trivialTypedecl
      | rank2Types opts && hasRank2Goto = fromMaybe "" (stateTypeSignature opts x True state)
      | showTypes opts = fromMaybe "" (stateTypeSignature opts x False state)
      | otherwise = ""

    trivialAcceptHeader = "state" ++ show (raw i state) ++ " = id"

    trivialAnnounceHeader = "state" ++ show (raw i state) ++ " = rule" ++ show rule where
      Announce' rule = defaultAction' state

    comment
      | comments opts = newlineMap "-- " (showItem g) (artCore state)
      | otherwise = ""

    typedecl
      | rank2Types opts && hasRank2Goto = fromMaybe "" (stateTypeSignature opts x True state)
      | rank2Types opts && hasRank2TypeSignature = fromMaybe "" (stateTypeSignature opts x False state)
      | showTypes opts = fromMaybe "" (stateTypeSignature opts x False state)
      | otherwise = ""

    shifts'' = newlineMap "  " shift (shifts' state)
    announces'' = newlineMap "  " announce (announces' state)
    fails'' = newlineMap "  " fail (fails' state)
    accepts'' = newlineMap "  " accept (accepts' state)
    gotos'' = where' ++ intercalate "\n" (map ("    " ++) lines) where
      lines = join (map goto (gotos' state))
      where' = if null (gotos' state) then "" else "  where\n"

    header
      | mlex opts = common ++ " = lexerWrapper $ \\t -> case t of"
      | otherwise = common ++ " ts = case ts of" where
        common = "state" ++ show (raw i state) ++ " " ++ headerKs
        headerKs = unwords $ map (kNoEta "") (artCore state)

    shift (token, (state, i))
      | mlex opts = paren tok ++ " -> state" ++ show state ++ " " ++ kcontent
      | otherwise = "t@" ++ paren tok ++ ":tr -> state" ++ show state ++ " " ++ kcontent ++ " tr" where
        i' = map dotleft i
        tok = replaceDollar rawToken (if wantsProjection then "v" else "_")
        kcontent = unwords (map (paren . k x) i') where
          x = if wantsProjection then " v" else " t"
        rawToken = fromJust $ lookup token (token_specs g)
        wantsProjection = "$$" == (rawToken \\ replaceDollar rawToken "") -- i.e. Tokens of form "TokenInt $$"

    announce (token, rule)
      | mlex opts = paren tokMaybeEof ++ " -> repeatTok t $ rule" ++ show rule ++ " " ++ paren (k "" item)
      | otherwise = if token == eof_term g then eofCase else normalCase
      where
        normalCase = paren tok ++ ":tr -> rule" ++ show rule ++ " " ++ paren (k "" item) ++ " ts"
        eofCase = "[] -> rule" ++ show rule ++ " " ++ paren (k "" item) ++ " ts"

        tokMaybeEof = if token == eof_term g then eof else tok
        Just (_, eof) = lexer common

        item = fromJust $ find matches (raw completion' state) where -- the item in the completion corresponding to (i.e. of the) rule which is announced. The dot must be at the recognition point.
          matches (Lr0 rule' dot) = rule == rule' && (recognitionPoints x) !! rule == dot
        tok = replaceDollar rawToken "_"
        rawToken = fromJust $ lookup token (token_specs g)

    accept token
      | mlex opts = paren tokMaybeEof ++ " -> repeatTok t $ " ++ k'
      | otherwise = if token == eof_term g then eofCase else normalCase
      where
        normalCase = "t@" ++ paren tok ++ ":tr -> " ++ k' ++ " ts"
        eofCase = "[] -> " ++ k' ++ " ts"

        tokMaybeEof = if token == eof_term g then eof else tok
        tok = removeDollar $ fromJust (lookup token (token_specs g))
        Just (_, eof) = lexer common

        removeDollar a = maybe a ($ "_") (mapDollarDollar a)
        k' = k "" (head (artCore state))

    fail token
      | mlex opts = paren tok ++ " -> happyErrorWrapper t"
      | otherwise = "t@" ++ paren tok ++ ":_ -> " ++ happyError ++ " ts"
      where
        tok = removeDollar $ fromJust (lookup token (token_specs g))
        removeDollar a = maybe a ($ "_") (mapDollarDollar a)
        happyError = fromMaybe "happyError" (error_handler common)

    goto (nt, (state, i))
      | hasRank2Type opts x nt = catMaybes [gototype, goto]
      | otherwise = catMaybes [goto]
      where
        i' = map dotleft i
        gototype = case symboltype opts x nt of
          Just t -> Just $ "g" ++ show nt ++ " :: " ++ t ++ " -> " ++ paren outtype
          Nothing -> Nothing
        goto
          | optimize opts = Just $ "g" ++ show nt ++ " x " ++ ts ++ " = state" ++ show state ++ " " ++ unwords (map (paren . k "x") i') ++ " " ++ ts
          | otherwise = Just $ "g" ++ show nt ++ " x = state" ++ show state ++ " " ++ unwords (map (paren . k "x") i')
        outtype = wrapperType opts ++ " r"
        ts = if mlex opts then "la" else "ts"

    defaultAction'' = "  " ++ case defaultAction' state of
      ErrorShift' state -> defaultErrorShift state
      Announce' rule -> defaultAnnounce rule
      Accept' -> defaultAccept
      Error' -> defaultError

    defaultErrorShift toState
      | mlex opts = "_ -> repeatTok t $ state" ++ show toState ++ " " ++ paren (k "ErrorToken" item)
      | otherwise = "_ -> state" ++ show toState ++ " " ++ paren (k "ErrorToken" item) ++ " ts" where
        item = head $ hdiv (raw completion' state) errorTok g

    defaultAnnounce rule
      | mlex opts = "_ -> repeatTok t $ rule" ++ show rule ++ " " ++ paren (k "" item)
      | otherwise = "_ -> rule" ++ show rule ++ " " ++ paren (k "" item) ++ " ts" where
        item = fromJust $ find matches (raw completion' state) where -- the item in the completion corresponding to (i.e. of the) rule which is announced. The dot must be at the recognition point.
          matches (Lr0 rule' dot) = rule == rule' && (recognitionPoints x) !! rule == dot

    defaultAccept
      | mlex opts = "_ -> repeatTok t $ " ++ k'
      | otherwise = "_ -> " ++ k' ++ " ts" where
        k' = k "" (head (artCore state))

    defaultError
      | mlex opts = "_ -> happyErrorWrapper t"
      | otherwise = "_ -> " ++ happyError ++ " ts" where
      happyError = fromMaybe "happyError" (error_handler common)

    k = if optimize opts then kEta else kNoEta

    -- Produce "k1 x" or "action5 g4 x", or without x:
    -- "k1" or "action5 g4"
    kNoEta x item@(Lr0 rule dot) = maybe noCore core $ elemIndex item (artCore state) where
      core idx
        | (length (artCore state) == 1) = "k " ++ x
        | otherwise = "k" ++ (show (idx + 1)) ++ " " ++ x
      noCore = "action" ++ show rule ++ " g" ++ show (lhs g item) ++ " " ++ x

    -- Produce "\z -> k1 x z" or "action5 (\y z -> g4 y z) x", or without x:
    -- "\z -> k1 z" or "action5 (\y z -> g4 y z)"
    kEta x item@(Lr0 rule dot) = maybe noCore core $ elemIndex item (artCore state) where
      core idx
        | (length (artCore state) == 1) = if kArity idx == 0 then "k " ++ x else "\\z -> k " ++ x ++ " z"
        | otherwise = if kArity idx == 0 then "k" ++ (show (idx + 1)) ++ " " ++ x else "\\z -> k" ++ (show (idx + 1)) ++ " " ++ x ++ " z"
      noCore = "action" ++ show rule ++ " (\\y z -> g" ++ show (lhs g item) ++ " y z) " ++ x
      kArity idx = kArity' idx - (if null x then 0 else 1)
      kArity' idx = length $ rhsAfterDot g ((artCore state) !! idx)

   -- Create the type signature for a state.
  stateTypeSignature :: GenOptions -> XGrammar -> Bool -> RADState -> Maybe String
  stateTypeSignature opts x forall_r state = do
    let start = "state" ++ show (raw i state) ++ " :: " ++ forall_pref
    components <- mapM component (artCore state)
    return $ start ++ intercalate " -> " (map paren (components ++ [outtype]))
    where
      component item@(Lr0 rule dot)
        | rule < 0 = if dot == 0 then component' [-rule] else component' [] -- artifical NT
        | rule >= 0 = component' (rhsAfterDot (g x) item)
      component' rhs = fmap (intercalate " -> " . (++ [outtype])) (mapM (symboltype opts x) rhs)
      outtype = wrapperType opts ++ " r"
      forall_pref = if forall_r then "forall r. " else ""


  -------------------- GENACTION --------------------
  -- Create the code for a semantic action, i.e. a reduce action.
  genAction :: GenOptions -> XGrammar -> Int -> String
  genAction opts x@XGrammar { RADTools.g = g } i = newline [comment, typedecl, code] where
    prod@(Production lhs' rhs' _ _) = lookupProdNo g i

    comment
      | comments opts = "-- " ++ showProd g i
      | otherwise = ""

    typedecl
      | showTypes opts || rank2Types opts = typedecl' -- some actions (not further specified) need to be explicitly typed in order for rank-n-types to work
      | otherwise = ""
      where
        typedecl' = fromMaybe "" $ fmap (("action" ++ show i ++ " :: ") ++) (actionTypedecl opts x i)

    code = header ++ (if isMonadic then monadicCode else normalCode)
    (customCode, isMonadic) = customProdCode prod
    header = "action" ++ show i ++ " g " ++ unwords (map v [1..length rhs']) ++ " = "
    normalCode = "g " ++ paren customCode
    monadicCode = paren customCode ++ " `thenWrapP` g"
    v n = "v" ++ show n

  -- Generate the type signature of a semantic action function.
  actionTypedecl :: GenOptions -> XGrammar -> Int -> Maybe String
  actionTypedecl opts x i = do
    lhstype <- symboltype opts x lhs'
    let lhs = paren $ intercalate " -> " $ [lhstype, outtype]
    rhstypes <- mapM (symboltype opts x) rhs'
    let rhs = intercalate " -> " $ rhstypes ++ [outtype]
    return (lhs ++ " -> " ++ rhs)
    where
      Production lhs' rhs' _ _ = lookupProdNo (g x) i
      outtype = wrapperType opts ++ " r"

  -- Read and translate the raw action code supplied by the user. Also return whether the action is monadic or not.
  customProdCode :: Production -> (String, Bool)
  customProdCode (Production _ _ (code, _) _) = case code of
    '%':'%':_ -> error "'{%%' actions not supported"
    '%':'^':_ -> error "'{%^' actions not supported"
    '%':rest -> (adapt rest, True)
    _ -> (adapt code, False)
    where
      adapt code
        | code == "no code" = v 1
        | otherwise = replaceHappyVars code
      v n = "v" ++ show n
      replaceHappyVars = unpack . replace (pack "happy_var_") (pack "v") . pack


  -------------------- PARSETERMINALS / PARSENTS -------------------
  -- Generate the code for parsing a single nonterminal.
  genParseNT :: GenOptions -> Grammar -> [RADState] -> Int -> Maybe String
  genParseNT opts g states token = do
    state <- find (\s -> (raw radType s == Type1 && raw nt s == token)) states
    let line = "parse" ++ show token ++ " = state" ++ show (raw i state)
    return (newline [comment, line]) where
      comment
        | comments opts = "-- " ++ (token_names g) ! token
        | otherwise = ""


  -- Generate the code for parsing a single terminal.
  genParseTerminal :: GenOptions -> XGrammar -> Int -> String
  genParseTerminal opts x@XGrammar { RADTools.g = g, RADTools.common = common } token = newline [comment, typedecl, code] where
    specialEof = ptype opts /= MonadLexer && token == eof_term g

    comment
      | comments opts = "-- " ++ (token_names g) ! token
      | otherwise = ""

    typedecl
      | specialEof && (showTypes opts || rank2Types opts) = typedecl''
      | showTypes opts || rank2Types opts = typedecl'
      | otherwise = ""
      where
        typedecl' = maybe "" (\token' -> "parse" ++ show token ++ " :: " ++ paren (token' ++ " -> " ++ parser) ++ " -> " ++ parser) token'
        typedecl'' = "parse" ++ show token ++ " :: " ++ parser ++ " -> " ++ parser
        token' = symboltype opts x token
        parser = wrapperType opts ++ " r"

    code
      | specialEof = newline $ [lineEof1, line2]
      | mlex opts = newline $ [lineLex1, lineLex2, lineLex3]
      | otherwise = newline $ [line1, line2]
      where
      lineEof1 = "parse" ++ show token ++ " k [] = k []"
      line1 = "parse" ++ show token ++ " k (t@" ++ paren tok ++ ":tr) = k " ++ t ++ " tr"
      line2 = "parse" ++ show token ++ " k ts = " ++ happyError ++ " ts"
      happyError = fromMaybe "happyError" (error_handler common)

      rawToken = fromJust $ lookup token (token_specs g)
      tok = replaceDollar rawToken (if wantsProjection then "v" else "_")
      t = if wantsProjection then "v" else "t"
      wantsProjection = "$$" == (rawToken \\ replaceDollar rawToken "") -- i.e. Tokens of form "TokenInt $$"

      lineLex1 = "parse" ++ show token ++ " k = lexerWrapper $ \\t -> case t of"

      lineLex2
        | token == eof_term g = "  " ++ paren eof ++ " -> k"
        | otherwise = "  " ++ paren tok ++ " -> k " ++ t
        where
        Just (_, eof) = lexer common

      lineLex3 = "  _ -> happyErrorWrapper t"


  -------------------- GENRULE -------------------
  -- Generate the code for a rule.
  genRule :: GenOptions -> XGrammar -> Int -> String
  genRule opts x rule
    | isTrivial = newline [inline, comment, typedecl, trivialCode]
    | otherwise = newline [inline, comment, typedecl, code]
    where

    recog = (recognitionPoints x) !! rule
    rhsAfterDot' = rhsAfterDot (RADTools.g x) (Lr0 rule recog)
    isTrivial = length rhsAfterDot' <= 1

    inline
      | optimize opts = "{-# INLINE rule" ++ show rule ++ " #-}"
      | otherwise = ""

    comment
      | comments opts = "-- " ++ showRecognitionPoint x rule
      | otherwise = ""

    typedecl
      | showTypes opts || rank2Types opts = typedecl'
      | otherwise = ""
      where
        typedecl' = fromMaybe "" $ fmap (("rule" ++ show rule ++ " :: ") ++) (ruleTypedecl opts x (rank2Types opts) rule)

    code = case (rulesTupleBased opts, ptype opts) of
      (True, Normal) -> tupleBasedCodeNormal
      (True, Monad) -> error "TODO: %monad without %lexer not yet supported"
      (True, MonadLexer) -> tupleBasedCodeLexer
      (False, Normal) -> continuationBasedCodeNormal
      (False, Monad) -> error "TODO: %monad without %lexer not yet supported"
      (False, MonadLexer) -> continuationBasedCodeLexer

    -- There are 3 types how we can generate the code:
    -- 1. trivial: 0 or 1 symbols are parsed
    -- 2. continuation-based, with optional type annotations for the continuations
    -- 3. tuple-based

    trivialCode = case rhsAfterDot' of
      [x] -> "rule" ++ show rule ++ " = parse" ++ show x
      [] -> "rule" ++ show rule ++ " = id"

    tupleBasedCodeNormal = newline $ firstLine:otherLines where
      firstLine = "rule" ++ show rule ++ " k ts0 = " ++ fullk ++ " where"
      fullk = "k " ++ (unwords $ map (\x -> "v" ++ show x) [1..length otherLines]) ++ " ts" ++ show (length otherLines)
      otherLines = map (uncurry toLine) (zip rhsAfterDot' [1..])
      toLine tok i = "  (v" ++ show i ++ ", ts" ++ show i ++ ") = parse" ++ show tok ++ " (,) ts" ++ show (i-1)

    tupleBasedCodeLexer = newline $ firstLine : otherLines ++ [finalLine] where
      firstLine = "rule" ++ show rule ++ " k la0 = do"
      fullk = "k " ++ (unwords $ map (\x -> "v" ++ show x) [1..length otherLines]) ++ " la" ++ show (length otherLines)
      otherLines = map (uncurry toLine) (zip rhsAfterDot' [1..])
      toLine tok i = "  (v" ++ show i ++ ", la" ++ show i ++ ") <- parse" ++ show tok ++ " (\\a b -> return (a, b)) la" ++ show (i-1)
      finalLine = "  " ++ fullk

    continuationBasedCodeNormal = continuationBasedCode "ts"
    continuationBasedCodeLexer = continuationBasedCode "la"
    continuationBasedCode ts
      | rank2Types opts = newline $ firstLine : (blend lineTypes (otherLines ++ [finalLine]))
      | otherwise = newline $ firstLine : (otherLines ++ [finalLine])
      where
      firstLine = "rule" ++ show rule ++ " k " ++ ts ++ " = parse" ++ show (head rhsAfterDot') ++ " cont1 " ++ ts ++ " where"
      otherLines = map (uncurry toLine) (zip (tail rhsAfterDot') [1..])
      toLine tok i = "  cont" ++ show i ++ " " ++ vs i ++ " " ++ ts ++ " = parse" ++ show tok ++ " " ++ paren ("cont" ++ show (i+1) ++ " " ++ vs i) ++ " " ++ ts
      vs i = unwords (map (\v -> "v" ++ show v) [1..i])
      finalLine = "  cont" ++ show n ++ " " ++ vs n ++ " " ++ ts ++ " = k " ++ vs n ++ " " ++ ts where
      n = length rhsAfterDot'

      lineTypes = map toType [1..n]
      toType i = fromMaybe "" (toType' i)
      toType' i = do
        lhs <- mapM (symboltype opts x) (take i rhsAfterDot')
        let lhsType = intercalate " -> " (lhs ++ [parser])
        return $ "  cont" ++ show i ++ " :: " ++ lhsType where
          parser = paren $ wrapperType opts ++ " r"

      blend (x:xs) ys = x:(blend ys xs)
      blend _ _ = []

  -- Generate the type signature of a rule function.
  ruleTypedecl :: GenOptions -> XGrammar -> Bool -> Int -> Maybe String
  ruleTypedecl opts x forall_r rule = do
    let g = RADTools.g x
    let recog = (recognitionPoints x) !! rule
    let lhs' = rhsAfterDot g (Lr0 rule recog)
    lhstypes <- mapM (symboltype opts x) lhs'
    let lhs = forall_pref ++ (paren $ intercalate " -> " $ lhstypes ++ [parser])
    return (lhs ++ " -> " ++ parser)
    where
      forall_pref = if forall_r then "forall r. " else ""
      parser = paren $ wrapperType opts ++ " r"


  -------------------- TOOLS --------------------

  -- Insert newlines between the strings; ignore empty strings
  newlines :: Int -> [String] -> String
  newlines n = intercalate (replicate n '\n') . filter (not . null)

  newline = newlines 1

  newlineMap prefix f x = newlines 1 $ map ((prefix ++) . f) x

  paren a = "(" ++ a ++ ")"

  hasRank2Type opts x nt = rank2Types opts && case symboltype opts x nt of
    Just t -> isInfixOf (forallMatch opts) t
    Nothing -> False

  symboltype opts x symbol
    | symbol == errorTok = Just (process $ errorTokenType opts)
    | symbol == (eof_term (g x)) = Nothing
    | elem symbol (non_terminals (g x)) = fmap process $ join (maybelookup (types (g x)) symbol)
    | wantsProjection = Nothing -- we don't know the type of the projection
    | otherwise = Just (process $ token_type (common x))
    where
    process = remNewlines . paren where
      remNewlines = map replace
      replace '\n' = ' '
      replace x = x
    maybelookup arr i = if elem i (indices arr) then Just (arr ! i) else Nothing
    wantsProjection = "$$" == (rawToken \\ replaceDollar rawToken "") -- i.e. Tokens of form "TokenInt $$"
    rawToken = fromJust $ lookup symbol (token_specs (g x))
