{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Dict.Par where
import Dict.Abs
import Dict.Lex
import Dict.ErrM

-- parser produced by Happy Version 1.18.6

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn10 (Ident)
	| HappyAbsSyn11 (Integer)
	| HappyAbsSyn12 (String)
	| HappyAbsSyn13 (Dictionary)
	| HappyAbsSyn14 ([Entry])
	| HappyAbsSyn15 ([Arg])
	| HappyAbsSyn16 ([Term])
	| HappyAbsSyn17 (Entry)
	| HappyAbsSyn18 (Term)
	| HappyAbsSyn19 (Arg)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37 :: () => Int -> ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24 :: () => ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

action_0 (13) = happyGoto action_24
action_0 (14) = happyGoto action_25
action_0 _ = happyReduce_11

action_1 (14) = happyGoto action_23
action_1 _ = happyReduce_11

action_2 (15) = happyGoto action_22
action_2 _ = happyReduce_13

action_3 (26) = happyShift action_8
action_3 (27) = happyShift action_13
action_3 (28) = happyShift action_14
action_3 (10) = happyGoto action_15
action_3 (11) = happyGoto action_10
action_3 (12) = happyGoto action_11
action_3 (16) = happyGoto action_20
action_3 (18) = happyGoto action_21
action_3 (19) = happyGoto action_17
action_3 _ = happyReduce_15

action_4 (26) = happyShift action_8
action_4 (10) = happyGoto action_18
action_4 (17) = happyGoto action_19
action_4 _ = happyFail

action_5 (26) = happyShift action_8
action_5 (27) = happyShift action_13
action_5 (28) = happyShift action_14
action_5 (10) = happyGoto action_15
action_5 (11) = happyGoto action_10
action_5 (12) = happyGoto action_11
action_5 (18) = happyGoto action_16
action_5 (19) = happyGoto action_17
action_5 _ = happyFail

action_6 (26) = happyShift action_8
action_6 (27) = happyShift action_13
action_6 (28) = happyShift action_14
action_6 (10) = happyGoto action_9
action_6 (11) = happyGoto action_10
action_6 (12) = happyGoto action_11
action_6 (19) = happyGoto action_12
action_6 _ = happyFail

action_7 (26) = happyShift action_8
action_7 _ = happyFail

action_8 _ = happyReduce_7

action_9 _ = happyReduce_22

action_10 _ = happyReduce_23

action_11 _ = happyReduce_24

action_12 (30) = happyAccept
action_12 _ = happyFail

action_13 _ = happyReduce_8

action_14 _ = happyReduce_9

action_15 (24) = happyShift action_30
action_15 _ = happyReduce_22

action_16 (30) = happyAccept
action_16 _ = happyFail

action_17 _ = happyReduce_21

action_18 (15) = happyGoto action_29
action_18 _ = happyReduce_13

action_19 (30) = happyAccept
action_19 _ = happyFail

action_20 (30) = happyAccept
action_20 _ = happyFail

action_21 (21) = happyShift action_28
action_21 _ = happyReduce_16

action_22 (26) = happyShift action_8
action_22 (27) = happyShift action_13
action_22 (28) = happyShift action_14
action_22 (30) = happyAccept
action_22 (10) = happyGoto action_9
action_22 (11) = happyGoto action_10
action_22 (12) = happyGoto action_11
action_22 (19) = happyGoto action_27
action_22 _ = happyFail

action_23 (26) = happyShift action_8
action_23 (30) = happyAccept
action_23 (10) = happyGoto action_18
action_23 (17) = happyGoto action_26
action_23 _ = happyFail

action_24 (30) = happyAccept
action_24 _ = happyFail

action_25 (26) = happyShift action_8
action_25 (10) = happyGoto action_18
action_25 (17) = happyGoto action_26
action_25 _ = happyReduce_10

action_26 (20) = happyShift action_34
action_26 _ = happyFail

action_27 _ = happyReduce_14

action_28 (26) = happyShift action_8
action_28 (27) = happyShift action_13
action_28 (28) = happyShift action_14
action_28 (10) = happyGoto action_15
action_28 (11) = happyGoto action_10
action_28 (12) = happyGoto action_11
action_28 (16) = happyGoto action_33
action_28 (18) = happyGoto action_21
action_28 (19) = happyGoto action_17
action_28 _ = happyReduce_15

action_29 (22) = happyShift action_32
action_29 (26) = happyShift action_8
action_29 (27) = happyShift action_13
action_29 (28) = happyShift action_14
action_29 (10) = happyGoto action_9
action_29 (11) = happyGoto action_10
action_29 (12) = happyGoto action_11
action_29 (19) = happyGoto action_27
action_29 _ = happyReduce_18

action_30 (26) = happyShift action_8
action_30 (27) = happyShift action_13
action_30 (28) = happyShift action_14
action_30 (10) = happyGoto action_15
action_30 (11) = happyGoto action_10
action_30 (12) = happyGoto action_11
action_30 (16) = happyGoto action_31
action_30 (18) = happyGoto action_21
action_30 (19) = happyGoto action_17
action_30 _ = happyReduce_15

action_31 (25) = happyShift action_36
action_31 _ = happyFail

action_32 (26) = happyShift action_8
action_32 (27) = happyShift action_13
action_32 (28) = happyShift action_14
action_32 (10) = happyGoto action_15
action_32 (11) = happyGoto action_10
action_32 (12) = happyGoto action_11
action_32 (16) = happyGoto action_35
action_32 (18) = happyGoto action_21
action_32 (19) = happyGoto action_17
action_32 _ = happyReduce_15

action_33 _ = happyReduce_17

action_34 _ = happyReduce_12

action_35 (23) = happyShift action_37
action_35 _ = happyFail

action_36 _ = happyReduce_20

action_37 _ = happyReduce_19

happyReduce_7 = happySpecReduce_1  10 happyReduction_7
happyReduction_7 (HappyTerminal (PT _ (TV happy_var_1)))
	 =  HappyAbsSyn10
		 (Ident happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  11 happyReduction_8
happyReduction_8 (HappyTerminal (PT _ (TI happy_var_1)))
	 =  HappyAbsSyn11
		 ((read happy_var_1) :: Integer
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  12 happyReduction_9
happyReduction_9 (HappyTerminal (PT _ (TL happy_var_1)))
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  13 happyReduction_10
happyReduction_10 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (Dict (reverse happy_var_1)
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_0  14 happyReduction_11
happyReduction_11  =  HappyAbsSyn14
		 ([]
	)

happyReduce_12 = happySpecReduce_3  14 happyReduction_12
happyReduction_12 _
	(HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_0  15 happyReduction_13
happyReduction_13  =  HappyAbsSyn15
		 ([]
	)

happyReduce_14 = happySpecReduce_2  15 happyReduction_14
happyReduction_14 (HappyAbsSyn19  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_0  16 happyReduction_15
happyReduction_15  =  HappyAbsSyn16
		 ([]
	)

happyReduce_16 = happySpecReduce_1  16 happyReduction_16
happyReduction_16 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn16
		 ((:[]) happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  16 happyReduction_17
happyReduction_17 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn16
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_2  17 happyReduction_18
happyReduction_18 (HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn17
		 (E happy_var_1 (reverse happy_var_2)
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happyReduce 5 17 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn16  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	(HappyAbsSyn10  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (EA happy_var_1 (reverse happy_var_2) happy_var_4
	) `HappyStk` happyRest

happyReduce_20 = happyReduce 4 18 happyReduction_20
happyReduction_20 (_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (TermC happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_1  18 happyReduction_21
happyReduction_21 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 (TermA happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  19 happyReduction_22
happyReduction_22 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn19
		 (NId happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  19 happyReduction_23
happyReduction_23 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn19
		 (NArg happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  19 happyReduction_24
happyReduction_24 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn19
		 (NStr happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 30 30 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS ";") -> cont 20;
	PT _ (TS ",") -> cont 21;
	PT _ (TS "{") -> cont 22;
	PT _ (TS "}") -> cont 23;
	PT _ (TS "(") -> cont 24;
	PT _ (TS ")") -> cont 25;
	PT _ (TV happy_dollar_dollar) -> cont 26;
	PT _ (TI happy_dollar_dollar) -> cont 27;
	PT _ (TL happy_dollar_dollar) -> cont 28;
	_ -> cont 29;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => [(Token)] -> Err a
happyError' = happyError

pDictionary tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn13 z -> happyReturn z; _other -> notHappyAtAll })

pListEntry tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn14 z -> happyReturn z; _other -> notHappyAtAll })

pListArg tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn15 z -> happyReturn z; _other -> notHappyAtAll })

pListTerm tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_3 tks) (\x -> case x of {HappyAbsSyn16 z -> happyReturn z; _other -> notHappyAtAll })

pEntry tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_4 tks) (\x -> case x of {HappyAbsSyn17 z -> happyReturn z; _other -> notHappyAtAll })

pTerm tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_5 tks) (\x -> case x of {HappyAbsSyn18 z -> happyReturn z; _other -> notHappyAtAll })

pArg tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_6 tks) (\x -> case x of {HappyAbsSyn19 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map prToken (take 4 ts))

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (1) tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 311 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
