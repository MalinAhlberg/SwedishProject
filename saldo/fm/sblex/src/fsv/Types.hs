module Types where

import General
import Invariant

-- Noun parameters.

data Gender = Masc | Fem | Neut
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Number = Sg | Pl
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data NumberD = Sing | Dual | Plu
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Species = Indef | Def
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Case = Nom | Gen | Dat | Ack
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data NN_Form = NN_C Number Case Species
  deriving (Eq, Ord, Show, Read)

instance Param Gender   where 
    values = enum
    prValue Masc = "m"
    prValue Fem = "f"
    prValue Neut = "n"
instance Param Number   where 
    values = enum
    prValue Sg = "sg"
    prValue Pl = "pl"
instance Param NumberD  where 
    values = enum
    prValue Sing = "sg"
    prValue Dual = "dl"
    prValue Plu  = "pl"
instance Param Species  
    where values = enum
          prValue Indef = "indef"
          prValue Def = "def"
instance Param Case     
    where values = enum
          prValue Nom = "nom" 
          prValue Gen = "gen"
          prValue Dat = "dat"
          prValue Ack = "ack"

instance Param NN_Form 
  where values = [NN_C a b c | c <- values, a <- values, b <- values]
	prValue (NN_C a b c) = unwords [prValue a, prValue b, prValue c]

type NN = NN_Form -> Str

data Modus = Ind | Conj
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Vox = Act | Pass
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Grade = Posit | Compar | Superl
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Person = P1 | P2 | P3
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Person12 = Pers1 | Pers2
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param Modus    where 
    values = enum
    prValue Ind = "ind"
    prValue Conj = "konj"
instance Param Vox      where 
    values = enum
    prValue Act  = "akt"
    prValue Pass = "pass"
instance Param Grade    where 
    values = enum
    prValue Posit  = "pos"
    prValue Compar = "komp"
    prValue Superl = "super"
instance Param Person   where 
    values = enum
    prValue P1 = "p1"
    prValue P2 = "p2"
    prValue P3 = "p3"
instance Param Person12 where 
    values = enum
    prValue Pers1 = "p1"
    prValue Pers2 = "p2"
-- adjectives

type AV = AdjForm -> Str

data AdjForm =
  StrongPos Gender Number Case 
  | WeakPos   Gender Number Case 
  | Comp      Gender Number Case 
  | Super 
  deriving (Eq, Ord, Show, Read)

instance Param AdjForm where 
  values =            
    [WeakPos   a b c | a <- values, b <- values, c <- values] ++
    [StrongPos a b c | a <- values, b <- values, c <- values] ++
    [Comp      a b c | a <- values, b <- values, c <- values] ++ [Super]
  prValue (WeakPos a b c)   = "pos weak "   ++ prValue a ++ " " ++ prValue b ++ " " ++ prValue c
  prValue (StrongPos a b c) = "pos strong " ++ prValue a ++ " " ++ prValue b ++ " " ++ prValue c
  prValue (Comp a b c)      = "comp "       ++ prValue a ++ " " ++ prValue b ++ " " ++ prValue c
  prValue (Super)           = "super"

type Numeral = NumeralForm -> Str

data NumeralForm = NumF Case Gender
 deriving (Show,Eq)

instance Param NumeralForm where
  values = [NumF a b | b <- values, a <- values]
  prValue (NumF c g) = prValue c ++ " " ++ prValue g


type Ordinal = OrdinalForm -> Str

data OrdinalForm = OrdF Gender Number Case
    deriving (Show,Eq)

instance Param OrdinalForm where
  values = [OrdF a b c | a <- values, b <- values, c <- values]
  prValue (OrdF g n c) = prValue g ++ " " ++ prValue n ++ " " ++ prValue c

-- verbs

type Verb = VerbForm -> Str

data VerbForm = 
   PresSg     Modus  Vox
 | PresPl     Person Modus Vox
 | PretInd    Number Person Vox
 | Inf Vox
 | PretConjSg Vox
 | PretConjPl Person Vox
 | ImperSg
 | ImperPl Person12
  deriving (Eq, Ord, Show, Read)

instance Param VerbForm where
  values = [PresSg     m v | v <- values, m <- values]              ++
           [PresPl   p m v | v <- values, m <- values, p <- values] ++
           [PretInd  n p v | v <- values, n <- values, p <- values] ++
           [PretConjSg   v | v <- values]                           ++
           [PretConjPl p v | v <- values, p <- values]              ++
           [Inf v | v <- values] ++
           [ImperSg] ++ 
           [ImperPl p | p <- values]
  prValue (PresSg m v)      = unwords ["pres",prValue m, prValue Sg, prValue v]
  prValue (PresPl p m v)    = unwords ["pres",prValue m, prValue Pl, prValue p, prValue v]
  prValue (PretInd n p v)   = unwords ["pret",prValue Ind, prValue n, prValue p, prValue v]
  prValue (PretConjSg v)    = unwords ["pret",prValue Conj, prValue Sg,prValue v] 
  prValue (PretConjPl p v)  = unwords ["pret",prValue Conj, prValue Pl,prValue p,prValue v] 
  prValue (ImperSg)         = unwords ["imper",prValue Sg]
  prValue (ImperPl p)       = unwords ["imper",prValue Pl, prValue p]
  prValue (Inf v)           = unwords ["inf",prValue v]

--- Pronoun

type Pronoun = PNForm -> Str

data PNForm = PNF Gender Number Case
   deriving (Eq,Ord,Show,Read)

instance Param PNForm where
   values = [PNF g n c | g <- values, n <- values, c <- values]
   prValue (PNF g n c) = unwords [prValue g, prValue n, prValue c]

type PPronoun = PPNForm -> Str

data PPNForm = First  NumberD Case |
               Second NumberD Case |
               Refl     NumberD Case
   deriving (Eq,Ord,Show,Read)

instance Param PPNForm where
   values = [First n c | n <- values, c <- values] ++
            [Second n c | n <- values, c <- values] ++
            [Refl n c | n <- values, c <- values]
   prValue (First n c)  = unwords ["p1",prValue n, prValue c]
   prValue (Second n c) = unwords ["p2",prValue n, prValue c]
   prValue (Refl n c)   = unwords ["refl",prValue n, prValue c]


type Adverb = AdverbForm -> Str

data AdverbForm = AdverbForm Grade
  deriving (Eq, Ord, Show, Read)

instance Param AdverbForm where 
    values = [AdverbForm g | g <- values]
    prValue (AdverbForm g) = prValue g

{-

-- price to pay for hierarchical types: if we want to define non-passive verbs
passiveForms :: [VerbForm]
passiveForms = 
  map VF (concat [[Pres m Pass, Pret m Pass] | m <- values]) ++ 
  map VI [Inf Pass, Sup Pass] ++ 
  partPretForms

partPretForms :: [VerbForm]
partPretForms = [VI (PtPret a c) | a <- values, c <- values]

positiveForms :: [AdjForm]
positiveForms = [AF (Pos a) c | a <- values, c <- values]

-- adverbs

-- invariant adverbs

type AdverbInv = AdverbInvForm -> Str

data AdverbInvForm = AdverbInvForm
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param AdverbInvForm where 
			     values = enum
			     prValue _ = invar

-- invariant interrogative adverbs

type InterrogInv = InterrogInvForm -> Str

data InterrogInvForm = InterrogInvForm
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param InterrogInvForm where 
			     values = enum
			     prValue _ = invar


-------------------------------
-- closed classes -------------
-------------------------------

-- pronouns

type PronPN  = PronCasus -> Str
type PronAdj = AdjPronForm -> Str

data PronCasus = PNom | PAcc | PGen GenNum
  deriving (Eq, Ord, Show, Read)

instance Param PronCasus where
  values = PNom : PAcc : map PGen values

data AdjPronForm = AP GenNum Casus
  deriving (Eq, Ord, Show, Read)


instance Param AdjPronForm where
  values = [AP g c | g <- values, c <- values]

type PronInv = PronInvForm -> Str

data PronInvForm = PronInvForm
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param PronInvForm where 
			     values = enum
			     prValue _ = invar


-- invariant
data InterjForm = InterjForm 
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

type Interjection = InterjForm -> Str

instance Param InterjForm where 
  values = enum
  prValue _ = invar

-- articles

type Article = ArticleForm -> Str

data ArticleForm = ArticleForm GenNum
 deriving(Eq,Ord,Show,Read)

instance Param ArticleForm where 
   values = [ArticleForm g | g <- values]
   prValue (ArticleForm g) = prValue g

-- auxiliary verbs

data AuxVerbForm = AuxInf | AuxPres | AuxPret | AuxSup 
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param AuxVerbForm where values = enum

type AuxVerb = AuxVerbForm -> Str

-- Prepositions

type Preposition = PrepForm -> Str

data PrepForm = PrepForm
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param PrepForm where 
			values = enum
			prValue _ = invar

-- Conjunction

type Conjunction = ConjForm -> Str

data ConjForm = ConjForm
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param ConjForm where 
			values = enum
			prValue _ = invar

-- Subjunction

type Subjunction = SubForm -> Str

data SubForm = SubForm
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param SubForm where 
		       values = enum
		       prValue _ = invar

-- Particles

type Particle = PartForm -> Str

data PartForm = PartForm
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param PartForm where 
			values = enum
			prValue _ = invar
		   
-- Infinitive mark
type InfMark = InfMarkForm -> Str

data InfMarkForm = InfMarkForm
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param InfMarkForm where 
			   values = enum
			   prValue _ = invar

-- Proper Noun
type PN = PNForm -> Str

data PNForm = PNForm Casus
 deriving (Eq, Ord, Show, Read)

instance Param PNForm where 
		      values = [PNForm c | c <- values]
		      prValue (PNForm c) = prValue c

-}
