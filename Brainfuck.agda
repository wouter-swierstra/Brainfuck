open import Coinduction hiding (unfold) renaming (♯_ to Thunk; ♭ to Force) 

module Brainfuck where

---------- A very basic Prelude ----------

record Pair (a b : Set) : Set where
  constructor _,_
  field
    fst : a
    snd : b

data Maybe (a : Set) : Set where
  Nothing : Maybe a
  Just : (x : a) -> Maybe a

return : {a : Set} -> a -> Maybe a 
return x = Just x

data Nat : Set where
  Zero : Nat 
  Succ : Nat -> Nat

{-# BUILTIN NATURAL Nat #-}
{-# BUILTIN ZERO Zero #-}
{-# BUILTIN SUC Succ #-}

data Bool : Set where
  True : Bool
  False : Bool

if_then_else : {a : Set} -> Bool -> a -> a -> a
if True then t else f = t
if False then t else f = f

---------- Bits and Bytes ----------

data Bit : Set where
  O : Bit
  I : Bit

data Vec (a : Set) : Nat -> Set where
  Nil : Vec a 0
  Cons : {n : Nat} -> (x : a) -> (xs : Vec a n) -> Vec a (Succ n)

Byte : Set
Byte = Vec Bit 8

incr : {n : Nat} -> Vec Bit n -> Vec Bit n
incr Nil = Nil
incr (Cons O xs) = Cons I xs
incr (Cons I xs) = Cons O (incr xs)

decr : {n : Nat} -> Vec Bit n -> Vec Bit n
decr Nil = Nil
decr (Cons O xs) = Cons I (decr xs)
decr (Cons I xs) = Cons O xs

isZero : {n : Nat} -> Vec Bit n -> Bool
isZero Nil = True
isZero (Cons O xs) = isZero xs
isZero (Cons I xs) = False

---------- The state of the machine ----------

data Stream (a : Set) : Set where
  Cons : a -> ∞ (Stream a) -> Stream a

record State : Set where
  constructor _,_,_,_,_
  field
    left : Stream Byte
    current : Byte
    right : Stream Byte
    stdin : Stream Byte
    stdout : Stream Byte

stepLeft : State -> State
stepLeft (Cons left lefts , current , right , stdin , stdout) = 
  (Force lefts) , left , (Cons current (Thunk right)) , stdin , stdout

stepRight : State -> State
stepRight (left , current , Cons right rights , stdin , stdout) = 
  Cons current (Thunk left) , right , Force rights , stdin , stdout

output : State -> State
output (left , current , right , stdin , stdout) = 
  left , current , right , stdin , (Cons current (Thunk stdout))

input : State -> State
input (left , current , right , Cons b stdin , stdout) = 
  left , b , right , Force stdin , stdout

increment : State -> State
increment (left , current , right , stdin , stdout) = 
  left , (incr current) , right , stdin , stdout

decrement : State -> State
decrement (left , current , right , stdin , stdout) =
  left , (decr current) , right , stdin , stdout

---------- The Brainfuck language ----------

data Command : Set where
  >_ : (c : Command) -> Command
  <_ : (c : Command) -> Command
  +_ : (c : Command) -> Command
  -_ : (c : Command) -> Command
  ·_ : (c : Command) -> Command
  ,_ : (c : Command) -> Command
  [_]_ : (body : Command) -> (c : Command) -> Command
  □ : Command

-- sequence c1 c2 computes a new command equivalent to c1 followed by c2
sequence : Command -> Command -> Command 
sequence (> c) c' = > (sequence c c')
sequence (< c) c' = < (sequence c c')
sequence (+ c) c' = + (sequence c c')
sequence (- c) c' = - (sequence c c')
sequence (· c) c' = · (sequence c c')
sequence (, c) c' = , (sequence c c')
sequence ([ body ] c) c' = [ body ] (sequence c c')
sequence □ c' = c'

step : Pair Command State -> Maybe (Pair Command State)
step ((> cmd) , s) = return (cmd , stepRight s)
step ((< cmd) , s) = return (cmd , stepLeft s)
step ((+ cmd) , s) = return (cmd , increment s)
step ((- cmd) , s) = return (cmd , decrement s)
step ((· cmd) , s) = return (cmd , output s)
step ((, cmd) , s) = return (cmd , input s)
step ([ body ] cmd , s) = if isZero (State.current s) 
                          then return (cmd , s) 
                          else (return ((sequence body ([ body ] cmd)) , s))
step (□ , s) = Nothing

data Trace : Set where
  Step : State -> ∞ Trace -> Trace
  Stop : State -> Trace

interpret : Pair Command State -> Trace
interpret (cmd , s) with step (cmd , s)
interpret (cmd , s) | Nothing = Stop s
interpret (cmd , s) | Just x = Step s (Thunk (interpret x))
