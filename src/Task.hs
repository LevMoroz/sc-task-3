module Task
  ( ErrorState (..),
    get,
    put,
    throwError,
    modify,
    BFState (..),
    BFError (..),
    BFMonad,
    readInput,
    writeOutput,
    CommandResult (..),
    shiftCommandR,
    shiftCommandL,
    shiftDataR,
    shiftDataL,
    readData,
    writeData,
    readCommand,
    executeCommand,
    evaluateProgram,
    executeProgram,
  )
where

import Data.Char

--   ____  _____   _       _                           _
--  | __ )|  ___| (_)_ __ | |_ ___ _ __ _ __  _ __ ___| |_ ___ _ __
--  |  _ \| |_    | | '_ \| __/ _ \ '__| '_ \| '__/ _ \ __/ _ \ '__|
--  | |_) |  _|   | | | | | ||  __/ |  | |_) | | |  __/ ||  __/ |
--  |____/|_|     |_|_| |_|\__\___|_|  | .__/|_|  \___|\__\___|_|
--                                     |_|

-- In this section you will implements an interpreter for the BF language.
-- If you are not familiar with it, please familiarize yourself:
--   https://en.wikipedia.org/wiki/Brainfuck

--   _____
--  |_   _|_ _ _ __  ___
--    | |/ _` | '_ \/ -_)
--    |_|\__,_| .__/\___|
--            |_|

-- | Represents a possibly infinite tape with a tape head pointing to one of the
-- tape value.
--
-- So, this object:
--
-- Tape
--  { leftTape :: [2, 6, ...],
--    tapeValue :: 1,
--    rightTape :: [5, 8, ...]
--  }
--
--  Would represent this tape:
--
--   ◀──┬─────┬─────┬─────┬─────┬─────┬──▶
--  ... │  6  │  2  │  1  │  5  │  8  │ ...
--   ◀──┴─────┴─────┴─────┴─────┴─────┴──▶
--                     ▲
--                     │
data Tape a
  = Tape
      { leftTape :: [a],
        tapeValue :: a,
        rightTape :: [a]
      }
  deriving (Eq, Ord)

instance Show a => Show (Tape a) where
  show (Tape l c right) = show (reverse (take 5 l)) <> " " <> show c <> " " <> show (take 5 right)

-- | Creates a tape from a list.
--
-- Note that the tape will be empty to the left.
--
-- >>> initializeTape [1, 5, 8, ...]
--
--   ◀──┬─────┬─────┬─────┬─────┬──▶
--  ... │     │  1  │  5  │  8  │ ...
--   ◀──┴─────┴─────┴─────┴─────┴──▶
--               ▲
--               │
initializeTape :: [a] -> Maybe (Tape a)
initializeTape [] = Nothing
initializeTape (a : aa) = Just (Tape [] a aa)

-- | This is represents the operation of moving the tape pointer to the left.
shiftTapeL :: Tape a -> Maybe (Tape a)
shiftTapeL (Tape (l : ll) c rr) = Just (Tape ll l (c : rr))
shiftTapeL _ = Nothing

-- | This is represents the operation of moving the tape pointer to the right.
shiftTapeR :: Tape a -> Maybe (Tape a)
shiftTapeR (Tape ll c (r : rr)) = Just (Tape (c : ll) r rr)
shiftTapeR _ = Nothing

-- | This operation allows you to modify the cell at the tape pointer.
touchTape :: (a -> a) -> Tape a -> Tape a
touchTape f (Tape l c r) = Tape l (f c) r

--   ___ _        _                                _
--  / __| |_ __ _| |_ ___   _ __  ___ _ _  __ _ __| |
--  \__ \  _/ _` |  _/ -_) | '  \/ _ \ ' \/ _` / _` |
--  |___/\__\__,_|\__\___| |_|_|_\___/_||_\__,_\__,_|

-- In this section we will implement the state monad with some basic operations
-- it supports.

-- | Since the standard `State` monad doesn't have error handling capabilities.
-- we will implement a version of `State`, which can also handle errors.
--
-- `e` is the type of the possible error
-- `s` is the type of the state that is carried through the monad
-- `a` is the value that is returned.
data ErrorState e s a
  = ErrorState
      {runErrorState :: s -> Either e (a, s)}

instance Functor (ErrorState e s) where
  fmap = error "TODO: Functor (ErrorState e s) - fmap"

instance Applicative (ErrorState e s) where
  pure = error "TODO: Applicative (ErrorState e s) - pure"

  -- NOTE: State from the left hand side should be passed to the right hand
  -- side.
  -- Conversely the state from the right hand side should be the state of the
  -- output.
  (<*>) = error "TODO: Applicative (ErrorState e s) - (<*>)"

instance Monad (ErrorState e s) where
  (>>=) = error "TODO: Monad (ErrorState e s) - >>="

-- | This operation returns the state that the monad currently contains.
get :: ErrorState e s s
get = error "TODO: get"

-- | This operation sets the state in the monad to a new value.
put :: s -> ErrorState e s ()
put = error "TODO: put"

-- | This operation throws an error in the monad.
throwError :: e -> ErrorState e s a
throwError = error "TODO: throwError"

-- | This operations allows you to encapsulate the process of reading the state,
-- modifying it and writing it into the monad in a single operation.
--
-- It should modify the current state with the given function.
modify :: (s -> s) -> ErrorState e s ()
modify = error "TODO: modify"

--   ___ ___                          _
--  | _ ) __|  _ __  ___ _ _  __ _ __| |
--  | _ \ _|  | '  \/ _ \ ' \/ _` / _` |
--  |___/_|   |_|_|_\___/_||_\__,_\__,_|

-- In this section we will implement the specific state we will be using and
-- some operations that operate on it within the state monad.

type BFCommand = Char

-- | This represents the state of our BF interpreter.
data BFState
  = BFState
      { -- | The command tape. It contains the a
        -- pointer to the command being executed.
        bfCommandTape :: Tape BFCommand,
        -- | The data tape.
        bfDataTape :: Tape Char,
        -- | The input stream of the BF interpreter.
        bfInput :: [Char],
        -- | The output stream of the BF interpreter.
        bfOutput :: [Char]
      }
  deriving (Eq, Show)

data BFError
  = NotEnoughInput
  | DataTapeExhausted
  deriving (Show, Eq)

type BFMonad a = ErrorState BFError BFState a

-- | This operation should consume one 'Char' from the input stream
-- and return it.
--
-- Should throw 'NotEnoughInput' error when the input stream is empty.
--
-- HINT: From now on it would be easier to use `do`-syntax.
readInput :: BFMonad Char
readInput = error "TODO: readInput"

-- | This operation should write one 'Char' to the output stream.
-- This should just prepend the character to the start of the string using the
-- `:` (cons) operator.
writeOutput :: Char -> BFMonad ()
writeOutput = error "TODO: writeOutput"

data CommandResult
  = ExecutionTerminated
  | ExecutionNotTerminated
  deriving (Eq, Show)

-- | This operations shifts the command pointer to the right.
--
-- NOTE: The execution of the program ends successfully if the command pointer
--   moves past the end of the command tape.
--
--   For this reason you should not throw an error, and instead return whether the
--   execution of the program should be terminated.
shiftCommandR :: BFMonad CommandResult
shiftCommandR = error "TODO: shiftCommandR"

-- | This operations shifts the command pointer to the left.
--
-- NOTE: The execution of the program ends successfully if the command pointer
--   moves past the end of the command tape.
--
--   For this reason you should not throw an error, and instead return whether the
--   execution of the program should be terminated.
shiftCommandL :: BFMonad CommandResult
shiftCommandL = error "TODO: shiftCommandL"

-- | This operations shifts the data pointer to the right.
--
-- NOTE: if the tape has ended, you should throw the 'DataTapeExhausted' error.
shiftDataR :: BFMonad ()
shiftDataR = error "TODO: shiftDataR"

-- | This operations shifts the data pointer to the left.
--
-- NOTE: if the tape has ended, you should throw the 'DataTapeExhausted' error.
shiftDataL :: BFMonad ()
shiftDataL = error "TODO: shiftDataL"

-- | This operation reads the 'Char' at the data pointer.
--
-- NOTE: if the input has been exhausted, you should throw the 'NotEnoughInput'
--   error.
readData :: BFMonad Char
readData = error "TODO: readData"

-- | This operation writes the 'Char' to the current data pointer.
writeData :: Char -> BFMonad ()
writeData = error "TODO: writeData"

readCommand :: BFMonad BFCommand
readCommand = error "TODO: readCommand"

--   _____ _          _     _                        _
--  |_   _| |_  ___  (_)_ _| |_ ___ _ _ _ __ _ _ ___| |_ ___ _ _
--    | | | ' \/ -_) | | ' \  _/ -_) '_| '_ \ '_/ -_)  _/ -_) '_|
--    |_| |_||_\___| |_|_||_\__\___|_| | .__/_| \___|\__\___|_|
--                                     |_|

-- | Executes one BF command.
--
-- The command should be read from the function argument, not the monad state.
--
-- Please have a look at this commands table:
--   https://en.wikipedia.org/wiki/Brainfuck#Commands
--
-- Note: non-command characters are ignored.
executeCommand :: BFCommand -> BFMonad CommandResult
executeCommand = error "TODO: executeCommand"

-- | This function should evaluate the whole program starting from the current
-- command pointer.
--
-- This will probably be recursive.
evaluateProgram :: BFMonad ()
evaluateProgram = error "TODO: evaluateProgram"

-- | This constant just contains the "zero" 'Char' value for your convenience.
zeroChar :: Char
zeroChar = toEnum 0

-- | This constant just contains an infinite empty tape. You can use this as
-- the initial data tape.
emptyTape :: Tape Char
emptyTape = Tape (repeat zeroChar) zeroChar (repeat zeroChar)

-- | In this function you should bring everything together and execute the given
-- list of commands.
--
-- The input stream is the second argument to the function.
--
-- The returned string should contain the output stream that the evaluation
-- produces.
--
-- NOTE: since the output stream of the program is written backwards, you
-- will need to reverse the output stream. (You can use the `reverse` function.)
--
-- You will need to construct the initial state for the monad, evaluate the
-- program from the initial state, and convert the resulting value to the
-- appropriate type.
--
-- You can use these two functions to convert between 'Char' and 'Int':
--   ord :: Char -> Int
--   chr :: Int -> Char
executeProgram :: [BFCommand] -> String -> Maybe String
executeProgram = error "TODO: executeProgram"
