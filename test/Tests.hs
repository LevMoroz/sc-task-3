{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}

module Tests
  ( tests,
  )
where

import Control.Arrow
import Data.List
import Data.Maybe
import Task
import Test.QuickCheck
import Test.Tasty.Hspec

tests :: Spec
tests = parallel $ do
  describe "ErrorState" $ do
    let eqState ::
          (Arbitrary s, Show s, Eq e, Eq a, Eq s, Show e, Show a) =>
          ErrorState e s a ->
          ErrorState e s a ->
          Property
        eqState lhs rhs = property $ \s -> runErrorState lhs s === runErrorState rhs s
    describe "Functor" $ do
      specify "fmap id = id" $ do
        let testProperty ::
              forall e s a.
              (Arbitrary s, Show s, Eq e, Eq a, Eq s, Show e, Show a) =>
              Blind (ErrorState e s a) ->
              Property
            testProperty (Blind s) = fmap id s `eqState` s
        conjoin
          [ property $ testProperty @() @Bool @Char,
            property $ testProperty @Int @Char @Bool,
            property $ testProperty @[Int] @[Bool] @Char,
            property $ testProperty @String @Bool @String
          ]
      specify "fmap (g . f) = fmap g . fmap f" $ do
        let testProperty ::
              forall e s a b c.
              (Arbitrary s, Show s, Eq e, Eq s, Show e, Eq c, Show c) =>
              Blind (ErrorState e s a) ->
              Fun a b ->
              Fun b c ->
              Property
            testProperty (Blind s) f' g' = fmap (g . f) s `eqState` (fmap g . fmap f) s
              where
                f = applyFun f'
                g = applyFun g'
        conjoin
          [ property $ testProperty @() @Bool @Char @Int @Char,
            property $ testProperty @Int @Char @Bool @() @Char,
            property $ testProperty @[Int] @[Bool] @Char @Int @Bool,
            property $ testProperty @String @Bool @String @Bool @Char
          ]
    describe "Applicative" $ do
      specify "pure id <*> v = v" $ do
        let testProperty ::
              forall e s a.
              (Arbitrary s, Show s, Eq e, Eq a, Eq s, Show e, Show a) =>
              Blind (ErrorState e s a) ->
              Property
            testProperty (Blind s) = (pure id <*> s) `eqState` s
        conjoin
          [ property $ testProperty @() @Bool @Char,
            property $ testProperty @Int @Char @Bool,
            property $ testProperty @[Int] @[Bool] @Char,
            property $ testProperty @String @Bool @String
          ]
      specify "pure f <*> pure x = pure (f x)" $ do
        let testProperty ::
              forall e s a b.
              (Arbitrary s, Show s, Eq e, Eq s, Show e, Eq b, Show b) =>
              a ->
              Fun a b ->
              Property
            testProperty x f' = (pure f <*> pure x) `eqState` (pure (f x) :: ErrorState e s b)
              where
                f = applyFun f'
        conjoin
          [ property $ testProperty @() @Bool @Char @Int,
            property $ testProperty @Int @Char @Bool @(),
            property $ testProperty @[Int] @[Bool] @Char @Bool,
            property $ testProperty @String @Bool @String @Char
          ]
    describe "Monad" $ do
      specify "m >>= return = m" $ do
        let testProperty ::
              forall e s a.
              (Arbitrary s, Show s, Eq e, Eq a, Eq s, Show e, Show a) =>
              Blind (ErrorState e s a) ->
              Property
            testProperty (Blind s) = (s >>= return) `eqState` s
        conjoin
          [ property $ testProperty @() @Bool @Char,
            property $ testProperty @Int @Char @Bool,
            property $ testProperty @[Int] @[Bool] @Char,
            property $ testProperty @String @Bool @String
          ]
      specify "return a >>= f = f a" $ do
        let testProperty ::
              forall e s a b.
              (Arbitrary s, Show s, Eq e, Eq s, Show e, Eq b, Show b) =>
              a ->
              Blind (Fun a (ErrorState e s b)) ->
              Property
            testProperty x (Blind f') = (return x >>= f) `eqState` f x
              where
                f = applyFun f'
        conjoin
          [ property $ testProperty @() @Bool @Char @Int,
            property $ testProperty @Int @Char @Bool @(),
            property $ testProperty @[Int] @[Bool] @Char @Bool,
            property $ testProperty @String @Bool @String @Char
          ]
      specify "m >>= (\\x -> f x >>= g) = (m >>= f) >>= g" $ do
        let testProperty ::
              forall e s a b c.
              (Arbitrary s, Show s, Eq e, Eq s, Show e, Eq c, Show c) =>
              Blind (ErrorState e s a) ->
              Blind (Fun a (ErrorState e s b)) ->
              Blind (Fun b (ErrorState e s c)) ->
              Property
            testProperty (Blind m) (Blind f') (Blind g') = (m >>= (\x -> f x >>= g)) `eqState` ((m >>= f) >>= g)
              where
                f = applyFun f'
                g = applyFun g'
        conjoin
          [ property $ testProperty @() @Bool @Char @Int @Char,
            property $ testProperty @Int @Char @Bool @() @Char,
            property $ testProperty @[Int] @[Bool] @Char @Int @Bool,
            property $ testProperty @String @Bool @String @Bool @Char
          ]
    specify "get" $ do
      let testGet :: forall b. (Show b, Eq b) => b -> Property
          testGet s = executeErrorState s get === (Right s :: Either () b)
      conjoin
        [ property $ testGet @(),
          property $ testGet @Int,
          property $ testGet @[Int],
          property $ testGet @String
        ]
    specify "put" $ do
      let testPut :: forall b. (Show b, Eq b) => b -> b -> Property
          testPut s s' = runErrorState (put s') s === (Right ((), s') :: Either () ((), b))
      conjoin
        [ property $ testPut @(),
          property $ testPut @Int,
          property $ testPut @[Int],
          property $ testPut @String
        ]
    specify "throwError" $ do
      let testThrowError :: forall e. (Show e, Eq e) => e -> Property
          testThrowError e = runErrorState (throwError e) undefined === (Left e :: Either e ((), ()))
      conjoin
        [ property $ testThrowError @(),
          property $ testThrowError @Int,
          property $ testThrowError @[Int],
          property $ testThrowError @String
        ]
    specify "modify" $ do
      let testModify :: forall b. (Show b, Eq b) => Fun b b -> b -> Property
          testModify f b = runErrorState (modify $ applyFun f) b === (Right ((), applyFun f b) :: Either () ((), b))
      conjoin
        [ property $ testModify @(),
          property $ testModify @Int,
          property $ testModify @[Int],
          property $ testModify @String
        ]
  describe "BFMonad" $ do
    describe "readInput" $ do
      let state i =
            BFState
              { bfDataTape = undefined,
                bfInput = i,
                bfOutput = undefined
              }
      specify "non-empty" $ do
        let testReadInput :: NonEmptyList Int -> Property
            testReadInput (NonEmpty l@(x : xs)) =
              ((fst &&& bfInput . snd) <$> runErrorState readInput (state l)) === Right (x, xs)
        property testReadInput
      specify "empty" $ do
        executeErrorState (state []) readInput
          `shouldBe` Left NotEnoughInput
    specify "writeOutput" $ do
      let state =
            BFState
              { bfDataTape = undefined,
                bfInput = undefined,
                bfOutput = []
              }
          testWriteOutput :: [Int] -> Property
          testWriteOutput l =
            (bfOutput . snd <$> runErrorState (mapM_ writeOutput l) state) === Right (reverse l)
      property testWriteOutput
    specify "shiftDataR" $ do
      let state t =
            BFState
              { bfDataTape = t,
                bfInput = undefined,
                bfOutput = undefined
              }
          testShiftDataR :: Tape Int -> Property
          testShiftDataR t = case shiftTapeR t of
            Nothing -> runErrorState shiftDataR (state t) === Left DataTapeExhausted
            Just t' -> (bfDataTape . snd <$> runErrorState shiftDataR (state t)) === Right t'
      property testShiftDataR
    specify "shiftDataL" $ do
      let state t =
            BFState
              { bfDataTape = t,
                bfInput = undefined,
                bfOutput = undefined
              }
          testShiftDataL :: Tape Int -> Property
          testShiftDataL t = case shiftTapeL t of
            Nothing -> runErrorState shiftDataL (state t) === Left DataTapeExhausted
            Just t' -> (bfDataTape . snd <$> runErrorState shiftDataL (state t)) === Right t'
      property testShiftDataL
    specify "readData" $ do
      let state t =
            BFState
              { bfDataTape = t,
                bfInput = undefined,
                bfOutput = undefined
              }
          testReadData :: Tape Int -> Property
          testReadData t =
            executeErrorState (state t) readData === Right (tapeValue t)
      property testReadData
    specify "writeData" $ do
      let state t =
            BFState
              { bfDataTape = t,
                bfInput = undefined,
                bfOutput = undefined
              }
          testWriteData :: Tape Int -> Int -> Property
          testWriteData t x =
            (bfDataTape . snd <$> runErrorState (writeData x) (state t)) === Right t {tapeValue = x}
      property testWriteData
  describe "BF" $ do
    describe "executeCommand" $ do
      specify "ShiftRight" $ do
        let state t =
              BFState
                { bfDataTape = t,
                  bfInput = undefined,
                  bfOutput = undefined
                }
            testShiftRight :: Tape Int -> Property
            testShiftRight t = case shiftTapeR t of
              Nothing -> runErrorState (executeCommand ShiftRight) (state t) === Left DataTapeExhausted
              Just t' -> (bfDataTape . snd <$> runErrorState (executeCommand ShiftRight) (state t)) === Right (t')
        property testShiftRight
      specify "ShiftLeft" $ do
        let state t =
              BFState
                { bfDataTape = t,
                  bfInput = undefined,
                  bfOutput = undefined
                }
            testShiftLeft :: Tape Int -> Property
            testShiftLeft t = case shiftTapeL t of
              Nothing -> runErrorState (executeCommand ShiftLeft) (state t) === Left DataTapeExhausted
              Just t' -> (bfDataTape . snd <$> runErrorState (executeCommand ShiftLeft) (state t)) === Right (t')
        property testShiftLeft
      specify "Increment" $ do
        let state t =
              BFState
                { bfDataTape = t,
                  bfInput = undefined,
                  bfOutput = undefined
                }
            testProperty :: Tape Int -> Property
            testProperty t =
              (bfDataTape . snd <$> runErrorState (executeCommand Increment) (state t)) === Right (touchTape (+ 1) t)
        property testProperty
      specify "Decrement" $ do
        let state t =
              BFState
                { bfDataTape = t,
                  bfInput = undefined,
                  bfOutput = undefined
                }
            testProperty :: Tape Int -> Property
            testProperty t =
              (bfDataTape . snd <$> runErrorState (executeCommand Decrement) (state t)) === Right (touchTape pred t)
        property testProperty
      specify "WriteOutput" $ do
        let state t o =
              BFState
                { bfDataTape = t,
                  bfInput = undefined,
                  bfOutput = o
                }
            testProperty :: Tape Int -> [Int] -> Property
            testProperty t l =
              ((bfDataTape &&& bfOutput) . snd <$> runErrorState (executeCommand WriteOutput) (state t l))
                === Right (t, tapeValue t : l)
        property testProperty
      specify "ReadInput" $ do
        let state t i =
              BFState
                { bfDataTape = t,
                  bfInput = i,
                  bfOutput = undefined
                }
            testProperty :: Tape Int -> [Int] -> Property
            testProperty t l = case l of
              [] -> runErrorState (executeCommand ReadInput) (state t l) === Left NotEnoughInput
              (x : xx) -> ((bfDataTape &&& bfInput) . snd <$> runErrorState (executeCommand ReadInput) (state t l)) === Right ((t {tapeValue = x}, xx))
        property testProperty
      specify "Loop" $ do
        let toList t = reverse (leftTape t) <> [tapeValue t] <> rightTape t
            testLoop i o l =
              (toList . bfDataTape . snd <$> runErrorState (executeCommand (Loop l)) (state . fromJust . initializeTape $ i))
                `shouldBe` Right o
            state t =
              BFState
                { bfDataTape = t,
                  bfInput = undefined,
                  bfOutput = undefined
                }
        testLoop [0] [0] []
        testLoop [0] [0] [Loop []]
        testLoop [5] [0] [Loop [Decrement]]
        testLoop [5, 5, 1, 2, 0] [6, 6, 2, 3, 0] [Increment, ShiftRight]
    describe "executeProgram" $ do
      specify "Hello World" $ do
        executeProgram "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++." "" `shouldBe` Just "Hello World!\n"
      specify "Bubble Sort" $ do
        let sort' x = executeProgram ">>>>>,[>>>,]<<<[<<<[>>>[-<<<-<+>[>]>>]<<<[<]>>[>>>+<<<-]<[>+>>>+<<<<-]<<]>>>[.[-]]>>>[>>>]<<<]" (x <> "\NUL")
        property . withMaxSuccess 10 $ \(l :: String) -> let l' = filter (/= '\NUL') l in sort' l' === Just (sort l')
      specify "wc" $ do
        let wc x = executeProgram ">>>+>>>>>+>>+>>+[<<],[-[-[-[-[-[-[-[-[<+>-[>+<-[>-<-[-[-[<++[<++++++>-]<[>>[-<]<[>]<-]>>[<+>-[<->[-]]]]]]]]]]]]]]]]<[-<<[-]+>]<<[>>>>>>+<<<<<<-]>[>]>>>>>>>+>[<+[>+++++++++<-[>-<-]++>[<+++++++>-[<->-]+[+>>>>>>]]<[>+<-]>[>>>>>++>[-]]+<]>[-<<<<<<]>>>>],]+<++>>>[[+++++>>>>>>]<+>+[[<++++++++>-]<.<<<<<]>>>>>>>>]" (x <> "\NUL")
        wc "Hello.\nThis is a thing.\nYay!" `shouldBe` Just "\t2\t6\t28\n"
        wc "Hello.\n This is text\nThis is a thing.\nYay!" `shouldBe` Just "\t3\t9\t42\n"
        wc "Hello.\n This is \n djnjd \n text\nThis is a thing.\nYay!" `shouldBe` Just "\t5\t10\t52\n"
      specify "Squares" $ do
        executeProgram "++++[>+++++<-]>[<+++++>-]+<+[>[>+>+<<-]++>>[<<+>>-]>>>[-]++>[-]+>>>+[[-]++++++>>>]<<<[[<++++++++<++>>-]+<.<[>----<-]<]<<[>>>>>[>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<]]<[>+<-]>]<<-]<<-]" ""
          `shouldBe` Just (unlines . fmap (show @Int) . takeWhile (<= 10000) . fmap (\x -> x * x) $ [0 ..])
      specify "Insertion Sort" $ do
        let sort' x = executeProgram ">>+>,[<[[>>+<<-]>[<<+<[->>+[<]]>>>[>]<<-]<<<]>>[<<+>>-]<[>+<-]>[>>]<,]<<<[<+<]>[>.>]" (x <> "\NUL")
        property . withMaxSuccess 10 $ \(l :: String) -> let l' = filter (/= '\NUL') l in sort' l' === Just (sort l')

instance Arbitrary a => Arbitrary (Tape a) where
  arbitrary = applyArbitrary3 Tape

instance
  (Arbitrary e, Arbitrary a, Function s, CoArbitrary s, Arbitrary s) =>
  Arbitrary (ErrorState e s a)
  where
  arbitrary = ErrorState . applyFun <$> arbitrary
