{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Dequeue as Q
import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.List

type Coordinate = (Integer, Integer)
type Board = M.Map Coordinate Integer

data SearchState = SearchState { history :: S.Set Board, frontier :: Q.BankersDequeue Board }
newtype SearchM a = SearchM (ContT Result (ListT (State (S.Set Board))) a)
  deriving (Functor, Applicative, Monad)

data Result = Finished [Board] | Unfinished (SearchM Board)

boardWidth = 4
boardHeight = 4
blockWidth = 2
blockHeight = 2

possibleEntries = [1, 2, 3, 4]

-- Helper function that makes an empty puzzle
makeEmptyBoard :: Board -> Integer -> Integer -> Board
makeEmptyBoard board row col =
  let newBoard = M.insert (row, col) 0 board in
  if (row == 0) then
    if (col == 0) then newBoard
      else makeEmptyBoard newBoard row (col - 1)
  else if (col == 0)
    then makeEmptyBoard newBoard (row - 1) (boardHeight - 1)
    else makeEmptyBoard newBoard row (col - 1)

-- The empty puzzle
emptyBoard :: Board
emptyBoard = makeEmptyBoard M.empty (boardWidth - 1) (boardHeight - 1)

-- testBoard :: Board
-- testBoard = M.fromList [((0,0),1),((0,1),2),((0,2),0),((0,3),0),
--                         ((1,0),3),((1,1),0),((1,2),0),((1,3),0),
--                         ((2,0),0),((2,1),0),((2,2),0),((2,3),3),
--                         ((3,0),0),((3,1),0),((3,2),0),((3,3),1)]

-- Basic pure functions of the search algorithm
getEmptyCells :: Board -> Board
getEmptyCells board = M.filter (< 1) board

checkFinished :: Board -> Bool
checkFinished board = if (getEmptyCells board == M.empty) then True else False

setCell :: Board -> Coordinate -> Integer -> Board
setCell board pos val = M.adjust (\_ -> val) pos board

getCell :: Board -> Coordinate -> Integer
getCell board pos =
  let res = M.lookup pos board in
  case res of Just val -> val
              Nothing -> error "Out of bound!"

singleRowValidate :: Board -> Integer -> Bool
singleRowValidate board rowIndex =
  let listOfVals = M.foldlWithKey (\acc -> \(x, y) -> \v ->
        if (x == rowIndex && not (v == 0)) then v:acc else acc) [] board in
  if (listOfVals == (L.nub listOfVals)) then True else False

singleColValidate :: Board -> Integer -> Bool
singleColValidate board colIndex =
  let listOfVals = M.foldlWithKey (\acc -> \(x, y) -> \v ->
        if (y == colIndex && not (v == 0)) then v:acc else acc) [] board in
  if (listOfVals == (L.nub listOfVals)) then True else False

singleBlockValidate :: Board -> Coordinate -> Bool
singleBlockValidate board startPos =
  let (curX, curY) = startPos in
  let validCell cord =
        let (x, y) = cord in
        if (x >= curX) && (x < curX + blockWidth)
          then (if (y >= curY) && (y < curY + blockHeight)
            then True else False) else False in
  let listOfVals = M.foldlWithKey (\acc -> \pos -> \v ->
        if (validCell pos && not (v == 0)) then v:acc else acc) [] board in
  if (listOfVals == (L.nub listOfVals)) then True else False

rowValidate :: Board -> Integer -> Bool
rowValidate board n =
  if n == 0 then singleRowValidate board n else
    let curStatus = singleRowValidate board n in
    if curStatus then rowValidate board (n-1) else curStatus

colValidate :: Board -> Integer -> Bool
colValidate board n =
  if n == 0 then singleColValidate board n else
    let curStatus = singleColValidate board n in
    if curStatus then colValidate board (n-1) else curStatus

blockValidate :: Board -> Coordinate -> Bool
blockValidate board coord =
  let (curX, curY) = coord in
  if (curX == 0) then
    if (curY == 0)
      then singleBlockValidate board coord else
        let curStatus = singleBlockValidate board coord in
        if curStatus
          then blockValidate board (curX, curY - blockWidth)
          else curStatus
  else if (curY == 0) then
    let curStatus = singleBlockValidate board coord in
    if curStatus
      then blockValidate board (curX - blockWidth, boardHeight - blockHeight)
      else curStatus
  else let curStatus = singleBlockValidate board coord in
    if curStatus
    then blockValidate board (curX, curY - blockHeight)
    else curStatus

validate :: Board -> Bool
validate board =
  let rowValid = rowValidate board (boardHeight - 1) in
  if rowValid then
    let colValid = colValidate board (boardWidth - 1) in
    if colValid then
      blockValidate board (boardWidth - blockWidth, boardHeight - blockHeight)
    else False
  else False

expand :: (Board, S.Set Board) -> ([Board], S.Set Board)
expand curState =
  let (curBoard, searches) = curState in
  let unfilled = getEmptyCells curBoard in
  let (cellToFill, _) = M.findMin unfilled in
  let newBoards = L.map (\n -> M.adjust (\_ -> n) cellToFill curBoard) possibleEntries in
  let possiblyValidBoards = L.filter (\b -> S.notMember b searches) newBoards in
  let validBoards = L.filter validate possiblyValidBoards in
  (validBoards, S.union searches $ S.fromList newBoards)

-- Monadic function that given a cache, will return either a
-- - Finished result (List of possible solutions)
-- - Unfinished computation (A list of suspended computation representing
--   the next steps to take)
step :: Board -> (ListT (State (S.Set Board)) Result)
step curBoard =
  ListT $
  get >>=
    (\curCache ->
      let (boards', history') = expand (curBoard, curCache) in
      let finished = L.filter checkFinished boards' in
      if not (L.null finished)
        then return [Finished finished]
        else
          return $ L.map (\bd -> Unfinished $ SearchM $ return bd) boards'
    )

-- Monadic functions that will implement the run of the algorithm
start :: Board -> SearchM Board
start initBoard = SearchM $ return initBoard

fullRun :: SearchM Board -> S.Set Board -> [Board]
fullRun smnd cache =
  let (res, cache') = runSearchM smnd step cache in
  let done = L.map (\r -> case r of Finished final -> final
                                    _ -> []) res in
  let done' = L.filter (\r -> not $ L.null r) done in
  if not (L.null done')
    then L.concat done
    else let Unfinished next = L.head res in fullRun next cache'

runSearchM :: SearchM a
           -> (a -> (ListT (State (S.Set Board)) Result))
           -> S.Set Board
           -> ([Result], S.Set Board)
runSearchM smonad contFn st =
  let SearchM cont = smonad in
  let temp = runContT cont contFn in
  runState (runListT temp) st

-------------------------------------------------------------------------------
-- Garbage Can

-- NOTE: Monads we need: State, Continuation, Non-determinism
-- NOTE: Maybe can be encoded by m0 for the non-determinism monad
-- NOTE: State should be universal; we want the user to specify the starting state
--       before we start the algorithm
-- NOTE: ContT r (ListT (State s)) a
-- (a -> m r) -> m r, m = ListT (State s)
-- (a -> ListT (State s) r) -> ListT (State s) r
-- (a -> (State s) [r]) -> (State s) [r]
-- (a -> (s -> ([r], s))) -> (s -> ([r], s))
-- [(a -> (s -> ([r], s))) -> (s -> ([r], s))]

-- s -> m (a, s), m = ContT Result [] = (a -> [r]) -> [r]
-- s -> (((a, s) -> [r]) -> [r])
-- newtype SearchM' a = SearchM' (StateT (S.Set Board) (ContT Result []) a)

-- runSearchM' :: SearchM' a
--             -> S.Set Board
--             -> ((a, S.Set Board) -> [Result])
--             -> [Result]
-- runSearchM' stmonad st contFn =
--   let SearchM' scmp = stmonad in
--   let temp = runStateT scmp st in
--   runContT temp contFn


-- pause :: SearchM ?
-- pause = SearchM $ ContT $
  -- \(k :: () -> (ListT (State (S.Set Board)) Result)) ->
  -- return (Unfinished $ (SearchM $ ContT $ \k -> k ()))
-- pause :: Board -> Result
-- pause nextBoard = Unfinished $ SearchM $ callCC $ \k -> k nextBoard

-- pause :: SearchM ?
-- pause = SearchM $ callCC $
-- \(k :: (Board -> ContT Result (ListT (State (S.Set Board))) b) -> ContT Result (ListT (State (S.Set Board))) a) ->
-- :: (Board ->
--       ((b -> (s -> ([Result], s))) -> (s -> ([Result], s)))) ->
--         ((Board -> (s -> ([Result], s))) -> (s -> ([Result], s)))
-- -- callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
-- -- callCC (f :: (a -> Cont r b) -> Cont r a) =
--   -- cont $ \(h :: (a -> r)) -> runCont ((f (\a -> cont $ \_ -> h a)) :: ((a -> r) -> r)) h
-- callCC f = cont $ \h -> runCont (f (\a -> cont $ \_ -> h a)) h
