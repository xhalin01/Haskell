{-
Funkcionalny projekt RKA-2-DKA
Autor: Michael Halinár
login: xhalin01
-}

import Data.List.Split
import System.Environment
import Data.List
import System.IO
import Data.Maybe
import Data.Char
import Text.Printf
import Foreign.Ptr
import Data.Typeable
import Debug.Trace

debug = flip trace

--Data structure wich represents finite state machine
data FiniteStateMachine = FiniteStateMachine {
    states :: [[Char]],
    alphabet :: [[Char]],
    transitions :: [Transition],
    start :: [Char],
    finalStates :: [[Char]]
} deriving (Show)

--Data structure for transitions
data Transition = Transition {
    begin :: [Char],
    end :: [Char],
    letter :: [Char]
} deriving (Show,Eq)

--Data structure for computing eps closure
data EpsTransition = EpsTransition {
    src :: [[Char]],
    dst :: [[Char]],
    symbol :: [Char],
    eps :: [[Char]]
} deriving (Show,Eq)

---------------------------------------------
--Arguments parsing
checkArgs args = if length args > 2 || length args < 1
    then False
    else if (args !! 0) /= "-i" && (args !! 0) /= "-t"
        then False
        else True

---------------------------------------------
--Input parsing to FSM structure

getFSM c = FiniteStateMachine {
            states = getStates (c !! 0),
            alphabet = nub (getAlphabet (drop 3 c)),
            transitions = getTransitions (drop 3 c),
            start = (c !! 1),
            finalStates = getFinalStates (c !! 2)
        } 

getAlphabet content = map (\x-> ((splitOn "," (x)) !! 1)) content    
getStates content = (splitOn "," content)
getFinalStates content = (splitOn "," content)
getTransitions content = tr
    where
        tr = map (\x-> tr' (splitOn "," x)) content
        tr' x =Transition {begin = (x !! 0),end = (x !! 2),letter = (x !! 1)}

----------------------------------------------
--Printing the finite state machine
printStates states n = do
    if((n) > 0)
        then do
            if((n-1)/=0)
                then do
                    putStr (states !! (n-1))
                    putChar (',')                    
                else do
                    putStr (states!!(n-1))
                    putChar ('\n')
            printStates states (n-1)
        else return ()

printStartState s = do
    putStr s
    putChar '\n'


getListOfTranstitions transitions = (map (\x-> gl x) transitions)
gl x = (begin x)++","++(letter x)++","++(end x)


printTransitions transitions = do
    if(transitions/=[])
        then do
            putStrLn (transitions !! 0)
            printTransitions (tail transitions)
        else return ()

prntFSM states start end transitions = do
    printStates (reverse states) (length states)
    printStartState start
    printStates (reverse end) (length end)
    printTransitions (sort $ getListOfTranstitions(transitions))

----------------------------------------------
--merge twolists
merge [] ys = ys
merge (x:xs) ys = x:merge ys xs  

myMap function array  = concat (map (\x-> function x) array)

----------------------------------------------
--EPS closure functions

--get all ending states from given transitions
getClosure temp fsm alphabet = e 
    where
        tmpTrans =  myMap (\x-> getNextStates temp x (transitions fsm)) alphabet
        preFinalStates = myMap (\x-> while temp tmpTrans (transitions fsm) (0) x) alphabet
        e = (nub (merge temp preFinalStates))

--cycle which finds all states reachable with epsilon transitions
while old new transitions oldCount symbol =      
    if ((length(nub old)) == (oldCount))
        then old
        else while (nub(merge new old)) (getNextStates new symbol (transitions)) transitions (length old) symbol

--function finds all states reachable from transitions with given symbol
getNextStates states symbol transitions =getEnd (concat ((getNextStates' states symbol transitions)))
getNextStates' states symbol transitions = map (\x -> (filter (\y-> begin y==x && symbol==letter y) transitions) ) states

--------------------------------------------
--Get ending state from transition
getEnd states = map (\x-> end x) states

---------------------------------------------
--Creating all epsTransitions till the new ones are being created
makeAllEpsTransitions epsTransitions newCount oldCount fsm = 
    if(oldCount==newCount)
        then epsTransitions
        else temp
            where
                oldCount = (length epsTransitions)
                newCount = (length (makeNewEpsTransitions epsTransitions fsm))
                temp = makeAllEpsTransitions (makeNewEpsTransitions epsTransitions fsm) newCount oldCount fsm

--Creating all possible transitions, with epsilon or some symbol
makeNewEpsTransitions oldTransitions fsm = nub (oldTransitions ++ (concat new))
    where
        myAlphabet = filter (\x-> x/="" ) (alphabet fsm)
        new = map (\x -> makeNewEpsTransitions' oldTransitions fsm x) myAlphabet


makeNewEpsTransitions' epsTransitions fsm symbol = map (\x-> makeNewEpsTransitions'' x fsm symbol) epsTransitions
makeNewEpsTransitions'' epsTransition fsm symbol = t
            where t = EpsTransition   {
                    src = sort(dst epsTransition),
                    symbol = symbol,
                    eps = sort(getNextStates (src t) symbol (transitions fsm)),
                    dst = sort(getClosure (eps t) fsm [""])
                } 

----------------------------------------------
--Creating starting epsTransition from staring state for each symbol from alphabet
makeStartEpsTransitions start alphabet transitions fsm = map (\s-> makeStartEpsTransitions' start s transitions fsm) alphabet
makeStartEpsTransitions' start s transitions fsm = EpsTransition {
                src = sort(start),
                symbol = s,
                eps = sort(getNextStates (start) s (transitions)),
                dst = sort(getClosure (getNextStates (start) s (transitions)) fsm [""])
            }
-----------------------------------------------
--Creating deterministic fsm from EpsTransitions
makeFinalStates states finalStates = concat(map (\x-> mfs states x) finalStates)
mfs states finalState = map (\x-> mfs2 x finalState states) states
mfs2 state finalState states =
    if(elem finalState state)
        then (getIndex state states)
        else []

--
makeTransitions transitions states = map (\x-> gnt x states) transitions
gnt transition states = Transition {
    begin = getIndex (src transition) states,
    letter = symbol transition,
    end = getIndex (dst transition) states
    } 

getIndex x array = show $ (fromJust $ elemIndex x array) + 1

----------------------------------------------
--sort transitions
--sortTransitions transitions = sorted
--    where
--        lowest=findLowest (transitions\\(transitions !! 0)) (transitions !! 0)
--        sorted=while3 (transitions\\(transitions !! 0)) (transitions !! 0) lowest
--
--while3 old new toAppend =
--    if(old==[])
--        then new
--        else (old\\toAppend) (new ++ toAppend) (findLowest old)

--findLowest transitions lowest = 
--    if(transitions==[])
--        then lowest
--        else 
--            if((begin (head transitions)) < (begin lowest))
--                then findLowest (tail transitions ((head transitions)))
--                else findLowest (tail transitions (lowest))

----------------------------------------------
--Validate input
validateFsm myLines fsm = do
    validateStart fsm
    validateFinalState fsm
    validateFsmWithoutTransitions fsm myLines
    validateReachableFinalState fsm
    validateNonExistingTransitions fsm myLines
    return ()

--checking reachability of at least one final state
validateReachableFinalState fsm = do
    let reachableStates = getClosure [start fsm] fsm (alphabet fsm)
    --print reachableStates
    let reachableFinalStates = map (\x-> elem x reachableStates) (finalStates fsm)
    let finalStateIsReachable = elem True reachableFinalStates
    if(finalStateIsReachable==False)
        then error "Final state is not reachable"
        else return ()

--checking if start state exist
validateStart fsm = do
    if((start fsm) == "\r")
        then error "Non-existing start state"
        else return ()

--checking if final state exist
validateFinalState fsm = do
    if((finalStates fsm)== ["\r"])
        then error "Non-existing final state"
        else return ()

--If fsm has 3 lines and starting state is not equal to final state - error
validateFsmWithoutTransitions fsm myLines= do
    if((length myLines) == 3)
        then if((start fsm)/=(((finalStates fsm) !! 0)))
            then error "Non-existing transitions"
            else return ()
        else return ()

--if transitions exist check if they are valid(from existing state to existing state)
validateNonExistingTransitions fsm myLines = do
    if((length myLines) > 3)
        then do
            let statesFromTr = nub ( merge ( map (\x-> end x) (transitions fsm) ) [(start fsm)] )
            if((statesFromTr \\ (states fsm)) /= [])
                then error "Not valid transitions"
                else return ()
        else return ()

----------------------------------------------
--checking if fsm has at least 3 lines(and "\n")
validateInputLines myLines = do
    if((length myLines) < 3)
        then error "Not correct FSM"
        else return ()

----------------------------------------------
--MAIN
main = do  
    arguments <- getArgs
    if checkArgs arguments == True
        then do
            input <- if(length arguments) == 2
                then openFile (arguments !! 1) ReadMode
                else return stdin
            content <- hGetContents input

            let myLines = init (splitOn "\n" content)
            validateInputLines myLines

            let fsm = getFSM myLines
            
            --print (myLines)
            --print (fsm)

            validateFsm myLines fsm


            if(head arguments=="-i")
                then do
                    prntFSM (states fsm) (start fsm) (finalStates fsm) (reverse (transitions fsm))
                else do

                    
                    let startingState = getClosure [start fsm] fsm [""]
                    let myAlphabet = filter (\x-> x/="") (alphabet fsm)
                            
                    let startingEpsTransition = makeStartEpsTransitions startingState (myAlphabet) (transitions fsm) fsm

                    let allEpsTransition = makeAllEpsTransitions startingEpsTransition 1 0 fsm

                    let nonRelevantTr = filter (\x -> dst x==[] || eps x==[]) allEpsTransition --odstranenie stavov s prazdnymi mnozinami

                    let deteterministicTransitions = allEpsTransition\\nonRelevantTr

                    let statesArray = nub (merge [sort(startingState)] (sort(map (\x-> dst x) deteterministicTransitions)))

                    --print (statesArray)

                    let newFsm = FiniteStateMachine {
                        alphabet = filter (\x -> x/="") (alphabet fsm),
                        states = map (\x-> getIndex x statesArray) (statesArray),
                        transitions = makeTransitions deteterministicTransitions statesArray,
                        start = getIndex (sort startingState) statesArray,
                        finalStates = nub(filter (\x-> x/="") (makeFinalStates statesArray (finalStates fsm)))
                    }

                    --print (sort $ getListOfTranstitions( transitions newFsm))

                    prntFSM (states newFsm) (start newFsm) (finalStates newFsm) (reverse (transitions newFsm))
                    --printStates (finalStates newFsm) (length (finalStates (newFsm)))

        else
            error "Wrong arguments."