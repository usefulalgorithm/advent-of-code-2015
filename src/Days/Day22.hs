module Days.Day22 (day22) where

import           Control.Monad
import           Data.Either
import           Data.Function
import           Data.List
import           Data.Maybe
import qualified Data.Set        as S
import           Data.Tuple
import           Days.ReadPuzzle

data Type = Hero | Boss
  deriving (Show, Eq, Ord)

data Player = Player
  { playerType                                                     :: Type
  , playerHP, playerMP, playerMPSpent, playerAttack, playerDefense :: Int }
  deriving (Eq, Ord)

instance Show Player where
  show p
    | playerType p == Hero = show Hero ++ ": HP=" ++ show (playerHP p) ++ ", MP=" ++ show (playerMP p) ++ ", spent=" ++ show (playerMPSpent p) ++ ", DEF=" ++ show (playerDefense p)
    | otherwise = show Boss ++ ": HP=" ++ show (playerHP p) ++ ", ATK=" ++ show (playerAttack p)

hero :: Player
hero = Player Hero 50 500 0 0 0

parseBoss :: String -> Player
parseBoss input =
  case words <$> lines input of
    [["Hit", "Points:", hp], ["Damage:", d]] ->
      Player Boss (read hp) 0 0 (read d) 0
    _ -> error "bad boss"

type Effect = (Player, Player) -> (Player, Player)

applyDamage :: Int -> Player -> Player
applyDamage dmg player = player { playerHP = playerHP player - (max 1 (dmg - playerDefense player)) }

applyHealing :: Int -> Player -> Player
applyHealing health player = player { playerHP = playerHP player + health }

applyRecharge :: Int -> Player -> Player
applyRecharge mana player = player { playerMP = playerMP player + mana }

missile :: Effect
missile (attacker, defender) =
  case playerType attacker of
    Boss -> error "boss cannot cast missile"
    Hero -> (attacker, applyDamage 4 defender)

drain :: Effect
drain (attacker, defender) =
  case playerType attacker of
    Boss -> error "boss cannot cast drain"
    Hero -> (applyHealing 2 attacker, applyDamage 2 defender)

shield :: Effect
shield (attacker, defender) =
  case playerType attacker of
    Boss -> (attacker, defender { playerDefense = 7 })
    Hero -> (attacker { playerDefense = 7 }, defender)

unshield :: Effect
unshield (attacker, defender) =
  case playerType attacker of
    Boss -> (attacker, defender { playerDefense = 0 })
    Hero -> (attacker { playerDefense = 0 }, defender)

poison :: Effect
poison (attacker, defender) =
  case playerType attacker of
    Boss -> (applyDamage 3 attacker, defender)
    Hero -> (attacker, applyDamage 3 defender)

recharge :: Effect
recharge (attacker, defender) =
  case playerType attacker of
    Boss -> (attacker, applyRecharge 101 defender)
    Hero -> (applyRecharge 101 attacker, defender)

data Spell = Spell
  { spellName    :: String
  , spellCost    :: Int
  , spellEffects :: [Effect] }

instance Eq Spell where
  (==) = (==) `on` spellName

instance Ord Spell where
  spell1 `compare` spell2
    | length (spellEffects spell1) /= length (spellEffects spell2) = length (spellEffects spell1) `compare` length (spellEffects spell2)
    | otherwise = spellName spell1 `compare` spellName spell2
instance Show Spell where
  show spell = "(" ++ spellName spell ++ ")x" ++ show (length (spellEffects spell))

spells :: [Spell]
spells =
  [ Spell "Magic Missile" 53 [missile]
  , Spell "Drain" 73 [drain]
  , Spell "Shield" 113 ((replicate 6 shield) ++ [unshield])
  , Spell "Poison" 173 (replicate 6 poison)
  , Spell "Recharge" 229 (replicate 5 recharge)]

applySpellCost :: Spell -> Player -> Player
applySpellCost spell player
  | playerType player == Boss = error "boss cannot cast spell"
  | mana < 0 = error "negative mana"
  | otherwise = player'
  where mana = playerMP player - spellCost spell
        player' = player { playerMP = mana, playerMPSpent = playerMPSpent player + spellCost spell }

data GameState = GameState
  { gamePlayers :: (Player, Player)
  , gameSpells  :: [Spell] }
  deriving (Show, Eq, Ord)

gameAttacker :: GameState -> Player
gameAttacker state = fst $ gamePlayers state

gameDefender :: GameState -> Player
gameDefender state = snd $ gamePlayers state

gameHero :: GameState -> Player
gameHero state =
  case playerType (gameAttacker state) of
    Hero -> gameAttacker state
    Boss -> gameDefender state

castSpell :: Spell -> GameState -> GameState
castSpell spell state =
  let
    newAttacker = applySpellCost spell (gameAttacker state)
    nextStatePlayers = (newAttacker, gameDefender state)
   in case length (spellEffects spell) of
        0 -> error "invalid spell"
        1 -> state { gamePlayers = (head (spellEffects spell)) nextStatePlayers }
        _ -> state { gamePlayers = nextStatePlayers, gameSpells = reverse . sort $ spell:(gameSpells state) }

data Result = Result
  { resultWinner :: Type
  , resultMana   :: Int }
  deriving (Show, Eq, Ord)

checkFinish :: GameState -> Either Result GameState
checkFinish state = do
  when (isDead attacker) (Left (Result (playerType defender) heroMana))
  when (isDead defender) (Left (Result (playerType attacker) heroMana))
  Right state
    where (attacker, defender) = gamePlayers state
          isDead player = playerHP player <= 0
          heroMana = playerMPSpent $ gameHero state

applyEffect :: Effect -> GameState -> Either Result GameState
applyEffect effect state = checkFinish state'
  where players = gamePlayers state
        state' = state { gamePlayers = effect players }

extractEffect :: Spell -> (Effect, Maybe Spell)
extractEffect spell =
  case length (spellEffects spell) of
    0 -> error "spell with no effect"
    1 -> (head (spellEffects spell), Nothing)
    _ -> (head (spellEffects spell), Just (spell { spellEffects = tail $ spellEffects spell }))

doEffects :: GameState -> Either Result GameState
doEffects state =
  foldM (flip applyEffect) state' todoEffects
  where (todoEffects, nextSpells) = unzip . map extractEffect $ gameSpells state
        state' = state { gameSpells = map fromJust $ filter isJust nextSpells }

availableSpells :: Player -> GameState -> [Spell]
availableSpells player state = filter canCast spells
  where
    canCast spell = playerMP player >= spellCost spell &&
      all (/= spell) (gameSpells state)

swapRole :: GameState -> GameState
swapRole state = state { gamePlayers = swap $ gamePlayers state }

runRound :: S.Set GameState -> S.Set (Either Result GameState)
runRound states =
  S.unions $ map ((either (S.singleton . Left) doRound) . doEffects) $ S.toList states

doRound :: GameState -> S.Set (Either Result GameState)
doRound state =
  let (attacker, defender) = gamePlayers state in
  case playerType attacker of
    Hero ->
      case availableSpells attacker state of
        [] -> S.singleton (Left (Result Boss (playerMPSpent defender)))
        okSpells -> S.fromList $ map (checkFinish . swapRole . flip castSpell state) okSpells
    Boss ->
      S.singleton . checkFinish $ state { gamePlayers = (applyDamage (playerAttack attacker) defender, attacker) }

processRound :: S.Set (Either Result GameState) -> S.Set (Either Result GameState)
processRound statuses =
  done `S.union` runRound running'
  where
    best = minimum $ S.toList statuses
    (running, done) = S.partition isRight statuses
    running' = S.filter lessThanBest $ S.map getState running
    lessThanBest s =
      case best of
        Right _              -> True
        Left (Result Boss _) -> True
        Left (Result Hero m) -> m > playerMPSpent (gameHero s)
    getState status =
      case status of
        Left _  -> error "impossible"
        Right s -> s

doFight :: GameState -> Result
doFight start =
  case minimum . S.toList . fromJust . find (isLeft . maximum . S.toList) . iterate processRound . S.singleton $ Right start of
    Right _ -> error "no solution found"
    Left r  -> r

day22 :: IO ()
day22 = do
  input <- readPuzzle 22
  run input

run :: String -> IO ()
run input = do
  let boss = parseBoss input
  putStr "First: "
  runEasy boss
  putStr "Second: "
  runHard boss

runEasy :: Player -> IO ()
runEasy boss = do
  print . resultMana . doFight $ GameState (hero, boss) []

runHard :: Player -> IO ()
runHard boss = do
  print . resultMana . doFight $ GameState (hero, boss) [hardMode]
    where hardMode = Spell "Hard Mode" 0 (replicate 1000 hardModeEffect)
          hardModeEffect (attacker, defender) =
            case playerType attacker of
              Hero -> (applyDamage 1 attacker, defender)
              Boss -> (attacker, defender)
