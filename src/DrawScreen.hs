module DrawScreen where

import System.IO
import System.Console.ANSI

import Pokemon.PokemonInfo (PokemonInfo (..))
import Pokemon.PokemonMove (MoveLogs (..), moveLogToStr, splitLogs)
import Util (AttackTurn (..))
import Pokemon.PokemonStat (currentHp, maxHp)
import Pokemon.BattleState (PokemonState)
import Pokemon.Status (statusToColor)


drawLargeText :: (Int, Int) -> [String] -> IO Int
drawLargeText (row, col) sprite = do
    let zipper = (zip sprite [0..])
    mapM_ (\(s, r) -> drawAt (row+r, col) s) zipper
    return $ snd $ last zipper
    where
        drawAt :: (Int,Int) -> String -> IO ()
        drawAt (row, col) draw = do
            setCursorPosition row col
            putStrLn draw

drawLargeTextWithColor :: Color -> (Int, Int) -> [String] -> IO Int
drawLargeTextWithColor color pos sprite  = do
    setSGR [SetColor Foreground Vivid color]
    size <- drawLargeText pos sprite
    setSGR [Reset]
    return size

drawGameState :: PokemonState -> PokemonState -> [MoveLogs] -> AttackTurn -> IO ()
drawGameState player enemy logs turn = do
    resetScreen
    let (playerLogs, enemyLogs) = splitLogs logs turn
    putStrLn (take 150 $ cycle "=")
    size <- drawEnemyPokemon enemy enemyLogs
    size' <- drawPlayerPokemon size player playerLogs
    putStrLn (take 150 $ cycle "=")
    return ()


drawPlayerPokemon :: Int -> PokemonState -> [MoveLogs] -> IO Int
drawPlayerPokemon startAt = drawInfoPokemon (startAt,0) (startAt+25, 75)


drawEnemyPokemon :: PokemonState -> [MoveLogs] -> IO Int
drawEnemyPokemon = drawInfoPokemon (0,90) (10,40)


drawInfoPokemon :: (Int,Int) -> (Int,Int) -> PokemonState -> [MoveLogs] -> IO Int
drawInfoPokemon spritePos descPos (pkInfo, pkStatus) logs = do
    let desc = (name pkInfo)++" Hp: "++(show $ currentHp $ stats pkInfo)++"/"++(show $ maxHp $ stats pkInfo)++" "++(show pkStatus)
        logsString = map moveLogToStr logs
    drawLargeText descPos ( desc:" ":logsString )
    size <- drawLargeText spritePos ( (getSprite $ name pkInfo) )
    return size


resetScreen ::  IO ()
resetScreen = do
    clearFromCursorToScreenBeginning
    setCursorPosition 0 0
    setSGR [Reset]


getSprite :: String -> [String]
getSprite name =
    if name == "Charizard" then
        ["                 .\"-,.__",
        "                 `.     `.  ,",
        "              .--'  .._,'\"-' `.",
        "             .    .'         `'",
        "             `.   /          ,'",
        "               `  '--.   ,-\"'",
        "                `\"`   |  \\",
        "                   -. \\, |",
        "                    `--Y.'      ___.",
        "                         \\     L._, \\",
        "               _.,        `.   <  <\\                _",
        "             ,' '           `, `.   | \\            ( `",
        "          ../, `.            `  |    .\\`.           \\ \\_",
        "         ,' ,..  .           _.,'    ||\\l            )  '\".",
        "        , ,'   \\           ,'.-.`-._,'  |           .  _._`.",
        "      ,' /      \\ \\        `' ' `--/   | \\          / /   ..\\",
        "    .'  /        \\ .         |\\__ - _ ,'` `        / /     `.`.",
        "    |  '          ..         `-...-\"  |  `-'      / /        . `.",
        "    | /           |L__           |    |          / /          `. `.",
        "   , /            .   .          |    |         / /             ` `",
        "  / /          ,. ,`._ `-_       |    |  _   ,-' /               ` \\",
        " / .           \\\"`_/. `-_ \\_,.  ,'    +-' `-'  _,        ..,-.    \\`.",
        ".  '         .-f    ,'   `    '.       \\__.---'     _   .'   '     \\ \\",
        "' /          `.'    l     .' /          \\..      ,_|/   `.  ,'`     L`",
        "|'      _.-\"\"` `.    \\ _,'  `            \\ `.___`.'\"`-.  , |   |    | \\",
        "||    ,'      `. `.   '       _,...._        `  |    `/ '  |   '     .|",
        "||  ,'          `. ;.,.---' ,'       `.   `.. `-'  .-' /_ .'    ;_   ||",
        "|| '              V      / /           `   | `   ,'   ,' '.    !  `. ||",
        "||/            _,-------7 '              . |  `-'    l         /    `||",
        ". |          ,' .-   ,' ||               | .-.        `.      .'     ||",
        " `'        ,'    `\".'    |               |    `.        '. -.'       `'",
        "          /      ,'      |               |,'    \\-.._,.'/'",
        "          .     /        .               .       \\    .''",
        "        .`.    |         `.             /         :_,'.'",
        "          \\ `...\\   _     ,'-.        .'         /_.-'",
        "           `-.__ `,  `'   .  _.>----''.  _  __  /",
        "                .'        /\"'          |  \"'   '_",
        "               /_|.-'\\ ,\".             '.'`__'-( \\",
        "                 / ,\"'\"\\,'               `/  `-.|\" mh"]
    else if name == "Blastoise" then
        ["",
        "",
        "",
        "                       _",
        "            _,..-\"\"\"--' `,.-\".",
        "          ,'      __.. --',  |",
        "        _/   _.-\"' |    .' | |       ____",
        "  ,.-\"\"'    `-\"+.._|     `.' | `-..,',--.`.",
        " |   ,.                      '    j 7    l \\__",
        " |.-'                            /| |    j||  .",
        " `.                   |         / L`.`\"\"','|\\  \\",
        "   `.,----..._       ,'`\"'-.  ,'   \\ `\"\"'  | |  l",
        "     Y        `-----'       v'    ,'`,.__..' |   .",
        "      `.                   /     /   /     `.|   |",
        "        `.                /     l   j       ,^.  |L",
        "          `._            L       +. |._   .' \\|  | \\",
        "            .`--...__,..-'\"\"'-._  l L  \"\"\"    |  |  \\",
        "          .'  ,`-......L_       \\  \\ \\     _.'  ,'.  l",
        "       ,-\"`. / ,-.---.'  `.      \\  L..--\"'  _.-^.|   l",
        " .-\"\".'\"`.  Y  `._'   '    `.     | | _,.--'\"     |   |",
        "  `._'   |  |,-'|      l     `.   | |\"..          |   l",
        "  ,'.    |  |`._'      |      `.  | |_,...---\"\"\"\"\"`    L",
        " /   |   j _|-' `.     L       | j ,|              |   |",
        "`--,\"._,-+' /`---^..../._____,.L',' `.             |\\  |",
        "   |,'      L                   |     `-.          | \\j",
        "            .                    \\       `,        |  |",
        "             \\                __`.Y._      -.     j   |",
        "              \\           _.,'       `._     \\    |  j",
        "              ,-\"`-----\"\"\"\"'           |`.    \\  7   |",
        "             /  `.        '            |  \\    \\ /   |",
        "            |     `      /             |   \\    Y    |",
        "            |      \\    .             ,'    |   L_.-')",
        "             L      `.  |            /      ]     _.-^._",
        "              \\   ,'  `-7         ,-'      / |  ,'      `-._",
        "             _,`._       `.   _,-'        ,',^.-            `.",
        "          ,-'     v....  _.`\"',          _:'--....._______,.-'",
        "        ._______./     /',,-'\"'`'--.  ,-'  `.",
        "                 \"\"\"\"\"`.,'         _\\`----...' mh",
        "                        --------\"\"'"]
    else []