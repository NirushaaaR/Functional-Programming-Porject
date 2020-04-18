module DrawScreen where

import System.IO
import System.Console.ANSI

import Pokemon.PokemonInfo (PokemonInfo (..))
import Pokemon.PokemonMove (MoveLogs (..))
import Util (AttackTurn (..))
import Pokemon.PokemonStat (currentHp)
import Data.List (partition)


drawAt :: (Int,Int) -> String -> IO ()
drawAt (row, col) draw = do
    setCursorPosition row col
    putStrLn draw

drawLargeSprite :: (Int, Int) -> [String] -> IO Int
drawLargeSprite (row, col) sprite = do
    let zipper = (zip sprite [0..])
    mapM_ (\(s, r) -> drawAt (row+r, col) s) zipper
    return $ snd $ last zipper


drawGameState :: PokemonInfo -> PokemonInfo -> [MoveLogs] -> IO ()
drawGameState player enemy logs = do
    resetScreen
    drawPlayerPokemon player logs
    drawEnemyPokemon enemy logs


drawPlayerPokemon :: PokemonInfo -> [MoveLogs] -> IO ()
drawPlayerPokemon pkInfo logs = do
    drawLargeSprite (0,0) ( (getSprite $ name pkInfo) )
    -- mapM_ print logs
    return ()


drawEnemyPokemon :: PokemonInfo -> [MoveLogs] -> IO ()
drawEnemyPokemon pkInfo logs = do
    drawLargeSprite (0,90) ( (getSprite $ name pkInfo) )
    -- mapM_ print logs
    return ()


drawInfoPokemon :: PokemonInfo -> [MoveLogs] -> [String]
drawInfoPokemon pkInfo logs = [ (name pkInfo)++" Hp: "++(show $ currentHp $ stats $ pkInfo) ] ++ logsList
            where logsList = map show logs




resetScreen ::  IO ()
resetScreen = do
    clearFromCursorToScreenBeginning
    setCursorPosition 0 0
    setSGR [Reset]
    




drawSprite :: String -> Color -> IO ()
drawSprite name color = do
    setSGR [SetColor Foreground Dull color]
    if name == "Charizard" then do
        putStr "                 .\"-,.__\n"
        putStr "                 `.     `.  ,\n"
        putStr "              .--'  .._,'\"-' `.\n"
        putStr "             .    .'         `'\n"
        putStr "             `.   /          ,'\n"
        putStr "               `  '--.   ,-\"'\n"
        putStr "                `\"`   |  \\\n"
        putStr "                   -. \\, |\n"
        putStr "                    `--Y.'      ___.\n"
        putStr "                         \\     L._, \\\n"
        putStr "               _.,        `.   <  <\\                _\n"
        putStr "             ,' '           `, `.   | \\            ( `\n"
        putStr "          ../, `.            `  |    .\\`.           \\ \\_\n"
        putStr "         ,' ,..  .           _.,'    ||\\l            )  '\".\n"
        putStr "        , ,'   \\           ,'.-.`-._,'  |           .  _._`.\n"
        putStr "      ,' /      \\ \\        `' ' `--/   | \\          / /   ..\\\n"
        putStr "    .'  /        \\ .         |\\__ - _ ,'` `        / /     `.`.\n"
        putStr "    |  '          ..         `-...-\"  |  `-'      / /        . `.\n"
        putStr "    | /           |L__           |    |          / /          `. `.\n"
        putStr "   , /            .   .          |    |         / /             ` `\n"
        putStr "  / /          ,. ,`._ `-_       |    |  _   ,-' /               ` \\\n"
        putStr " / .           \\\"`_/. `-_ \\_,.  ,'    +-' `-'  _,        ..,-.    \\`.\n"
        putStr ".  '         .-f    ,'   `    '.       \\__.---'     _   .'   '     \\ \\\n"
        putStr "' /          `.'    l     .' /          \\..      ,_|/   `.  ,'`     L`\n"
        putStr "|'      _.-\"\"` `.    \\ _,'  `            \\ `.___`.'\"`-.  , |   |    | \\\n"
        putStr "||    ,'      `. `.   '       _,...._        `  |    `/ '  |   '     .|\n"
        putStr "||  ,'          `. ;.,.---' ,'       `.   `.. `-'  .-' /_ .'    ;_   ||\n"
        putStr "|| '              V      / /           `   | `   ,'   ,' '.    !  `. ||\n"
        putStr "||/            _,-------7 '              . |  `-'    l         /    `||\n"
        putStr ". |          ,' .-   ,' ||               | .-.        `.      .'     ||\n"
        putStr " `'        ,'    `\".'    |               |    `.        '. -.'       `'\n"
        putStr "          /      ,'      |               |,'    \\-.._,.'/'\n"
        putStr "          .     /        .               .       \\    .''\n"
        putStr "        .`.    |         `.             /         :_,'.'\n"
        putStr "          \\ `...\\   _     ,'-.        .'         /_.-'\n"
        putStr "           `-.__ `,  `'   .  _.>----''.  _  __  /\n"
        putStr "                .'        /\"'          |  \"'   '_\n"
        putStr "               /_|.-'\\ ,\".             '.'`__'-( \\\n"
        putStr "                 / ,\"'\"\\,'               `/  `-.|\" mh\n"
    else if name == "Blastoise" then do
        putStr "                       _\n"
        putStr "            _,..-\"\"\"--' `,.-\".\n"
        putStr "          ,'      __.. --',  |\n"
        putStr "        _/   _.-\"' |    .' | |       ____\n"
        putStr "  ,.-\"\"'    `-\"+.._|     `.' | `-..,',--.`.\n"
        putStr " |   ,.                      '    j 7    l \\__\n"
        putStr " |.-'                            /| |    j||  .\n"
        putStr " `.                   |         / L`.`\"\"','|\\  \\\n"
        putStr "   `.,----..._       ,'`\"'-.  ,'   \\ `\"\"'  | |  l\n"
        putStr "     Y        `-----'       v'    ,'`,.__..' |   .\n"
        putStr "      `.                   /     /   /     `.|   |\n"
        putStr "        `.                /     l   j       ,^.  |L\n"
        putStr "          `._            L       +. |._   .' \\|  | \\\n"
        putStr "            .`--...__,..-'\"\"'-._  l L  \"\"\"    |  |  \\\n"
        putStr "          .'  ,`-......L_       \\  \\ \\     _.'  ,'.  l\n"
        putStr "       ,-\"`. / ,-.---.'  `.      \\  L..--\"'  _.-^.|   l\n"
        putStr " .-\"\".'\"`.  Y  `._'   '    `.     | | _,.--'\"     |   |\n"
        putStr "  `._'   |  |,-'|      l     `.   | |\"..          |   l\n"
        putStr "  ,'.    |  |`._'      |      `.  | |_,...---\"\"\"\"\"`    L\n"
        putStr " /   |   j _|-' `.     L       | j ,|              |   |\n"
        putStr "`--,\"._,-+' /`---^..../._____,.L',' `.             |\\  |\n"
        putStr "   |,'      L                   |     `-.          | \\j\n"
        putStr "            .                    \\       `,        |  |\n"
        putStr "             \\                __`.Y._      -.     j   |\n"
        putStr "              \\           _.,'       `._     \\    |  j\n"
        putStr "              ,-\"`-----\"\"\"\"'           |`.    \\  7   |\n"
        putStr "             /  `.        '            |  \\    \\ /   |\n"
        putStr "            |     `      /             |   \\    Y    |\n"
        putStr "            |      \\    .             ,'    |   L_.-')\n"
        putStr "             L      `.  |            /      ]     _.-^._\n"
        putStr "              \\   ,'  `-7         ,-'      / |  ,'      `-._\n"
        putStr "             _,`._       `.   _,-'        ,',^.-            `.\n"
        putStr "          ,-'     v....  _.`\"',          _:'--....._______,.-'\n"
        putStr "        ._______./     /',,-'\"'`'--.  ,-'  `.\n"
        putStr "                 \"\"\"\"\"`.,'         _\\`----...' mh\n"
        putStr "                        --------\"\"'\n"
        putStr "\n"
        putStr "\n"
    else do
        putStrLn "No Sprite Yet..."
    setSGR [Reset]
    

getSprite :: String -> [String]
getSprite name =
    if name == "Charizard" then
        ["                 .\"-,.__"] ++
        ["                 `.     `.  ,"] ++
        ["              .--'  .._,'\"-' `."] ++
        ["             .    .'         `'"] ++
        ["             `.   /          ,'"] ++
        ["               `  '--.   ,-\"'"] ++
        ["                `\"`   |  \\"] ++
        ["                   -. \\, |"] ++
        ["                    `--Y.'      ___."] ++
        ["                         \\     L._, \\"] ++
        ["               _.,        `.   <  <\\                _"] ++
        ["             ,' '           `, `.   | \\            ( `"] ++
        ["          ../, `.            `  |    .\\`.           \\ \\_"] ++
        ["         ,' ,..  .           _.,'    ||\\l            )  '\"."] ++
        ["        , ,'   \\           ,'.-.`-._,'  |           .  _._`."] ++
        ["      ,' /      \\ \\        `' ' `--/   | \\          / /   ..\\"] ++
        ["    .'  /        \\ .         |\\__ - _ ,'` `        / /     `.`."] ++
        ["    |  '          ..         `-...-\"  |  `-'      / /        . `."] ++
        ["    | /           |L__           |    |          / /          `. `."] ++
        ["   , /            .   .          |    |         / /             ` `"] ++
        ["  / /          ,. ,`._ `-_       |    |  _   ,-' /               ` \\"] ++
        [" / .           \\\"`_/. `-_ \\_,.  ,'    +-' `-'  _,        ..,-.    \\`."] ++
        [".  '         .-f    ,'   `    '.       \\__.---'     _   .'   '     \\ \\"] ++
        ["' /          `.'    l     .' /          \\..      ,_|/   `.  ,'`     L`"] ++
        ["|'      _.-\"\"` `.    \\ _,'  `            \\ `.___`.'\"`-.  , |   |    | \\"] ++
        ["||    ,'      `. `.   '       _,...._        `  |    `/ '  |   '     .|"] ++
        ["||  ,'          `. ;.,.---' ,'       `.   `.. `-'  .-' /_ .'    ;_   ||"] ++
        ["|| '              V      / /           `   | `   ,'   ,' '.    !  `. ||"] ++
        ["||/            _,-------7 '              . |  `-'    l         /    `||"] ++
        [". |          ,' .-   ,' ||               | .-.        `.      .'     ||"] ++
        [" `'        ,'    `\".'    |               |    `.        '. -.'       `'"] ++
        ["          /      ,'      |               |,'    \\-.._,.'/'"] ++
        ["          .     /        .               .       \\    .''"] ++
        ["        .`.    |         `.             /         :_,'.'"] ++
        ["          \\ `...\\   _     ,'-.        .'         /_.-'"] ++
        ["           `-.__ `,  `'   .  _.>----''.  _  __  /"] ++
        ["                .'        /\"'          |  \"'   '_"] ++
        ["               /_|.-'\\ ,\".             '.'`__'-( \\"] ++
        ["                 / ,\"'\"\\,'               `/  `-.|\" mh"]
    else if name == "Blastoise" then
        [""] ++
        [""] ++
        [""] ++
        ["                       _"] ++
        ["            _,..-\"\"\"--' `,.-\"."] ++
        ["          ,'      __.. --',  |"] ++
        ["        _/   _.-\"' |    .' | |       ____"] ++
        ["  ,.-\"\"'    `-\"+.._|     `.' | `-..,',--.`."] ++
        [" |   ,.                      '    j 7    l \\__"] ++
        [" |.-'                            /| |    j||  ."] ++
        [" `.                   |         / L`.`\"\"','|\\  \\"] ++
        ["   `.,----..._       ,'`\"'-.  ,'   \\ `\"\"'  | |  l"] ++
        ["     Y        `-----'       v'    ,'`,.__..' |   ."] ++
        ["      `.                   /     /   /     `.|   |"] ++
        ["        `.                /     l   j       ,^.  |L"] ++
        ["          `._            L       +. |._   .' \\|  | \\"] ++
        ["            .`--...__,..-'\"\"'-._  l L  \"\"\"    |  |  \\"] ++
        ["          .'  ,`-......L_       \\  \\ \\     _.'  ,'.  l"] ++
        ["       ,-\"`. / ,-.---.'  `.      \\  L..--\"'  _.-^.|   l"] ++
        [" .-\"\".'\"`.  Y  `._'   '    `.     | | _,.--'\"     |   |"] ++
        ["  `._'   |  |,-'|      l     `.   | |\"..          |   l"] ++
        ["  ,'.    |  |`._'      |      `.  | |_,...---\"\"\"\"\"`    L"] ++
        [" /   |   j _|-' `.     L       | j ,|              |   |"] ++
        ["`--,\"._,-+' /`---^..../._____,.L',' `.             |\\  |"] ++
        ["   |,'      L                   |     `-.          | \\j"] ++
        ["            .                    \\       `,        |  |"] ++
        ["             \\                __`.Y._      -.     j   |"] ++
        ["              \\           _.,'       `._     \\    |  j"] ++
        ["              ,-\"`-----\"\"\"\"'           |`.    \\  7   |"] ++
        ["             /  `.        '            |  \\    \\ /   |"] ++
        ["            |     `      /             |   \\    Y    |"] ++
        ["            |      \\    .             ,'    |   L_.-')"] ++
        ["             L      `.  |            /      ]     _.-^._"] ++
        ["              \\   ,'  `-7         ,-'      / |  ,'      `-._"] ++
        ["             _,`._       `.   _,-'        ,',^.-            `."] ++
        ["          ,-'     v....  _.`\"',          _:'--....._______,.-'"] ++
        ["        ._______./     /',,-'\"'`'--.  ,-'  `."] ++
        ["                 \"\"\"\"\"`.,'         _\\`----...' mh"] ++
        ["                        --------\"\"'"]
    else []