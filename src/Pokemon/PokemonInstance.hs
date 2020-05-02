module Pokemon.PokemonInstance (
    charizard,
    blastoise,
    pikachu,
    getSprite,
    gengar
) where

import Pokemon.PokemonInfo (PokemonInfo (..))
import Pokemon.PokemonMove (Move (..), MoveEffect (..), MoveTarget (..))
import Pokemon.PokemonType (Type (..))
import Pokemon.PokemonStat (PokemonStats (PokemonStats), StatsModifer (StatsModifer))
import Pokemon.Status (Status (..))

-- Charizard --
charizard = PokemonInfo {
    name="Charizard",
    stats=(PokemonStats 50 50 12 9 15),
    pokemonType=Fire,
    moves=[flameThrowerMove, swordDanceMove, flyMove]
}
flameThrowerMove = Move {
    moveName="FlameThrower",
    moveEffect=[(DealDamage 25 0.0), (AttachStatus Burn (1,3) Opponent)],
    accuracy=100, 
    description="A powerfull fire move has a 1/3 chance to attach burn to op"
}
swordDanceMove = Move { 
    moveName="SwordDance", 
    moveEffect=[(ChangeStats (StatsModifer 0 0.5 0 0) Self)],
    accuracy=100, 
    description="A move that increase user attack by 1.5 percent"
}
flyMove = Move {
    moveName="Fly",
    moveEffect=[(AttachStatus Flying (1,1) Self)], 
    accuracy=100,
    description="A very powerful move that also hurt attacker by 50% of damage deal to opponent"
}
-- Charizard --

-- blastoise --
blastoise = PokemonInfo {
    name="Blastoise",
    stats=(PokemonStats 61 61 10 15 8),
    pokemonType=Water,
    moves=[hydroPumpMove, shellSmashMove, aquaTailMove]
}
hydroPumpMove = Move {
    moveName="HydroPump",
    moveEffect=[(DealDamage 35 0.0)],
    accuracy=50,
    description="The target is blasted by a huge volume of water launched under great pressure"
}
shellSmashMove = Move {
    moveName="Shell Smash",
    moveEffect=[(ChangeStats (StatsModifer 0 0.5 (-0.5) 0.5) Self)],
    accuracy=100,
    description="The user breaks its shell, which lowers Defense stat but raises its Attack and Speed stats"
}
aquaTailMove = Move {
    moveName="Aqua Tail",
    moveEffect=[(DealDamage 25 0.0)],
    accuracy=100,
    description="The user attacks by swinging its tail as if it were a vicious wave in a raging storm"
}
-- blastoise --

-- pikachu--
pikachu = PokemonInfo {
    name="Pikachu",
    stats=(PokemonStats 56 56 14 10 16),
    pokemonType=Electric,
    moves=[electroBall, voltTackle, thunderWave]
}
electroBall = Move {
    moveName="Electro Ball",
    moveEffect=[(DealDamage 25 0.0)],
    accuracy=100,
    description="The user hurls an electric orb at the target."
}
voltTackle = Move {
    moveName="Volt Tackle",
    moveEffect=[(DealDamage 45 0.5)],
    accuracy=70,
    description="The user throws an electrified tackle. It will hurts the user."
}
thunderWave = Move {
    moveName="Thuner Wave",
    moveEffect=[(AttachStatus Paralyzed (1,1)) Opponent],
    accuracy=70,
    description="A weak electric shock that is sure to cause paralysis if it hits."
}
-- pikachu--

-- venusaur --
venusaur = PokemonInfo {
    name="Venusaur",
    stats=(PokemonStats 78 78 11 14 10),
    pokemonType=Grass,
    moves=[poisonPowder, doubleEdge, razorLeaf]
}
poisonPowder = Move {
    moveName="Poison Powder",
    moveEffect=[(AttachStatus Poison (1,1)) Opponent],
    accuracy=100,
    description="The user scatters a cloud of poisonous dust that poisons the target."
}
doubleEdge = Move {
    moveName="Double-Edge",
    moveEffect=[(DealDamage 40 0.25)],
    accuracy=90,
    description="A reckless, life-risking tackle. This also damages the user quite a lot."
}
razorLeaf = Move {
    moveName="Razor Leaf",
    moveEffect=[(DealDamage 32 0.0)],
    accuracy=95,
    description="A sharp-edged leaf is launched to slash at the foe. It has a high critical-hit ratio."
}
-- venusaur --

-- gengar --
gengar = PokemonInfo {
    name="Gengar",
    stats=(PokemonStats 1 1 17 8 13),
    pokemonType=Grass,
    moves=[confuseRay, shadowBall, lick]
}
confuseRay = Move {
    moveName="Confuse Ray",
    moveEffect=[(AttachStatus Confuse (1,2)) Opponent],
    accuracy=80,
    description="The target is exposed to a sinister ray that triggers confusion."
}
shadowBall = Move {
    moveName="Shadow Ball",
    moveEffect=[(DealDamage 40 0.0)],
    accuracy=60,
    description="A reckless, life-risking tackle. This also damages the user quite a lot."
}
lick = Move {
    moveName="Lick",
    moveEffect=[(DealDamage 15 0.0)],
    accuracy=10,
    description="The foe is licked with a long tongue, causing damage. It may also paralyze the target."
}
-- gengar --

------ sprite --------
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
        "                 / ,\"'\"\\,'               `/  `-.|\""]
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
        "                 \"\"\"\"\"`.,'         _\\`----...'",
        "                        --------\"\"'"]
    else if name == "Pikachu" then
        [
    "                                             ,-.",
    "                                          _.|  '",
    "                                        .'  | /",
    "                                      ,'    |'",
    "                                     /      /",
    "                       _..----\"\"---.'      /",
    " _.....---------...,-\"\"                  ,'",
    " `-._  \\                                /",
    "     `-.+_            __           ,--. .",
    "          `-.._     .:  ).        (`--\"| \\",
    "               7    | `\" |         `...'  \\",
    "               |     `--'     '+\"        ,\". ,\"\"-",
    "               |   _...        .____     | |/    '",
    "          _.   |  .    `.  '--\"   /      `./     j",
    "         \\' `-.|  '     |   `.   /        /     /",
    "         '     `-. `---\"      `-\"        /     /",
    "          \\       `.                  _,'     /",
    "           \\        `                        .",
    "            \\                                j",
    "             \\                              /",
    "              `.                           .",
    "                +                          \\",
    "                |                           L",
    "                |                           |",
    "                |  _ /,                     |",
    "                | | L)'..                   |",
    "                | .    | `                  |",
    "                '  \\'   L                   '",
    "                 \\  \\   |                  j",
    "                  `. `__'                 /",
    "                _,.--.---........__      /",
    "               ---.,'---`         |   -j\"",
    "                .-'  '....__      L    |",
    "              \"\"--..    _,-'       \\ l||",
    "                  ,-'  .....------. `||'",
    "               _,'                /",
    "             ,'                  /",
    "            '---------+-        /",
    "                     /         /",
    "                   .'         /",
    "                 .'          /",
    "               ,'           /",
    "             _'....----\"\"\"\"\""
        ]

    else if name == "Venusaur" then 
        [
    "                           _._       _,._",
    "                        _.'   `. ' .'   _`.",
    "                ,\"\"\"/`\"\"-.-.,/. ` V'\\-,`.,--/\"\"\".\"-..",
    "              ,'    `...,' . ,\\-----._|     `.   /   \\",
    "             `.            .`  -'`\"\" .._   :> `-'   `.",
    "            ,'  ,-.  _,.-'| `..___ ,'   |'-..__   .._ L",
    "           .    \\_ -'   `-'     ..      `.-' `.`-.'_ .|",
    "           |   ,',-,--..  ,--../  `.  .-.    , `-.  ``.",
    "           `.,' ,  |   |  `.  /'/,,.\\/  |    \\|   |",
    "                `  `---'    `j   .   \\  .     '   j",
    "              ,__`\"        ,'|`'\\_/`.'\\'        |\\-'-, _,.",
    "       .--...`-. `-`. /    '- ..      _,    /\\ ,' .--\"'  ,'\".",
    "     _'-\"\"-    --  _`'-.../ __ '.'`-^,_`-\"\"\"\"---....__  ' _,-`",
    "   _.----`  _..--.'        |  \"`-..-\" __|'\"'         .\"\"-. \"\"'--.._",
    "  /        '    /     ,  _.+-.'  ||._'   \"\"\"\". .          `     .__\\",
    " `---    /        /  / j'       _/|..`  -. `-`\\ \\   \\  \\   `.  \\ `-..",
    ",\" _.-' /    /` ./  /`_|_,-\"   ','|       `. | -'`._,   L  \\ .  `.   |",
    "`\"' /  /  / ,__...-----| _.,  ,'            `|----.._`-.|' |. .` ..  .",
    "   /  '| /.,/   \\--.._ `-,' ,          .  '`.'  __,., '  ''``._ \\ \\`,'",
    "  /_,'---  ,     \\`._,-` \\ //  / . \\    `._,  -`,  / / _   |   `-L -",
    "   /       `.     ,  ..._ ' `_/ '| |\\ `._'       '-.'   `.,'     |",
    "  '         /    /  ..   `.  `./ | ; `.'    ,\"\" ,.  `.    \\      |",
    "   `.     ,'   ,'   | |\\  |       \"        |  ,'\\ |   \\    `    ,L",
    "   /|`.  /    '     | `-| '                  /`-' |    L    `._/  \\",
    "  / | .`|    |  .   `._.'                   `.__,'   .  |     |  (`",
    " '-\"\"-'_|    `. `.__,._____     .    _,        ____ ,-  j     \".-'\"'",
    "        \\      `-.  \\/.    `\"--.._    _,.---'\"\"\\/  \"_,.'     /-'",
    "         )        `-._ '-.        `--\"      _.-'.-\"\"        `.",
    "        ./            `,. `\".._________...\"\"_.-\"`.          _j",
    "       /_\\.__,\"\".   ,.'  \"`-...________.---\"     .\".   ,.  / \\",
    "              \\_/\"\"\"-'                           `-'--(_,`\"`-` "

        ]
    else if name == "Gengar" then
        ["                 |`._         |\\",
    "                 `   `.  .    | `.    |`.",
    "                  .    `.|`-. |   `-..'  \\           _,.-'",
    "                  '      `-. `.           \\ /|   _,-'   /",
    "              .--..'        `._`           ` |.-'      /",
    "               \\   |                                  /",
    "            ,..'   '                                 /",
    "            `.                                      /",
    "            _`.---                                 /",
    "        _,-'               `.                 ,-  /\"-._",
    "      ,\"                   | `.             ,'|   `    `.",
    "    .'                     |   `.         .'  |    .     `.",
    "  ,'                       '   ()`.     ,'()  '    |       `.",
    "'-.                    |`.  `.....-'    -----' _   |         .",
    " / ,   ________..'     '  `-._              _.'/   |         :",
    " ` '-\"\" _,.--\"'         \\   | `\"+--......-+' //   j `\"--.. , '",
    "    `.'\"    .'           `. |   |     |   / //    .       ` '",
    "      `.   /               `'   |    j   /,.'     '",
    "        \\ /                  `-.|_   |_.-'       /\\",
    "         /                        `\"\"          .'  \\",
    "        j                                           .",
    "        |                                 _,        |",
    "        |             ,^._            _.-\"          '",
    "        |          _.'    `'\"\"`----`\"'   `._       '",
    "        j__     _,'                         `-.'-.\"`",
    "          ',-.,' "
        ]
    else []





