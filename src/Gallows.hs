module Gallows where

gallows :: [String]
gallows = [
    "",
    "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n||=====================",
    "||\n||\n||\n||\n||\n||\n||\n||\n||\n||\n||\n||\n||\n||\n||\n||=====================",
    "||   //\n\
    \||  //\n\
    \|| //\n\
    \||//\n\
    \||/\n\
    \||\n\
    \||\n\
    \||\n\
    \||\n\
    \||\n\
    \||\n\
    \||\n\
    \||\n\
    \||\n\
    \||\n\
    \||=====================",
    "||=====================\n\
    \||   //\n\
    \||  //\n\
    \|| //\n\
    \||//\n\
    \||/\n\
    \||\n\
    \||\n\
    \||\n\
    \||\n\
    \||\n\
    \||\n\
    \||\n\
    \||\n\
    \||\n\
    \||\n\
    \||=====================",
    "||=====================\n\
    \||   //              |\n\
    \||  //              _|_\n\
    \|| //              |. .|\n\
    \||//               |_-_|\n\
    \||/\n\
    \||\n\
    \||\n\
    \||\n\
    \||\n\
    \||\n\
    \||\n\
    \||\n\
    \||\n\
    \||\n\
    \||\n\
    \||=====================",
    "||=====================\n\
    \||   //              |\n\
    \||  //              _|_\n\
    \|| //              |. .|\n\
    \||//               |_-_|\n\
    \||/                 _|_\n\
    \||                   |\n\
    \||                   |\n\
    \||                   |\n\
    \||                  _|_\n\
    \||\n\
    \||\n\
    \||\n\
    \||\n\
    \||\n\
    \||\n\
    \||=====================",
    "||=====================\n\
    \||   //              |\n\
    \||  //              _|_\n\
    \|| //              |. .|\n\
    \||//               |_-_|\n\
    \||/                 _|_\n\
    \||                   | \\\n\
    \||                   |  |\n\
    \||                   |  |\n\
    \||                  _|_ |\n\
    \||\n\
    \||\n\
    \||\n\
    \||\n\
    \||\n\
    \||\n\
    \||=====================",
    "||=====================\n\
    \||   //              |\n\
    \||  //              _|_\n\
    \|| //              |. .|\n\
    \||//               |_-_|\n\
    \||/                 _|_\n\
    \||                 / | \\\n\
    \||                |  |  |\n\
    \||                |  |  |\n\
    \||                | _|_ |\n\
    \||\n\
    \||\n\
    \||\n\
    \||\n\
    \||\n\
    \||\n\
    \||=====================",
    "||=====================\n\
    \||   //              |\n\
    \||  //              _|_\n\
    \|| //              |. .|\n\
    \||//               |_-_|\n\
    \||/                 _|_\n\
    \||                 / | \\\n\
    \||                |  |  |\n\
    \||                |  |  |\n\
    \||                | _|_ |\n\
    \||                     \\\n\
    \||                      |\n\
    \||                      |\n\
    \||                      |\n\
    \||\n\
    \||\n\
    \||=====================",
    "||=====================\n\
    \||   //              |\n\
    \||  //              _|_\n\
    \|| //              |x x|\n\
    \||//               |_-_|\n\
    \||/                 _|_\n\
    \||                 / | \\\n\
    \||                |  |  |\n\
    \||                |  |  |\n\
    \||                | _|_ |\n\
    \||                 /   \\\n\
    \||                |     |\n\
    \||                |     |\n\
    \||                |     |\n\
    \||\n\
    \||\n\
    \||====================="
    ]


buildGallows :: Int -> String
buildGallows step = gallows !! step
