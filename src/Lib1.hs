module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = ["PC", "CPU", "GPU", "MB", "RAM", "Storage", "PSU", "Case", "GHz", "MHz", "GB", "TB", "DDR", "4", "5", "6", "LGA1200", "LGA1700", "AM4", "AM5", "SSD", "HDD", "W", "ATX", "Micro-ATX", "Add", "Remove", "Update", "Search", "Compare", "ShowBuild"]