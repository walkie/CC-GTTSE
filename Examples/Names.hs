{-# LANGUAGE DeriveDataTypeable #-}

module Examples.Names where


import CC.Syntax


-- some simple binary dimensions
dimA = Dim "A" ["a","b"]
dimB = Dim "B" ["c","d"]
dimC = Dim "C" ["e","f"]

-- some binary choices
chcA = chc "A"
chcB = chc "B"
chcC = chc "B"

-- some binary choices for plain objects
chc'A = chc' "A"
chc'B = chc' "B"
chc'C = chc' "C"

