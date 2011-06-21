{-# LANGUAGE DeriveDataTypeable #-}

module Examples.Names where


import CC.Syntax


-- some simple binary dimensions
dimA = Dim "A" ["a","a'"]
dimB = Dim "B" ["b","b'"]
dimC = Dim "C" ["c","c'"]

-- some binary choices
chcA = chc "A"
chcB = chc "B"
chcC = chc "B"

-- some binary choices for plain objects
chc'A = chc' "A"
chc'B = chc' "B"
chc'C = chc' "C"

-- encapsulated choices
dimchcA = dimA .chcA
dimchcB = dimB .chcB
dimchcC = dimC .chcC
