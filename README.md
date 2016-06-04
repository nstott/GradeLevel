Measure the grade level of text through the Coleman-Liau index, and the Flesch-Kincaid algorithms

see 	

http://en.wikipedia.org/wiki/Coleman-Liau_Index

http://en.wikipedia.org/wiki/Flesch%E2%80%93Kincaid_readability_test

functions are exported from NLP.ReadingLevel, 

fleschReadingEase :: String -> Float

fleschKincaidGradeLevel :: String -> Float

colemanLiauIndex :: String -> Float

there are also convenience functions totalWords, totalSentences there.

NLP.Syllables just tries to count the number of syllables in a word, which is more complicated thenit should be

only thought about using this for english, not sure on the linguistic rules of other languages, there will be nuances there I haven't adressed here

