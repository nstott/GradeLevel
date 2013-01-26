module NLP.ReadingLevel where 

	import NLP.Syllables
	import qualified Data.Text as T
	import Data.Char (isAlphaNum)



	-- http://en.wikipedia.org/wiki/Coleman-Liau_Index
	-- http://en.wikipedia.org/wiki/Flesch%E2%80%93Kincaid_readability_test

	fleschReadingEase :: String -> Float
	fleschReadingEase s = 206.835 - ( 1.015 * (fromIntegral w / fromIntegral sent)) - (84.6 * (fromIntegral syl / fromIntegral w))
	  where
	  	w = totalWords s
	  	sent = totalSentences s
	  	syl = sum $ map countSyllables $ words s 

	fleschKincaidGradeLevel :: String -> Float
	fleschKincaidGradeLevel s = 0.39 * (fromIntegral w / fromIntegral sent) + 11.8 * (fromIntegral  syl / fromIntegral w) - 15.59
	  where
	  	w = totalWords s
	  	sent = totalSentences s
	  	syl = sum $ map countSyllables $ words s 

	totalSentences :: String -> Int
	totalSentences s = length $ filter (\x -> len x > 2) options
	   where 
	     text = T.pack s
	     options = map T.unpack $ T.split (\x -> x == '.' || x == '?' || x == '!') text
	     len s = length $ filter isAlphaNum s


	colemanLiauIndex :: String -> Float
	colemanLiauIndex str = (0.0588 * l) - (0.296 * s) - 15.8
	  where
	  	l = 100.0 * (fromIntegral (length (filter isAlphaNum str)) / fromIntegral (totalWords str))
	  	s = 100.0 * (fromIntegral (totalSentences str) / fromIntegral (totalWords str))

	totalWords :: String -> Int
	totalWords = length . words 


