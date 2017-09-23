module Util where
import Data.Word
import Data.ByteString
import Text.Printf


-- Replaces the nth element in a list.
replaceNth :: Int -> a -> [a] -> [a]
replaceNth n v xs =
  case Prelude.splitAt n xs of
    ([], [])     -> []
    (xs, [])     -> xs
    (xs, y:ys) -> xs ++ (v:ys)

-- Replaces the nth byte in a ByteString.
replaceNthWord8 :: Int -> Word8 -> ByteString -> ByteString
replaceNthWord8 n w = pack . replaceNth n w . unpack
