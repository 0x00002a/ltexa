module PrettyPrint where

class PrettyPrintable a where
  pretty_print :: a -> IO ()
  pretty_println :: a -> IO ()
  pretty_println p = pretty_print p >> putStr "\n"
