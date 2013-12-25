-- file: redo.hs
import System.Process
main = do
	_ <- createProcess $ shell "sh redo.do"	
	return ()

