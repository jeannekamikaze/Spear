import Spear.Sys.Timer

main = do
	initialiseTimingSubsystem
	wait 3
	putStrLn "Done"
	
	
wait secs = do
	timer <- start newTimer
	wait' secs timer
	
	
wait' secs timer = do
	timer' <- tick timer
	let t = getTime timer'
	
	putStrLn $ show t
	
	if t >= secs then return ()
	else wait' secs timer'
	