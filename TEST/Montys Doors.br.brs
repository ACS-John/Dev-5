library 'S:\Core\Library': fnAddOneN
doorCount=3
howManyToEliminate=doorCount-2
tryCount=150000
for itteration=1 to 2
	doSwitchForSecondGuess=itteration-1
	successCount=0
	successFirstQuessCount=0
	failCount=0
	for try=1 to tryCount
		mat eliminatedDoor(0)
		mat priorGuess(0)
		pr f '2,2,C': str$(int(try/tryCount*100))&'%'
		doorWinner=fn_randomDoor
		guess=fn_randomDoor
		if doorWinner=guess then
			successFirstQuessCount+=1
		else if doorWinner<>guess then
			for x=1 to howManyToEliminate
			fnAddOneN(mat eliminatedDoor,fn_randomDoor(1,1))
			nex x
			if doSwitchForSecondGuess then
				fnAddOneN(mat priorGuess,guess)
				guess=fn_randomDoor(1,1)
				if guess=doorWinner then
					successCount+=1
				else
					failCount+=1
				end if
			else
				failCount+=1
			end if
		end if
		
		NextTry: !
	nex try
	if doSwitchForSecondGuess then 
		pr '**Switch For Second Guess: '&str$((successFirstQuessCount+successCount)/tryCount*100)&'% **'
	else 
		pr '**    Trust your instinct: '&str$((successFirstQuessCount+successCount)/tryCount*100)&'% **'
	end if
nex itteration
! pr 'successes on first guess=';successFirstQuessCount
! pr 'successes=';successCount
! pr 'failures=';failCount
end
def fn_randomDoor(; avoidEliminatedDoors,avoidGuess,avoidPriorGuess,___,returnN)
	returnN=0
	do
		returnN=int(rnd*doorCount)+1
		if avoidGuess and guess=returnN then returnN=0
		if avoidEliminatedDoors and srch(mat eliminatedDoor,returnN)>0 then returnN=0
		if avoidPriorGuess and srch(mat priorGuess,returnN)>0 then returnN=0
	loop until returnN<>0
	
	if returnN<1 or returnN>doorCount then
		pr bell;'invalid randomDoor: ';returnN
		pause
	end if
	
	fn_randomDoor=returnN
fnend
