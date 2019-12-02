pr 'test 1: change spaces into not spaces' ! r:
tmp$='"xx more xx", ''ad more ad'',_____xs more "aftd"  more more'
pr 'before: *'&tmp$&'*'
pr 'after:  *'&fn_srepExcludeStringLiterals$(tmp$,' ','')&'*' ! /r

pr 'test 2: change more into less' ! r:
dim tmp$*1024
! tmp$='more "xx more xx",  _____x ''ad more _____x ad'', this is more "aftd"  more more'
tmp$='more "xx more xx", ''ad more ad'', this is more "aftd"  more more'
pr 'before: *'&tmp$&'*'
pr 'after:  *'&fn_srepExcludeStringLiterals$(tmp$,'more','less')&'*' ! /r
end


def library fnSrepExcludeStringLiterals$*1024(in$*1024,srepFrom$,srepTo$)
	fnSrepExcludeStringLiterals$=fn_srepExcludeStringLiterals$(in$,srepFrom$,srepTo$)
fnend
def fn_srepExcludeStringLiterals$*1024(in$*1024,srepFrom$,srepTo$; ___,return$*1024,x,map$*1024,elementToRep,posI,posX,inLen,inQuoteS,inQuoteD,closeingQuoteD,closeingQuoteS)
	dim startPoints(0)
	return$=in$
	
	do
		mat startPoints(0)
		inLen = len(in$)
		map$  = rpt$(' ',inLen)
		
		for x=1 to inLen
			if in$(x:x)='"' and ~inQuoteS then
				if inQuoteD=0 then
					inQuoteD=1
				else if ~inQuoteS then
					inQuoteD=0
					closeingQuoteD=1
				end if
			else if in$(x:x)="'" and ~inQuoteD then
				if inQuoteS=0 then
					inQuoteS=1
				else if ~inQuoteD then
					inQuoteS=0
					closeingQuoteS=1
				end if
			end if
			if inQuoteD or closeingQuoteD then
				map$(x:x)='x' 
				closeingQuoteD=0
			else if inQuoteS or closeingQuoteS then
				map$(x:x)='x' 
				closeingQuoteS=0
			else 
				map$(x:x)='_'
			end if
			
			! Added by Mike D Miller
			if (x == 1 and map$(x:x) == '_') or (x > 1 and map$(x:x) == '_' and map$(x - 1:x - 1) == 'x') then
				mat startPoints(udim(mat startPoints) + 1)
				startPoints(udim(mat startPoints)) = x
			end if
		nex x
		posI=0
		posX=0
		
		elementToRep += 1
		
		if elementToRep <= udim(mat startPoints) then
			posI = startPoints(elementToRep)
			posX = pos(map$, 'x', posI)
			if ~posX then
				posX = inLen
			end if
			
			return$(posI:posX) = srep$(in$(posI:posX), srepFrom$, srepTo$)
			in$ = return$
		end if
	loop while elementToRep < udim(mat startPoints)
	fn_srepExcludeStringLiterals$=return$
fnend