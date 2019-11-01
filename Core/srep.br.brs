dim tmp$*1024
! tmp$='more "xx more xx",  _____x ''ad more _____x ad'', this is more "aftd"  more more'
tmp$='more "xx more xx", ''ad more ad'', this is more "aftd"  more more'
pr 'before: *'&tmp$&'*'
pr 'after:  *'&fn_srepExcludeStringLiterals$(tmp$,'more','less')&'*'
end

def library fnSrepExcludeStringLiterals$*1024(in$*1024,srepFrom$,srepTo$)
	fnSrepExcludeStringLiterals$=fn_srepExcludeStringLiterals$(in$,srepFrom$,srepTo$)
fnend
def fn_srepExcludeStringLiterals$*1024(in$*1024,srepFrom$,srepTo$; ___,return$*1024,x,map$*1024,posI,posX,inLen,inQuoteS,inQuoteD,closeingQuoteD,closeingQuoteS)
	inLen=len(in$)
	map$=rpt$(' ',inLen)
	return$=in$
	for x=1 to inLen
		if in$(x:X)='"' and ~inQuoteS then
			! map$(x:x)='x' ! '"'
			if inQuoteD=0 then
				inQuoteD=1
			else if ~inQuoteS then
				inQuoteD=0
				closeingQuoteD=1
			end if
		else if in$(x:x)="'" and ~inQuoteD then
			! map$(x:x)='x' ! '"'
			if inQuoteS=0 then
				inQuoteS=1
			else if ~inQuoteD then
				inQuoteS=0
				closeingQuoteS=1
			end if
		end if
		if inQuoteD or closeingQuoteD then
			map$(x:x)='x' ! '"'
			closeingQuoteD=0
		else if inQuoteS or closeingQuoteS then
			map$(x:x)='x' ! "'"
			closeingQuoteS=0
		else 
			map$(x:x)='_'
		end if
	nex x
	pr ' in$=   *'&in$ &'*'
	pr 'map$=   *'&map$&'*'
	pause
	posI=0
	posI=pos(map$,'_',posI)
	do while posI<=inLen and posI>0
		posI=pos(map$,'_',posI+1) ! find next replaceable character
		posX=pos(map$,'x',posI+1) ! find next end of replaceable characters
		if posX>0 then posX-=1 else posX=inLen
		return$(posI:posX)=srep$(in$(posI:posX),srepFrom$,srepTo$)
		! pr posI,posX : pause
		in$=return$
	loop
	fn_srepExcludeStringLiterals$=return$
fnend
