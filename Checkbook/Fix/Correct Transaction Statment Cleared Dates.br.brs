 
! If a check has a statement cleared date of 7/31/18
! 	and it a record number lower than 3838 than
! 	change the statement cleared date to
! 	the end of the month it is written in.
 
 
autoLibrary
dim t$(0)*128,tN(0)
hTrans=fn_open('CL Transaction',mat t$,mat tN,mat form$)
on error goto Ertn
fnTop(program$)
matchClearDay=days('07/31/2018','mm/dd/ccyy')
olderThanTransDay=days('07/31/2018','mm/dd/ccyy')
do
	read #hTrans,using form$(hTrans): mat t$,mat tN eof EoTrans
	readCount+=1
	transDate=tN(tran_dateYy)*10000+tN(tran_dateMmdd)
	transDay=days(transDate,'yymmdd')
	statementClearDay=days(tN(tran_clearDate),'mmddyy')
	
	if rec(hTrans)<=3838 then
		passFilter1Count+=1
		! pr date$(transDay,'mm/dd/ccyy'),date$(matchClearDay,'mm/dd/ccyy')
		if statementClearDay=matchClearDay then
			passFilter2Count+=1
			if transDay<=olderThanTransDay then
				matchCount+=1
				pr date$(transDay,'mm/dd/ccyy');' ';date$(fnEndOfMonth(transDay),'mm/dd/ccyy')
				tN(tran_clearDate)=date(fnEndOfMonth(transDay),'mmddyy')
				rewrite #hTrans,using form$(hTrans): mat t$,mat tN
			end if
		end if
	end if
	
loop
EoTrans: !
pr 'readCount=';readCount
pr 'passFilter1Count=';passFilter1Count
pr 'passFilter2Count=';passFilter2Count
pr 'matchCount=';matchCount
end
Xit: fnXit
include: fn_open
include: Ertn
