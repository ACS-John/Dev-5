! Replace S:\acsCL\RmTrans
! Remove Transactions

autoLibrary
fnTop(program$)
on error goto Ertn

dim de$*30,cap$*128,tr$(5)*35

right=1 : center=2
open #20: 'Name=[Q]\CLmstr\Company.h[cno],Shr',internal,input 
read #20,using 'form pos 417,N 1': rcn
close #20:
open #trmstr:=fnH: 'Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx1.h[cno]',internal,outIn,keyed
open #work1:=fnH: 'Name=[Q]\CLmstr\Work1.[wsid],version=2,Size=0,RecL=84,Replace',i,outi,r
open #tralloc:=fnH: 'Name=[Q]\CLmstr\TrAlloc.h[cno],KFName=[Q]\CLmstr\TrAlloc-idx.h[cno]',i,i,k
open #work2=fnH: 'Name=[Q]\CLmstr\Work2.[wsid],version=2,Size=0,RecL=80,Replace',i,outi,r
fnTos
	mylen=21 : mypos=mylen+2 : lc=0
	fnLbl(lc+=1,1,'Oldest Retained Date:',mylen,right)
	fnTxt(lc,mypos,10,0,0,'1003')
	resp$(1)=str$(date('ccyymmdd')-50000)
	lc+=1
	if rcn=1 then
		fnLbl(lc+=1,1,'All cleared transactions with a',mylen*2,center)
	else
		fnLbl(lc+=1,1,'All transactions with a',mylen*2,center)
	end if
	fnLbl(lc+=1,1,'date prior to this date will be removed.',mylen*2,center)
	fnCmdSet(2)
ckey=fnAcs(mat resp$)
if ckey=5 or ckey=99 then 
	goto Xit 
else 
	rd1=val(resp$(1))
end if
! fnwait
READ_TRMSTR: !
	read #trmstr,using 'form pos 1,G 2,G 1,C 8,G 6,PD 10.2,C 8,C 35,G 1,G 6,G 1': bank_code,tcde,tr$(1),tr$(2),tr3,tr$(4),tr$(5),pcde,clr,scd eof END1
	if fndate_mmddyy_to_ccyymmdd(val(tr$(2)))>=rd1 then goto KEEP
	if tr3=0 and uprc$(trim$(tr$(5)))<>'VOID' then delete #trmstr: : goto READ_TRMSTR
	if rcn><1 then goto READ_TRMSTR
	if clr=0 then goto KEEP
goto READ_TRMSTR
KEEP: !
	write #work1,using 'form pos 1,G 2,G 1,C 8,G 6,pd 10.2,C 8,C 35,G 1,G 6,G 1,2*PD 3': bank_code,tcde,tr$(1),tr$(2),tr3,tr$(4),tr$(5),pcde,clr,scd
	restore #tralloc:
	key$=cnvrt$('Pic(ZZ)',bank_code)&str$(tcde)&tr$(1)
	restore #tralloc,key>=key$: nokey EO_TRALLOC
	READ_TRALLOC: !
		read #tralloc,using 'form pos 1,C 11,C 12,PD 5.2,C 30,G 6,X 3,C 12,N 1': newkey$,gl$,amt,de$,ivd,po$,postd eof EO_TRALLOC
		if key$<>newkey$ then goto EO_TRALLOC
		write #work2,using 'form pos 1,C 11,C 12,PD 5.2,C 30,G 6,PD 3,C 12,N 1': newkey$,gl$,amt,de$,ivd,0,po$,postd
	goto READ_TRALLOC
EO_TRALLOC: !
goto READ_TRMSTR
 
END1: !
	close #work1:
	close #work2:
	close #trmstr,free:
	close #tralloc,free:
	execute 'Rename [Q]\CLmstr\Work1.[wsid] '&'[Q]\CLmstr\TRmstr.h[cno] -n'
	execute 'Rename [Q]\CLmstr\Work2.[wsid] '&'[Q]\CLmstr\TrAlloc.h[cno] -n'
	execute 'Index [Q]\CLmstr\TrMstr.h[cno]'&' '&'[Q]\CLmstr\TrIdx1.h[cno] 1 11 Replace DupKeys -n'
	execute 'Index [Q]\CLmstr\TrMstr.h[cno]'&' '&'[Q]\CLmstr\TrIdx2.h[cno] 28/1 8/11 Replace DupKeys -n'
	execute 'Index [Q]\CLmstr\TrMstr.h[cno]'&' '&'[Q]\CLmstr\TrIdx3.h[cno] 16/12/4 2/4/8 Replace DupKeys -n'
	execute 'Index [Q]\CLmstr\TrAlloc.h[cno]'&' '&'[Q]\CLmstr\TrAlloc-idx.h[cno] 1 11 Replace DupKeys -n'
goto Xit
 
Xit: fnXit
 
include: ertn
