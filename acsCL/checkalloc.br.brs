! determine if cash or accrual by checking for any 
! accounts payable numbers in the general ledger control file
autoLibrary
fnopenprn
open #trmstr=1: "Name=[Q]\CLmstr\TrMstr.H[cno],KFName=[Q]\CLmstr\TrIdx1.H[cno],Shr",internal,outIn,keyed 
open #tralloc=3: "Name=[Q]\CLmstr\TrAlloc.H[cno],KFName=[Q]\CLmstr\tralloc-idx.h[cno],Shr",internal,outIn,keyed 
open #bankmstr=4: "Name=[Q]\CLmstr\BankMstr.H[cno],KFName=[Q]\CLmstr\BankIdx1.H[cno],Shr",internal,outIn,keyed 
open #work=5: "Name="&env$('Temp')&"\Work."&session$&",SIZE=0,RecL=76,Replace",internal,output 
READ_TRMSTR: ! 
do
	read #trmstr,using 'Form pos 1,n 2,n 1,C 8,N 6,PD 10.2,pos 28,C 8,pos 71,N 1,X 6,N 1': trbank_code,trtcde,ck$,pd,ca1,vn$,,pcde,scd eof Finis
	if fndate_mmddyy_to_ccyymmdd(pd)=>20050530 then !  ! skip everything older than this date
		restore #tralloc,key>=cnvrt$("pic(zz)",trbank_code)&cnvrt$("pic(#)",trtcde)&ck$: ! Nokey 210
		totalalloc=0
		READ_TRALLOC: ! 
		do
			read #tralloc,using 'Form pos 1,N 2,N 1,c 8,C 12,PD 5.2,C 12,X 18,N 6,pos 80,N 1': bank_code,tcde,trck$,gl$,amt,iv$,ivd,gde eof READ_TRMSTR
			! pr TRBANK_CODE,BANK_CODE,TCDE,TRTCDE,CK$,TRCK$
			if trbank_code=bank_code and tcde=trtcde and ck$=trck$ then 
				totalalloc+=amt
			end if
		loop while trbank_code=bank_code and tcde=trtcde and ck$=trck$
		if totalalloc<>ca1 then 
			pr #255: "Check # "&ck$ &"  total check="&str$(ca1)&" total allocations ="&str$(totalalloc)&" date: "&str$(pd)
		end if
	end if
loop
Finis: ! 
fncloseprn
Xit: fnXit
