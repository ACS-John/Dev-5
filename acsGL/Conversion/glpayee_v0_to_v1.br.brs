! Replace S:\acsGL\Conversion\glpayee_v0_to_v1
! converts general ledger payee files to new rln
! from recl=127 to recl=276 and version 1
def library fnglpayee_v0_to_v1
		autoLibrary
		on error goto Ertn
 
		dim cap$*128
		dim nam$*35,ad1$*20,ad2$*20,csz$*20,ss$*11,ph$*12,contact$*30,email$*50
		dim fax$*12,myact$*20
 
 
		fnStatus('updating Payee file format.')
		open #1: "Name=[Q]\GLmstr\GL1099.h[cno],KFName=[Q]\GLmstr\GL109IDX.h[cno]",internal,outIn,keyed ioerr ignore
		if exists("[Q]\GLmstr\paymstr.h[cno]")=0 then open #2: "Name=[Q]\GLmstr\paymstr.h[cno],KFName=[Q]\GLmstr\payidx1.h[cno],RecL=276,kps=1,kln=8,replace",internal,outIn,keyed: version(2,1): close #2:
		open #2: "Name=[Q]\GLmstr\paymstr.h[cno]",internal,outIn,relative  ! open #2: "Name=[Q]\GLmstr\paymstr.h[cno],KFName=[Q]\GLmstr\payidx1.h[cno]",internal,outIn,keyed
		if rln(2)<>276 then
			close #2:
			fnCopy("[Q]\GLmstr\paymstr.h[cno]",env$('temp')&"\WORK."&session$,276)
			fnCopy(env$('temp')&"\WORK."&session$,"[Q]\GLmstr\paymstr.h[cno]")
			open #2: "Name=[Q]\GLmstr\paymstr.h[cno]",internal,outIn,relative  ! open #2: "Name=[Q]\GLmstr\paymstr.h[cno],KFName=[Q]\GLmstr\payidx1.h[cno]",internal,outIn,keyed
		end if
		do
			read #1,using "form pos 1,c 8,c 35,3*c 20,pd 5.2,n 2,c 11": vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$ eof EOD_GL1099 ioerr EOD_GL1099
			write #2,using 'Form Pos 1,C 8,4*c 30,x 5,n 2,c 11,x 6,c 12,c 30,c 50,c 12,c 20',release: vn$,nam$(1:30),ad1$,ad2$,csz$,typ,ss$,ph$,contact$,email$,fax$,myact$
		loop
EOD_GL1099: !
		version(2,1)
		close #1,free: ioerr ignore
		close #2: ioerr ignore
		fnindex_it("[Q]\GLmstr\paymstr.h[cno]","[Q]\GLmstr\payidx1.h[cno]","1 8")
		fnindex_it("[Q]\GLmstr\paymstr.h[cno]","[Q]\GLmstr\payidx2.h[cno]","9 38")
		fnindex_it("[Q]\GLmstr\payeeglbreakdown.h[cno]","[Q]\GLmstr\payeeglbkdidx.h[cno]","1 8")
		if ~exists("[Q]\GLmstr\gltr1099.h[cno]") then let fnCopy("S:\General Ledger\mstr\gltr1099.h99999","[Q]\GLmstr\gltr1099.h[cno]")
		fnindex_it("[Q]\GLmstr\gltr1099.h[cno]","[Q]\GLmstr\gltridx.h[cno]","1 8")
		goto Xit
 
 
Xit: fnend
include: Ertn
 
