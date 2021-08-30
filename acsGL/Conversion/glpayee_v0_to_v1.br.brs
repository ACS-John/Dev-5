! Replace S:\acsGL\Conversion\glpayee_v0_to_v1
! converts general ledger payee files to new rln
! from recl=127 to recl=276 and version 1
def library fnglpayee_v0_to_v1
	autoLibrary
	fnStatus('updating Payee file format.')
	open #1: "Name=[Q]\GLmstr\GL1099.h[cno],KFName=[Q]\GLmstr\GL109IDX.h[cno]",internal,outIn,keyed ioerr ignore
	if ~exists("[Q]\GLmstr\PayMstr.h[cno]") then 
		open #2: "Name=[Q]\GLmstr\PayMstr.h[cno],KFName=[Q]\GLmstr\payidx1.h[cno],RecL=276,kps=1,kln=8,replace",internal,outIn,keyed
		version(2,1)
		close #2:
	end if
	open #2: "Name=[Q]\GLmstr\PayMstr.h[cno]",i,outi,r  ! open #2: "Name=[Q]\GLmstr\PayMstr.h[cno],KFName=[Q]\GLmstr\payidx1.h[cno]",internal,outIn,keyed
	if rln(2)<>276 then
		close #2:
		fnCopy("[Q]\GLmstr\PayMstr.h[cno]","[Q]\GLmstr\PayMstr.h[cno]",276)
		open #2: "Name=[Q]\GLmstr\PayMstr.h[cno]",i,outi,r  ! open #2: "Name=[Q]\GLmstr\PayMstr.h[cno],KFName=[Q]\GLmstr\payidx1.h[cno]",internal,outIn,keyed
	end if
	do
		dim nam$*35,ad1$*20,ad2$*20,csz$*20,ss$*11
		read #1,using "form pos 1,c 8,c 35,3*c 20,pd 5.2,n 2,c 11": vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$ ioerr Eo1
		dim ph$*12,contact$*30,email$*50
		dim fax$*12,myact$*20
		write #2,using 'Form Pos 1,C 8,4*c 30,x 5,n 2,c 11,x 6,c 12,c 30,c 50,c 12,c 20',release: vn$,nam$(1:30),ad1$,ad2$,csz$,typ,ss$,ph$,contact$,email$,fax$,myact$
	loop
	Eo1: !
	version(2,1)
	close #1,free: ioerr ignore
	close #2: ioerr ignore
	fnIndex("[Q]\GLmstr\PayMstr.h[cno]","[Q]\GLmstr\payidx1.h[cno]","1 8")
	fnIndex("[Q]\GLmstr\PayMstr.h[cno]","[Q]\GLmstr\payidx2.h[cno]","9 38")
	fnIndex("[Q]\GLmstr\payeeglbreakdown.h[cno]","[Q]\GLmstr\payeeglbkdidx.h[cno]","1 8")
fnend

