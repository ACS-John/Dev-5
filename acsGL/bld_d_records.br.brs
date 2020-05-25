! Replace S:\acsGL\Bld_D_Records
! Create Type "D" Records
!
	autoLibrary
	fnTop(program$,cap$="Financial Statement")
	on error goto Ertn
!
	dim cnam$*40,dat$*20,io1$(9),gln(2,3),ta(2),ac(18),te$*1,cap$*128
	dim d$*50,bc(13),bp(13),bm(13),rf(6),dn$*3,an$*6,sn$*3,glk$*12,fsk$*5
!
!
	on fkey 5 goto Xit
	open #1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",internal,outIn,keyed
	fil$(1)="ACGLFNSB" : idx$(1)="agfsidx4"
	fil$(2)="ACGLFNSc" : idx$(2)="agfsidx1"
	fil$(3)="ACGLFNSi" : idx$(3)="agfsidx3"
	fil$(4)="ACGLFNSj" : idx$(4)="agfsidx2"
	fil$(5)="ACGLfNSf" : idx$(5)="agfsidx5"
	fil$(6)="ACGLfNSg" : idx$(6)="agfsidx6"
	io1$(1)="5,32,Nz 3,UT,N"
	io1$(2)="5,36,Nz 6,UT,N"
	io1$(3)="5,43,Nz 3,UT,N"
	io1$(4)="5,50,Nz 3,UT,N"
	io1$(5)="5,54,Nz 6,UT,N"
	io1$(6)="5,61,Nz 3,UT,N"
	io1$(7)="8,68,Cu 1,UT,N"
	io1$(8)="11,73,Nz 1,UT,N"
	io1$(9)="13,49,Nz 5,UT,N"
!
L310: pr newpage
	close #2: ioerr ignore ! close any reference file that is opened
L330: win=101
	cap$='Create Type "D" Records' ! use correct name above for printing
	fnwin3b(win,cap$,14,75,0,2,5,2)
	pr #win: newpage
L370: pr #win,fields "2,1,Cc 75,R,N": 'Create "D" records for a range of general ledger numbers'
	pr #win,fields "4,32,Cc 14,N": "From"
	pr #win,fields "4,50,Cc 14,N": "To"
	pr #win,fields "5,2,Cr 28,N": "Range of General Ledger #s:"
	pr #win,fields "7,1,Cc 75,R,N": "Type of G/L Accounts Being Selected (Only one type at a time!)"
	pr #win,fields "8,2,Cr 65,N": "A (Asset),L (Liability),Q (Equity),I (Income), E (Expense:"
	pr #win,fields "10,2,Cr 46,N": "Starting Financial Statement Reference Number:"
	pr #win,fields "10,1,Cc 75,r,N": "Type of Financial Statement Being Designed"
	pr #win,fields "11,2,Cr 70,N": "1=Balance Sheet, 2=2nd B/S, 3=I/C, 4=2nd I/C, 5=Fund, 6=2nd Fund:"
	pr #win,fields "13,2,Cr 46,N": "Starting Financial Statement Reference Number:"
	input #win,fields mat io1$: mat gln,type$,fs,fin conv CONV1
	if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
	if curfld<>8 or fin>0 then goto L550
	close #2: ioerr L510
L510: open #2: "Name=[Q]\GLmstr\"&fil$(fs)&".h[cno],KFName=[Q]\GLmstr\"&idx$(fs)&".h[cno],Shr",internal,outIn,keyed
L520: read #2,using L920: rno eof L540
	goto L520
L540: pr #win,fields io1$(9): rno+20
L550: if cmdkey>0 then goto L620 else ce=curfld+1
	if ce>udim(io1$) then ce=1
L570: io1$(ce)=rtrm$(uprc$(io1$(ce))) : ce1=pos(io1$(ce),"U",9)
	ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L370
CONV1: if ce>0 then io1$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERR1: pr f "24,78,C 1": bell : goto L570
L620: !
	if cmdkey=5 then goto Xit
!
	if gln(1,2)=0 then ce=1: goto ERR1
	if gln(2,2)=0 then ce=4: goto ERR1
	gl1=val(cnvrt$("PIC(###)",gln(1,1))&cnvrt$("PIC(######)",gln(1,2))&cnvrt$("PIC(###)",gln(1,3)))
	gl2=val(cnvrt$("PIC(###)",gln(2,1))&cnvrt$("PIC(######)",gln(2,2))&cnvrt$("PIC(###)",gln(2,3)))
	if gl1=0 then ce=1: goto ERR1
	if gl2=0 then ce=4: goto ERR1
	gf=gl3-gl1
	glk$=lpad$(str$(gln(1,1)),3)&lpad$(str$(gln(1,2)),6)&lpad$(str$(gln(1,3)),3)
	if fin=0 then ce=8: goto ERR1
	if uprc$(type$)="A" or uprc$(type$)="L" or uprc$(type$)="Q" or uprc$(type$)="I" or uprc$(type$)="E" then goto L750 else ce=7: goto ERR1
L750: if fs<1 or fs>5 then ce=9: goto ERR1
!
	pr #101: newpage
	pr #win,fields "5,1,Cc 75,N": "Please wait..."
	ff=20 ! incrument reference numbers
	te$="D" ! all detail records
	ac(1)=3 ! indent all d records to position 3
	rno=fin
	close #2: ioerr L840
L840: open #2: "Name=[Q]\GLmstr\"&fil$(fs)&".h[cno],KFName=[Q]\GLmstr\"&idx$(fs)&".h[cno],Shr",internal,outIn,keyed
	restore #1,key=glk$: nokey L980
L860: read #1,using L870: dno,ano,sno,d$,mat rf eof L980
L870: form pos 1,n 3,n 6,n 3,c 50,6*pd 3,42*pd 6.2,2*pd 3
	gl=val(cnvrt$("PIC(###)",dno)&cnvrt$("PIC(######)",ano)&cnvrt$("PIC(###)",sno))
	if gl>gl2 then goto L310
	if type$="A" or type$="E" then ac(5)=0 else ac(5)=1           ! set liabilities, equity and revenue to reverse sign
	write #2,using L920: rno,d$,te$,mat ac
L920: form pos 1,n 5,c 50,c 1,2*n 2,15*n 1,n 3
	rf(fs)=rno ! rewrite new reference back into g/l account  ?
	rewrite #1,using L950: mat rf
L950: form pos 63,6*pd 3
	rno=rno+ff
	goto L860
L980: close #2:
	if fs>0 then
		execute "Index [Q]\GLmstr\"&fil$(fs)&".h[cno],[Q]\GLmstr\"&idx$(fs)&".h[cno],1,5,Replace,DupKeys"
	end if
	goto L310
!
Xit: fnchain("General Ledger\Financial Statement Design")
include: Ertn
