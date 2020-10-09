! Replace S:\acsGL\glCkRec
! Bank Reconciliation Report  (prints the actual bank reconciliation form the general ledger system
 
	autoLibrary
	on error goto Ertn
 
	dim flo2$(3),io1$(6),sc$(9),sd$(7),se$(7)*30,sf$(4),cnam$*40,dat$*20
	dim gl$*12,c$*12,p$*30,s$*2,a(3),dcode$*24,glc$*24,holdgc$*24,currgl$*12
	dim cap$*128,resp$(10)*80
 
	fnTop(program$,cap$="Bank Reconciliation Report")
	fnconsole(off=0)
	fncno(cno,cnam$)
	fndat(dat$)
	right=1 : center=2 : pointtwo$="32"
	fnTos(sn$="glCkRec") : _
	lc=0 : mylen=40 : mypos=mylen+2
	fnLbl(lc+=1,1,"General Ledger Bank Account Number:",mylen,right)
	fnqgl(lc,mypos) : _
	resp$(1)=fnrgl$(resp$(1))
! iO1$(1)="2,43,Nz 3,UT,N" : iO1$(2)="2,47,Nz 6,UT,N" : _
	! iO1$(3)="2,54,Nz 3,UT,N" : iO1$(4)="3,43,c 20,UT,N" : _
	! iO1$(5)="4,43,Nz 12.2,UT,N" : iO1$(6)="5,43,Nz 6,UT,N"
! pr f "15,29,C 10,B,1": "Print (F1)" : _
	! pr f "15,41,C 09,B,5": "Exit (F5)"
	fnLbl(lc+=1,1,"Report Heading Date:",mylen,right)
	fnTxt(lc,mypos,20) : _
	resp$(2)=dat$
	fnLbl(lc+=1,1,"Balance per Bank Statement:",mylen,right)
	fnTxt(lc,mypos,12,0,0,'PointTwo') : _
	resp$(3)=str$(bankbal)
	fnLbl(lc+=1,1,"Last Check Date for Reconciliation:",mylen,right)
	fnTxt(lc,mypos,0,0,0,'CCYYMMDD') : _
	resp$(4)=str$(lcd)
	fnCmdSet(3)
! Rinput #101,Fields MAT IO1$: GL1,GL2,GL3,DAT$,BANKBAL,LCD : _
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	resp$(1)=fnagl$(resp$(1)) : _
	gl1=val(resp$(1)(1:3)) : _
	gl2=val(resp$(1)(4:9)) : _
	gl3=val(resp$(1)(10:12)) : _
	dat$=resp$(2) : _
	bankbal=val(resp$(3)) : _
	lcd=val(resp$(4))
	currgl$=resp$(1)
	fnwait("Printing: Please wait...",1) : _
	on fkey 5 goto DONE
	open #glbrec=1: "Name=[Q]\GLmstr\glbrec.h[cno],KFName=[Q]\GLmstr\glrecidx.h[cno],Shr",internal,input,keyed ioerr Xit
	read #glbrec,using 'Form POS 1,C 12,C 12,C 30,C 2,N 6,PD 5.2,N 1',key>=currgl$&"            ": gl$,c$,p$,s$,mat a nokey DONE
	fnopenprn
	if currgl$<>gl$ then goto DONE
	gosub HDR : _
	pr #255,using 'Form POS 20,C 35,N 16.2': "* Balance Per Bank Statement *",bankbal : _
	pr #255: ""
	goto RD_NXT
 
READ_GLBREC: !
	read #glbrec,using 'Form POS 1,C 12,C 12,C 30,C 2,N 6,PD 5.2,N 1': gl$,c$,p$,s$,mat a eof TOTAL
	if currgl$<>gl$ then goto TOTAL
	if fndate_mmddyy_to_ccyymmdd(a(1))>lcd then goto READ_GLBREC
RD_NXT: !
	if a(3)<>0 then goto READ_GLBREC
	pr #255,using 'Form POS 1,C 12,POS 15,C 30,POS 48,PIC(ZZ/ZZ/ZZ),POS 58,N 13.2': c$,p$,a(1),a(2) pageoflow PGOF
	tot+=a(2)
	goto READ_GLBREC
 
PGOF: pr #255: newpage : _
	gosub HDR : _
	continue
 
TOTAL: !
	pr #255,using 'Form POS 59,C 12': "------------" : _
	pr #255,using 'Form POS 57,N 14.2': bankbal-tot : _
	pr #255,using 'Form POS 59,C 12': "============"
	goto DONE
 
HDR: !
	pr #255,using 'Form POS 1,C 8,Cc 56': date$('mm/dd/yy'),cnam$
	pr #255,using 'Form POS 1,C 8,Cc 56': time$,cap$
	pr #255,using 'Form POS 29,C 3,X 1,C 6,X 1,C 3': currgl$(1:3),currgl$(4:9),currgl$(10:12)
	pr #255,using 'Form POS 1,Cc 72': dat$
	pr #255: ""
	pr #255,using 'Form POS 1,C 12,POS 15,C 5,POS 50,C 4,POS 65,C 6': "Check Number","Payee","Date","Amount"
	pr #255: ""
return
 
DONE: fncloseprn
	close #glbrec:
Xit: fnXit
 
include: ertn
 
