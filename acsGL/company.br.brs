! Replace S:\acsGL\Company
! GL Company Information File Editor
 
autoLibrary
on error goto Ertn
 
dim a$(3)*40,b$(2)*12,c$*5,d(2),e$(2)*12,lastact$*12,tb$*30
dim miscname$(10)*20
dim dedcode(10)
dim dedfed(10)
dim dedfica(10)
dim dedst(10)
dim resp$(100)*40
dim prgl(5,3)
dim deduc(10)
dim miscgl$(10)*12
dim postingOption$(2)*30
	postingOption$(1)="Post Immediately"
	postingOption$(2)="Retain in Holding Files"
dim gl$(5)*12
dim dedOrAddOption$(2)
	dedOrAddOption$(1)="Deducttion"
	dedOrAddOption$(2)="Addition"
 
 
fnTop(program$)
gltyp=7
fnstyp(0)
 
open #glmstr=11: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",internal,outIn,keyed ioerr L220
 
L220: !
open #20: "Name=[Q]\GLmstr\GLBucket.h[cno],RecL=1,Use",internal,outIn,relative
if lrec(20)=0 then
	write #20,using 'Form POS 1,N 1',rec=1: 1
end if
read #20,using 'Form POS 1,N 1',rec=1: glb
close #20:
 
open #company=1: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,outIn,relative ioerr BLD_COINFO
goto READ_COINFO
 
COINFO_READ_ERR: close #company: ioerr BLD_COINFO : goto BLD_COINFO
 
BLD_COINFO: !
	open #company=1: "Name=[Q]\GLmstr\Company.h[cno],RecL=882,Replace",internal,outIn,relative
COINFO_WRITE: !
	write #company,using 'Form POS 1,3*C 40,2*C 12,C 5,2*N 1,2*C 12,N 3,N 6,N 3,PD 7.2,C 30,POS 298,15*PD 4,POS 382,N 2,N 2,PD 5.3,PD 5.2,PD 5.3,PD 5.2,G 1,PD 5.3,PD 5.2,N 1,10*C 20,50*N 1,10*C 12',rec=1: mat a$,mat b$,c$,mat d,mat e$,a1,a2,a3,ucm,tb$,mat prgl,jccode,nap,ficarate,ficawage,feducrat,feducwag,actr$,mcr,mcm,reccode,mat miscname$,mat dedcode,mat dedfed,mat dedfica,mat dedst,mat deduc,mat miscgl$
 
READ_COINFO: !
read #company,using 'Form POS 1,3*C 40,2*C 12,C 5,2*N 1,2*C 12,N 3,N 6,N 3,PD 7.2,C 30,POS 298,15*PD 4,POS 382,N 2,N 2,PD 5.3,PD 5.2,PD 5.3,PD 5.2,G 1,PD 5.3,PD 5.2,N 1,10*C 20,50*N 1,10*C 12',rec=1: mat a$,mat b$,c$,mat d,mat e$,a1,a2,a3,ucm,tb$,mat prgl,jccode,nap,ficarate,ficawage,feducrat,feducwag,actr$,mcr,mcm,reccode,mat miscname$,mat dedcode,mat dedfed,mat dedfica,mat dedst,mat deduc,mat miscgl$ conv COINFO_WRITE, ioerr COINFO_READ_ERR
 
lastgl$=cnvrt$("pic(zz#)",a1)&cnvrt$("pic(zzzzz#)",a2)&cnvrt$("pic(zz#)",a3)
SCREEN_1: ! r:
	fnTos(sn$="Company-1")
	mylen=30: mypos=mylen+3 : right=1
	respc=0
	fnLbl(1,30,"Company # [cno]")
	fnLbl(3,1,"Company Name:",mylen,right)
	fnTxt(3,mypos,40,0,left,"",0,"",0 )
	resp$(resp_cnam:=respc+=1)=a$(1)
	fnLbl(4,1,"Company Address:",mylen,right)
	fnTxt(4,mypos,40,0,left,"",0,"",0 )
	resp$(resp_caddr1:=respc+=1)=a$(2)
	fnLbl(5,1,"City, State, Zip:",mylen,right)
	fnTxt(5,mypos,40,0,left,"",0,"",0 )
	resp$(resp_ccsz:=respc+=1)=a$(3)
	fnLbl(6,1,"Federal ID #:",mylen,right)
	fnTxt(6,mypos,12,0,left,"",0,"",0 )
	resp$(resp_fedid:=respc+=1)=b$(1)
	fnLbl(7,1,"State ID #:",mylen,right)
	fnTxt(7,mypos,12,0,left,"",0,"",0 )
	resp$(resp_stateid:=respc+=1)=b$(2)
	fnLbl(8,1,"State U/C Rate:",mylen,right)
	fnTxt(8,mypos,5,0,left,"",0,"",0 )
	resp$(resp_stateUcRate:=respc+=1)=c$
	fnLbl(9,1,"State U/C Maximum:",mylen,right)
	fnTxt(9,mypos,10,0,left,"10",0,"",0 )
	resp$(resp_ucm:=respc+=1)=str$(ucm)
	fnLbl(10,1,"Type of Business:",mylen,right)
	fnTxt(10,mypos,30,0,left,"",0,"",0 )
	resp$(resp_typeOfBusiness:=respc+=1)=tb$
	fnLbl(11,1,"Number of Periods:",mylen,right)
	fnTxt(11,mypos,2,0,left,"30",0,"",0 )
	resp$(resp_nap:=respc+=1)=str$(nap)
	fnChk(12,60,"Use Department Number Field:",1)
	resp$(resp_useDeptNo:=respc+=1)="False"
	if d(1)=1 then resp$(resp_useDeptNo)="True" else resp$(resp_useDeptNo)="False"
	fnChk(13,60,"Use Sub Number Field:",1)
	resp_useSubAcct:=respc+=1
	if d(2)=1 then resp$(resp_useSubAcct)="True" else resp$(resp_useSubAcct)="False"
	fnChk(15,60,"Utilize Bank Reconciliation:",1)
	resp_useBankRec:=respc+=1
	if recc$="Y" or reccode=1 then resp$(resp_useBankRec)="True" else resp$(resp_useBankRec)="False"
	fnLbl(16,1,"Last Balance Sheet Account #:",mylen,right)
	fnqgl(16,mypos,0,2,pas)
	resp$(resp_lastBalSheetAccount:=respc+=1)=fnrgl$(lastgl$)
	fnChk(17,60,"Allocate Expenses to Job Cost:",1)
	resp_AllocExpToJc:=respc+=1
	if jcc$="Y" or jccode=1 then resp$(resp_AllocExpToJc)="True" else resp$(resp_AllocExpToJc)="False"
	fnLbl(18,1,"Posting Method:",mylen,right)
	fncomboa("PostMethod",18,mypos,mat postingOption$,"Normally you would post immediately. You would only consider posting to holding files if the general ledger is months behind.",mylen)
	resp_glb=respc+=1
	if glb=1 or glb=0 then resp$(resp_glb)=postingOption$(1) else resp$(resp_glb)=postingOption$(2)
	fnCmdKey("&Next",1,1,0,"Moves to 2nd screen of company information.")
	fnCmdKey("&Save",4,0,0,"Saves any changes and returns to menu without reviewing remainter of screens.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without saving any changes.")
	fnAcs2(mat resp$,ckey)
	if ckey=5 then
		goto Xit
	else
		a$(1)=resp$(resp_cnam)
		a$(2)=resp$(resp_caddr1)
		a$(3)=resp$(resp_ccsz)
		b$(1)=resp$(resp_fedid)
		b$(2)=resp$(resp_stateid)
		c$=resp$(resp_stateUcRate)
		ucm=val(resp$(resp_ucm))
		tb$=resp$(resp_typeOfBusiness)
		nap=val(resp$(resp_nap))
		if resp$(resp_useDeptNo) ="True" then d1$="Y": d(1)=1 else d1$="N": d(1)=0
		if resp$(resp_useSubAcct)="True" then d2$="Y": d(2)=1 else d2$="N": d(2)=0
		if resp$(resp_useBankRec)="True" then recc$="Y": reccode=1 else recc$="N": reccode=0
		lastgl$=fnagl$(resp$(resp_lastBalSheetAccount)) : a1=val(lastgl$(1:3)) : a2=val(lastgl$(4:9)) : a3=val(lastgl$(10:12)) ! gl number
		if resp$(resp_AllocExpToJc)="True" then jcc$="Y": jccode=1 else jcc$="N": jccode=0
		if resp$(resp_glb)=postingOption$(1) then glb$="P": glb=1 else glb$="R": glb=2
		
		if ckey=4 then !  save and exit
			gosub SAVE
			goto Xit
		end if
	end if
goto SCREEN_2 ! /r
SCREEN_2: ! r:
	fnTos(sn$="Company-2")
	mylen=30: mypos=mylen+3 : right=1
	fnLbl(1,1,"   The system will allow you to summarize the Payroll Withholding entries into",85,left)
	fnLbl(2,1,"one entry for each Withholding Account on your Trial Balance.  If you wish",85,left)
	fnLbl(3,1,"to utilize this option, enter the Account Numbers, otherwise leave both numbers",85,left)
	fnLbl(4,1,"as blank to pr all details.",85,left)
! BEGCNT,BEGACCT,BEGSUB,ENDCNT,ENDACCT,ENCSUB
	fnLbl(6,1,"First Account to summarize:",mylen,right)
	fnqgl(6,mypos,0,2,pas)
	resp$(1)=fnrgl$(e$(1))
	fnLbl(7,1,"Last Account to summarize:",mylen,right)
	fnqgl(7,mypos,0,2,pas)
	resp$(2)=fnrgl$(e$(2))
	fnCmdKey("&Next",1,1,0,"Moves to 3nd screen of company information.")
	fnCmdKey("&Save",4,0,0,"Saves any changes and returns to menu without reviewing remainter of screens.")
	fnCmdKey("&Back",2,0,0,"Returns to previous screen.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without saving any changes.")
	fnAcs2(mat resp$,ckey)
	e$(1)=fnagl$(resp$(1)) ! Summary # 1
	e$(2)=fnagl$(resp$(2)) ! Summary # 1
	if ckey=4 then !  save and exit
		gosub SAVE
		goto Xit
	end if
	if ckey=2 then goto SCREEN_1
goto SCREEN_3 ! /r
 
SCREEN_3: ! r:
	for j=1 to 5
		gl$(j)=cnvrt$("pic(zz#)",prgl(j,1))&cnvrt$("pic(zzzzz#)",prgl(j,2))&cnvrt$("pic(zz#)",prgl(j,3))
	next j
! FICARATE,FICAWAGE,FEDUCRAT,FEDUCWAG,MCR,MCM,MAT PRGL
	fnTos(sn$="Company-3")
	mylen=32: mypos=mylen+3 : right=1
	fnLbl(1,25,"After-the Fact Payroll Information")
	fnLbl(3,1,"Social Security Rate:",mylen,right)
	fnTxt(3,mypos,8,0,left,"34",0,"Format would be 6.2 ",0 )
	resp$(1)=str$(ficarate)
	fnLbl(4,1,"Social Security Maximum Wage:",mylen,right)
	fnTxt(4,mypos,12,0,left,"10",0,"Example would be 90000.00 ",0 )
	resp$(2)=str$(ficawage)
	fnLbl(5,1,"Federal U/C Rate:",mylen,right)
	fnTxt(5,mypos,8,0,left,"34",0,"Example would be .800 ",0 )
	resp$(3)=str$(feducrat)
	fnLbl(6,1,"Federal U/C Maximum Wage:",mylen,right)
	fnTxt(6,mypos,12,0,left,"10",0,"An example of the Federal unemployment compensation rate would be 9000.00 ",0 )
	resp$(4)=str$(feducwag)
	fnLbl(7,1,"Medicare Rate:",mylen,right)
	fnTxt(7,mypos,8,0,left,"34",0,"Format would be 1.45",0 )
	resp$(5)=str$(mcr)
	fnLbl(8,1,"Medicare Maximum Wage:",mylen,right)
	fnTxt(8,mypos,12,0,left,"10",0,"There is no maximun at this time.  Ener enough 9s to exceed the highest paid employee.  (eg.  9999999.00 ",0 )
	resp$(6)=str$(mcm)
	fnLbl(10,25,"General Ledger Account Numbers")
	fnLbl(11,1,"Federal Withholding:",mylen,right)
	fnqgl(11,mypos,0,2,pas)
	resp$(7)=fnrgl$(gl$(1))
	fnLbl(12,1,"FICA Withholding:",mylen,right)
	fnqgl(12,mypos,0,2,pas)
	resp$(8)=fnrgl$(gl$(2))
	fnLbl(13,1,"State Withholding:",mylen,right)
	fnqgl(13,mypos,0,2,pas)
	resp$(9)=fnrgl$(gl$(3))
	fnLbl(14,1,"Local Withholding:",mylen,right)
	fnqgl(14,mypos,0,2,pas)
	resp$(10)=fnrgl$(gl$(4))
	fnLbl(15,1,"Earned Income Credit:",mylen,right)
	fnqgl(15,mypos,0,2,pas)
	resp$(11)=fnrgl$(gl$(5))
	fnCmdKey("&Next",1,1,0,"Moves to 4th screen of company information.")
	fnCmdKey("&Save",4,0,0,"Saves any changes and returns to menu without reviewing remainter of screens.")
	fnCmdKey("&Back",2,0,0,"Returns to previous screen.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without saving any changes.")
	fnAcs2(mat resp$,ckey)
	if ckey=5 then goto Xit
	if ckey=2 then goto SCREEN_2
	ficarate=val(resp$(1))
	ficawage=val(resp$(2))
	feducrat=val(resp$(3))
	feducwag=val(resp$(4))
	mcr=val(resp$(5))
	mcm=val(resp$(6))
	for j=1 to 5
		gl$(j)=fnagl$(resp$(j+6))
		prgl(j,1)=val(gl$(j)(1:3)): prgl(j,2)=val(gl$(j)(4:9)): prgl(j,3)=val(gl$(j)(10:12))
	next j
	if ckey=4 then !  save and exit
		gosub SAVE
		goto Xit
	end if
goto SCREEN_4 ! /r
 
SCREEN_4: ! r:
	fnTos(sn$="Company-4")
	mylen=32: mypos=mylen+3 : right=1
	fnLbl(1,1,"Enter the names of the 10 Miscellaneous Deductions and indicate",90,left)
	fnLbl(2,1,"how each deduction is to be handled by the system.  A check",90,left)
	fnLbl(3,1,"mark will indicate that the miscellaneous deduction should be ",90,left)
	fnLbl(4,1,"subtracted from gross before calculating federal withholding, ",90,left)
	fnLbl(5,1,"fica and social security, state withholdings, or state U/C.",80,left)
	fnLbl(7,29,"Ded         Ded  Ded   Ded    Ded",40,left)
	fnLbl(8,1,"Deduction Name              Add         Fed  FICA  State  U/C     GL Number",80,left)
	resp=0
	for j=1 to 10
		fnTxt(j+8,1,20,0,left,"",0,"Enter you deduction name.",0 )
		resp$(resp+=1)=miscname$(j)
		fncomboa("MIscdeduct",j+8,26,mat dedOrAddOption$,"Indicate whether the deduction should be deducted from the check or added to the check.",10)
		if dedcode(j)=0 then dedcode(j)=1
		resp$(resp+=1)=dedOrAddOption$(dedcode(j))
		fnChk(j+8,41,"",1)
		if dedfed(j)>0 then resp$(resp+=1)="True" else resp$(resp+=1)="False"
		fnChk(j+8,47,"",1)
		if dedfica(j)>0 then resp$(resp+=1)="True" else resp$(resp+=1)="False"
		fnChk(j+8,53,"",1)
		if dedst(j)>0 then resp$(resp+=1)="True" else resp$(resp+=1)="False"
		fnChk(j+8,59,"",1)
		if deduc(j)>0 then resp$(resp+=1)="True" else resp$(resp+=1)="False"
		linecount=j+8
		fnqgl(linecount,64,0,2,pas)
		resp$(resp+=1)=fnrgl$(miscgl$(j))
	next j
	fnCmdKey("&Next",1,1,0,"Saves changes and returns to main menu.")
	fnCmdKey("&Save",4,0,0,"Saves any changes and returns to menu.")
	fnCmdKey("&Back",2,0,0,"Returns to previous screen.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without saving any changes.")
	fnAcs2(mat resp$,ckey)
	if ckey=5 then goto Xit
	resp=0
	for j=1 to 10
		miscname$(j)=resp$(resp+=1)
		if resp$(resp+=1)=dedOrAddOption$(1) then dedcode(j)=1 else dedcode(j)=2
		if resp$(resp+=1)="True" then dedfed(j)=1 else dedfed(j)=0
		if resp$(resp+=1)="True" then dedfica(j)=1 else dedfica(j)=0
		if resp$(resp+=1)="True" then dedst(j)=1 else dedst(j)=0
		if resp$(resp+=1)="True" then deduc(j)=1 else deduc(j)=0
		miscgl$(j)=fnagl$(resp$(resp+=1))
	next j
	gosub SAVE
	close #company:
goto Xit ! /r
 
Xit: fnXit
SAVE: ! r:
	rewrite #company,using 'Form POS 1,3*C 40,2*C 12,C 5,2*N 1,2*C 12,N 3,N 6,N 3,PD 7.2,C 30,POS 298,15*PD 4,POS 382,N 2,N 2,PD 5.3,PD 5.2,PD 5.3,PD 5.2,G 1,PD 5.3,PD 5.2,N 1,10*C 20,50*N 1,10*C 12',rec=1: mat a$,mat b$,c$,mat d,mat e$,a1,a2,a3,ucm,tb$,mat prgl,jccode,nap,ficarate,ficawage,feducrat,feducwag,unused,mcr,mcm,reccode,mat miscname$,mat dedcode,mat dedfed,mat dedfica,mat dedst,mat deduc,mat miscgl$
	open #20: "Name=[Q]\GLmstr\GLBucket.h[cno],RecL=1,Use",internal,outIn,relative
	rewrite #20,using 'Form POS 1,N 1',rec=1: glb
	close #20:
return ! /r
include: Ertn
