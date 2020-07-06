on error goto Ertn
autoLibrary
fnTop(program$)
 
dim em$*30,tcp(32),tdc(10),cp(32),ttdc(10)
dim dedcode(20),calcode(20),dedfed(20),fullname$(20)*20,resp$(50)*60
dim abbrevname$(20)*8,dedfica(20),dedst(20),deduc(20)
dim sel_ded(20),sel_pen(20)
fnDedNames(mat fullname$,mat abbrevname$,mat dedcode,mat calcode,mat dedfed,mat dedfica,mat dedst,mat deduc)
gosub L710
open #1: "Name=[Q]\PRmstr\Employee.h[cno],Shr",internal,input,relative
open #4: "Name=[Q]\PRmstr\payrollchecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno]",internal,outIn,keyed
open #2: "Name=[Q]\PRmstr\RPTRAIL.h[cno],Shr",internal,input,relative
fnopenprn
gosub HDR
L250: !
	read #1,using L260: eno,em$,ss$ eof END1
	L260: form pos 1,n 8,c 30,pos 99,c 11
	a=pos (rtrm$(em$)," ",1)
	b=pos (rtrm$(em$)," ",a+1)
	em$=rtrm$(em$(max(a+1,b+1):30))&" "&em$(1:a)
	reg_earnings=deferred_comp_wh=deferred_comp_match=0
goto L560
L320: !
	if deferred_comp_wh=0 and deferred_comp_match=0 then goto L360 ! skip if no deferred comp
	pr #255,using L340: em$(1:24),ss$,reg_earnings,deferred_comp_wh,deferred_comp_match pageoflow L1020
	L340: form pos 1,c 24,c 12,4*n 12.2
	total_salary+=reg_earnings
	total_wh+=deferred_comp_wh
	total_deferred_comp+=deferred_comp_match
	L360: !
goto L250
END1: !
	close #1: ioerr ignore
	close #2: ioerr ignore
	pr #255: "                                      ----------  ----------  ---------- "
	pr #255,using L410: " "," ",total_salary,total_wh,total_deferred_comp
	L410: form pos 1,c 24,c 12,4*n 12.2
	pr #255: "                                      =========-  ==========  ========== "
	form pos 1,c 1,c 32,n 6,3*n 9,c 14,c 2
	fncloseprn
	close #25: ioerr Xit
Xit: fnXit
!
L560: !
	checkkey$=cnvrt$("pic(ZZZZZZZ#)",eno)&"         "
	form pos 9,c 30
	restore #4,key>=checkkey$: nokey L250
	mat tcp=(0): mat ttdc=(0)
	do
		read #4,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat cp eof L320
		if heno<>eno then goto L320
		if prd=>beg_date and prd<=end_date then
			mat tcp=tcp+cp
			mat ttdc=ttdc+tdc
			for j=1 to 20
				if sel_ded(j)=1 and dedcode(j)=1 then deferred_comp_wh+=cp(j+4) ! Deferred_Comp wH
				if sel_ded(j)=1 and dedcode(j)>1 then deferred_comp_wh-=cp(j+4) ! Deferred_Comp wH
				if sel_pen(j)=1 then deferred_comp_match+=cp(j+4) ! Deferred_Comp match
			next j
			reg_earnings+=cp(31) ! REGULAR EARNINGS
		end if
	loop
L710: ! r:
	fnTos
	rc=cf=0
	fnFra(1,1,20,23,"Deferred Comp W/H","Mark the Deferred Comp Withholding deduction",0)
	cf+=1 : fratype=cf
	for j=1 to 20
		fnChk(j,3,fullname$(j),0,fratype)
		resp$(rc+=1)="False"
	next j
	fnFra(1,30,20,23,"Deferred Comp Match","Mark the deferred compensation match.",0)
	cf+=1 : fratype=cf
	for j=1 to 20
		fnOpt(j,3,fullname$(j),0,fratype)
		resp$(rc+=1)="False"
	next j
	fnFra(1,60,3,42,"Date Range","Enter the beginning and ending date range covered by this report.")
	cf+=1 : fradate=cf : mylen=26 : mypos=mylen+2
	fnLbl(1,1,"Starting Date:",mylen,1,0,fradate)
	fnTxt(1,mypos,10,0,1,"3",0,empty$,fradate)
	resp$(rc+=1)=str$(beg_date)
	fnLbl(2,1,"Ending Date:",mylen,1,0,fradate)
	fnTxt(2,mypos,10,0,1,"3",0,empty$,fradate)
	resp$(rc+=1)=str$(end_date)
	fnCmdKey("Next",1,1,0,"Prints the report")
	fnCmdKey("Cancel",5,0,1,"Returns to menu")
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	for j=1 to 20
		if resp$(j)="True" then sel_ded(j)=1
	next j
	for j=1 to 20
		if resp$(j+20)="True" then sel_pen(j)=1
	next j
	beg_date=val(resp$(41))
	end_date=val(resp$(42))
return ! /r
HDR: !  r:
	pr #255: "\qc  {\f181 \fs18 \b "&trim$(env$('cnam'))&"}"
	pr #255: "\qc  {\f181 \fs24 \b "&env$('program_caption')&"}"
	pr #255: "\qc  {\f181 \fs16 \b From: "&cnvrt$("pic(zzzz/zz/zz)",beg_date)&" To: "&cnvrt$("pic(zzzz/zz/zz)",end_date)&"}"
	pr #255: "\ql   "
	pr #255: "Name                    SS Number     Total Wage     Comp WH  Comp Match"
return ! /r
L1020: ! r:
pr #255: newpage
	gosub HDR
continue ! /r
include: Ertn
