! r: setup
! ken: "
! never could get it work with an RO record if all zeroes;  
! without RO record i had to put an RE record in front of every RW record and 
! had to follow with an RT record -  
! Right now this program will not create an RO record
! "
	library 'S:\Core\Library': fntop,fnxit, fnerror,fnAcs,fnLbl,fnTxt,fnTos,fnCmdKey,fnureg_read,fnureg_write,fngethandle,fnDedNames,fncreg_read,fncreg_write,fncomboa
	on error goto ERTN
	fntop(program$,cap$="Electronic W-2")
! ______________________________________________________________________
	dim em$(3)*30,ss$*11,tcp(32),cap$*128,tmp$*128
	dim tdc(10),newdedcode(20),newcalcode(20),newdedfed(20),dedfica(20)
	dim dedst(20),deduc(20),abrevname$(20)*8,fullname$(20)*20
	dim a$(3)*40,federal_id$*12,s2(2)
	dim w3(2),i2(2),t2(2)
	dim emppin$*17 ! Personal ID Number (used in RA Record)
	dim tlcn$*6,contact$*27,contactph$*15,phoneext$*5,email$*40
	dim w2(9),i1(9),t1(9),ct$*20,st$*2
	dim terminat$*1,first$*15,mid$*15,last$*20,resp$(40)*256,path$*256
	open #hCompany:=fngethandle: "Name=[Q]\PRmstr\Company.h[cno],Shr",internal,input 
	read #hCompany,using fCompany: mat a$,federal_id$,loccode
	fCompany: form pos 1,3*c 40,c 12,pos 150,x 80,n 2
	close #hCompany: 
	!
	dim optNameFormat$(2)*20,nameFormat_sf$(2)*1
	optNameFormat$(1)='First Name First' : nameFormat_sf$(1)='F'
	optNameFormat$(2)='Last Name First'  : nameFormat_sf$(2)='S'
	!
	disable=1 ! 
	med$="Y"
! /r
	fnDedNames(mat fullname$,mat abrevname$,mat newdedcode,mat newcalcode,mat newdedfed,mat dedfica,mat dedst,mat deduc)
	open #hEmployee:=fngethandle: "Name=[Q]\PRmstr\RPMSTR.h[cno],KFName=[Q]\PRmstr\RPINDEX.h[cno],Shr",internal,input,keyed 
	open #hChecks:=fngethandle: "Name=[Q]\PRmstr\payrollchecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno]",internal,outIn,keyed 
! r: initialize variables

	fnureg_read('electronic w-2 file',path$,os_filename$(env$('Desktop')&'\w2elec-[cno]'))
	beg_date=val('0101'&date$(days(date$)-180,'YY'))
	end_date=val('1231'&date$(days(date$)-180,'YY'))
	yr=date(days(date$)-180,'ccyy')
	ssmax =127200
	ssrate=.062
	mcmax =.0145
	mcrate=999999
	fncreg_read('W-2 Company Name',a$(1),a$(1))
	fncreg_read('W-2 Company Street Address',a$(2),a$(2))
	fncreg_read('W-2 Company City',ct$)
	fncreg_read('W-2 Company State',st$)
	fncreg_read('W-2 Company Zip',zip$)
	if ct$='' or st$='' or zip$='' then
		p1=pos(a$(3),",",1): comma=1
		if p1=0 then p1=pos(a$(3)," ",1): comma=0
		ct$=a$(3)(1:p1-1)
		if comma=1 then st$=a$(3)(p1+2:p1+3) else st$=a$(3)(p1+1:p1+2)
		p2=len(rtrm$(a$(3)))
		p1=p2-4
		zip$=a$(3)(p1:p2)
	end if
	fncreg_read('Employee Name Format',tmp$,optNameFormat$(1)) : nameFormat=max(1,srch(mat optNameFormat$,tmp$))
	fncreg_read('Miscellaneous Deduction Containing Employer Cost Group-Term Life Ins',tmp$) : ins=val(tmp$)
	fncreg_read('Miscellaneous Deduction Used For Pension',tmp$) : pen=val(tmp$)
	fncreg_read('Miscellaneous Deduction Used For Deferred Compensation',tmp$) : dfc=val(tmp$)
	fncreg_read('Miscellaneous Deduction Used For Dependent Care Assistance',tmp$) : dcan=val(tmp$)
!
	dim contact$*27
	dim email$*40
	fncreg_read('W-2 Personal ID Number',emppin$)
	fncreg_read('W-2 Resub Indicator',resub$,"0")
	fncreg_read('W-2 Resub TLCN',tlcn$)
	fncreg_read('W-2 Contact Name',contact$)
	fncreg_read('W-2 Contact Phone Number',contactph$)
	fncreg_read('W-2 Contact Phone Extension',phoneext$)
	fncreg_read('W-2 Contact E-Mail',email$)
	fncreg_read('W-2 Terminating Business Indicator',terminat$,"0")
!
	fncreg_read('W-2 State Code',tmp$) : sr1=val(tmp$)
	fncreg_read('W-2 FIPS',tmp$) : sr2=val(tmp$)
!
!
! /r
SCREEN1_NEW: ! r:
	fnTos(sn$="W2-1") 
	rc=lyne=0: mylen=17 : mypos=mylen+2
	fnLbl(lyne+=1,1,"Starting Date:",mylen,1,0,0)
	fnTxt(lyne,mypos,10,0,1,"3",0,"First day of calendar year",0) 
	resp$(respc_dateStart:=rc+=1)=str$(beg_date)
	fnLbl(lyne+=1,1,"Ending Date:",mylen,1,0,0)
	fnTxt(lyne,mypos,10,0,1,"3",0,"Last day of calendar year",0) 
	resp$(respc_dateEnd:=rc+=1)=str$(end_date)
	lyne+=1
	fnLbl(lyne+=1,1,"Output File Name:",mylen,1,0,0)
	fnTxt(lyne,mypos,30,0,0,'70',0,"Destination and file name you wish to use.",0) 
	resp$(respc_path:=rc+=1)=path$
	lyne+=1
	fnLbl(lyne+=1,1,"Company Name:",mylen,1,0,0)
	fnTxt(lyne,mypos,40,0,0,"",0,"Enter the name of the company submitting",0) 
	resp$(resp_cnam:=rc+=1)=a$(1)
	fnLbl(lyne+=1,1,"Street Address:",mylen,1,0,0)
	fnTxt(lyne,mypos,40,0,0,"",0,"Address of the company submitting",0) 
	resp$(resp_cstreet:=rc+=1)=a$(2)
	fnLbl(lyne+=1,1,"City:",mylen,1,0,0)
	fnTxt(lyne,mypos,10,22,0,"",0,"City of the company submitting",0) 
	resp$(resp_ccity:=rc+=1)=ct$
	fnLbl(lyne,mypos+10+2,"State:",6,1,0,0)
	fnTxt(lyne,mypos+10+2+6+2,2,0,0,"",0,"State for the company being submitted",0) 
	resp$(resp_cstate:=rc+=1)=st$
	fnLbl(lyne,mypos+10+2+6+2+2+2,"Zip:",4,1,0,0)
	fnTxt(lyne,mypos+10+2+6+2+2+2+4+2,5,0,0,"",0,"Zip code for the company being submitted",0) 
	resp$(resp_czip:=rc+=1)=zip$
	fnLbl(lyne+=1,1,"Federal ID:",mylen,1,0,0)
	fnTxt(lyne,mypos,12,0,0,"1000",0,"Enter the Federal Id number without slashes or dashes.",0) 
	resp$(resp_fid:=rc+=1)=federal_id$ 
	!
	lyne=0
	col3=17+2+30+2 :   mylen=29 : mypos=col3+mylen+2
	fnLbl(lyne+=1,col3,"Payment Year:",mylen,1,0,0)
	fnTxt(lyne,mypos,4,0,0,"1030",0,"Enter the year for which the wages were paid in ccyy format.",0) 
	resp$(resp_paymentYear:=rc+=1)=str$(yr)
	fnLbl(lyne+=1,col3,"Social Security Maximum Wage:",mylen,1,0,0)
	fnTxt(lyne,mypos,10,0,0,"10",disable,"Enter the social security maximum wage for the year just completed.",0) 
	resp$(resp_ssmax:=rc+=1)=str$(ssmax)
	fnLbl(lyne+=1,col3,"Social Security Rate:",mylen,1,0,0)
	fnTxt(lyne,mypos,6,0,0,"34",disable,"Enter the social security rate for the year just completed.",0) 
	resp$(resp_ssrate:=rc+=1)=str$(ssrate)
	fnLbl(lyne+=1,col3,"Medicare Maximum Wage:",mylen,1,0,0)
	fnTxt(lyne,mypos,10,0,0,"10",disable,"Enter the medicare maximum wage for the year just completed.",0) 
	resp$(resp_mcmax:=rc+=1)=str$(mcmax)
	fnLbl(lyne+=1,col3,"Medicare Rate:",mylen,1,0,0)
	fnTxt(lyne,mypos,6,0,0,"30",disable,"Enter the medicare rate for the year just completed.",0) 
	resp$(resp_mcrate:=rc+=1)=str$(mcrate)
	!
	lyne=10 : mylen=69 : mypos=mylen+2
	fnLbl(lyne+=1,1,"Miscellaneous Deduction Containing Employer Cost Group-Term Life Ins:",mylen,1,0,0)
	fnTxt(lyne,mypos,2,0,0,"30",0,"",0) 
	resp$(resp_ins:=rc+=1)=str$(ins)
	fnLbl(lyne+=1,1,"Miscellaneous Deduction Used For Pension:",mylen,1,0,0)
	fnTxt(lyne,mypos,2,0,0,"30",0,"",0) 
	resp$(resp_pen:=rc+=1)=str$(pen)
	fnLbl(lyne+=1,1,"Miscellaneous Deduction Used For Deferred Compensation:",mylen,1)
	fnTxt(lyne,mypos,2,0,0,"30",0,"",0) 
	resp$(resp_dfc:=rc+=1)=str$(dfc)
	fnLbl(lyne+=1,1,"Miscellaneous Deduction Used For Dependent Care Assistance:",mylen,1)
	fnTxt(lyne,mypos,2,0,0,"30",0,"",0) 
	resp$(resp_dcan:=rc+=1)=str$(dcan)
	lyne+=1
	mylen=31 : mypos=mylen+2
	fnLbl(lyne+=1,1,"Employee Name Format:",mylen,1)
	fncomboa('nameFormat',lyne,mypos,mat optNameFormat$, '',20)
	resp$(resp_nameFormat:=rc+=1)=optNameFormat$(nameFormat)
	lyne+=1
	fnLbl(lyne+=1,1,"Personal ID Number:",mylen,1)
	fnTxt(lyne,mypos,17,0,0,"",0,"",0) 
	resp$(resp_emppin:=rc+=1)=emppin$
	fnLbl(lyne+=1,1,"Resub Indicator:",mylen,1)
	fnTxt(lyne,mypos,1,0,0,"30",0,"",0) 
	resp$(resp_resub:=rc+=1)=resub$
				fnLbl(lyne+=1,1,"Resub TLCN:",mylen,1)
				fnTxt(lyne,mypos,6,0,0,"30",0,"",0) 
				resp$(resp_tlcn:=rc+=1)=tlcn$
	fnLbl(lyne+=1,1,"Contact Name:",mylen,1)
	fnTxt(lyne,mypos,27,0,0,"30",0,"",0) 
	resp$(resp_contact:=rc+=1)=contact$
	fnLbl(lyne+=1,1,"Contact Phone Number:",mylen,1)
	fnTxt(lyne,mypos,15,0,0,"30",0,"",0) 
	resp$(resp_contactph:=rc+=1)=contactph$
				fnLbl(lyne+=1,1,"Contact Phone Extension:",mylen,1)
				fnTxt(lyne,mypos,5,0,0,"",0,"",0) 
				resp$(resp_phoneext:=rc+=1)=phoneext$
	fnLbl(lyne+=1,1,"Contact E-Mail:",mylen,1)
	fnTxt(lyne,mypos,40,0,0,"",0,"",0) 
	resp$(resp_email:=rc+=1)=email$
	fnLbl(lyne+=1,1,"Terminating Business Indicator:",mylen,1)
	fnTxt(lyne,mypos,1,0,0,"30",0,"",0) 
	resp$(resp_terminat:=rc+=1)=terminat$
	lyne+=1
	mylen=62 : mypos=mylen+2
	fnLbl(lyne+=1,1,"Some states require filing electronic W-2s.",80,2)
	fnLbl(lyne+=1,1,"Answer the following questions if you wish to create 'RS' records during this run.",80,2)
	fnLbl(lyne+=1,1,"State code used in your record to identify the selected state:",mylen,1)
	fnTxt(lyne,mypos,2,0,0,"30",0,"",0) 
	resp$(resp_state_code:=rc+=1)=str$(sr1)
	fnLbl(lyne+=1,1,"Appropriate FIPS postal numeric code:",mylen,1)
	fnTxt(lyne,mypos,2,0,0,"30",0,"",0) 
	resp$(resp_fips:=rc+=1)=str$(sr2)
	fnLbl(lyne+=1,1,'(See an appendix in your electronic booklet for the postal code!)',80,2)
	!
	fnCmdKey("Next",1,1,0,"Creates the export")
	fnCmdKey("Cancel",5,0,1,"Returns to menu")
	fnAcs(sn$,0,mat resp$,ckey) ! /r
	if ckey=5 then goto XIT
! r: populate local variables from mat resp$
	beg_date=val(resp$(respc_dateStart))
	end_date=val(resp$(respc_dateEnd))
	path$=resp$(respc_path)
	!
	a$(1)=resp$(resp_cnam)
	a$(2)=resp$(resp_cstreet)
	ct$=resp$(resp_ccity)
	st$=resp$(resp_cstate)
	zip$=resp$(resp_czip)
	!
	federal_id$=resp$(resp_fid) : federal_id_val=val(srep$(federal_id$,'-',''))
	yr=val(resp$(resp_paymentYear))
	ssmax=val(resp$(resp_ssmax))
	ssrate=val(resp$(resp_ssrate))
	mcmax=val(resp$(resp_mcmax))
	mcrate=val(resp$(resp_mcrate))
	!
	nameFormat=srch(mat optNameFormat$,resp$(resp_nameFormat))
	!
	ins=val(resp$(resp_ins))
	pen=val(resp$(resp_pen))
	dfc=val(resp$(resp_dfc))
	dcan=val(resp$(resp_dcan))
	!
	emppin$=resp$(resp_emppin)
	resub$=resp$(resp_resub)
	tlcn$=resp$(resp_tlcn)
	contact$=resp$(resp_contact)
	contactph$=resp$(resp_contactph)
	phoneext$=resp$(resp_phoneext)
	email$=resp$(resp_email)
	terminat$=resp$(resp_terminat)
	!
	sr1=val(resp$(resp_state_code))
	sr2=val(resp$(resp_fips))
	!
! /r
! r: validate screen1 values
	if beg_date=0 then goto SCREEN1_NEW
	if end_date=0 then goto SCREEN1_NEW
	if nameFormat<=0 or nameFormat>udim(mat nameFormat_sf$) then goto SCREEN1_NEW
	if yr<(date('ccyy')-10) then goto SCREEN1_NEW
	if ssmax<53400 then goto SCREEN1_NEW
	if ins<0 or ins>10 then goto SCREEN1_NEW
	if pen<0 or pen>10 then goto SCREEN1_NEW
	if dfc<0 or dfc>10 then goto SCREEN1_NEW
	if resub$="1" and rtrm$(tlcn$)="" then goto SCREEN1_NEW
	if terminat$<>"0" and terminat$<>"1" then goto SCREEN1_NEW
	if sr1<0 or sr1>20 then goto SCREEN1_NEW
	if sr1>0 and sr2=0 then goto SCREEN1_NEW
! /r
! r: save screen1 values
	fnureg_write('electronic w-2 file',path$)
	fncreg_write('W-2 Company Name',a$(1))
	fncreg_write('W-2 Company Street Address',a$(2))
	fncreg_write('W-2 Company City',ct$)
	fncreg_write('W-2 Company State',st$)
	fncreg_write('W-2 Company Zip',zip$)
	open #hCompany:=fngethandle: "Name=[Q]\PRmstr\Company.h[cno],Shr",internal,outIn 
	rewrite #hCompany,using 'form pos 1,x 120,c 12,pos 150,x 80,n 2',rec=1: federal_id$,loccode
	close #hCompany: 
	fncreg_write('Miscellaneous Deduction Containing Employer Cost Group-Term Life Ins',str$(ins))
	fncreg_write('Miscellaneous Deduction Used For Pension',str$(pen))
	fncreg_write('Miscellaneous Deduction Used For Deferred Compensation',str$(dfc))
	fncreg_write('Miscellaneous Deduction Used For Dependent Care Assistance',str$(dcan))
	!
	fncreg_write('W-2 Personal ID Number',emppin$)
	fncreg_write('W-2 Resub Indicator',resub$)
	fncreg_write('W-2 Resub TLCN',tlcn$)
	fncreg_write('W-2 Contact Name',contact$)
	fncreg_write('W-2 Contact Phone Number',contactph$)
	fncreg_write('W-2 Contact Phone Extension',phoneext$)
	fncreg_write('W-2 Contact E-Mail',email$)
	fncreg_write('W-2 Terminating Business Indicator',terminat$)
	!
	fncreg_write('W-2 State Code',str$(sr1))
	fncreg_write('W-2 FIPS',str$(sr2))
	!
! /r
	open #hOut:=fngethandle: "Name=W2REPORT,RecL=512,eol=crlf,replace",display,output 
!
	gosub RecRA
	gosub RecRE ! kj 22610  was commented
NEXT_EMPLOYEE: ! r: main loop
! pr f "12,32,N 3,UT,N": readCount+=1/LREC(1)*100
	read #hEmployee,using fEmployee: eno,mat em$,ss$,em6,ta eof FINIS
	fEmployee: form pos 1,n 8,3*c 30,c 11,pos 122,n 2,pos 173,pd 3
	gosub NameParse
	p1=pos(em$(3),",",1) : comma=1
	if p1=0 then p1=pos(em$(3)," ",1): comma=0
	emct$=em$(3)(1:p1-1)
	gosub EXTRACT_STATE : emst$=holdst$ ! If COMMA=1 Then eMST$=EM$(3)(P1+2:P1+3) Else eMST$=EM$(3)(P1+1:P1+2)
	p2=len(rtrm$(em$(3)))
	p1=p2-4
	emzip$=em$(3)(p1:p2)
L2070: p1=pos(ss$,"-",1)
	if p1>0 then ss$(p1:p1)="": goto L2070 else ssn=val(ss$)
	checkkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",0)&cnvrt$("pd 6",0) ! index employee#,department# and payroll date
	restore #hChecks,key>=checkkey$: nokey NEXT_EMPLOYEE
L2120: read #hChecks,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat tcp eof L2480
	if heno<>eno then goto L2480
	if prd<beg_date or prd>end_date then goto L2120 ! not this year
! form pos 1,n 8,pos 48,n 2,pos 168,21*pd 5.2,pos 468,pd 3
	if tcd<1 or tcd>10 then tcd=1
! FILE_SHUFFLEh with this employee
	dedret=0
	cafded=0
	for j=1 to 20
		if newdedfed(j)=1 then 
			dedret=dedret+tcp(j+4)
		end if
		if dedfica(j)=1 then 
			cafded=cafded+tcp(j+4)
		end if
	next j
	w2(1)=min(w2(1)+tcp(31)-tcp(30)-cafded,ssmax-tcp(30)) ! TOTAL SOC-SEC WAGES
	w3=w3+tcp(2) ! TOTAL FICA WITHHELD
	w3(1)=w3(1)+tcp(31)-cafded ! TOTAL MEDICARE WAGES & TIPS
! if client$="Washington Parrish" then w3(1)=w3(1)+tcp(5) ! add deferred comp match to medicare wages
	w3(1)=min(mcmax,w3(1)) ! MC WAGES CANNOT EXCEED MAXIMUM
	if uprc$(med$)="Y" then  ! SS WH
		w2=w2+tcp(2) 
	else 
		w2=round(min(w3/(ssrate+mcrate)*ssrate,ssmax*ssrate),2)
	end if
	if uprc$(med$)="Y" then  ! MEDICARE WITHHELD
		w3(2)=w3(2)+tcp(3) 
		else 
		w3(2)=w3-w2
	end if
	w2(2)=w2(2)+tcp(30) ! FICA TIPS YTD
	w2(3)=w2(3)+tcp(31)-dedret ! TOTAL FEDERAL WAGES
	w2(4)=w2 ! W2(4)+tcp(2) ! FICA W/H YTD       (COULD BE +W2 INSTEAD OF +tcp(2)IF ONLY FICA PORTION GOES INTO W2 RECORD)
! w2(4)=W2 ! PUT SS W/H ONLY IN 2-W RECORD (EXCLUDE MEDICARE W/H)
	w2(5)=w2(5)+tcp(1) ! FED W/H YTD
	if ins>0 then w2(6)=w2(6)+tcp(4+ins) ! EMPLOYER COST GROUP LIFE INS.
	w2(7)=w2(7)+0 ! UNCOLLECTED EMPLOYEE FICA TAX ON TIPS
	w2(8)=w2(8)+tcp(24) ! EIC TOTAL
	w2(9)=w2(9)+0 ! ALLOCATED TIPS
	if dfc>0 then dc1=dc1+tcp(4+dfc)*100 ! DEFERRED COMPENSATION
	if dcan>0 then dca=dca+tcp(4+dcan)*100 ! DEPENDENT CARE ASSISTANCE
	if sr1><tcd then goto L2470
	s2(1)=s2(1)+((tcp(31)-dedret)*100)
	s2(2)=s2(2)+(tcp(4)*100)
L2470: !
	goto L2120
L2480: !
	if em6=9 then w2(1)=w2(4)=w3(1)=w3(2)=0 ! NO SS OR MC
	if em6=1 then w3(1)=w3(2)=0 ! NO MEDICARE
	if em6=2 then w2(1)=w2(4)=0 ! NO SOC-SEC
	if w2(3)<>0 or w2(1)<>0 then 
		! Gosub RecRE   kj 22610
		gosub RecRW
		gosub RecRS
		tw1=tw1+1
		tw2=tw2+1
		! Gosub RecRT   ! KJ 22610
		! tW2=0  kj 22610
	end if
goto NEXT_EMPLOYEE ! /r
! ______________________________________________________________________
RecRA: ! r:
	pr #hOut,using fRecRA: "RA",federal_id_val,emppin$(1:8),"",resub$,tlcn$,"98",a$(1),"",a$(2)(1:22),ct$,st$,zip$,"","","","","",a$(1),"",a$(2)(1:22),ct$,st$,zip$,"","","","","",contact$,contactph$,phoneext$,"",email$,"","","2","L",""
	fRecRA: form pos 1,c 2,pic(#########),c 8,c 9,c 1,c 6,c 2,c 57,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,c 57,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,c 27,c 15,c 5,c 3,c 40,c 3,c 10,c 1,c 1,c 12
return ! /r
RecRE: ! r:
! if client$="PiattCO" then emptype$="S"
	pr #hOut,using fRecRE: "RE",yr,"",federal_id_val,"",terminat$,"","",a$(1),"",a$(2)(1:22),ct$,st$,zip$,"",emptype$,"","","","","R","",0,""
	fRecRE: form pos 1,c 2,pic(####),c 1,pic(#########),c 9,c 1,c 4,c 9,c 57,c 22,c 22,c 22,c 2,c 5,c 4,c 1,c 4,c 23,c 15,c 2,c 1,c 1,n 1,c 291
return ! /r
! r: unused (2E record type??)
! ! form pos 1,c 2,pic(#########),c 15,c 15,c 20,c 4,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,18*pic(###########),c 22,2*pic(###########),c 56,n 1,c 1,c 1,n 1,c 23,
!   pr #hOut,using L2710: "2E",ct$,st$,"",zip$,nameFormat_sf$(nameFormat),typemp$(1:1),"","","",""
! L2710: form pos 1,c 2,g 25,g 10,2*g 5,2*g 1,g 2,g 4,g 2,c 71
!   return 
! /r
RecRW: ! r:
	for j=1 to 9: w2(j)=w2(j)*100: next j
	for j=1 to 2: w3(j)=w3(j)*100 : next j
	if pen=0 then pen$="0" else pen$="1"
	if dfc=0 then dfc$="" else dfc$="D"
	pr #hOut,using fRecRW: "RW",ssn,first$,mid$,last$,"","",em$(2)(1:22),emct$,emst$,emzip$,"","","","","",w2(3),w2(5),w2(1),w2(4),w3(1),w3(2),w2(2),w2(8),dca,dc1,0,0,0,0,0,0,0,0,0,"",w2(6),0,0,0,0,"",0,"",pen$,0,""
	fRecRW: form pos 1,c 2,pic(#########),c 15,c 15,c 20,c 4,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,19*pic(###########),c 11,5*pic(###########),c 23,n 1,c 1,c 1,n 1,c 23
	! pr #hOut,USING 2270: "RO","",W2(9),W2(7),0,0,0,0,0,"","","",0,0,0,0,0,0,0,"",0,0,""
	! form pos 1,c 2,c 9,7*pic(###########),c 176,c 1,c 9,7*pic(###########),c 11,2*pic(###########),c 128
return ! /r
RecRS: ! r: STATE RECORD
! if sr1=0 then goto 2880 ! NO STATE SELECTED
	if s2(1)<>0 or s2(2)<>0 then ! NO STATE WAGES
		! totrsrecs+=1
		pr #hOut,using fRecRS: "RS",sr2,"",ssn,first$,mid$,last$,"","",em$(2)(1:22),emct$,emst$,emzip$,"","","","","","","",0,0,0,0,0,"","","",sr2,s2(1),s2(2),"","",0,0,"","","",""
		fRecRS: form pos 1,c 2,g 2,c 5,pic(#########),c 15,c 15,c 20,c 4,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,c 2,c 6,2*pic(###########),pic(##),2*pic(########),c 5,c 20,c 6,g 2,2*pic(###########),c 10,c 1,2*pic(###########),c 7,c 75,c 75,c 25
	end if
	t1=t1+1: mat t1=t1+w2
	mat i1=i1+w2
	mat i2=i2+w3
	mat t2=t2+w3
	dc2=dc2+dc1
	dc3=dc3+dc1
	dca2=dca2+dca
	dca3=dca3+dca
	w2=w3=dca=dc1=0
	mat w2=(0)
	mat w3=(0)
! totalstatewages+=s2(1)
! totalstatewh+=s2(2)
! totalpeople+=1
	mat s2=(0)
return ! /r
RecRT: ! r:
	pr #hOut,using L3050: "RT",tw2,t1(3),t1(5),t1(1),t1(4),t2(1),t2(2),t1(2),t1(8),dca3,dc3,0,0,0,0,0,0,0,0,0,"",t1(6),0,0,0,0,0,""
	dc3=0 ! kj 120805
L3050: form pos 1,c 2,pic(#######),16*pic(###############),3*pic(###############),c 15,6*pic(###############),c 113
! pr #hOut,USING 2520: "RU",TW2,T1(9),T1(7),0,0,0,0,0,"",0,0,0,0,0,0,0,0,0,""
! form pos 1,c 2,pic(#######),7*pic(###############),c 240,9*pic(###############),c 23
	t1=0: mat t1=(0)
	mat t2=(0)
	return ! /r
! RecRV: ! r:
!   pr #hOut,using L3121: "RV",totalpeople,totalstatewages,totalstatewh," "
!   L3121: form pos 1,c 2,pic(#######),2*pic(###############),c 473
! return ! /r
RecRF: ! r:
	pr #hOut,using L3130: "RF"," ",tw1,""
	L3130: form pos 1,c 2,c 5,pic(#########),c 496
return ! /r
FINIS: ! r:
	gosub RecRT ! kj 22610
	gosub RecRF
	gosub FILE_SHUFFLE
XIT: fnxit
! /r
FILE_SHUFFLE: ! r:
	dim a$*512
	close #24: ioerr ignore
	close #hOut: ioerr ignore
	open #24: "Name=X,RecL=513,EOL=NONE,REPLACE",external,output 
	open #hOut:=fngethandle: "Name=w2report,RecL=512",display,input 
	do 
		linput #hOut: a$ eof L3320
		if a$(512:512)="X" then a$(512:512)=""
		write #24,using 'form pos 1,c 512,c 1': rpad$(a$,512),chr$(10)
	loop 
L3320: close #24: 
	close #hOut: 
	execute "COPY x "&path$
return ! /r
NameParse: ! r:
	dim first$*15,mid$*15,last$*20,em$(3)*30
	em$(1)=uprc$(rtrm$(em$(1))): ! nameFormat$="s"
	x1=pos(em$(1)," ",1)
	x2=pos(em$(1)," ",x1+1)
	! x3=pos(em$(1)," ",x2+1)
	if uprc$(nameFormat_sf$(nameFormat))="S" then ! last name first
		if x1=0 then x1=pos(em$(1),",",1)
		if x1>0 and em$(1)(x1-1:x1-1)="," then last$=em$(1)(1:x1-2) else last$=em$(1)(1:max(x1-1,1))
		if x2>0 then first$=em$(1)(x1+1:x2-1): mid$=em$(1)(x2+1:len(em$(1)))
		if x2=0 then first$=em$(1)(x1+1:len(em$(1)))(1:15): mid$=""
		x=pos(first$,",",1): if x>0 then first$(x:x)=""
	else
		first$=em$(1)(1:min(15,max(x1-1,1)))
		if x2>0 then mid$=em$(1)(x1+1:x2-1): last$=em$(1)(x2+1:len(em$(1)))
		if x2=0 then last$=em$(1)(x1+1:len(em$(1))): mid$=""
	end if
	! pr FIRST$,MID$,LAST$
return ! /r
EXTRACT_STATE: ! r: extract state name
	holdst$="          "
	p3=oldp3=0
	p4=10
	for j=1 to 10
		p3=pos(rtrm$(em$(3))," ",p3+1)
		if oldp3>p3 then 
			goto L5110 ! end of address reached
		end if
		if p3>0 then 
			oldp3=p3 
		end if
	next j
	L5110: !
	for j=1 to 10
		if rtrm$(em$(3)(oldp3-j:oldp3-j))="" or em$(3)(oldp3-j:oldp3-j)="," then
			if rtrm$(holdst$)<>"" then 
				goto L5150
			end if
		else 
			p4=p4-1: holdst$(p4:p4)=em$(3)(oldp3-j:oldp3-j)
		end if
	next j
		L5150: !
		holdst$=ltrm$(holdst$)(1:2)
	if holdst$="TE" then holdst$="TX"
return ! /r
include: ertn