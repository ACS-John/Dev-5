! formerly S:\acsPR\newprOPDReg
! Other Pay and Deductions Register
! r: setup library, on error, dims, open files, etc
autoLibrary
  on error goto Ertn
!
  dim em$*30,tdc(10),tcp(32),rpTemp(25),rptot(25),message$*40
  dim dat$*20
	dim name$(27)*8
	dim cp(32)
	dim printline1(20)
  dim printline1(20),printline2(20),dedname$(20)*8
  dim fullname$(20)*20,abbrevname$(20)*8, newdedcode(20)
!
  fnTop(program$)
  fnDedNames(mat fullname$,mat abbrevname$,mat newdedcode)
	d1=fnPayPeriodEndingDate
  open #4: "Name=[Q]\PRmstr\payrollchecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno]",internal,outIn,keyed
! r: setup mat name$, mat dedname$, numberded1, numberded2
  name$(1)="O/T"
  name$(2)="Other"
  name$(3)="Meals"
  name$(4)="Tips"
  name$(5)="Total"
  name$(26)="EIC"
  name$(27)="Total"
  numberded=0
  for j=6 to 25
    if trim$(abbrevname$(j-5))<>"" then
      dedname$(numberded+=1)=abbrevname$(j-5)(1:5)
    end if
  next j
  mat dedname$(numberded)
  numberded1=max(round(numberded/2,0),1) ! # of deductions listed on line 1
  numberded2=max(int(numberded/2),1) ! # of deductions listed on line 2
! /r
  if fnprocess=1 then goto START_REPORT else goto ASK_PAYROLL_DATE
! /r
ASK_PAYROLL_DATE: ! r:
  fnTos(sn$="OtherPayded")
  respc=0
  fnLbl(1,1,"",34,1) ! bigger screen
  fnLbl(2,1,"Payroll Date:",20,1)
  fnTxt(2,23,10,0,1,"3",0,"You can pr or reprint for any pay period.  Normally you would use the last payroll date.")
  resp$(respc+=1)=str$(d1)
  fnCmdKey("&Next",1,1,0,"Proceed with importing time." )
  fnCmdKey("E&xit",5,0,1,"Returns to menu")
  fnAcs(mat resp$,ckey) ! ask employee #
  if ckey=5 then goto Xit
  ppd=val(resp$(1))
goto START_REPORT ! /r
START_REPORT: !  r: main report loop
!
  on fkey 5 goto DONE
  fnopenprn
  gosub HDR
  open #1: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr",internal,outIn,keyed
  do
    ReadEmployee: !
    read #1,using "Form POS 1,N 8,C 30,pos 162,n 6": eno,em$,lastpaydate eof FINALTOTALS
    if fndate_mmddyy_to_ccyymmdd(lastpaydate)<>ppd then goto ReadEmployee
    mat rpTemp=(0): mat tcp=(0)
    a=pos (rtrm$(em$)," ",1)
    b=pos (rtrm$(em$)," ",a+1)
    ! if env$('client')="West Rest Haven" then goto L590
    em$=rtrm$(em$(max(a,b):30))&" "&em$(1:a)
    ! L590: form pos 1,n 8,c 30,pos 162,n 6,pos 173,2*pd 3
    checkkey$=cnvrt$("pic(ZZZZZZZ#)",eno)&"         "
    restore #4,key>=checkkey$: nokey ReadEmployee
    do
      read #4,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat cp eof L680
      if heno=eno and prd=ppd then
        holdckno=ckno
        mat tcp=tcp+cp : mat ttdc=ttdc+tdc
      end if
    loop while heno=eno
    L680: !
    if sum(tcp)=0 and sum(ttdc)=0 then goto ReadEmployee ! no pay on this person for this payroll date
    for j=1 to 25
      if j>4 then
        rpTemp(j)=rpTemp(j)+tcp(j)
      else
        rpTemp(j)=rpTemp(j)+tcp(j+26)
      end if
    nex j
    eic+=tcp(25)
    totaleic+=tcp(25)
    gosub L790
  loop
  !
  L790: mat rptot=rptot+rpTemp
  for j=1 to 15
    if rpTemp(j)><0 then goto L830
  next j
  L830: totTemp=0
  for j=1 to 20
    if newdedcode(j)=3 then goto L900
    if newdedcode(j)=1 then goto L890
    totTemp=totTemp-rpTemp(j+4)
    goto L900
    L890: totTemp=totTemp+rpTemp(j+4)
  L900: next j
  totTemp=totTemp-rpTemp(25)
  totded=totded+totTemp
  rpXxxx=rpTemp(1)+rpTemp(2)+rpTemp(3)+rpTemp(4)
  w=x=z=0: y=4: for j=1 to 20
    if trim$(abbrevname$(j))="" then goto L1000
    w+=1: if int(w/2)=w/2 then goto L990 else goto L970
  L970: printline1(x+=1)=rpTemp(j+4) ! set variables to pr line
    goto L1000
    L990: printline2(z+=1)=rpTemp(j+4) ! set line two
  L1000: next j
  mat printline1(numberded1): mat printline2(numberded2)
  line2part1$=rpt$(' ',65)
  if int(numberded1/2)=numberded1/2 then
    ! if rpTemp(4)>99.99 then
      ! pr #255,using 'form pos 1,Cr 46,c ': name$(4)&' (-1) too large to fit on next line:',' '&str$(rpTemp(4))
      pr #255,using L1040: eno,em$(1:15),rpTemp(1),rpTemp(2),rpTemp(3),-1,rpXxxx,mat printline1,totTemp pageoflow PGOF
      pr #255,using 'form pos 40,N 12.2 ': rpTemp(4)
      L1040: form pos 1,pic(zzzzzzzz),pos 10,c 15,n 7.2,n 8.2,pos 40,n 6.2,n 6.2,pos 52,n 8.2,pos 60,numberded1*n 12.2,n 12.2,skip 1
      line2part1$(41:41+12)=cnvrt$('N 12.2',rpTemp(4))
    ! else
    !   pr #255,using L1040: eno,em$(1:15),rpTemp(1),rpTemp(2),rpTemp(3),rpTemp(4),rpXxxx,mat printline1,totTemp pageoflow PGOF
    ! end if
  else
    ! if rpTemp(4)>99.99 then
      ! pr #255,using 'form pos 1,Cr 46,c ': name$(4)&' (-1) too large to fit on next line:',' '&str$(rpTemp(4))
      line2part1$(41:41+12)=cnvrt$('N 12.2',rpTemp(4))
      pr #255,using L105b: eno,em$(1:15),rpTemp(1),rpTemp(2),rpTemp(3),0,rpXxxx,mat printline1,totTemp pageoflow PGOF
      L105b: form pos 1,pic(zzzzzzzz),pos 10,c 15,n 7.2,n 8.2,pos 40,n 6.2,nz 6.2,pos 52,n 8.2,pos 60,numberded1*n 12.2,x 6,n 12.2,skip 1
    ! else
    !   pr #255,using L105a: eno,em$(1:15),rpTemp(1),rpTemp(2),rpTemp(3),rpTemp(4),rpXxxx,mat printline1,totTemp pageoflow PGOF
    !   L105a: form pos 1,pic(zzzzzzzz),pos 10,c 15,n 7.2,n 8.2,pos 40,n 6.2,n 6.2,pos 52,n 8.2,pos 60,numberded1*n 12.2,x 6,n 12.2,skip 1
    ! end if
  end if
  dim line2part1$*65
  pr #255,using L1060: line2part1$,mat printline2 pageoflow PGOF
  line2part1$=''
  L1060: form pos 1,c 65,numberded2*n 12.2,skip 1
return  ! /r
PGOF: ! r:
  pr #255: newpage
  gosub HDR
  continue
return  ! /r
FINALTOTALS: ! r:
  pr #255,using 'form skip 2,pos 10,c 12,skip 2': "Final Totals"
  mat rpTemp=rptot
  totTemp=totded
  rpXxxx=rpTemp(1)+rpTemp(2)+rpTemp(3)+rpTemp(4)
  for j=1 to 27
    if j=5 then
      pr #255,using FlabelAndCurrency: name$(j),rpXxxx
    else if j=26 then
      pr #255,using FlabelAndCurrency: name$(j),totaleic
    else if j=27 then
      pr #255,using FlabelAndCurrency: name$(j),totTemp
    else if j>5 and j<26 then
      if trim$(abbrevname$(j-5))<>"" then  ! skip ded if not used
        pr #255,using FlabelAndCurrency: rtrm$(abbrevname$(j-5)),rpTemp(j-1)
      end if
    else
      pr #255,using FlabelAndCurrency: name$(j),rpTemp(j)
      FlabelAndCurrency: form pos 5,c 8,pos 12,pic(---,---,---.##),skip 1
    end if
  next j
goto DONE ! /r
DONE: ! r:
  close #1: ioerr ignore
  close #2: ioerr ignore
  fncloseprn
Xit: fnXit ! /r

HDR: ! r:
  ! pr #255,Using 1380: DATE$,TIME$,env$('program_caption')
  pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
  pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
  pr #255: "\qc  {\f201 \fs20 \b "&env$('program_caption')&"}"
  pr #255: "\qc  {\f181 \fs16 \b Payroll Date: "&cnvrt$("pic(zzzz/zz/zz)",ppd)&"}"
  pr #255: "\ql   "
  !
  form skip 3,pos 1,c 8,pos namtab,c 40,skip 1,pos 1,c 8,pos 49,c 33,skip 1
  pr #255,using L1560: dat$
  L1560: form pos dattab,c 20,skip 2
  pr #255,using L1580: "<------------Other Pay------------>","     Other Deductions-->"
  L1580: form pos 25,c 35,pos 60,c 34,pos 94,c 39,skip 1
  if int(numberded1/2)=numberded1/2 then total$=" Total" else total$="       Total"
  pr #255,using L1610: "Number","Name",name$(1),name$(2),name$(3),name$(4),name$(5),mat dedname$,total$
  L1610: form pos 2,c 6,pos 14,c 4,pos 29,c 6,c 6,c 7,c 7,c 7,pos 66,numberded*c 6,c 12,skip 1
return  ! /r
include: ertn
