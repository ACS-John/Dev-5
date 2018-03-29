00010 ! formerly S:\acsPR\newprRegTot
00020 ! beginning with 4.0 the tax deposit reads from the checkhistory file
00030 ! ______________________________________________________________________
00040 library 'S:\Core\Library': fntop,fnxit, fnerror,fnDedNames,fnopenprn,fncloseprn,fngethandle,fnTos,fnFra,fnTxt,fnLbl,fnCmdKey,fnAcs,fnss_employee,fnss_employer,fnGetPayrollDates
00050 on error goto ERTN
00070 ! 
00080 dim deptot(999,2),t(36)
00090 dim fullname$(20)*20,ab$(20)*8,cp(32),tdc(10)
00100 dim d1$*20
00110 dim em$*30
00120 ! 
00130 fntop(program$)
00150 ! 
00220 fnGetPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4,d2,d1$)
00242 ssr1=fnss_employee
00243 ssr2=fnss_employer
00250 ! If FNPROCESS=1 Then Goto 410
00260 fnTos(sn$="TaxDeposit") 
00262 rc=0: mylen=22: mypos=mylen+3: frameno=1
00270 fnFra(1,1,3,40,"Date Range of Deposit","Enter the date range for the payrolls to be included.")
00280 fnLbl(1,1,"Beginning Date:",mylen,1,0,frameno)
00290 fnTxt(1,mypos,12,0,1,"3",0,"Enter the date of the first payroll to be included in this deposit. ",frameno)
00292 resp$(rc+=1)=str$(beg_date)
00300 fnLbl(2,1,"Ending Date:",mylen,1,0,frameno)
00310 fnTxt(2,mypos,12,0,1,"3",0,"Enter the last payroll date that should be included in this deposit. ",frameno)
00312 resp$(rc+=1)=str$(end_date)
00320 fnCmdKey("Next",1,1,0,"Calculate tax deposit.")
00330 fnCmdKey("Cancel",5,0,1,"Returns to menu without printing.")
00340 fnAcs(sn$,0,mat resp$,ckey)
00350 if ckey=5 then goto XIT
00360 beg_date=val(resp$(1)) 
00362 end_date=val(resp$(2))
00370 ! 
00380 fnopenprn
00390 ! 
00400 fnDedNames(mat fullname$,mat ab$)
00430 for j=1 to 20
00440   ab$(j)=lpad$(rtrm$(ab$(j)),8)
00450 next j
00460 gosub PrHeader
00470 open #h_employee:=fngethandle: "Name=[Q]\PRmstr\RPMstr.h[cno],Shr",internal,input,relative 
00480 open #h_checks:=fngethandle: "Name=[Q]\PRmstr\payrollchecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno]",internal,outIn,keyed 
00490 do ! 
00492   ReadEmployee: ! 
00500   read #h_employee,using "Form POS 1,N 8,C 30": eno,em$ eof TOTAL_THAT
00510   checkkey$=cnvrt$("pic(ZZZZZZZ#)",eno)&"         "
00520   foundone=1
00530   restore #h_checks,key>=checkkey$: nokey ReadEmployee
00536   do
00542     read #h_checks,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,dep,prd,ckno,mat tdc,mat cp eof ReadEmployee
00550     if eno=heno and prd=>beg_date and prd<=end_date then
00562       if dep>0 then ! department 0 not totaled
00570         deptot(dep,1)=deptot(dep,1)+cp(31)
00580         deptot(dep,2)=deptot(dep,2)+cp(2)+cp(3)
00581       end if
00582       ! pr #255: 'employee number: '&str$(heno)&' employee record: '&str$(rec(h_employee))&' check number: '&str$(ckno)&' check history record number: '&str$(rec(h_checks))
00590       pr #255,using 'form pos 1,pic(zzzz/zz/zz),pos 11,19*n 10.2,n 4': prd,cp(31),cp(2)+cp(3),cp(1),cp(4),cp(5),cp(6),cp(7),cp(8),cp(9),cp(10),cp(11),cp(12),cp(13),cp(14),cp(32),cp(2)+cp(3) pageoflow PGOF
00600       cp15throughCp24=cp(15)+cp(16)+cp(17)+cp(18)+cp(19)+cp(20)+cp(21)+cp(22)+cp(23)+cp(24)
00610       if cp15throughCp24<>0 then 
00620         pr #255,using 'form pos 61,10*c 10': ab$(11),ab$(12),ab$(13),ab$(14),ab$(15),ab$(16),ab$(17),ab$(18),ab$(19),ab$(20)
00630         pr #255,using 'form pos 59,10*n 10.2': cp(15),cp(16),cp(17),cp(18),cp(19),cp(20),cp(21),cp(22),cp(23),cp(24) pageoflow PGOF
00640       end if
00660       for j=1 to 32 : t(j)=t(j)+cp(j) : next j
00670       t(34)=t(34)+tdc(10)
00672       t(35)=t(35)+tdc(9)
00680       if foundone=1 then foundone=0: t(36)=t(36)+1
00682     end if
00690   loop while eno=heno and prd<=end_date
00699 loop
00700 ! ______________________________________________________________________
00710 PGOF: ! r:
00720   pr #255: newpage
00730   gosub PrHeader
00740 continue ! /r
00750 ! ______________________________________________________________________
00760 TOTAL_THAT: ! r: EoF target
00768   pr #255:''
00770   pr #255,using 'form pos 4,c 5,pos 11,16*n 10.2,n 5': "Total",t(31),t(2)+t(3),t(1),t(4),t(5),t(6),t(7),t(8), t(9),t(10),t(11),t(12),t(13),t(14),t(32),t(2)+t(3) pageoflow PGOF
00790   if t(15)+t(16)+t(17)+t(18)+t(19)+t(20)+t(21)+t(22)+t(23)+t(24)<>0 then 
00800     pr #255,using 'form pos 59,10*n 10.2': t(15),t(16),t(17),t(18),t(19),t(20),t(21),t(22),t(23),t(24) pageoflow PGOF
00810   end if
00811   pr #255: ''
00812   pr #255: ''
00814   pr #255,using L820: "Calculated Tax Deposit:" ,"Medicare W/H",t(3),"SS Withholding",t(2),"Federal Withholding",t(1),"Employer's FICA Match",round(t(2)/ssr1*ssr2,2)+t(3),"Less EIC",-t(25),"Total Deposit",t(3)+t(2)+t(1)+round(t(2)/ssr1*ssr2,2)+t(3)-t(25) ! 2013
00820   L820: form pos 8,c 30,skip 1,pos 10,c 30,n 12.2,skip 1,pos 10,c 30,n 12.2,skip 1,pos 10,c 30,n 12.2,skip 1,pos 10,c 30,n 12.2,skip 1,pos 10,c 30,n 12.2,skip 1,pos 42,"----------",skip 1,pos 20,c 20,n 12.2,skip 1,pos 42,"=========="
00828   pr #255: ''
00830   pr #255,using 'form pos 8,c 40': "Summary of FICA Match by Department:"
00850   pr #255,using L860: "Dept", "FICA Match"
00860   L860: form pos 10,c 4,pos 25,c 15,skip 2
00870   for j=1 to 999
00880     if deptot(j,2)<>0 then
00890       pr #255,using 'form pos 10,n 4,pos 25,n 10.2': j, round(deptot(j,2),2) ! 2013
00910       gtotal=gtotal+round(deptot(j,2),2) ! 2013
00920     end if
00922   next j
00930   pr #255,using L940: "Total",gtotal
00940   L940: form pos 25,"__________",skip 1,pos 10,c 6,pos 23,n 12.2,skip 1,pos 25,"=========="
00945   pr #255,using "form skip 2,pos 1,c 40": "Total Employees: "&str$(t(36))
00960   fncloseprn
00970   close #1: ioerr ignore
00980   goto XIT
00990 ! /r
01000 PrHeader: ! r:
01002   pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
01010   pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
01020   pr #255: "\qc  {\f201 \fs20 \b "&env$('program_caption')&"}"
01030   pr #255: "\qc  {\f181 \fs16 \b From: "&cnvrt$("pic(zzzz/zz/zz)",beg_date)&"  To: "&cnvrt$("pic(zzzz/zz/zz)",end_date)&"}"
01040   pr #255: "\ql   "
01050   ! pr #255,Using 1060: TIME$,"From ",BEGD," To ",ENDD
01070   pr #255,using L1080: "Date","     Gross","   FICA/ME","   Federal","     State",ab$(1),ab$(2),ab$(3),ab$(4),ab$(5),ab$(6),ab$(7),ab$(8),ab$(9),ab$(10),"   Net","  Emp FICA"
01080   L1080: form pos 3,c 4,pos 11,4*c 10,x 2,10*c 10,x 2,c 6,c 10,c 5,skip 2
01090   return ! /r
01110 XIT: fnxit
01120 IGNORE: continue 
01130 ! <Updateable Region: ERTN>
01140 ERTN: fnerror(program$,err,line,act$,"xit")
01150   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01160   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01170   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01180 ERTN_EXEC_ACT: execute act$ : goto ERTN
01190 ! /region
