00010 ! formerly S:\acsPR\newprOPDReg
00020 ! Other Pay and Deductions Register
22000 ! r: setup library, on error, dims, open files, etc
22020   library 'S:\Core\Library': fntop,fnxit, fnwait,fnopenprn,fncloseprn,fnerror,fndate_mmddyy_to_ccyymmdd,fnTos,fnLbl,fnTxt,fnCmdKey,fnAcs,fnprocess,fnDedNames,fnGetPayrollDates
22040   on error goto ERTN
22060 ! 
22080   dim em$*30,tdc(10),tcp(32),rpTemp(25),rptot(25),message$*40
22100   dim dat$*20,name$(27)*8,cp(32),printline1(20)
22120   dim printline1(20),printline2(20),dedname$(20)*8
22140   dim fullname$(20)*20,abbrevname$(20)*8, newdedcode(20)
22160 ! ______________________________________________________________________
22180   fntop(program$)
22200   fnDedNames(mat fullname$,mat abbrevname$,mat newdedcode)
22220   fnGetPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4,d1)
22240   open #4: "Name=[Q]\PRmstr\payrollchecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno]",internal,outIn,keyed 
22260 ! r: setup mat name$, mat dedname$, numberded1, numberded2
22280   name$(1)="O/T"
22300   name$(2)="Other"
22320   name$(3)="Meals"
22340   name$(4)="Tips"
22360   name$(5)="Total"
22380   name$(26)="EIC"
22400   name$(27)="Total"
22420   numberded=0
22440   for j=6 to 25
22460     if trim$(abbrevname$(j-5))<>"" then 
22480       dedname$(numberded+=1)=abbrevname$(j-5)(1:5)
22500     end if 
22520   next j
22540   mat dedname$(numberded)
22560   numberded1=max(round(numberded/2,0),1) ! # of deductions listed on line 1
22580   numberded2=max(int(numberded/2),1) ! # of deductions listed on line 2
22600 ! /r
22620   if fnprocess=1 then goto START_REPORT else goto ASK_PAYROLL_DATE
22640 ! /r
28000 ASK_PAYROLL_DATE: ! r:
28020   fnTos(sn$="OtherPayded") 
28040   respc=0
28060   fnLbl(1,1,"",34,1) ! bigger screen
28080   fnLbl(2,1,"Payroll Date:",20,1)
28100   fnTxt(2,23,10,0,1,"3",0,"You can pr or reprint for any pay period.  Normally you would use the last payroll date.")
28120   resp$(respc+=1)=str$(d1)
28140   fnCmdKey("&Next",1,1,0,"Proceed with importing time." ) 
28160   fnCmdKey("E&xit",5,0,1,"Returns to menu")
28180   fnAcs(sn$,0,mat resp$,ckey) ! ask employee #
28200   if ckey=5 then goto XIT
28220   ppd=val(resp$(1))
28240 goto START_REPORT ! /r
30000 START_REPORT: !  r: main report loop
30020 ! 
30040   on fkey 5 goto DONE
30060   fnopenprn
30080   gosub HDR
30100   open #1: "Name=[Q]\PRmstr\RPMstr.h[cno],KFName=[Q]\PRmstr\RPIndex.h[cno],Shr",internal,outIn,keyed 
30120   do
30140     ReadEmployee: !
30160     read #1,using "Form POS 1,N 8,C 30,pos 162,n 6": eno,em$,lastpaydate eof FINALTOTALS
30180     if fndate_mmddyy_to_ccyymmdd(lastpaydate)<>ppd then goto ReadEmployee
30200     mat rpTemp=(0): mat tcp=(0)
30220     a=pos (rtrm$(em$)," ",1)
30240     b=pos (rtrm$(em$)," ",a+1)
30260     ! if env$('client')="West Rest Haven" then goto L590
30280     em$=rtrm$(em$(max(a,b):30))&" "&em$(1:a)
30300     ! L590: form pos 1,n 8,c 30,pos 162,n 6,pos 173,2*pd 3
30320     checkkey$=cnvrt$("pic(ZZZZZZZ#)",eno)&"         "
30340     restore #4,key>=checkkey$: nokey ReadEmployee
32000     do 
32020       read #4,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat cp eof L680
32040       if heno=eno and prd=ppd then 
32060         holdckno=ckno
32080         mat tcp=tcp+cp : mat ttdc=ttdc+tdc
32100       end if
32120     loop while heno=eno
35000     L680: !
35020     if sum(tcp)=0 and sum(ttdc)=0 then goto ReadEmployee ! no pay on this person for this payroll date
35040     for j=1 to 25
35060       if j>4 then 
35080         rpTemp(j)=rpTemp(j)+tcp(j)
35100       else
35120         rpTemp(j)=rpTemp(j)+tcp(j+26)
35140       end if
35160     nex j
35180     eic+=tcp(25)
35200     totaleic+=tcp(25)
35220     gosub L790
35240   loop
35260   !
36000   L790: mat rptot=rptot+rpTemp
36020   for j=1 to 15
36040     if rpTemp(j)><0 then goto L830
36060   next j
36080   L830: totTemp=0
36100   for j=1 to 20
36120     if newdedcode(j)=3 then goto L900
36140     if newdedcode(j)=1 then goto L890
36160     totTemp=totTemp-rpTemp(j+4)
36180     goto L900
36200     L890: totTemp=totTemp+rpTemp(j+4)
36220   L900: next j
36240   totTemp=totTemp-rpTemp(25)
36260   totded=totded+totTemp
36280   rpXxxx=rpTemp(1)+rpTemp(2)+rpTemp(3)+rpTemp(4)
36300   w=x=z=0: y=4: for j=1 to 20
36320     if trim$(abbrevname$(j))="" then goto L1000
37000     w+=1: if int(w/2)=w/2 then goto L990 else goto L970
37020   L970: printline1(x+=1)=rpTemp(j+4) ! set variables to pr line
37040     goto L1000
37060     L990: printline2(z+=1)=rpTemp(j+4) ! set line two
37080   L1000: next j
37100   mat printline1(numberded1): mat printline2(numberded2)
38000   line2part1$=rpt$(' ',65)
42000   if int(numberded1/2)=numberded1/2 then 
42020     ! if rpTemp(4)>99.99 then
42040       ! pr #255,using 'form pos 1,Cr 46,c ': name$(4)&' (-1) too large to fit on next line:',' '&str$(rpTemp(4))
42060       pr #255,using L1040: eno,em$(1:15),rpTemp(1),rpTemp(2),rpTemp(3),-1,rpXxxx,mat printline1,totTemp pageoflow PGOF 
42080       pr #255,using 'form pos 40,N 12.2 ': rpTemp(4)
42100       L1040: form pos 1,pic(zzzzzzzz),pos 10,c 15,n 7.2,n 8.2,pos 40,n 6.2,n 6.2,pos 52,n 8.2,pos 60,numberded1*n 12.2,n 12.2,skip 1
42120       line2part1$(41:41+12)=cnvrt$('N 12.2',rpTemp(4))
42140     ! else
42160     !   pr #255,using L1040: eno,em$(1:15),rpTemp(1),rpTemp(2),rpTemp(3),rpTemp(4),rpXxxx,mat printline1,totTemp pageoflow PGOF 
42180     ! end if
42200   else
42220     ! if rpTemp(4)>99.99 then
42240       ! pr #255,using 'form pos 1,Cr 46,c ': name$(4)&' (-1) too large to fit on next line:',' '&str$(rpTemp(4))
42260       line2part1$(41:41+12)=cnvrt$('N 12.2',rpTemp(4))
42280       pr #255,using L105b: eno,em$(1:15),rpTemp(1),rpTemp(2),rpTemp(3),0,rpXxxx,mat printline1,totTemp pageoflow PGOF 
42300       L105b: form pos 1,pic(zzzzzzzz),pos 10,c 15,n 7.2,n 8.2,pos 40,n 6.2,nz 6.2,pos 52,n 8.2,pos 60,numberded1*n 12.2,x 6,n 12.2,skip 1
42320     ! else
42340     !   pr #255,using L105a: eno,em$(1:15),rpTemp(1),rpTemp(2),rpTemp(3),rpTemp(4),rpXxxx,mat printline1,totTemp pageoflow PGOF
42360     !   L105a: form pos 1,pic(zzzzzzzz),pos 10,c 15,n 7.2,n 8.2,pos 40,n 6.2,n 6.2,pos 52,n 8.2,pos 60,numberded1*n 12.2,x 6,n 12.2,skip 1
42380     ! end if
42400   end if
42420   dim line2part1$*65
42440   pr #255,using L1060: line2part1$,mat printline2 pageoflow PGOF
42460   line2part1$=''
42480   L1060: form pos 1,c 65,numberded2*n 12.2,skip 1
42500 return  ! /r
44000 PGOF: ! r:
44020   pr #255: newpage
44040   gosub HDR
44060   continue 
44080 return  ! /r
48000 FINALTOTALS: ! r:
48020   pr #255,using 'form skip 2,pos 10,c 12,skip 2': "Final Totals"
48040   mat rpTemp=rptot
48060   totTemp=totded
48080   rpXxxx=rpTemp(1)+rpTemp(2)+rpTemp(3)+rpTemp(4)
48100   for j=1 to 27
48120     if j=5 then 
48140       pr #255,using FlabelAndCurrency: name$(j),rpXxxx
48160     else if j=26 then 
48180       pr #255,using FlabelAndCurrency: name$(j),totaleic
48200     else if j=27 then 
48220       pr #255,using FlabelAndCurrency: name$(j),totTemp
48240     else if j>5 and j<26 then 
48260       if trim$(abbrevname$(j-5))<>"" then  ! skip ded if not used
48280         pr #255,using FlabelAndCurrency: rtrm$(abbrevname$(j-5)),rpTemp(j-1)
48300       end if
48320     else
48340       pr #255,using FlabelAndCurrency: name$(j),rpTemp(j)
48360       FlabelAndCurrency: form pos 5,c 8,pos 12,pic(---,---,---.##),skip 1
48380     end if
48400   next j
48420 goto DONE ! /r
54000 DONE: ! r:
54020   close #1: ioerr ignore
54040   close #2: ioerr ignore
54060   fncloseprn
54080 XIT: fnxit ! /r
54100 IGNORE: continue 
58000 HDR: ! r:
58020   ! pr #255,Using 1380: DATE$,TIME$,env$('program_caption')
58040   pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
58060   pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
58080   pr #255: "\qc  {\f201 \fs20 \b "&env$('program_caption')&"}"
58100   pr #255: "\qc  {\f181 \fs16 \b Payroll Date: "&cnvrt$("pic(zzzz/zz/zz)",ppd)&"}"
58120   pr #255: "\ql   "
58140   ! 
58160   form skip 3,pos 1,c 8,pos namtab,c 40,skip 1,pos 1,c 8,pos 49,c 33,skip 1
58180   pr #255,using L1560: dat$
58200   L1560: form pos dattab,c 20,skip 2
58220   pr #255,using L1580: "<------------Other Pay------------>","     Other Deductions-->"
58240   L1580: form pos 25,c 35,pos 60,c 34,pos 94,c 39,skip 1
58260   if int(numberded1/2)=numberded1/2 then total$=" Total" else total$="       Total"
58280   pr #255,using L1610: "Number","Name",name$(1),name$(2),name$(3),name$(4),name$(5),mat dedname$,total$
58300   L1610: form pos 2,c 6,pos 14,c 4,pos 29,c 6,c 6,c 7,c 7,c 7,pos 66,numberded*c 6,c 12,skip 1
58320 return  ! /r
60000 ! <Updateable Region: ERTN>
60020 ERTN: fnerror(program$,err,line,act$,"xit")
60040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
60060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
60080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
60100 ERTN_EXEC_ACT: execute act$ : goto ERTN
60120 ! /region
