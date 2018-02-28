00010 ! formerly S:\acsGL\acGLClos
00020 ! Close Books at Year End
24000 ! r: setup and read constants
24020   library 'S:\Core\Library': fntop,fnxit,fnsearch,fnerror,fnUseDeptNo,fnTos,fnLbl,fnCmdSet,fnAcs,fnTxt,fnqgl,fnagl$,fnOpt,fnFra,fncreg_read,fncreg_write,fngethandle,fnGetFundList,fnrgl$
24040   on error goto ERTN
24060 ! 
24080   dim acno$*12,bc(13),bp(13),bud(13)
24100   dim resp$(10)*80
24120   fntop(program$,"Close Books at Year End")
24140   open #hCompany:=fngethandle: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input,relative 
24160   read #hCompany,using 'Form Pos 384,N 2',rec=1: nap
24180   close #hCompany: 
24200   fnGetFundList(mat fund_list)
24220 ! /r
30000 do ! r: the first screen
30020   fnTos(sn$="CloseYear1") 
30040   lc=rc=frame=0 : mylen=30 : mypos=mylen+2 : width=0
30060   fnLbl(lc+=1,1,"* * *   Warning   * * *",width,2)
30080   fnLbl(lc+=1,1,"This program is to be used only at the end of the",width,2)
30100   fnLbl(lc+=1,1,"year, after all reports have been processed.",width,2)
30120   fnLbl(lc+=1,1,"Enter CLOSE to continue:",mylen,1)
30140   fnTxt(lc,mypos,5) 
30160   resp$(rc_erase:=rc+=1)=""
30180   lc+=1
30200   fnLbl(lc+=1,1,"Year being closed:",mylen,1)
30220   fnTxt(lc,mypos,2,0,1,"30",0,"Enter the two digit code for the year you are closing.") 
30240   resp$(rc_year:=rc+=1)=""

31000   if fnUseDeptNo then

32000     ! lc+=1 : col3_pos=1 ! mypos+20
32020     ! resp_lrea_fund_1=rc+1
32060     ! col4_pos=mypos ! col3_pos+10
32080     ! fnLbl(lc+=1,col3_pos,'Last Retained Earnings Account(s)')
32100     ! for fund_item=1 to udim(mat fund_list)
32120     !   fnLbl(lc+=1,col3_pos,"Fund "&str$(fund_list(fund_item))&":",mylen,1)
32140     !   fnqgl(lc,col4_pos)
32160     !   rc+=1
32180     !   fncreg_read("last retained earnings account - fund "&str$(fund_list(fund_item)),resp$(rc)) : resp$(rc)=fnrgl$(resp$(rc))
32200     ! next fund_item

34000       lc+=1
34020       mylen=30 : mypos=mylen+3 : width=0
34040       fnFra(lc+=1,1,2,70,"Method of Closing","You must indicate if you will be closing to one equity account or to multiple accounts.",0)
34060       frame+=1
34080       fnOpt(1,3,"Close each fund to a separate account",0,frame) 
34100       fncreg_read('Close each fund to a separate account',resp$(rc_close1:=rc+=1),'False')
34120       fnOpt(2,3,"Close all departments to one retained earnings (equity) account",0,frame) 
34140       fncreg_read('Close all departments to one retained earnings (equity) account',resp$(rc_close2:=rc+=1),'False')

35500   ! else 
35520   !   col4_pos=col3_pos+32
35540   !   fnLbl(lc+=1,col3_pos,'Last Retained Earnings Account:',31,1)
35560   !   fnqgl(lc,col4_pos)
35580   !   rc+=1
35600   !   fncreg_read("last retained earnings account - no fund ",resp$(rc)) : resp$(rc)=fnrgl$(resp$(rc))
35900   end if
36000   fnCmdSet(2)
36020   fnAcs(sn$,0,mat resp$,ckey)
36040   if ckey=5 then goto XIT
36060   pas$=resp$(rc_erase)
36080   yr$=cnvrt$("pic(##)",val(resp$(rc_year)))
36100   if fnUseDeptNo then
36120     !   for fund_item=1 to udim(mat fund_list)
36140     !     last_retained_earnings_acct$(fund_item)=fnagl$(resp$(rc+=1))
36160     !     fncreg_write("last retained earnings account - fund "&str$(fund_list(fund_item)),last_retained_earnings_acct$(fund_item))
36180     !   next fund_item
36200     fncreg_write('Close each fund to a separate account',resp$(rc_close1))
36220     fncreg_write('Close all departments to one retained earnings (equity) account',resp$(rc_close2))
36240     if resp$(rc_close1)="True" then 
36260       closeDeptToRetainedEarnings=0 
36280     else 
36300       closeDeptToRetainedEarnings=1
36320     end if
36340   else 
36360     last_retained_earnings_acct$(1)=fnagl$(resp$(rc+=1))
36380     fncreg_write("last retained earnings account - no fund ",last_retained_earnings_acct$(1))
36400   end if
36420 loop until lwrc$(pas$)=lwrc$("CLOSE") ! /r
38000 open #hGlMstr1:=fngethandle: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLINDEX.h[cno],Shr",internal,outIn,keyed 
38020 open #hGlMstr2:=fngethandle: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\glIndx2.h[cno],Shr",internal,outIn,keyed 
38040 fGlMstr1: form pos 1,c 12,pos 81,41*pd 6.2
38060 open #hBudgetInfo:=fngethandle: "Name=[Q]\GLmstr\BudgetInfo.h[cno],KFName=[Q]\GLmstr\BudIndx.h[cno],Shr",internal,outIn,keyed 
38080 ! r: empty GLmstr\acprcks - file handle (#1) used to conflict hGlMstr1 (also #1) and it didn't close, but it did ioerr ignore, so it probably didn't do anything for years
38100 open #hAcPrCks:=fngethandle: "Name=[Q]\GLmstr\acprcks.h[cno],SIZE=0,RecL=110,Replace",internal,output ioerr ignore
38120 close #hAcPrCks: ioerr ignore
38140 ! /r
38160 ! r: reset some stuff in "[Q]\GLmstr\PRmstr.h[cno]"
38180 open #hPrMstr:=fngethandle: "Name=[Q]\GLmstr\PRmstr.h[cno],KFName=[Q]\GLmstr\PRINDEX.h[cno]",internal,outIn,keyed ioerr SCR2
38200 do
38220   read #hPrMstr,using 'Form POS 271,2*N 5': n1,n2 eof L500
38240   rewrite #hPrMstr,using 'Form POS 271,2*N 5': 0,0
38260 loop
38280 L500: !
38300 close #hPrMstr: 
38320 ! /r
44000 SCR2: ! 
44020   t5=0
44040   fnTos(sn$='CloseYear3')
44060   lc=0 : mylen=30 : mypos=mylen+2 : width=80
44080   fnLbl(lc+=1,1,"Enter the Last Retained Earnings Account or Equity Account.",width,2)
44100   fnLbl(lc+=1,1,"The account that dividend, income, and expenses will be closed to.",width,2)
44120   lc+=1 
44140   if fnUseDeptNo=0 or closeDeptToRetainedEarnings=1 then 
44160     fnLbl(lc+=1,1,"All accounts after this ",width,2)
44180   else 
44200     fnLbl(lc+=1,1,"All Accounts for this Cost Center after this ",width,2)
44220   end if 
44240   fnLbl(lc+=1,1,"be reset with zero balances.",width,2)
44260   fnLbl(lc+=1,1,"account will be reset with zero balances.",width,2)
44280   fnLbl(lc+=1,1,"Enter Account Number:",mylen,1)
44300   fnqgl(lc,mypos)
44320   resp$(1)=''
44440   fnCmdSet(11)
44460   fnAcs(sn$,0,mat resp$,ckey)
44480   if ckey=5 then goto XIT
44500   glnumber$=fnagl$(resp$(1))
44520   if closeDeptToRetainedEarnings then 
44540     fncreg_write("last retained earnings account - fund "&str$(val(glnumber$(1:3))),resp$(1))
44560   else 
44580     fncreg_write("last retained earnings account - no fund",resp$(1))
44600   end if 
44620   read #hGlMstr1,using 'form pos 1,c 3,c 6,c 3',key=glnumber$: dno$,ano$,sno$ nokey SCR2
44640   acno$=glnumber$(1:3)&"         "
44660 ! 
44680 read #hGlMstr1,using fGlMstr1,key>=acno$: acno$,bb,cb,mat bc,mat bp,mat bud nokey SCR2
44700 goto L770
48000 do
48020   read #hGlMstr1,using fGlMstr1: acno$,bb,cb,mat bc,mat bp, mat bud eof L940
48040   L770: !
48060   if fnUseDeptNo=0 or closeDeptToRetainedEarnings=1 then goto L790
48080   if glnumber$(1:3)><acno$(1:3) then dno=ano=sno=0: goto SCR2
48100   L790: !
48120   if acno$=glnumber$ then 
48140     cb=-t5
48160     ! bC(NAP)=CB   ! SET RETAINED BALANCE IN HISTORY AFTER CLOSING
48180     if nap=0 or nap>13 then nap=12
48200   end if
48220   pbp=bp(nap)
48240   mat bp=bc
48260   mat bc=(0)
48280   bb=cb
48300   t5=t5+cb
48320   if acno$>glnumber$ then  ! create a budget history record
48340     write #hBudgetInfo,using "form pos 1,c 12,c 2,2*pd 6.2": acno$,yr$,cb,sum(bud)
48360     cb=bb=0
48380   end if
48400   rewrite #hGlMstr1,using 'form pos 1,c 12,pos 81,41*pd 6.2,pos 327,pd 6.2': acno$,bb,cb,mat bc,mat bp,mat bud,pbp
48420 loop
52000 L940: !
52020   if fnUseDeptNo=0 or closeDeptToRetainedEarnings=1 then 
52040     goto FINIS
52060   end if
52080   dno=ano=sno=0 
52100 goto SCR2
52120 ! 
54000 FINIS: ! r:
54020   close #hGlMstr1: 
54040   close #hGlMstr2: 
54060   close #hBudgetInfo: 
54480 goto XIT ! /r
58000 XIT: fnxit
58500 IGNORE: continue 
61010 ! <Updateable Region: ERTN>
61020 ERTN: fnerror(program$,err,line,act$,"xit")
61030   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
61040   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
61050   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
61060 ERTN_EXEC_ACT: execute act$ : goto ERTN
61070 ! /region
61080 ! ______________________________________________________________________
