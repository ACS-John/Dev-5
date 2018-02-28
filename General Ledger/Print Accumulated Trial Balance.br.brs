00010 ! formerly S:\acsGL\AcGLAcTB
00020 ! pr Accumulated Trial Balance
12000 ! r: setup library, on error, dims, and constants
12020   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnerror,fnprocess,fnpedat$,fnTos,fnFra,fnOpt,fnLbl,fnqgl,fnCmdSet,fnAcs,fnagl$,fnChk,fnTxt,fngethandle,fncreg_read,fncreg_write
12040   on error goto ERTN
12060 ! ______________________________________________________________________
12080   dim d$*50,tr(7),tr$*12,td$*30,n$*12,t$*12,x$*3,cap$*128
12100   dim resp$(20)*128,bp(13)
12120   dim cogl$(3)*12,u$*12
12140   dim a$(9)*3
12160   a$(1)="C/D"
12180   a$(2)="C/R"
12200   a$(3)="ADJ"
12220   a$(4)="A/P"
12240   a$(5)="PR"
12260   a$(6)="A/R"
12280   a$(7)="S/J"
12300   a$(8)="P/J"
12320   a$(9)=" "
12340 ! /r
14000   fntop(program$,cap$="Print Accumulated Trial Balance")
14020   open #20: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input,relative 
14040   read #20,using 'Form Pos 152,3*C 12',rec=1: mat cogl$
14060   read #20,using "Form pos 296,N 2",rec=1: lmu
14080   read #20,using 'Form Pos 384,n 2',rec=1: nap
14100   close #20: 
14120   fncreg_read('Last "Capital" Account',lastCapitalAccount$,cogl$(3))
14140   fncreg_Read('Print Ending Balance on First Line',petro_opt$,'False')
14160   fncreg_Read(cap$&': DayStart',tmp$) : startday=val(tmp$)
14180   fncreg_Read(cap$&': DayEnd'  ,tmp$) : endday=val(tmp$)
14200   m2GlmCbAmtPos=87
14220   if nap=13 then m1GlmBbAmtPos=171-6 else m1GlmBbAmtPos=171-12 ! 171 was 249
14240   ! last=val(lastCapitalAccount$(4:9))
14260   open #h_glmstr:=1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",internal,input,keyed 
14280   open #h_actrans:=fngethandle: "Name=[Q]\GLmstr\AcTrans.h[cno],KFName=[Q]\GLmstr\AcTrIdx.h[cno],Shr",internal,input,keyed 
14300   if fnprocess=1 then s1=1 : goto mainLoopInit
14320 goto SCREEN1
16000 SCREEN1: ! r:
16020   fnTos(sn$="Acglactb")
16040   mylen=53: mypos=mylen+3 : rc=0 : right=1
16060   fnLbl(1,1,'General ledger number for the last "Capital" account:',mylen,right)
16080   fnqgl(1,mypos,0,2)
16100   resp$(respc_lastCapitalAccount:=rc+=1)=lastCapitalAccount$
16120   fnChk(2,mypos,"Print Ending Balance on First Line:",1,0)
16140   resp$(respc_prBalFirst:=rc+=1)=petro_opt$
16160   fnLbl(3,1,"Period Code to pr (blank for all):",mylen,right)
16180   fnTxt(3,mypos,2,0,1,"30",0,"You can pr any month or the entire year.")
16200   resp$(respc_periodCode:=rc+=1)="" ! STR$(LMU)
16220   fnFra(5,1,5,90,"Selection Type"," ",0) : frameno=1
16240   fnOpt(1,3,"Print All GL Accounts",0,frameno)
16260   resp$(respc_printAll:=rc+=1)="True"
16280   fnOpt(2,3,"Print Selected GL Accounts",0,frameno)
16300   resp$(respc_printSelected:=rc+=1)="False"
16320   fnOpt(3,3,"Print a Range of Accounts",0,frameno)
16340   resp$(respc_printRange:=rc+=1)="False"
16380   mylen=6 : mypos=mylen+2
16400   fnLbl(4,1+10,'First:',mylen,right,0,frameno)
16420   fnqgl(4,mypos+10,frameno,2)
16440   resp$(respc_rangeStart:=rc+=1)=""
16460   fnLbl(5,1+10,'Last:',mylen,right,0,frameno)
16480   fnqgl(5,mypos+10,frameno,2)
16500   resp$(respc_rangeEnd:=rc+=1)=""
16520   fnFra(12,1,4,90,"Filters"," ",0) : frameno=2
16540   mylen=14 : mypos=mylen+2
16560   fnLbl(1,1,'Starting Date:',mylen,right,0,frameno,0,"Enter a date to filter results or blank for all")
16580   fnTxt(1,mypos,10,0,1,"3",0,"Enter a date to filter results or blank for all",frameno) 
16600   resp$(resp_dateStart:=rc+=1)=date$(startday,'ccyymmdd')
16620   fnLbl(2,1,'Ending Date:',mylen,right,0,frameno,0,"Enter a date to filter results or blank for all")
16640   fnTxt(2,mypos,10,0,1,"3",0,"Enter a date to filter results or blank for all",frameno) 
16660   resp$(resp_dateEnd:=rc+=1)=date$(endday,'ccyymmdd')
16680   fnLbl(4,1,'Fund Number:',mylen,right,0,frameno,0,"Select a Cost Center to filter results or blank for all") ! costCenterFilter
16700   fnTxt(4,mypos,2,0,1,"30",0,"Select a Cost Center to filter results or blank for all",frameno)
16720   resp$(resp_costCenter:=rc+=1)=""
16740   fnCmdSet(2)
16760   fnAcs(sn$,0,mat resp$,ckey)
22000   if ckey=5 then goto XIT
22020   lastCapitalAccount$=cogl$(3)=fnagl$(resp$(respc_lastCapitalAccount))
22060   petro_opt$=resp$(respc_prBalFirst)
22080   periodToPrint=val(resp$(respc_periodCode)) ! period code to print
22100   if resp$(respc_printAll)="True" then s1=1 ! method of selecting
22120   if resp$(respc_printSelected)="True" then s1=2
22140   if resp$(respc_printRange)="True" then s1=3
22160   n1$=fnagl$(resp$(respc_rangeStart))
22180   n2$=fnagl$(resp$(respc_rangeEnd))
22200   startday=days(resp$(resp_dateStart),'ccyymmdd')
22220   endday=days(resp$(resp_dateEnd),'ccyymmdd')
22240   costCenterFilter=val(resp$(resp_costCenter))
24000   fncreg_write('Last "Capital" Account',lastCapitalAccount$)
24020   fncreg_write('Print Ending Balance on First Line',petro_opt$)
24040   fncreg_write(cap$&': DayStart',str$(startday)) 
24060   fncreg_write(cap$&': DayEnd'  ,str$(endday)) 
24080   fncreg_write('Print Ending Balance on First Line',petro_opt$)
26000   if periodToPrint>1 then m1GlmBbAmtPos=periodToPrint*6+81
26020   m2GlmCbAmtPos=periodToPrint*6+87
26040   if periodToPrint><1 then goto L640
26060   if nap=13 then m1GlmBbAmtPos=171-6 else m1GlmBbAmtPos=171-12 ! 171 was 249
26080 L640: if s1=3 and n2$<n1$ then goto SCREEN1
26100 ! Read #h_GLmstr,Using 880,Key=N1$: N$,D$,BB,CB Nokey 670
26120   if f1=1 then goto AfterReadGlmstr
26140   f1=1
26160   ! if fnUseDeptNo=0 or fnprocess=1 then goto READ_GLMSTR ! L840
28200   n$=cnvrt$("N 3",costCenterFilter)&"         "
28220   restore #h_glmstr,key>=n$: nokey SCREEN1
28240   on pageoflow goto PGOF
28260   on fkey 5 goto TOTALS
28280 goto mainLoopInit ! /r (costCenterFilter)
32000 mainLoopInit: ! r: main loop setup (costCenterFilter)
32020   fnopenprn
32040   gosub HDR
32060   goto READ_GLMSTR ! /r main loop setup
34000 READ_GLMSTR: ! r: main loop
34020   if s1=2 then 
34040     gosub SELECT_ACCOUNT
34060   else
34080     do
34100       read #h_glmstr,using F_GLMSTR: n$,d$,bb,cb,mat bp eof TOTALS
34110       ! m2GlmCbAmtPos=87=current balance
34120       F_GLMSTR: form pos 1,c 12,c 50,pos m1GlmBbAmtPos,pd 6.2,pos m2GlmCbAmtPos,pd 6.2,pos 171,13*pd 6.2
34140       bb=bp(nap)
34150 !     pause ! 
34160     loop while s1=3 and n$<n1$
34180     if s1=3 and n$>n2$ then goto TOTALS
34200   end if
34220   if costCenterFilter><0 and val(n$(1:3))><costCenterFilter then goto TOTALS
34240 AfterReadGlmstr: !
34260   dno=val(n$(1:3))
34280   ano=val(n$(4:9))
34300   sno=val(n$(10:12))
34320   if (periodToPrint=0 or periodToPrint=1) and (dno>val(lastCapitalAccount$(1:3)) or ano>val(lastCapitalAccount$(4:9))) then  ! added the dno logic on 2/4/2017
34322     bb=0
34324 ! else
34326 !   bb=cb-activity  ! added the activity logic on 2/7/2017, before it was just bb
34328 !   activity=0
34329 !   pr str$(dno)&'-'&str$(ano)&'-'&str$(sno) : pause
34330   end if
34340   if petro_opt$='True' then 
34360     pr #255,using L1380: dno,ano,sno,d$,bb,cb
34380     L1380: form pos 1,pic(zzz),x 1,pic(------),x 1,pic(---),x 2,c 50,pos 80,pic(--,---,--z.## cr),pos 110,pic(zz,zzz,zzz.## cr)
34400   else 
34420     pr #255,using L1390: dno,ano,sno,d$,bb
34440     L1390: form pos 1,pic(zzz),x 1,pic(------),x 1,pic(---),x 2,c 50,pos 80,pic(--,---,--z.## cr)
34460   end if 
34480   restore #h_actrans,key>=n$&cnvrt$("N 2",periodToPrint)&"      ": nokey END_OF_TRANS
34500   t9=0
34520   do
34540     gosub READ_TR
34560     if t9=9 then goto END_OF_TRANS
34580     gosub PRINT_A_TRANS
34600   loop
34620 ! ______________________________________________________________________
34640 END_OF_TRANS: ! 
34660   gosub PRINT_CB_OR_SUMTR
34680 goto READ_GLMSTR ! /r
38000 TOTALS: ! r: EOF ON MASTER FILE
38020   pr #255: ""
38040   pr #255,using L1100: "Trial Balance Proof Totals",begbal,trtotal,curbal
38060   L1100: form pos 1,cr 78,pos 80,pic(zz,zzz,zzz.## cr),pic(z,zzz,zzz.## cr),pic(z,zzz,zzz.## cr)
38080   close #h_glmstr: 
38100   close #h_actrans: 
38120   fncloseprn
38140   goto XIT ! /r
39000 XIT: fnxit
41000 PGOF: ! r:
41020   pr #255: newpage
41040   gosub HDR
41060 continue ! /r
42000 HDR: ! r:
42020   pr #255,using fHeader1: env$('cnam'),date$('mm/dd/yy')
42040   pr #255,using fHeader1: 'Accumulated Trial Balance', time$
42060   pr #255,using fHeader1: fnpedat$,"Page "&str$(p1+=1)
42080   fHeader1: form pos 21,cc 80,cr 21
42100   pr #255: ""
42120   pr #255: "      Account";
42140   pr #255: tab(70);"Reference";tab(84);"Beginning";tab(99);"Current";
42160   pr #255: tab(115);"Ending"
42180   pr #255,using fHeader2: "Number","Account Name/Transaction Description","Date  Source","Number","Balance","Activity","Balance"
42200   fHeader2: form pos 6,c 6,pos 17,c 36,pos 54,c 13,pos 71,c 6,pos 85,c 7,pos 99,c 8,pos 115,c 7
42220   pr #255,using fHeader3: "__________","____________________________________","____","______","___________","_________","__________","_________"
42240   fHeader3: form pos 4,c 10,pos 17,c 36,pos 54,c 4,pos 60,c 6,pos 69,c 11,pos 84,c 9,pos 98,c 10,pos 114,c 10
42260 return ! /r
46000 READ_TR: ! r:
46020   read #h_actrans,using F_ACTRANS: t$,tr(4),tr(5),tr(6),tr(7),tr$,td$,pcde eof ReadTrFinisT9
46040   F_ACTRANS: form pos 1,c 12,n 6,pd 6.2,n 2,n 2,c 12,c 30,n 2
46060   if startday>0 and days(tr(4),'mmddyy')<startday then goto READ_TR
46080   if endday>0 and days(tr(4),'mmddyy')>endday then goto READ_TR
46100   if t$><n$ then goto ReadTrFinisT9
46120   if periodToPrint<>0 and periodToPrint><pcde then 
46140     goto ReadTrFinisT9
46160   end if
46180   if tr(5)=0 then goto READ_TR
46200   if tr(6)><0 then goto ReadTrXit else tr(6)=9
46220   goto ReadTrXit
46240   ReadTrFinisT9: ! r:
46260     t9=9
46280   goto ReadTrXit ! /r 
46300   ReadTrXit: !
46320 return ! /r
48000 PRINT_A_TRANS: ! r:
48020   x$=a$(tr(6))
48040   if val(cogl$(1)(4:9))=0 or val(cogl$(2)(4:9))=0 then goto PRINT_TRANS
48060   if t$>=cogl$(1) and t$<=cogl$(2) then 
48080       if tr(5)>0 then goto PRINT_TRANS
48100       u0=u0+tr(5)
48120       trtotal=trtotal+tr(5)
48140       u$=t$
48160       goto L1630
48180   end if
48200   if tr$="999999999999" then tr$=" "
48220   PRINT_TRANS: ! 
48240   ! 
48260   pr #255,using L1610: td$,tr(4),x$,lpad$(rtrm$(tr$),12),tr(5)
48280   L1610: form pos 21,c 30,pos 52,pic(zz/zz/zz),pos 62,c 3,pos 67,c 12,pos 95,pic(zz,zzz,zzz.## cr)
48300   trtotal=trtotal+tr(5)
48320   u$=t$
48340   L1630: ! 
48360 return ! /r
52000 PRINT_CB_OR_SUMTR: ! r:
52020   if u0 and u$=>cogl$(1) and u$<=cogl$(2) then 
52060     pr #255,using L1690: "Summary Transaction",u0
52080     L1690: form pos 21,c 30,pos 95,pic(zz,zzz,zz#.## cr)
52100     u0=0
52120   end if
52140   if petro_opt$='False' then 
52160     pr #255,using 'form pos 110,pic(zz,zzz,zzz.## cr)': cb
52180   end if 
52200   curbal=curbal+cb
52220   begbal=begbal+bb
52240 return ! /r
56000 ! <Updateable Region: ERTN>
56020 ERTN: fnerror(program$,err,line,act$,"xit")
56040   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
56060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
56080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
56100 ERTN_EXEC_ACT: execute act$ : goto ERTN
56120 ! /region
58000 SELECT_ACCOUNT: ! r:
58020   fnTos(sn$="Acglactb3")
58040   mylen=38: mypos=mylen+3 : right=1
58060   fnLbl(1,1,'General ledger # to print:',mylen,right,0,0)
58080   fnqgl(1,mypos,0,2)
58100   resp$(1)=""
58120   fnCmdSet(2)
58140   fnAcs(sn$,0,mat resp$,ckey)
58160   if ckey=5 then goto TOTALS
58180   n$=fnagl$(resp$(1))
58200   read #h_glmstr,using F_GLMSTR,key=n$: n$,d$,bb,cb,mat bp nokey SELECT_ACCOUNT
58220   bb=bp(nap)
58240 return ! /r
