12000 ! formerly S:\acsGL\AcGLBalC
12020 ! Comparative Balance Sheet
14000 ! r: setup library, on error, dims, fntop
14020   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnpglen,fnerror,fnprocess,fnpedat$,fnps,fnpriorcd,fnfscode,fnUseDeptNo,fnactpd,fnglfs,fntos,fnlbl,fntxt,fncmdkey,fnacs,fnactpd$
14040   on error goto ERTN
14060 ! 
14080   dim fl1$*256,cogl$(3)*12,accum(9,2),bp(13),by(13)
14100   dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*56
14120 ! ______________________________________________________________________
14140   fntop(program$)
14160   if fnglfs=5 then goto XIT ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Pprior,period to print)
14180   actpd$=fnactpd$ 
14200   actpd=fnactpd 
14220   ! fnfscode 
14240   ! fnpriorcd
14260   ! if fnglfs=5 then goto XIT ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Pprior,period to print)
14280   ! fnfscode
14300   ! fnpriorcd
14320   ! pr newpage
14340   if fnps=2 then 
14360     mp1=66 
14380     fl1$="Name="&env$('Q')&"\GLmstr\AcGLFnSc.h"&env$('cno')&"," 
14400     fl1$=fl1$&"KFName="&env$('Q')&"\GLmstr\FnScIndx.h"&env$('cno')&",Shr" 
14420   else 
14440     mp1=63 
14460     fl1$="Name="&env$('Q')&"\GLmstr\ACGLFNSB.h"&env$('cno')&"," 
14480     fl1$=fl1$&"KFName="&env$('Q')&"\GLmstr\FnSBIndx.h"&env$('cno')&",Shr"
14500   end if
14520   ! if actpd>0 and actpd<13 then goto L230
14540   ! pr newpage
14560   ! pr f "10,2,C 78,N": "THIS PROGRAM CANNOT PROCESS WITHOUT THE NUMBER OF THE ACCOUNTING MONTH END"
14580   ! pr f "12,2,c 78,n": "USE OPTION 1 ON THE CURRENT PERIOD PROCESSING MENU TO ENTER THIS INFORMATION"
14600   ! input fields "23,2,c 1,e,n": pause$
14620   ! goto XIT
14640   ! L230: 
14660   open #1: fl1$,internal,input,keyed 
14680   ! /r
20000   ! r: ask cost center
20020   if fnprocess=1 or fnUseDeptNo=0 then goto L320
20040   fntos(sn$="Acglbalc") 
20060   mylen=30: mypos=mylen+3 : right=1
20080   fnlbl(1,1,"Cost Center or Department #:",mylen,right)
20100   fntxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 ) 
20120   resp$(1)=""
20140   fnlbl(2,1,"(Blank for all Departments)",mylen,right)
20160   fncmdkey("&Next",1,1,0,"Prints the financial statement.")
20180   fncmdkey("&Cancel",5,0,1,"Returns to menu without posting.")
20200   fnacs(sn$,0,mat resp$,ckey)
20220   if ckey=5 then goto XIT
20240   L320: !
20260   costcntr=val(resp$(1))
20280   ! /r
22000   ! r: open glmstr with new fsindex
22020   if fnps=2 then ! secondary
22040     execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&" "&env$('temp')&"\fsindex.H"&env$('cno')&" 66 3 Replace DupKeys -N"
22060   else
22080     execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&" "&env$('temp')&"\fsindex.H"&env$('cno')&" 63 3 Replace DupKeys -N"
22100   end if
22120   open #3: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&",KFName="&env$('temp')&'\'&"fsindex.h"&env$('cno')&",Shr",internal,input,keyed 
22140   ! /r
23020   fnopenprn 
23040   report$=env$('program_caption')
24000 MainRead: ! r: main loop (on financial statement design)
24020   read #1,using L470: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L1660
24040   L470: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
24060   if ltrm$(r$)="" or ltrm$(r$)="0" then goto MainRead
24080   if costcntr and costcntr><fc then goto MainRead
24100   if te$="S" or te$="F" then goto L500
24120   if heading=0 and te$><"R" then gosub PrHeading
24140   L500: !
24160 on pos ("RFHDTSPE",te$,1) goto TypeRandS,TypeF,TypeH,TypeDandE,TypeTandP,TypeRandS,TypeTandP,TypeDandE none MainRead
24180 ! /r
26000 TypeH: ! r:
26020   pr #255,using L520: d$
26040   L520: form pos sp,c 50,skip 1
26060   gosub L1180
26080   gosub L1120
26100 goto MainRead ! /r
28000 TypeDandE: ! r:
28020   if notrans=1 then goto L710
28040   if br>=val(r$) and val(r$)><0 then goto L650
28060   ReadGLmstr: ! read general ledger master file for amounts
28080   read #3,using 'Form POS MP1,PD 3,POS 87,27*PD 6.2': br,cb,mat by,mat bp eof L700
28100   if br=0 then goto ReadGLmstr
28120   if fnfscode=0 or (fnfscode=actpd and fnpriorcd=1) then goto L650
28140   if fnfscode<1 or fnfscode>13 then let fnfscode(1)
28160   if fnpriorcd=1 then cb=by(fnfscode)
28180   L650: !
28200   if br=val(r$) then 
28220     total=total+cb 
28240     total2+=bp(fnfscode)
28260     goto ReadGLmstr
28280   end if
28300   if br<val(r$) then goto ReadGLmstr
28320   if br>val(r$) then goto L710
28340   L700: !
28360   notrans=1
28380   L710: !
28400   if te$="E" then total=-accum(ap,1) : total2=-accum(ap,2)
28420   for j=1 to 9
28440     if ac(j)<>9 then 
28460       accum(j,1)=accum(j,1)+total : accum(j,2)=accum(j,2)+total2
28480     end if
28500   next j
28520   if rs=1 then total=-total : total2=-total2
28540   if ds=1 then dollar$="$" else dollar$=" "
28560   dollar=24+14*bc
28580   if total><0 or total2><0 then goto L800
28600   if ls+ul+ds+ic>0 then goto L800 else goto MainRead
28620   L800: !
28640   sp2=dollar-sp-1
28660   if ul=1 then pr #255,using L816: d$(1:sp2),dollar$,"{\ul ",total,"}",dollar$,"{\ul ",total2,"}" pageoflow PgOf : goto L830
28680   pr #255,using L820: d$(1:sp2),dollar$,total,dollar$,total2 pageoflow PgOf
28700   L816: form pos sp,c sp2,pos dollar,c 1,c 5,pic(--,---,---.##),c 1,x 28,c 1,c 5,pic(--,---,---.##),c 1,skip 1
28720   L820: form pos sp,c sp2,pos dollar,c 1,pic(--,---,---.##),x 28,c 1,pic(--,---,---.##),skip 1
28740   L830: !
28760   total=0
28780   total2=0
28800   gosub L1120
28820   if ul=1 then goto L870
28840   gosub L1370
28860   L870: !
28880   gosub L1180
28900 goto MainRead ! /r
32000 TypeTandP: ! r:
32020   if ap=0 then ap=1
32040   if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
32060   if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
32080   if ds=1 then dollar$="$" else dollar$=" "
32100   dollar=24+14*bc
32120   sp2=dollar-sp-1
32140   if ul=1 then pr #255,using L816: d$(1:sp2),dollar$,"{\ul ",accum1,"}",dollar$,"{\ul ",accum2,"}" pageoflow PgOf : goto L960
32160   pr #255,using L820: d$(1:sp2),dollar$,accum1,dollar$,accum2 pageoflow PgOf
32180   L960: !
32200   gosub L1120
32220   if ul=1 then goto L980
32240   gosub L1370
32260   L980: !
32280   gosub L1180
32300   if te$><"P" then goto L1010
32320   for j=1 to 9 
32340     accum(j,1)=accum(j,1)-accum(ap,1) 
32360     accum(j,2)=accum(j,2)-accum(ap,2) 
32380   next j
32400   L1010: !
32420 goto MainRead ! /r
34000 TypeRandS: ! r:
34020   if te$="R" then 
34040     report$=d$ 
34060   else if te$="S" then 
34080     secondr$=d$
34100   end if
34120   gosub L1180
34140   goto MainRead
34160   TypeF: !
34180   if foot1=1 then 
34200     foot$=rtrm$(foot$)&d$ 
34220   else
34240     tabnote=sp : foot1=1 : foot$=d$
34260   end if
34280 goto MainRead ! /r
38000 L1120: ! r:
38020   for j=1 to 9
38040     if ac(j)=0 or ac(j)=9 then goto L1150
38060     accum(j,1)=0 : accum(j,2)=0
38080     L1150: !
38100   next j
38120 return ! /r
42000 L1180: ! r: maybe skip some lines if ls>0 - if ls=99 then do the newpage thing 
42020   if ls<>0 then 
42040     if ls=99 then 
42060       gosub PrNewPageThing
42080     else
42100       pr #255,using fSkipLs: " "
42120       fSkipLs: form pos 1,c 1,skip ls
42140     end if
42160   end if
42180 return ! /r
44000 PrNewPageThing: ! ! r: newpage thing.  pr footer on page and if eofcode<>1 then pr newpage and heading
44020   fnpglen(pglen)
44040   sk=pglen-krec(255): fl=len(rtrm$(foot$))
44060   if trim$(foot$)<>'' then pr #255,using L1280: rtrm$(foot$)
44080   L1280: form skip sk,pos tabnote,c fl,skip 1
44100   if eofcode<>1 then 
44120     pr #255: newpage
44140     gosub PrHeading
44160   end if
44180   L1320: !
44200 return ! /r
48000 PgOf: ! r:
48020   gosub PrNewPageThing
48040 continue ! /r
52000 L1370: ! r:
52020   if ul=0 then goto L1480
52040   underlin=24+14*bc
52060   if ul=1 then goto L1450
52080   underlin$="==============                            =============="
52100   pr #255,using L1420: underlin$
52120   L1420: form pos underlin,c 56,skip 1
52140   goto L1480
52160   ! 
52180   L1450: !
52200   underlin$="______________                            ______________"
52220   pr #255,using L1470: underlin$
52240   L1470: form pos underlin,c 56,skip 1
52260   L1480: !
52280   L1490: form skip 1,c 1,skip 1
52300 return ! /r
54000 PrHeading: ! r: heading
54020   heading=1
54040   pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
54060   pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
54080   if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
54100   pr #255: "\qc  {\f181 \fs16 \b "&trim$(fnpedat$)&"}"
54120   pr #255: '' ! "\ql "  moved down to the beginning of another line 
54140   pr #255: ""
54160   pr #255: "\ql {\f181                                                                                                                             Current Year                                                                    Prior Year }"
54180   pr #255,using L1630: "__________________________________________"," _________________________________________"
54200   L1630: form pos 38,cc 42,x 1,cc 42,skip 1
54220 return ! /r
58000 L1660: ! r:
58020   eofcode=1
58040   gosub PrNewPageThing
58060   fnfscode(actpd)
58080   fnpriorcd(1)
58100   fncloseprn
58120 goto XIT ! /r
58140 XIT: fnxit
62000 ! <Updateable Region: ERTN>
62020 ERTN: fnerror(program$,err,line,act$,"xit")
62040   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
62060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
62080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
62100 ERTN_EXEC_ACT: execute act$ : goto ERTN
62120 ! /region
