00010 ! formerly S:\acsGL\glSchPrt
00020 ! pr schedules
00030 !
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnerror,fncch$,fnpedat$,fnfscode,fnpriorcd,fnprocess,fnGlAskFormatPriorCdPeriod,fnTos,fnChk,fnAcs,fnCmdKey,fnLbl,fnpglen,fnactpd,fngethandle
00050   on error goto Ertn
00060 !
00070   dim dollar$*1,k$*3,by(13),bp(13),byt(13)
00080   dim gl2$*12,d2$*50,by2(13),bp2(13)
00090   dim sn$*78,ft$*78,gl$(80)*12,prtsch(0),d$*50,dol$*80,scheduleno(50)
00100   dim text$*45,resp$(45)*50
00110 !
00120   fntop(program$)
00124   pedat=fnactpd
00125   fnfscode
00126   fnpriorcd
00130   if fnGlAskFormatPriorCdPeriod=5 then goto XIT
00135   fnfscode
00136   fnpriorcd
00150   open #20: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input,relative: read #20,using "Form pos 296,N 2",rec=1: lmu : close #20: 
00170   open #hAcGlSchs1:=1: "Name=[Q]\GLmstr\ACGLSCHS.h[cno],KFName=[Q]\GLmstr\schindex.h[cno],Shr",internal,input,keyed ioerr DONE
00180   open #3: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",internal,input,keyed 
24000   if fnprocess=1 then 
24020     prtall=1 
24040   else
24060     if ~fn_selectSchedules(mat prtsch) then goto XIT
24080   end if
24100   fnopenprn( 0,0,0,0) ! str$(sn)&" - "&trim$(sn$))
24120   for prtSchItem=1 to udim(mat prtsch)
24180     if prtall=1 or prtsch(prtSchItem)<>0 then 
24200       if prtall=1 then
24220         read #hAcGlSchs1,using L270: sn,sn$,ft$,dp,rs,cm eof DONE
24240         L270: form pos 1,n 3,2*c 78,3*n 1
24260       else
24280         k$=lpad$(str$(prtsch(prtSchItem)),3) 
24300         read #hAcGlSchs1,using L270,key=k$: sn,sn$,ft$,dp,rs,cm nokey NextSchedule
24340       end if
24350       ! pr 'SN=';sn : pause
24360       if dp=1 then dollar$="$" else dollar$=" "
24380       gosub PrintHeadings
24400       open #hSchedule:=fngethandle: "Name=[Q]\GLmstr\schedule"&str$(sn)&".H[cno],KFName=[Q]\GLmstr\schedule_idx"&str$(sn)&".H[cno],Shr",internal,outIn,keyed 
26000       do 
26020         read #hSchedule,using "form pos 1,c 12": gl$ eof EoSchedule
26040         ! pr '"'&gl$&'"' : pause
26060         if gl$="  0     0  0" then goto NextGLinSchedule
26080         if j1><51 then goto L380
26100         gosub PrintPageFooter
26120         gosub PrintHeadings
28000         L380: read #3,using L450,key=gl$: d$,bb,cb,mat by,mat bp nokey NextGLinSchedule
28020         if cno<>99 then goto L450
28040         L400: read #3,using L410: gl2$,d2$,bb2,cb2,mat by2,mat bp2 eof L450
28060         L410: form pos 1,c 12,pos 13,c 50,pos 81,41*pd 6.2
28080         if gl2$=gl$(j) then goto L430 else goto L450
28100         L430: bb+=bb2 : cb+=cb2 : mat by=by+by2 : mat bp=bp+bp2
28120         goto L400
28140         L450: form pos 13,c 50,pos 81,41*pd 6.2
28160         if fnfscode=0 or (fnfscode=pedat and fnpriorcd=1) then goto L530 ! CURRENT OR PRIOR
28180         if fnfscode<0 or fnfscode>12 then let fnfscode=1
28200         if fnpriorcd=1 then cb=by(fnfscode) else cb=bp(fnfscode)
28220         if fnpriorcd=2 then goto L520
28240         if fnfscode>1 then bb=by(fnfscode-1) else bb=0
28260         goto L530
28280         L520: if fnfscode>1 then bb=bp(fnfscode-1) else bb=0
28300         L530: curmo=cb-bb
28320         if rs=1 then cb=-cb
28340         if rs=1 then curmo=-curmo
28360         if rs><1 then goto L580
28380         for rv=1 to 13 : by(rv)=-by(rv) : next rv
28400         L580: ! 
32000         if cm=1 then
32020           pr #255,using L660: d$,dollar$,curmo,dollar$,cb pageoflow PGOF
32040           j1=j1+1
32060           dollar$=" "
32080           L660: form pos 1,c 50,pos 51,c 1,pic(--,---,---.##),pos 67,c 1,pic(--,---,---.##)
32100         else if cm=2 then
32120           if lmu<2 or lmu>13 then goto L700
32140           for l=lmu to 2 step -1 : by(l)=by(l)-by(l-1) : next l
32160           L700: !
32180           if dp=1 then 
32200             dol$="FORM POS 1,C 32,14*PIC(---,---,--$.##)" 
32220           else 
32240             dol$="FORM POS 1,C 32,14*PIC(---,---,---.##)"
32260           end if
32280           pr #255,using dol$: d$(1:30),mat by,cb pageoflow PGOF
32300           mat byt=byt+by
32320         else if cm=3 then
32340           if dp=1 then 
32360             dol$="FORM POS 1,C 42,13*PIC(---,---,--$.##)" 
32380           else 
32400             dol$="FORM POS 1,C 42,13*PIC(---,---,---.##)"
32420           end if
32440           pr #255,using dol$: d$(1:40),mat by pageoflow PGOF
32460           mat byt=byt+by
32480         else
32500           pr #255,using L610: d$,dollar$,cb pageoflow PGOF
32520           L610: form pos 1,c 50,pos 67,c 1,pic(--,---,---.##)
32540           dollar$=" "
32560         end if
32580         gosub AccumulateTotals
32600         NextGLinSchedule: !
32620       loop
34000       EoSchedule: !
34020       j1=0
34040       gosub PrintTotals
34060       gosub PrintPageFooter
34080     end if
34800     NextSchedule: !
34980   next prtSchItem
34990   goto DONE
38000 PrintTotals: ! r: 
38020   if dp=1 then dollar$="$" else dollar$=" "
38040   if cm=0 then
38060     pr #255,using L610: "    Total",dollar$,ytdtot
38080     pr #255,using 'form pos 67,c 14': "=============="
38100   else if cm=1 then
38120     pr #255,using L920: "______________","______________"
38140     L920: form pos 51,c 14,pos 67,c 14
38160     pr #255,using L660: "    Total",dollar$,cmtot,dollar$,ytdtot
38180     pr #255,using L920: "==============","=============="
38200   else if cm=2 then
38220     dol$="FORM POS 1,C 32,14*'  ------------'"
38240     pr #255,using dol$: ""
38260     if dp=1 then 
38280       dol$="FORM POS 1,C 32,14*PIC(---,---,--$.##)" 
38300     else
38320       dol$="FORM POS 1,C 32,14*PIC(---,---,---.##)"
38340     end if
38360     pr #255,using dol$: "  Totals",mat byt,ytdtot pageoflow PGOF
38380     dol$="FORM POS 1,C 32,14*'  ============'"
38400     pr #255,using dol$: ""
38420   else if cm=3 then
38440     dol$="FORM POS 1,C 42,13*'  ------------'"
38460     pr #255,using dol$: ""
38480     if dp=1 then 
38500       dol$="FORM POS 1,C 42,13*PIC(---,---,--$.##)" 
38520     else 
38540       dol$="FORM POS 1,C 42,13*PIC(---,---,---.##)"
38560     end if
38580     pr #255,using dol$: "  Totals",mat byt pageoflow PGOF
38600     dol$="FORM POS 1,C 42,13*'  ============'"
38620     pr #255,using dol$: ""
38640   else
38660     pr #255,using 'form pos 67,c 14': "______________"
38680   end if
38700   cmtot=0
38720   ytdtot=0
38740   mat byt=(0)
38760 return ! /r
42000 PrintPageFooter: ! r:
42020   fnpglen(pglen)
42040   sk=pglen-krec(255): fl=len(rtrm$(ft$))
42060   pr #255,using L1190: rtrm$(ft$)
42080   L1190: form skip sk,pos tabnote,c fl,skip 1
42100   if eofcode<>1 then 
42120     pr #255: newpage
42140   end if
42160 return ! /r
44000 PGOF: ! r:
44020   gosub PrintPageFooter
44040   gosub PrintHeadings
44060 continue ! /r
46000 PrintHeadings: ! r: 
46020   pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
46040   pr #255: "\qc  {\f181 \fs22 \b "&rtrm$("Schedule "&str$(sn))&" - "&trim$(sn$)&"}"
46060   pr #255: "\qc  {\f181 \fs16 \b "&rtrm$(fnpedat$)&"}"
46080   pr #255: "\ql "
46100   pr #255: ""
46120   if cm=1 then
46140     pr #255,using L1380: lpad$(rtrm$(fncch$),20),"Year To Date"
46160     L1380: form pos 45,c 20,pos 69,c 12,skip 2
46180   else if cm=2 then
46200     pr #255: "Description                           Period 1      Period 2      Period 3      Period 4      Period 5      Period 6      Period 7      Period 8      Period 9     Period 10     Period 11     Period 12     Period 13  Year To Date"
46220     pr #255: "______________________________    ____________  ____________  ____________  ____________  ____________  ____________  ____________  ____________  ____________  ____________  ____________  ____________  ____________  ____________"
46240   else if cm=3 then
46260     pr #255: "Description                                     Period 1      Period 2      Period 3      Period 4      Period 5      Period 6      Period 7      Period 8      Period 9     Period 10     Period 11     Period 12     Period 13"
46280     pr #255: "________________________________________    ____________  ____________  ____________  ____________  ____________  ____________  ____________  ____________  ____________  ____________  ____________  ____________  ____________"
46300   end if
46320 return ! /r
48000 DONE: ! r:
48020   fnfscode(pedat)
48040   fnpriorcd(1)
48060   fncloseprn
48080 goto XIT ! /r
52000 AccumulateTotals: ! r:
52020   ytdtot=ytdtot+cb
52040   if cm=1 then 
52060     cmtot+=curmo
52080   end if
52100 return ! /r
54000 XIT: fnxit
58000 def fn_selectSchedules(mat prtsch)
58020   selectSchedulesReturn=0
58040   restore #hAcGlSchs1,key>="   ": nokey SelectSchedulesXit
58060   ln=1 : totallisted=0
58080   fnTos(sn$="GLschprt") 
58100   fnLbl(1,15,"Select Schedules to Be Printed")
58120   do
58140     read #hAcGlSchs1,using L270: sn,sn$,ft$,dp,rs,cm eof L1720
58160     ln=ln+1
58180     text$=cnvrt$("pic(zzz)",sn)&"  "&sn$(1:40)
58200     fnChk(ln,1,text$,0)
58220     totallisted+=1
58240     scheduleno(totallisted)=val(text$(1:3))
58260     ! if ln>21 and 1>1 then goto L1720 ! quit if more than two columns
58280     ! if ln>21 then ln=1  : colpos+=52
58300   loop
58320   L1720: !
58340   if totallisted then
58360     fnCmdKey("&Next",1,1,0,"Allows you to enter transactions.")
58380   end if
58400   fnCmdKey("&Cancel",5,0,1,"Returns to menu without printing.")
58420   fnAcs(sn$,0,mat resp$,ckey)
58440   if ckey<>5 then 
58460     mat prtsch(totallisted)
58480     for j=1 to totallisted
58500       if resp$(j)="True" then 
58520         prtsch(j)=scheduleno(j)
58540       end if
58560     next j
58580     selectSchedulesReturn=totallisted
58600   end if
58620   SelectSchedulesXit: !
58640   fn_selectSchedules=selectSchedulesReturn
58660 fnend
64000 ! <Updateable Region: ERTN>
64020 ERTN: fnerror(program$,err,line,act$,"xit")
64040   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
64060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
64080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
64100 ERTN_EXEC_ACT: execute act$ : goto ERTN
64120 ! /region
64140 !
