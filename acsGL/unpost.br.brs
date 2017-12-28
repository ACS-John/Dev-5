00010 ! Replace S:\acsGL\Unpost
00020 ! Remove Transactions (for a date range)
00030 ! r: setup library and dims
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fndate_mmddyy_to_ccyymmdd, fnTos,fnLbl,fnCmdSet,fnTxt,fnAcs,fnChk,fngethandle,fnStatus,fnStatusPause,fnindex_it
00050   on error goto ERTN
00060 ! 
00070   dim k(10,8),p$*30,ta(2),cap$*128,t$*12
00080   dim cnam$*40,n(2),l$*12
00090 ! /r
00100   fntop(program$,cap$="Remove Entries")
00120   fncno(cno,cnam$)
18000 MENU1: ! r:
18020   fnTos(sn$='UnPost')
18040   lc=0 : mylen=47 : mypos=mylen+2
18060   fnLbl(lc+=1,1,"Starting Date to Remove:",mylen,1)
18080   fnTxt(lc,mypos,0,0,0,'ccyymmdd')
18100   resp$(1)="" ! STR$(fndate_mmddyy_to_ccyymmdd(BEGDAT))
18120   fnLbl(lc+=1,1,"Ending Date to Remove:",mylen,1)
18140   fnTxt(lc,mypos,0,0,0,'ccyymmdd')
18160   resp$(2)="" ! STR$(fndate_mmddyy_to_ccyymmdd(ENDDAT))
18170   lc+=1
18180   fnChk(lc+=1,50,'Process History instead of Current Transactions',1)
18200   resp$(3)="False"
18210   lc+=1
18220   fnChk(lc+=1,50,'Remove Duplicates Only',1)
18240   resp$(4)='False'
18260   fnCmdSet(2)
18280   fnAcs(sn$,0,mat resp$,ckey)
18300   if ckey=5 then goto XIT
18320   begdat=val(resp$(1))
18340   enddat=val(resp$(2))
18360   if resp$(3)='True' then code$='H' else code$='C'
18380   if resp$(4)='True' then del_dupe_only=1 else del_dupe_only=0
18400   if enddat<begdat or (enddat=0 and begdat=0) then pr bell; : goto MENU1
18420 ! /r
18440 ! r: get ready to run
24000   fnStatus('date range: '&str$(begdat)&' - '&str$(enddat))
24020   if del_dupe_only then let fnStatus('only deleting duplicate entries')
24040   open #1: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\GLIndex.h"&env$('cno')&",Shr",internal,outIn,keyed 
24060   if uprc$(code$)="H" then 
24080     fnStatus('Processing history instead of current transactions')
24100     if del_dupe_only then 
24120       fnindex_it(env$('Q')&"\GLmstr\AcTrans.h"&env$('cno'),env$('Q')&"\GLmstr\tmp70.h"&env$('cno'),"1,70")
24140     end if  ! del_dupe_only
24160     open #h_trans:=fngethandle: "Name="&env$('Q')&"\GLmstr\AcTrans.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\AcTrIdx.h"&env$('cno')&",Shr",internal,outIn,keyed  ! 3
24180     if del_dupe_only then 
24200       open #h_trans_dupe:=fngethandle: "Name="&env$('Q')&"\GLmstr\AcTrans.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\tmp70.h"&env$('cno')&",Shr",internal,input,keyed 
24220     end if  ! del_dupe_only
24240   else 
24260     fnStatus('Processing current transactions only')
24280     if del_dupe_only then 
24300       fnindex_it(env$('Q')&"\GLmstr\GLTrans.h"&env$('cno'),env$('Q')&"\GLmstr\tmp70.h"&env$('cno'),"1,70")
24320     end if  ! del_dupe_only
24340     open #h_trans=fngethandle: "Name="&env$('Q')&"\GLmstr\GLTrans.h"&env$('cno')&",Shr",internal,outIn,relative  ! 2
24360     if del_dupe_only then 
24380       open #h_trans_dupe:=fngethandle: "Name="&env$('Q')&"\GLmstr\GLTrans.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\tmp70.h"&env$('cno')&",Shr",internal,input,keyed 
24400     end if  ! del_dupe_only
24420   end if 
24500 ! /r
28000 READ_H_TRANS: ! r: main loop
28020   read #h_trans,using 'Form POS 1,C 12,N 6,PD 6.2,N 2,N 2,C 12,C 30': t$,s,k,mat n,l$,p$ eof EO_H_TRANS
28040   reread #h_trans,using 'Form POS 1,C 70': hd_key_one$
28060   if fndate_mmddyy_to_ccyymmdd(s)<begdat or fndate_mmddyy_to_ccyymmdd(s)>enddat then goto READ_H_TRANS
28080 ! if val(t$(1:3))=0 and val(t$(4:9))=0 and val(t$(10:12))=0 then goto READ_H_TRANS
28100   if t$(3:3)=" " then t$(3:3)="0"
28120   if t$(12:12)=" " then t$(12:12)="0"
28140   read #1,using 'Form POS 81,2*PD 6.2',key=t$: bb,cb nokey DEL_H_TRANS ! delete any transactions without a matching master record.
28160   cb=cb-k
28180   if uprc$(code$)="H" then bb=bb-k
28200   rewrite #1,using 'Form POS 81,2*PD 6.2',key=t$: bb,cb
28220 DEL_H_TRANS: ! 
28240 ! rec_to_delete=rec(h_trans)
28260 ! if trim$(l$)='4905' and t$='  6   507  1' then pause
28280   if ~del_dupe_only or fn_has_dupe(h_trans_dupe,rec(h_trans),'Form pos 1,C 70') then 
28300     fnStatus('deleting transaction: '&hd_key_one$)
28320     delete #h_trans: ioerr ignore
28340   end if  ! ~del_dupe_only or fn_has_dupe
28360   goto READ_H_TRANS
28380 ! _____________________________________________________________________
32000 EO_H_TRANS: ! /r
32020   fnStatus('Reassigning Transaction Addresses...') ! r:
32040   restore #1,key>="            ": eof ignore
32060   do 
32080     read #1,using 'Form POS 333,2*PD 3': mat ta eof L470
32100     rewrite #1,using 'Form POS 333,2*PD 3': 0,0
32120   loop 
32140 L470: ! 
32160   lr2=lrec(2)
32180   if uprc$(code$)<>"H" then rewrite #h_trans,using 'Form POS 71,PD 3',rec=1: lr2
32200   for j=1 to lr2
32220     read #h_trans,using 'Form POS 1,C 12,POS 71,PD 3',rec=j: k$,nta noRec L580
32240     if k$="  0     0  0" then goto L580
32260     read #1,using 'Form POS 333,2*PD 3',key=k$: mat ta nokey L580
32280     if ta(1)=0 then ta(1)=j
32300     if ta(2)>0 then 
32320       rewrite #h_trans,using 'Form POS 71,PD 3',rec=ta(2): j
32340     end if 
32360     ta(2)=j
32380     rewrite #1,using 'Form POS 333,2*PD 3',key=k$: mat ta
32400     if uprc$(code$)<>"H" then rewrite #h_trans,using 'Form POS 71,PD 3',rec=j: 0
32420 L580: ! 
32440   next j
32450 ! /r
32460   fnStatusPause
32480   goto XIT
34000 XIT: fnxit
36000 ! <Updateable Region: ERTN>
36020 ERTN: fnerror(program$,err,line,act$,"xit")
36040   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
36060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
36080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
36100 ERTN_EXEC_ACT: execute act$ : goto ERTN
36120 ! /region
38000 IGNORE: continue 
40000   def fn_has_dupe(h_trans_dupe,hd_rec,hd_form$)
40020 ! hd_key$ must be formatted properly and contain ENTIRE key not just part, h_trans_dupe must be keyed and it's record pointer will be changed
40040 ! hd_form$ - form for key only
40060     dim hd_key_one$*128,hd_key_two$*128
40080     hd_return=0
40100     hd_key_two$=''
40120 ! restore #h_trans_dupe:
40140 ! release #h_trans:
40160 ! read #h_trans_dupe,using hd_form$,rec=hd_rec,release: hd_key_one$ noRec HD_XIT
40180     read #h_trans_dupe,using hd_form$,key=hd_key_one$: hd_key_one$ noRec HD_XIT
40200     read #h_trans_dupe,using hd_form$: hd_key_two$ eof HD_EOF
40220 HD_EOF: ! 
40240     if hd_key_one$=hd_key_two$ then hd_return=1
40260 HD_XIT: ! 
40280     fn_has_dupe=hd_return
40300   fnend  ! fn_has_dupe
