00010 ! formerly S:\acsCL\cl1099
00030 ! r: setup library, fntop, dims, on error
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fndate_mmddyy_to_ccyymmdd,fngethandle,fn1099print,fn1099print_close,fnask_1099_info
00050   let fntop(program$,cap$="Print 1099 Forms")
00080   dim vn$*8,nam$*30,ss$*11,box(11),ad$(3)*30
00100   dim cap$*128,key$*19,tr$(5)*35
00060   on error goto ERTN
00290 ! /r
32000 ! r: body of program
32020   if fnask_1099_info(seltp,unused_type,minamt,beg_date,end_date) then 
32040     open #payee=fngethandle: "Name="&env$('Q')&"\CLmstr\PayMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\PayIdx2.h"&env$('cno')&",Shr",internal,input,keyed 
32060     open #trmstr2=fngethandle: "Name="&env$('Q')&"\CLmstr\TrMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\TrIdx2.h"&env$('cno')&",Shr",internal,input,keyed 
36000     do
36020       read #payee,using 'Form Pos 1,C 8,4*c 30,x 5,n 2,c 11',release: vn$,nam$,mat ad$,typ,ss$ eof FINIS
36040       gosub READ_TRANSACTIONS
36060       if typ<>0 and ytdp>minamt then 
36080         if seltp=0 or seltp=typ then 
36100           mat box=(0)
36120           if typ<1 or typ>8 then let typ=1
36140           let box(typ)=ytdp
36145 ! pr mat ad$ : pause
36160           fn1099print(vn$,nam$,mat ad$,ss$,mat box)
36180         end if
36200       end if
36220     loop
44000     FINIS: !
44020     close #payee: ioerr ignore
44040     close #trmstr2: ioerr ignore
44060     fn1099print_close  !  if lz1$="E" then close #5: else    gosub RELEASE_PRINT
44080   end if
44900 goto XIT ! /r
48000 XIT: let fnxit
50000 ! <Updateable Region: ERTN>
50020 ERTN: let fnerror(program$,err,line,act$,"xit")
50040   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
50060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
50080   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
50100 ERTN_EXEC_ACT: execute act$ : goto ERTN
50120 ! /region
54000 READ_TRANSACTIONS: ! r: passed trmstr2,vn$,beg_date,end_date    returns ytdp
54010   let ytdp=0
54020   let wbc=0: let wtt=1 ! all banks and only checks
54040   let key$=vn$&cnvrt$('pic(Z#)',wbc)&cnvrt$("pic(#)",wtt)&rpt$(chr$(0),8) 
54060   restore #trmstr2,key>=key$: nokey rtFinis 
54100   do
54120     read #trmstr2,using 'Form Pos 1,n 2,n 1,C 8,G 6,PD 10.2,C 8,C 35,N 1,N 6,N 1',release: bank_code,tcde,tr$(1),tr$(2),tr3,tr$(4),tr$(5),pcde,clr,scd eof rtFinis
54140     if trim$(vn$)=trim$(tr$(4)) then 
54160       tranDate=fndate_mmddyy_to_ccyymmdd(val(tr$(2)))
54180       if tranDate=>beg_date and tranDate<=end_date then
54200         let ytdp+=tr3
54220       end if
54240     end if
54260   loop while trim$(vn$)=trim$(tr$(4))
54280   rtFinis: !
54300 return ! /r