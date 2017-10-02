02000 ! formerly S:\acsUB\hhto
02010 ! -- Tranfer Data From Computer to Hand Held
02020   fn_setup
02160   let fntop(program$)
02280   ! 
02330   open #h_customer_i1:=1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed 
02340   open #h_customer_i5:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&env$('cno')&",Shr",internal,input,keyed 
02342   ! 
02368   goto SEL_ACT
08000 def fn_setup
08020   library 'S:\Core\Library': fnerror,fntos,fnlbl,fncomboa,fnacs,fncmbrt2,fnxit,fncmbact,fnbutton,fncustomer_search,fnfra,fncmdset,fntop,fncmdkey,fnmsgbox,fntxt,fngethandle,fnpause,fnopt,fnget_services,fnhand_held_device$,fncreg_read,fncreg_write,fnCopy,fnureg_read,fnAddOneC
08030   library 'S:\Core\Library': fnMeterAddressLocationID,fncsz
08040   on error goto ERTN
08060 ! ______________________________________________________________________
08080   dim f$(3)*12,ft$*20,resp$(2)*100,text$*50,cap$*128,e2$*30
08100   dim tip$*100
08120   dim z$*10,e$(4)*30,d(15),a(7),rm$*60,rm$(20)*60
08140   dim res$*41,m$(2)*80,rm$*1320
08160   dim notefile$*100,notedir$*100
08180   dim servicename$(10)*20,servicecode$(10)*2
08200   dim rt$*4,extra(23)
08240   let fn_constants_setup
08260   let fnget_services(mat servicename$, mat servicecode$)
08280   dim device$*20
08300   let device$=fnhand_held_device$
08320 fnend
08340 def fn_constants_setup
08360   dim drive$(22)*3
08380   let drive$(1)="E:\"
08400   let drive$(2)="F:\"
08420   let drive$(3)="G:\"
08440   let drive$(4)="H:\"
08460   let drive$(5)="I:\"
08480   let drive$(6)="J:\"
08500   let drive$(7)="K:\"
08520   let drive$(8)="L:\"
08540   let drive$(9)="M:\"
08560   let drive$(10)="N:\"
08580   let drive$(11)="O:\"
08600   let drive$(12)="P:\"
08620   let drive$(13)="Q:\"
08640   let drive$(14)="R:\"
08660   let drive$(15)="S:\"
08680   let drive$(16)="T:\"
08700   let drive$(17)="U:\"
08720   let drive$(18)="V:\"
08740   let drive$(19)="W:\"
08760   let drive$(20)="X:\"
08780   let drive$(21)="Y:\"
08800   let drive$(22)="Z:\"
08860   crlf$=chr$(13)&chr$(10)
08880 fnend  ! fn_constants_setup
10370 SEL_ACT: ! r:
10380   let fn_scr_selact
10390   if ckey=5 then goto XIT
10400   if workopen=0 then let fn_openwork ! open work files based on type of Hand Held
10410   if ckey=2 then 
10420     goto TRANSFER
10430   else if selection_method=1 then 
10440     goto SELECT_ALL
10450   else if selection_method=2 then 
10460     goto ASK_BOOK
10470   else if selection_method=3 then 
10480     goto RANGE
10490   else if selection_method=4 then 
10500     goto ASK_ACT
10510   end if  ! /r
10520 ! ______________________________________________________________________
10530 ASK_BOOK: ! r:
10535   let fntos(sn$="Ask_Book")
10540   if hbk<>0 then 
10545     let fnlbl(1,1,"Last Route Number Selected: "&str$(hbk))
10550     let myline=3
10555   else 
10560     let myline=1
10565   end if 
10570   let fnlbl(myline,1,"Route Number to Read:")
10575   let fncmbrt2(myline,22,0)
10580   let resp$(1)=""
10585   let fncmdkey("&Next",1,1,0,"Add the selected route" )
10590   let fncmdkey("&Finish",2,0,1,"Completed with all routes")
10595   let fncmdkey("&Cancel",5,0,0,"Don't sent to Hand Held")
10600   let fnacs(sn$,0,mat resp$, ckey)
10605   if resp$(1)="[All]" and ckey=1 then let selection_method=1 : goto SELECT_ALL ! if they select all on the route screen, handle same as pr all option from 1st menu
10610   bk1=val(resp$(1)) conv L850
10615   let resp$(1)=""
10620 L850: ! 
10625   if ckey=1 then 
10630     goto SELECT_ALL
10635   else if ckey=2 then 
10640     goto TRANSFER
10645   else if ckey=5 then 
10650     goto SEL_ACT
10655   else 
10660     goto SELECT_ALL
10665   end if 
10670 ! 
10675 ! /r
10750 SELECT_ALL: ! r:
10760   ! if env$('client')="Gilbertown" then goto GILBERTOWN
10770   if bk1=0 then bk1=1
10780   restore #h_customer_i5,key>=cnvrt$("pic(zz)",bk1)&"       ": nokey ASK_BOOK
10790 SELECT_ALL_READ: ! 
10800   if fn_customerRead=-54 then goto END1
10820   if selection_method=1 then goto L980
10830   if selection_method=2 and route=0 then goto SELECT_ALL_READ
10840   if selection_method=2 and bk1><route then goto END1
10850 L980: goto FINAL_BILLING_CODE ! /r
10860 ! ______________________________________________________________________
10990 END1: ! r:
11000   if device$='Itron FC300' then let fn_itron_close
11010 ! close #h_customer_i5: ioerr IGNORE ! was close #111: ioerr IGNORE
11020   if selection_method=1 then goto TRANSFER
11030   if selection_method=2 then let hbk=bk1 : goto ASK_BOOK ! /r
11040 ASK_ACT: ! r:
11050   let fntos(sn$="Ask_Act")
11060   if z$<>"" then 
11070     let fnlbl(1,1,"Last Account Selected: "&z$,40,2)
11080     let myline=3
11090   else 
11100     let myline=1
11110   end if 
11120   let fnlbl(myline,1,"Account:",15,1)
11130   let fncmbact(myline,16)
11140   let resp$(1)=z$
11150   let fncmdset(5)
11160   let fnacs(sn$,0,mat resp$,ckey)
11170   if ckey=6 then let fncustomer_search(resp$(1))
11180   if ckey=99 or ckey=5 or resp$(1)="          " then goto SEL_ACT
11190   let z$=lpad$(trim$(resp$(1)(1:10)), 10)
11200   if fn_customerRead(z$)=-4272 then goto ASK_ACT
11210   goto FINAL_BILLING_CODE
11220 ! /r
11270 FINAL_BILLING_CODE: ! r: doesn't seem to be very well named.
11280 ! if trim$(z$)="200710.01" or trim$(z$)="201650.00" then pr 'final=';final : pause
11290   if final><0 then goto NEXT_RECORD ! SKIP IF FINAL BILLED
11300   let fn_rmk1
11310   if sq1=0 then let sq1=1234 ! DEFALT SEQ=W,E,D,G
11320   let seq$=str$(sq1)
11330   if device$="Psion Workabout" then let fn_workabout : goto NEXT_RECORD
11340   if device$="Badger" then let fn_badger : goto NEXT_RECORD
11350   if device$="Boson" then let fn_boson : goto NEXT_RECORD
11360   if device$="Sensus" then let fn_sensus : goto NEXT_RECORD
11370   if device$="LapTop" then let fn_laptop : goto NEXT_RECORD
11380   if device$="Green Tree" then let fn_greentree : goto NEXT_RECORD
11390   if device$="Hersey" then let fn_hersey : goto NEXT_RECORD
11400   if device$="EZReader" then let fn_ezreader : goto NEXT_RECORD
11410   if device$="AMR" then let fn_amr : goto NEXT_RECORD
11420   if device$="Unitech HT630" then let fn_unitech_ht630 : goto NEXT_RECORD
11430   if device$="ACS Meter Reader" then let fn_acs_meter_reader : goto NEXT_RECORD
11440   if device$="Itron FC300" then let fn_itron : goto NEXT_RECORD
11442   if device$="Aclara" then let fn_aclara : goto NEXT_RECORD
11444   if device$="Master Meter" then let fn_masterMeter : goto NEXT_RECORD
11446   if device$="READy Water" then let fn_READy_Water : goto NEXT_RECORD
11450   goto SEL_ACT ! go back if Hand Held information is not available for their selection
11460 NEXT_RECORD: ! 
11470   on selection_method goto SELECT_ALL_READ,SELECT_ALL_READ,NEXT_REC_SM3,ASK_ACT none SELECT_ALL_READ
11472 ! /r
11480 def fn_workabout
11490   for j=1 to len(seq$)
11500     on val(seq$(j:j)) goto WORKABOUT_WATER,WORKABOUT_ELECTRIC,WORKABOUT_DEMAND,WORKABOUT_GAS none WORKABOUT_NEXT_SEQUENCE
11510     ! ___________________________
11520     FM_WORKABOUT: form pos 1,c 10,2*c 20,2*n 9,n 1,c 10,c 20
11530     ! ___________________________
11540     WORKABOUT_WATER: ! 
11550       if a(1)=0 then goto WORKABOUT_NEXT_SEQUENCE
11560       let m$=ltrm$(f$(1))(1:10)
11570       pr #h_out,using FM_WORKABOUT: z$,e$(2)(1:16)&" (W)",e$(1)(1:20),d(1),d(3),1,m$,ft$
11580     goto WORKABOUT_NEXT_SEQUENCE
11590     ! ___________________________
11600     WORKABOUT_ELECTRIC: ! 
11610       if a(3)=0 or trim$(servicename$(3))<>"Electric" then goto WORKABOUT_LAWNMETER
11620       let m$=ltrm$(f$(2))(1:10)
11630       pr #h_out,using FM_WORKABOUT: z$,e$(2)(1:16)&" (E)",e$(1)(1:20),d(5),d(7),3,m$,ft$
11640       WORKABOUT_LAWNMETER: ! 
11650       if a(3)=0 or trim$(servicename$(3))<>"Lawn Meter" then goto WORKABOUT_NEXT_SEQUENCE
11660       let m$=ltrm$(f$(2))(1:10)
11670       pr #h_out,using FM_WORKABOUT: z$,e$(2)(1:16)&" (L)",e$(1)(1:20),d(5),d(7),3,m$,ft$
11680     goto WORKABOUT_NEXT_SEQUENCE
11690     ! ___________________________
11700     WORKABOUT_DEMAND: ! 
11710     goto WORKABOUT_NEXT_SEQUENCE
11750     ! ___________________________
11760     WORKABOUT_GAS: ! 
11770       if a(4)=0 or trim$(servicename$(4))<>"Gas" then goto WORKABOUT_NEXT_SEQUENCE
11780       let m$=ltrm$(f$(3))(1:10)
11790       pr #h_out,using FM_WORKABOUT: z$,e$(2)(1:16)&" (G)",e$(1)(1:20),d(9),d(11),2,m$,ft$
11800     goto WORKABOUT_NEXT_SEQUENCE
11810     ! ___________________________
11820     WORKABOUT_NEXT_SEQUENCE: ! 
11830   next j
11840 fnend 
11850 def fn_laptop
11860   ! LAPTOPWATER: !
11870   if a(1)=0 or trim$(servicename$(1))<>"Water" then goto LAPTOPELECTRIC ! 
11880   write #h_out,using "form pos 1,c 10,c 30,c 30,c 1,4*n 9,c 12,c 20": z$,e$(2),e$(1),"W",watread,watusage,d(1),d(3),f$(1),ft$ : goto LAPTOP_XIT
11890   LAPTOPELECTRIC: ! 
11900   if a(3)=0 or trim$(servicename$(3))<>"Electric" then goto LAPTOPGAS
11910   write #h_out,using "form pos 1,c 10,c 30,c 30,c 1,4*n 9,c 12,c 20": z$,e$(2),e$(1),"E",elecread,elecusage,d(5),d(8),f$(2),ft$ : goto LAPTOP_XIT
11920   LAPTOPGAS: ! 
11930   if a(4)=0 or trim$(servicename$(4))<>"Gas" then goto LAPTOP_XIT
11940   write #h_out,using "form pos 1,c 10,c 30,c 30,c 1,4*n 9,c 12,c 20,n 3,n 7": z$,e$(2),e$(1),"G",gasread,gasusage,d(9),d(12),f$(3),ft$,route,sequence : goto LAPTOP_XIT
11950   LAPTOP_XIT: ! 
11960 fnend 
11970 def fn_badger
11980   for j=1 to len(seq$)
11990     on val(seq$(j:j)) goto BADGER_WATER,BADGER_ELECTRIC,BADGER_DEMAND,BADGER_GAS none BADGER_NEXT_SEQUENCE
12000     BADGER_WATER: ! 
12010     if a(1)=0 then goto BADGER_NEXT_SEQUENCE
12020     let m$=ltrm$(f$(1))(1:10)
12030     if env$('client')="Moweaqua" then let manual_or_dialog$=extra$(3)
12040     if env$('client')="Moweaqua" then let extra$(3)=f$(1) ! they have meter number in first water meter number and a code in the second number
12050     if env$('client')="Moweaqua" then let d(1)=d(1): let d(2)=d(2): let d(3)=d(3)
12060     if env$('client')="Sangamon" then let manual_or_dialog$=f$(1)(1:1)
12070     if env$('client')='Sangamon' then let z$=trim$(z$)
12080     let rt$=cnvrt$("pic(##)",extra(1))&"  "
12081     if env$('client')='Raymond' then let manual_or_dialog$="N"
12082     if env$('client')='Raymond' and trim$(extra$(7))='' then let extra$(7)='54'
12090     pr #h_out,using 'Form POS 1,C 8,2*C 20,C 9,C 4,C 1,C 1,C 2,C 2,C 9,C 1,3*PIC(#########),C 8,C 2,C 2,C 4,C 15,C 8,C 1,3*C 6,C 2,PIC(######),C 20,C 30,C 3,C 2,C 2,C 2,C 6,C 18,C 1': "",e$(2)(1:20),e$(1)(1:20),trim$(extra$(3))(1:9),"","A","","1 ","  ","        "," ",d(1)+(d(3)*2),d(1),0,"        ","  ","  ",rt$,z$,"        ",manual_or_dialog$(1:1)," "," "," ",extra$(7)(1:2),sequence," "," "," "," "," "," "," "," ","X"
12100     ! serial # can be extra$(3) rather than f$(1)
12110     ! replaced UPRC$(TRIM$(F$(1)))(1:1) with manual_or_dialog$
12120     goto BADGER_NEXT_SEQUENCE
12130     ! ___________________________
12140     BADGER_ELECTRIC: ! 
12150     if a(3)=0 or trim$(servicename$(3))<>"Electric" then goto BADGER_NEXT_SEQUENCE
12160     let m$=ltrm$(f$(2))(1:10)
12170     pr #h_out,using 'Form POS 1,C 8,2*C 20,C 9,C 4,C 1,C 1,C 2,C 2,C 9,C 1,3*PIC(#########),C 8,C 2,C 2,C 4,C 15,C 8,C 1,3*C 6,C 2,PIC(######),C 20,C 30,C 3,C 2,C 2,C 2,C 6,C 18,C 1': " ",e$(2)(1:20),e$(1)(1:20),trim$(extra$(3))(1:9)," ","A"," ","3 "," ",f$(2)(1:9)," ",d(5)+(d(7)*1.5),d(5),0," "," "," "," ",z$," ",uprc$(trim$(f1$))(1:1)," "," "," ",extra$(7)(1:2),sequence," "," "," "," "," "," "," "," ","X"
12180     L2010: form pos 1,c 8,2*c 20,c 9,c 4,c 1,c 1,c 2,c 2,c 9,c 1,3*pic(#########),c 8,c 2,c 2,c 4,c 15,c 8,c 1,3*c 6,c 2,pic(######),c 20,c 30,c 3,c 2,c 2,c 2,c 6,c 18,c 1
12190     goto BADGER_NEXT_SEQUENCE
12200     ! ___________________________
12210     BADGER_DEMAND: ! 
12220     goto BADGER_NEXT_SEQUENCE
12230     let m$=""
12240     pr #h_out,using L2010: " ",e$(2)(1:20),e$(1)(1:20),trim$(extra$(3))(1:9)," ","A"," ","4 "," ",f$(2)(1:9)," ",d(15)+(d(15)*.5),d(15)-(d(15)*.5),0," "," "," "," ",z$," ",manual_or_dialog$," "," "," ",extra$(7)(1:2),sequence," "," "," "," "," "," "," "," ","X"
12250     goto BADGER_NEXT_SEQUENCE
12260     ! ___________________________
12270     BADGER_GAS: ! 
12280     if a(4)=0 or trim$(servicename$(4))<>"Gas" then goto BADGER_NEXT_SEQUENCE
12290     let m$=ltrm$(f$(3))(1:10)
12300     pr #h_out,using L2010: " ",e$(2)(1:20),e$(1)(1:20),trim$(extra$(3))(1:9)," ","A"," ","2 "," ",f$(2)(1:9)," ",d(9)+(d(11)*1.5),d(9),0," "," "," "," ",z$," ","D"," "," "," ",extra$(7)(1:2),sequence," "," "," "," "," "," "," "," ","X"
12310     goto BADGER_NEXT_SEQUENCE
12320     ! ___________________________
12330     BADGER_NEXT_SEQUENCE: ! 
12340   next j
12350 fnend 
12870 def fn_sensus
12880   if (env$('client')="Oakland" or env$('client')="Lovington") and trim$(extra$(7))="1" then cd$="B" else cd$="M"
12890   L2520: form pos 1,c 10,2*c 20,2*n 9,n 1,c 10,c 1,n 9
12900   if final><0 then goto L2710
12910   c$=""
12920   ! cD$="" ! TYPE OF METER
12930   if a(1)>0 then c$="1"
12940   if a(3)=5 then c$=c$&"5": goto L2590
12950   if a(3)>0 then c$=c$&"3"
12960   L2590: if a(4)>0 then c$=c$&"4"
12970   goto L2610
12980   L2610: let j=0
12990   if rtrm$(f$)="" then let f$=z$
13000   L2630: let j=j+1
13010   if j>len(c$) then goto L2710
13020   on val(c$(j:j)) goto SENSUSWATER,L2630,SENSUSELECTRIC,SENSUSGAS,SENSUSDEMAND none L2630
13030   SENSUSWATER: pr #h_out,using L2520: z$,e$(2)(1:18)&"-W",e$(1)(1:20),d(1),d(3),1,extra$(3)(1:10),cd$: goto L2630
13040   SENSUSELECTRIC: if d(14)<>0 then let d(7)=d(7)/(d(14)*.01) ! COMPARE USAGE BEFORE MULTIPLIER
13050   pr #h_out,using L2520: z$,e$(2)(1:18)&"-E",e$(1)(1:20),d(5),d(7),3,extra$(3)(1:10),cd$ : goto L2630
13060   SENSUSGAS: goto L2630 ! pr #h_out,USING 470: Z$,E$(2)(1:18)&"-G",E$(1)(1:20),D(9),D(11),2 : GOTO 760
13070   SENSUSDEMAND: pr #h_out,using L2520: z$,e$(2)(1:18)&"-D",e$(1)(1:20),d(15),d(7),4,extra$(3)(1:9)&"D",cd$ : goto L2630
13080   L2710: ! 
13090 fnend 
13100 def fn_greentree
13110   cd$="M" ! if env$('client')="Gilbertown" and trim$(extra$(7))="1" then cd$="B" else cd$="M"
13120   if final><0 then goto L2920
13130   c$=""
13140   ! cD$="" ! TYPE OF METER
13150   if a(1)>0 then c$="1"
13160   if a(3)=5 then c$=c$&"5": goto L2820
13170   if a(3)>0 then c$=c$&"3"
13180   L2820: if a(4)>0 then c$=c$&"4"
13190   ! 
13200   goto L2850
13210   L2850: let j=0
13220   if rtrm$(f$)="" then let f$=z$
13230   let j=j+1
13240   if j>len(c$) then goto L2920
13250   on val(c$(j:j)) goto GREENTREEWATER none L2920 ! only water
13260   GREENTREEWATER: pr #h_out,using L2910: z$,e$(2)(1:18)&"-W",e$(1)(1:20),d(1),d(3),1,extra$(3)(1:10),cd$
13270   L2910: form pos 1,c 10,2*c 20,2*n 9,n 1,c 10,c 1
13280   L2920: ! 
13290 fnend 
13300 def fn_hersey
13310   cd$="M" ! if env$('client')="Gilbertown" and trim$(extra$(7))="1" then cd$="B" else cd$="M"
13320   if final><0 then goto L3140
13330   c$=""
13340   ! cD$="" ! TYPE OF METER
13350   if a(1)>0 then c$="1"
13360   if a(3)=5 then c$=c$&"5": goto L3020
13370   if a(3)>0 then c$=c$&"3"
13380   L3020: if a(4)>0 then c$=c$&"4"
13390   ! 
13400   ! 
13410   goto L3060
13420   L3060: let j=0
13430   if rtrm$(f$)="" then let f$=z$
13440   let j=j+1
13450   if j>len(c$) then goto L3140
13460   on val(c$(j:j)) goto HERSEYWATER none L3140 ! only water
13470   HERSEYWATER: ! 
13480   !  GreenCo #6 , Hot Rod version,  compatible with Easy Reader
13490   pr #h_out,using L3130: z$," "," ","W",e$(2)(1:25),e$(1)(1:21),f$(1),"V",d(1)+(d(3)*2),d(1)," "," "," "," ",z$," ",chr$(13)&chr$(10)
13500   L3130: form pos 1,c 10,c 4,c 6,c 1,c 25,c 21,c 20,c 1,n 10,n 10,c 100,c 2,c 1,c 5,c 12,c 52,pos 281,c 2
13510   L3140: ! 
13520 fnend 
13530 def fn_amr ! amr software solutions  ! same as ezreader, but specifically for Albany (who no longer uses ACS UB)
13540   if header=0 then 
13550     if alp$(1:1)<>"*" then 
13560       if bk1>0 then let route=bk1 else let route=1 ! if they selected all for route number, make route number =1 else use the actual route number
13570       pr #h_out,using "form pos 1,c 2,pic(##),pic(######),c 2": "R1",1,route,crlf$ : let header=1 ! create header record
13580     end if
13590   else 
13600     if final=0 then
13610       c$="W" ! read type code
13620       ! AMR Water
13630       pr #h_out,using L3230: "M1", lpad$(rtrm$(z$),20),f$(1)(1:10),extra(2),"W",d(1)+(d(3)*2),d(1)+(d(3)*.50),"    ","    ","    ",e$(1),e$(2)(1:20),d(1),extra(8),0,0,0,0,0,0,0,0,0,0,0,0,crlf$
13640       L3230: form pos 1,c 2,c 20,c 10,pic(######),c 4,2*pic(##########),3*c 4,c 40,c 20,pic(##########),n 4,pic(##),pic(#),2*pic(##########),2*pic(############),5*pic(##########),pic(########),c 2
13650     end if
13660   end if
13662 fnend 
13670 def fn_ezreader
13680   if final><0 then goto L3430
13690   c$=""
13700   ! cD$="" ! TYPE OF METER
13710   if a(1)>0 then c$="1"
13720   if a(3)>0 then c$=c$&"3"
13730   if a(4)>0 then c$=c$&"4"
13740   let j=0
13750   if rtrm$(f$)="" then let f$=z$
13760   let j=j+1
13770   if j>len(c$) then goto L3430
13780   on val(c$(j:j)) goto L3400 none L3430 ! only water
13790   L3400: ! EZReaderWater
13800   pr #h_out,using L3420: cnvrt$("pic(##)",route)&cnvrt$("pic(#######)",sequence),"  ","W",e$(2),e$(1),f$(1),extra$(3)(1:1),d(1)+(d(3)*2),d(1),0," "," "," ",z$," "," ","X",chr$(13)&chr$(10)
13810   L3420: form pos 1,c 12,c 2,c 1,c 66,c 64,c 14,c 1,2*pic(##########),pic(##),c 120,c 24,c 24,c 20,c 80,c 125,c 1,c 2
13820   L3430: ! 
13830 fnend 
14000 def fn_searchScreen(x$,&res$)
14020   let fncustomer_search(x$)
14040   if x$<>"" then 
14060     read #h_customer_i1,using "Form POS 1,C 10,x 30,c 30",key=x$: z$,e2$
14080     let res$=rpad$(trim$(z$),10)&" "&trim$(e2$)
14100   end if 
14120 fnend
14290 RANGE: ! r:
14310   let fntos(sn$:="Range")
14320   let fnfra(1,1,1,57,"Starting Account:")
14330   let fnfra(4,1,1,57,"Ending Account:")
14340   let fncmbact(1,1,0,1)
14350   let text$="Search"
14360   let fnbutton(1,48,text$,6,blank$,0,7,1)
14370   let resp$(1)=resp$(2)=""
14380   let fncmbact(1,1,0,2)
14390   let fnbutton(1,48,text$,7,blank$,0,7,2)
14400   let fncmdkey("&Finish",2,1,0,"Completed with all routes")
14410   let fncmdset(2)
14420   let fnacs(sn$,0,mat resp$,ckey)
14430   bk1$=lpad$(trim$(resp$(1)(1:10)), 10)
14440   bk2$=lpad$(trim$(resp$(2)(1:10)), 10)
14450   if ckey=2 then goto TRANSFER
14460   if ckey=99 or ckey=5 then mat resp$=(""): goto SEL_ACT
14470   if ckey=6 then 
14480     fn_searchScreen(x$,resp$(1))
14500     goto RANGE
14510   else if ckey=7 then 
14520     fn_searchScreen(x$,resp$(2))
14540     goto RANGE
14550   end if 
14560   mat resp$=("")
14570   ! read #h_customer_i1,using F_CUSTOMER,key=bk1$,release: z$,mat e$,mat a,final,mat d,mat f$,route,sequence,extra$(3),extra$(7),extra(1),alp$ eof RANGE ! get first and last route and sequence number to select
14580   ! read #h_customer_i1,using F_CUSTOMER,key=bk2$,release: z$,mat e$,mat a,final,mat d,mat f$,last_route,last_sequence,extra$(3),extra$(7),extra(1),alp$ eof RANGE
14582   read #h_customer_i1,using 'form pos 1741,n 2,n 7',key=bk1$,release: route,sequence ! get first and last route and sequence number to select
14584   read #h_customer_i1,using 'form pos 1741,n 2,n 7',key=bk2$,release: last_route,last_sequence
14590   restore #h_customer_i5,key=cnvrt$("pic(zz)",route)&cnvrt$("pic(zzzzzzz",sequence): 
14600 NEXT_REC_SM3: ! 
14610   if fn_customerRead=-54 then goto RANGE
14620 ! If (ROUTE=LAST_ROUTE AND SEQUENCE>LAST_SEQUENCE) OR ROUTE>LAST_ROUTE Then Goto RANGE
14630   if trim$(z$)<trim$(bk1$) or trim$(z$)>trim$(bk2$) then goto RANGE
14640   goto FINAL_BILLING_CODE ! /r
14650 ! 
14720 ! <Updateable Region: ERTN>
14730 ERTN: let fnerror(program$,err,line,act$,"xit")
14740   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
14750   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
14760   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
14770 ERTN_EXEC_ACT: execute act$ : goto ERTN
14780 ! /region
14810 def fn_rmk1
14820   let ft$="                    "
14830   ! read the footnote from the note file  (any note with * as first character
14840   let notedir$=env$('Q')&"\UBmstr\notes.h"&env$('cno')
14850   let notefile$=notedir$&"\"&trim$(z$)&".txt"
14860   if exists(notedir$)=0 then goto L4000
14870   open #20: "Name="&notefile$,display,input ioerr L4010
14880   L3960: linput #20: rm$ eof L4000
14890   if rm$(1:1)="*" then goto L3990
14900   goto L3960
14910   L3990: let ft$=rpad$(rm$(2:21),20)
14920   L4000: close #20: ioerr ignore
14930   L4010: ! 
14940 fnend 
14950 ! 
14960 def fn_openwork ! open work areas based on type of Hand Held
14962   dim out_filename$*256
14964   let fnureg_read('Hand Held To File',out_filename$)
14966   if out_filename$<>'' then 
14968     if device$='Itron FC300' then 
14970       let fn_itron_open
14972     else 
14974       open #h_out:=fngethandle: 'Name='&br_filename$(out_filename$)&',RecL=1024,Replace',display,output 
14976     end if 
14980   else if device$="Badger" and env$('client')="Sangamon" then 
14990     if ~exists("c:\progra~1\CONNECT3") then execute "MkDir c:\progra~1\CONNECT3 -n"
15000     open #h_out:=2: "Name=c:\progra~1\CONNECT3\CONNECT.IN3,RecL=256,Replace",display,output 
15010   else if device$="Sensus" and (env$('client')="Oakland" or env$('client')="Lovington") then 
15020     open #h_out:=2: "Name=c:\vol002\amrs\READINGS.DAT,RecL=80,Replace",display,output 
15030   else if device$="Green Tree" then 
15040     open #h_out:=2: "Name=c:\READINGS.DAT,RecL=80,Replace",display,output 
15050   else if device$="Badger" then 
15060     if ~exists('C:\connect') then execute "sy md C:\connect -n"
15070     open #h_out:=fngethandle: "Name=C:\CONNECT\CONNECT.IN3,RecL=256,Replace",display,output 
15080   else if device$="Boson" then 
15090     open #h_out:=2: "Name="&env$('Q')&"\UBmstr\intopalm.txt,RecL=204,Replace",display,output 
15100   else if trim$(device$)="LapTop" then 
15110     open #h_out:=2: "Name="&env$('Q')&"\UBmstr\Laptop.Out,RecL=200,Replace",internal,output,relative 
15120   else if trim$(device$)="AMR" then 
15130     open #h_out:=2: "Name=c:\ezreader\download.dat,RecL=256,Replace",display,output 
15140   else if trim$(device$)="Hersey" then 
15150     open #h_out:=2: "Name="&env$('Q')&"\UBmstr\READINGS.DAT,RecL=282,eol=none,Replace",display,output 
15160   else if trim$(device$)="EZReader" then 
15170     open #h_out:=2: "Name=c:\ezreader\READINGS.DAT,RecL=578,eol=none,Replace",display,output 
15180   else if device$='Unitech HT630' then 
15190     let out_filename$=env$('temp')&'\'&session$&'_uni_ht630.dat'
15200     let out_recl=256
15210     open #h_out:=fngethandle: "Name="&out_filename$&",RecL="&str$(out_recl)&",Replace",display,output 
15220   else if device$='ACS Meter Reader' then 
15230     let out_filename$=env$('temp')&'\'&session$&'_acs_meter_data.txt'
15240     let out_recl=256
15250     open #h_out:=fngethandle: "Name="&out_filename$&",RecL="&str$(out_recl)&",Replace",display,output 
15252   else if device$='Psion Workabout' and (env$('client')='Findlay' or env$('client')='Ash Grove') then ! maybe other clients too!!! XXX
15254     if ~exists('UBmstr') then execute 'mkdir UBmstr'
15256     open #h_out:=fngethandle: "Name="&env$('Q')&"\UBmstr\Readings.dat,RecL=128,Replace",display,output 
15260   else if device$='Psion Workabout' then ! and (env$('client')='Raymond' or env$('client')='Ash Grove' or env$('client')='Kincaid') then ! maybe other clients too!!! XXX
15270     open #h_out:=fngethandle: "Name="&env$('Q')&"\UBmstr\Readings.dat,RecL=128,Replace",display,output 
15280   else if device$='Itron FC300' then 
15290     let fn_itron_open
15292   ! else if device$="Master Meter"  then
15300   else 
15310     open #h_out:=fngethandle: 'Name='&br_filename$(env$('userprofile')&'\Desktop\ACS Hand Held Out.txt')&',RecL=1048,Replace',display,output
15312     ! open #h_out:=fngethandle: "Name=Readings.dat,RecL=99,Replace",display,output  ! all others use this for now
15320   end if 
15330   let workopen=1
15340 fnend  ! fn_openwork
15990 def fn_unitech_ht630
16000   ! INPUT FILE (from ACS to Hand Held) needs to contain the following fields:
16010   !   Account - 10 characters
16020   !   Route and Sequence - 12 digits (this is the order for accounts to be displayed in - it might contain duplicates and/or skip large ranges of numbers)
16030   !   Meter Type - 10 characters - "Gas", "Water", "Electric", etc.  Each house may have multiple meters that need to be read.  If a house has both gas and water than it would have two records in the file so that both can be ask.  The Meter Type will need to be displayed so the user will know which they should be entering.
16040   !   Customer Name - 40 characters - The name of the customer who's meter is being read.  This should be displayed when the reading is ask for.
16050   !   Meter Address - 40 characters - The address of the customer who's meter is being read.
16060   !   This should be displayed when the reading is ask for.
16070   !   Reading High - 10 digits - used to validate entry of new reading
16080   !   Reading Low - 10 digits - used to validate entry of new reading
16090   for a_item=1 to udim(mat a)
16100     if servicecode$(a_item)='WA' or servicecode$(a_item)='GA' or servicecode$(a_item)='EL' then ! or (demand)   it is a metered service
16110       if a(a_item)>0 then 
16120         if servicecode$(a_item)='WA' then 
16130           let usage_current=d(3) ! Water usage - current
16140           let reading_current=d(1)
16150         else if servicecode$(a_item)='GA' then 
16160           let usage_current=d(11) ! Gas usage - curent
16170           let reading_current=d(9)
16180         else ! if servicecode$(a_item)='EL' then
16190           pr 'developer note: add code to copy '&servicename$(a__item)&' usage current and reading current from mat d into usage_current and reading_current' : let fnpause
16200         end if 
16210         let unusual_usage_low=round(reading_current+usage_current*fn_pcent,2)
16220         let unusual_usage_high=round(reading_current+usage_current+usage_current*fn_pcent,2)
16230         pr #h_out,using FORM_UH_OUT: z$,route*100000000+sequence,servicename$(a_item)(1:10),e$(2),e$(1)(1:20),unusual_usage_low,unusual_usage_high
16240         FORM_UH_OUT: form pos 1,c 10,n 12,c 10,2*c 40,2*n 10
16250       end if  ! a(a_item)>0
16260     end if  ! it is a metered service
16270   next a_item
16280 fnend  ! fn_Unitech_HT630
16290 def fn_acs_meter_reader
16300   ! FILE (from ACS to Hand Held and from Hand Held to ACS) needs to contain the following fields:
16310   !   Account - 10 characters
16320   !   Route and Sequence - 12 digits (this is the order for accounts to be displayed in - it might contain duplicates and/or skip large ranges of numbers)
16330   !   Meter Type - 10 characters - "Gas", "Water", "Electric", etc.  Each house may have multiple meters that need to be read.  If a house has both gas and water than it would have two records in the file so that both can be ask.  The Meter Type will need to be displayed so the user will know which they should be entering.
16340   !   Customer Name - 40 characters - The name of the customer who's meter is being read.  This should be displayed when the reading is ask for.
16350   !   Meter Address - 40 characters - The address of the customer who's meter is being read.
16360   !   This should be displayed when the reading is ask for.
16370   !   Reading High - 10 digits - used to validate entry of new reading
16380   !   Reading Low - 10 digits - used to validate entry of new reading
16390   !   Reading - 10 digits - the new reading
16400   for a_item=1 to udim(mat a)
16410     if servicecode$(a_item)='WA' or servicecode$(a_item)='GA' or servicecode$(a_item)='EL' then ! or (demand)   it is a metered service
16420       if a(a_item)>0 then 
16430         if servicecode$(a_item)='WA' then 
16440           let usage_current=d(3) ! Water usage - current
16450           let reading_current=d(1)
16460         else if servicecode$(a_item)='GA' then 
16470           let usage_current=d(11) ! Gas usage - curent
16480           let reading_current=d(9)
16490         else ! if servicecode$(a_item)='EL' then
16500           pr 'developer note: add code to copy '&servicename$(a__item)&' usage current and reading current from mat d into usage_current and reading_current' : let fnpause
16510         end if 
16520         let unusual_usage_low=round(reading_current+usage_current*fn_pcent,2)
16530         let unusual_usage_high=round(reading_current+usage_current+usage_current*fn_pcent,2)
16540         pr #h_out,using FORM_ACSMR: z$,route*100000000+sequence,servicename$(a_item)(1:10),e$(2),e$(1)(1:20),unusual_usage_low,unusual_usage_high,0
16550         FORM_ACSMR: form pos 1,c 10,n 12,c 10,2*c 40,2*n 10,n 10
16560       end if  ! a(a_item)>0
16570     end if  ! it is a metered service
16580   next a_item
16590 fnend  ! fn_acs_meter_reader
16600 def fn_pcent
16620   if ~pcent_setup then 
16630     let pcent_setup=1
16640     open #h_company:=fngethandle: "Name="&env$('Q')&"\UBmstr\Company.h"&env$('cno'),internal,input 
16650     read #h_company,using "Form POS 130,n 4": pcent_return
16660     close #h_company: 
16670     if pcent_return=0 then let pcent_return=100
16680     let pcent_return=pcent_return*.01 ! convert to percent
16690   end if  ! ~pcent_setup
16700   let fn_pcent=pcent_return
16710 fnend  ! fn_pcent
17640 ! r: itron
17650 def fn_itron_open
17660   open #h_out:=fngethandle: "Name="&env$('Q')&"\HH"&ssession$&".int,RecL=128,EoL=None,Replace",internal,outin,relative 
17670   let fn_itron_record_fhd
17680   let itron_rdg_count=0
17690   let itron_cus_count=0
17700   let itron_mtr_count=0
17710   let itron_rtr_count=0
17730   let itron_chd_count=0
17740 fnend 
17750 def fn_itron_close
17760   let fn_itron_route_trailer
17770   let fn_itron_record_ftr
17780   ! 
17790   let rec_current=0 ! restore #h_out:
17800   do 
17810     let rec_current+=1
17820     if rec_current>lrec(h_out) then goto IC_EOF_1
17830     read #h_out,using 'form pos 1,C 126',rec=rec_current: rec_line$
17840     let rec_type$=rec_line$(1:3)
17850     if rec_type$='RHD' then ! route header
17860       ! itron_rhd_current=rec(h_out)
17870     else if rec_type$='RTR' then ! route trailer
17880       let itron_rtr_current=rec(h_out)
17900       rewrite #h_out,using 'form pos 18,n 4,pos 34,N 4,N 4,N 4,Pos 52,3*N 4',rec=itron_rtr_current: itron_rdg_count,itron_rff_count,itron_cus_count,itron_mtr_count,itron_mtr_g_count,itron_mtr_w_count,itron_mtr_e_count norec ignore
17910       let itron_rdg_count=0
17920       let itron_rdg_count=0
17930       let itron_rff_count=0
17940       let itron_cus_count=0
17950       let itron_mtr_count=0
17960       let itron_mtr_e_count=0
17970       let itron_mtr_g_count=0
17980       let itron_mtr_i_count=0
17990       let itron_mtr_s_count=0
18000       let itron_mtr_w_count=0
18010     else if rec_type$='RFF' then 
18020       let itron_rff_count+=1
18030     else if rec_type$='CUS' then 
18040       let itron_cus_count+=1
18050     else if rec_type$='MTR' then 
18060       let itron_mtr_count+=1
18070       let itron_meter_category$=rec_line$(102:102)
18080       if itron_meter_category$="E" then 
18090         let itron_mtr_e_count+=1
18100       else if itron_meter_category$="G" then 
18110         let itron_mtr_g_count+=1
18120       else if itron_meter_category$="I" then 
18130         let itron_mtr_i_count+=1
18140       else if itron_meter_category$="S" then 
18150         let itron_mtr_s_count+=1
18160       else if itron_meter_category$="W" then 
18170         let itron_mtr_w_count+=1
18180       end if 
18190     else if rec_type$='RDG' then 
18200       let itron_rdg_count+=1
18210     else if rec_type$='FTR' then 
18220       let itron_ftr_current=rec(h_out)
18230       rewrite #h_out,using 'form pos 14,n 2',rec=itron_fhd_current: itron_chd_count
18240       rewrite #h_out,using 'form pos 14,n 2',rec=itron_ftr_current: itron_chd_count
18250       let itron_chd_count=0
18260     else if rec_type$='FHD' then 
18270       let itron_fhd_current=rec(h_out)
18280     else if rec_type$='CHD' then 
18290       let itron_chd_count+=1
18300     end if  ! rec_type$=...
18310   loop 
18320   IC_EOF_1: ! 
18330   ! 
18340   open #h_out2:=fngethandle: "Name="&env$('Q')&"\Download.dat,RecL=128,EoL=None,Replace",display,output 
18350   restore #h_out: 
18360   do 
18370     read #h_out,using 'form pos 1,C 126': rec_line$ eof IC_EOF_2
18380     pr #h_out2,using 'form pos 1,C 126,c 2': rec_line$,crlf$
18390   loop 
18400   IC_EOF_2: ! 
18405   close #h_out2: 
18410   close #h_out,free: 
18412   if out_filename$<>'' then 
18413     let fnCopy(env$('Q')&"\Download.dat",br_filename$(out_filename$))
18414     let fn_report_created_file(out_filename$)
18415   else if env$('client')='Findlay' then 
18430     let fnCopy(env$('Q')&"\Download.dat",'\\vof-pc\itronshared\FCS\Import\Input\download.dat')
18435     let fn_report_created_file(os_filename$('\\vof-pc\itronshared\FCS\Import\Input\download.dat'))
18436   else 
18437     if ~exists("c:\mvrs") then execute 'mkdir c:\mvrs'
18438     if ~exists("c:\mvrs\xfer") then execute 'mkdir c:\mvrs\xfer'
18440     if ~exists("C:\mvrs\xfer\Download") then execute 'mkdir c:\mvrs\xfer\Download'
18445     execute 'copy '&env$('Q')&'\Download.dat C:\mvrs\xfer\Download\*.*'
18455     let fn_report_created_file(os_filename$('C:\mvrs\xfer\Download\Download.dat'))
18460   end if 
18470   !   if exists ("C:\MVRS\MVRSWin5.exe") then
18480   !     if ~exists ("C:\MVRS\MVRSWin5.cmd") then
18490   !       open #h_tmp:=fngethandle: 'Name=C:\MVRS\MVRSWin5.cmd,RecL=256,replace',display,output
18500   !       pr #h_tmp: 'c:'
18510   !       pr #h_tmp: 'cd \MVRS'
18520   !       pr #h_tmp: 'C:\MVRS\MVRSWin5.exe'
18530   !       close #h_tmp:
18540   !     end if
18550   !     execute 'Sy -c C:\MVRS\MVRSWin5.cmd'
18560   !   end if
18570 fnend 
18580 def fn_itron_route_trailer
18590   let fn_itron_record_rtr
18600   let fn_itron_record_ctr
18610   let itron_rtr_count+=1
18620 fnend  ! fn_itron_route_trailer
18630 def fn_itron
18640   for a_item=1 to udim(mat a)
18650     if servicecode$(a_item)='WA' or servicecode$(a_item)='GA' or servicecode$(a_item)='EL' then ! or (demand)   it is a metered service
18660       if a(a_item)>0 then 
18670         if servicecode$(a_item)='WA' then 
18680           let usage_current=d(3) ! Water usage - current
18690           let reading_current=d(1)
18700         else if servicecode$(a_item)='GA' then 
18710           let usage_current=d(11) ! Gas usage - curent
18720           let reading_current=d(9)
18730         else if servicecode$(a_item)='EL' then 
18740           let usage_current=d(7) ! KWH usage - curent
18750           let reading_current=d(5)
18760         else ! if servicecode$(a_item)='EL' then
18770           pr 'developer note: add code to copy '&servicename$(a__item)&' usage current and reading current from mat d into usage_current and reading_current' : let fnpause
18780         end if 
18790         let unusual_usage_low=int(usage_current-usage_current*fn_pcent) : if unusual_usage_low<0 then let unusual_usage_low=0
18800         let unusual_usage_high=int(usage_current+usage_current*fn_pcent)
18810         if z$<>z_prior$ then 
18820           let z_prior$=z$
18830           if route<>route_prior then 
18840             if route_prior<>0 then 
18850               let fn_itron_route_trailer
18860             end if  ! route_prior<>0
18870             let route_prior=route
18880             let fn_itron_record_chd
18890             let route_itron$=cnvrt$('pic(##)',route)&cnvrt$('pic(######)',route)
18900             let fn_itron_record_rhd
18910           end if  ! route<>route_prior
18920           let fn_itron_record_cus
18930         end if  ! z$<>z_prior$
18940         let fn_itron_record_mtr
18950         let fn_itron_record_mtx
18960         let unusual_usage_low=int(usage_current-usage_current*fn_pcent) : if unusual_usage_low<0 then let unusual_usage_low=0
18970         let unusual_usage_high=int(usage_current+usage_current*fn_pcent)
18980         let fn_itron_record_rdg
18990         let fn_itron_record_rff
19000       end if  ! a(a_item)>0
19010     end if  ! it is a metered service
19020   next a_item
19030 fnend 
19040 def fn_itron_record_rdg ! reading - pg 19
19050   let fn_record_init
19060   let fn_record_addc(3,'RDG')
19070   let fn_record_addc(8,route_itron$)
19080   let fn_record_addc(4,servicecode$(a_item))
19090   let fn_record_addc(1,'Y')
19100   let fn_record_addc(1,'L') ! field 5
19102   let fn_record_addn(3,0)
19104   let fn_record_addn(3,0)
19106   let fn_record_addx(1)
19108   let fn_record_addn(2,0)
19110   ! 
19112   let itron_number_of_dials=val(fn_meter_info$('Number of Dials',z$,servicecode$(a_item)))
19114   if itron_number_of_dials=0 then let itron_number_of_dials=6
19150   let fn_record_addn(2,itron_number_of_dials) ! field 10  -  number of dials
19160   let fn_record_addn(2,0) ! 
19170   let transmitter_number$=fn_meter_info$('transmitter number',z$,servicecode$(a_item))
19180   if transmitter_number$<>'' then let fn_record_addc(1,'R') else let fn_record_addc(1,'K') : let skip_next_rff_record=1
19190   let fn_record_addn(10,reading_current)
19200   let fn_record_addn(10,unusual_usage_high)
19210   let fn_record_addn(10,unusual_usage_low) ! field 15
19220   let fn_record_addn(6,0)
19230   let fn_record_addn(1,0)
19240   let fn_record_addn(1,0)
19250   let fn_record_addn(5,0)
19260   let fn_record_addn(1,0) ! field 20
19270   let fn_record_addx(1)
19272   ! 
19274   let itron_read_type=val(fn_meter_info$('Read Type',z$,servicecode$(a_item)))
19276   if itron_read_type=0 then let itron_read_type=a_item ! gas, water, electric a unique number for each - a_item (service number) is as good as any
19280   let fn_record_addc(2,cnvrt$('pic(##)',itron_read_type))
19290   let fn_record_addn(6,0)
19300   let fn_record_addn(6,0)
19310   let fn_record_addn(5,0) ! field 25
19320   let fn_record_addx(31)
19330   ! fn_record_addc(2,crlf$)
19340   let fn_record_write(h_out)
19350 fnend  ! fn_itron_record_rdg
19360 def fn_itron_record_rhd ! route header - pg 6
19370   let fn_record_init
19380   let fn_record_addc(3,'RHD')
19390   let fn_record_addc(8,route_itron$)
19400   let fn_record_addc(1,'N')
19410   let fn_record_addc(1,'N')
19420   let fn_record_addn(4,0) ! field 5 - total number of keys
19430   let fn_record_addn(4,0) ! field 6 - total number of reading records
19440   let fn_record_addn(4,0) ! field 7 - total number of demand meters
19450   let fn_record_addn(4,0) ! field 8 - total number of keyed readings
19460   let fn_record_addn(4,0) ! field 9 - total number of optical probe readings
19470   let fn_record_addn(4,0) ! field 10 - total number of off-site (Radio) readings
19480   let fn_record_addn(4,0) ! field 11 - total number of customer records
19490   let fn_record_addn(4,0) ! field 12 - total number of meter records
19500   let fn_record_addn(6,0)
19510   let fn_record_addn(4,0)
19520   let fn_record_addn(4,0) ! field 15
19530   let fn_record_addn(4,0)
19540   let fn_record_addn(4,0)
19550   let fn_record_addn(4,0)
19560   let fn_record_addc(2,'')
19570   let fn_record_addc(2,'') ! field 20 - zone
19580   let fn_record_addc(2,'')
19590   let fn_record_addn(2,0)
19600   let fn_record_addn(2,0)
19610   let fn_record_addn(4,0)
19620   let fn_record_addc(1,'') ! field 25
19630   let fn_record_addx(40)
19640   ! fn_record_addc(2,crlf$)
19650   let fn_record_write(h_out)
19660 fnend  ! fn_itron_record_rhd
19670 def fn_itron_record_rtr ! route trailer - pg 6
19680   let fn_record_init
19690   let fn_record_addc(3,'RTR')
19700   let fn_record_addc(8,route_itron$)
19710   let fn_record_addc(1,'N')
19720   let fn_record_addc(1,'N')
19730   let fn_record_addn(4,0) ! field 5 - total number of keys
19740   let fn_record_addn(4,0) ! field 6 - total number of reading records
19750   let fn_record_addn(4,0) ! field 7 - total number of demand meters
19760   let fn_record_addn(4,0) ! field 8 - total number of keyed readings
19770   let fn_record_addn(4,0) ! field 9 - total number of optical probe readings
19780   let fn_record_addn(4,0) ! field 10 - total number of off-site (Radio) readings
19790   let fn_record_addn(4,0) ! field 11 - total number of customer records
19800   let fn_record_addn(4,0) ! field 12 - total number of meter records
19810   let fn_record_addn(6,0)
19820   let fn_record_addn(4,0)
19830   let fn_record_addn(4,0) ! field 15
19840   let fn_record_addn(4,0)
19850   let fn_record_addn(4,0)
19860   let fn_record_addn(4,0)
19870   let fn_record_addc(2,'')
19880   let fn_record_addc(2,'') ! field 20 - zone
19890   let fn_record_addc(2,'')
19900   let fn_record_addn(2,0)
19910   let fn_record_addn(2,0)
19920   let fn_record_addn(4,0)
19930   let fn_record_addc(1,'') ! field 25
19940   let fn_record_addx(40)
19950       ! fn_record_addc(2,crlf$)
19960   let fn_record_write(h_out)
19970 fnend  ! fn_itron_record_rtr
19980 def fn_itron_record_cus ! Customer - pg 11
19990   let fn_record_init
20000   let fn_record_addc(3,'CUS')
20010   let fn_record_addc(8,route_itron$)
20020   let fn_record_addn(3,fn_cnt_of_metered_svcs_active)
20030   let fn_record_addc(20,z$)
20040   let fn_record_addc(20,e$(2)) ! field 5 - name
20050   let fn_record_addc(20,e$(1))
20060   ! fn_record_addc(6,'')
20070   ! fn_record_addc(14,'')
20080   let fn_record_addc(20,'')
20090   let fn_record_addx(2)
20100   let fn_record_addn(1,0)
20110   let fn_record_addc(20,'') ! field 10 - Customer Information
20120   let fn_record_addc(1,'N')
20130   let fn_record_addc(4,'')
20140   let fn_record_addc(2,'')
20150   let fn_record_addc(1,'')
20160   let fn_record_addx(1) ! field 15
20170   ! fn_record_addc(2,crlf$)
20180   let fn_record_write(h_out)
20190 fnend  ! fn_itron_record_cus
20200 def fn_itron_record_mtx ! latitude, longitude, etc - pg 16
20210   let fn_record_init
20220   let fn_record_addc(3,'MTX')
20230   let fn_record_addc(8,route_itron$)
20240   let fn_record_addc(12,fn_meter_info$('meter number',z$,servicecode$(a_item)))
20250   dim irm_tmp$*20
20260   let irm_tmp$=lwrc$(fn_meter_info$('longitude',z$,servicecode$(a_item)))
20270   if irm_tmp$(1:1)="n" or irm_tmp$(1:1)="s" or irm_tmp$(1:1)="e" or irm_tmp$(1:1)="w" then let irm_tmp$=str$(fn_dms_to_dec(irm_tmp$))
20280   let fn_record_addc(17,irm_tmp$)
20290   let irm_tmp$=lwrc$(fn_meter_info$('latitude',z$,servicecode$(a_item)))
20300   if irm_tmp$(1:1)="n" or irm_tmp$(1:1)="s" or irm_tmp$(1:1)="e" or irm_tmp$(1:1)="w" then let irm_tmp$=str$(fn_dms_to_dec(irm_tmp$))
20310   let fn_record_addc(17,irm_tmp$)
20320   let fn_record_addc(12,'')
20330   let fn_record_addx(57)
20340   ! fn_record_addc(2,crlf$)
20350   let fn_record_write(h_out)
20360 fnend  ! fn_itron_record_mtx
20370 def fn_dms_to_dec(dtd_in$*20) ! for longitude and latitude
20380 ! N31 35 47.8
20390   if dtd_in$(1:1)="n" then let dtd_sign$='+' : let dtd_in$(1:1)=''
20400   if dtd_in$(1:1)="e" then let dtd_sign$='+' : let dtd_in$(1:1)=''
20410   if dtd_in$(1:1)="s" then let dtd_sign$='-' : let dtd_in$(1:1)=''
20420   if dtd_in$(1:1)="w" then let dtd_sign$='-' : let dtd_in$(1:1)=''
20430   ! 
20440   let dtd_pos_space=pos(dtd_in$,' ')
20450   let dtd_degrees=val(dtd_in$(1:dtd_pos_space))
20460   let dtd_in$(1:dtd_pos_space)=''
20470   ! 
20480   let dtd_pos_space=pos(dtd_in$,' ')
20490   let dtd_minutes=val(dtd_in$(1:dtd_pos_space))
20500   let dtd_in$(1:dtd_pos_space)=''
20510   ! 
20520   let dtd_seconds=val(dtd_in$) conv ignore
20540   let dtd_return=dtd_degrees+dtd_minutes/60+dtd_seconds/3600
20550   if dtd_sign$='-' then let dtd_return=-dtd_return
20560   let fn_dms_to_dec=dtd_return
20570 fnend  ! fn_dms_to_dec
20580 def fn_itron_record_rff ! off-site (Radio) reads - pg 22
20582   if skip_next_rff_record=1 then 
20584     let skip_next_rff_record=0
20586   else 
20590     let fn_record_init
20600     let fn_record_addc(3,'RFF')
20610     let fn_record_addc(8,route_itron$)
20620     let fn_record_addc(8,fn_meter_info$('transmitter number',z$,servicecode$(a_item)))
20630     let fn_record_addc(6,'')
20640     let fn_record_addc(4,'ERT ') ! field 5
20650     let fn_record_addx(7)
20660     let fn_record_addn(2,0)
20670     let fn_record_addn(12,0)
20680     let fn_record_addn(4,0)
20690     let fn_record_addx(10) ! field 10
20700     let fn_record_addc(2,'16')
20710     let fn_record_addc(1,'')
20720     let fn_record_addc(1,'')
20730     let fn_record_addc(1,'')
20740     let fn_record_addc(1,'') ! field 15
20750     let fn_record_addx(56)
20760     ! fn_record_addc(2,crlf$)
20770     let fn_record_write(h_out)
20772   end if 
20780 fnend  ! fn_itron_record_rff
20790 def fn_itron_record_fhd ! file header - pg 3
20800   let fn_record_init
20810   let fn_record_addc(3,'FHD')
20820   let fn_record_addc(1,'N')
20830   let fn_record_addc(1,'N')
20840   let fn_record_addc(5,'')
20850   let fn_record_addx(3) ! field 5
20860   let fn_record_addn(2,99) ! field 6 - number of cycles - should be one for each route
20870   let fn_record_addc(1,'Y') ! field 7 - RFF records present?  Y/N
20880   let fn_record_addc(1,'N')
20890   let fn_record_addc(1,'N')
20900   let fn_record_addx(108) ! field 10
20910   ! fn_record_addc(2,crlf$)
20920   let fn_record_write(h_out)
20930 fnend  ! fn_itron_record_fhd
20940 def fn_itron_record_ftr ! file trailer - pg 3
20950   let fn_record_init
20960   let fn_record_addc(3,'FTR')
20970   let fn_record_addc(1,'N')
20980   let fn_record_addc(1,'N')
20990   let fn_record_addc(5,'')
21000   let fn_record_addx(3) ! field 5
21010   let fn_record_addn(2,99) ! field 6 - number of cycles - should be one for each route
21020   let fn_record_addc(1,'Y') ! field 7 - RFF records present?  Y/N
21030   let fn_record_addc(1,'N')
21040   let fn_record_addc(1,'N')
21050   let fn_record_addx(108) ! field 10
21060   ! fn_record_addc(2,crlf$)
21070   let fn_record_write(h_out)
21080 fnend  ! fn_itron_record_FTR
21090 def fn_itron_record_chd ! cycle header - pg 5
21100   let fn_record_init
21110   let fn_record_addc(3,'CHD')
21120   let fn_record_addc(2,cnvrt$('pic(##)',route))
21130   let fn_record_addn(4,1)
21140   let fn_record_addc(8,date$('mmddccyy'))
21150   let fn_record_addx(109) ! field 5
21160   ! fn_record_addc(2,crlf$)
21170   let fn_record_write(h_out)
21180 fnend  ! fn_itron_record_chd
21190 def fn_itron_record_ctr ! cycle trailer - pg 5
21200   let fn_record_init
21210   let fn_record_addc(3,'CTR')
21220   let fn_record_addc(2,cnvrt$('pic(##)',route))
21230   let fn_record_addn(4,1)
21240   let fn_record_addc(8,date$('mmddccyy'))
21250   let fn_record_addx(109) ! field 5
21260   ! fn_record_addc(2,crlf$)
21270   let fn_record_write(h_out)
21280 fnend  ! fn_itron_record_ctr
21290 def fn_itron_record_mtr ! meter record - pg 13
21300   let fn_record_init
21310   let fn_record_addc(3,'MTR')
21320   let fn_record_addc(8,route_itron$)
21330   let fn_record_addn(3,1)
21340   let fn_record_addx(2)
21350   let fn_record_addn(1,0) ! field 5
21360   let fn_record_addx(8)
21370   let fn_record_addn(1,0)
21380   let fn_record_addx(2)
21390   let fn_record_addn(1,0)
21400   let fn_record_addc(1,' ') ! field 10
21410   let fn_record_addc(1,'A')
21420   let fn_record_addc(14,'') ! field 12 - optiocal probe recorder ID
21430   let fn_record_addc(12,fn_meter_info$('Meter Number',z$,servicecode$(a_item)))
21440   let fn_record_addx(2)
21450   let fn_record_addc(2,'00') ! field 15 - meter type
21460   let fn_record_addn(8,sequence*10+a_item)
21470   let fn_record_addx(20)
21480   let fn_record_addx(1)
21490   let fn_record_addc(2,'00')
21500   let fn_record_addx(1) ! field 20
21510   let fn_record_addc(2,'00')
21520   let fn_record_addx(1)
21530   let fn_record_addc(2,'00')
21540   let fn_record_addn(1,3)
21550   let fn_record_addc(1,'Y') ! field 25
21560   let fn_record_addc(1,'N')
21570   let fn_record_addc(1,servicecode$(a_item)(1:1))
21580   let fn_record_addc(1,'L')
21590   let fn_record_addn(3,0)
21600   let fn_record_addc(2,'') ! field 30 - meter audit 1
21610   let fn_record_addc(2,'')
21620   let fn_record_addc(1,'')
21630   let fn_record_addc(1,'')
21640   let fn_record_addx(14)
21650   ! fn_record_addc(2,crlf$) ! field 35 (the end CR/LF)
21660   let fn_record_write(h_out)
21670 fnend  ! fn_itron_record_mtr
21680 ! /r
24000 def fn_aclara ! z$,mat e$,extra$(1-2),route
24020   dim tmpCity$*64,tmpState$*64,tmpZip$*64
24040   fncsz(e$(4),tmpCity$,tmpState$,tmpZip$)
24060   !
24080   fn_record_init(chr$(9))                                           ! Aclara Name               ACS Name (if different)
24100   fn_record_addn(11,fnMeterAddressLocationID(e$(1), 1))          ! LocationID
24120   fn_record_addc(10,z$)                                             ! Account Number
24140   fn_record_addc(30,e$(2))                                          ! Customer Name
24150   fn_record_addc(12,extra$(2))                                      ! Phone Number
24160   fn_record_addc(30,e$(3))                                          ! Service Address 1          Address 1 - Primary
24180   fn_record_addc(30,extra$(1))                                      ! Service Address 2          Address 2 - Primary
24200   fn_record_addc(30,tmpCity$)                      
24220   fn_record_addc(10,tmpState$)                     
24240   fn_record_addc(15,tmpZip$)                       
24260   fn_record_addn(3,route)                                           ! Cycle and Route            Route Number
24270   fn_record_addn(7,sequence)                                        ! Sequence                   Sequence
24280   fn_record_addc(8,fn_meter_info$('Meter Number',z$,'WA'))        ! Meter Serial Number        Meter.Meter Number
24300   fn_record_addc(20,fn_meter_info$('Transmitter Number',z$,'WA')) ! Transmitter Serial Number  Meter.Transmitter Number
24320 ! fn_record_addc(20,'(Rate Code Description??)')                      ! Service Type
24340   fn_record_addc(40,fn_meter_info$('Meter Type',z$,'WA'))         ! Meter Model/Type           
24360 ! fn_record_addc(9,,fn_meter_info$('reading multipler',z$,'WA'))      ! Meter Size
24380   fn_record_write(h_out)
24400 fnend
25000 def fn_masterMeter ! z$,mat e$,extra$(1-2),route
25010   dim tmpCity$*64,tmpState$*64,tmpZip$*64
25020   fncsz(e$(4),tmpCity$,tmpState$,tmpZip$)
25030   usage_current=d(3) ! Water usage - current
25040   reading_current=d(1)
25050   unusual_usage_low=round(reading_current+usage_current*fn_pcent,2)
25060   unusual_usage_high=round(reading_current+usage_current+usage_current*fn_pcent,2)
25070   !
25080   fn_record_init(chr$(9))                                           ! 
25090   fn_record_addc(10,z$)                                             ! Account Number
25100   fn_record_addc(30,e$(2))                                          ! Customer Name
25110   fn_record_addc(30,e$(1))                                          ! Meter Address
25120   fn_record_addn(3,route)                                           ! Route Number
25130   fn_record_addn(7,sequence)                                        ! Sequence
25140   fn_record_addc(12,fn_meter_info$('Meter Number',z$,'WA'))       ! Meter.Meter Number
25150   fn_record_addc(20,fn_meter_info$('Transmitter Number',z$,'WA')) ! Transmitter Serial Number  Meter.Transmitter Number
25160   fn_record_addn(9,d(1))                                            ! Service 1 (Water) – Reading – Current
25162   ! pr 'AAA - '&srep$(rec_line$,chr$(9),'>') : pause
25170   fn_record_addc(17,fn_meter_info$('longitude',z$,'WA'))          ! Meter.Longitude
25172   ! pr 'BBB - '&srep$(rec_line$,chr$(9),'>') : pause
25180   fn_record_addc(17,fn_meter_info$('latitude',z$,'WA'))           ! Meter.Latitude
25190   fn_record_addc(40,fn_meter_info$('Meter Type',z$,'WA'))         ! Meter Model/Type           
25200   tmp$=fn_meter_info$('reading multipler',z$,'WA') : if tmp$='' then let tmp$='1'
25210   fn_record_addc(40,tmp$)                                           ! Meter Reading Multiplier (default to 1 if blank)
25220   fn_record_addc(9,'')                                              ! Service 1 (Water) – Reading – Bring Back (leave an empty column for it
25230   fn_record_addc(9,'')                                              ! Service 1 (Water) – Reading Date – Bring Back (leave an empty column for it
25240   fn_record_addn(10,unusual_usage_low)                              ! Unusual Usage Low Reading
25250   fn_record_addn(10,unusual_usage_high)                             ! Unusual Usage High Reading
25260   fn_record_write(h_out)
25270 fnend
25280 !
25500 def fn_READy_Water ! z$,mat e$,extra$(1-2),route
25520   dim tmpCity$*64,tmpState$*64,tmpZip$*64
25540   fncsz(e$(4),tmpCity$,tmpState$,tmpZip$)
25560   fn_record_init(chr$(9))                                           ! ACS Name (if different)
25580   fn_record_addc(10,z$)                                             ! Account Number
25600   fn_record_addc(30,e$(2))                                          ! Customer Name
25620   fn_record_addc(12,extra$(2))                                      ! Phone Number
25640   fn_record_addc(30,e$(1))                                          ! Meter Address (switched to 7/5/17 as per request by Sheri)
25642   ! fn_record_addc(30,e$(3))                                          ! Address 1 - Primary
25660   fn_record_addc(30,extra$(1))                                      ! Address 2 - Primary
25680   fn_record_addc(30,tmpCity$)                                       ! City
25700   fn_record_addc(10,tmpState$)                                      ! State
25720   fn_record_addc(15,tmpZip$)                                        ! Zip
25740   fn_record_addn(3,route)                                           ! Route Number
25760   fn_record_addn(7,sequence)                                        ! Sequence
25780   fn_record_addc(8,fn_meter_info$('Meter Number',z$,'WA'))        ! Meter.Meter Number
25800   fn_record_write(h_out)
25820 fnend
26000 def fn_record_init(; setDelimiter$)
26020   dim rec_line$*512
26040   let rec_line$=''
26050   gRecordDelimiter$=setDelimiter$
26060 fnend  ! fn_record_init
26080 def fn_record_addc(rac_field_length,rac_field_text$*256)
26100   let rec_line$=rec_line$&rpad$(rac_field_text$(1:rac_field_length),rac_field_length)&gRecordDelimiter$
26120 fnend
26140 def fn_record_addn(ran_field_length,ran_field_value)
26160   let rec_line$=rec_line$&lpad$(str$(ran_field_value)(1:ran_field_length),ran_field_length)&gRecordDelimiter$
26180 fnend
26200 def fn_record_addx(ran_field_length)
26220   let rec_line$=rec_line$&rpt$(' ',ran_field_length)&gRecordDelimiter$
26240 fnend  ! fn_record_addx
27000 def fn_record_write(h_out)
27020   if gRecordDelimiter$<>'' then ! remove trailing delimiter
27040     rec_line$((len(rec_line$)-len(gRecordDelimiter$)+1):len(rec_line$))=''
27060   end if
27080   if device$='Itron FC300' then 
27100     write #h_out,using 'form pos 1,C '&str$(len(rec_line$)): rec_line$
27120   else
27140     pr #h_out,using 'form pos 1,C '&str$(len(rec_line$)): rec_line$
27150     ! pr srep$(rec_line$,chr$(9),'>') : pause
27160   end if
27180 fnend 
28000 def fn_boson
28020   dim z_out$*14,custname$*30
28040   for j=1 to len(seq$)
28060     if val(seq$(j:j))=1 then 
28080       let svc_flag$="W"
28100     else if val(seq$(j:j))=2 then 
28120       let svc_flag$="E"
28140     else if val(seq$(j:j))=4 then 
28160       let svc_flag$="G"
28180     end if 
28280     custname$=e$(2)
28320     let z_out$=trim$(z$)&svc_flag$
28340     on val(seq$(j:j)) goto WATER_BOSON,ELECTRIC_BOSON,DEMAND_BOSON,GAS_BOSON none BOSON_NEXT_SEQUENCE
28360     WATER_BOSON: if a(1)=0 or final<>0 then goto BOSON_NEXT_SEQUENCE
28380     let x$=cnvrt$("pic(######)",d(5)) : let readdate$=x$(1:2)&"-"&x$(3:4)&"-"&x$(5:6)
28400     if env$('client')='Kincaid' then 
28420       let readingt$="S"
28440     else if env$('client')="Moweaqua" then 
28460       if trim$(f$(1))="" then 
28480         let readingt$="S"
28500       else 
28520         let readingt$="P"
28540       end if 
28560     else if trim$(extra$(3))="" then 
28580       let readingt$="S" 
28600     else 
28620       let readingt$="P"
28640     end if
28660     if env$('client')="Purdy" or env$('client')="Billings" or env$('client')="Cerro Gordo" then let readingt$="S"
28680     let metertag=0: let metertag=val(extra$(3)) conv ignore
28700     if env$('client')="Moweaqua" then let metertag=0: let metertag=val(f$(1)) conv ignore
28720     if env$('client')="Moweaqua" and (a(1)=1 or a(1)=2) then let d(1)=d(1): let d(2)=d(2): let d(3)=d(3)
28740     if env$('client')="Monticello" and trim$(extra$(7))="22" then let d(1)=d(1)*100: let d(2)=d(2)*100: let d(3)=d(3)*100
28760     if env$('client')="Monticello" and trim$(extra$(7))="23" then let d(1)=d(1)*10: let d(2)=d(2)*10: let d(3)=d(3)*10
28780     ! If env$('client')="Monticello" AND (TRIM$(EXTRA$(7))="24" then don't do anything
28800     if env$('client')="Monticello" and trim$(extra$(7))="65" then let d(1)=d(1)*100: let d(2)=d(2)*100: let d(3)=d(3)*100
28820     if env$('client')="Monticello" and trim$(extra$(7))="66" then let d(1)=d(1)*100: let d(2)=d(2)*100: let d(3)=d(3)*100
28840     let meterdials=0 ! if env$('client')="Purdy" or env$('client')="Billings" then let meterdials=0 else let meterdials=7
28860     if trim$(z_out$)='200670' then pause 
28880     pr #h_out,using F_BOSON_OUT: lpad$(rtrm$(z_out$),14),"",custname$,e$(1),"","",svc_flag$,f$(1)," ",0,d(1)+(d(3)*2),d(1)+(d(3)*.50),readdate$,route,"",sequence,meterdials,d(1),readingt$,metertag
28900     !     pr #h_out,using F_BOSON_OUT: lpad$(rtrm$(z_out$),14),"",custname$,e$(1),"","",svc_flag$,f$(1)," ",0,d(1)+(d(3)*2),d(1)+(d(3)*.50),readdate$,val(z$(1:2)),"",val(z$(3:7)),meterdials,d(1),readingt$,metertag
28920     F_BOSON_OUT: form pos 1,c 14,c 3,3*c 30,2*c 1,c 20,c 5,3*pic(#########),pic(########),pic(####),c 1,pic(######),pic(##),pic(#########),c 1,pic(############)
28940     goto BOSON_NEXT_SEQUENCE
28960     ! ___________________________
28980     ELECTRIC_BOSON: if a(3)=0 or trim$(servicename$(3))<>"Electric" then goto BOSON_NEXT_SEQUENCE
29000     pr #h_out,using F_BOSON_OUT: lpad$(rtrm$(z_out$),14),"",custname$,e$(1),"","",svc_flag$,f$(1)," ",0,d(5)+(d(7)*2),d(5)+(d(7)*.50),d(5),route,"",sequence,0,d(5),"R",f$(2)
29020     !     pr #h_out,using F_BOSON_OUT: lpad$(rtrm$(z_out$),14),"",custname$,e$(1),"","",svc_flag$,f$(1)," ",0,d(5)+(d(7)*2),d(5)+(d(7)*.50),d(5),val(z$(1:2)),"",val(z$(3:7)),0,d(5),"R",f$(2)
29040     goto BOSON_NEXT_SEQUENCE
29060     ! ___________________________
29080     DEMAND_BOSON: goto BOSON_NEXT_SEQUENCE
29100     goto BOSON_NEXT_SEQUENCE
29120     ! ___________________________
29140     GAS_BOSON: if a(4)=0 or trim$(servicename$(4))<>"Gas" then goto BOSON_NEXT_SEQUENCE
29160     let readingt$="R"
29180     pr #h_out,using F_BOSON_OUT: lpad$(rtrm$(z_out$),14),"",custname$,e$(1),"","",svc_flag$,f$(1)," ",0,d(9)+(d(11)*2),d(9)+(d(11)*.50),d(9),route,"",sequence,0,d(9),readingt$,f$(2)
29200     !     pr #h_out,using F_BOSON_OUT: lpad$(rtrm$(z_out$),14),"",custname$,e$(1),"","",svc_flag$,f$(1)," ",0,d(9)+(d(11)*2),d(9)+(d(11)*.50),d(9),val(z$(1:2)),"",val(z$(3:7)),0,d(9),readingt$,f$(2)
29220     goto BOSON_NEXT_SEQUENCE
29240     ! ___________________________
29260     BOSON_NEXT_SEQUENCE: ! 
29280   next j
29300 fnend 
58000 TRANSFER: ! r: Transfer to or from Hand Held Computer
58020   dim out_filename_report$*512
58040   let out_filename_report$=file$(h_out)
58060   close #h_out: ioerr ignore
58080   close #h_customer_i1: ioerr ignore
58100   let fn_report_created_file(out_filename_report$)
58120   let fn_transfer
58200 goto XIT ! /r
60000 XIT: let fnxit
60010 IGNORE: continue 
62000 def fn_transfer
62020   if device$="ACS Meter Reader" then 
62040     let fntos(sn$="ACSMR_ASK_DEST")
62060     mat resp$=("")
62080     let fnlbl(1,1,"Android Drive:",20,1)
62100     let tip$="Drive letter of the destination android device."
62120     let fncomboa("USB-Drive",1,23,mat drive$,tip$)
62140     let fncmdset(2)
62160     let fnacs(sn$,0,mat resp$,ckey)
62180     if ckey<>5 then 
62200       let dest$=resp$(1)
62220       execute "copy "&out_filename$&" "&trim$(dest$)&"acs_meter_data.txt"
62240     end if  ! ckey<>5
62260     goto TRANSFER_XIT
62280   end if  ! device$="ACS Meter Reader"
62400   !   else if device$="Badger" then
62420   !     goto TRANSFER_XIT ! output file already if folder for                                               badger to read
62440   if device$="LapTop" then ! else if...
62460     goto TRANSFER_TO_LAPTOP
62480   else if device$="Psion Workabout" then 
62500     if exists("S:\RCom\RComW.exe")<>0 then ! else  if ...
62520       execute 'Sy "'&os_filename$("S:\RCom\RComW.exe")&'" /w -n'
62540     else 
62560       execute 'Sy "'&os_filename$("S:\acsUB\PreRoute.bat")&'" -n' ! "Psion Workabout"
62580     end if  ! device$="Psion Workabout"
62600   end if 
62620   goto TRANSFER_XIT
63000   TRANSFER_TO_LAPTOP: ! r: transfer files for laptop
63020   let fntos(sn$="trtolaptop")
63040   mat resp$=("")
63060   let fnlbl(1,1,"Destination Drive:",20,1)
63080   let tip$="Destination can be a drive designation including folders"
63100   let fntxt(1,23,20,100,0,"",0,tip$)
63120   if resp$(1)="" then let resp$(1)="A:\"
63140   let fncmdset(2)
63160   let fnacs(sn$,0,mat resp$,ckey) ! 
63180   if ckey=5 then goto TRANSFER_XIT
63200   let dest$=resp$(1)
63220   if len(dest$)=0 then goto TRANSFER_TO_LAPTOP
63240   if len(dest$)=1 then let dest$=dest$=":"
63260   if len(dest$)=3 and dest$(3:3)="/" then let dest$(3:3)=""
63280   execute "Copy "&env$('Q')&"\UBmstr\laptop.out "&trim$(dest$)&"\laptop.out"
63300   goto TRANSFER_XIT ! /r
63320   TRANSFER_XIT: ! 
63340 fnend  ! fn_transfer
64000 def fn_scr_selact
64020   mat resp$(5)=('')
64030   let fncreg_read('hhto.selection_method',selection_method$) : let selection_method=val(selection_method$) conv ignore
64040   if selection_method=0 then let selection_method=2
64060   let fntos(sn$="hhto1")
64080   let fnlbl(2,1,"Hand Held model:",16,1)
64100   !   let fncomboa("HH-FroCBox",1,18,mat ctext$)
64120   !   let resp$(0)=device$
64140   let fnlbl(2,18,device$)
64160   let fnlbl(4,1,"Select:",16,1)
64180   let fnopt(4,18,"[All]")
64200   if selection_method=1 then let resp$(1)='True' else let resp$(1)='False'
64220   let fnopt(5,18,"An Entire Route")
64240   if selection_method=2 then let resp$(2)='True' else let resp$(2)='False'
64260   let fnopt(6,18,"A Range of Accounts")
64280   if selection_method=3 then let resp$(3)='True' else let resp$(3)='False'
64300   let fnopt(7,18,"Specific Accounts")
64320   if selection_method=4 then let resp$(4)='True' else let resp$(4)='False'
64340   if lrec(2)>0 then 
64360     let fncmdset(19)
64380     let fnlbl(9,1,"Select Finish to initiate link with Hand Held.",46,2)
64400   else 
64410     let fnlbl(9,1,"",46,2)
64420     let fncmdset(2)
64440   end if 
64460   let fnacs(sn$,0,mat resp$,ckey)
64480   if ckey<>5 then 
64520     if resp$(1)='True' then 
64540       let selection_method=1
64560     else if resp$(2)='True' then 
64580       let selection_method=2
64600     else if resp$(3)='True' then 
64620       let selection_method=3
64640     else if resp$(4)='True' then 
64660       let selection_method=4
64680     end if 
64690     let selection_method$=str$(selection_method) : let fncreg_write('hhto.selection_method',selection_method$)
64700   end if 
64720   mat resp$=("")
64740 fnend 
65000 def fn_report_created_file(out_filename_report$*512)
65100   if out_filename_report$<>'' and out_filename_report$<>':CON:' and device$<>'Psion Workabout' and device$<>'LapTop' then 
65120     mat m$(2)
65140     let m$(1)="Hand Held File created:"
65160     let m$(2)=os_filename$(out_filename_report$)
65180     let fnmsgbox(mat m$, response$, cap$,64)
65200   end if 
65240 fnend 
68000 def fn_cnt_of_metered_svcs_active
68020   ! this function should return the number of metered services the customer has that have a non 0 rate code.
68040   if env$('client')='Bethany' then ! the new way
68060     let nomsa_return=0
68080     if a(1)<>0 then let nomsa_return+=1 ! service1  WA
68100     if a(3)<>0 then let nomsa_return+=1 ! service3  EL
68120     if a(4)<>0 then let nomsa_return+=1 ! service4  GA
68140   else ! the old way
68160     let nomsa_return=max(1,d(13))
68180   end if 
68200   let fn_cnt_of_metered_svcs_active=nomsa_return
68220 fnend 
70000 def fn_meter_info$*20(mi_field$,z$*10,servicecode$)
70020   if ~mi_setup then 
70060     let mi_setup=1
70080     dim mi_data$(7)*20
70100     dim mt_data$(5)*40
70120     dim mi_return$*20
70140     open #mi_h_meter:=fngethandle: "Name="&env$('Q')&"\UBmstr\Meter.h"&env$('cno')&",Version=1,KFName="&env$('Q')&"\UBmstr\Meter_Idx.h"&env$('cno')&",Shr",internal,input,keyed  ! let mi_h_meter=fnopen_meter ! open #mi_h_meter:=fngethandle: "Name="&env$('Q')&"\UBmstr\Meter.h"&env$('cno')&",Version=1,KFName="&env$('Q')&"\UBmstr\Meter_Idx.h"&env$('cno')&",Shr",internal,input,keyed
70160     F_METER: form pos 1,c 10,c 2,c 17,c 17,c 12,c 20,c 5
70180     open #mi_h_metertype:=fngethandle: "Name="&env$('Q')&"\UBmstr\MeterType.h"&env$('cno')&",Version=1,KFName="&env$('Q')&"\UBmstr\MeterTypeIdx.h"&env$('cno')&",Shr",internal,input,keyed 
70200     F_METER_TYPE: form pos 1,c 5,c 40,c 9,c 2,c 2
70220   end if  ! ~mi_setup
70240   let mi_return$=''
70260   if z$<>mi_z_prior$ or mi_servicecode_prior$<>servicecode$ then 
70280     let mi_z_prior$=z$ : let mi_servicecode_prior$=servicecode$
70300     mat mi_data$=("")
70320     read #mi_h_meter,using F_METER,key=rpad$(trim$(z$),10)&rpad$(servicecode$,2),release: mat mi_data$ nokey MI_FINIS
70340   end if  ! z$<>mi_z_prior$
70360   let mi_field$=lwrc$(trim$(mi_field$))
70380   if mi_field$='longitude' then 
70400     let mi_return$=rtrm$(mi_data$(3))
70420   else if mi_field$='latitude' then 
70440     let mi_return$=rtrm$(mi_data$(4))
70460   else if mi_field$='meter number' then 
70480     let mi_return$=rtrm$(mi_data$(5))
70500   else if mi_field$='transmitter number' then 
70520     let mi_return$=rtrm$(mi_data$(6))
70540   else if mi_field$='meter type' then 
70560     let mi_return$=rtrm$(mi_data$(7))
70580   else ! it's probably a MeterType field
70600     if mt_key_prior$<>mi_data$(7) then 
70620       let mt_key_prior$=mi_data$(7)
70640       mat mt_data$=("")
70660       read #mi_h_metertype,using F_METER_TYPE,key=rpad$(trim$(mi_data$(7)),kln(mi_h_metertype)): mat mt_data$ nokey MI_FINIS
70680     end if  ! z$<>mi_z_prior$
70720     if mi_field$='reading multipler' then 
70740       let mi_return$=rtrm$(mt_data$(3))
70760     else if mi_field$='number of dials' then 
70780       let mi_return$=rtrm$(mt_data$(4))
70800     else if mi_field$='read type' then 
70820       let mi_return$=rtrm$(mt_data$(5))
70840     end if 
70860   end if 
70880   MI_FINIS: ! 
70900   let fn_meter_info$=mi_return$
70920 fnend  ! fn_meter_info$
72000 def library fnHand_Held_Device_list(mat device$)
72010   if ~setup then let fn_setup
72020   mat device$(0)
72040   fnAddOneC(mat device$,"Boson"           )
72060   fnAddOneC(mat device$,"Itron FC300"     )
72080   fnAddOneC(mat device$,"Sensus"          )
72100   fnAddOneC(mat device$,"Badger"          )
72120   fnAddOneC(mat device$,"ACS Meter Reader")
72130   fnAddOneC(mat device$,"Aclara"          )
72132   fnAddOneC(mat device$,"Master Meter"    )
72134   fnAddOneC(mat device$,"READy Water"    )
72140   ! r: developed but currently unused
72160   ! fnAddOneC(mat device$,"Psion Workabout")
72180   ! fnAddOneC(mat device$,"LapTop"         )
72200   ! fnAddOneC(mat device$,"Green Tree"     )
72220   ! fnAddOneC(mat device$,"Hersey"         )
72240   ! fnAddOneC(mat device$,"EZReader"       )
72260   ! fnAddOneC(mat device$,"AMR"            )
72280   ! fnAddOneC(mat device$,"Unitech HT630"  )
72300   ! /r
72320 fnend
74000 def fn_customerRead(; accountKey$) ! all values read are passed back as local variables
74020   ! #h_customer_i1 and #h_customer_i5 are inherrited local variables
74040   dim extra$(11)*30
74060   crReturn=0
74080   F_CUSTOMER: form pos 1,c 10,4*c 30,pos 143,7*pd 2,pos 1821,n 2,pos 217,15*pd 5,pos 131,c 12,pos 361,2*c 12,pos 1741,n 2,n 7,pos 1864,C 30,7*C 12,3*C 30,pos 1741,n 2,pos 354,c 7
74100   if accountKey$='' then 
74120     read #h_customer_i5,using F_CUSTOMER: z$,mat e$,mat a,final,mat d,mat f$,route,sequence,mat extra$,extra(1),alp$ eof CrEoF
74140   else
74160     read #h_customer_i1,using F_CUSTOMER,key=z$: z$,mat e$,mat a,final,mat d,mat f$,route,sequence,mat extra$,extra(1),alp$ nokey CrNoKey
74180   end if
74200   crReturn=1
74220   goto CrFinis
74240   CrNoKey: ! r:
74260     crReturn=-4272
74280   goto CrFinis ! /r
74300   CrEoF: ! r:
74320     crReturn=-54
74340   goto CrFinis ! /r
74360   CrFinis: !
74380   fn_customerRead=crReturn
74400 fnend

