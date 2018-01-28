02000 ! formerly S:\acsUB\hhto
02010 ! -- Tranfer Data From Computer to Hand Held
02020 fn_setup
02160 fntop(program$)
02330 open #h_customer_i1:=1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed
02340 open #h_customer_i5:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&env$('cno')&",Shr",internal,input,keyed
02368 goto SEL_ACT
03000 def fn_setup
03020   if ~setup then
03040     setup=1
03060     library 'S:\Core\Library': fnerror,fnTos,fnLbl,fncomboa,fnAcs,fncmbrt2,fnxit,fncmbact,fnButton
03080     library 'S:\Core\Library': fncustomer_search,fnFra,fnCmdSet,fntop,fnCmdKey,fnmsgbox,fnTxt
03100     library 'S:\Core\Library': fngethandle,fnpause,fnOpt,fnget_services,fnhand_held_device$
03120     library 'S:\Core\Library': fncreg_read,fncreg_write,fnCopy,fnureg_read,fnureg_write
03140     library 'S:\Core\Library': fnAddOneC
03160     library 'S:\Core\Library': fnMeterAddressLocationID,fncsz,fnmakesurepathexists,fnAccountFromLocationId$
03170     library 'S:\Core\Library': fnOpenFile,fnbuildkey$
03180     on error goto ERTN
03200     ! ______________________________________________________________________
03220     dim resp$(64)*125
03240     dim f$(3)*12,e2$*30
03260     dim z$*10,e$(4)*30,d(15),a(7)
03280     dim res$*41,m$(2)*80
03300     dim serviceName$(10)*20,serviceCode$(10)*2
03320     dim rt$*4,extra(23)
03340     dim filterAccount$(0)
03360     ! r: set mat drive 
03380       dim drive$(22)*3
03400       drive$(1)="E:\"
03420       drive$(2)="F:\"
03440       drive$(3)="G:\"
03460       drive$(4)="H:\"
03480       drive$(5)="I:\"
03500       drive$(6)="J:\"
03520       drive$(7)="K:\"
03540       drive$(8)="L:\"
03560       drive$(9)="M:\"
03580       drive$(10)="N:\"
03600       drive$(11)="O:\"
03620       drive$(12)="P:\"
03640       drive$(13)="Q:\"
03660       drive$(14)="R:\"
03680       drive$(15)="S:\"
03700       drive$(16)="T:\"
03720       drive$(17)="U:\"
03740       drive$(18)="V:\"
03760       drive$(19)="W:\"
03780       drive$(20)="X:\"
03800       drive$(21)="Y:\"
03820       drive$(22)="Z:\"
03840     ! /r
03860     crlf$=chr$(13)&chr$(10)
03880     fnget_services(mat serviceName$, mat serviceCode$)
03900     dim devicePreference$*20
03920     devicePreference$=fnhand_held_device$
03940     dim deviceName$(0)*20,deviceNameCompleteList$(0)*20,deviceNameCompleteListOption$(0)*128
03950     fn_handHeldList(mat deviceNameCompleteList$,mat deviceNameCompleteListOption$)
03952     for dnclItem=1 to udim(mat deviceNameCompleteList$)
03954       if pos(deviceNameCompleteListOption$(dnclItem),'ImportOnly')<=0 then
03956         fnAddOneC(mat deviceName$,deviceNameCompleteList$(dnclItem))
03958       end if
03960     nex dnclItem
03980     dim deviceSelected$*20
04000     if lwrc$(devicePreference$)='[ask]' then
04020       fnureg_read('Hand Held Device Asked',deviceSelected$)
04040       if trim$(deviceSelected$)='' then 
04060         deviceSelected$=deviceName$(1)
04080       end if
04100     else
04120       deviceSelected$=devicePreference$
04140     end if
04160   end if
04180   sm_allExceptFinal=1
04200   sm_aRoute=2
04220   sm_routeRange=3
04240   sm_Individuals=4
04260   sm_LocationId=5
04280   !
04300   meterDataSourceOverrideEnabled=1
04990 fnend
05000 def fn_scr_selact
05020   fncreg_read('hhto.selection_method',selection_method$,'2') : selection_method=val(selection_method$) conv ignore
05040   fnTos(sn$="hhto1")
05060   fnLbl(2,1,"Hand Held model:",16,1)
05080   if lwrc$(devicePreference$)='[ask]' then
05100     fncomboa("HH-FroCBox",2,18,mat deviceName$)
05120     resp$(rc_Device:=respc+=1)=deviceSelected$
05140   else
05160     fnLbl(2,18,deviceSelected$)
05180   end if
05200   fnLbl(4,1,"Select:",16,1)
05220   fnOpt(4,18,"[All] (excluding final billed)")
05240   rc_selectionMethod1:=respc+=1 : if selection_method=sm_allExceptFinal then resp$(rc_selectionMethod1)='True' else resp$(rc_selectionMethod1)='False'
05260   fnOpt(5,18,"An Entire Route")
05280   rc_selectionMethod2:=respc+=1 : if selection_method=sm_aRoute then resp$(rc_selectionMethod2)='True' else resp$(rc_selectionMethod2)='False'
05300   fnOpt(6,18,"A Range of Accounts")
05320   rc_selectionMethod3:=respc+=1 : if selection_method=sm_routeRange then resp$(rc_selectionMethod3)='True' else resp$(rc_selectionMethod3)='False'
05340   fnOpt(7,18,"Specific Accounts")
05360   rc_selectionMethod4:=respc+=1 : if selection_method=sm_Individuals then resp$(rc_selectionMethod4)='True' else resp$(rc_selectionMethod4)='False'
05380   ! if lrec(2)>0 then
05400   !   fnCmdSet(19)
05420   !   fnLbl(9,1,"Select Finish to initiate link with Hand Held.",46,2)
05440   ! else
05460     fnLbl(9,1,"",46,2)
05480     fnCmdSet(2)
05500   ! end if
05520   fnAcs(sn$,0,mat resp$,ckey)
05540   if ckey<>5 then
05560       if lwrc$(devicePreference$)='[ask]' then
05580         deviceSelected$=resp$(rc_Device)
05600         fnureg_write('Hand Held Device Asked',deviceSelected$)
05620       else
05640         deviceSelected$=devicePreference$
05660       end if
05680     if resp$(rc_selectionMethod1)='True' then
05700       selection_method=sm_allExceptFinal
05720     else if resp$(rc_selectionMethod2)='True' then
05740       selection_method=sm_aRoute
05760     else if resp$(rc_selectionMethod3)='True' then
05780       selection_method=sm_routeRange
05800     else if resp$(rc_selectionMethod4)='True' then
05820       selection_method=sm_Individuals
05840     end if
05860     fncreg_write('hhto.selection_method',str$(selection_method))
05880   end if
05900   mat resp$=("")
05920 fnend
08000 SEL_ACT: ! r:
08020 fn_scr_selact
08040 if ckey=5 then 
08060   goto XIT
08080 else if ckey=2 then 
08100   goto Finis
08120 else ! ckey=1
08140   if ~workopen then 
08160     fn_openOutFile ! open work files based on type of Hand Held
08180   end if
08200   if deviceSelected$='Aclara' then
08220     includeFinalBilled=0
08240     selection_method=sm_LocationId ! all Location IDs
08260   end if
08280   if selection_method=sm_allExceptFinal then
08300     goto SELECT_ALL
08320   else if selection_method=sm_aRoute then
08340     goto AskRoute
08360   else if selection_method=sm_routeRange then
08380     goto AskRange
08400   else if selection_method=sm_Individuals then
08420     goto NextAskAccount
08422   else if selection_method=sm_LocationId then
08424     goto NextLocationId
08440   end if
08460 end if  ! /r
08480 ! ______________________________________________________________________
09000 AskRoute: ! r:
09020   fnTos(sn$="AskRoute")
09040   if hbk<>0 then
09060     fnLbl(1,1,"Last Route Number Selected: "&str$(hbk))
09080     myline=3
09100   else
09120     myline=1
09140   end if
09160   fnLbl(myline,1,"Route Number:")
09180   fncmbrt2(myline,22,0)
09200   resp$(1)=""
09220   fnCmdKey("&Next",1,1,0,"Add the selected route" )
09240   fnCmdKey("&Finish",2,0,1,"Completed with all routes")
09260   fnCmdKey("&Cancel",5,0,0,"Don't sent to Hand Held")
09280   fnAcs(sn$,0,mat resp$, ckey)
09300   if resp$(1)="[All]" and ckey=1 then selection_method=sm_allExceptFinal : goto SELECT_ALL ! if they select all on the route screen, handle same as pr all option from 1st menu
09320   bk1=val(resp$(1)) conv L850
09340   resp$(1)=""
09360 L850: !
09380   if ckey=1 then
09400     goto SELECT_ALL
09420   else if ckey=2 then
09440     goto Finis
09460   else if ckey=5 then
09480     goto SEL_ACT
09500   else
09520     goto SELECT_ALL
09540   end if
09560 !
09580 ! /r
09600 NextLocationId: ! r:
09610 !  pr 'readLocationId=';readLocationId : pause ! if readLocationId=118 then pr 'about to do location 119' : pause
09620   nliCustomerReadResponse=fn_customerRead( '',readLocationId+=1)
09622   if final<>0 then ! can not trust accounts to be unique if they are not active.
09624     goto NextLocationId
09626   else if nliCustomerReadResponse=0 then ! no active account found for LocationID
09640     goto NextLocationId
09650   else if nliCustomerReadResponse=-54 then ! end of file
09660     goto END1
09670   end if
09680 goto SendRecordToWorkFile ! /r
10000 SELECT_ALL: ! r:
10020   if deviceSelected$='Aclara Work Order' then
10040     fn_getFilterAccount(mat filterAccount$)
10060   end if
10100   if bk1=0 then bk1=1
10120   restore #h_customer_i5,key>=cnvrt$("pic(zz)",bk1)&"       ": nokey AskRoute
10140 goto NextReadForAll ! /r
11000 NextReadForAll: ! ! r:
11020   if fn_customerRead=-54 then 
11040     goto END1
11060   else if selection_method=sm_aRoute then
11080     if route=0 then 
11100       goto NextReadForAll
11120     else if bk1><route then 
11140       goto END1
11160     end if
11180   end if 
11200   goto SendRecordToWorkFile
11220   !
11240   END1: !
11260   if deviceSelected$='Itron FC300' then 
11280     fn_itron_close
11300   end if
11320   !
11340   if selection_method=sm_allExceptFinal then 
11360     goto Finis
11380   else if selection_method=sm_aRoute then 
11400     hbk=bk1 
11420     goto AskRoute 
11422   else if selection_method=sm_LocationId then
11424     goto Finis
11440   else
11460     goto NextAskAccount 
11480   end if
11500 goto NextAskAccount ! /r
12000 NextAskAccount: ! r:
12020   fnTos(sn$="NextAskAccount")
12040   if z$<>"" then
12060     fnLbl(1,1,"Last Account Selected: "&z$,40,2)
12080     myline=3
12100   else
12120     myline=1
12140   end if
12160   fnLbl(myline,1,"Account:",15,1)
12180   fncmbact(myline,16)
12200   resp$(1)=z$
12220   fnCmdSet(5)
12240   fnAcs(sn$,0,mat resp$,ckey)
12260   if ckey=6 then let fncustomer_search(resp$(1))
12280   if ckey=99 or ckey=5 or resp$(1)="          " then goto SEL_ACT
12300   z$=lpad$(trim$(resp$(1)(1:10)), 10)
12320   if fn_customerRead(z$)=-4272 then goto NextAskAccount
12340   goto SendRecordToWorkFile
12360 ! /r
13000 SendRecordToWorkFile: ! r: doesn't seem to be very well named.
13020   ! if trim$(z$)='100100.99' then pause
13040   if udim(mat filterAccount$)<>0 or final=0 or includeFinalBilled then ! SKIP IF FINAL BILLED
13060     ft$=fn_rmk1$(z$)
13080     if sq1=0 then sq1=1234 ! DEFALT SEQ=W,E,D,G
13100     seq$=str$(sq1)
13120     if deviceSelected$="Aclara" then 
13140       fn_aclara(readLocationId)
13160     else if deviceSelected$="Aclara Work Order" then 
13180       fn_aclaraWorkOrder
13200     else if deviceSelected$="ACS Meter Reader" then 
13220       fn_acs_meter_reader
13240     else if deviceSelected$="AMR" then 
13260       fn_amr
13280     else if deviceSelected$="Badger" then 
13300       fn_badger
13320     else if deviceSelected$="Boson" then 
13340       fn_boson
13360     else if deviceSelected$="EZReader" or deviceSelected$="Green Tree" or deviceSelected$="Hersey" or deviceSelected$="Sensus" then 
13460       fn_legacyMultiDevice
13480     else if deviceSelected$="Itron FC300" then 
13500       fn_itron
13520     else if deviceSelected$="LapTop" then 
13540       fn_laptop
13560     else if deviceSelected$="Master Meter" then 
13580       fn_masterMeter
13600     else if deviceSelected$="Psion Workabout" then 
13620       fn_workabout
13640     else if deviceSelected$="READy Water" then 
13660       fn_READy_Water
13720     else if deviceSelected$="Unitech HT630" then 
13740       fn_unitech_ht630
13760     else
13780       goto SEL_ACT ! go back if Hand Held information is not available for their selection
13800     end if
13820   end if
13850   ! SendRecordToWorkFileFinis: !
13860   if selection_method=sm_allExceptFinal then
13870     goto NextReadForAll
13880   else if selection_method=sm_aRoute then
13890     goto NextReadForAll
13900   else if selection_method=sm_routeRange then
13910     goto NextReadForRange
13920   else if selection_method=sm_Individuals then
13930     goto NextAskAccount
13932   else if selection_method=sm_LocationId then
13934     goto NextLocationId
13940   else
13950     goto NextReadForAll
13960   end if
13970 ! /r
14000 def fn_workabout
14010   dim ft$*20
14020   for j=1 to len(seq$)
14040     on val(seq$(j:j)) goto WORKABOUT_WATER,WORKABOUT_ELECTRIC,WORKABOUT_DEMAND,WORKABOUT_GAS none WORKABOUT_NEXT_SEQUENCE
14060     ! ___________________________
14080     FM_WORKABOUT: form pos 1,c 10,2*c 20,2*n 9,n 1,c 10,c 20
14100     ! ___________________________
14120     WORKABOUT_WATER: !
14140       if a(1)=0 then goto WORKABOUT_NEXT_SEQUENCE
14160       m$=ltrm$(f$(1))(1:10)
14180       pr #h_out,using FM_WORKABOUT: z$,e$(2)(1:16)&" (W)",e$(1)(1:20),d(1),d(3),1,m$,ft$
14200     goto WORKABOUT_NEXT_SEQUENCE
14220     ! ___________________________
14240     WORKABOUT_ELECTRIC: !
14260       if a(3)=0 or trim$(serviceName$(3))<>"Electric" then goto WORKABOUT_LAWNMETER
14280       m$=ltrm$(f$(2))(1:10)
14300       pr #h_out,using FM_WORKABOUT: z$,e$(2)(1:16)&" (E)",e$(1)(1:20),d(5),d(7),3,m$,ft$
14320       WORKABOUT_LAWNMETER: !
14340       if a(3)=0 or trim$(serviceName$(3))<>"Lawn Meter" then goto WORKABOUT_NEXT_SEQUENCE
14360       m$=ltrm$(f$(2))(1:10)
14380       pr #h_out,using FM_WORKABOUT: z$,e$(2)(1:16)&" (L)",e$(1)(1:20),d(5),d(7),3,m$,ft$
14400     goto WORKABOUT_NEXT_SEQUENCE
14420     ! ___________________________
14440     WORKABOUT_DEMAND: !
14460     goto WORKABOUT_NEXT_SEQUENCE
14480     ! ___________________________
14500     WORKABOUT_GAS: !
14520       if a(4)=0 or trim$(serviceName$(4))<>"Gas" then goto WORKABOUT_NEXT_SEQUENCE
14540       m$=ltrm$(f$(3))(1:10)
14560       pr #h_out,using FM_WORKABOUT: z$,e$(2)(1:16)&" (G)",e$(1)(1:20),d(9),d(11),2,m$,ft$
14580     goto WORKABOUT_NEXT_SEQUENCE
14600     ! ___________________________
14620     WORKABOUT_NEXT_SEQUENCE: !
14640   next j
14660 fnend
15000 def fn_laptop
15020   ! LAPTOPWATER: !
15040   if a(1)=0 or trim$(serviceName$(1))<>"Water" then goto LAPTOPELECTRIC !
15060   write #h_out,using "form pos 1,c 10,c 30,c 30,c 1,4*n 9,c 12,c 20": z$,e$(2),e$(1),"W",watread,watusage,d(1),d(3),f$(1),ft$ : goto LAPTOP_XIT
15080   LAPTOPELECTRIC: !
15100   if a(3)=0 or trim$(serviceName$(3))<>"Electric" then goto LAPTOPGAS
15120   write #h_out,using "form pos 1,c 10,c 30,c 30,c 1,4*n 9,c 12,c 20": z$,e$(2),e$(1),"E",elecread,elecusage,d(5),d(8),f$(2),ft$ : goto LAPTOP_XIT
15140   LAPTOPGAS: !
15160   if a(4)=0 or trim$(serviceName$(4))<>"Gas" then goto LAPTOP_XIT
15180   write #h_out,using "form pos 1,c 10,c 30,c 30,c 1,4*n 9,c 12,c 20,n 3,n 7": z$,e$(2),e$(1),"G",gasread,gasusage,d(9),d(12),f$(3),ft$,route,sequence : goto LAPTOP_XIT
15200   LAPTOP_XIT: !
15220 fnend
16000 def fn_badger
16020   for j=1 to len(seq$)
16040     on val(seq$(j:j)) goto BADGER_WATER,BADGER_ELECTRIC,BADGER_DEMAND,BADGER_GAS none BADGER_NEXT_SEQUENCE
16060     BADGER_WATER: !
16080     if a(1)=0 then goto BADGER_NEXT_SEQUENCE
16100     m$=ltrm$(f$(1))(1:10)
16120     if env$('client')="Moweaqua" then manual_or_dialog$=extra$(3)
16140     if env$('client')="Moweaqua" then extra$(3)=f$(1) ! they have meter number in first water meter number and a code in the second number
16160     if env$('client')="Moweaqua" then d(1)=d(1): d(2)=d(2): d(3)=d(3)
16220     rt$=cnvrt$("pic(##)",extra(1))&"  "
16240     if env$('client')='Raymond' then manual_or_dialog$="N"
16260     if env$('client')='Raymond' and trim$(extra$(7))='' then extra$(7)='54'
16280     pr #h_out,using 'Form POS 1,C 8,2*C 20,C 9,C 4,C 1,C 1,C 2,C 2,C 9,C 1,3*PIC(#########),C 8,C 2,C 2,C 4,C 15,C 8,C 1,3*C 6,C 2,PIC(######),C 20,C 30,C 3,C 2,C 2,C 2,C 6,C 18,C 1': "",e$(2)(1:20),e$(1)(1:20),trim$(extra$(3))(1:9),"","A","","1 ","  ","        "," ",d(1)+(d(3)*2),d(1),0,"        ","  ","  ",rt$,z$,"        ",manual_or_dialog$(1:1)," "," "," ",extra$(7)(1:2),sequence," "," "," "," "," "," "," "," ","X"
16300     ! serial # can be extra$(3) rather than f$(1)
16320     ! replaced UPRC$(TRIM$(F$(1)))(1:1) with manual_or_dialog$
16340     goto BADGER_NEXT_SEQUENCE
16360     ! ___________________________
16380     BADGER_ELECTRIC: !
16400     if a(3)=0 or trim$(serviceName$(3))<>"Electric" then goto BADGER_NEXT_SEQUENCE
16420     m$=ltrm$(f$(2))(1:10)
16440     pr #h_out,using 'Form POS 1,C 8,2*C 20,C 9,C 4,C 1,C 1,C 2,C 2,C 9,C 1,3*PIC(#########),C 8,C 2,C 2,C 4,C 15,C 8,C 1,3*C 6,C 2,PIC(######),C 20,C 30,C 3,C 2,C 2,C 2,C 6,C 18,C 1': " ",e$(2)(1:20),e$(1)(1:20),trim$(extra$(3))(1:9)," ","A"," ","3 "," ",f$(2)(1:9)," ",d(5)+(d(7)*1.5),d(5),0," "," "," "," ",z$," ",uprc$(trim$(f1$))(1:1)," "," "," ",extra$(7)(1:2),sequence," "," "," "," "," "," "," "," ","X"
16460     L2010: form pos 1,c 8,2*c 20,c 9,c 4,c 1,c 1,c 2,c 2,c 9,c 1,3*pic(#########),c 8,c 2,c 2,c 4,c 15,c 8,c 1,3*c 6,c 2,pic(######),c 20,c 30,c 3,c 2,c 2,c 2,c 6,c 18,c 1
16480     goto BADGER_NEXT_SEQUENCE
16500     ! ___________________________
16520     BADGER_DEMAND: !
16540     goto BADGER_NEXT_SEQUENCE
16560     m$=""
16580     pr #h_out,using L2010: " ",e$(2)(1:20),e$(1)(1:20),trim$(extra$(3))(1:9)," ","A"," ","4 "," ",f$(2)(1:9)," ",d(15)+(d(15)*.5),d(15)-(d(15)*.5),0," "," "," "," ",z$," ",manual_or_dialog$," "," "," ",extra$(7)(1:2),sequence," "," "," "," "," "," "," "," ","X"
16600     goto BADGER_NEXT_SEQUENCE
16620     ! ___________________________
16640     BADGER_GAS: !
16660     if a(4)=0 or trim$(serviceName$(4))<>"Gas" then goto BADGER_NEXT_SEQUENCE
16680     m$=ltrm$(f$(3))(1:10)
16700     pr #h_out,using L2010: " ",e$(2)(1:20),e$(1)(1:20),trim$(extra$(3))(1:9)," ","A"," ","2 "," ",f$(2)(1:9)," ",d(9)+(d(11)*1.5),d(9),0," "," "," "," ",z$," ","D"," "," "," ",extra$(7)(1:2),sequence," "," "," "," "," "," "," "," ","X"
16720     goto BADGER_NEXT_SEQUENCE
16740     ! ___________________________
16760     BADGER_NEXT_SEQUENCE: !
16780   next j
16800 fnend
18000 def fn_legacyMultiDevice
18020   ! r: set cd$ - included in several records - maybe some sort of meter id - not sure
18040   cd$="M"
18060   if env$('client')="Oakland" or env$('client')="Lovington" then
18080     if trim$(extra$(7))="1" then 
18100       cd$="B"
18120     end if
18140   end if
18160   ! /r
18180   ! r: make c$ - a legacy customer service list for the following loop to walk through
18200   c$=""
18220   if a(1)>0 then c$="1"
18240   if a(3)=5 then c$=c$&"5" else if a(3)>0 then c$=c$&"3"
18260   if a(4)>0 then c$=c$&"4"
18280   ! /r
18300   if rtrm$(f$)="" then f$=z$
18320   for j=1 to len(c$)
18340     if deviceSelected$="Green Tree" then
18360       if val(c$(j:j))=1 then ! Water
18380         pr #h_out,using 'form pos 1,c 10,2*c 20,2*n 9,n 1,c 10,c 1': z$,e$(2)(1:18)&"-W",e$(1)(1:20),d(1),d(3),1,extra$(3)(1:10),cd$
18400       end if
18420     else if deviceSelected$="Hersey" then
18440       if val(c$(j:j))=1 then ! Water
18460         pr #h_out,using 'form pos 1,c 10,c 4,c 6,c 1,c 25,c 21,c 20,c 1,n 10,n 10,c 100,c 2,c 1,c 5,c 12,c 52,pos 281,c 2': z$," "," ","W",e$(2)(1:25),e$(1)(1:21),f$(1),"V",d(1)+(d(3)*2),d(1)," "," "," "," ",z$," ",chr$(13)&chr$(10)
18480       end if
18500     else if deviceSelected$="EZReader" then
18520       if val(c$(j:j))=1 then ! Water
18540         pr #h_out,using 'form pos 1,c 12,c 2,c 1,c 66,c 64,c 14,c 1,2*pic(##########),pic(##),c 120,c 24,c 24,c 20,c 80,c 125,c 1,c 2': cnvrt$("pic(##)",route)&cnvrt$("pic(#######)",sequence),"  ","W",e$(2),e$(1),f$(1),extra$(3)(1:1),d(1)+(d(3)*2),d(1),0," "," "," ",z$," "," ","X",chr$(13)&chr$(10)
18560       end if
18580     else if deviceSelected$="Sensus" then 
18590       L2520: form pos 1,c 10,2*c 20,2*n 9,n 1,c 10,c 1,n 9
18600       if val(c$(j:j))=1 then ! Water
18620         pr #h_out,using L2520: z$,e$(2)(1:18)&"-W",e$(1)(1:20),d(1),d(3),1,extra$(3)(1:10),cd$
18640       else if val(c$(j:j))=3 then ! Electric
18660         if d(14)<>0 then d(7)=d(7)/(d(14)*.01) ! COMPARE USAGE BEFORE MULTIPLIER
18680         pr #h_out,using L2520: z$,e$(2)(1:18)&"-E",e$(1)(1:20),d(5),d(7),3,extra$(3)(1:10),cd$
18700       else if val(c$(j:j))=5 then ! Demand
18720         pr #h_out,using L2520: z$,e$(2)(1:18)&"-D",e$(1)(1:20),d(15),d(7),4,extra$(3)(1:9)&"D",cd$
18740       end if
18760     end if
18780   next j
18800 fnend
19000 def fn_amr ! AMR software solutions  ! same as ezreader, but specifically for Albany (who no longer uses ACS UB)
19010   if header=0 then
19020     if alp$(1:1)<>"*" then
19030       header=1 ! create header record
19040       if bk1>0 then route=bk1 else route=1 ! if they selected all for route number, make route number =1 else use the actual route number
19050       pr #h_out,using "form pos 1,c 2,pic(##),pic(######),c 2": "R1",1,route,crlf$ 
19060     end if
19070   end if
19080   ! AMR Water
19090   pr #h_out,using L3230: "M1", lpad$(rtrm$(z$),20),f$(1)(1:10),extra(2),"W",d(1)+(d(3)*2),d(1)+(d(3)*.50),"    ","    ","    ",e$(1),e$(2)(1:20),d(1),extra(8),0,0,0,0,0,0,0,0,0,0,0,0,crlf$
19100   L3230: form pos 1,c 2,c 20,c 10,pic(######),c 4,2*pic(##########),3*c 4,c 40,c 20,pic(##########),n 4,pic(##),pic(#),2*pic(##########),2*pic(############),5*pic(##########),pic(########),c 2
19110 fnend
22000 def fn_searchScreen(x$,&res$)
22020   fncustomer_search(x$)
22040   if x$<>"" then
22060     read #h_customer_i1,using "Form POS 1,C 10,x 30,c 30",key=x$: z$,e2$
22080     res$=rpad$(trim$(z$),10)&" "&trim$(e2$)
22100   end if
22120 fnend
23000 AskRange: ! r:
23020   fnTos(sn$:="AskRange")
23040   fnFra(1,1,1,57,"Starting Account:")
23060   fnFra(4,1,1,57,"Ending Account:")
23080   fncmbact(1,1,0,1)
23120   fnButton(1,48,"Search",6,blank$,0,7,1)
23140   resp$(1)=resp$(2)=""
23160   fncmbact(1,1,0,2)
23180   fnButton(1,48,"Search",7,blank$,0,7,2)
23200   fnCmdKey("&Finish",2,1,0,"Completed with all routes")
23220   fnCmdSet(2)
23240   fnAcs(sn$,0,mat resp$,ckey)
23260   bk1$=lpad$(trim$(resp$(1)(1:10)), 10)
23280   bk2$=lpad$(trim$(resp$(2)(1:10)), 10)
23300   if ckey=2 then goto Finis
23320   if ckey=99 or ckey=5 then mat resp$=(""): goto SEL_ACT
23340   if ckey=6 then
23360     fn_searchScreen(x$,resp$(1))
23380     goto AskRange
23400   else if ckey=7 then
23420     fn_searchScreen(x$,resp$(2))
23440     goto AskRange
23460   end if
23480   mat resp$=("")
23500   ! read #h_customer_i1,using F_CUSTOMER,key=bk1$,release: z$,mat e$,mat a,final,mat d,mat f$,route,sequence,extra$(3),extra$(7),extra(1),alp$ eof AskRange ! get first and last route and sequence number to select
23520   ! read #h_customer_i1,using F_CUSTOMER,key=bk2$,release: z$,mat e$,mat a,final,mat d,mat f$,last_route,last_sequence,extra$(3),extra$(7),extra(1),alp$ eof AskRange
23540   read #h_customer_i1,using 'form pos 1741,n 2,n 7',key=bk1$,release: route,sequence ! get first and last route and sequence number to select
23560   read #h_customer_i1,using 'form pos 1741,n 2,n 7',key=bk2$,release: last_route,last_sequence
23580   restore #h_customer_i5,key=cnvrt$("pic(zz)",route)&cnvrt$("pic(zzzzzzz",sequence):
23600   NextReadForRange: !
23620   if fn_customerRead=-54 then goto AskRange
23640   ! If (ROUTE=LAST_ROUTE AND SEQUENCE>LAST_SEQUENCE) OR ROUTE>LAST_ROUTE Then Goto AskRange
23660   if trim$(z$)<trim$(bk1$) or trim$(z$)>trim$(bk2$) then goto AskRange
23680 goto SendRecordToWorkFile ! /r
23700 !
24000 ! <Updateable Region: ERTN>
24020 ERTN: fnerror(program$,err,line,act$,"xit")
24040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
24060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
24080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
24100 ERTN_EXEC_ACT: execute act$ : goto ERTN
24120 ! /region
25000 def fn_rmk1$*20(z$)
25020   ! read the footnote from the note file  (any note with * as first character
25040   dim rm$*1320
25060   dim notefile$*256
25080   ft$="                    "
25100   notefile$=env$('Q')&'\UBmstr\notes.h'&env$('cno')&'\'&trim$(z$)&'.txt'
25120   if exists(notefile$) then 
25140     open #20: "Name="&notefile$,display,input ioerr Rmk1_Finis
25160     do  
25180       linput #20: rm$ eof Rmk1_Finis
25200       if rm$(1:1)="*" then 
25220         ft$=rpad$(rm$(2:21),20)
25240       end if
25260     loop until rm$(1:1)="*"
25280   end if
25300   Rmk1_Finis: !
25320   close #20: ioerr ignore
25340 fnend
26000 def fn_openOutFile ! open work areas based on type of Hand Held
26020   dim out_filename$*256
26040   fnureg_read('Hand Held To File',out_filename$,'C:\mvrs\xfer\Download\Download.dat')
26060   if deviceSelected$='Itron FC300' then
26080     fn_itron_open ! default
26100   else
26120     h_out                  =fn_ifMatchOpenDo("Sensus",           "C:\vol002\amrs\READINGS.DAT",                                        80)
26140     if h_out<=0 then h_out=fn_ifMatchOpenDo("Green Tree",       "C:\READINGS.DAT",                                                    80)
26160     if h_out<=0 then h_out=fn_ifMatchOpenDo("Badger",           "C:\CONNECT\CONNECT.IN3",                                            256)
26180     if h_out<=0 then h_out=fn_ifMatchOpenDo("Boson",            env$('Q')&"\UBmstr\intopalm.txt",                                   204)
26200     if h_out<=0 then h_out=fn_ifMatchOpenDo("LapTop",           env$('Q')&"\UBmstr\Laptop.Out",                                     200)
26220     if h_out<=0 then h_out=fn_ifMatchOpenDo("AMR",              "C:\ezreader\download.dat",                                          256)
26240     if h_out<=0 then h_out=fn_ifMatchOpenDo("Hersey",           env$('Q')&"\UBmstr\READINGS.DAT",                                   282,',eol=none')
26260     if h_out<=0 then h_out=fn_ifMatchOpenDo("EZReader",         "c:\ezreader\Download.dat",                                          578,',eol=none')
26280     if h_out<=0 then h_out=fn_ifMatchOpenDo("Unitech HT630",    env$('temp')&'\'&session$&'_uni_ht630.dat',                         256)
26300     if h_out<=0 then h_out=fn_ifMatchOpenDo("Unitech HT630",    env$('temp')&'\'&session$&'_uni_ht630.dat',                         256, ',eol=none')
26320     if h_out<=0 then h_out=fn_ifMatchOpenDo("ACS Meter Reader", env$('temp')&'\'&session$&'_acs_meter_data.txt',                    256)
26340     if h_out<=0 then h_out=fn_ifMatchOpenDo("Psion Workabout",  env$('Q')&"\UBmstr\Readings.dat",                                   128)
26360     if h_out<=0 then h_out=fn_ifMatchOpenDo("Aclara Work Order",br_filename$(env$('userprofile')&'\Desktop\Aclara Work Order.txt'),1048)
26380     if h_out<=0 then h_out=fn_ifMatchOpenDo('',                 br_filename$(env$('userprofile')&'\Desktop\ACS Hand Held Out.txt'),1048)
26440   end if
26460   workopen=1
26480 fnend
27000 def fn_ifMatchOpenDo(deviceTest$*40,defaultOut_filename$*256,recordLength; extraParameter$*256)
27020   ! inherrits deviceSelected$,out_filename$
27040   ! returns open file handle
27060   if deviceTest$='' or deviceSelected$=deviceTest$ then
27080     if out_filename$='' then out_filename$=defaultOut_filename$
27100     fnmakesurepathexists(out_filename$)
27120     open #hImodoReturn:=fngethandle: 'Name='&env$('at')&out_filename$&',RecL='&str$(recordLength)&extraParameter$&',Replace',display,output
27140   end if
27160   fn_ifMatchOpenDo=hImodoReturn
27180 fnend
28000 def fn_unitech_ht630
28020   ! INPUT FILE (from ACS to Hand Held) needs to contain the following fields:
28040   !   Account - 10 characters
28060   !   Route and Sequence - 12 digits (this is the order for accounts to be displayed in - it might contain duplicates and/or skip large ranges of numbers)
28080   !   Meter Type - 10 characters - "Gas", "Water", "Electric", etc.  Each house may have multiple meters that need to be read.  If a house has both gas and water than it would have two records in the file so that both can be ask.  The Meter Type will need to be displayed so the user will know which they should be entering.
28100   !   Customer Name - 40 characters - The name of the customer who's meter is being read.  This should be displayed when the reading is ask for.
28120   !   Meter Address - 40 characters - The address of the customer who's meter is being read.
28140   !   This should be displayed when the reading is ask for.
28160   !   Reading High - 10 digits - used to validate entry of new reading
28180   !   Reading Low - 10 digits - used to validate entry of new reading
28200   for a_item=1 to udim(mat a)
28220     if serviceCode$(a_item)='WA' or serviceCode$(a_item)='GA' or serviceCode$(a_item)='EL' then ! or (demand)   it is a metered service
28240       if a(a_item)>0 then
28260         if serviceCode$(a_item)='WA' then
28280           usage_current=d(3) ! Water usage - current
28300           reading_current=d(1)
28320         else if serviceCode$(a_item)='GA' then
28340           usage_current=d(11) ! Gas usage - curent
28360           reading_current=d(9)
28380         else ! if serviceCode$(a_item)='EL' then
28400           pr 'developer note: add code to copy '&serviceName$(a__item)&' usage current and reading current from mat d into usage_current and reading_current' : fnpause
28420         end if
28440         unusual_usage_low=round(reading_current+usage_current*fn_pcent,2)
28460         unusual_usage_high=round(reading_current+usage_current+usage_current*fn_pcent,2)
28480         pr #h_out,using FORM_UH_OUT: z$,route*100000000+sequence,serviceName$(a_item)(1:10),e$(2),e$(1)(1:20),unusual_usage_low,unusual_usage_high
28500         FORM_UH_OUT: form pos 1,c 10,n 12,c 10,2*c 40,2*n 10
28520       end if  ! a(a_item)>0
28540     end if  ! it is a metered service
28560   next a_item
28580 fnend  ! fn_Unitech_HT630
29000 def fn_acs_meter_reader
29020   ! FILE (from ACS to Hand Held and from Hand Held to ACS) needs to contain the following fields:
29040   !   Account - 10 characters
29060   !   Route and Sequence - 12 digits (this is the order for accounts to be displayed in - it might contain duplicates and/or skip large ranges of numbers)
29080   !   Meter Type - 10 characters - "Gas", "Water", "Electric", etc.  Each house may have multiple meters that need to be read.  If a house has both gas and water than it would have two records in the file so that both can be ask.  The Meter Type will need to be displayed so the user will know which they should be entering.
29100   !   Customer Name - 40 characters - The name of the customer who's meter is being read.  This should be displayed when the reading is ask for.
29120   !   Meter Address - 40 characters - The address of the customer who's meter is being read.
29140   !   This should be displayed when the reading is ask for.
29160   !   Reading High - 10 digits - used to validate entry of new reading
29180   !   Reading Low - 10 digits - used to validate entry of new reading
29200   !   Reading - 10 digits - the new reading
29220   for a_item=1 to udim(mat a)
29240     if serviceCode$(a_item)='WA' or serviceCode$(a_item)='GA' or serviceCode$(a_item)='EL' then ! or (demand)   it is a metered service
29260       if a(a_item)>0 then
29280         if serviceCode$(a_item)='WA' then
29300           usage_current=d(3) ! Water usage - current
29320           reading_current=d(1)
29340         else if serviceCode$(a_item)='GA' then
29360           usage_current=d(11) ! Gas usage - curent
29380           reading_current=d(9)
29400         else ! if serviceCode$(a_item)='EL' then
29420           pr 'developer note: add code to copy '&serviceName$(a__item)&' usage current and reading current from mat d into usage_current and reading_current' : fnpause
29440         end if
29460         unusual_usage_low=round(reading_current+usage_current*fn_pcent,2)
29480         unusual_usage_high=round(reading_current+usage_current+usage_current*fn_pcent,2)
29500         pr #h_out,using FORM_ACSMR: z$,route*100000000+sequence,serviceName$(a_item)(1:10),e$(2),e$(1)(1:20),unusual_usage_low,unusual_usage_high,0
29520         FORM_ACSMR: form pos 1,c 10,n 12,c 10,2*c 40,2*n 10,n 10
29540       end if  ! a(a_item)>0
29560     end if  ! it is a metered service
29580   next a_item
29600 fnend  ! fn_acs_meter_reader
30000 def fn_pcent
30020   if ~pcent_setup then
30040     pcent_setup=1
30060     open #h_company:=fngethandle: "Name="&env$('Q')&"\UBmstr\Company.h"&env$('cno'),internal,input
30080     read #h_company,using "Form POS 130,n 4": pcent_return
30100     close #h_company:
30120     if pcent_return=0 then pcent_return=100
30140     pcent_return=pcent_return*.01 ! convert to percent
30160   end if  ! ~pcent_setup
30180   fn_pcent=pcent_return
30200 fnend  ! fn_pcent
32000 ! r: itron
32020 def fn_itron_open
32040   open #h_out:=fngethandle: "Name="&env$('Q')&"\HH"&ssession$&".int,RecL=128,EoL=None,Replace",internal,outIn,relative
32060   fn_itron_record_fhd
32080   itron_rdg_count=0
32100   itron_cus_count=0
32120   itron_mtr_count=0
32140   itron_rtr_count=0
32160   itron_chd_count=0
32180 fnend
32200 def fn_itron_close
32220   fn_itron_route_trailer
32240   fn_itron_record_ftr
32260   !
32280   rec_current=0 ! restore #h_out:
32300   do
32320     rec_current+=1
32340     if rec_current>lrec(h_out) then goto IC_EOF_1
32360     read #h_out,using 'form pos 1,C 126',rec=rec_current: rec_line$
32380     rec_type$=rec_line$(1:3)
32400     if rec_type$='RHD' then ! route header
32420       ! itron_rhd_current=rec(h_out)
32440     else if rec_type$='RTR' then ! route trailer
32460       itron_rtr_current=rec(h_out)
32480       rewrite #h_out,using 'form pos 18,n 4,pos 34,N 4,N 4,N 4,Pos 52,3*N 4',rec=itron_rtr_current: itron_rdg_count,itron_rff_count,itron_cus_count,itron_mtr_count,itron_mtr_g_count,itron_mtr_w_count,itron_mtr_e_count noRec ignore
32500       itron_rdg_count=0
32520       itron_rdg_count=0
32540       itron_rff_count=0
32560       itron_cus_count=0
32580       itron_mtr_count=0
32600       itron_mtr_e_count=0
32620       itron_mtr_g_count=0
32640       itron_mtr_i_count=0
32660       itron_mtr_s_count=0
32680       itron_mtr_w_count=0
32700     else if rec_type$='RFF' then
32720       itron_rff_count+=1
32740     else if rec_type$='CUS' then
32760       itron_cus_count+=1
32780     else if rec_type$='MTR' then
32800       itron_mtr_count+=1
32820       itron_meter_category$=rec_line$(102:102)
32840       if itron_meter_category$="E" then
32860         itron_mtr_e_count+=1
32880       else if itron_meter_category$="G" then
32900         itron_mtr_g_count+=1
32920       else if itron_meter_category$="I" then
32940         itron_mtr_i_count+=1
32960       else if itron_meter_category$="S" then
32980         itron_mtr_s_count+=1
33000       else if itron_meter_category$="W" then
33020         itron_mtr_w_count+=1
33040       end if
33060     else if rec_type$='RDG' then
33080       itron_rdg_count+=1
33100     else if rec_type$='FTR' then
33120       itron_ftr_current=rec(h_out)
33140       rewrite #h_out,using 'form pos 14,n 2',rec=itron_fhd_current: itron_chd_count
33160       rewrite #h_out,using 'form pos 14,n 2',rec=itron_ftr_current: itron_chd_count
33180       itron_chd_count=0
33200     else if rec_type$='FHD' then
33220       itron_fhd_current=rec(h_out)
33240     else if rec_type$='CHD' then
33260       itron_chd_count+=1
33280     end if  ! rec_type$=...
33300   loop
33320   IC_EOF_1: !
33340   !
33360   open #h_out2:=fngethandle: "Name="&env$('Q')&"\Download.dat,RecL=128,EoL=None,Replace",display,output
33380   restore #h_out:
33400   do
33420     read #h_out,using 'form pos 1,C 126': rec_line$ eof IC_EOF_2
33440     pr #h_out2,using 'form pos 1,C 126,c 2': rec_line$,crlf$
33460   loop
33480   IC_EOF_2: !
33500   close #h_out2:
33520   close #h_out,free:
33540   fnmakesurepathexists(env$('at')&out_filename$)
33560   fnCopy(env$('Q')&"\Download.dat",env$('at')&out_filename$)
33580   fn_report_created_file(out_filename$)
33600   !   if exists ("C:\MVRS\MVRSWin5.exe") then
33620   !     if ~exists ("C:\MVRS\MVRSWin5.cmd") then
33640   !       open #h_tmp:=fngethandle: 'Name=C:\MVRS\MVRSWin5.cmd,RecL=256,replace',display,output
33660   !       pr #h_tmp: 'c:'
33680   !       pr #h_tmp: 'cd \MVRS'
33700   !       pr #h_tmp: 'C:\MVRS\MVRSWin5.exe'
33720   !       close #h_tmp:
33740   !     end if
33760   !     execute 'Sy -c C:\MVRS\MVRSWin5.cmd'
33780   !   end if
33800 fnend
33820 def fn_itron_route_trailer
33840   fn_itron_record_rtr
33860   fn_itron_record_ctr
33880   itron_rtr_count+=1
33900 fnend  ! fn_itron_route_trailer
33920 def fn_itron
33940   for a_item=1 to udim(mat a)
33960     if serviceCode$(a_item)='WA' or serviceCode$(a_item)='GA' or serviceCode$(a_item)='EL' then ! or (demand)   it is a metered service
33980       if a(a_item)>0 then
34000         if serviceCode$(a_item)='WA' then
34020           usage_current=d(3) ! Water usage - current
34040           reading_current=d(1)
34060         else if serviceCode$(a_item)='GA' then
34080           usage_current=d(11) ! Gas usage - curent
34100           reading_current=d(9)
34120         else if serviceCode$(a_item)='EL' then
34140           usage_current=d(7) ! KWH usage - curent
34160           reading_current=d(5)
34180         else ! if serviceCode$(a_item)='EL' then
34200           pr 'developer note: add code to copy '&serviceName$(a__item)&' usage current and reading current from mat d into usage_current and reading_current' : fnpause
34220         end if
34240         unusual_usage_low=int(usage_current-usage_current*fn_pcent) : if unusual_usage_low<0 then unusual_usage_low=0
34260         unusual_usage_high=int(usage_current+usage_current*fn_pcent)
34280         if z$<>z_prior$ then
34300           z_prior$=z$
34320           if route<>route_prior then
34340             if route_prior<>0 then
34360               fn_itron_route_trailer
34380             end if  ! route_prior<>0
34400             route_prior=route
34420             fn_itron_record_chd
34440             route_itron$=cnvrt$('pic(##)',route)&cnvrt$('pic(######)',route)
34460             fn_itron_record_rhd
34480           end if  ! route<>route_prior
34500           fn_itron_record_cus
34520         end if  ! z$<>z_prior$
34540         fn_itron_record_mtr
34560         fn_itron_record_mtx
34580         unusual_usage_low=int(usage_current-usage_current*fn_pcent) : if unusual_usage_low<0 then unusual_usage_low=0
34600         unusual_usage_high=int(usage_current+usage_current*fn_pcent)
34620         fn_itron_record_rdg
34640         fn_itron_record_rff
34660       end if  ! a(a_item)>0
34680     end if  ! it is a metered service
34700   next a_item
34720 fnend
34740 def fn_itron_record_rdg ! reading - pg 19
34760   fn_record_init
34780   fn_record_addc(3,'RDG')
34800   fn_record_addc(8,route_itron$)
34820   fn_record_addc(4,serviceCode$(a_item))
34840   fn_record_addc(1,'Y')
34860   fn_record_addc(1,'L') ! field 5
34880   fn_record_addn(3,0)
34900   fn_record_addn(3,0)
34920   fn_record_addx(1)
34940   fn_record_addn(2,0)
34960   !
34980   itron_number_of_dials=val(fn_meterInfo$('Number of Dials',z$,serviceCode$(a_item)))
34981   ! pr z$&' '&serviceCode$(a_item)&' itron_number_of_dials=';itron_number_of_dials : prdebugcount+=1 : if prdebugcount/24=int(prdebugcount/24) then pause
34982   ! if trim$(z$)='200030.00' then pr 'itron_number_of_dials=';itron_number_of_dials : pause
35000   if itron_number_of_dials=0 then itron_number_of_dials=6
35020   fn_record_addn(2,itron_number_of_dials) ! field 10  -  number of dials
35040   fn_record_addn(2,0) !
35050   dim transmitter_number$*128
35060   transmitter_number$=fn_meterInfo$('transmitter number',z$,serviceCode$(a_item))
35080   if transmitter_number$<>'' then let fn_record_addc(1,'R') else let fn_record_addc(1,'K') : skip_next_rff_record=1
35100   fn_record_addn(10,reading_current)
35120   fn_record_addn(10,unusual_usage_high)
35140   fn_record_addn(10,unusual_usage_low) ! field 15
35160   fn_record_addn(6,0)
35180   fn_record_addn(1,0)
35200   fn_record_addn(1,0)
35220   fn_record_addn(5,0)
35240   fn_record_addn(1,0) ! field 20
35260   fn_record_addx(1)
35280   !
35300   itron_read_type=val(fn_meterInfo$('Read Type',z$,serviceCode$(a_item)))
35320   if itron_read_type=0 then itron_read_type=a_item ! gas, water, electric a unique number for each - a_item (service number) is as good as any
35340   fn_record_addc(2,cnvrt$('pic(##)',itron_read_type))
35360   fn_record_addn(6,0)
35380   fn_record_addn(6,0)
35400   fn_record_addn(5,0) ! field 25
35420   fn_record_addx(31)
35440   ! fn_record_addc(2,crlf$)
35460   fn_record_write(h_out)
35480 fnend  ! fn_itron_record_rdg
35500 def fn_itron_record_rhd ! route header - pg 6
35520   fn_record_init
35540   fn_record_addc(3,'RHD')
35560   fn_record_addc(8,route_itron$)
35580   fn_record_addc(1,'N')
35600   fn_record_addc(1,'N')
35620   fn_record_addn(4,0) ! field 5 - total number of keys
35640   fn_record_addn(4,0) ! field 6 - total number of reading records
35660   fn_record_addn(4,0) ! field 7 - total number of demand meters
35680   fn_record_addn(4,0) ! field 8 - total number of keyed readings
35700   fn_record_addn(4,0) ! field 9 - total number of optical probe readings
35720   fn_record_addn(4,0) ! field 10 - total number of off-site (Radio) readings
35740   fn_record_addn(4,0) ! field 11 - total number of customer records
35760   fn_record_addn(4,0) ! field 12 - total number of meter records
35780   fn_record_addn(6,0)
35800   fn_record_addn(4,0)
35820   fn_record_addn(4,0) ! field 15
35840   fn_record_addn(4,0)
35860   fn_record_addn(4,0)
35880   fn_record_addn(4,0)
35900   fn_record_addc(2,'')
35920   fn_record_addc(2,'') ! field 20 - zone
35940   fn_record_addc(2,'')
35960   fn_record_addn(2,0)
35980   fn_record_addn(2,0)
36000   fn_record_addn(4,0)
36020   fn_record_addc(1,'') ! field 25
36040   fn_record_addx(40)
36060   ! fn_record_addc(2,crlf$)
36080   fn_record_write(h_out)
36100 fnend  ! fn_itron_record_rhd
36120 def fn_itron_record_rtr ! route trailer - pg 6
36140   fn_record_init
36160   fn_record_addc(3,'RTR')
36180   fn_record_addc(8,route_itron$)
36200   fn_record_addc(1,'N')
36220   fn_record_addc(1,'N')
36240   fn_record_addn(4,0) ! field 5 - total number of keys
36260   fn_record_addn(4,0) ! field 6 - total number of reading records
36280   fn_record_addn(4,0) ! field 7 - total number of demand meters
36300   fn_record_addn(4,0) ! field 8 - total number of keyed readings
36320   fn_record_addn(4,0) ! field 9 - total number of optical probe readings
36340   fn_record_addn(4,0) ! field 10 - total number of off-site (Radio) readings
36360   fn_record_addn(4,0) ! field 11 - total number of customer records
36380   fn_record_addn(4,0) ! field 12 - total number of meter records
36400   fn_record_addn(6,0)
36420   fn_record_addn(4,0)
36440   fn_record_addn(4,0) ! field 15
36460   fn_record_addn(4,0)
36480   fn_record_addn(4,0)
36500   fn_record_addn(4,0)
36520   fn_record_addc(2,'')
36540   fn_record_addc(2,'') ! field 20 - zone
36560   fn_record_addc(2,'')
36580   fn_record_addn(2,0)
36600   fn_record_addn(2,0)
36620   fn_record_addn(4,0)
36640   fn_record_addc(1,'') ! field 25
36660   fn_record_addx(40)
36680       ! fn_record_addc(2,crlf$)
36700   fn_record_write(h_out)
36720 fnend  ! fn_itron_record_rtr
36740 def fn_itron_record_cus ! Customer - pg 11
36760   fn_record_init
36780   fn_record_addc(3,'CUS')
36800   fn_record_addc(8,route_itron$)
36820   fn_record_addn(3,fn_cnt_of_metered_svcs_active)
36840   fn_record_addc(20,z$)
36860   fn_record_addc(20,e$(2)) ! field 5 - name
36880   fn_record_addc(20,e$(1))
36900   ! fn_record_addc(6,'')
36920   ! fn_record_addc(14,'')
36940   fn_record_addc(20,'')
36960   fn_record_addx(2)
36980   fn_record_addn(1,0)
37000   fn_record_addc(20,'') ! field 10 - Customer Information
37020   fn_record_addc(1,'N')
37040   fn_record_addc(4,'')
37060   fn_record_addc(2,'')
37080   fn_record_addc(1,'')
37100   fn_record_addx(1) ! field 15
37120   ! fn_record_addc(2,crlf$)
37140   fn_record_write(h_out)
37160 fnend  ! fn_itron_record_cus
37180 def fn_itron_record_mtx ! latitude, longitude, etc - pg 16
37200   fn_record_init
37220   fn_record_addc(3,'MTX')
37240   fn_record_addc(8,route_itron$)
37260   fn_record_addc(12,fn_meterInfo$('meter number',z$,serviceCode$(a_item)))
37280   dim irm_tmp$*20
37300   irm_tmp$=lwrc$(fn_meterInfo$('longitude',z$,serviceCode$(a_item)))
37320   if irm_tmp$(1:1)="n" or irm_tmp$(1:1)="s" or irm_tmp$(1:1)="e" or irm_tmp$(1:1)="w" then irm_tmp$=str$(fn_dms_to_dec(irm_tmp$))
37340   fn_record_addc(17,irm_tmp$)
37360   irm_tmp$=lwrc$(fn_meterInfo$('latitude',z$,serviceCode$(a_item)))
37380   if irm_tmp$(1:1)="n" or irm_tmp$(1:1)="s" or irm_tmp$(1:1)="e" or irm_tmp$(1:1)="w" then irm_tmp$=str$(fn_dms_to_dec(irm_tmp$))
37400   fn_record_addc(17,irm_tmp$)
37420   fn_record_addc(12,'')
37440   fn_record_addx(57)
37460   ! fn_record_addc(2,crlf$)
37480   fn_record_write(h_out)
37500 fnend  ! fn_itron_record_mtx
37520 def fn_dms_to_dec(dtd_in$*20) ! for longitude and latitude
37540 ! N31 35 47.8
37560   if dtd_in$(1:1)="n" then dtd_sign$='+' : dtd_in$(1:1)=''
37580   if dtd_in$(1:1)="e" then dtd_sign$='+' : dtd_in$(1:1)=''
37600   if dtd_in$(1:1)="s" then dtd_sign$='-' : dtd_in$(1:1)=''
37620   if dtd_in$(1:1)="w" then dtd_sign$='-' : dtd_in$(1:1)=''
37640   !
37660   dtd_pos_space=pos(dtd_in$,' ')
37680   dtd_degrees=val(dtd_in$(1:dtd_pos_space))
37700   dtd_in$(1:dtd_pos_space)=''
37720   !
37740   dtd_pos_space=pos(dtd_in$,' ')
37760   dtd_minutes=val(dtd_in$(1:dtd_pos_space))
37780   dtd_in$(1:dtd_pos_space)=''
37800   !
37820   dtd_seconds=val(dtd_in$) conv ignore
37840   dtd_return=dtd_degrees+dtd_minutes/60+dtd_seconds/3600
37860   if dtd_sign$='-' then dtd_return=-dtd_return
37880   fn_dms_to_dec=dtd_return
37900 fnend  ! fn_dms_to_dec
37920 def fn_itron_record_rff ! off-site (Radio) reads - pg 22
37940   if skip_next_rff_record=1 then
37960     skip_next_rff_record=0
37980   else
38000     fn_record_init
38020     fn_record_addc(3,'RFF')
38040     fn_record_addc(8,route_itron$)
38060     fn_record_addc(8,fn_meterInfo$('transmitter number',z$,serviceCode$(a_item)))
38080     fn_record_addc(6,'')
38100     fn_record_addc(4,'ERT ') ! field 5
38120     fn_record_addx(7)
38140     fn_record_addn(2,0)
38160     fn_record_addn(12,0)
38180     fn_record_addn(4,0)
38200     fn_record_addx(10) ! field 10
38220     fn_record_addc(2,'16')
38240     fn_record_addc(1,'')
38260     fn_record_addc(1,'')
38280     fn_record_addc(1,'')
38300     fn_record_addc(1,'') ! field 15
38320     fn_record_addx(56)
38340     ! fn_record_addc(2,crlf$)
38360     fn_record_write(h_out)
38380   end if
38400 fnend  ! fn_itron_record_rff
38420 def fn_itron_record_fhd ! file header - pg 3
38440   fn_record_init
38460   fn_record_addc(3,'FHD')
38480   fn_record_addc(1,'N')
38500   fn_record_addc(1,'N')
38520   fn_record_addc(5,'')
38540   fn_record_addx(3) ! field 5
38560   fn_record_addn(2,99) ! field 6 - number of cycles - should be one for each route
38580   fn_record_addc(1,'Y') ! field 7 - RFF records present?  Y/N
38600   fn_record_addc(1,'N')
38620   fn_record_addc(1,'N')
38640   fn_record_addx(108) ! field 10
38660   ! fn_record_addc(2,crlf$)
38680   fn_record_write(h_out)
38700 fnend  ! fn_itron_record_fhd
38720 def fn_itron_record_ftr ! file trailer - pg 3
38740   fn_record_init
38760   fn_record_addc(3,'FTR')
38780   fn_record_addc(1,'N')
38800   fn_record_addc(1,'N')
38820   fn_record_addc(5,'')
38840   fn_record_addx(3) ! field 5
38860   fn_record_addn(2,99) ! field 6 - number of cycles - should be one for each route
38880   fn_record_addc(1,'Y') ! field 7 - RFF records present?  Y/N
38900   fn_record_addc(1,'N')
38920   fn_record_addc(1,'N')
38940   fn_record_addx(108) ! field 10
38960   ! fn_record_addc(2,crlf$)
38980   fn_record_write(h_out)
39000 fnend  ! fn_itron_record_FTR
39020 def fn_itron_record_chd ! cycle header - pg 5
39040   fn_record_init
39060   fn_record_addc(3,'CHD')
39080   fn_record_addc(2,cnvrt$('pic(##)',route))
39100   fn_record_addn(4,1)
39120   fn_record_addc(8,date$('mmddccyy'))
39140   fn_record_addx(109) ! field 5
39160   ! fn_record_addc(2,crlf$)
39180   fn_record_write(h_out)
39200 fnend  ! fn_itron_record_chd
39220 def fn_itron_record_ctr ! cycle trailer - pg 5
39240   fn_record_init
39260   fn_record_addc(3,'CTR')
39280   fn_record_addc(2,cnvrt$('pic(##)',route))
39300   fn_record_addn(4,1)
39320   fn_record_addc(8,date$('mmddccyy'))
39340   fn_record_addx(109) ! field 5
39360   ! fn_record_addc(2,crlf$)
39380   fn_record_write(h_out)
39400 fnend  ! fn_itron_record_ctr
39420 def fn_itron_record_mtr ! meter record - pg 13
39440   fn_record_init
39460   fn_record_addc(3,'MTR')
39480   fn_record_addc(8,route_itron$)
39500   fn_record_addn(3,1)
39520   fn_record_addx(2)
39540   fn_record_addn(1,0) ! field 5
39560   fn_record_addx(8)
39580   fn_record_addn(1,0)
39600   fn_record_addx(2)
39620   fn_record_addn(1,0)
39640   fn_record_addc(1,' ') ! field 10
39660   fn_record_addc(1,'A')
39680   fn_record_addc(14,'') ! field 12 - optiocal probe recorder ID
39700   fn_record_addc(12,fn_meterInfo$('Meter Number',z$,serviceCode$(a_item)))
39720   fn_record_addx(2)
39740   fn_record_addc(2,'00') ! field 15 - meter type
39760   fn_record_addn(8,sequence*10+a_item)
39780   fn_record_addx(20)
39800   fn_record_addx(1)
39820   fn_record_addc(2,'00')
39840   fn_record_addx(1) ! field 20
39860   fn_record_addc(2,'00')
39880   fn_record_addx(1)
39900   fn_record_addc(2,'00')
39920   fn_record_addn(1,3)
39940   fn_record_addc(1,'Y') ! field 25
39960   fn_record_addc(1,'N')
39980   fn_record_addc(1,serviceCode$(a_item)(1:1))
40000   fn_record_addc(1,'L')
40020   fn_record_addn(3,0)
40040   fn_record_addc(2,'') ! field 30 - meter audit 1
40060   fn_record_addc(2,'')
40080   fn_record_addc(1,'')
40100   fn_record_addc(1,'')
40120   fn_record_addx(14)
40140   ! fn_record_addc(2,crlf$) ! field 35 (the end CR/LF)
40160   fn_record_write(h_out)
40180 fnend  ! fn_itron_record_mtr
40200 ! /r
44000 def fn_aclara(aclaraLocationId) ! z$,mat e$,extra$(1-2),route
44020   dim tmpCity$*64,tmpState$*64,tmpZip$*64
44040   fncsz(e$(4),tmpCity$,tmpState$,tmpZip$)
44060   transmitterSerialNumber$=trim$(fn_meterInfo$('Transmitter Number',z$,'WA'))
44080   portNumber$=''
44100   posTsnDash=pos(transmitterSerialNumber$,'-')
44120   if posTsnDash>0 then
44140     portNumber$=transmitterSerialNumber$(posTsnDash+1:len(transmitterSerialNumber$))
44160     transmitterSerialNumber$(posTsnDash:len(transmitterSerialNumber$))=''
44180   end if
44200   !
44220   fn_record_init(chr$(9))                                                            ! Aclara Name               ACS Name (if different)
44240   fn_record_addc(5,cnvrt$('pic(#####)',aclaraLocationId))     ! LocationID
44260   fn_record_addc(10,z$)                                                              ! Account Number
44280   fn_record_addc(30,e$(2))                                                           ! Customer Name
44300   fn_record_addc(12,extra$(2))                                                       ! Phone Number
44320   fn_record_addc(30,e$(3))                                                           ! Service Address 1          Address 1 - Primary
44340   fn_record_addc(30,extra$(1))                                                       ! Service Address 2          Address 2 - Primary
44360   fn_record_addc(30,tmpCity$)
44380   fn_record_addc(10,tmpState$)
44400   fn_record_addc(15,tmpZip$)
44420   fn_record_addn(3,route)                                                            ! Cycle and Route            Route Number
44440   fn_record_addn(7,sequence)                                                         ! Sequence                   Sequence
44460   fn_record_addc(8,fn_meterInfo$('Meter Number',z$,'WA'))                         ! Meter Serial Number        Meter.Meter Number
44480   fn_record_addc(20,transmitterSerialNumber$)                  ! Transmitter Serial Number  Meter.Transmitter Number
44500   fn_record_addc(40,fn_meterInfo$('Meter Type',z$,'WA'))                          ! Meter Model/Type
44520   fn_record_addc(1,portNumber$)                          ! Port Number
44540   fn_record_write(h_out, enableTrailingDelimiterOnLine=1)
44560 fnend
45000 def fn_aclaraWorkOrder ! z$,mat e$,extra$(1-2),route
45020   dim tmpCity$*64,tmpState$*64,tmpZip$*64
45040   fncsz(e$(4),tmpCity$,tmpState$,tmpZip$)
45060   !
45080   fn_record_init(chr$(9))                                                            ! Aclara Name               ACS Name (if different)
45100   fn_record_addc(5,cnvrt$('pic(#####)',fnMeterAddressLocationID(e$(1), 1)))     ! LocationID
45120   fn_record_addc(10,z$)                                                              ! Account Number
45140   fn_record_addc(30,e$(2))                                                           ! Customer Name
45160   fn_record_addc(30,e$(1))                                                           ! Meter Address
45180   fn_record_addc(30,tmpCity$)
45200   fn_record_addc(10,tmpState$)
45220   fn_record_addc(15,tmpZip$)
45240   fn_record_addn(3,route)                                                            ! Cycle and Route            Route Number
45260   ! fn_record_addn(7,sequence)                                                         ! Sequence                   Sequence
45280   fn_record_addc(12,f$(1)) ! fn_meterInfo$('Meter Number',z$,'WA')                         ! Meter Serial Number        Meter.Meter Number
45300   fn_record_addc(20,fn_meterInfo$('Transmitter Number',z$,'WA'))                  ! Transmitter Serial Number  Meter.Transmitter Number
45320 ! fn_record_addc(20,'(Rate Code Description??)')                                       ! Service Type
45340   aWmeterType=val(fn_meterInfo$('Meter Type',z$,'WA'))
45360   if aWmeterType=1 then ! r: get aWmeterType$
45380     aWmeterType$='1 inch'
45400   else if aWmeterType=21 then
45420     aWmeterType$='2 inch T-10'
45440   else if aWmeterType=15 then
45460     aWmeterType$='1.5 inch'
45480   else if aWmeterType=2 then
45500     aWmeterType$='2 inch Turbine'
45520   else if aWmeterType=3 then
45540     aWmeterType$='3 inch'
45560   else if aWmeterType=4 then
45580     aWmeterType$='4 inch'
45600   else if aWmeterType=6 then
45620     aWmeterType$='6 inch'
45640   else
45660     if aWmeterType<>5 then pr aWmeterType : pause
45680     aWmeterType$='5/8x3/4'
45700   end if ! /r
45720   fn_record_addc(40,aWmeterType$)                                                   ! Meter Model/Type
45740   fn_record_addn(10,d(1))                                                           ! Service 1 (Water) – Reading – Current
45760 ! fn_record_addc(9,,fn_meterInfo$('reading multiplier',z$,'WA'))                       ! Meter Size
45780   fn_record_addc(30,e$(3))                                                           ! Service Address 1          Address 1 - Primary
45800   fn_record_addc(30,extra$(1))                                                       ! Service Address 2          Address 2 - Primary
45820   fn_record_write(h_out)
45840 fnend
46000 def fn_masterMeter ! z$,mat e$,extra$(1-2),route
46010   dim tmpCity$*64,tmpState$*64,tmpZip$*64
46020   fncsz(e$(4),tmpCity$,tmpState$,tmpZip$)
46030   usage_current=d(3) ! Water usage - current
46040   reading_current=d(1)
46050   unusual_usage_low=round(reading_current+usage_current*fn_pcent,2)
46060   unusual_usage_high=round(reading_current+usage_current+usage_current*fn_pcent,2)
46070   !
46080   fn_record_init(chr$(9))                                           !
46090   fn_record_addc(10,z$)                                             ! Account Number
46100   fn_record_addc(30,e$(2))                                          ! Customer Name
46110   fn_record_addc(30,e$(1))                                          ! Meter Address
46120   fn_record_addn(3,route)                                           ! Route Number
46130   fn_record_addn(7,sequence)                                        ! Sequence
46140   fn_record_addc(12,fn_meterInfo$('Meter Number',z$,'WA'))       ! Meter.Meter Number
46150   fn_record_addc(20,fn_meterInfo$('Transmitter Number',z$,'WA')) ! Transmitter Serial Number  Meter.Transmitter Number
46160   fn_record_addn(9,d(1))                                            ! Service 1 (Water) – Reading – Current
46162   ! pr 'AAA - '&srep$(rec_line$,chr$(9),'>') : pause
46170   fn_record_addc(17,fn_meterInfo$('longitude',z$,'WA'))          ! Meter.Longitude
46172   ! pr 'BBB - '&srep$(rec_line$,chr$(9),'>') : pause
46180   fn_record_addc(17,fn_meterInfo$('latitude',z$,'WA'))           ! Meter.Latitude
46190   fn_record_addc(40,fn_meterInfo$('Meter Type',z$,'WA'))         ! Meter Model/Type
46200   tmp$=fn_meterInfo$('reading multiplier',z$,'WA') : if tmp$='' then tmp$='1'
46210   fn_record_addc(40,tmp$)                                           ! Meter Reading Multiplier (default to 1 if blank)
46220   fn_record_addc(9,'')                                              ! Service 1 (Water) – Reading – Bring Back (leave an empty column for it
46230   fn_record_addc(9,'')                                              ! Service 1 (Water) – Reading Date – Bring Back (leave an empty column for it
46240   fn_record_addn(10,unusual_usage_low)                              ! Unusual Usage Low Reading
46250   fn_record_addn(10,unusual_usage_high)                             ! Unusual Usage High Reading
46260   fn_record_write(h_out)
46270 fnend
46280 !
46500 def fn_READy_Water ! z$,mat e$,extra$(1-2),route
46520   dim tmpCity$*64,tmpState$*64,tmpZip$*64
46540   fncsz(e$(4),tmpCity$,tmpState$,tmpZip$)
46560   fn_record_init(chr$(9))                                           ! ACS Name (if different)
46580   fn_record_addc(10,z$)                                             ! Account Number
46600   fn_record_addc(30,e$(2))                                          ! Customer Name
46620   fn_record_addc(12,extra$(2))                                      ! Phone Number
46640   fn_record_addc(30,e$(1))                                          ! Meter Address (switched to 7/5/17 as per request by Sheri)
46642   ! fn_record_addc(30,e$(3))                                          ! Address 1 - Primary
46660   fn_record_addc(30,extra$(1))                                      ! Address 2 - Primary
46680   fn_record_addc(30,tmpCity$)                                       ! City
46700   fn_record_addc(10,tmpState$)                                      ! State
46720   fn_record_addc(15,tmpZip$)                                        ! Zip
46740   fn_record_addn(3,route)                                           ! Route Number
46760   fn_record_addn(7,sequence)                                        ! Sequence
46780   fn_record_addc(8,fn_meterInfo$('Meter Number',z$,'WA'))         ! Meter.Meter Number
46800   fn_record_write(h_out)
46820 fnend
47000 def fn_record_init(; setDelimiter$)
47020   dim rec_line$*512
47040   rec_line$=''
47050   gRecordDelimiter$=setDelimiter$
47060 fnend  ! fn_record_init
47080 def fn_record_addc(rac_field_length,rac_field_text$*256)
47100   rec_line$=rec_line$&rpad$(rac_field_text$(1:rac_field_length),rac_field_length)&gRecordDelimiter$
47120 fnend
47140 def fn_record_addn(ran_field_length,ran_field_value)
47160   rec_line$=rec_line$&lpad$(str$(ran_field_value)(1:ran_field_length),ran_field_length)&gRecordDelimiter$
47180 fnend
47200 def fn_record_addx(ran_field_length)
47220   rec_line$=rec_line$&rpt$(' ',ran_field_length)&gRecordDelimiter$
47240 fnend  ! fn_record_addx
47500 def fn_record_write(h_out; enableTrailingDelimiterOnLine)
47520   if ~enableTrailingDelimiterOnLine and gRecordDelimiter$<>'' then ! remove trailing delimiter
47540     rec_line$((len(rec_line$)-len(gRecordDelimiter$)+1):len(rec_line$))=''
47560   end if
47580   if deviceSelected$='Itron FC300' then
47600     write #h_out,using 'form pos 1,C '&str$(len(rec_line$)): rec_line$
47620   else
47640     pr #h_out,using 'form pos 1,C '&str$(len(rec_line$)): rec_line$
47650     ! pr srep$(rec_line$,chr$(9),'>') : pause
47660   end if
47680 fnend
48000 def fn_boson
48020   dim z_out$*14,custname$*30
48040   for j=1 to len(seq$)
48060     if val(seq$(j:j))=1 then
48080       svc_flag$="W"
48100     else if val(seq$(j:j))=2 then
48120       svc_flag$="E"
48140     else if val(seq$(j:j))=4 then
48160       svc_flag$="G"
48180     end if
48280     custname$=e$(2)
48320     z_out$=trim$(z$)&svc_flag$
48340     on val(seq$(j:j)) goto WATER_BOSON,ELECTRIC_BOSON,DEMAND_BOSON,GAS_BOSON none BOSON_NEXT_SEQUENCE
48360     WATER_BOSON: if a(1)=0 or final<>0 then goto BOSON_NEXT_SEQUENCE
48380     x$=cnvrt$("pic(######)",d(5)) : readdate$=x$(1:2)&"-"&x$(3:4)&"-"&x$(5:6)
48400     if env$('client')='Kincaid' then
48420       readingt$="S"
48440     else if env$('client')="Moweaqua" then
48460       if trim$(f$(1))="" then
48480         readingt$="S"
48500       else
48520         readingt$="P"
48540       end if
48560     else if trim$(extra$(3))="" then
48580       readingt$="S"
48600     else
48620       readingt$="P"
48640     end if
48660     if env$('client')="Purdy" or env$('client')="Billings" or env$('client')="Cerro Gordo" then readingt$="S"
48680     metertag=0: metertag=val(extra$(3)) conv ignore
48700     if env$('client')="Moweaqua" then metertag=0: metertag=val(f$(1)) conv ignore
48720     if env$('client')="Moweaqua" and (a(1)=1 or a(1)=2) then d(1)=d(1): d(2)=d(2): d(3)=d(3)
48740     if env$('client')="Monticello" and trim$(extra$(7))="22" then d(1)=d(1)*100: d(2)=d(2)*100: d(3)=d(3)*100
48760     if env$('client')="Monticello" and trim$(extra$(7))="23" then d(1)=d(1)*10: d(2)=d(2)*10: d(3)=d(3)*10
48780     ! If env$('client')="Monticello" AND (TRIM$(EXTRA$(7))="24" then don't do anything
48800     if env$('client')="Monticello" and trim$(extra$(7))="65" then d(1)=d(1)*100: d(2)=d(2)*100: d(3)=d(3)*100
48820     if env$('client')="Monticello" and trim$(extra$(7))="66" then d(1)=d(1)*100: d(2)=d(2)*100: d(3)=d(3)*100
48840     meterdials=0 ! if env$('client')="Purdy" or env$('client')="Billings" then meterdials=0 else meterdials=7
48860     if trim$(z_out$)='200670' then pause
48880     pr #h_out,using F_BOSON_OUT: lpad$(rtrm$(z_out$),14),"",custname$,e$(1),"","",svc_flag$,f$(1)," ",0,d(1)+(d(3)*2),d(1)+(d(3)*.50),readdate$,route,"",sequence,meterdials,d(1),readingt$,metertag
48900     !     pr #h_out,using F_BOSON_OUT: lpad$(rtrm$(z_out$),14),"",custname$,e$(1),"","",svc_flag$,f$(1)," ",0,d(1)+(d(3)*2),d(1)+(d(3)*.50),readdate$,val(z$(1:2)),"",val(z$(3:7)),meterdials,d(1),readingt$,metertag
48920     F_BOSON_OUT: form pos 1,c 14,c 3,3*c 30,2*c 1,c 20,c 5,3*pic(#########),pic(########),pic(####),c 1,pic(######),pic(##),pic(#########),c 1,pic(############)
48940     goto BOSON_NEXT_SEQUENCE
48960     ! ___________________________
48980     ELECTRIC_BOSON: if a(3)=0 or trim$(serviceName$(3))<>"Electric" then goto BOSON_NEXT_SEQUENCE
49000     pr #h_out,using F_BOSON_OUT: lpad$(rtrm$(z_out$),14),"",custname$,e$(1),"","",svc_flag$,f$(1)," ",0,d(5)+(d(7)*2),d(5)+(d(7)*.50),d(5),route,"",sequence,0,d(5),"R",f$(2)
49020     !     pr #h_out,using F_BOSON_OUT: lpad$(rtrm$(z_out$),14),"",custname$,e$(1),"","",svc_flag$,f$(1)," ",0,d(5)+(d(7)*2),d(5)+(d(7)*.50),d(5),val(z$(1:2)),"",val(z$(3:7)),0,d(5),"R",f$(2)
49040     goto BOSON_NEXT_SEQUENCE
49060     ! ___________________________
49080     DEMAND_BOSON: goto BOSON_NEXT_SEQUENCE
49100     goto BOSON_NEXT_SEQUENCE
49120     ! ___________________________
49140     GAS_BOSON: if a(4)=0 or trim$(serviceName$(4))<>"Gas" then goto BOSON_NEXT_SEQUENCE
49160     readingt$="R"
49180     pr #h_out,using F_BOSON_OUT: lpad$(rtrm$(z_out$),14),"",custname$,e$(1),"","",svc_flag$,f$(1)," ",0,d(9)+(d(11)*2),d(9)+(d(11)*.50),d(9),route,"",sequence,0,d(9),readingt$,f$(2)
49200     !     pr #h_out,using F_BOSON_OUT: lpad$(rtrm$(z_out$),14),"",custname$,e$(1),"","",svc_flag$,f$(1)," ",0,d(9)+(d(11)*2),d(9)+(d(11)*.50),d(9),val(z$(1:2)),"",val(z$(3:7)),0,d(9),readingt$,f$(2)
49220     goto BOSON_NEXT_SEQUENCE
49240     ! ___________________________
49260     BOSON_NEXT_SEQUENCE: !
49280   next j
49300 fnend
58000 Finis: ! r: Transfer to or from Hand Held Computer
58020   dim out_filename_report$*512
58040   out_filename_report$=file$(h_out)
58060   close #h_out: ioerr ignore
58080   close #h_customer_i1: ioerr ignore
58100   fn_report_created_file(out_filename_report$)
58120   fn_transfer
58200 goto XIT ! /r
60000 XIT: fnxit
60020 IGNORE: continue
62000 def fn_transfer
62020   if deviceSelected$="ACS Meter Reader" then
62040     fnTos(sn$="ACSMR_ASK_DEST")
62060     mat resp$=("")
62080     fnLbl(1,1,"Android Drive:",20,1)
62120     fncomboa("USB-Drive",1,23,mat drive$,"Drive letter of the destination android device.")
62140     fnCmdSet(2)
62160     fnAcs(sn$,0,mat resp$,ckey)
62180     if ckey<>5 then
62200       dest$=resp$(1)
62220       execute "copy "&out_filename$&" "&trim$(dest$)&"acs_meter_data.txt"
62240     end if  ! ckey<>5
62260     goto TRANSFER_XIT
62280   end if  ! deviceSelected$="ACS Meter Reader"
62400   !   else if deviceSelected$="Badger" then
62420   !     goto TRANSFER_XIT ! output file already if folder for                                               badger to read
62440   if deviceSelected$="LapTop" then ! else if...
62460     goto TRANSFER_TO_LAPTOP
62480   else if deviceSelected$="Psion Workabout" then
62500     if exists("S:\RCom\RComW.exe")<>0 then ! else  if ...
62520       execute 'Sy "'&os_filename$("S:\RCom\RComW.exe")&'" /w -n'
62540     else
62560       execute 'Sy "'&os_filename$("S:\acsUB\PreRoute.bat")&'" -n' ! "Psion Workabout"
62580     end if  ! deviceSelected$="Psion Workabout"
62600   end if
62620   goto TRANSFER_XIT
63000   TRANSFER_TO_LAPTOP: ! r: transfer files for laptop
63020     fnTos(sn$="trtolaptop")
63040     mat resp$=("")
63060     fnLbl(1,1,"Destination Drive:",20,1)
63100     fnTxt(1,23,20,100,0,"",0,"Destination can be a drive designation including folders")
63120     if resp$(1)="" then resp$(1)="A:\"
63140     fnCmdSet(2)
63160     fnAcs(sn$,0,mat resp$,ckey)
63180     if ckey=5 then goto TRANSFER_XIT
63200     dest$=resp$(1)
63220     if len(dest$)=0 then goto TRANSFER_TO_LAPTOP
63240     if len(dest$)=1 then dest$=dest$=":"
63260     if len(dest$)=3 and dest$(3:3)="/" then dest$(3:3)=""
63280     fnCopy(env$('Q')&"\UBmstr\laptop.out",env$('at')&trim$(dest$)&"\laptop.out")
63300   goto TRANSFER_XIT ! /r
63320   TRANSFER_XIT: !
63340 fnend  ! fn_transfer
65000 def fn_report_created_file(out_filename_report$*512)
65100   if out_filename_report$<>'' and out_filename_report$<>':CON:' and deviceSelected$<>'Psion Workabout' and deviceSelected$<>'LapTop' then
65120     mat m$(2)
65140     m$(1)="Hand Held File created:"
65160     m$(2)=os_filename$(out_filename_report$)
65180     fnmsgbox(mat m$, response$, '',64)
65200   end if
65240 fnend
68000 def fn_cnt_of_metered_svcs_active
68020   ! this function should return the number of metered services the customer has that have a non 0 rate code.
68040   if env$('client')='Bethany' then ! the new way
68060     nomsa_return=0
68080     if a(1)<>0 then nomsa_return+=1 ! service1  WA
68100     if a(3)<>0 then nomsa_return+=1 ! service3  EL
68120     if a(4)<>0 then nomsa_return+=1 ! service4  GA
68140   else ! the old way
68160     nomsa_return=max(1,d(13))
68180   end if
68200   fn_cnt_of_metered_svcs_active=nomsa_return
68220 fnend
69000 def library fnMeterInfo$*20(mi_field$,z$*10,serviceCode$; closeHandle)
69010   if ~setup then let fn_setup
69020   fnMeterInfo$=fn_meterInfo$(mi_field$,z$,serviceCode$, closeHandle)
69030 fnend
70000 def fn_meterInfo$*20(mi_field$,z$*10,serviceCode$; closeHandle)
70010   if ~mi_setup then
70020     mi_setup=1
70040     dim mt_data$(5)*40
70050     dim mi_return$*20
70080     dim location$(0)*128
70090     dim locationN(0)
70100     hLocation=fn_open('U4 Meter Location',mat location$,mat locationN,mat form$, 1,4)
70160     open #mi_h_metertype:=fngethandle: "Name="&env$('Q')&"\UBmstr\MeterType.h"&env$('cno')&",Version=1,KFName="&env$('Q')&"\UBmstr\MeterTypeIdx.h"&env$('cno')&",Shr",internal,input,keyed
70170     F_METER_TYPE: form pos 1,c 5,c 40,c 9,c 2,c 2
70180     !
70190   end if  ! ~mi_setup
70200   mi_return$=''
70202   location$(loc_activeCustomer)=trim$(z$)
70204   location$(loc_serviceId)=serviceCode$
70206   locationKey$=fnbuildkey$('U4 Meter Location',mat location$,mat locationN, 4) ! pr locationKey$ : pause
70210   if mi_locationKey_prior$<>locationKey$ then
70220     mat location$=('') : mat location=(0)
70230     mi_locationKey_prior$=locationKey$
70250     read #hLocation,using form$(hLocation),key=locationKey$,release: mat location$,mat locationN nokey MI_FINIS
70290   end if
70300   mi_field$=lwrc$(trim$(mi_field$))
70310   if mi_field$='longitude' then
70320     mi_return$=location$(loc_longitude)
70330   else if mi_field$='latitude' then
70340     mi_return$=location$(loc_latitude)
70350   else if mi_field$='meter number' then
70360     mi_return$=location$(loc_meterNumber)
70370   else if mi_field$='transmitter number' then
70380     mi_return$=location$(loc_transmitter)
70390   else if mi_field$='meter type' then
70400     mi_return$=location$(loc_meterType)
70410   else ! it's probably a MeterType field
70420     mt_key$=location$(loc_meterType)
70430     if mt_key_prior$<>mt_key$ then
70440       mt_key_prior$=mt_key$
70450       mat mt_data$=("")
70460       read #mi_h_metertype,using F_METER_TYPE,key=rpad$(trim$(mt_key$),kln(mi_h_metertype)): mat mt_data$ nokey MI_FINIS
70470     end if
70480     if mi_field$='reading multipler' or mi_field$='reading multiplier' then
70490       mi_return$=rtrm$(mt_data$(3))
70500     else if mi_field$='number of dials' then
70510       mi_return$=rtrm$(mt_data$(4))
70520     else if mi_field$='read type' then
70530       mi_return$=rtrm$(mt_data$(5))
70540     end if
70550   end if
70560   MI_FINIS: !
70570   if closeHandle then
70580     close #hLocation: ioerr ignore
70590     close #mi_h_meter: ioerr ignore
70600     close #mi_h_metertype: ioerr ignore
70610   end if
70620   fn_meterInfo$=mi_return$
70630 fnend
71000 def library fnHandHeldList(mat deviceName$; mat deviceOption$)
71020   if ~setup then let fn_setup
71040   fnHandHeldList=fn_handHeldList(mat deviceName$)
71060 fnend
72000 def fn_handHeldList(mat deviceName$; mat deviceOption$)
72020   if ~hhlSetup then 
72040     hhlSetup=1
72060     dim deviceNameCache$(0)*20
72080     dim deviceOptionCache$(0)*128
72100     mat deviceNameCache$(0)
72120     mat deviceOptionCache$(0)
72140     fnAddOneC(mat deviceNameCache$,'Aclara'           ) : fnAddOneC(mat deviceOptionCache$,'')
72160     fnAddOneC(mat deviceNameCache$,'Aclara Work Order') : fnAddOneC(mat deviceOptionCache$,'')
72180     fnAddOneC(mat deviceNameCache$,'ACS Meter Reader' ) : fnAddOneC(mat deviceOptionCache$,'')
72200     fnAddOneC(mat deviceNameCache$,'Badger'           ) : fnAddOneC(mat deviceOptionCache$,'')
72220     fnAddOneC(mat deviceNameCache$,'Boson'            ) : fnAddOneC(mat deviceOptionCache$,'')
72240     fnAddOneC(mat deviceNameCache$,'CSV by LocationID') : fnAddOneC(mat deviceOptionCache$,'ImportOnly')
72260     fnAddOneC(mat deviceNameCache$,'Itron FC300'      ) : fnAddOneC(mat deviceOptionCache$,'')
72280     fnAddOneC(mat deviceNameCache$,'Master Meter'     ) : fnAddOneC(mat deviceOptionCache$,'')
72300     fnAddOneC(mat deviceNameCache$,'READy Water'      ) : fnAddOneC(mat deviceOptionCache$,'')
72320     fnAddOneC(mat deviceNameCache$,'Sensus'           ) : fnAddOneC(mat deviceOptionCache$,'')
72340     ! r: developed but currently unused
72360     ! fnAddOneC(mat deviceNameCache$,"Psion Workabout") : fnAddOneC(mat deviceOptionCache$,'')
72380     ! fnAddOneC(mat deviceNameCache$,"LapTop"         ) : fnAddOneC(mat deviceOptionCache$,'')
72400     ! fnAddOneC(mat deviceNameCache$,"Green Tree"     ) : fnAddOneC(mat deviceOptionCache$,'')
72420     ! fnAddOneC(mat deviceNameCache$,"Hersey"         ) : fnAddOneC(mat deviceOptionCache$,'')
72440     ! fnAddOneC(mat deviceNameCache$,"EZReader"       ) : fnAddOneC(mat deviceOptionCache$,'')
72460     ! fnAddOneC(mat deviceNameCache$,"AMR"            ) : fnAddOneC(mat deviceOptionCache$,'')
72480     ! fnAddOneC(mat deviceNameCache$,"Unitech HT630"  ) : fnAddOneC(mat deviceOptionCache$,'')
72500     ! /r
72520   end if
72540   mat deviceName$(udim(mat deviceNameCache$))
72560   mat deviceName$=deviceNameCache$
72580   !
72600   DeviceOptionArrayPassed=0
72620   on error goto HhlContinue
72640   mat deviceOption$(1) 
72660   deviceOption$(1)=rpt$('*',128)
72670   on error goto ERTN
72680   DeviceOptionArrayPassed=1
72700   HhlContinue: !
72720   if DeviceOptionArrayPassed then
72740     mat deviceOption$(udim(mat deviceOptionCache$))
72760     mat deviceOption$=deviceOptionCache$
72780   end if
72800 fnend
74000 def fn_customerRead(; accountKey$,locationId) ! all values read are passed back as local variables
74010   if locationId and ~LastLocationIdOnFileSetup then ! r: get LastLocationIdOnFile
74020     LastLocationIdOnFileSetup=1
74030     dim form$(0)*256
74040     if meterDataSourceOverrideEnabled then
74050       dim location$(0)*128,locationN(0)
74060       hLocationByLocationID=fn_open('U4 Meter Location',mat location$,mat locationN,mat form$, 1)
74070       read #hLocationByLocationID,using form$(hLocationByLocationID),last: mat location$,mat locationN
74080       close #hLocationByLocationID:
74090       LastLocationIdOnFile=locationN(loc_LocationID)
74100     else
74110       dim maData$(0)*30,maDataN(0)
74120       hMeterAddressLocationID=fn_open('UB Meter Address',mat maData$,mat maDataN,mat form$, 1)
74130       read #hMeterAddressLocationID,using form$(hMeterAddressLocationID),last: mat maData$,mat maDataN
74140       close #hMeterAddressLocationID:
74150       LastLocationIdOnFile=maDataN(ma_LocationID)
74160     end if
74170   end if ! /r
74180   ! #h_customer_i1 and #h_customer_i5 are inherited local variables
74190   dim extra$(11)*30
74200   crReturn=0
74210   ! r: clear all the variables that are returned (locally) by this function
74220     z$=''
74230     mat e$=('')
74240     mat a=(0)
74250     final=0
74260     mat d=(0)
74270     mat f$=('')
74280     route=0
74290     sequence=0
74300     mat extra$=('')
74310     mat extra=(0)
74320     alp$=''
74330   ! /r
74340   F_CUSTOMER: form pos 1,c 10,4*c 30,pos 143,7*pd 2,pos 1821,n 2,pos 217,15*pd 5,pos 131,c 12,pos 361,2*c 12,pos 1741,n 2,n 7,pos 1864,C 30,7*C 12,3*C 30,pos 1741,n 2,pos 354,c 7
74350   if accountKey$='' and locationId=0 then ! read Sequential
74360     CrReadSequential: !
74370     read #h_customer_i5,using F_CUSTOMER: z$,mat e$,mat a,final,mat d,mat f$,route,sequence,mat extra$,extra(1),alp$ eof CrEoF
74380     if udim(mat filterAccount$)>0 and trim$(filterAccount$(1))<>'' then
74390       if srch(mat filterAccount$,trim$(z$))<=0 then
74400         goto CrReadSequential
74410       end if
74420     end if
74430   else if locationId<>0 then
74440     accountFromLocationId$=fnAccountFromLocationId$(locationId,1)
74450     if accountFromLocationId$='' then
74460       if locationId>LastLocationIdOnFile then 
74470         goto CrEoF
74480       else
74490         crReturn=0
74500         goto CrFinis
74510       end if
74520     end if
74530     read #h_customer_i1,using F_CUSTOMER,key=fnAccountFromLocationId$(locationId,1): z$,mat e$,mat a,final,mat d,mat f$,route,sequence,mat extra$,extra(1),alp$ nokey CrNoKey
74540   else
74550     read #h_customer_i1,using F_CUSTOMER,key=z$: z$,mat e$,mat a,final,mat d,mat f$,route,sequence,mat extra$,extra(1),alp$ nokey CrNoKey
74560   end if
74570   crReturn=1
74580   goto CrFinis
74590   CrNoKey: ! r:
74600     crReturn=-4272
74610   goto CrFinis ! /r
74620   CrEoF: ! r:
74630     crReturn=-54
74640   goto CrFinis ! /r
74650   CrFinis: !
74660   fn_customerRead=crReturn
74670 fnend
76000 def fn_getFilterAccount(mat filterAccount$)
76002   mat filterAccount$(0)
76004   fnAddOneC(mat filterAccount$,'100050.05')
76006   fnAddOneC(mat filterAccount$,'100110.00')
76008   fnAddOneC(mat filterAccount$,'100111.00')
76010   fnAddOneC(mat filterAccount$,'100114.00')
76012   fnAddOneC(mat filterAccount$,'100115.01')
76014   fnAddOneC(mat filterAccount$,'100120.00')
76016   fnAddOneC(mat filterAccount$,'100125.01')
76018   fnAddOneC(mat filterAccount$,'100130.01')
76020   fnAddOneC(mat filterAccount$,'100135.04')
76022   fnAddOneC(mat filterAccount$,'100140.04')
76024   fnAddOneC(mat filterAccount$,'100145.10')
76026   fnAddOneC(mat filterAccount$,'100150.02')
76028   fnAddOneC(mat filterAccount$,'100260.00')
76030   fnAddOneC(mat filterAccount$,'100270.05')
76032   fnAddOneC(mat filterAccount$,'100275.02')
76034   fnAddOneC(mat filterAccount$,'100285.00')
76036   fnAddOneC(mat filterAccount$,'100290.00')
76038   fnAddOneC(mat filterAccount$,'100295.00')
76040   fnAddOneC(mat filterAccount$,'100300.12')
76042   fnAddOneC(mat filterAccount$,'100305.00')
76044   fnAddOneC(mat filterAccount$,'100310.00')
76046   fnAddOneC(mat filterAccount$,'100315.05')
76048   fnAddOneC(mat filterAccount$,'100320.03')
76050   fnAddOneC(mat filterAccount$,'100330.02')
76052   fnAddOneC(mat filterAccount$,'100345.00')
76054   fnAddOneC(mat filterAccount$,'100350.00')
76056   fnAddOneC(mat filterAccount$,'100355.00')
76058   fnAddOneC(mat filterAccount$,'100360.00')
76060   fnAddOneC(mat filterAccount$,'100365.00')
76062   fnAddOneC(mat filterAccount$,'100370.00')
76064   fnAddOneC(mat filterAccount$,'100375.00')
76066   fnAddOneC(mat filterAccount$,'100385.03')
76068   fnAddOneC(mat filterAccount$,'100395.00')
76070   fnAddOneC(mat filterAccount$,'100400.01')
76072   fnAddOneC(mat filterAccount$,'100410.04')
76074   fnAddOneC(mat filterAccount$,'100415.05')
76076   fnAddOneC(mat filterAccount$,'100420.06')
76078   fnAddOneC(mat filterAccount$,'100425.05')
76080   fnAddOneC(mat filterAccount$,'100430.04')
76082   fnAddOneC(mat filterAccount$,'100440.01')
76084   fnAddOneC(mat filterAccount$,'100450.02')
76086   fnAddOneC(mat filterAccount$,'100455.01')
76088   fnAddOneC(mat filterAccount$,'100460.03')
76090   fnAddOneC(mat filterAccount$,'100465.04')
76092   fnAddOneC(mat filterAccount$,'100470.00')
76094   fnAddOneC(mat filterAccount$,'100475.02')
76096   fnAddOneC(mat filterAccount$,'100505.00')
76098   fnAddOneC(mat filterAccount$,'100510.00')
76100   fnAddOneC(mat filterAccount$,'100515.02')
76102   fnAddOneC(mat filterAccount$,'100520.01')
76104   fnAddOneC(mat filterAccount$,'100525.02')
76106   fnAddOneC(mat filterAccount$,'100530.07')
76108   fnAddOneC(mat filterAccount$,'100535.02')
76110   fnAddOneC(mat filterAccount$,'100540.04')
76112   fnAddOneC(mat filterAccount$,'100545.03')
76114   fnAddOneC(mat filterAccount$,'100550.00')
76116   fnAddOneC(mat filterAccount$,'100555.03')
76118   fnAddOneC(mat filterAccount$,'100560.00')
76120   fnAddOneC(mat filterAccount$,'100565.01')
76122   fnAddOneC(mat filterAccount$,'100570.02')
76124   fnAddOneC(mat filterAccount$,'100575.02')
76126   fnAddOneC(mat filterAccount$,'100580.00')
76128   fnAddOneC(mat filterAccount$,'100585.00')
76130   fnAddOneC(mat filterAccount$,'100590.08')
76132   fnAddOneC(mat filterAccount$,'100595.04')
76134   fnAddOneC(mat filterAccount$,'100600.01')
76136   fnAddOneC(mat filterAccount$,'100605.03')
76138   fnAddOneC(mat filterAccount$,'100610.02')
76140   fnAddOneC(mat filterAccount$,'100615.07')
76142   fnAddOneC(mat filterAccount$,'100620.09')
76144   fnAddOneC(mat filterAccount$,'100625.01')
76146   fnAddOneC(mat filterAccount$,'100635.05')
76148   fnAddOneC(mat filterAccount$,'100640.04')
76150   fnAddOneC(mat filterAccount$,'100645.02')
76152   fnAddOneC(mat filterAccount$,'100650.04')
76154   fnAddOneC(mat filterAccount$,'100655.01')
76156   fnAddOneC(mat filterAccount$,'100660.04')
76158   fnAddOneC(mat filterAccount$,'100670.01')
76160   fnAddOneC(mat filterAccount$,'100675.00')
76162   fnAddOneC(mat filterAccount$,'100690.02')
76164   fnAddOneC(mat filterAccount$,'100695.02')
76166   fnAddOneC(mat filterAccount$,'100700.00')
76168   fnAddOneC(mat filterAccount$,'100705.00')
76170   fnAddOneC(mat filterAccount$,'100710.02')
76172   fnAddOneC(mat filterAccount$,'100715.09')
76174   fnAddOneC(mat filterAccount$,'100730.01')
76176   fnAddOneC(mat filterAccount$,'100735.00')
76178   fnAddOneC(mat filterAccount$,'100745.04')
76180   fnAddOneC(mat filterAccount$,'100750.11')
76182   fnAddOneC(mat filterAccount$,'100755.03')
76184   fnAddOneC(mat filterAccount$,'100760.06')
76186   fnAddOneC(mat filterAccount$,'100765.02')
76188   fnAddOneC(mat filterAccount$,'100770.03')
76190   fnAddOneC(mat filterAccount$,'100775.08')
76192   fnAddOneC(mat filterAccount$,'100780.01')
76194   fnAddOneC(mat filterAccount$,'100785.02')
76196   fnAddOneC(mat filterAccount$,'100790.03')
76198   fnAddOneC(mat filterAccount$,'100795.00')
76200   fnAddOneC(mat filterAccount$,'100800.00')
76202   fnAddOneC(mat filterAccount$,'100810.01')
76204   fnAddOneC(mat filterAccount$,'100820.01')
76206   fnAddOneC(mat filterAccount$,'100830.04')
76208   fnAddOneC(mat filterAccount$,'100835.06')
76210   fnAddOneC(mat filterAccount$,'100840.01')
76212   fnAddOneC(mat filterAccount$,'100845.00')
76214   fnAddOneC(mat filterAccount$,'100850.02')
76216   fnAddOneC(mat filterAccount$,'100855.00')
76218   fnAddOneC(mat filterAccount$,'100860.01')
76220   fnAddOneC(mat filterAccount$,'100865.03')
76222   fnAddOneC(mat filterAccount$,'100870.00')
76224   fnAddOneC(mat filterAccount$,'100875.01')
76226   fnAddOneC(mat filterAccount$,'100880.00')
76228   fnAddOneC(mat filterAccount$,'100885.01')
76230   fnAddOneC(mat filterAccount$,'100890.00')
76232   fnAddOneC(mat filterAccount$,'100900.00')
76234   fnAddOneC(mat filterAccount$,'101015.01')
76236   fnAddOneC(mat filterAccount$,'101025.00')
76238   fnAddOneC(mat filterAccount$,'101055.03')
76240   fnAddOneC(mat filterAccount$,'101060.04')
76242   fnAddOneC(mat filterAccount$,'101065.08')
76244   fnAddOneC(mat filterAccount$,'101070.05')
76246   fnAddOneC(mat filterAccount$,'101070.06')
76248   fnAddOneC(mat filterAccount$,'101090.09')
76250   fnAddOneC(mat filterAccount$,'101095.07')
76252   fnAddOneC(mat filterAccount$,'101100.05')
76254   fnAddOneC(mat filterAccount$,'101110.11')
76256   fnAddOneC(mat filterAccount$,'101120.06')
76258   fnAddOneC(mat filterAccount$,'101125.03')
76260   fnAddOneC(mat filterAccount$,'101130.08')
76262   fnAddOneC(mat filterAccount$,'101135.11')
76264   fnAddOneC(mat filterAccount$,'101140.00')
76266   fnAddOneC(mat filterAccount$,'101145.00')
76268   fnAddOneC(mat filterAccount$,'101150.02')
76270   fnAddOneC(mat filterAccount$,'101165.11')
76272   fnAddOneC(mat filterAccount$,'101175.00')
76274   fnAddOneC(mat filterAccount$,'101200.03')
76276   fnAddOneC(mat filterAccount$,'101235.01')
76278   fnAddOneC(mat filterAccount$,'101240.02')
76280   fnAddOneC(mat filterAccount$,'101245.04')
76282   fnAddOneC(mat filterAccount$,'101250.01')
76284   fnAddOneC(mat filterAccount$,'101255.04')
76286   fnAddOneC(mat filterAccount$,'101260.01')
76288   fnAddOneC(mat filterAccount$,'101265.00')
76290   fnAddOneC(mat filterAccount$,'101270.01')
76292   fnAddOneC(mat filterAccount$,'101275.01')
76294   fnAddOneC(mat filterAccount$,'101280.06')
76296   fnAddOneC(mat filterAccount$,'101285.00')
76298   fnAddOneC(mat filterAccount$,'101290.00')
76300   fnAddOneC(mat filterAccount$,'101295.00')
76302   fnAddOneC(mat filterAccount$,'101300.02')
76304   fnAddOneC(mat filterAccount$,'101305.00')
76306   fnAddOneC(mat filterAccount$,'101310.00')
76308   fnAddOneC(mat filterAccount$,'101315.11')
76310   fnAddOneC(mat filterAccount$,'101320.03')
76312   fnAddOneC(mat filterAccount$,'101330.00')
76314   fnAddOneC(mat filterAccount$,'101335.02')
76316   fnAddOneC(mat filterAccount$,'101340.07')
76318   fnAddOneC(mat filterAccount$,'101345.05')
76320   fnAddOneC(mat filterAccount$,'101350.01')
76322   fnAddOneC(mat filterAccount$,'101355.00')
76324   fnAddOneC(mat filterAccount$,'101360.10')
76326   fnAddOneC(mat filterAccount$,'101365.02')
76328   fnAddOneC(mat filterAccount$,'101375.01')
76330   fnAddOneC(mat filterAccount$,'101380.02')
76332   fnAddOneC(mat filterAccount$,'101385.01')
76334   fnAddOneC(mat filterAccount$,'101390.01')
76336   fnAddOneC(mat filterAccount$,'101395.00')
76338   fnAddOneC(mat filterAccount$,'101400.00')
76340   fnAddOneC(mat filterAccount$,'101405.05')
76342   fnAddOneC(mat filterAccount$,'101415.00')
76344   fnAddOneC(mat filterAccount$,'101420.00')
76346   fnAddOneC(mat filterAccount$,'101425.02')
76348   fnAddOneC(mat filterAccount$,'101435.14')
76350   fnAddOneC(mat filterAccount$,'101440.00')
76352   fnAddOneC(mat filterAccount$,'101445.00')
76354   fnAddOneC(mat filterAccount$,'101450.00')
76356   fnAddOneC(mat filterAccount$,'101455.01')
76358   fnAddOneC(mat filterAccount$,'101460.01')
76360   fnAddOneC(mat filterAccount$,'101465.03')
76362   fnAddOneC(mat filterAccount$,'101475.19')
76364   fnAddOneC(mat filterAccount$,'101480.12')
76366   fnAddOneC(mat filterAccount$,'101485.01')
76368   fnAddOneC(mat filterAccount$,'101495.03')
76370   fnAddOneC(mat filterAccount$,'101500.02')
76372   fnAddOneC(mat filterAccount$,'101505.01')
76374   fnAddOneC(mat filterAccount$,'101510.02')
76376   fnAddOneC(mat filterAccount$,'101515.09')
76378   fnAddOneC(mat filterAccount$,'101520.01')
76380   fnAddOneC(mat filterAccount$,'101530.01')
76382   fnAddOneC(mat filterAccount$,'101550.04')
76384   fnAddOneC(mat filterAccount$,'101560.03')
76386   fnAddOneC(mat filterAccount$,'101565.00')
76388   fnAddOneC(mat filterAccount$,'101570.03')
76390   fnAddOneC(mat filterAccount$,'101620.14')
76392   fnAddOneC(mat filterAccount$,'101625.10')
76394   fnAddOneC(mat filterAccount$,'101630.18')
76396   fnAddOneC(mat filterAccount$,'101635.09')
76398   fnAddOneC(mat filterAccount$,'101645.19')
76400   fnAddOneC(mat filterAccount$,'101650.06')
76402   fnAddOneC(mat filterAccount$,'101665.16')
76404   fnAddOneC(mat filterAccount$,'101770.00')
76406   fnAddOneC(mat filterAccount$,'101775.06')
76408   fnAddOneC(mat filterAccount$,'101780.03')
76410   fnAddOneC(mat filterAccount$,'101785.09')
76412   fnAddOneC(mat filterAccount$,'101790.00')
76414   fnAddOneC(mat filterAccount$,'101795.02')
76416   fnAddOneC(mat filterAccount$,'101850.00')
76418   fnAddOneC(mat filterAccount$,'101900.06')
76420   fnAddOneC(mat filterAccount$,'101905.00')
76422   fnAddOneC(mat filterAccount$,'101910.01')
76424   fnAddOneC(mat filterAccount$,'101915.00')
76426   fnAddOneC(mat filterAccount$,'101920.00')
76428   fnAddOneC(mat filterAccount$,'101925.03')
76430   fnAddOneC(mat filterAccount$,'101930.00')
76432   fnAddOneC(mat filterAccount$,'101935.02')
76434   fnAddOneC(mat filterAccount$,'101940.00')
76436   fnAddOneC(mat filterAccount$,'101945.03')
76438   fnAddOneC(mat filterAccount$,'101950.00')
76440   fnAddOneC(mat filterAccount$,'101955.00')
76442   fnAddOneC(mat filterAccount$,'101960.14')
76444   fnAddOneC(mat filterAccount$,'101965.00')
76446   fnAddOneC(mat filterAccount$,'101970.00')
76448   fnAddOneC(mat filterAccount$,'101975.00')
76450   fnAddOneC(mat filterAccount$,'101980.07')
76452   fnAddOneC(mat filterAccount$,'101985.01')
76454   fnAddOneC(mat filterAccount$,'101990.00')
76456   fnAddOneC(mat filterAccount$,'102000.00')
76458   fnAddOneC(mat filterAccount$,'110745.01')
76460   fnAddOneC(mat filterAccount$,'110750.09')
76462   fnAddOneC(mat filterAccount$,'110755.02')
76464   fnAddOneC(mat filterAccount$,'110760.02')
76466   fnAddOneC(mat filterAccount$,'110765.07')
76468   fnAddOneC(mat filterAccount$,'110770.04')
76470   fnAddOneC(mat filterAccount$,'110775.02')
76472   fnAddOneC(mat filterAccount$,'110785.00')
76474   fnAddOneC(mat filterAccount$,'110790.00')
76476   fnAddOneC(mat filterAccount$,'110795.00')
76478   fnAddOneC(mat filterAccount$,'110800.02')
76480   fnAddOneC(mat filterAccount$,'110805.01')
76482   fnAddOneC(mat filterAccount$,'110815.01')
76484   fnAddOneC(mat filterAccount$,'110820.07')
76486   fnAddOneC(mat filterAccount$,'110825.00')
76488   fnAddOneC(mat filterAccount$,'110830.00')
76490   fnAddOneC(mat filterAccount$,'110835.01')
76492   fnAddOneC(mat filterAccount$,'110840.00')
76494   fnAddOneC(mat filterAccount$,'110845.00')
76496   fnAddOneC(mat filterAccount$,'110850.07')
76498   fnAddOneC(mat filterAccount$,'110855.10')
76500   fnAddOneC(mat filterAccount$,'110865.09')
76502   fnAddOneC(mat filterAccount$,'110870.01')
76504   fnAddOneC(mat filterAccount$,'110875.15')
76506   fnAddOneC(mat filterAccount$,'110880.09')
76508   fnAddOneC(mat filterAccount$,'110890.00')
76510   fnAddOneC(mat filterAccount$,'110891.00')
76512   fnAddOneC(mat filterAccount$,'110895.00')
76514   fnAddOneC(mat filterAccount$,'110900.03')
76516   fnAddOneC(mat filterAccount$,'110905.12')
76518   fnAddOneC(mat filterAccount$,'110910.06')
76520   fnAddOneC(mat filterAccount$,'110915.08')
76522   fnAddOneC(mat filterAccount$,'110920.00')
76524   fnAddOneC(mat filterAccount$,'110925.01')
76526   fnAddOneC(mat filterAccount$,'110930.03')
76528   fnAddOneC(mat filterAccount$,'110935.02')
76530   fnAddOneC(mat filterAccount$,'110940.01')
76532   fnAddOneC(mat filterAccount$,'110950.00')
76534   fnAddOneC(mat filterAccount$,'120000.00')
76536   fnAddOneC(mat filterAccount$,'200000.00')
76538   fnAddOneC(mat filterAccount$,'200010.00')
76540   fnAddOneC(mat filterAccount$,'200040.00')
76542   fnAddOneC(mat filterAccount$,'200051.01')
76544   fnAddOneC(mat filterAccount$,'200052.00')
76546   fnAddOneC(mat filterAccount$,'200060.00')
76548   fnAddOneC(mat filterAccount$,'200105.00')
76550   fnAddOneC(mat filterAccount$,'200110.01')
76552   fnAddOneC(mat filterAccount$,'200111.05')
76554   fnAddOneC(mat filterAccount$,'200112.01')
76556   fnAddOneC(mat filterAccount$,'200113.00')
76558   fnAddOneC(mat filterAccount$,'200120.15')
76560   fnAddOneC(mat filterAccount$,'200125.04')
76562   fnAddOneC(mat filterAccount$,'200126.12')
76564   fnAddOneC(mat filterAccount$,'200127.01')
76566   fnAddOneC(mat filterAccount$,'200130.00')
76568   fnAddOneC(mat filterAccount$,'200131.03')
76570   fnAddOneC(mat filterAccount$,'200133.10')
76572   fnAddOneC(mat filterAccount$,'200135.04')
76574   fnAddOneC(mat filterAccount$,'200136.00')
76576   fnAddOneC(mat filterAccount$,'200137.03')
76578   fnAddOneC(mat filterAccount$,'200138.11')
76580   fnAddOneC(mat filterAccount$,'200139.29')
76582   fnAddOneC(mat filterAccount$,'200150.02')
76584   fnAddOneC(mat filterAccount$,'200155.15')
76586   fnAddOneC(mat filterAccount$,'200160.16')
76588   fnAddOneC(mat filterAccount$,'200165.00')
76590   fnAddOneC(mat filterAccount$,'200166.02')
76592   fnAddOneC(mat filterAccount$,'200168.01')
76594   fnAddOneC(mat filterAccount$,'200170.00')
76596   fnAddOneC(mat filterAccount$,'200180.00')
76598   fnAddOneC(mat filterAccount$,'200185.11')
76600   fnAddOneC(mat filterAccount$,'200190.00')
76602   fnAddOneC(mat filterAccount$,'200191.02')
76604   fnAddOneC(mat filterAccount$,'200193.02')
76606   fnAddOneC(mat filterAccount$,'200195.14')
76608   fnAddOneC(mat filterAccount$,'200198.17')
76610   fnAddOneC(mat filterAccount$,'200200.15')
76612   fnAddOneC(mat filterAccount$,'200210.13')
76614   fnAddOneC(mat filterAccount$,'200215.13')
76616   fnAddOneC(mat filterAccount$,'200216.01')
76618   fnAddOneC(mat filterAccount$,'200217.00')
76620   fnAddOneC(mat filterAccount$,'200230.02')
76622   fnAddOneC(mat filterAccount$,'200231.02')
76624   fnAddOneC(mat filterAccount$,'200240.02')
76626   fnAddOneC(mat filterAccount$,'200241.00')
76628   fnAddOneC(mat filterAccount$,'200242.00')
76630   fnAddOneC(mat filterAccount$,'200243.01')
76632   fnAddOneC(mat filterAccount$,'200244.00')
76634   fnAddOneC(mat filterAccount$,'200245.00')
76636   fnAddOneC(mat filterAccount$,'200246.00')
76638   fnAddOneC(mat filterAccount$,'200247.00')
76640   fnAddOneC(mat filterAccount$,'200249.00')
76642   fnAddOneC(mat filterAccount$,'200250.00')
76644   fnAddOneC(mat filterAccount$,'200255.01')
76646   fnAddOneC(mat filterAccount$,'200260.00')
76648   fnAddOneC(mat filterAccount$,'200265.00')
76650   fnAddOneC(mat filterAccount$,'200270.10')
76652   fnAddOneC(mat filterAccount$,'200272.00')
76654   fnAddOneC(mat filterAccount$,'200280.00')
76656   fnAddOneC(mat filterAccount$,'200282.00')
76658   fnAddOneC(mat filterAccount$,'200285.01')
76660   fnAddOneC(mat filterAccount$,'200315.03')
76662   fnAddOneC(mat filterAccount$,'200358.09')
76664   fnAddOneC(mat filterAccount$,'200360.00')
76666   fnAddOneC(mat filterAccount$,'200365.00')
76668   fnAddOneC(mat filterAccount$,'200366.01')
76670   fnAddOneC(mat filterAccount$,'200367.13')
76672   fnAddOneC(mat filterAccount$,'200368.07')
76674   fnAddOneC(mat filterAccount$,'200370.01')
76676   fnAddOneC(mat filterAccount$,'200375.02')
76678   fnAddOneC(mat filterAccount$,'200386.11')
76680   fnAddOneC(mat filterAccount$,'200387.00')
76682   fnAddOneC(mat filterAccount$,'200388.01')
76684   fnAddOneC(mat filterAccount$,'200390.03')
76686   fnAddOneC(mat filterAccount$,'200395.04')
76688   fnAddOneC(mat filterAccount$,'200400.02')
76690   fnAddOneC(mat filterAccount$,'200405.00')
76692   fnAddOneC(mat filterAccount$,'200406.07')
76694   fnAddOneC(mat filterAccount$,'200407.00')
76696   fnAddOneC(mat filterAccount$,'200408.02')
76698   fnAddOneC(mat filterAccount$,'200409.01')
76700   fnAddOneC(mat filterAccount$,'200410.00')
76702   fnAddOneC(mat filterAccount$,'200411.01')
76704   fnAddOneC(mat filterAccount$,'200412.00')
76706   fnAddOneC(mat filterAccount$,'200413.00')
76708   fnAddOneC(mat filterAccount$,'200416.09')
76710   fnAddOneC(mat filterAccount$,'200420.01')
76712   fnAddOneC(mat filterAccount$,'200421.02')
76714   fnAddOneC(mat filterAccount$,'200422.00')
76716   fnAddOneC(mat filterAccount$,'200423.02')
76718   fnAddOneC(mat filterAccount$,'200424.01')
76720   fnAddOneC(mat filterAccount$,'200425.02')
76722   fnAddOneC(mat filterAccount$,'200437.00')
76724   fnAddOneC(mat filterAccount$,'200440.00')
76726   fnAddOneC(mat filterAccount$,'200474.01')
76728   fnAddOneC(mat filterAccount$,'200475.02')
76730   fnAddOneC(mat filterAccount$,'200480.00')
76732   fnAddOneC(mat filterAccount$,'200485.07')
76734   fnAddOneC(mat filterAccount$,'200490.00')
76736   fnAddOneC(mat filterAccount$,'200495.09')
76738   fnAddOneC(mat filterAccount$,'200500.01')
76740   fnAddOneC(mat filterAccount$,'200505.01')
76742   fnAddOneC(mat filterAccount$,'200510.04')
76744   fnAddOneC(mat filterAccount$,'200511.04')
76746   fnAddOneC(mat filterAccount$,'200512.01')
76748   fnAddOneC(mat filterAccount$,'200515.05')
76750   fnAddOneC(mat filterAccount$,'200516.05')
76752   fnAddOneC(mat filterAccount$,'200517.00')
76754   fnAddOneC(mat filterAccount$,'200525.09')
76756   fnAddOneC(mat filterAccount$,'200530.07')
76758   fnAddOneC(mat filterAccount$,'200535.01')
76760   fnAddOneC(mat filterAccount$,'200545.01')
76762   fnAddOneC(mat filterAccount$,'200547.02')
76764   fnAddOneC(mat filterAccount$,'200550.00')
76766   fnAddOneC(mat filterAccount$,'200551.07')
76768   fnAddOneC(mat filterAccount$,'200552.01')
76770   fnAddOneC(mat filterAccount$,'200555.03')
76772   fnAddOneC(mat filterAccount$,'200556.08')
76774   fnAddOneC(mat filterAccount$,'200560.02')
76776   fnAddOneC(mat filterAccount$,'200565.07')
76778   fnAddOneC(mat filterAccount$,'200570.09')
76780   fnAddOneC(mat filterAccount$,'200575.07')
76782   fnAddOneC(mat filterAccount$,'200580.01')
76784   fnAddOneC(mat filterAccount$,'200585.02')
76786   fnAddOneC(mat filterAccount$,'200590.00')
76788   fnAddOneC(mat filterAccount$,'200595.02')
76790   fnAddOneC(mat filterAccount$,'200600.00')
76792   fnAddOneC(mat filterAccount$,'200605.04')
76794   fnAddOneC(mat filterAccount$,'200610.07')
76796   fnAddOneC(mat filterAccount$,'200615.00')
76798   fnAddOneC(mat filterAccount$,'200620.02')
76800   fnAddOneC(mat filterAccount$,'200625.03')
76802   fnAddOneC(mat filterAccount$,'200630.01')
76804   fnAddOneC(mat filterAccount$,'200635.01')
76806   fnAddOneC(mat filterAccount$,'200645.03')
76808   fnAddOneC(mat filterAccount$,'200647.00')
76810   fnAddOneC(mat filterAccount$,'200675.00')
76812   fnAddOneC(mat filterAccount$,'200680.01')
76814   fnAddOneC(mat filterAccount$,'200690.00')
76816   fnAddOneC(mat filterAccount$,'200695.01')
76818   fnAddOneC(mat filterAccount$,'200700.00')
76820   fnAddOneC(mat filterAccount$,'200705.00')
76822   fnAddOneC(mat filterAccount$,'200715.01')
76824   fnAddOneC(mat filterAccount$,'200720.21')
76826   fnAddOneC(mat filterAccount$,'200721.25')
76828   fnAddOneC(mat filterAccount$,'200722.20')
76830   fnAddOneC(mat filterAccount$,'200723.09')
76832   fnAddOneC(mat filterAccount$,'200725.01')
76834   fnAddOneC(mat filterAccount$,'200726.05')
76836   fnAddOneC(mat filterAccount$,'200727.02')
76838   fnAddOneC(mat filterAccount$,'200730.01')
76840   fnAddOneC(mat filterAccount$,'200731.10')
76842   fnAddOneC(mat filterAccount$,'200735.01')
76844   fnAddOneC(mat filterAccount$,'200740.01')
76846   fnAddOneC(mat filterAccount$,'200750.01')
76848   fnAddOneC(mat filterAccount$,'200760.10')
76850   fnAddOneC(mat filterAccount$,'200765.01')
76852   fnAddOneC(mat filterAccount$,'200770.00')
76854   fnAddOneC(mat filterAccount$,'200772.02')
76856   fnAddOneC(mat filterAccount$,'200773.00')
76858   fnAddOneC(mat filterAccount$,'200775.05')
76860   fnAddOneC(mat filterAccount$,'200776.03')
76862   fnAddOneC(mat filterAccount$,'200780.03')
76864   fnAddOneC(mat filterAccount$,'200781.01')
76866   fnAddOneC(mat filterAccount$,'200785.08')
76868   fnAddOneC(mat filterAccount$,'200790.01')
76870   fnAddOneC(mat filterAccount$,'200792.17')
76872   fnAddOneC(mat filterAccount$,'200793.02')
76874   fnAddOneC(mat filterAccount$,'200795.07')
76876   fnAddOneC(mat filterAccount$,'200796.01')
76878   fnAddOneC(mat filterAccount$,'200800.04')
76880   fnAddOneC(mat filterAccount$,'200801.05')
76882   fnAddOneC(mat filterAccount$,'200804.00')
76884   fnAddOneC(mat filterAccount$,'200805.00')
76886   fnAddOneC(mat filterAccount$,'200807.00')
76888   fnAddOneC(mat filterAccount$,'200810.00')
76890   fnAddOneC(mat filterAccount$,'200815.00')
76892   fnAddOneC(mat filterAccount$,'200820.00')
76894   fnAddOneC(mat filterAccount$,'200825.00')
76896   fnAddOneC(mat filterAccount$,'200830.00')
76898   fnAddOneC(mat filterAccount$,'200831.04')
76900   fnAddOneC(mat filterAccount$,'200835.02')
76902   fnAddOneC(mat filterAccount$,'200845.00')
76904   fnAddOneC(mat filterAccount$,'200851.01')
76906   fnAddOneC(mat filterAccount$,'200853.00')
76908   fnAddOneC(mat filterAccount$,'200855.02')
76910   fnAddOneC(mat filterAccount$,'200860.01')
76912   fnAddOneC(mat filterAccount$,'200861.02')
76914   fnAddOneC(mat filterAccount$,'200862.01')
76916   fnAddOneC(mat filterAccount$,'200863.00')
76918   fnAddOneC(mat filterAccount$,'200864.02')
76920   fnAddOneC(mat filterAccount$,'200865.15')
76922   fnAddOneC(mat filterAccount$,'200866.01')
76924   fnAddOneC(mat filterAccount$,'200867.04')
76926   fnAddOneC(mat filterAccount$,'200869.00')
76928   fnAddOneC(mat filterAccount$,'200870.00')
76930   fnAddOneC(mat filterAccount$,'200871.01')
76932   fnAddOneC(mat filterAccount$,'200872.01')
76934   fnAddOneC(mat filterAccount$,'200873.00')
76936   fnAddOneC(mat filterAccount$,'200874.00')
76938   fnAddOneC(mat filterAccount$,'200890.00')
76940   fnAddOneC(mat filterAccount$,'200891.06')
76942   fnAddOneC(mat filterAccount$,'200895.00')
76944   fnAddOneC(mat filterAccount$,'200897.00')
76946   fnAddOneC(mat filterAccount$,'200898.01')
76948   fnAddOneC(mat filterAccount$,'200899.00')
76950   fnAddOneC(mat filterAccount$,'200900.00')
76952   fnAddOneC(mat filterAccount$,'200901.02')
76954   fnAddOneC(mat filterAccount$,'200902.06')
76956   fnAddOneC(mat filterAccount$,'200903.03')
76958   fnAddOneC(mat filterAccount$,'200904.03')
76960   fnAddOneC(mat filterAccount$,'200905.04')
76962   fnAddOneC(mat filterAccount$,'200906.07')
76964   fnAddOneC(mat filterAccount$,'200907.15')
76966   fnAddOneC(mat filterAccount$,'200908.02')
76968   fnAddOneC(mat filterAccount$,'200909.10')
76970   fnAddOneC(mat filterAccount$,'200910.00')
76972   fnAddOneC(mat filterAccount$,'200911.01')
76974   fnAddOneC(mat filterAccount$,'200912.19')
76976   fnAddOneC(mat filterAccount$,'200913.01')
76978   fnAddOneC(mat filterAccount$,'200914.00')
76980   fnAddOneC(mat filterAccount$,'200915.03')
76982   fnAddOneC(mat filterAccount$,'200916.03')
76984   fnAddOneC(mat filterAccount$,'200917.08')
76986   fnAddOneC(mat filterAccount$,'200918.01')
76988   fnAddOneC(mat filterAccount$,'200920.05')
76990   fnAddOneC(mat filterAccount$,'200921.02')
76992   fnAddOneC(mat filterAccount$,'200922.00')
76994   fnAddOneC(mat filterAccount$,'200923.00')
76996   fnAddOneC(mat filterAccount$,'200925.00')
76998   fnAddOneC(mat filterAccount$,'200930.01')
77000   fnAddOneC(mat filterAccount$,'201000.03')
77002   fnAddOneC(mat filterAccount$,'201003.01')
77004   fnAddOneC(mat filterAccount$,'201005.01')
77006   fnAddOneC(mat filterAccount$,'201015.02')
77008   fnAddOneC(mat filterAccount$,'201020.03')
77010   fnAddOneC(mat filterAccount$,'201025.00')
77012   fnAddOneC(mat filterAccount$,'201030.01')
77014   fnAddOneC(mat filterAccount$,'201032.00')
77016   fnAddOneC(mat filterAccount$,'201040.00')
77018   fnAddOneC(mat filterAccount$,'201045.00')
77020   fnAddOneC(mat filterAccount$,'201050.07')
77022   fnAddOneC(mat filterAccount$,'201055.00')
77024   fnAddOneC(mat filterAccount$,'201060.03')
77026   fnAddOneC(mat filterAccount$,'202000.00')
77028   fnAddOneC(mat filterAccount$,'202010.00')
77030   fnAddOneC(mat filterAccount$,'210000.00')
77032   fnAddOneC(mat filterAccount$,'210001.00')
77034   fnAddOneC(mat filterAccount$,'210002.00')
77036   fnAddOneC(mat filterAccount$,'210003.00')
77038   fnAddOneC(mat filterAccount$,'210004.00')
77040 fnend
78000 ! <updateable region: fn_open (supressprompt:=2)>  
78020 def fn_open(filename$*255, mat f$, mat fn, mat form$; inputonly, keynum, dont_sort_subs, path$*255, mat descr$, mat field_widths,dontupdate,___,index)
78040   dim _fileiosubs$(1)*800, loadedsubs$(1)*32
78060   fn_open=fnOpenFile(filename$, mat f$, mat fn, mat form$, inputonly, keynum, dont_sort_subs, path$, mat descr$, mat field_widths, mat _fileiosubs$,supressprompt:=2)
78080   if ~max(srch(loadedsubs$,uprc$(filename$)),0) then 
78100     mat loadedsubs$(udim(loadedsubs$)+1) 
78120     loadedsubs$(udim(loadedsubs$))=uprc$(filename$)
78140     for index=1 to udim(mat _fileiosubs$) 
78160       execute (_fileiosubs$(index)) 
78180     next index
78200   end if
78220 fnend
78240 ! </updateable region: fnopen>