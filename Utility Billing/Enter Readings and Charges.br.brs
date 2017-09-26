00010 ! formerly S:\acsUB\UBIpChg
00020 fn_setup
00030 fntop(program$)
00040 goto MENU1
02000 def fn_setup
02020   if ~setup then
02040     setup=1
02080     library 'S:\Core\Library': fnerror,fnopenprn,fncloseprn,fnmsgbox,fnchain,fnd1,fnxit,fntop
02100     library 'S:\Core\Library': fndate_mmddyy_to_ccyymmdd,fncomboa,fncustomer,fnhand_held_device$
02120     library 'S:\Core\Library': fngethandle,fnbutton,fnregistered_for_hh,fnretrieve_hand_held_file,fnask_account
02140     library 'S:\Core\Library': fnlbl,fntxt,fnacs,fntos,fnopt,fnchk,fnflexinit1,fnflexadd1
02160     library 'S:\Core\Library': fncmbact,fncmbrt2,fnfra,fncmdset,fncmdkey
02180     library 'S:\Core\Library': fncreg_read,fncreg_write,fngetdir2
02200     library 'S:\Core\Library': fnapply_default_rates,fnget_services,fnCopy
02210     library 'S:\Core\Library': fnstatus_close,fnindex_it
02220     on error goto ERTN
02240   ! dims, constants, top, etc
02250     dim resp$(40)*256
02260     dim aname$*30,d(15),alp$*1
02264     dim workFile$*256,workFileIndex$*256
02280     dim t(16),message$(1)*60,a$*256
02300     dim txt$*80,txt$(6)*70
02320     dim extra(23),item$(30)*20,extra$(11)*30
02340     dim x$*10,px$*10,x(15),ln$*256,last_ln$*256
02360     dim ft$*21,rm$*60,ra(2),colhdr$(30)*40,cm$(30)
02380     dim est1(3,3),e1$*30,e2$*30,a(7),tg(11)
02400     dim cd1(8)
02420     dim penalty$(10)*1,reporth$*300,form$*300,ctext$(10)*20
02440     dim mroll(3) ! meter roll code from hand held file
02460     dim serviceoption$(10)*25,srvnamc$(10)*21,srvnam$(10)*20,srv$(10)*2
02480     dim opt_final_billing$(5)*33
02500     let opt_final_billing$(1)="0 = Not Finaled"
02520     let opt_final_billing$(2)="1 = Final Bill"
02540     let opt_final_billing$(3)="2 = Final & Refund Deposit"
02560     let opt_final_billing$(4)="3 = Active, but do not Bill"
02580     let opt_final_billing$(5)="4 = Finaled, but not billed"
02600   ! 
02620     let fnd1(d1)
02700     if days(d1,'mmddyy')<days(date$('mmddyy'),'mmddyy')-15 then let d1=0
02720     open #1: "Name="&env$('Q')&"\UBmstr\Company.h"&env$('cno'),internal,input 
02740     read #1,using "form pos 130,n 4": pcent ioerr ignore ! percent for unusual usage
02760     close #1: 
02780     if pcent=0 then 
02800       let pcent=100
02820       open #1: "Name="&env$('Q')&"\UBmstr\Company.h"&env$('cno'),internal,outin 
02840       rewrite #1,using "form pos 130,n 4": pcent ioerr ignore ! percent for unusual usage
02860       close #1: 
02880     end if 
02900     let fncreg_read('unusual usage minimum water',uum_water$) : let uum_water=val(uum_water$)
02920     let fncreg_read('unusual usage minimum gas',uum_gas$) : let uum_gas=val(uum_gas$)
02940     let fncreg_read('unusual usage minimum electric',uum_electric$) : let uum_electric=val(uum_electric$)
02960   ! 
02980     let pcent=pcent*.01 ! convert unusual usage % to decimal
03000     let cancel=5
03020     let workFile$=env$('Q')&"\UBmstr\Reads_and_Chgs.h"&env$('cno')
03040     let workFileIndex$=env$('Q')&"\UBmstr\Reads_and_Chgs-Key.h"&env$('cno')
03060   ! synchronize these settings with S:\acsUB\ubCalk
03100     let from_holding_file=3 ! enumerations for addmethod
03120     let from_hh_file=5
03140   ! ______________________________________________________________________
03160   ! Open_Stuff: !
03180     let fn_setup_service(mat service_enabled)
03200     open #hTrans:=fngethandle: "Name="&env$('Q')&"\UBmstr\ubTransVB.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubTrIndx.h"&env$('cno')&",Shr,Use,RecL=102,KPs=1,KLn=19",internal,outin,keyed 
03220     open #hCustomer1:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,outin,keyed  ! was file #1, but it was getting closed incorrectly
03240   F_CUSTOMER_C: form pos 1,c 10,pos 41,c 30,pos 143,7*pd 2,pos 1821,n 1,pos 217,15*pd 5,pos 354,c 1,pos 1741,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,pos 1954,c 12,pos 1906,c 12
03260   ! let fn_CHECK_FOR_CALCULATION  ! checking to see if file is uncalculated - never could get it work correctly - need to know current billing date asked first and also need work file opened before this screen displayed (shows all records entered for file maintenance)
03300     open #hWork:=fngethandle: "Name="&workFile$&",KFName="&workFileIndex$&",Shr,Use,RecL=74,KPs=1,KLn=10",internal,outin,keyed 
03380     open #Customer2:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndx2.h"&env$('cno')&",Shr",internal,outin,keyed 
03400     open #Customer3:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndx3.h"&env$('cno')&",Shr",internal,outin,keyed 
03420     open #Customer4:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndx2.h"&env$('cno')&",Shr",internal,outin,keyed 
03440     open #hCustomer5:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&env$('cno')&",Shr",internal,outin,keyed 
03460     fncreg_read('Meter Reading Date Current',tmp$,date$("MMDDYY")) : d2=val(tmp$)
03480   end if
03500 fnend
11000 def fn_setup_service(mat service_enabled)
11010   ! r: older stuff
11012   let fnget_services(mat srvnam$,mat srv$,mat tax_code$,mat penalty$)
11050   for j=1 to udim(mat srvnam$)
11060     let srvnam$(j)=trim$(srvnam$(j))
11070     let srvnamc$(j)=srvnam$(j)&":"
11080   next j
11090   if srvnamc$(6)="Bad Check Charge:" then let srvnamc$(6)="Check Charge:"
11100   ! /r
11110   ! return explaind
11120   ! +10    penalty$(   ) <>  "Y"
11130   ! +100   srvnam$(    ) <>  ""
11140   mat service_enabled=(0)
11150   ! r: set mat service_is_not_blank
11160   for x=1 to udim(mat srvnam$)
11170     if srvnam$(x)<>"" then let service_is_not_blank(x)+=100
11180   next x
11190   ! /r
11200   ! r: set mat service_is_not_a_penalty
11210   for x=1 to udim(mat srvnam$)
11220     if penalty$(x) <>"Y" then let service_is_not_a_penalty(x)+=100
11230   next x
11240   ! /r
11250   ! r: set mat service_type
11260   ! service_type returns:
11270   !                         1   - Water
11280   !                         2   - Sewer
11290   !                         3   - Electric or SRV is EL
11300   !                         3.1 - Lawn Meter
11310   !                         3.2 - Reduc
11320   !                         4   -  Gas or SRV is GA
11330   !                         5   - Other
11340   for x=1 to udim(mat srvnam$)
11350     if srvnam$(x)='Water' then 
11360       let service_type(x)=1
11370     else if srvnam$(x)='Sewer' then 
11380       let service_type(x)=2
11390     else if srvnam$(x)="Electric" or srv$(x)="EL" then 
11400       let service_type(x)=3
11410     else if srvnam$(x)='Lawn Meter' then 
11420       let service_type(x)=3.1
11430     else if srvnam$(x)(1:5)='Reduc' then 
11440       let service_type(x)=3.2
11450     else if srvnam$(x)="Gas" or srv$(x)="GA" then 
11460       let service_type(x)=4
11470     else if uprc$(srvnam$(x)(1:5))="OTHER" then 
11480       let service_type(x)=5
11482     else if uprc$(srvnam$(x))="GAS CONNECT" then 
11484       let service_type(x)=6
11485     else if uprc$(srvnam$(x))="WATER CONNECT" then 
11486       let service_type(x)=7
11487     else if uprc$(srvnam$(x))="BAD CHECK CHARGE" then 
11488       let service_type(x)=8
11490     end if 
11500   next x
11510   ! /r
11520   ! r: set the mat service_enabled
11530   for x=1 to udim(mat srvnam$)
11540     if srvnam$(x)='Water' then 
11550       let service_enabled(x)+=1
11560     end if 
11570   next x
11580   if service_is_not_blank(1) then 
11590     let service_enabled(1)=1
11600   end if 
11610   ! 
11620   if service_is_not_blank(2) then 
11630     let service_enabled(2)=1
11640   end if 
11650   ! 
11660   if service_type(3)=3 then 
11670     let service_enabled(3)=1
11680   else if service_type(3)=3.1 then ! Lawn Meter" then
11690     let service_enabled(3)=2
11700   else if service_type(3)=3.2 then ! Reduc" then
11710     let service_enabled(3)=3
11720   end if 
11730   ! 
11740   if service_type(4)=4 then 
11750     let service_enabled(4)=1
11760   end if 
11770   ! 
11780   if service_type(5)=5 or (service_is_not_blank(5) and service_is_not_a_penalty(5)) then 
11790     let service_enabled(5)=1
11800   end if 
11810   ! 
11820   if service_type(6)=5 or (service_is_not_blank(6) and service_is_not_a_penalty(6)) then 
11830     let service_enabled(6)=1
11840   end if 
11850   ! 
11860   if service_type(7)=5 or (service_is_not_blank(7) and service_is_not_a_penalty(7)) then ! Service 7 seems to incompletly implemented
11870     let service_enabled(7)=1
11880   end if 
11890   ! 
11900   if service_type(8)=5 or (service_is_not_blank(8) and service_is_not_a_penalty(8)) then 
11910     let service_enabled(8)=1
11920   end if 
11930   ! 
11940   !   if service_is_not_blank(9) then
11950   !     let service_enabled(9)=1
11960   !   end if
11970   !   !
11980   !   if service_is_not_blank(10) then
11990   !     let service_enabled(10)=1
12000   !   end if
12010   ! /r
12020   ! r: set mat serviceoption$, serviceoption_count and service  (for fn_meter_change_out)
12030   let serviceoption_count=0
12040   if trim$(srvnam$(1))="Water" then 
12050     let serviceoption$(serviceoption_count+=1)=srv$(1)&"-"&trim$(srvnam$(1)(1:20))
12060     let service=service+1
12070   end if 
12080   if trim$(srvnam$(3))="Electric" or trim$(srv$(3))="EL" then 
12090     let serviceoption$(serviceoption_count+=1)=srv$(3)&"-"&srvnam$(3)(1:20)
12100     let service=service+1
12110   end if 
12120   if (trim$(srvnam$(4))="Gas" or trim$(srv$(4))="GA") then 
12130     let serviceoption$(serviceoption_count+=1)=srv$(4)&"-"&srvnam$(4)(1:20)
12140     let service=service+1
12150   end if 
12160   mat serviceoption$(serviceoption_count)
12170   ! /r
12180 fnend 
12600 AUTO_REC: ! r:
12610   let done_with_readings=0
12620   let fntos(sn$="ubipchg-3")
12630   let fnlbl(1,1,"Starting Account:" ,24,1)
12640   let fncmbact(1,26,1)
12650   let resp$(1)="[All]"
12660   let fncmdset(2)
12670   let fnacs(sn$,0,mat resp$,ck)
12680   if ck=cancel then 
12690     let done_with_readings=1
12700     goto MENU1
12710   end if 
12720   if uprc$(resp$(1))=uprc$("[All]") then let resp$(1)=""
12730   let x$=lpad$(trim$(resp$(1)(1:10)),10)
12740   let px$=x$
12750   if trim$(x$)="" then goto SEL_ACC
12760   read #hCustomer1,using F_CUSTOMER_C,key=x$,release: x$,aname$,mat a,final,mat d,alp$,mat extra,extra$(3) nokey AUTO_REC
12770   if addmethod=1 and env$('client')<>"Choctaw" then 
12780     let seq$=cnvrt$("pic(zz)",extra(1))&cnvrt$("pic(zzzzzzz)",extra(2))
12790     read #hCustomer5,using F_CUSTOMER_C,key=seq$,release: x$,aname$,mat a,final,mat d,alp$,mat extra,extra$(7),extra$(3) nokey AUTO_REC
12800   end if 
12802   let fnapply_default_rates(mat extra, mat a)
12810   if final=1 or final=2 or final=3 or (trim$(px$)<>"" and x$<>px$) then goto READ_ROUTE_SEQUENCE ! ken  ! john -- this is so users can select an account without being pushed to the incorrect account with the same rt/seq
12820   let px$=""
12830   goto ENTER_READING
12840 ! /r
13000 SEL_ACC: ! r:
13002 ! let passcheckwater=passcheckgas=passcheckelec=0
13004   if addmethod=1 then goto READ_ROUTE_SEQUENCE
13006   let x$=""
13008 SEL_ACT_TOS: ! 
13012   let ck=fnask_account('ubipchg',x$,hCustomer1)
13014   if ck=5 or ck=cancel then 
13016     let addmethod=0
13018     goto MENU1
13020   end if 
13145   if addmethod=1 then 
13146     goto READ_ROUTE_SEQUENCE
13148   else 
13150     read #hCustomer1,using 'form pos 41,c 30,pos 143,7*pd 2,pos 1821,n 1,pos 217,15*pd 5',key=x$,release: aname$,mat a,final,mat d nokey SEL_ACT_TOS
13152     let fnapply_default_rates(mat extra, mat a)
13155     goto ENTER_READING2
13158   end if 
13160 ! /r
13200 READ_ROUTE_SEQUENCE: ! r:
13205   if env$('client')="Choctaw" then ! read in Account order
13210     read #hCustomer1,using F_CUSTOMER_C: x$,aname$,mat a,final,mat d,alp$,mat extra,extra$(7),extra$(3) eof MENU1
13215   else 
13220     read #hCustomer5,using F_CUSTOMER_C,release: x$,aname$,mat a,final,mat d,alp$,mat extra,extra$(7),extra$(3) eof MENU1
13225   end if 
13226   let fnapply_default_rates(mat extra, mat a)
13230   if final=1 or final=2 or final=3 or (trim$(px$)<>"" and x$<>px$) then goto READ_ROUTE_SEQUENCE
13235   let px$=""
13240   goto ENTER_READING ! /r
14000 def fn_meter_roll
14020   mat txt$(4)
14040   let txt$(1)="Reading: "&str$(cur_read)&"   Prior: "&str$(prior_read)&"   Usage: "&str$(x0)
14060   let txt$(2)="Account: "&x$&" - "&aname$
14080   let txt$(3)="Negative Usage on "&sn$
14100   let txt$(4)="Is this a Meter Roll?"
14120   let fnmsgbox(mat txt$,resp$,'',35)
14140   if resp$="No" then 
14160     let passcheck=ckfail
14180     goto METER_ROLL_XIT
14200   else if mroll(1)=1 then 
14220     goto L3110
14240   end if 
14260   if uprc$(sn$)=uprc$(srvnam$(1)) then 
14280     let cde=1
14300   else 
14320     goto L3110
14340   end if 
14360   let xcde=1 : let xcd2=11 : let mroll(1)=1
14380   goto METER_ROLL_DONE ! water
14400   L3110: if mroll(3)=1 then goto L3140
14420   if uprc$(sn$)=uprc$(srvnam$(3)) then 
14440     let cde=5
14460   else 
14480     goto L3140
14500   end if 
14520   let xcde=3 : let xcd2=10 : let mroll(3)=1
14540   goto METER_ROLL_DONE ! electric
14560   L3140: ! 
14580   if mroll(2)=1 then 
14600     goto METER_ROLL_XIT
14620   end if 
14640   if uprc$(sn$)=uprc$(srvnam$(4)) then 
14660     let cde=9
14680   else 
14700     goto METER_ROLL_XIT
14720   end if 
14740   let xcde=2 : let xcd2=12 : let mroll(2)=1
14760   METER_ROLL_DONE: ! 
14780   let digits=len(str$(d(cde)))
14800   let x(xcde+xcd2)=10**digits-d(cde)+x(xcde)
14820   ! ** means to the power of
14840   METER_ROLL_XIT: ! 
14860 fnend 
16000 def fn_print_readings(hWork; printReadings_altHeading$*40) ! print proof of readings file
16020   let totwat=totele=totgas=0
16040   let fnopenprn
16060   let fn_printReadings_Heading( printReadings_altHeading$)
16080   restore #hWork: ! ,search>="": nokey PR_TOTALS    <-- needs to work with or without an index
16200   do 
16220     read #hWork,using F_WORK: x$,mat x eof PR_TOTALS
16240     let totwat+=x(1): let totele+=x(3): let totgas+=x(2)
16260     let e1$=e2$=""
16270     read #hCustomer1,using F_CUSTOMER_B,key=x$: e1$,e2$,mat d,f,mat a nokey PR_CUSTOMER_NOKEY
16280     F_CUSTOMER_B: form pos 11,2*c 30,pos 217,15*pd 5,pos 296,pd 4,pos 143,7*pd 2
16300     ! place usage in usage column if not usage already there so it shows on proof list
16320     ! Water
16340     if f<>d1 then let oldreading=d(1) else let oldreading=d(2)
16360     ! if x(12)=0 then let x(12)=x(1)-oldreading
16380     if x(12)=0 and a(1)<>0 then let x(12)=x(1)-oldreading ! A(1) checking was added 10/4/11 to prevent usage (and negative usage) on customers who have an (0) inactive rate code ! the whole line was commented out but added back in on 2/13/12
16400     ! Electric
16420     if f<>d1 then let oldreading=d(5) else let oldreading=d(6)
16440     if x(13)=0 then let x(13)=x(3)-oldreading
16460     ! Gas
16480     if f<>d1 then let oldreading=d(9) else let oldreading=d(10)
16500     if x(14)=0 and a(4)<>0 then let x(14)=max(0,x(2)-oldreading) ! A(4) checking was added 9/21/11 to prevent usage (and negative usage) on customers who have an (0) inactive rate code
16520     PR_CUSTOMER_NOKEY: ! 
16540     let rc=0
16560     mat pc_data=(0)
16580     if service_enabled(1) then 
16600       let pc_data(rc+=1)=x(1)
16620       let pc_data(rc+=1)=x(9)
16640       let pc_data(rc+=1)=x(12)
16660     end if 
16680     if service_enabled(2) then 
16700       let pc_data(rc+=1)=x(5)
16720     end if 
16740     if service_enabled(3) then 
16760       let pc_data(rc+=1)=x(3)
16780       let pc_data(rc+=1)=x(10)
16800       let pc_data(rc+=1)=x(13)
16820       let pc_data(rc+=1)=x(4)
16840     end if 
16860     if service_enabled(4) then 
16880       let pc_data(rc+=1)=x(2)
16900       let pc_data(rc+=1)=x(11)
16920       let pc_data(rc+=1)=x(14)
16940     end if 
16960     if service_enabled(5) then let pc_data(rc+=1)=x(6)
16980     if service_enabled(6) then let pc_data(rc+=1)=x(7)
17000     if service_enabled(7) then let pc_data(rc+=1)=x(8)
17020     let rc+=1
17040     if udim(mat pc_data)<rc then mat pc_data(rc)
17060     let pc_data(rc)=x(15) ! Final Billing Code
17080     mat pc_data(rc)
17100     print #255,using form$: x$,e2$(1:25),e1$(1:25),mat pc_data pageoflow PrintReadings_PgOf
17120   loop 
17140   PR_TOTALS: !
17150   print #255,using "form skip 2,c 30": "Batch Totals for Readings"
17160   print #255,using "form pos 1,c 10,nz 20,skip 1,pos 1,c 10,nz 20,skip 1,pos 1,c 10,nz 20,skip 1": srvnam$(1)(1:10),totwat,srvnam$(3)(1:10),totele,srvnam$(4)(1:10),totgas
17180   let fncloseprn
17220 fnend 
17240 def fn_printReadings_Heading(;altHeading$*40)
17260   if altHeading$='' then let altHeading$="Readings Proof List"
17280   print #255,using 'Form Pos 20,Cc 40': altHeading$
17300   print #255,using 'Form Pos 20,Cc 40': cnvrt$("pic(zz/zz/zz",d2)
17320   print #255,using 'Form POS 1,C 220': reporth$
17340 fnend 
17360 PrintReadings_PgOf: !
17380   print #255: newpage 
17400   let fn_printReadings_Heading( printReadings_altHeading$)
17420 continue 
17750 MAKE_CORRECTIONS: ! r:
17770   read #hWork,using F_WORK,key=x$: x$,mat x nokey MENU1
17790   let t(1)-=1 ! SUBTRACT PROOF TOTALS
17800   for j1=1 to 15 : let t(j1+1)-=x(j1) : next j1
17810   read #hCustomer1,using F_CUSTOMER_C,key=x$,release: x$,aname$,mat a,final,mat d,alp$,mat extra,extra$(3)
17812   let fnapply_default_rates(mat extra, mat a)
17820   let editmode=1
17840   goto ENTER_READING3 ! /r
17860 REWRITE_WORK: ! r:
17870   rewrite #hWork,using F_WORK,key=x$: trim$(x$),mat x nokey L3900
17880   goto L3910
17890 L3900: if trim$(uprc$(x$))<>trim$(uprc$("DELETED")) then let fn_writeWork(hWork,x$,mat x)
17900 L3910: if trim$(uprc$(x$))=trim$(uprc$("DELETED")) then goto MAKE_CORRECTIONS
17910   let fn_accumulateprooftotals
17920   if editmode=1 then return 
17930   goto MENU1 ! /r MAKE_CORRECTIONS
17950 CHANGE_ACT_NUM: ! r:
17960   let fntos(sn$="new_act_num")
17970   let mylen=19 : let mypos=mylen+2
17980   let fnlbl(1,1,"New Account:",mylen)
17990   let fntxt(1,mypos,10)
18000   let resp$(1)=""
18010   let fncmdset(1)
18020   let fnacs(sn$,0,mat resp$,ck)
18030   let x$=trim$(resp$(1))
18040   read #hCustomer1,using "Form POS 36,C 25",key=x$,release: aname$ nokey CHANGE_ACT_NUM
18050   goto REWRITE_WORK ! /r
18070 def fn_lo_pr_rec
18080   print #255,using "form pos 1,c 10,x 2,4*pic(zzzzzzzzzz)": x$,x(1),x(2),x(3),x(4)
18090 fnend  ! fn_lo_pr_rec
18160 def fn_accumulateprooftotals
18170   let t(1)+=1
18180   for j=1 to 15
18190     let t(j+1)+=x(j)
18200   next j
18210 fnend 
18220 def fn_checkwater
18230   if wr1=0 then let fn_us1
18240   if a(1)<>0 then ! skip routine if no water code
18250     let sn$=srvnam$(1)
18260     if trim$(srvnam$(1))="" or mroll(1)=1 or (d(wr1)=0 and x(1)=0) then 
18270       let passcheck=ckpass
18280       goto CHECKWATER_FINIS
18290     end if 
18300     if trim$(sn$)="Water" and x(12)>0 then 
18310       let passcheck=ckpass
18320       goto CHECKWATER_FINIS ! don't give warning if usage entered
18330     end if 
18335     if env$('client')="Billings" and len(str$(x(1)))=6 and len(str$(d(wr1)))=7 then let x(1)=val(str$(d(wr1))(1:1)&str$(x(1)))
18340     let x4=x(1)-d(wr1)
18350     let sn$=srvnam$(1) : let x0=x4 : let prior_read=d(wr1) : let cur_read=x(1)
18360     if x4>=0 then goto CHECKWATER_L4260
18370     if x(12)>0 then let sn$=srvnam$(1) : goto CHECKWATER_FINIS
18380     if x4<0 then let fn_meter_roll
18390   end if  ! a(1)<>0
18400   goto CHECKWATER_FINIS
18410   CHECKWATER_L4260: ! 
18420   if d(3)=0 then goto CHECKWATER_FINIS
18422   if uum_water<>0 and x0<uum_water then 
18424     let passcheck=ckpass
18430   else if x4<d(3)-d(3)*pcent or x4>d(3)+d(3)*pcent then 
18440     let passcheck=ckfail
18450   else 
18460     let passcheck=ckpass
18470   end if 
18480   CHECKWATER_FINIS: ! 
18490   let fn_checkend
18500 fnend 
18510 def fn_checkelec
18520   if er1=0 then let fn_us1
18530   if a(3)=0 then goto CHECKELEC_FINIS ! if no electric code skip
18540   if trim$(sn$)="Electric" and x(13)>0 then let passcheck=ckpass : goto CHECKELEC_FINIS ! don't give warning if usage entered
18550   if (service_type(3)=3 or (service_type(3)=3.1 and env$('client')<>"Thomasboro")) then 
18560     goto L4350
18570   else 
18580     let passcheck=ckpass
18590     goto CHECKELEC_FINIS
18600   end if 
18610   L4350: ! 
18620   if x(3)=0 and d(er1)=0 then goto CHECKELEC_FINIS
18630   let x2=x(3)-d(er1)
18640   let sn$=srvnam$(3) : let x0=x2 : : let prior_read=d(er1) : let cur_read=x(3)
18650   if x2>=0 then goto L4420
18660   if x(13)>0 then let sn$=srvnam$(3) : goto CHECKELEC_FINIS
18670   if x2<0 then let fn_meter_roll
18680   goto CHECKELEC_FINIS
18690   L4420: ! 
18700   if d(7)=0 then goto CHECKELEC_FINIS
18702   if uum_electric<>0 and x0<uum_electric then 
18704     let passcheck=ckpass
18710   else if x2<d(7)-d(7)*pcent or x2>d(7)+d(7)*pcent then 
18720     let passcheck=ckfail
18730   else 
18740     let passcheck=ckpass
18750   end if 
18760   CHECKELEC_FINIS: ! 
18770   let fn_checkend
18780 fnend 
18790 def fn_checkgas
18800   if a(4)=0 then goto CHECKGAS_FINIS ! skip if no gas codes
18810   let sn$=srvnam$(4)
18820   if trim$(srvnam$(4))<>"Gas" or mroll(2)=1 then 
18830     let passcheck=ckpass
18840     goto CHECKGAS_FINIS
18850   end if 
18860   if trim$(sn$)="Gas" and x(14)>0 then let passcheck=ckpass : goto CHECKGAS_FINIS ! don't give warning if usage entered
18870   if x(2)=0 and d(gr1)=0 then goto CHECKGAS_FINIS
18880   let x3=x(2)-d(gr1)
18890   let sn$=srvnam$(4): let x0=x3 : let prior_read=d(gr1): let cur_read=x(2)
18900   if x3>=0 then goto CHECKGAS_L4580
18910   if x(14)>0 then let sn$=srvnam$(4): goto CHECKGAS_FINIS
18920   if x3<0 then let fn_meter_roll
18930   goto CHECKGAS_FINIS
18940   CHECKGAS_L4580: ! 
18950   if d(11)=0 then goto CHECKGAS_FINIS
18952   if uum_gas<>0 and x0<uum_gas then 
18954     let passcheck=ckpass
18960   else if x3<d(11)-d(11)*pcent or x3>d(11)+d(11)*pcent then 
18970     let passcheck=ckfail
18980   else 
18990     let passcheck=ckpass
19000   end if 
19010   CHECKGAS_FINIS: ! 
19020   let fn_checkend
19030 fnend 
19040 def fn_checkend
19050   if addmethod=from_holding_file and passcheck=ckfail then 
19060     let fn_print_unusual
19070     goto CHECKEND_XIT
19080   end if 
19090   if passcheck=ckpass then goto CHECKEND_XIT
19100   if passcheck=ckfail and x0>=0 then 
19110     mat txt$(8) : let txt$(1)=sn$&" - Unusual Usage Warning"
19120     let txt$(2)="Account: "&x$&" - "&aname$ : let txt$(3)=""
19130     let txt$(4)="Reading: "&str$(cur_read)&"   Prior: "&str$(prior_read)&"   Calculated Usage: "&str$(x0) : let txt$(5)=""
19140     let txt$(6)="Yes = Continue, the usage is correct."
19150     let txt$(7)="No = Go Back, so I can re-enter the reading;"
19160     let txt$(8)="Cancel = Do not enter a reading for that Customer."
19170     let fnmsgbox(mat txt$,resp$,'',51)
19180   else 
19190     goto CHECKEND_XIT
19200   end if 
19210   if resp$="Yes" then 
19220     let passcheck=ckpass
19230   else if resp$="No" then 
19240     let passcheck=ckfail
19250   else if resp$="Cancel" then 
19260     let passcheck=ckfail
19270     let editmode=0
19280   end if 
19290   CHECKEND_XIT: ! 
19300 fnend 
19320 def fn_print_unusual
19330   let fnopenprn
19340   if ~setup_printunusual<=0 then 
19350     let setup_printunusual=1
19360     dim fmun$*80
19370     print #255: " Account    Name                    Old Reading  New Reading   Usage"
19380     print #255: "----------  ----------------------  -----------  -----------  -----------"
19390     let fmun$="form c 12,c 22,3*n 13,x 2,c 20,skip 1"
19400   end if 
19410   print #255,using fmun$: x$,e2$(1:20),prior_read,cur_read,x0,sn$
19420 fnend 
19520 def fn_hh_readings(ip1$; listonly) ! HH_READINGS: ! hand held routines
19530   ! r: get device for the client
19540   let device$=fnhand_held_device$
19550   ! /r
19560   if device$="Psion Workabout" then 
19570     goto HH_WORKABOUT
19580   else if device$="Badger" then 
19590     goto HH_BADGER
19600   else if device$="Boson" then 
19610     goto HH_BOSON
19620   else if device$="Laptop" then 
19630     gosub LAPTOP
19640     if listonly=1 then let fn_lo_pr_rec : goto HH_W_NXT
19650     goto HH_CONTINUE
19660   else ! if device$='Master Meter' or device$='READy Water' or device$="AMR" or device$="Other" or device$="Sensus" or device$="Green Tree" or device$="Hersey" or device$="EZReader" or device$="Itron FC300" or device$="" then 
19670     goto HH_OTHER
19680   end if
19760   HH_WORKABOUT: ! r: hand held routines for workabout
19780   open #h_readings:=13: "Name="&env$('Q')&"\UBmstr\Readings."&ip1$&",RecL=1",external,input,relative ioerr L4990
19790   goto L5000
19800   L4990: restore #h_readings: 
19810   L5000: if listonly=1 then let fnopenprn( 0,0,0,0, 'Book '&ip1$)
19820   let j1=29 : let j2=97
19830   HH_W_READ: ! 
19840   let ln$="" : mat x=(0)
19850   for j=j1 to j2
19860     read #h_readings,using "Form POS 1,C 1",rec=j: c$ norec HH_W_END
19870     let ln$=ln$&c$
19880   next j
19890   let x$=lpad$(trim$(ln$(1:10)),10) : let x(1)=val(ln$(11:19))
19900   let mroll(1)=val(ln$(20:20)) : let x(3)=val(ln$(21:29))
19910   let mroll(3)=val(ln$(30:30)) : let x(2)=val(ln$(31:39))
19920   let mroll(2)=val(ln$(40:40)) : let x(4)=val(ln$(41:49))
19930   let ft$=rtrm$(ln$(50:69))
19940   if ft$="00000000000000000000" then let ft$=""
19950   if listonly=1 then let fn_lo_pr_rec : goto HH_W_NXT
19960   if x$(1:1)="0" then let x$(1:1)=" " ! drop leading zero
19970   if file(255)=-1 and rtrm$(ft$)<>"" then 
19980     let fnopenprn
19990   end if 
20000   if trim$(ft$)<>"" then 
20010     print #255: "NEW NOTE! "&x$&" - "&ft$
20020   end if 
20030   goto HH_CONTINUE ! /r
20040   HH_BADGER: ! r: Hand Held routines for Badger (badger file is copied from                        \connect\connect\x to readings.x in the transfer from                           Hand Held routine)
20060   if listonly=1 then let fnopenprn
20070   close #h_readings: ioerr ignore
20080   open #h_readings:=13: "Name="&env$('Q')&"\UBmstr\Readings."&ip1$&",RecL=256",display,input 
20090   HH_BADGER_READ: linput #h_readings: ln$ eof HH_W_END
20100   if ln$(1:1)="T" or ln$(1:1)="H" then goto HH_BADGER_READ
20110   mat x=(0)
20120   let x$=lpad$(rtrm$(ln$(121:130)),10) conv HH_BADGER_READ ! Account Key
20130   let ti1=1: let ti1=val(ln$(64:64)) conv HH_BADGER_READ
20140   let x(ti1)=val(ln$(96:104)) conv HH_BADGER_READ
20150   ! if env$('client')="Moweaqua" Then Let X(TI1)=X(TI1)
20160   if listonly=1 then let fn_lo_pr_rec : goto HH_W_NXT
20170   goto HH_CONTINUE ! /r
20180   ! 
20190   HH_BOSON: ! r: Hand Held routines for Boson (boson file is copied from                        '&env$('Q')&'\UBmstr\outofpalm.txt in hhfro to readings.(route# (which is asked))
20210   let last_ln$=""
20220   if listonly=1 then let fnopenprn
20230   close #h_readings: ioerr ignore
20240   open #h_readings:=13: "Name="&env$('Q')&"\UBmstr\Readings."&ip1$&",RecL=204",display,input 
20250   HH_BOSON_READ: if last_ln$="" then linput #h_readings: ln$ eof HH_W_END else let ln$=last_ln$ : let last_ln$=''
20260   if ln$(1:1)="T" or ln$(1:1)="H" then goto HH_BOSON_READ
20270   mat x=(0)
20280   if env$('client')="Monticello" then let x$=lpad$(rtrm$(ln$(1:10)),10) conv HH_BOSON_READ ! Account Key :goto 5150
20300   let ti$=ln$(14:14)
20310   if ti$="W" or ti$="G" or ti$="E" then let x$=lpad$(rtrm$(ln$(4:13)),10) conv HH_BOSON_READ else let x$=lpad$(rtrm$(ln$(5:14)),10) conv HH_BOSON_READ : let ti$="" : let ti1=1
20370   if env$('client')="Monticello" then let ti1=1: goto L5420
20380   if uprc$(ti$)="W" then let ti1=1
20390   if uprc$(ti$)="G" then let ti1=2
20400   if uprc$(ti$)="E" then let ti1=3
20410   L5420: let x(ti1)=0: let x(ti1)=val(ln$(89:97)) conv L5440 ! kj 120308 allow boson to place codes in field if cant read meter
20415   ! if env$('client')="Billings" and ln$(91:91)=" " then x(ti1)=val("1"&ln$(92:97))
20420   if env$('client')="Moweaqua" and (a(1)=1 or a(1)=2) then let x(ti1)=round(x(ti1)*.1,0)
20430   if ti$="" or ti$="W" then 
20440     linput #h_readings: ln$ eof L5440
20450     if ti$="" then let x_g$=lpad$(rtrm$(ln$(5:14)),10) conv L5440 else let x_g$=lpad$(rtrm$(ln$(4:13)),10) conv L5440 : let ti$=ln$(14:14)
20460     if x_g$=x$ and ti$="" or ti$="G" then 
20470       let x(2)=val(ln$(89:97))
20480       let last_ln$=""
20490     else 
20500       let last_ln$=ln$
20510     end if 
20520   end if 
20530   L5440: ! 
20532   if env$('client')="Monticello" then 
20540     read #hCustomer1,using 'form pos 1954,c 12',key=lpad$(trim$(x$),10): extra$(7) nokey L5480 ! monticello
20560     if trim$(extra$(7))="22" then let x(ti1)=round(x(ti1)/100,0) ! monticello
20570   L5480: ! 
20572     if trim$(extra$(7))="23" then let x(ti1)=round(x(ti1)/10,0) ! monticello
20580   !       If TRIM$(EXTRA$(7))="24" or TRIM$(EXTRA$(7))="65" or TRIM$(EXTRA$(7))="66"Then don't do anything ! monticello
20590     if trim$(extra$(7))="66" then let x(ti1)=round(x(ti1)/10,0) ! monticello
20600     if trim$(extra$(7))="65" then let x(ti1)=round(x(ti1)/100,0) ! monticello
20602   end if  ! t$="Monticello"
20610   goto HH_CONTINUE ! /r
20620   LAPTOP: ! r: readings from a laptop using acs meter reading software
20640     if listonly=1 then let fnopenprn
20650     close #h_readings: ioerr ignore
20660     open #h_readings:=13: "Name="&env$('Q')&"\UBmstr\Readings."&ip1$&",RecL=50",display,input 
20670     HH_LAPTOP_READ: linput #h_readings: ln$ eof HH_W_END
20680     mat x=(0)
20690     let x$=lpad$(rtrm$(ln$(1:10)),10) conv HH_LAPTOP_READ ! Account Key
20700     ! 
20710     let ti1=1: let ti$=ln$(20:20) ! type of reading
20720     if uprc$(ti$)="W" then let ti1=1
20730     if uprc$(ti$)="E" then let ti1=2
20740     if uprc$(ti$)="G" then let ti1=3
20750     let x(ti1)=val(ln$(11:19)) conv HH_LAPTOP_READ
20760     read #hCustomer1,using F_CUSTOMER_C,key=x$,release: x$,aname$,mat a nokey ignore
20770   return ! if listonly=1 then let fn_lo_pr_rec : goto HH_W_NXT
20780   ! goto HH_CONTINUE ! /r
20790   HH_OTHER: ! r:
20800   if env$('client')="Oakland" then goto HH_OTHER_TYPE1
20810   if env$('client')="Lovington" then goto HH_OTHER_TYPE1
20820   ! if env$('client')="Gilbertown" then goto HH_OTHER_TYPE1 ! Green Tree
20830   !   if env$('client')="Albany" then goto HH_OTHER_TYPE1 ! AMR
20840   if env$('client')="GreenCo" then goto HH_OTHER_TYPE1 ! Hersey   or Ezreader
20850   if env$('client')="Brier Lake" then goto HH_OTHER_TYPE1
20852   if device$='READy Water' then goto HH_OTHER_TYPE1
20854   if device$='Master Meter' then goto HH_OTHER_TYPE1
20860   let fn_hh_other_type2
20870   goto HH_W_END
20880   HH_OTHER_TYPE1: ! 
20900   if listonly=1 then let fnopenprn
20910   close #h_readings: ioerr ignore
20930   open #h_readings:=13: "Name="&env$('Q')&"\UBmstr\Readings."&ip1$&",RecL=30",display,input 
20940   HH_OTHER_TYPE1_READ: ! 
20950   linput #h_readings: ln$ eof HH_W_END
20960   mat x=(0)
20970   let x$=lpad$(rtrm$(ln$(1:10)),10) conv HH_OTHER_TYPE1_READ ! Account Key
20980   let ti1=1 ! water
20982   let x(ti1)=0
20984   if device$='READy Water' then ! or env$('client')="Gilbertown"
20986     let x(ti1)=val(ln$(11:len(ln$))) conv ignore
20990   else if env$('client')="Lovington" then ! or env$('client')="Gilbertown"
20992     let x(ti1)=val(ln$(11:19)) conv ignore
21000   else
21010     let x(ti1)=val(ln$(11:20)) conv ignore
21020   end if
21030   read #hCustomer1,using F_CUSTOMER_C,key=x$,release: x$,aname$,mat a nokey ignore
21050   if listonly=1 then let fn_lo_pr_rec : goto HH_W_NXT
21060   goto HH_CONTINUE ! /r
21070   ! ______________________________________________________________________
21080   HH_CONTINUE: ! Continue with standard Hand Held routine
21090   read #hCustomer1,using F_CUSTOMER_C,key=x$,release: x$,aname$,mat a,final,mat d,alp$,mat extra,extra$(3) nokey HH_W_NXT
21100   let fn_us1
21110   mat est1=(0)
21120   if x(1)=999999 then let est1(1,1)=1 : let est1(1,2)=100
21130   if x(2)=999999 then let est1(3,1)=1 : let est1(3,2)=100
21140   if x(3)=999999 then let est1(2,1)=1 : let est1(2,2)=100
21150   if sum(est1)=0 then goto L6010
21160   read #hCustomer1,using F_CUSTOMER_A,key=x$,release: x$,e2$,mat a,f,final,mat d,mat extra,extra$(3) nokey HH_W_NXT
21170   gosub EST2B
21180   L6010: ! 
21190   gosub CHECK_UNUSUAL
21200   if skiprec=1 then let skiprec=0 : goto HH_W_NXT ! skip record   !kj 3/24/06
21210   let fn_writeWork(hWork,x$,mat x)
21220   let fn_accumulateprooftotals
21230   let fn_rmk1
21240   HH_W_NXT: ! 
21250   if device$="Badger" then 
21255     goto HH_BADGER_READ
21260   else if device$="Boson" then 
21265     goto HH_BOSON_READ
21270   else if device$="Laptop" then 
21271     goto HH_LAPTOP_READ
21272   else if device$="Psion Workabout" then 
21274     let j1+=72 
21276     let j2+=72
21278     goto HH_W_READ
21280   else ! if device$="Other" or device$="Sensus" or device$="AMR" or device$="Green Tree" or device$="Hersey" or device$="EZReader" then 
21285     goto HH_OTHER_TYPE1_READ
21288   end if
21310   ! ___________________________
21320   HH_W_END: ! 
21330   let fncloseprn
21340   let addmethod=2 ! set back to regular readings
21350   close #h_readings: 
21360 fnend  ! goto MENU1
21370 def fn_hh_other_type2
21390   open #h_readings:=13: "Name="&env$('Q')&"\UBmstr\Readings."&ip1$,display,input 
21400   dim hot_ver$*512,hot_line$*512
21410   linput #h_readings: hot_ver$
21420   let hot_ver$=trim$(hot_ver$)
21430   let hot_z_prior$=hot_z$=''
21440   if hot_ver$='[ACS Hand Held File Generic Version 2]' then 
21450     do 
21460       do 
21470         let hot_z_prior$=hot_z$
21480         linput #h_readings: hot_line$ eof HOT_EOF
21490         let fn_hot_parse_line(hot_line$)
21500       loop until hot_z$<>hot_z_prior$ and hot_z_prior$<>''
21510       if listonly=1 then 
21520         let fn_lo_pr_rec
21530       else 
21540         !       pause
21550         let fn_writeWork(hWork,hot_z_prior$,mat x)
21560       end if 
21570       let hot_z_prior$=hot_z$
21580       mat x=(0)
21590     loop 
21600     HOT_EOF: ! 
21610     let fn_writeWork(hWork,hot_z$,mat x)
21620   end if  ! hot_ver$='[ACS Hand Held File Generic Version 2]'
21630 fnend  ! fn_hh_other_type2
21640 def fn_hot_parse_line(line$*512)
21650   ! sets any one of the following local variables each call:
21660   ! hot_z$, mat x
21670   let pos_equal=pos(line$,'=')
21680   dim line_field$*256
21690   dim line_value$*256
21700   let line_field$=line$(1:pos_equal-1)
21710   let line_value$=line$(pos_equal+1:len(line$))
21720   let line_value=0
21730   let line_value=val(line_value$) conv HPL_LV_CONV
21740   let line_field_len=len(line_field$)
21750   let line_field$=lwrc$(trim$(line_field$))
21760   let line_field_pos_dot1=pos(line_field$,'.')
21770   if line_field_pos_dot1>0 then 
21780     dim line_field$(2)*256
21790     mat line_field$(2)
21800     let line_field$(1)=line_field$(1:line_field_pos_dot1-1)
21810     let line_field$(2)=line_field$(line_field_pos_dot1+1:line_field_len)
21820   end if  ! 
21830   if line_field$(2)='kwh' then let line_field$(2)="electric"
21840   HPL_LV_CONV: ! 
21850   if line_field$(1)="customer" then 
21860     if line_field$(2)="number" then 
21870       let hot_z$=line_value$
21880     end if 
21890   else if line_field$(1)="reading" then 
21900     if line_field$(2)="water" then 
21910       let x(1)=line_value
21920     else if line_field$(2)="gas" then 
21930       let x(2)=line_value
21940     else if line_field$(2)="electric" then 
21950       let x(3)=line_value
21960     else if line_field$(2)="demand" then 
21970       let x(4)=line_value
21980     end if 
21990   else if line_field$(1)="charge" then 
22000     if line_field$(2)="sewer" then 
22010       let x(5)=line_value
22020     else if line_field$(2)="sanitation" then 
22030       let x(6)=line_value
22040     else if line_field$(2)="fire protection" then 
22050       let x(7)=line_value
22060     else if line_field$(2)="other" then 
22070       let x(8)=line_value
22080     else if line_field$(2)="water" then 
22090       let x(9)=line_value
22100     else if line_field$(2)="electric" then 
22110       let x(10)=line_value
22120     else if line_field$(2)="gas" then 
22130       let x(11)=line_value
22140     end if 
22150   else if line_field$(1)="used" or line_field$(1)="usage" then 
22160     if line_field$(2)="water" then 
22170       let x(12)=line_value
22180     else if line_field$(2)="kwh" then 
22190       let x(13)=line_value
22200     else if line_field$(2)="gas" then 
22210       let x(14)=line_value
22220     end if 
22230   else if line_field$(1)="final billing code" then 
22240     let x(15)=line_value
22250   else if line_field$(1)="meter" then 
22260     if line_field$(2)="tamper" then 
22270       let fn_write_tamper(hot_z$,line_value)
22280     end if 
22290   end if 
22300 fnend 
22320 EST1: ! r: ESTIMATEING ROUTINE
22330   close #hWork: 
22340   execute 'Index '&workFile$&' '&workFileIndex$&' 1 10 Replace,DupKeys -n'
22360   open #hWork:=fngethandle: "Name="&workFile$&",KFName="&workFileIndex$,internal,outin,keyed 
22370   ASK_EST: ! 
22380   let fntos(sn$="ubipchg-ask_est")
22390   ! let services=0
22400   if srvnam$(1)="Water" then let srvest$(1)=srvnam$(1) else let srvest$(1)="Unused"
22410   if service_enabled(3) then let srvest$(2)=srvnam$(3) else let srvest$(2)="Unused"
22420   if service_enabled(4) then let srvest$(3)=srvnam$(4) else let srvest$(3)="Unused"
22430   let mylen=0
22440   for j=1 to 3 : let mylen=max(mylen,len(srvest$(j))) : next j
22450   let fnfra(1,1,8,mylen+50,"Select and Configure Services to Estimate")
22460   let fnlbl(2,mylen+12,"% of Average",15,0,0,1)
22470   for j=1 to 3
22480     let resp$(j*2-1)="False" : let resp$(j*2)=""
22490     if srvest$(j)="" or srvest$(j)="Unused" then let disable=1 else let disable=0
22500     let fnchk(j+2,mylen,srvest$(j),0,1) ! add disable here
22510     ! Let FNTXT(J+2,MYLEN+4,2,0,0,"30",DISABLE,EMPTY$,1)
22520     let fntxt(j+2,mylen+14,3,0,0,"30",disable,empty$,1)
22530   next j
22540   let fnlbl(7,1,"% of average would normally be from 75 to 125 %",52,2,0,1)
22550   let fnfra(11,1,3,50,"Account Selection Method")
22560   let fnopt(1,1,"Individual Accounts",0,2)
22570   let resp$(7)="False"
22580   let fnopt(2,1,"Route",0,2)
22590   let resp$(8)="True"
22600   let fncmdset(2)
22610   let fnacs(sn$,0,mat resp$,ck)
22620   if ck=cancel then goto MENU1
22630   for j=1 to 3
22640     if uprc$(resp$(j*2-1))=uprc$("True") then let est1(j,1)=1 else let est1(j,1)=0
22650     let est1(j,2)=val(resp$(j*2)) conv EST1
22660   next j
22670   if est1(1,1)=0 and est1(2,1)=0 and est1(3,1)=0 then goto L6520 else goto L6530
22680   L6520: ! 
22690   mat message$(1)
22700   let message$(1)="You must select at least one service to estimate"
22710   let fnmsgbox(mat message$,resp$,'',0)
22720   goto ASK_EST
22730   L6530: for j=1 to 3
22740     if est1(j,1)=0 then goto L6570
22750     if est1(j,2)<50 or est1(j,2)>150 then goto L6560 else goto L6570
22760   L6560: ! 
22770     mat message$(1): let mytype=0
22780     let message$(1)="You percent must be between 50% and 150%"
22790     let fnmsgbox(mat message$,resp$,'',mytype)
22800     goto ASK_EST
22810   L6570: ! 
22820   next j
22830   if ck=cancel then goto MENU1
22840   if uprc$(resp$(7))=uprc$("True") then let est1=1
22850   if uprc$(resp$(8))=uprc$("True") then let est1=2 ! select route #
22860   let fn_est_dates
22870   if est1=1 then goto EST3 ! selecting individual accounts to estimate
22880   if est1=2 then goto ASK_ROUTE
22890 goto ASK_EST ! /r
22910 EST3: ! r:
22920   let fntos(sn$="ubipchg-est3")
22930   let mylen=27 : let mypos=mylen+2
22940   let fnlbl(1,1,"Account to Estimate:",mylen,1)
22950   let fncmbact(1,mypos)
22960   let resp$(1)=""
22970   if ex$<>"" then 
22980     let fnlbl(3,1,"Last Account entered:",mylen,1)
22990     let fntxt(3,mypos,10,0,0,empty$,1)
23000     let resp$(2)=ex$
23010   end if 
23020   let fncmdset(11)
23030   let fnacs(sn$,0,mat resp$,ck)
23040   let x$=lpad$(trim$(resp$(1)(1:10)),10)
23050   if ck=cancel or trim$(x$)="" then goto MENU1
23060   let x$=lpad$(trim$(x$),10) conv EST3
23070   read #hCustomer1,using F_CUSTOMER_A,key=x$,release: x$,e2$,mat a,f,final,mat d,mat extra,extra$(3) nokey EST3
23072   let fnapply_default_rates(mat extra, mat a)
23080   F_CUSTOMER_A: form pos 1,c 10,pos 41,c 30,pos 143,7*pd 2,pos 296,pd 4,pos 1821,n 1,pos 217,15*pd 5,pos 1741,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,c 30,7*c 12,3*c 30
23090   gosub EST2
23100   let ex$=x$
23110   goto EST3 ! /r
23130 ASK_ROUTE: ! r:
23140   let fntos(sn$="ubipchg-ask_Route")
23150   let mylen=21 : let mypos=mylen+2 : let respc=0
23160   let fnlbl(1,1,"Route to Estimate:",mylen,1)
23170   let fncmbrt2(1,mypos,0)
23180   let resp$(respc+=1)="1"
23190   if eb2>0 then 
23200     let fnlbl(3,1,"Last Route estimated:")
23210     let fntxt(3,mypos,4)
23220     let resp$(respc+=1)=str$(eb2)
23230   end if 
23240   let fncmdset(11)
23250   let fnacs(sn$,0,mat resp$,ck)
23260   if resp$(1)="[All]" then let eb1=0 : goto L6890
23270   let eb1=val(resp$(1))
23280 L6890: ! 
23290   if ck=cancel then goto MENU1 ! finish
23300   restore #hCustomer1: 
23310 READ_CUSTOMER: ! 
23320   read #hCustomer1,using F_CUSTOMER_A,release: x$,e2$,mat a,f,final,mat d,mat extra,extra$(3) eof ASK_ROUTE
23330   if final=1 or final=2 or final=3 then goto READ_CUSTOMER
23332   let fnapply_default_rates(mat extra, mat a)
23340 ! let fn_US1
23350   if eb1>0 and extra(1)><eb1 then goto READ_CUSTOMER ! if route selected and does not match route
23360   if final=1 or final=3 then goto READ_CUSTOMER ! SKIP FINAL BILLED
23370   gosub EST2
23380   let eb2=eb1
23390   goto READ_CUSTOMER ! /r
23400 EST2: ! r:
23410   mat x=(0) ! actually calculate the estimated usage
23420   EST2B: ! 
23430   let a1=est4=0
23440   read #hWork,using F_WORK,key=x$: x$,mat x nokey L7060
23450   let a1=1
23460   let t(1)-=1 ! Reverse Proof Totals
23470   for j=1 to 15 : let t(j+1)=t(j+1)-x(j) : next j
23480   L7060: ! 
23490   for j=1 to 3
23500     if j=1 and a(1)=0 then 
23510       goto L7140
23520     else if j=2 and a(3)=0 then 
23530       goto L7140
23540     else if j=3 and a(4)=0 then 
23550       goto L7140 ! took this off front 71509  If EST1(J,1)=0 Then Goto 6790 Else 
23560     end if 
23570     let fn_est5
23580     if f=d1 then let oldwatread=d(2) else let oldwatread=d(1) ! old water reading equals the prior reading if recalculation else current reading if new calculation
23590     if f=d1 then let oldelecread=d(6) else let oldelecread=d(5) ! old electric reading equals the prior reading if recalculation else current reading if new calculation
23600     if f=d1 then let oldgasread=d(10) else let oldgasread=d(9) ! old gas reading equals the prior reading if recalculation else current reading if new calculation
23610     if j=1 then 
23620       let x(1)=oldwatread+watavg
23630     else if j=2 then 
23640       let x(3)=oldelecread+elecavg
23650     else if j=3 then 
23660       let x(2)=oldgasread+gasavg
23670     end if 
23680     let est4=1
23690     L7140: ! 
23700   next j
23710   ! If A(2)>0 AND EST1(1,1)=1 Then Let EST4=1 ! Sewer
23720   if est4=0 then goto L7220
23730   if addmethod=from_holding_file then goto L7220 ! FROM Hand Held
23740   if a1=1 then 
23750     rewrite #hWork,using F_WORK: trim$(x$),mat x
23760     goto L7210
23770   end if 
23780   let fn_writeWork(hWork,x$,mat x)
23790   rewrite #hCustomer1,using "Form pos 1831,n 9",key=x$: d1 ! write billing date into bill estimated field  extra(19) any time bill estimated
23800   L7210: ! 
23810   let fn_accumulateprooftotals
23820   L7220: ! 
23830 return  ! /r
34040 XIT: let fnxit
34050 IGNORE: continue 
34320 def fn_us1
34340   let rc1=0 ! SET USAGE FIELDS
34360   let wr1=1 : let er1=5 : let gr1=9
34380   read #hCustomer1,using "Form POS 296,PD 4",key=x$,release: f nokey US1_XIT
34400   if f><d1 then goto US1_XIT
34420   let wr1=2 : let er1=6 : let gr1=10 : let rc1=1 ! Re-Calculation
34440   US1_XIT: ! 
34460 fnend 
34480 def fn_rmk1
34500   ! let rk$=x$(1:10)
34520   if ft$="" then goto RMK1_XIT
34540   let ft$="*"&ft$
34560   ! Read #note1,Using "Form POS 1,C 10,2*PD 3",Key=RK$: RK$,MAT RA Nokey 6580
34580   let r32=ra(1)
34600   RMK1_L8110: ! 
34620   if r32=0 then goto RMK1_L8190
34640   ! Read #note2,Using "Form POS 1,C 10,C 60,PD 3",Rec=R32: K32$,RM$,N32
34660   if rm$(1:1)><"*" then goto RMK1_L8160
34680   ! Rewrite #note2,Using "Form POS 1,C 10,C 60,PD 3",Rec=R32: K32$,FT$
34700   goto RMK1_XIT
34720   RMK1_L8160: ! 
34740   let r32=n32
34760   goto RMK1_L8110
34780   mat ra=(0)
34800   ! Write #note1,Using "Form POS 1,C 10,2*PD 3": RK$,MAT RA
34820   RMK1_L8190: ! 
34840   let r32=lrec(32)+1
34860   ! Write #note2,Using "Form POS 1,C 10,C 60,PD 3",Rec=R32: RK$,FT$,0
34880   let rn=rn+1
34900   ! If RA(2)>0 Then
34920   ! Rewrite #note2,Using "Form POS 68,PD 3",Rec=RA(2): R32
34940   ! end if
34960   if ra(1)=0 then let ra(1)=r32
34980   let ra(2)=r32
35000   ! Rewrite #note1,Using "Form POS 11,2*PD 3",Key=RK$: MAT RA
35020   RMK1_XIT: ! 
35040 fnend 
35060 ! <Updateable Region: ERTN>
35080 ERTN: let fnerror(program$,err,line,act$,"XIT")
35100   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
35120   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
35140   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
35160 ERTN_EXEC_ACT: execute act$ : goto ERTN
35180 ! /region
35200 def fn_est_dates
35220   EST_DATES: ! 
35240   let fntos(sn$="estimate-1")
35260   let mylen=51 : let mypos=mylen+2
35280   let fnlbl(2,70,"",0,1)
35300   let fnlbl(1,1,"Billing Dates of Months to be Averaged:",mylen,1)
35320   for j=1 to 8
35340     let fntxt(j,mypos,10,0,0,"3")
35360     let resp$(j)=""
35380   next j
35400   let fncmdset(2)
35420   let fnacs(sn$,0,mat resp$,ckey)
35440   if ckey=cancel then goto XIT
35460   for j=1 to 8
35480     let cd1(j)=val(resp$(j)) conv EST_DATES
35500   next j
35520   if cd1(1)=0 then 
35540     mat message$(1)
35560     let message$(1)="You must enter at least one date!"
35580     let fnmsgbox(mat message$,resp$)
35600     goto EST_DATES
35620   end if 
35640 fnend 
35660 def fn_est5(;___,j) ! calculate averages
35680   let watermonths=elecmonths=gasmonths=watused=elecused=gasused=0
35700   restore #hTrans,key>=x$&"         ": nokey EST5_XIT ! no average but active customer (use 0 usage)
35720   EST5_READ_TRANS: ! 
35740   read #hTrans,using F_TRANS: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof EST5_FINIS
35760   F_TRANS: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
35780   if p$<>x$ then goto EST5_FINIS
35800   if tcode<>1 then goto EST5_READ_TRANS ! only charge transactions
35820   for j=1 to 8
35840     if est1(1,1)=1 and cd1(j)=tdate then let watermonths+=1: let watused+=wu
35860     if est1(2,1)=1 and cd1(j)=tdate then let elecmonths+=1: let elecused+=eu
35880     if est1(3,1)=1 and cd1(j)=tdate then let gasmonths+=1 : let gasused+=gu
35900   next j
35920   ! probably a mat x proof total problem right here
35940   goto EST5_READ_TRANS
35960   EST5_FINIS: ! 
35980   let watavg=elecavg=gasavg=0
36000   if watermonths>0 then let watavg=int(watused/watermonths)
36020   if elecmonths>0 then let elecavg=int(elecused/elecmonths)
36040   if gasmonths>0 then let gasavg=int(gasused/gasmonths)
36060   EST5_XIT: ! write enter readings entry
36080 fnend 
38700 def fn_rewrite_usage
38720   if servicetype$="WA" then let x(12)=usage
38740   if servicetype$="GA" then let x(14)=usage
38760   if servicetype$="EL" then let x(13)=usage
38780   rewrite #hWork,using F_WORK: trim$(x$),mat x
38800 fnend 
38820 def fn_sel_act
38840   let fntos(sn$="Sel_Act")
38860   let fnlbl(1,1,"Hand Held model:",16,1)
38880   let ctext$(1)="Psion Workabout"
38900   let ctext$(2)="Psion Organizer"
38920   let ctext$(3)="DriveBy"
38940   let ctext$(4)="Unisys"
38960   let ctext$(5)="Badger"
38980   let ctext$(6)="Boson"
39000   let ctext$(7)="Sensus"
39020   let ctext$(8)="LapTop"
39040   let ctext$(9)="Green Tree"
39060   let ctext$(10)="AMR"
39080   let fncomboa("HH-FroCBox",1,18,mat ctext$)
39100   let resp$(1)=ctest$(9)
39120   let fncmdkey("&Next",1,1,0): let fncmdkey("&Cancel",5,0,1)
39140   let fnacs(sn$,0,mat resp$,ckey)
39160   if ckey=5 then goto XIT
39180   let device$=resp$(1)
39200 fnend 
39520 def fn_write_tamper(custno$*10,tval)
39540   read #hCustomer1,using TMPFORM,key=lpad$(trim$(custno$),10): tmp$ nokey WT_XIT
39560   rewrite #hCustomer1,using TMPFORM: lpad$(str$(tval),2)
39580   TMPFORM: form pos 438,c 2
39600   WT_XIT: ! 
39620 fnend 
40000 INPUT_TEXT: ! r:
40020 ! r: phase 1 - import from text file into readings.tmp file
40040   open #h_tmp:=fngethandle: "Name=OPEN:Tab Delimited Text (*.txt) |*.txt,RecL=129,Shr",display,input ioerr IT_XIT
40060 !   open #h_tmp:=2: "Name=L:\readings.txt,RecL=129",display,input
40080   open #h_readings_tmp:=fngethandle: "Name="&env$('Temp')&"\readings.tmp,RecL=30,replace",display,output 
40100   do 
40120 IT_TEXT_READ: ! 
40140     linput #h_tmp: a$ eof IT_TEXT_EOF
40160     let x=val(a$(1:3)) conv IT_TEXT_READ
40180     let z$=""
40200     for j=1 to 8
40220       let x=val(a$(j:j)) conv IT_L1060
40240       let z$=z$&a$(j:j)
40260     next j
40280 IT_L1060: ! 
40300     let z=val(z$)
40320     let z$=cnvrt$("pic(zzzzzzz.##",z)
40340     let reading$=""
40360     for j1=1 to 20
40380       let x=val(a$(j1+j:j1+j)) conv IT_L1120
40400       let reading$=reading$&a$(j1+j:j1+j)
40420 IT_L1120: ! 
40440     next j1
40460     print #h_readings_tmp,using "form pos 1,c 10,c 9": z$,trim$(reading$)
40480   loop 
40500 IT_TEXT_EOF: ! 
40520   close #h_tmp: ioerr ignore
40540   close #h_readings_tmp: ioerr ignore
40560 ! /r
40580 ! r: phase 2 - from readings.tmp file
40600   close #h_readings: ioerr ignore
40620   open #h_readings:=13: "Name="&env$('temp')&"\Readings.tmp,RecL=30",display,input 
40640   do 
40660     linput #h_readings: ln$ eof IT_FINIS
40680     mat x=(0)
40700     let x$=lpad$(rtrm$(ln$(1:10)),10) conv IT_W_NEXT ! Account Key
40720     let ti1=1 ! water
40740     if env$('client')="Lovington" then let x(ti1)=0: let x(ti1)=val(ln$(11:19)) conv IT_L5870 : goto IT_L5870
40760     ! if env$('client')="Gilbertown" then let x(ti1)=0: let x(ti1)=val(ln$(11:19)) conv IT_L5870 : goto IT_L5870
40780     let x(ti1)=0: let x(ti1)=val(ln$(11:20)) conv ignore
40800 IT_L5870: ! 
40820     read #hCustomer1,using F_CUSTOMER_C,key=x$,release: x$,aname$,mat a nokey ignore
40860 !       goto HH_CONTINUE
40880 !   ! ______________________________________________________________________
40900 !   HH_CONTINUE: ! Continue with standard Hand Held routine
40920     read #hCustomer1,using F_CUSTOMER_C,key=x$,release: x$,aname$,mat a,final,mat d,alp$,mat extra,extra$(3) nokey IT_W_NEXT
40930     let fnapply_default_rates(mat extra, mat a)
40940     let fn_us1
40960     mat est1=(0)
40980     if x(1)=999999 then let est1(1,1)=1 : let est1(1,2)=100
41000     if x(2)=999999 then let est1(3,1)=1 : let est1(3,2)=100
41020     if x(3)=999999 then let est1(2,1)=1 : let est1(2,2)=100
41040     if sum(est1)<>0 then 
41060       read #hCustomer1,using F_CUSTOMER_A,key=x$,release: x$,e2$,mat a,f,final,mat d,mat extra,extra$(3) nokey IT_W_NEXT
41070       let fnapply_default_rates(mat extra, mat a)
41080       gosub EST2B
41100     end if 
41120     gosub CHECK_UNUSUAL
41160     let fn_writeWork(hWork,x$,mat x)
41180     let fn_accumulateprooftotals
41200     let fn_rmk1
41220 IT_W_NEXT: ! 
41240   loop 
41260 ! /r
41280 IT_FINIS: ! 
41300   let fncloseprn
41320   let addmethod=2 ! set back to regular readings
41340   close #h_readings,free: 
41350   execute 'free '&workFileIndex$ ioerr ignore
41360 IT_XIT: ! 
41380   goto MENU1 ! /r
42000 MENU1: ! r:
42020   let editmode=0
42060   let addmethod=0
42080   let fntos(sn$="readings-1b")
42100   let mylen=28 : let mypos=mylen+3
42120   let frame_bd_witdh=42
42140   let fnfra(1,1,5,frame_bd_witdh,"Batch Data")
42160   let disable=0 ! If LREC(hWork)>1 Then Let DISABLE=1 Else Let DISABLE=0
42180   let fnlbl(2,2,"Billing Date (mmddyy):",mylen,1,0,1)
42200   let fntxt(2,mypos,8,0,0,"1001",disable,empty$,1)
42220   let resp$(1)=str$(d1)
42240   let fnlbl(4,2,"Meter Reading Date (mmddyy):",mylen,1,0,1)
42260   let fntxt(4,mypos,8,0,0,"1",disable,empty$,1)
42280   resp$(2)=str$(d2)
42300 ! 
42640   let moe_button_width=frame_bd_witdh-1
42660   let fnfra(8,1,20,moe_button_width+1,"Add Readings") : let frame_current=2 : let frame_line=0
42680   let fnlbl(frame_line+=2,2,"Individuals:",0,0,0,frame_current)
42700   let fnbutton(frame_line+=1,1,"Display customers in route sequence",2001,'Display each customer in route sequence',0,moe_button_width,frame_current)
42720   let fnbutton(frame_line+=1,1,"Ask Account, then enter Reading",2002,'Ask Account, then enter Reading',0,moe_button_width,frame_current)
42740 ! 
42760   let fnlbl(frame_line+=2,2,"Bulk:",0,0,0,frame_current)
42780   let fnbutton(frame_line+=1,1,"Load Holding File",2003,'Retrieve readings previously saved to a Holding File',0,moe_button_width,frame_current)
42800   let fnbutton(frame_line+=1,1,"Estimate Readings",2004,'',0,moe_button_width,frame_current)
42820   let fnbutton(frame_line+=1,1,"Import from Tab Delimited Text File",2006,'',0,moe_button_width,frame_current)
42840 ! 
42860   if fnregistered_for_hh then 
42880     let fnlbl(frame_line+=2,2,"Hand Held:",0,0,0,frame_current)
42900     let fnbutton(frame_line+=1,1,"Import from Hand Held to Book",2007,'Retrieve Hand Held File',0,moe_button_width,frame_current)
42920     let fnbutton(frame_line+=1,1,"Load Hand Held Book",2005,'Generally for use after "Retreive (Import) from Hand Held to Book"',0,moe_button_width,frame_current)
42940   end if  ! fnregistered_for_hh
44000 ! r: add the grid
44010 ! 
44020   let chc=0
44030   mat colhdr$(30)
44040   mat cm$(30)
44050   mat item$(30)
44060   let colhdr$(chc+=1)="Account"
44070   let reporth$="   Account  Customer Name              Customer Address           "
44080   let form$="Form pos 1,c 10,x 2,c 25,x 2,c 25"
44090 ! r: Service 1 - Water
44100   if service_enabled(1) then 
44110     let colhdr$(chc+=1)=srvnam$(1)&" Reading"
44120     let cm$(chc)="20"
44130     let colhdr$(chc+=1)=srvnam$(1)&" Charge"
44140     let cm$(chc)="10"
44150     let colhdr$(chc+=1)=srvnam$(1)&" Usage"
44160     let cm$(chc)="20"
44170 ! 
44180     let reporth$=reporth$&srvnam$(1)(1:2)&" Read   "&srvnam$(1)(1:2)&" Chg  "&srvnam$(1)(1:2)&" Usage "
44190     let form$=form$&",n 9,n 9.2,n 9"
44200   end if 
44210 ! /r
44220 ! r: Service 2 - Sewer
44230   if service_enabled(2) then 
44240     let colhdr$(chc+=1)=srvnam$(2)&" Charge"
44250     let cm$(chc)="10"
44260 ! 
44270     let reporth$=reporth$&" "&srvnam$(2)(1:2)&" Chg  "
44280     let form$=form$&",n 9.2"
44290   end if 
44300 ! /r
44310 ! r: Service 3 - Electric
44320   if service_type(3)=3 then 
44330     let colhdr$(chc+=1)=srvnam$(3)&" Reading"
44340     let cm$(chc)="20"
44350     let colhdr$(chc+=1)=srvnam$(3)&" Charge"
44360     let cm$(chc)="10"
44370     let colhdr$(chc+=1)=srvnam$(3)&" Usage"
44380     let cm$(chc)="20"
44390     let colhdr$(chc+=1)="Demand"
44400     let cm$(chc)="20"
44410 ! 
44420     let reporth$=reporth$&srvnam$(3)(1:2)&" Read   "&srvnam$(3)(1:2)
44430     let reporth$=reporth$&" Chg  "&srvnam$(3)(1:2)&" Usage   Demand "
44440     let form$=form$&",n 9,n 9.2,n 9,n 9"
44450   else if service_type(3)=3.1 then 
44460     let colhdr$(chc+=1)=srvnam$(3)&" Reading"
44470     let cm$(chc)="20"
44480     let colhdr$(chc+=1)=srvnam$(3)&" Charge"
44490     let cm$(chc)="10"
44500     let colhdr$(chc+=1)=srvnam$(3)&" Usage"
44510     let cm$(chc)="20"
44520 ! 
44530     let reporth$=reporth$&srvnam$(3)(1:2)&" Read   "&srvnam$(3)(1:2)
44540     let reporth$=reporth$&" Chg  "&srvnam$(3)(1:2)&" Usage "
44550     let form$=form$&",n 9,n 9.2,n 9"
44560   else if service_type(3)=3.2 then 
44570     let colhdr$(chc+=1)=srvnam$(3)&" Usage"
44580     let cm$(chc)="20"
44590 ! 
44600 ! LOOKS LIKE SOMETHING IS MiSSING HERE
44610   end if 
44620 ! /r
44630 ! r: Service 4 - Gas
44640   if service_enabled(4) then ! if service_type(4)=4 then
44650     let colhdr$(chc+=1)=srvnam$(4)&" Reading"
44660     let cm$(chc)="20"
44670     let colhdr$(chc+=1)=srvnam$(4)&" Charge"
44680     let cm$(chc)="10"
44690     let colhdr$(chc+=1)=srvnam$(4)&" Usage"
44700     let cm$(chc)="20"
44710 ! 
44720     let reporth$=reporth$&srvnam$(4)(1:2)&" Read   "&srvnam$(4)(1:2)
44730     let reporth$=reporth$&" Chg  "&srvnam$(4)(1:2)&" Usage "
44740     let form$=form$&",n 9,n 9.2,n 9"
44750   end if 
44760 ! /r
44770 ! r: Service 5 - Oother
44780   if service_enabled(5) then ! always show "Other Charge"
44790     let colhdr$(chc+=1)=srvnam$(5)&" Charge"
44800     let cm$(chc)="10"
44810 ! 
44820     let reporth$=reporth$&" "&srvnam$(5)(1:2)&" Chg  "
44830     let form$=form$&",n 9.2"
44840   end if 
44850 ! /r
44860 ! r: Service 6
44870   if service_enabled(6) then ! always show "Other Charge"
44880     let colhdr$(chc+=1)=srvnam$(6)&" Charge"
44890     let cm$(chc)="10"
44892 ! 
44900     let reporth$=reporth$&" "&srvnam$(6)(1:2)&" Chg  "
44910     let form$=form$&",n 9.2"
44920   end if 
44930 ! /r
44940 ! r: Service 7
44950   if service_enabled(7) then ! always show "Other Charge"
44960     let colhdr$(chc+=1)=srvnam$(7)&" Charge"
44970     let cm$(chc)="10"
44980 ! 
44990     let reporth$=reporth$&" "&srvnam$(7)(1:2)&" Chg  "
45000     let form$=form$&",n 9.2"
45010   end if 
45020 ! /r
45030 ! r: Service 8
45040   if service_enabled(8) then ! always show "Other Charge"
45050     let colhdr$(chc+=1)=srvnam$(8)&" Charge"
45060     let cm$(chc)="10"
45070 ! 
45080     let reporth$=reporth$&" "& srvnam$(8)(1:2)&" Chg  "
45090     let form$=form$&",n 9.2"
45100   end if 
45110 ! /r
45120 ! r: final billing code
45130   let colhdr$(chc+=1)=" F/B"
45140   let cm$(chc)="30"
45150 ! 
45160   let reporth$=reporth$&" Final "
45170 ! 
45180   let form$=form$&",n 9"
45190 ! /r
45200   mat colhdr$(chc) : mat cm$(chc)
45210 ! 
45220   let fnflexinit1("Work",2,frame_bd_witdh+3,28,74,mat colhdr$,mat cm$,1)
45750    entryCount=0
45760   let ic=0
45770   restore #hWork: 
45780   let batchtot=0
45790   do
45800     read #hWork,using F_WORK: x$,mat x eof MENU1READWORKEOF
45810     let ic=0
45820     let item$(ic+=1)=x$
45830     let batchtot+=val(x$) conv L1100
45840     L1100: ! 
45850     if service_enabled(1) then 
45860       let item$(ic+=1)=str$(x(01))
45870       let item$(ic+=1)=str$(x(09))
45880       let item$(ic+=1)=str$(x(12)) ! water
45890     end if 
45900     if service_enabled(2) then 
45910       let item$(ic+=1)=str$(x(05)) ! sewer
45920     end if 
45930     if service_type(3)=3.2 then 
45940       let item$(ic+=1)=str$(x(13)) ! eletric
45950     else if service_type(3)=3 then 
45960       let item$(ic+=1)=str$(x(03))
45970       let item$(ic+=1)=str$(x(10))
45980       let item$(ic+=1)=str$(x(13))
45990       let item$(ic+=1)=str$(x(04)) ! eletric
46000     end if 
46010     if service_type(3)=3.1 then 
46020       let item$(ic+=1)=str$(x(03))
46030       let item$(ic+=1)=str$(x(10))
46040       let item$(ic+=1)=str$(x(13))
46050     end if 
46060     if service_enabled(4) then 
46070       let item$(ic+=1)=str$(x(02))
46080       let item$(ic+=1)=str$(x(11))
46090       let item$(ic+=1)=str$(x(14)) ! gas
46100     end if 
46110     if service_enabled(5) then ! service 5
46120       let item$(ic+=1)=str$(x(06))
46130     end if 
46140     if service_enabled(6) then ! service 6
46150       let item$(ic+=1)=str$(x(07))
46160     end if 
46170     if service_enabled(7) then ! service 7
46180       let item$(ic+=1)='' ! str$(x(07))  ! x(??)   07 is used in another place for service 7 but it is also used for service 6
46190     end if 
46200     if service_enabled(8) then ! service 8
46210       let item$(ic+=1)=str$(x(08))
46220     end if 
46230     let item$(ic+=1)=str$(x(15)) ! final billing code
46232     entryCount+=1
46240     let fnflexadd1(mat item$) ! pr mat item$ : pause
46250   loop
47580 MENU1READWORKEOF: ! /r
47590   fnlbl(1,frame_bd_witdh+4,'Entry Count: '&str$(entryCount))
47600   if lrec(hWork)>0 then 
47620 !   let fncmdkey("&Add",1)
47640     let fncmdkey("E&dit",2,1,0,'Edit highlighted record by clicking this button, pressing enter or double clicking the record.')
47660     let fncmdkey("&Print",4,0,0,'Print a proof listing of the entered records.')
47680     let fncmdkey("Save to &Holding File",6,0,0,'Save entered readings to a Holding File for later calculation.')
47700     let fncmdkey("&Delete",8)
47720     let fncmdkey("&Close",5,0,1)
47740     let fncmdkey("&Meter Change",9,0,0,"Calculates usage on meter change out.")
47760     let fncmdkey("&Finish and Calculate",10,0,0,'Calculate entered readings')
47780   else 
47800     let fncmdkey("&Close",5,0,1)
47801     ! fncmdset(1)
47820   end if 
47840   let fnacs(sn$,0,mat resp$,ck)
47860   if ck=cancel then 
47920     goto XIT
47940   end if 
47960   let d1=val(resp$(1))
47980   let d2=val(resp$(2))
48000   fnd1(d1,1)
48020   fncreg_write('Meter Reading Date Current',str$(d2))
48240   let x$=lpad$(trim$(resp$(3)(1:10)),10) ! formerly resp$(9)
48260   if lrec(hWork)>0 and ck=2 then 
48280     goto MAKE_CORRECTIONS
48300   end if 
49000   if ck=4 then 
49020     let fn_print_readings(hWork)
49040   else if ck=6 then ! add to holding file
49060     if fn_holdingFileSave(hWork) then goto XIT
49080   else if ck=8 then 
49100     delete #hWork,key=x$: 
49140   else if ck=9 then 
49160     if fn_meter_change_out=3 then goto ENTER_READING3
49180   else if ck=10 then 
49200     fnchain("S:\acsUB\ubCalk") ! goto CALCULATE
49220   else if ck=2001 then 
49240     let addmethod=1
49260     goto AUTO_REC
49280   else if ck=1 or ck=2002 then 
49300     let addmethod=2
49320     goto SEL_ACC
49340   else if ck=2003 then 
49360     let addmethod=3
49380     fn_loadBookOrHoldingFile(addmethod)
49400   else if ck=2004 then 
49420     let addmethod=4
49440     goto EST1
49460   else if ck=2005 then 
49480     let addmethod=from_hh_file
49500     fn_loadBookOrHoldingFile(addmethod)
49520   else if ck=2006 then 
49540     let addmethod=6
49560     goto INPUT_TEXT
49580   else if ck=2007 then 
49600     let fnretrieve_hand_held_file
49610     let fntop(program$)
49620   end if 
49640   goto MENU1
49680 ! /r MENU1

52000 def fn_holdingFileLoad
52020   let holdingFile$=env$('Q')&"\UBmstr\IpHold"&ip1$&".h"&env$('cno')
52040   open #hld9=9: "Name="&holdingFile$,internal,input ioerr L7460
52060   do 
52080     read #hld9,using F_WORK: x$,mat x eof IPHOLD_EO_HLD9
52100     let fn_writeWork(hWork,x$,mat x, 1)
52120     let fn_accumulateprooftotals
52140   loop 
52160   IPHOLD_EO_HLD9: ! 
52180   close #hld9: 
52200   L7460: ! 
52220   let addmethod=1 ! set addmethod back to 1 once holding file read in
52240 fnend 
56000 def fn_holdingFileSave(hWork) ! probably requires more than just hWork 
56020   holdingFileSaveReturn=0
56040   HoldingFileSave: !
56060   let fntos(sn$="holding")
56080   let mylen=19 : let mypos=mylen+2
56100   let fnlbl(1,1,"Holding File Number:",mylen)
56120   let fntxt(1,mypos,3,0,0,"30")
56140   let resp$(1)=""
56160   let fnfra(4,1,3,94,"Create new file or append to existing file","If you have a different file for each route, you will always take the option to create a new file.  If you only use one file, clean it on the first batch of readings and append the rest.")
56180   let fnopt(1,1,"Create new file (deletes all previous readings in holding file)",0,1)
56200   let resp$(respc_CreateNew:=2)="False"
56220   let fnopt(2,1,"Append to existing file (retain previous readings, merge new ones in, overwrites duplicates)",0,1)
56240   let resp$(3)="True"
56260   let fncmdkey("&Save",1,1)
56280   let fncmdkey("&Cancel",5,0,1)
56300   let fnacs(sn$,0,mat resp$,ck)
56320   if ck<>cancel then
56340     holdingFileSaveReturn=1
56360     let bk1=val(resp$(1)) conv HoldingFileSave
56380     if bk1<=0 then goto HoldingFileSave
56400     if uprc$(resp$(respc_CreateNew))=uprc$("True") and exists(env$('Q')&'\UBmstr\IpHold'&str$(bk1)&'.h'&env$('cno')) then ! Create New Holding File
56420       exec 'free "'&env$('Q')&'\UBmstr\IpHold'&str$(bk1)&'.h'&env$('cno')&'"' 
56440     end if
56460     ! Append to Existing Holding File
56480     dim holdingFile$*256
56500     dim holdingFileIndex$*256
56520     let holdingFile$=env$('Q')&"\UBmstr\IpHold"&str$(bk1)&".h"&env$('cno')
56540     let holdingFileIndex$=env$('temp')&"\ACS\IpHold"&str$(bk1)&"-Index.h"&env$('cno')
56560     fnindex_it(holdingFile$,holdingFileIndex$,'1 10')
56580     open #hld8:=fngethandle: "Name="&holdingFile$&",KFName="&holdingFileIndex$&',Shr,Use,RecL=74,KPs=1,KLn=10',internal,outin,keyed 
56600     restore #hWork: ! ,search>="": nokey AppendFinis
56620     do
56640       read #hWork,using F_WORK: x$,mat x eof AppendFinis
56660       fn_writeWork(hld8,x$,mat x, 1)
56680     loop
56700     AppendFinis: !
56720     close #hld8:
56740     fnstatus_close
56760     close #hWork:
56780     exec 'free "'&workFile$&'"'
56800     exec 'free "'&workFileIndex$&'"'
56820   end if
56840   fn_holdingFileSave=holdingFileSaveReturn
56860 fnend
57000 ! r:  obsolueted    found a better way   def fn_holdingFilePrint(ip1$) ! needs more columns
57020 !   obsolueted    found a better way       dim hpHoldingFile$*256
57040 !   obsolueted    found a better way       ! dim hpHoldingFileIndex$*256
57060 !   obsolueted    found a better way       let hpHoldingFile$=env$('Q')&"\UBmstr\IpHold"&ip1$&".h"&env$('cno')
57080 !   obsolueted    found a better way       ! let hpHoldingFileIndex$=env$('temp')&"\ACS\IpHold"&ip1$&"-Index.h"&env$('cno')
57100 !   obsolueted    found a better way       ! fnindex_it(hpHoldingFile$,hpHoldingFileIndex$,'1 10')
57120 !   obsolueted    found a better way       ! open #hpHoldingFile:=fngethandle: "Name="&hpHoldingFile$&",KFName="&hpHoldingFileIndex$&',Shr,Use,RecL=74,KPs=1,KLn=10',internal,outin,keyed 
57140 !   obsolueted    found a better way       ! restore #hpHoldingFile: ! ,search>="": nokey HpFinis
57160 !   obsolueted    found a better way       open #hpHoldingFile:=fngethandle: "Name="&hpHoldingFile$,internal,outin,relative
57180 !   obsolueted    found a better way       fnopenprn(0,0,0,0,'Holding File '&ip1$)
57200 !   obsolueted    found a better way       do
57220 !   obsolueted    found a better way         read #hpHoldingFile,using F_WORK: x$,mat x eof HpFinis
57240 !   obsolueted    found a better way         print #255: x$ pageoflow HpPgOf ! , mat x pageoflow HpPgOf
57260 !   obsolueted    found a better way       loop
57280 !   obsolueted    found a better way       goto HpFinis
57300 !   obsolueted    found a better way       HpHeader: ! r:
57320 !   obsolueted    found a better way         pr #255: '  Account  '
57340 !   obsolueted    found a better way         pr #255: '__________ '
57360 !   obsolueted    found a better way       return ! /r
57380 !   obsolueted    found a better way       HpPgOf: ! r:
57400 !   obsolueted    found a better way       pr #255: newpage
57420 !   obsolueted    found a better way       gosub HpHeader
57440 !   obsolueted    found a better way       continue ! /r
57460 !   obsolueted    found a better way       HpFinis: !
57480 !   obsolueted    found a better way       close #hpHoldingFile:
57500 !   obsolueted    found a better way       fncloseprn
57520 !   obsolueted    found a better way       ! fnstatus_close
57540 ! /r  obsolueted    found a better way   fnend
58000 def fn_loadBookOrHoldingFile(&addmethod)
58020   dim ihDirFileMask$*64
58040   ! 
58060   if addmethod=from_hh_file then 
58080     let book_or_holding_file$='Book'
58100     ihDirFileMask$='Readings.*'
58120   else if addmethod=from_holding_file then 
58140     let book_or_holding_file$='Holding File'
58160     ihDirFileMask$='IPHold*.h'&env$('cno')
58180   else 
58200     print bell;'addmethod not recognized by INPUT_HAND routine.' : goto IH_XIT
58220   end if 
58240   INPUT_HAND: !
58260   let fntos(sn$="ubipchg-inh")
58280   let txt$="Select "&book_or_holding_file$&" for Input:"
58300   let mylen=len(txt$)+1 : let mypos=mylen+2
58320   let fnlbl(1,1,txt$,mylen,1)
58340 ! r: book or holding file grid
58360   let colhdr$(1)=book_or_holding_file$ ! "Book"
58380   let colhdr$(2)="Size"
58400   let colhdr$(3)="Date"
58420   let colhdr$(4)="Time"
58440   mat bookItem$(4)
58460   mat colhdr$(4)
58480   ihFileCount=fngetdir2(env$('Q')&'\UBmstr\',mat ihFilename$, '',ihDirFileMask$,mat ihFileDate$,mat ihFileTime$,0,mat ihFileSize)
58500   let fnflexinit1("book_"&book_or_holding_file$(1:1),1,mypos,10,32,mat colhdr$,mat cm2$,1)
58520   ! open #ih_file_dir=9: "Name="&ih_file_dir$,display,input 
58540   for ihFileItem=1 to ihFileCount
58560     if book_or_holding_file$='Book' then 
58580       ! pause        
58600       tmpBookNumber=val(ihFilename$(ihFileItem)(10:len(ihFilename$(ihFileItem)))) conv ihInvalidFile
58620       let bookItem$(1)=str$(tmpBookNumber)
58640       let bookItem$(2)=cnvrt$("pic(zzz,zzz,zzz,zzz)",ihFileSize(ihFileItem))
58660       let bookItem$(3)=ihFileDate$(ihFileItem)
58680       let bookItem$(4)=ihFileTime$(ihFileItem)
58700       let fnflexadd1(mat bookItem$)
58720     else ! if book_or_holding_file$='Holding File' then 
58740       ihTmpHoldingFileNumber$=ihFilename$(ihFileItem)(7:pos(ihFilename$(ihFileItem),".",-1)-1) conv ihInvalidFile
58760       if ihTmpHoldingFileNumber$='-index' then goto ihInvalidFile
58780       bookItem$(1)=ihTmpHoldingFileNumber$
58800       bookItem$(2)=cnvrt$("pic(zzz,zzz,zzz,zzz)",ihFileSize(ihFileItem))
58820       bookItem$(3)=ihFileDate$(ihFileItem)
58840       bookItem$(4)=ihFileTime$(ihFileItem)
58860       fnflexadd1(mat bookItem$)
58880     end if 
58900     ihInvalidFile: !
58920   next ihFileItem
60000 IH_FILE_DIR_EOF: ! 
60020   ! close #ih_file_dir: ioerr IH_XIT ! /r
60040   let fnlbl(11,1," ",15,1)
60060   let fncmdkey("&Next",1,1)
60080   let fncmdkey("&Delete",4)
60100   let fncmdkey("&Print",6)
60120   let fncmdkey("&Cancel",5,0,1)
60140   let fnacs(sn$,0,mat resp$,ck)
60160   let holdingFile$=""
60180   let ip1$=resp$(1)
60300   ! let listonly=0
60320   ! if ck=6 then let listonly=1: let ck=1
60340   if ck=cancel or ip1$='' then 
60360     goto IH_XIT
60380   else if ck=6 then
60400     if book_or_holding_file$='Holding File' then
60404       open #hpHoldingFile:=fngethandle: "Name="&env$('Q')&"\UBmstr\IpHold"&ip1$&".h"&env$('cno'),internal,outin,relative
60406       fn_print_readings(hpHoldingFile, 'Holding File '&ip1$)
60408       close #hpHoldingFile:
60420       ! fn_holdingFilePrint(ip1$) ! print for Holding Files
60440     else
60460       fn_hh_readings(ip1$, 1) ! print for Books
60480     end if
60490     goto INPUT_HAND
60500   else if ck=4 then 
60520     mat txt$(1)
60540     let txt$(1)="Are you sure you wish to delete "&book_or_holding_file$&" "&ip1$&"?"
60560     let fnmsgbox(mat txt$,resp$,'',36)
60580     if resp$="Yes" then 
60600       if addmethod=from_hh_file then 
60620         execute 'Free "'&env$('Q')&"\UBmstr\Readings."&ip1$&'"'
60640       else if addmethod=from_holding_file then 
60660         execute 'Free "'&env$('Q')&"\UBmstr\IPHold"&ip1$&".h"&env$('cno')&'"'
60680       end if 
60700     end if 
60720     goto INPUT_HAND
61000   else if addmethod=from_holding_file then 
61020     let fn_holdingFileLoad
61040   else 
61060     let fn_hh_readings(ip1$)
61080   end if 
61100   IH_XIT: ! 
61120 fnend
70000 ENTER_READING: ! r:
70010   if alp$="*" then goto READ_ROUTE_SEQUENCE
70020   ENTER_READING2: ! 
70030   let fn_us1
70040   ENTER_READING3: ! 
70050   let fntos(sn$="enter_reading")
70060   let rc=0 : let frac=0
70070   ! 
70080   let fnfra(1,1,3,39,"Account Data")
70090   let mylen=15 : let mypos=mylen+2 : let fraad=frac+=1
70100   let fnlbl(1,1,"Account:",mylen,1,0,fraad)
70110   let fntxt(1,mypos,10,0,0,empty$,1,empty$,1)
70120   let resp$(rc+=1)=x$
70130   let fnlbl(2,1,"Name:",mylen,1,0,fraad)
70140   let fntxt(2,mypos,30,30,0,empty$,1,empty$,fraad)
70150   let resp$(rc+=1)=aname$
70160   let fnlbl(3,1,"Meter Address:",mylen,1,0,fraad)
70170   let fntxt(3,mypos,10,0,0,empty$,1,empty$,fraad)
70180   let resp$(rc+=1)=e2$
70190   ! 
70200   let fnfra(7,1,12,60,"Readings & Overrides")
70210   let mylen=0 : for j=1 to 8 : let mylen=max(mylen,len(srvnam$(j))) : next j
70220   let mypos1=mylen+2 : let mypos2=mypos1+12
70230   let mypos3=mypos2+12 : let mypos4=mypos3+12 : let mypos5=mypos4+12+4
70240   let lc=0 : let fraro=frac+=1
70250   let fnlbl(lc+=1,mypos1,"Reading",10,2,0,fraro)
70260   let fnlbl(lc,mypos2,"Charge",10,2,0,fraro)
70270   let fnlbl(lc,mypos3,"Usage",10,2,0,fraro)
70280   if srvnam$(3)="Electric" then 
70290     let fnlbl(lc,mypos4,"Demand",10,2,0,fraro)
70300   end if 
70310   ! r: Service 1
70312   tmpService=1
70320   let first_read_rc=rc
70330   if a(1)=0 and a(2)=0 then let disa=1 else let disa=0 ! water and sewer rate codes
70332   if onlyMonth(tmpService)>0 and onlyMonth(tmpService)<>date(days(d1,'mmddyy'),'mm') then let disa=1
70340   ! water
70350   if service_enabled(tmpService) then 
70360     let lc+=1
70370     let fnlbl(lc,1,srvnamc$(1),mylen,1,0,2)
70380     let fntxt(lc,mypos1,10,11,1,"20",disa,empty$,fraro) ! reading
70390     let fntxt(lc,mypos2,10,10,1,"10",disa,empty$,fraro) ! charge
70400     let fntxt(lc,mypos3,10,11,1,"20",disa,empty$,fraro) ! usage
70410     if editmode=1 then 
70420       let resp$(rc+=1)=str$(x(tmpService)) ! water reading
70430       let resp$(rc+=1)=str$(x(09)) ! water charge
70440       let resp$(rc+=1)=str$(x(12)) ! water used
70450     else 
70460       let resp$(rc+=1)=''
70470       let resp$(rc+=1)=''
70480       let resp$(rc+=1)=''
70490     end if 
70500   end if ! /r
70510   ! r: Service 2 - Sewer
70512   tmpService=2
70520   if a(tmpService)=0 then let disa=1 else let disa=0 ! sewer rate code
70522   if onlyMonth(tmpService)>0 and onlyMonth(tmpService)<>date(days(d1,'mmddyy'),'mm') then let disa=1
70540   if service_enabled(tmpService) then 
70550     let fnlbl(lc+=1,1,srvnamc$(tmpService),mylen,1,0,2)
70560     let fntxt(lc,mypos2,10,0,1,"10",disa,empty$,fraro) ! charge
70570     if editmode=1 then 
70580       let resp$(rc+=1)=str$(x(05)) ! sewer charge
70590     else 
70600       let resp$(rc+=1)=''
70610     end if 
70620   end if ! /r
70630   ! r: Service 3 - Electric
70632   tmpService=3
70640   if a(tmpService)=0 then let disa=1 else let disa=0 ! electric rate code
70642   if onlyMonth(tmpService)>0 and onlyMonth(tmpService)<>date(days(d1,'mmddyy'),'mm') then let disa=1
70650   ! 
70660   if service_type(tmpService)=3 then 
70670     let fnlbl(lc+=1,1,srvnamc$(tmpService),mylen,1,0,2)
70680     let fntxt(lc,mypos1,10,11,1,"20",disa,empty$,fraro) ! reading
70690     let fntxt(lc,mypos2,10,10,1,"10",disa,empty$,fraro) ! charge
70700     let fntxt(lc,mypos3,10,11,1,"20",disa,empty$,fraro) ! usage
70710     let fntxt(lc,mypos4,10,11,1,"20",disa,empty$,fraro) ! demand
70720     if editmode=1 then 
70730       let resp$(rc+=1)=str$(x(tmpService)) ! electric reading
70740       let resp$(rc+=1)=str$(x(10)) ! electric charge
70750       let resp$(rc+=1)=str$(x(13)) ! electric usage
70760       let resp$(rc+=1)=str$(x(04)) ! electric demand
70770     else 
70780       let resp$(rc+=1)=""
70790       let resp$(rc+=1)=""
70800       let resp$(rc+=1)=""
70810       let resp$(rc+=1)=""
70820     end if 
70830   else if service_type(tmpService)=3.1 then 
70840     let fnlbl(lc+=1,1,srvnamc$(tmpService),mylen,1,0,2)
70850     let fntxt(lc,mypos1,10,11,1,"20",disa,empty$,fraro) ! reading
70860     let fntxt(lc,mypos2,10,10,1,"10",disa,empty$,fraro) ! charge
70870     let fntxt(lc,mypos3,10,11,1,"20",disa,empty$,fraro) ! usage
70880     if editmode=1 then 
70890       let resp$(rc+=1)=str$(x(03)) ! electric reading
70900       let resp$(rc+=1)=str$(x(10)) ! electric charge
70910       let resp$(rc+=1)=str$(x(13)) ! electric usage
70920     else 
70930       let resp$(rc+=1)=""
70940       let resp$(rc+=1)=""
70950       let resp$(rc+=1)=""
70960     end if 
70970   else if service_type(tmpService)=3.2 then 
70980     if a(1)=0 and a(2)=0 then let disa=1 else let disa=0 ! water rate code
70982     if onlyMonth(tmpService)>0 and onlyMonth(tmpService)<>date(days(d1,'mmddyy'),'mm') then let disa=1
70990     let fnlbl(lc+=1,1,srvnamc$(tmpService),mylen,1,0,2)
71000     let fntxt(lc,mypos3,10,11,1,"20",disa,empty$,fraro) ! usage
71010     if editmode=1 then 
71020       let resp$(rc+=1)=str$(x(13)) ! Reduction Usage
71030     else 
71040       let resp$(rc+=1)=""
71050     end if 
71060   end if 
71070   ! /r
71080   ! r: Service 4 - Gas
71082   tmpService=4
71090   if service_enabled(tmpService) then
71100     if a(tmpService)=0 then let disa=1 else let disa=0 ! gas rate code
71102     if onlyMonth(tmpService)>0 and onlyMonth(tmpService)<>date(days(d1,'mmddyy'),'mm') then let disa=1
71110     let lc+=1
71120     let fnlbl(lc,1,srvnamc$(tmpService),mylen,1,0,2)
71130     let fntxt(lc,mypos1,10,11,1,"20",disa,empty$,fraro) ! reading
71140     let fntxt(lc,mypos2,10,10,1,"10",disa,empty$,fraro) ! charge
71150     let fntxt(lc,mypos3,10,11,1,"20",disa,empty$,fraro) ! usage
71160     if editmode=1 then 
71170       let resp$(rc+=1)=str$(x(02))
71180       let resp$(rc+=1)=str$(x(11))
71190       let resp$(rc+=1)=str$(x(14))
71200     else 
71210       let resp$(rc+=1)='' ! gas reading
71220       let resp$(rc+=1)='' ! gas charge
71230       let resp$(rc+=1)='' ! gas usage
71240     end if 
71250   end if 
71260   ! /r
71270   ! r: service 5
71272   tmpService=5
71280   if service_enabled(tmpService) then 
71290     if a(tmpService)=0 then let disa=1 else let disa=0 ! service 5 rate code
71292     if onlyMonth(tmpService)>0 and onlyMonth(tmpService)<>date(days(d1,'mmddyy'),'mm') then let disa=1
71300     if trim$(srvnam$(tmpService))="Reconnect Fee" then let disa=0
71310     let fnlbl(lc+=1,1,srvnamc$(tmpService),mylen,1,0,fraro)
71320     let fntxt(lc,mypos2,10,0,1,"10",disa,empty$,fraro)
71330     if editmode=1 then 
71340       let resp$(rc+=1)=str$(x(tmpService)) ! Service 5 charge
71350     else 
71360       let resp$(rc+=1)=''
71370     end if 
71380   end if 
71390   ! /r
71400   ! r: service 6
71402   tmpService=6
71410   if service_enabled(6) then 
71420     if service_type(6)=5 or service_type(6)=7 or service_type(6)=8 then ! service 6 rate code
71430       let disa=0
71440     else if extra(11)=0 then 
71450       let disa=1
71460     else 
71470       let disa=0
71480     end if 
71490     let fnlbl(lc+=1,1,srvnamc$(6),mylen,1,0,fraro)
71500     let fntxt(lc,mypos2,10,0,1,"10",disa,empty$,fraro) ! charge
71510     if editmode=1 then ! Service 6 charge
71520       let resp$(rc+=1)=str$(x(tmpService))
71530     else 
71540       let resp$(rc+=1)=''
71550     end if 
71560   end if 
71570   ! /r
71580   ! r: Service 7
71582   tmpService=7
71590   if service_enabled(tmpService) then 
71600     if service_type(tmpService)=5 then 
71610       let disa=0 ! don't disable other charge
71620     else if extra(12)=0 then 
71630       let disa=1
71640     else 
71650       let disa=0 ! service 7 rate code
71660     end if 
71662     if onlyMonth(tmpService)>0 and onlyMonth(tmpService)<>date(days(d1,'mmddyy'),'mm') then let disa=1
71670     let fnlbl(lc+=1,1,srvnamc$(tmpService),mylen,1,0,2)
71690     let fntxt(lc,mypos2,10,0,1,"10",disa,empty$,2) ! charge
71700     if editmode=1 then ! Service 7 charge
71710       let resp$(rc+=1)=str$(x(tmpService))
71720     else 
71730       let resp$(rc+=1)=''
71740     end if 
71750   end if 
71760   ! /r
71770   ! r: service 8
71772   tmpService=8
71780   if service_enabled(tmpService) then 
71790     if service_type(tmpService)=5 or service_type(tmpService)=6 then ! don't disable other charge nor gas connect
71800       let disa=0
71810     else if extra(13)=0 then 
71820       let disa=1
71830     else 
71840       let disa=0 ! service 8 rate code
71850     end if 
71852     if onlyMonth(tmpService)>0 and onlyMonth(tmpService)<>date(days(d1,'mmddyy'),'mm') then let disa=1
71860     let lc+=1
71870     let fnlbl(lc,1,srvnamc$(tmpService),mylen,1,0,2)
71880     let fntxt(lc,mypos2,10,0,1,"10",disa,empty$,fraro) ! charge
71890     if editmode=1 then 
71900       let resp$(rc+=1)=str$(x(tmpService)) ! Service 8 charge
71910     else 
71920       let resp$(rc+=1)=''
71930     end if 
71940   end if 
71950   ! /r
71960   ! 
71970   let lc=lc+2
71980   let fnlbl(lc,1,"Final Billing Code:",mylen+8,1,0,2)
71990   let fncomboa("finalbill",lc,24,mat opt_final_billing$,"Used to record final billing code in customer record",28,fraro) ! final billing code
72000   let resp_fianl_billing_code=(rc+=1)
72010   if editmode=1 then 
72020     let resp$(resp_fianl_billing_code)=str$(x(15)) ! Final Billing Code
72030   else 
74000     let resp$(resp_fianl_billing_code)=opt_final_billing$(final+1)
74020   end if 
74040   if editmode=1 and x(15)=1 then let resp$(resp_fianl_billing_code)=opt_final_billing$(2)
74060   if editmode=1 and x(15)=2 then let resp$(resp_fianl_billing_code)=opt_final_billing$(3)
74080   let begdate=fndate_mmddyy_to_ccyymmdd(d1)-20000
74100   let fn_flexRead(1,mypos5+2,hTrans,x$,begdate,0,fraro) ! beginning date=billing date less one year
74120   let fncmdkey("&Meter Change",9,0,0,"Calculates usage on meter change out.")
74140   let fncmdkey("&Review Customer Record",8,0,0,"Allow you to review any customer while entering readings.")
74180   if addmethod=1 or addmethod=from_hh_file then let fncmdset(17) else let fncmdset(11) ! kj   3/24/06
74200   let fnacs(sn$,0,mat resp$,ck)
74220   if ck=8 then 
74240     let fncustomer(x): read #hCustomer1,using F_CUSTOMER_C,key=x$,release: x$,aname$,mat a,final,mat d,alp$,mat extra,extra$(3)
74242     let fnapply_default_rates(mat extra, mat a)
74260     goto ENTER_READING3
74280   end if 
74300   let rc=first_read_rc
74320   ! If PASSCHECK=CKFAIL Then Let EDITMODE=0 ! xxx Ken
74340   if ck=3 then let done_with_readings=1 ! code as done with entering readings is select finish
74360   if service_enabled(1) then ! Service 1 - Water
74380     let x(01)=val(resp$(rc+=1))
74400     let x(09)=val(resp$(rc+=1))
74420     let x(12)=val(resp$(rc+=1))
74440   end if 
74460   if service_enabled(2) then ! Service 2 - Sewer
74480     let x(05)=val(resp$(rc+=1))
74500   end if 
74520   if service_type(3)=3 then ! electric
74540     let x(03)=val(resp$(rc+=1))
74560     let x(10)=val(resp$(rc+=1))
74580     let x(13)=val(resp$(rc+=1))
74600     let x(04)=val(resp$(rc+=1)) ! electric/lawn meter
74620   else if service_type(3)=3.1 then ! lawn meter
74640     let x(03)=val(resp$(rc+=1))
74660     let x(10)=val(resp$(rc+=1))
74680     let x(13)=val(resp$(rc+=1))
74700   else if service_type(3)=3.2 then 
74720     let x(13)=val(resp$(rc+=1))
74740   end if  ! if srvnam$(3)=...
74760   if service_type(3)=3.1 and x(03)=0 and d(5)>0 and a(3)>0 then 
74780     let x(03)=d(5) ! if they skip reading the lawn meters, just write the previous reading into the current reading
74800   end if 
74820   if service_enabled(4) then 
74840     let x(02)=val(resp$(rc+=1))
74860     let x(11)=val(resp$(rc+=1))
74880     let x(14)=val(resp$(rc+=1)) ! gas
74900   end if 
74920   ! 
74940   if service_enabled(5)=1 then ! service 5
74960     let x(06)=val(resp$(rc+=1))
74980   end if 
75000   ! 
75020   if service_enabled(6)=1 then ! service 6
75040     let x(07)=val(resp$(rc+=1))
75060   end if 
75080   ! 
75100   if service_enabled(7) then ! service 7
75120     let x(07)=val(resp$(rc+=1))
75140   end if 
75160   ! 
75180   if service_enabled(8) then ! service 8
75200     let x(08)=val(resp$(rc+=1))
75220   end if 
75240   ! pause
75260   let rc+=1
75280   let x(15)=val(resp$(resp_fianl_billing_code)(1:1)) ! final billing code
75300   if ck=2 and addmethod=from_hh_file then 
75320     let skiprec=1
75340     goto L2910 ! if choose skip on pulling from hh file, then skip writing the record   ! kj 3/24/06
75360   else if addmethod=from_holding_file then 
75380     goto CHECK_UNUSUAL
75400   else if ck=2 and editmode=0 then 
75420     goto SEL_ACC
75440   else if ck=2 and editmode=1 then 
75460     goto MENU1
75480   else if ck=3 or ck=cancel then 
75500     let addmethod=0
75520     goto MENU1
75540   else if ck=9 then 
75560     if fn_meter_change_out=3 then goto ENTER_READING3
75580     goto MENU1
75600   end if 
75620   CHECK_UNUSUAL: ! 
75640   if addmethod<>3 then mat mroll=(0)
75660   let passcheck=ckpass=0 : let ckfail=1 : let ckcancel=2
75670   ! 
75680   let fn_checkwater
75700   if passcheck=ckfail then 
75720     let editmode=1
75740     goto ENTER_READING3
75760   else if passcheck=ckcancel then 
75780     goto ERXIT
75800   end if 
75810   ! 
75820   let fn_checkgas
75840   if passcheck=ckfail then 
75860     let editmode=1
75880     goto ENTER_READING3
75900   else if passcheck=ckcancel then 
75920     let editmode=1
75940     goto ENTER_READING3 ! Then Goto ERXIT
75960   end if 
75970   ! 
75980   let fn_checkelec
76000   if passcheck=ckfail then 
76020     let editmode=1
76040     goto ENTER_READING3 
76060   else if passcheck=ckcancel then 
76080     goto ERXIT
76100   end if 
76120   L2910: ! 
76140   if addmethod=from_hh_file or addmethod=6 then return  ! Hand Held or input_text
76160   ! 
76180   if editmode=0 and unusual<>2 then let fn_writeWork(hWork,x$,mat x)
76200   if editmode=1 and unusual<>2 then gosub REWRITE_WORK
76220   ERXIT: ! 
76240   if addmethod=0 then goto MENU1
76260   if addmethod=1 and done_with_readings=0 then let editmode=0 ! set editmode back after any corrections during the addmethod 1
76280   if addmethod=1 and editmode=0 then goto READ_ROUTE_SEQUENCE
76300   if addmethod=1 and editmode=1 then goto MENU1
76320   ! If ADDMETHOD=1 AND EDITMODE=1 Then Goto READ_ROUTE_SEQUENCE ! MENU1
76340   if addmethod=2 and (editmode=0 or editmode=1) then mat x=(0): goto SEL_ACC ! kj 92407
76360   ! If ADDMETHOD=2 AND EDITMODE=1 Then Goto MENU1 ! kj 92407
76380 goto MENU1 ! /r
80000 def fn_setupFlexRead
80020   if ~setupFlexRead then
80040     setupFlexRead=1
80080     dim colmask$(30),frColHdr$(30)*20,servicename$(10)*20,item$(25)*70
80100     dim tg(11),a(7)
80120     fnget_services(mat servicename$)
80140     let tcode$(1)="Charge"
80160     let tcode$(2)="Penalty"
80180     let tcode$(3)="Collect"
80200     let tcode$(4)="C/M"
80220     let tcode$(5)="D/M"
80240   end if
80260 fnend
82000 def fn_flexRead(myline,mypos,filnum,z$,begdate,enddate,selcode) ! library ready
82020   if ~setupFlexRead then let fn_setupFlexRead
82040   let z$=trim$(z$)
82060   if z$<>'' then 
82080     open #tmp=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed 
82100     let z$=lpad$(trim$(z$),10)
82120     read #tmp,using "Form Pos 143,7*pd 2",key=z$: mat a
82140     close #tmp: 
82160   end if 
82180   mat frColHdr$(30) : mat colmask$(30)
82200   let frColHdr$(headers=1)="Date" : let colmask$(headers)="3"
82220   if trim$(servicename$(1))<>"" and (z$<>'' and a(1)>0) then 
82240     let frColHdr$(headers+=1)="Water Reading" : let colmask$(headers)="20"
82260     let frColHdr$(headers+=1)="Water Usage" : let colmask$(headers)="20"
82280   end if 
82300   if trim$(servicename$(3))="Electric" and (z$<>'' and a(3)>0) then 
82320     let frColHdr$(headers+=1)="Electric Reading" : let colmask$(headers)="20"
82340     let frColHdr$(headers+=1)="Electric Usage" : let colmask$(headers)="20"
82360   end if 
82380   if trim$(servicename$(3))="Lawn Meter" and (z$<>'' and a(3)>0) then 
82400     let frColHdr$(headers+=1)="Lawn Meter Reading" : let colmask$(headers)="20"
82420     let frColHdr$(headers+=1)="Lawn Meter Usage" : let colmask$(headers)="20"
82440   end if 
82460   if trim$(servicename$(4))="Gas" and (z$<>'' and a(4)>0) then 
82480     let frColHdr$(headers+=1)="Gas Reading" : let colmask$(headers)="20"
82500     let frColHdr$(headers+=1)="Gas Usage" : let colmask$(headers)="20"
82520   end if 
82540   mat frColHdr$(headers)
82560   mat colmask$(headers)
82580   let fnflexinit1("ubread",myline,mypos,13,30,mat frColHdr$,mat colmask$,1)
82600   let items=0
82620   mat item$=('')
82640   restore #filnum,key>=rpad$(z$,kln(filnum)): nokey NO_RECORDS_FOUND
82660   do
82680     FlexReadCustomerRead: ! 
82700     read #filnum,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1',release: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof FlexReadXit
82720     if p$<>z$ then goto FlexReadXit ! not same account
82740     if (selcode>1 and tcode<>selcode-1) or (begdate>0 and tdate<begdate) or (enddate>0 and tdate>enddate) then goto FlexReadCustomerRead
82760     if tcode=0 then let tcode=1 ! temporary to prevent bad transaction codes
82780     let item$(1)=str$(tdate)
82800     let items=1
82820     if trim$(servicename$(1))<>"" and (z$<>'' and a(1)>0) then 
82840       let item$(items+=1)=str$(wr)
82860       let item$(items+=1)=str$(wu)
82880     end if 
82900     if trim$(servicename$(3))="Electric" and (z$<>'' and a(3)>0) then 
82920       let item$(items+=1)=str$(er)
82940       let item$(items+=1)=str$(eu)
82960     end if 
82980     if trim$(servicename$(3))="Lawn Meter" and (z$<>'' and a(3)>0) then 
83000       let item$(items+=1)=str$(er)
83020       let item$(items+=1)=str$(eu)
83040     end if 
83060     if trim$(servicename$(4))<>"" and (z$<>'' and a(4)>0) then 
83080       let item$(items+=1)=str$(gr)
83100       let item$(items+=1)=str$(gu)
83120     end if 
83140     let fnflexadd1(mat item$)
83160   loop
83180   ! ______________________________________________________________________
83200   NO_RECORDS_FOUND: ! 
83220     if items=0 then mat item$=("")
83240     let fnflexadd1(mat item$)
83260   FlexReadXit: ! 
83280 fnend 
86000 def fn_writeWork(hWork,x$,mat x; overwriteDupeAccount) ! write to hWork file
86020   !
86040   if overwriteDupeAccount then
86060     rewrite #hWork,using F_WORK,key=lpad$(trim$(x$),kln(hWork)): trim$(x$),mat x nokey ww_overwriteAdd
86080     goto ww_overwriteFinis
86100     ww_overwriteAdd: !
86120     write #hWork,using F_WORK: trim$(x$),mat x
86140     ww_overwriteFinis: !
86160   else
86180     ww_writeWorkIncriment: ! write to hWork file
86200     let rctr=lrec(hWork)+1
86220     write #hWork,using F_WORK,rec=rctr: trim$(x$),mat x duprec ww_writeWorkIncriment
86240     F_WORK: form pos 1,cr 10,4*pd 5,7*pd 4.2,3*pd 5,n 1
86260   end if
86280 fnend 
88000 def fn_meter_change_out
88020   mco_return=0
88040   do 
88060     let fntos(sn$="Method")
88080     let rc=0 : let lc=0
88100     let fnfra(1,1,2,49,"Method of Change Out")
88120     let fnopt(1,1,"Current customer only",0,1)
88140     let resp$(1)="True"
88160     let fnopt(2,1,"All Customers",0,1)
88180     let resp$(2)="False"
88200     let fnlbl(5,1,"Service Type:",18,1)
88220     let fncomboa("ServiceType",5,20,mat serviceoption$)
88240     let resp$(3)=serviceoption$(1)
88260     let fnlbl(6,1,"Beginning Customer:",18,1)
88280     let fncmbact(6,20,1)
88300     let resp$(4)="[All]"
88320     let fncmdkey("&Next",1,1,0): let fncmdkey("&Cancel",5,0,1)
88340     let fnacs(sn$,0,mat resp$,ckey)
88360     if ckey=5 then goto Mco_Xit
88380     let servicetype$=resp$(3)(1:2)
88400     let begx$=resp$(4)(1:10)
88420     if resp$(2)="True" then let method$="File" : goto MCO_UPDATE_FULL_FILE ! working from a file
88440     if resp$(1)="True" then let method$="Customer" : goto MCO_RECORD_READINGS
88460   loop 
88480   MCO_RECORD_READINGS: ! 
88500   let fntos(sn$="Meter_Change")
88520   let rc=0 : let lc=0: let resprc=0
88540   let fnlbl(lc+=1,1,x$&"  "&aname$,50,0)
88560   let fnlbl(lc+=2,32,"Old Meter",10,2)
88580   let fnlbl(lc,55,"New Meter",10,2)
88600   let fnlbl(lc+=1,25,"Prior",10,2)
88620   let fnlbl(lc,37,"Current",10,2)
88640   let fnlbl(lc,49,"Prior",10,2)
88660   let fnlbl(lc,61,"Current",10,2)
88680   if trim$(servicetype$)="WA" then 
88700     let fnlbl(lc+=1,1,srvnamc$(1),20,1)
88720     let fntxt(lc,25,10,11,1,"30",0,"Enter the prior reading on the old meter")
88740     let resp$(resprc+=1)=str$(d(1))
88760     let fntxt(lc,37,10,10,1,"30",0,"Enter the current reading on the old meter.")
88780     let resp$(resprc+=1)=""
88800     let fntxt(lc,49,10,11,1,"30",0,"Enter the beginning reading the new meter")
88820     let resp$(resprc+=1)=""
88840     let fntxt(lc,61,10,11,1,"30",0,"Enter the ending reading on new meter")
88860     let resp$(resprc+=1)=str$(x(1))
88880   else if trim$(servicetype$)="EL" then 
88900     let fnlbl(lc+=1,1,srvnamc$(3),20,1)
88920     let fntxt(lc,25,10,11,1,"30",0,"Enter the prior reading on the old meter")
88940     let resp$(resprc+=1)=str$(d(5))
88960     let fntxt(lc,37,10,10,1,"30",0,"Enter the current reading on the old meter.")
88980     let resp$(resprc+=1)=""
89000     let fntxt(lc,49,10,11,1,"30",0,"Enter the beginning reading the new meter")
89020     let resp$(resprc+=1)=""
89040     let fntxt(lc,61,10,11,1,"30",0,"Enter the ending reading on new meter")
89060     let resp$(resprc+=1)=str$(x(3))
89080   else if trim$(servicetype$)="GA" then 
89100     let fnlbl(lc+=1,1,srvnamc$(4),20,1)
89120     let fntxt(lc,25,10,11,1,"30",0,"Enter the prior reading on the old meter")
89140     let resp$(resprc+=1)=str$(d(9))
89160     let fntxt(lc,37,10,10,1,"30",0,"Enter the current reading on the old meter.")
89180     let resp$(resprc+=1)=""
89200     let fntxt(lc,49,10,11,1,"30",0,"Enter the beginning reading the new meter")
89220     let resp$(resprc+=1)=""
89240     let fntxt(lc,61,10,11,1,"30",0,"Enter the ending reading on new meter")
89260     let resp$(resprc+=1)=str$(x(2))
89280   end if 
89300   let fncmdkey("&Next",1,1,0): let fncmdkey("&Cancel",5,0,1)
89320   let fncmdkey("&Finish",10)
89340   let fnacs(sn$,0,mat resp$,ckey)
89360   if ckey=5 then goto Mco_Xit
89380   if ckey=10 then goto Mco_Xit
89400   let oldmeterprior=val(resp$(1))
89420   let oldmetercurrent=val(resp$(2))
89440   let newmeterprior=val(resp$(3))
89460   let newmetercurrent=val(resp$(4))
89480   let usage=oldmetercurrent-oldmeterprior+newmetercurrent-newmeterprior
89500   if usage<0 then 
89520     mat txt$(3)
89540     let txt$(1)="The readings you entered create a negative uuage."
89560     let txt$(2)="Correct one of the readings or choose Cancel to"
89580     let txt$(3)="skip this record!"
89600     let fnmsgbox(mat txt$,resp$,'',1)
89620     if resp$="OK" then goto MCO_RECORD_READINGS
89640   end if 
89660   if method$="File" then let fn_rewrite_usage : goto MCO_WORK_READ ! read new record from readings file
89680   if method$="Customer" and servicetype$="WA" then let x(1)=newmetercurrent: let x(12)=usage
89700   if method$="Customer" and servicetype$="GA" then let x(2)=newmetercurrent: let x(14)=usage
89720   if method$="Customer" and servicetype$="EL" then let x(3)=newmetercurrent: let x(13)=usage
89740   if method$="Customer" then let passcheck=ckfail: let editmode=1 : goto mco_ENTER_READING3
89760   ! goto somewhere
89780   MCO_UPDATE_FULL_FILE: ! meter change over - update full file
89800   close #hWork: ioerr ignore
89820   open #hWork: "Name="&workFile$,internal,outin,relative 
89840   if lrec(hWork)=0 then goto MCO_L9290
89860   MCO_WORK_READ: ! 
89880   read #hWork,using F_WORK: x$,mat x eof MCO_L9350
89900   MCO_L9290: ! 
89920   if trim$(begx$)="" or trim$(begx$)="[All]" then let begx$="" : goto MCO_L9320
89940   if trim$(begx$)<>trim$(x$) then goto MCO_WORK_READ
89960   let begx$=""
89980   MCO_L9320: ! 
90000   read #hCustomer1,using MCO_F_CUSTOMER,key=x$,release: aname$, mat d nokey MCO_WORK_READ
90020   MCO_F_CUSTOMER: form pos 41,c 20,pos 217,15*pd 5
90040   goto MCO_RECORD_READINGS
90060   MCO_L9350: ! 
90080   close #hWork: ioerr ignore
90100   open #hWork: "Name="&workFile$&",KFName="&workFileIndex$,internal,outin,keyed 
90120   goto Mco_Xit
90140   mco_ENTER_READING3: !
90160   mco_return=3
90180   Mco_Xit: !
90200   fn_meter_change_out=mco_return
90220 fnend 