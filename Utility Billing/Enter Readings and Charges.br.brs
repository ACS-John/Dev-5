00010 ! formerly S:\acsUB\UBIpChg
00020 fn_setup
00030 fntop(program$)
00040 goto MENU1
02000 def fn_setup
02020   if ~setup then
02040     setup=1
02060     library 'S:\Core\Library': fnerror,fnopenprn,fncloseprn
02080     library 'S:\Core\Library': fnmsgbox,fnchain,fnLastBillingDate,fnxit,fntop
02100     library 'S:\Core\Library': fndate_mmddyy_to_ccyymmdd,fncomboa
02120     library 'S:\Core\Library': fncustomer,fnhand_held_device$
02140     library 'S:\Core\Library': fngethandle,fnButton,fnregistered_for_hh
02160     library 'S:\Core\Library': fnRetrieveHandHeldFile,fnask_account
02180     library 'S:\Core\Library': fnLbl,fnTxt,fnAcs,fnTos
02200     library 'S:\Core\Library': fnOpt,fnChk,fnflexinit1,fnflexadd1
02220     library 'S:\Core\Library': fncmbact,fncmbrt2,fnFra,fnCmdSet,fnCmdKey
02240     library 'S:\Core\Library': fncreg_read,fncreg_write,fngetdir2,fnfree
02260     library 'S:\Core\Library': fnapply_default_rates,fnget_services
02280     library 'S:\Core\Library': fnStatusClose,fnindex_it
02300     library 'S:\Core\Library': fnAccountFromLocationId$
02320     library 'S:\Core\Library': fnAddOneC
02322     library 'S:\Core\Library': fnOpenFile,fnCloseFile,fnbuildkey$
02324     library 'S:\Core\Library': fnCustomerData$
03000     on error goto ERTN
03020   ! dims, constants, top, etc
03040     dim resp$(40)*256
03060     dim aname$*30,d(15),alp$*1
03080     dim workFile$*256,workFileIndex$*256
03100     dim t(16),message$(1)*128,a$*256
03120     dim txt$*80,txt$(6)*70
03140     dim extra(23),item$(30)*20,extra$(11)*30
03160     dim x$*10,px$*10,x(15),ln$*256,last_ln$*256
03180     dim ft$*21,rm$*60,ra(2),colhdr$(30)*40,cm$(30)
03200     dim est1(3,3),e1$*30,e2$*30,a(7),tg(11)
03220     dim cd1(8)
03240     dim penalty$(10)*1,reporth$*300,form$*300
03260     dim mroll(3) ! meter roll code from hand held file
03280     dim serviceoption$(10)*25,srvnamc$(10)*21,srvnam$(10)*20,srv$(10)*2
04000     dim opt_final_billing$(5)*33
04020     opt_final_billing$(1)="0 = Not Finaled"
04040     opt_final_billing$(2)="1 = Final Bill"
04060     opt_final_billing$(3)="2 = Final & Refund Deposit"
04080     opt_final_billing$(4)="3 = Active, but do not Bill"
04100     opt_final_billing$(5)="4 = Finaled, but not billed"
04120   !
04140     fnLastBillingDate(d1)
04160     if days(d1,'mmddyy')<days(date$('mmddyy'),'mmddyy')-23 then d1=0
04180     open #1: "Name=[Q]\UBmstr\Company.h[cno]",internal,input
04200     read #1,using "form pos 130,n 4": pcent ioerr ignore ! percent for unusual usage
04220     close #1:
04240     if pcent=0 then
04260       pcent=100
04280       open #1: "Name=[Q]\UBmstr\Company.h[cno]",internal,outIn
04300       rewrite #1,using "form pos 130,n 4": pcent ioerr ignore ! percent for unusual usage
04320       close #1:
04340     end if
04360     fncreg_read('unusual usage minimum water',uum_water$) : uum_water=val(uum_water$)
04380     fncreg_read('unusual usage minimum gas',uum_gas$) : uum_gas=val(uum_gas$)
04400     fncreg_read('unusual usage minimum electric',uum_electric$) : uum_electric=val(uum_electric$)
04420   !
04440     pcent=pcent*.01 ! convert unusual usage % to decimal
04460     cancel=5
04480     workFile$="[Q]\UBmstr\Reads_and_Chgs.h[cno]"
04500     workFileIndex$="[Q]\UBmstr\Reads_and_Chgs-Key.h[cno]"
04520   ! synchronize these settings with S:\acsUB\ubCalk
04540     from_holding_file=3 ! enumerations for addmethod
04560     from_hh_file=5
04580   ! ______________________________________________________________________
05000   ! Open_Stuff: !
05020     fn_setup_service(mat service_enabled)
05040     open #hTrans:=fngethandle: "Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\ubTrIndx.h[cno],Shr,Use,RecL=102,KPs=1,KLn=19",internal,outIn,keyed
05060     open #hCustomer1:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed  ! was file #1, but it was getting closed incorrectly
05080   F_CUSTOMER_C: form pos 1,c 10,pos 41,c 30,pos 143,7*pd 2,pos 1821,n 1,pos 217,15*pd 5,pos 354,c 1,pos 1741,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,pos 1954,c 12,pos 1906,c 12
05120     open #hWork:=fngethandle: "Name="&workFile$&",KFName="&workFileIndex$&",Shr,Use,RecL=74,KPs=1,KLn=10",internal,outIn,keyed
05140     open #hCustomer2:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx2.h[cno],Shr",internal,outIn,keyed
05160     open #hCustomer3:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx3.h[cno],Shr",internal,outIn,keyed
05180     open #hCustomer4:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx4.h[cno],Shr",internal,outIn,keyed
05200     open #hCustomer5:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",internal,outIn,keyed
05220     fncreg_read('Meter Reading Date Current',tmp$,date$("MMDDYY")) : d2=val(tmp$)
05240   end if
05260 fnend
11000 def fn_setup_service(mat service_enabled)
11010   ! r: older stuff
11012   fnget_services(mat srvnam$,mat srv$,mat tax_code$,mat penalty$)
11050   for j=1 to udim(mat srvnam$)
11060     srvnam$(j)=trim$(srvnam$(j))
11070     srvnamc$(j)=srvnam$(j)&":"
11080   next j
11090   if srvnamc$(6)="Bad Check Charge:" then srvnamc$(6)="Check Charge:"
11100   ! /r
11110   ! return explaind
11120   ! +10    penalty$(   ) <>  "Y"
11130   ! +100   srvnam$(    ) <>  ""
11140   mat service_enabled=(0)
11150   ! r: set mat service_is_not_blank
11160   for x=1 to udim(mat srvnam$)
11170     if srvnam$(x)<>"" then service_is_not_blank(x)+=100
11180   next x
11190   ! /r
11200   ! r: set mat service_is_not_a_penalty
11210   for x=1 to udim(mat srvnam$)
11220     if penalty$(x) <>"Y" then service_is_not_a_penalty(x)+=100
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
11360       service_type(x)=1
11370     else if srvnam$(x)='Sewer' then
11380       service_type(x)=2
11390     else if srvnam$(x)="Electric" or srv$(x)="EL" then
11400       service_type(x)=3
11410     else if srvnam$(x)='Lawn Meter' then
11420       service_type(x)=3.1
11430     else if srvnam$(x)(1:5)='Reduc' then
11440       service_type(x)=3.2
11450     else if srvnam$(x)="Gas" or srv$(x)="GA" then
11460       service_type(x)=4
11470     else if uprc$(srvnam$(x)(1:5))="OTHER" then
11480       service_type(x)=5
11482     else if uprc$(srvnam$(x))="GAS CONNECT" then
11484       service_type(x)=6
11485     else if uprc$(srvnam$(x))="WATER CONNECT" then
11486       service_type(x)=7
11487     else if uprc$(srvnam$(x))="BAD CHECK CHARGE" then
11488       service_type(x)=8
11490     end if
11500   next x
11510   ! /r
11520   ! r: set the mat service_enabled
11530   for x=1 to udim(mat srvnam$)
11540     if srvnam$(x)='Water' then
11550       service_enabled(x)+=1
11560     end if
11570   next x
11580   if service_is_not_blank(1) then
11590     service_enabled(1)=1
11600   end if
11610   !
11620   if service_is_not_blank(2) then
11630     service_enabled(2)=1
11640   end if
11650   !
11660   if service_type(3)=3 then
11670     service_enabled(3)=1
11680   else if service_type(3)=3.1 then ! Lawn Meter" then
11690     service_enabled(3)=2
11700   else if service_type(3)=3.2 then ! Reduc" then
11710     service_enabled(3)=3
11720   end if
11730   !
11740   if service_type(4)=4 then
11750     service_enabled(4)=1
11760   end if
11770   !
11780   if service_type(5)=5 or (service_is_not_blank(5) and service_is_not_a_penalty(5)) then
11790     service_enabled(5)=1
11800   end if
11810   !
11820   if service_type(6)=5 or (service_is_not_blank(6) and service_is_not_a_penalty(6)) then
11830     service_enabled(6)=1
11840   end if
11850   !
11860   if service_type(7)=5 or (service_is_not_blank(7) and service_is_not_a_penalty(7)) then ! Service 7 seems to incompletly implemented
11870     service_enabled(7)=1
11880   end if
11890   !
11900   if service_type(8)=5 or (service_is_not_blank(8) and service_is_not_a_penalty(8)) then
11910     service_enabled(8)=1
11920   end if
11930   !
11940   !   if service_is_not_blank(9) then
11950   !     service_enabled(9)=1
11960   !   end if
11970   !   !
11980   !   if service_is_not_blank(10) then
11990   !     service_enabled(10)=1
12000   !   end if
12010   ! /r
12020   ! r: set mat serviceoption$, serviceoption_count and service  (for fn_meter_change_out)
12030   serviceoption_count=0
12040   if trim$(srvnam$(1))="Water" then
12050     serviceoption$(serviceoption_count+=1)=srv$(1)&"-"&trim$(srvnam$(1)(1:20))
12060     service=service+1
12070   end if
12080   if trim$(srvnam$(3))="Electric" or trim$(srv$(3))="EL" then
12090     serviceoption$(serviceoption_count+=1)=srv$(3)&"-"&srvnam$(3)(1:20)
12100     service=service+1
12110   end if
12120   if (trim$(srvnam$(4))="Gas" or trim$(srv$(4))="GA") then
12130     serviceoption$(serviceoption_count+=1)=srv$(4)&"-"&srvnam$(4)(1:20)
12140     service=service+1
12150   end if
12160   mat serviceoption$(serviceoption_count)
12170   ! /r
12180 fnend
12600 AUTO_REC: ! r:
12610   done_with_readings=0
12620   fnTos(sn$="ubipchg-3")
12630   fnLbl(1,1,"Starting Account:" ,24,1)
12640   fncmbact(1,26,1)
12650   resp$(1)="[All]"
12660   fnCmdSet(2)
12670   fnAcs(sn$,0,mat resp$,ck)
12680   if ck=cancel then
12690     done_with_readings=1
12700     goto MENU1
12710   end if
12720   if uprc$(resp$(1))=uprc$("[All]") then resp$(1)=""
12730   x$=lpad$(trim$(resp$(1)(1:10)),10)
12740   px$=x$
12750   if trim$(x$)="" then goto SEL_ACC
12760   read #hCustomer1,using F_CUSTOMER_C,key=x$,release: x$,aname$,mat a,final,mat d,alp$,mat extra,extra$(3) nokey AUTO_REC
12770   if addmethod=1 and env$('client')<>"Choctaw" then
12780     seq$=cnvrt$("pic(zz)",extra(1))&cnvrt$("pic(zzzzzzz)",extra(2))
12790     read #hCustomer5,using F_CUSTOMER_C,key=seq$,release: x$,aname$,mat a,final,mat d,alp$,mat extra,extra$(7),extra$(3) nokey AUTO_REC
12800   end if
12802   fnapply_default_rates(mat extra, mat a)
12810   if final=1 or final=2 or final=3 or (trim$(px$)<>"" and x$<>px$) then goto READ_ROUTE_SEQUENCE ! ken  ! john -- this is so users can select an account without being pushed to the incorrect account with the same rt/seq
12820   px$=""
12830   goto ENTER_READING
12840 ! /r
13000 SEL_ACC: ! r:
13002 ! passcheckwater=passcheckgas=passcheckelec=0
13004   if addmethod=1 then goto READ_ROUTE_SEQUENCE
13006   x$=""
13008 SEL_ACT_TOS: !
13012   ck=fnask_account('ubipchg',x$,hCustomer1)
13014   if ck=5 or ck=cancel then
13016     addmethod=0
13018     goto MENU1
13020   end if
13145   if addmethod=1 then
13146     goto READ_ROUTE_SEQUENCE
13148   else
13150     read #hCustomer1,using 'form pos 41,c 30,pos 143,7*pd 2,pos 1821,n 1,pos 217,15*pd 5',key=x$,release: aname$,mat a,final,mat d nokey SEL_ACT_TOS
13152     fnapply_default_rates(mat extra, mat a)
13155     goto ENTER_READING2
13158   end if
13160 ! /r
13200 READ_ROUTE_SEQUENCE: ! r:
13205   if env$('client')="Choctaw" then ! read in Account order
13210     read #hCustomer1,using F_CUSTOMER_C: x$,aname$,mat a,final,mat d,alp$,mat extra,extra$(7),extra$(3) eof MENU1
13215   else
13220     read #hCustomer5,using F_CUSTOMER_C,release: x$,aname$,mat a,final,mat d,alp$,mat extra,extra$(7),extra$(3) eof MENU1
13225   end if
13226   fnapply_default_rates(mat extra, mat a)
13230   if final=1 or final=2 or final=3 or (trim$(px$)<>"" and x$<>px$) then goto READ_ROUTE_SEQUENCE
13235   px$=""
13240   goto ENTER_READING ! /r
14000 def fn_meter_roll
14020   mat txt$(4)
14040   txt$(1)="Reading: "&str$(cur_read)&"   Prior: "&str$(prior_read)&"   Usage: "&str$(x0)
14060   txt$(2)="Account: "&x$&" - "&aname$
14080   txt$(3)="Negative Usage on "&sn$
14100   txt$(4)="Is this a Meter Roll?"
14120   fnmsgbox(mat txt$,resp$,'',35)
14140   if resp$="No" then
14160     passcheck=ckfail
14180     goto METER_ROLL_XIT
14200   else if mroll(1)=1 then
14220     goto L3110
14240   end if
14260   if uprc$(sn$)=uprc$(srvnam$(1)) then
14280     cde=1
14300   else
14320     goto L3110
14340   end if
14360   xcde=1 : xcd2=11 : mroll(1)=1
14380   goto METER_ROLL_DONE ! water
14400   L3110: if mroll(3)=1 then goto L3140
14420   if uprc$(sn$)=uprc$(srvnam$(3)) then
14440     cde=5
14460   else
14480     goto L3140
14500   end if
14520   xcde=3 : xcd2=10 : mroll(3)=1
14540   goto METER_ROLL_DONE ! electric
14560   L3140: !
14580   if mroll(2)=1 then
14600     goto METER_ROLL_XIT
14620   end if
14640   if uprc$(sn$)=uprc$(srvnam$(4)) then
14660     cde=9
14680   else
14700     goto METER_ROLL_XIT
14720   end if
14740   xcde=2 : xcd2=12 : mroll(2)=1
14760   METER_ROLL_DONE: !
14780   digits=len(str$(d(cde)))
14800   x(xcde+xcd2)=10**digits-d(cde)+x(xcde)
14820   ! ** means to the power of
14840   METER_ROLL_XIT: !
14860 fnend
16000 def fn_print_readings(hWork; printReadings_altHeading$*40) ! pr proof of readings file
16020   totwat=totele=totgas=0
16040   fnopenprn
16060   fn_printReadings_Heading( printReadings_altHeading$)
16080   restore #hWork: ! ,search>="": nokey PR_TOTALS    <-- needs to work with or without an index
16200   do
16220     read #hWork,using F_WORK: x$,mat x eof PR_TOTALS
16240     totwat+=x(1): totele+=x(3): totgas+=x(2)
16260     e1$=e2$=""
16270     read #hCustomer1,using F_CUSTOMER_B,key=x$: e1$,e2$,mat d,f,mat a nokey PR_CUSTOMER_NOKEY
16280     F_CUSTOMER_B: form pos 11,2*c 30,pos 217,15*pd 5,pos 296,pd 4,pos 143,7*pd 2
16300     ! place usage in usage column if not usage already there so it shows on proof list
16320     ! Water
16340     if f<>d1 then oldreading=d(1) else oldreading=d(2)
16360     ! if x(12)=0 then x(12)=x(1)-oldreading
16380     if x(12)=0 and a(1)<>0 then x(12)=x(1)-oldreading ! A(1) checking was added 10/4/11 to prevent usage (and negative usage) on customers who have an (0) inactive rate code ! the whole line was commented out but added back in on 2/13/12
16400     ! Electric
16420     if f<>d1 then oldreading=d(5) else oldreading=d(6)
16440     if x(13)=0 then x(13)=x(3)-oldreading
16460     ! Gas
16480     if f<>d1 then oldreading=d(9) else oldreading=d(10)
16500     if x(14)=0 and a(4)<>0 then x(14)=max(0,x(2)-oldreading) ! A(4) checking was added 9/21/11 to prevent usage (and negative usage) on customers who have an (0) inactive rate code
16520     PR_CUSTOMER_NOKEY: !
16540     rc=0
16560     mat pc_data=(0)
16580     if service_enabled(1) then
16600       pc_data(rc+=1)=x(1)
16620       pc_data(rc+=1)=x(9)
16640       pc_data(rc+=1)=x(12)
16660     end if
16680     if service_enabled(2) then
16700       pc_data(rc+=1)=x(5)
16720     end if
16740     if service_enabled(3) then
16760       pc_data(rc+=1)=x(3)
16780       pc_data(rc+=1)=x(10)
16800       pc_data(rc+=1)=x(13)
16820       pc_data(rc+=1)=x(4)
16840     end if
16860     if service_enabled(4) then
16880       pc_data(rc+=1)=x(2)
16900       pc_data(rc+=1)=x(11)
16920       pc_data(rc+=1)=x(14)
16940     end if
16960     if service_enabled(5) then pc_data(rc+=1)=x(6)
16980     if service_enabled(6) then pc_data(rc+=1)=x(7)
17000     if service_enabled(7) then pc_data(rc+=1)=x(8)
17020     rc+=1
17040     if udim(mat pc_data)<rc then mat pc_data(rc)
17060     pc_data(rc)=x(15) ! Final Billing Code
17080     mat pc_data(rc)
17100     pr #255,using form$: x$,e2$(1:25),e1$(1:25),mat pc_data pageoflow PrintReadings_PgOf
17120   loop
17140   PR_TOTALS: !
17150   pr #255,using "form skip 2,c 30": "Batch Totals for Readings"
17160   pr #255,using "form pos 1,c 10,nz 20,skip 1,pos 1,c 10,nz 20,skip 1,pos 1,c 10,nz 20,skip 1": srvnam$(1)(1:10),totwat,srvnam$(3)(1:10),totele,srvnam$(4)(1:10),totgas
17180   fncloseprn
17220 fnend
17240 def fn_printReadings_Heading(;altHeading$*40)
17260   if altHeading$='' then altHeading$="Readings Proof List"
17280   pr #255,using 'Form Pos 20,Cc 40': altHeading$
17300   pr #255,using 'Form Pos 20,Cc 40': cnvrt$("pic(zz/zz/zz",d2)
17320   pr #255,using 'Form POS 1,C 220': reporth$
17340 fnend
17360 PrintReadings_PgOf: !
17380   pr #255: newpage
17400   fn_printReadings_Heading( printReadings_altHeading$)
17420 continue
17750 MAKE_CORRECTIONS: ! r:
17770   read #hWork,using F_WORK,key=x$: x$,mat x nokey MENU1
17790   t(1)-=1 ! SUBTRACT PROOF TOTALS
17800   for j1=1 to 15 : t(j1+1)-=x(j1) : next j1
17810   read #hCustomer1,using F_CUSTOMER_C,key=x$,release: x$,aname$,mat a,final,mat d,alp$,mat extra,extra$(3)
17812   fnapply_default_rates(mat extra, mat a)
17820   editmode=1
17840   goto ENTER_READING3 ! /r
17860 REWRITE_WORK: ! r:
17870   rewrite #hWork,using F_WORK,key=x$: trim$(x$),mat x nokey L3900
17880   goto L3910
17890 L3900: if trim$(uprc$(x$))<>trim$(uprc$("DELETED")) then let fn_writeWork(hWork,x$,mat x)
17900 L3910: if trim$(uprc$(x$))=trim$(uprc$("DELETED")) then goto MAKE_CORRECTIONS
17910   fn_accumulateprooftotals
17920   if editmode=1 then return
17930   goto MENU1 ! /r MAKE_CORRECTIONS
17950 CHANGE_ACT_NUM: ! r:
17960   fnTos(sn$="new_act_num")
17970   mylen=19 : mypos=mylen+2
17980   fnLbl(1,1,"New Account:",mylen)
17990   fnTxt(1,mypos,10)
18000   resp$(1)=""
18010   fnCmdSet(1)
18020   fnAcs(sn$,0,mat resp$,ck)
18030   x$=trim$(resp$(1))
18040   read #hCustomer1,using "Form POS 36,C 25",key=x$,release: aname$ nokey CHANGE_ACT_NUM
18050   goto REWRITE_WORK ! /r
18070 def fn_lo_pr_rec(x$,mat x)
18080   pr #255,using "form pos 1,c 10,x 2,4*pic(zzzzzzzzzz)": x$,x(1),x(2),x(3),x(4)
18090 fnend  ! fn_lo_pr_rec
18160 def fn_accumulateprooftotals
18170   t(1)+=1
18180   for j=1 to 15
18190     t(j+1)+=x(j)
18200   next j
18210 fnend
18220 def fn_checkwater
18230   if wr1=0 then let fn_us1
18240   if a(1)<>0 then ! skip routine if no water code
18250     sn$=srvnam$(1)
18260     if trim$(srvnam$(1))="" or mroll(1)=1 or (d(wr1)=0 and x(1)=0) then
18270       passcheck=ckpass
18280       goto CHECKWATER_FINIS
18290     end if
18300     if trim$(sn$)="Water" and x(12)>0 then
18310       passcheck=ckpass
18320       goto CHECKWATER_FINIS ! don't give warning if usage entered
18330     end if
18335     if env$('client')="Billings" and len(str$(x(1)))=6 and len(str$(d(wr1)))=7 then x(1)=val(str$(d(wr1))(1:1)&str$(x(1)))
18340     x4=x(1)-d(wr1)
18350     sn$=srvnam$(1) : x0=x4 : prior_read=d(wr1) : cur_read=x(1)
18360     if x4>=0 then goto CHECKWATER_L4260
18370     if x(12)>0 then sn$=srvnam$(1) : goto CHECKWATER_FINIS
18380     if x4<0 then let fn_meter_roll
18390   end if  ! a(1)<>0
18400   goto CHECKWATER_FINIS
18410   CHECKWATER_L4260: !
18420   if d(3)=0 then goto CHECKWATER_FINIS
18422   if uum_water<>0 and x0<uum_water then
18424     passcheck=ckpass
18430   else if x4<d(3)-d(3)*pcent or x4>d(3)+d(3)*pcent then
18440     passcheck=ckfail
18450   else
18460     passcheck=ckpass
18470   end if
18480   CHECKWATER_FINIS: !
18490   fn_checkend
18500 fnend
18510 def fn_checkelec
18520   if er1=0 then let fn_us1
18530   if a(3)=0 then goto CHECKELEC_FINIS ! if no electric code skip
18540   if trim$(sn$)="Electric" and x(13)>0 then passcheck=ckpass : goto CHECKELEC_FINIS ! don't give warning if usage entered
18550   if (service_type(3)=3 or (service_type(3)=3.1 and env$('client')<>"Thomasboro")) then
18560     goto L4350
18570   else
18580     passcheck=ckpass
18590     goto CHECKELEC_FINIS
18600   end if
18610   L4350: !
18620   if x(3)=0 and d(er1)=0 then goto CHECKELEC_FINIS
18630   x2=x(3)-d(er1)
18640   sn$=srvnam$(3) : x0=x2 : : prior_read=d(er1) : cur_read=x(3)
18650   if x2>=0 then goto L4420
18660   if x(13)>0 then sn$=srvnam$(3) : goto CHECKELEC_FINIS
18670   if x2<0 then let fn_meter_roll
18680   goto CHECKELEC_FINIS
18690   L4420: !
18700   if d(7)=0 then goto CHECKELEC_FINIS
18702   if uum_electric<>0 and x0<uum_electric then
18704     passcheck=ckpass
18710   else if x2<d(7)-d(7)*pcent or x2>d(7)+d(7)*pcent then
18720     passcheck=ckfail
18730   else
18740     passcheck=ckpass
18750   end if
18760   CHECKELEC_FINIS: !
18770   fn_checkend
18780 fnend
18790 def fn_checkgas
18800   if a(4)=0 then goto CHECKGAS_FINIS ! skip if no gas codes
18810   sn$=srvnam$(4)
18820   if trim$(srvnam$(4))<>"Gas" or mroll(2)=1 then
18830     passcheck=ckpass
18840     goto CHECKGAS_FINIS
18850   end if
18860   if trim$(sn$)="Gas" and x(14)>0 then passcheck=ckpass : goto CHECKGAS_FINIS ! don't give warning if usage entered
18870   if x(2)=0 and d(gr1)=0 then goto CHECKGAS_FINIS
18880   x3=x(2)-d(gr1)
18890   sn$=srvnam$(4): x0=x3 : prior_read=d(gr1): cur_read=x(2)
18900   if x3>=0 then goto CHECKGAS_L4580
18910   if x(14)>0 then sn$=srvnam$(4): goto CHECKGAS_FINIS
18920   if x3<0 then let fn_meter_roll
18930   goto CHECKGAS_FINIS
18940   CHECKGAS_L4580: !
18950   if d(11)=0 then goto CHECKGAS_FINIS
18952   if uum_gas<>0 and x0<uum_gas then
18954     passcheck=ckpass
18960   else if x3<d(11)-d(11)*pcent or x3>d(11)+d(11)*pcent then
18970     passcheck=ckfail
18980   else
18990     passcheck=ckpass
19000   end if
19010   CHECKGAS_FINIS: !
19020   fn_checkend
19030 fnend
19040 def fn_checkend
19050   if addmethod=from_holding_file and passcheck=ckfail then
19060     fn_print_unusual
19070     goto CHECKEND_XIT
19080   end if
19090   if passcheck=ckpass then goto CHECKEND_XIT
19100   if passcheck=ckfail and x0>=0 then
19110     mat txt$(8) : txt$(1)=sn$&" - Unusual Usage Warning"
19120     txt$(2)="Account: "&x$&" - "&aname$ : txt$(3)=""
19130     txt$(4)="Reading: "&str$(cur_read)&"   Prior: "&str$(prior_read)&"   Calculated Usage: "&str$(x0) : txt$(5)=""
19140     txt$(6)="Yes = Continue, the usage is correct."
19150     txt$(7)="No = Go Back, so I can re-enter the reading;"
19160     txt$(8)="Cancel = Do not enter a reading for that Customer."
19170     fnmsgbox(mat txt$,resp$,'',51)
19180   else
19190     goto CHECKEND_XIT
19200   end if
19210   if resp$="Yes" then
19220     passcheck=ckpass
19230   else if resp$="No" then
19240     passcheck=ckfail
19250   else if resp$="Cancel" then
19260     passcheck=ckfail
19270     editmode=0
19280   end if
19290   CHECKEND_XIT: !
19300 fnend
19320 def fn_print_unusual
19330   fnopenprn
19340   if ~setup_printunusual<=0 then
19350     setup_printunusual=1
19360     dim fmun$*80
19370     pr #255: " Account    Name                    Old Reading  New Reading   Usage"
19380     pr #255: "----------  ----------------------  -----------  -----------  -----------"
19390     fmun$="form c 12,c 22,3*n 13,x 2,c 20,skip 1"
19400   end if
19410   pr #255,using fmun$: x$,e2$(1:20),prior_read,cur_read,x0,sn$
19420 fnend
19520 def fn_hh_readings(ip1$; listonly) ! HH_READINGS: ! hand held routines
19540   device$=fnhand_held_device$
19560   if device$="Psion Workabout" then
19570     goto HH_WORKABOUT
19580   else if device$="Badger" then
19590     goto HH_BADGER
19600   else if device$="Boson" then
19610     goto HH_BOSON
19620   else if device$="Laptop" then
19630     gosub LAPTOP
19640     if listonly=1 then let fn_lo_pr_rec(x$,mat x) : goto HH_W_NXT
19650     goto HH_CONTINUE
19660   else ! if device$='Master Meter' or device$='READy Water' or device$="AMR" or device$="Other" or device$="Sensus" or device$="Green Tree" or device$="Hersey" or device$="EZReader" or device$="Itron FC300" or device$="" then
19670     goto HH_OTHER
19680   end if
19760   HH_WORKABOUT: ! r: hand held routines for workabout
19780   open #h_readings:=13: "Name=[Q]\UBmstr\Readings."&ip1$&",RecL=1",external,input,relative ioerr L4990
19790   goto L5000
19800   L4990: restore #h_readings:
19810   L5000: if listonly=1 then let fnopenprn( 0,0,0,0, 'Book '&ip1$)
19820   j1=29 : j2=97
19830   HH_W_READ: !
19840   ln$="" : mat x=(0)
19850   for j=j1 to j2
19860     read #h_readings,using "Form POS 1,C 1",rec=j: c$ noRec HH_W_END
19870     ln$=ln$&c$
19880   next j
19890   x$=lpad$(trim$(ln$(1:10)),10) : x(1)=val(ln$(11:19))
19900   mroll(1)=val(ln$(20:20)) : x(3)=val(ln$(21:29))
19910   mroll(3)=val(ln$(30:30)) : x(2)=val(ln$(31:39))
19920   mroll(2)=val(ln$(40:40)) : x(4)=val(ln$(41:49))
19930   ft$=rtrm$(ln$(50:69))
19940   if ft$="00000000000000000000" then ft$=""
19950   if listonly=1 then let fn_lo_pr_rec(x$,mat x) : goto HH_W_NXT
19960   if x$(1:1)="0" then x$(1:1)=" " ! drop leading zero
19970   if file(255)=-1 and rtrm$(ft$)<>"" then
19980     fnopenprn
19990   end if
20000   if trim$(ft$)<>"" then
20010     pr #255: "NEW NOTE! "&x$&" - "&ft$
20020   end if
20030   goto HH_CONTINUE ! /r
20040   HH_BADGER: ! r: Hand Held routines for Badger (badger file is copied from                        \connect\connect\x to readings.x in the transfer from                           Hand Held routine)
20060   if listonly=1 then let fnopenprn
20070   close #h_readings: ioerr ignore
20080   open #h_readings:=13: "Name=[Q]\UBmstr\Readings."&ip1$&",RecL=256",display,input
20090   HH_BADGER_READ: linput #h_readings: ln$ eof HH_W_END
20100   if ln$(1:1)="T" or ln$(1:1)="H" then goto HH_BADGER_READ
20110   mat x=(0)
20120   x$=lpad$(rtrm$(ln$(121:130)),10) conv HH_BADGER_READ ! Account Key
20130   ti1=1: ti1=val(ln$(64:64)) conv HH_BADGER_READ
20140   x(ti1)=val(ln$(96:104)) conv HH_BADGER_READ
20150   ! if env$('client')="Moweaqua" Then x(TI1)=X(TI1)
20160   if listonly=1 then let fn_lo_pr_rec(x$,mat x) : goto HH_W_NXT
20170   goto HH_CONTINUE ! /r
20180   !
20190   HH_BOSON: ! r: Hand Held routines for Boson (boson file is copied from                        [Q]\UBmstr\outofpalm.txt in hhfro to readings.(route# (which is asked))
20210   last_ln$=""
20220   if listonly=1 then let fnopenprn
20230   close #h_readings: ioerr ignore
20240   open #h_readings:=13: "Name=[Q]\UBmstr\Readings."&ip1$&",RecL=204",display,input
20250   HH_BOSON_READ: if last_ln$="" then linput #h_readings: ln$ eof HH_W_END else ln$=last_ln$ : last_ln$=''
20260   if ln$(1:1)="T" or ln$(1:1)="H" then goto HH_BOSON_READ
20270   mat x=(0)
20280   if env$('client')="Monticello" then x$=lpad$(rtrm$(ln$(1:10)),10) conv HH_BOSON_READ ! Account Key :goto 5150
20300   ti$=ln$(14:14)
20310   if ti$="W" or ti$="G" or ti$="E" then x$=lpad$(rtrm$(ln$(4:13)),10) conv HH_BOSON_READ else x$=lpad$(rtrm$(ln$(5:14)),10) conv HH_BOSON_READ : ti$="" : ti1=1
20370   if env$('client')="Monticello" then ti1=1: goto L5420
20380   if uprc$(ti$)="W" then ti1=1
20390   if uprc$(ti$)="G" then ti1=2
20400   if uprc$(ti$)="E" then ti1=3
20410   L5420: x(ti1)=0: x(ti1)=val(ln$(89:97)) conv L5440 ! kj 120308 allow boson to place codes in field if cant read meter
20415   ! if env$('client')="Billings" and ln$(91:91)=" " then x(ti1)=val("1"&ln$(92:97))
20420   if env$('client')="Moweaqua" and (a(1)=1 or a(1)=2) then x(ti1)=round(x(ti1)*.1,0)
20430   if ti$="" or ti$="W" then
20440     linput #h_readings: ln$ eof L5440
20450     if ti$="" then x_g$=lpad$(rtrm$(ln$(5:14)),10) conv L5440 else x_g$=lpad$(rtrm$(ln$(4:13)),10) conv L5440 : ti$=ln$(14:14)
20460     if x_g$=x$ and ti$="" or ti$="G" then
20470       x(2)=val(ln$(89:97))
20480       last_ln$=""
20490     else
20500       last_ln$=ln$
20510     end if
20520   end if
20530   L5440: !
20532   if env$('client')="Monticello" then
20540     read #hCustomer1,using 'form pos 1954,c 12',key=lpad$(trim$(x$),10): extra$(7) nokey L5480 ! monticello
20560     if trim$(extra$(7))="22" then x(ti1)=round(x(ti1)/100,0) ! monticello
20570   L5480: !
20572     if trim$(extra$(7))="23" then x(ti1)=round(x(ti1)/10,0) ! monticello
20580   !       If TRIM$(EXTRA$(7))="24" or TRIM$(EXTRA$(7))="65" or TRIM$(EXTRA$(7))="66"Then don't do anything ! monticello
20590     if trim$(extra$(7))="66" then x(ti1)=round(x(ti1)/10,0) ! monticello
20600     if trim$(extra$(7))="65" then x(ti1)=round(x(ti1)/100,0) ! monticello
20602   end if  ! t$="Monticello"
20610   goto HH_CONTINUE ! /r
20620   LAPTOP: ! r: readings from a laptop using acs meter reading software
20640     if listonly=1 then let fnopenprn
20650     close #h_readings: ioerr ignore
20660     open #h_readings:=13: "Name=[Q]\UBmstr\Readings."&ip1$&",RecL=50",display,input
20670     HH_LAPTOP_READ: linput #h_readings: ln$ eof HH_W_END
20680     mat x=(0)
20690     x$=lpad$(rtrm$(ln$(1:10)),10) conv HH_LAPTOP_READ ! Account Key
20700     !
20710     ti1=1: ti$=ln$(20:20) ! type of reading
20720     if uprc$(ti$)="W" then ti1=1
20730     if uprc$(ti$)="E" then ti1=2
20740     if uprc$(ti$)="G" then ti1=3
20750     x(ti1)=val(ln$(11:19)) conv HH_LAPTOP_READ
20760     read #hCustomer1,using F_CUSTOMER_C,key=x$,release: x$,aname$,mat a nokey ignore
20770   return ! if listonly=1 then let fn_lo_pr_rec(x$,mat x) : goto HH_W_NXT
20780   ! goto HH_CONTINUE ! /r
20790   HH_OTHER: ! r:
20810   if device$='AMR' then goto HH_OTHER_TYPE1
20820   if device$='EZReader' then goto HH_OTHER_TYPE1
20830   if device$='Green Tree' then goto HH_OTHER_TYPE1
20840   if device$='Hersey' then goto HH_OTHER_TYPE1
20850   if device$='Master Meter' then goto HH_OTHER_TYPE1
20860   if device$='READy Water' then goto HH_OTHER_TYPE1
20870   if device$='Sensus' then goto HH_OTHER_TYPE1
20880   fn_hh_other_type2(listonly)
20890   goto HH_W_END ! /r
20892   HH_OTHER_TYPE1: ! r:
20900   if listonly=1 then let fnopenprn
20910   close #h_readings: ioerr ignore
20930   open #h_readings:=13: "Name=[Q]\UBmstr\Readings."&ip1$&",RecL=30",display,input
20940   HH_OTHER_TYPE1_READ: !
20950   linput #h_readings: ln$ eof HH_W_END
20960   mat x=(0)
20970   x$=lpad$(rtrm$(ln$(1:10)),10) conv HH_OTHER_TYPE1_READ ! Account Key
20980   ti1=1 ! water
20982   x(ti1)=0
20984   if device$='READy Water' then 
20986     x(ti1)=val(ln$(11:len(ln$))) conv ignore
20990   else if env$('client')="Lovington" then 
20992     x(ti1)=val(ln$(11:19)) conv ignore
21000   else
21010     x(ti1)=val(ln$(11:20)) conv ignore
21020   end if
21030   read #hCustomer1,using F_CUSTOMER_C,key=x$,release: x$,aname$,mat a nokey ignore
21050   if listonly=1 then let fn_lo_pr_rec(x$,mat x) : goto HH_W_NXT
21060   goto HH_CONTINUE ! /r
21070   ! ______________________________________________________________________
21080   HH_CONTINUE: ! Continue with standard Hand Held routine
21090   read #hCustomer1,using F_CUSTOMER_C,key=x$,release: x$,aname$,mat a,final,mat d,alp$,mat extra,extra$(3) nokey HH_W_NXT
21100   fn_us1
21110   mat est1=(0)
21120   if x(1)=999999 then est1(1,1)=1 : est1(1,2)=100
21130   if x(2)=999999 then est1(3,1)=1 : est1(3,2)=100
21140   if x(3)=999999 then est1(2,1)=1 : est1(2,2)=100
21150   if sum(est1)=0 then goto L6010
21160   read #hCustomer1,using F_CUSTOMER_A,key=x$,release: x$,e2$,mat a,f,final,mat d,mat extra,extra$(3) nokey HH_W_NXT
21170   gosub EST2B
21180   L6010: !
21190   gosub CHECK_UNUSUAL
21200   if skiprec=1 then skiprec=0 : goto HH_W_NXT ! skip record   !kj 3/24/06
21210   fn_writeWork(hWork,x$,mat x)
21220   fn_accumulateprooftotals
21230   fn_rmk1
21240   HH_W_NXT: !
21250   if device$="Badger" then
21255     goto HH_BADGER_READ
21260   else if device$="Boson" then
21265     goto HH_BOSON_READ
21270   else if device$="Laptop" then
21271     goto HH_LAPTOP_READ
21272   else if device$="Psion Workabout" then
21274     j1+=72
21276     j2+=72
21278     goto HH_W_READ
21280   else ! if device$="Other" or device$="Sensus" or device$="AMR" or device$="Green Tree" or device$="Hersey" or device$="EZReader" then
21285     goto HH_OTHER_TYPE1_READ
21288   end if
21310   ! ___________________________
21320   HH_W_END: !
21330   fncloseprn
21340   addmethod=2 ! set back to regular readings
21350   close #h_readings:
21360 fnend  ! goto MENU1
22320 EST1: ! r: ESTIMATEING ROUTINE
22330   close #hWork:
22340   execute 'Index '&workFile$&' '&workFileIndex$&' 1 10 Replace,DupKeys -n'
22360   open #hWork:=fngethandle: "Name="&workFile$&",KFName="&workFileIndex$,internal,outIn,keyed
22370   ASK_EST: !
22380   fnTos(sn$="ubipchg-ask_est")
22390   ! services=0
22400   if srvnam$(1)="Water" then srvest$(1)=srvnam$(1) else srvest$(1)="Unused"
22410   if service_enabled(3) then srvest$(2)=srvnam$(3) else srvest$(2)="Unused"
22420   if service_enabled(4) then srvest$(3)=srvnam$(4) else srvest$(3)="Unused"
22430   mylen=0
22440   for j=1 to 3 : mylen=max(mylen,len(srvest$(j))) : next j
22450   fnFra(1,1,8,mylen+50,"Select and Configure Services to Estimate")
22460   fnLbl(2,mylen+12,"% of Average",15,0,0,1)
22470   for j=1 to 3
22480     resp$(j*2-1)="False" : resp$(j*2)=""
22490     if srvest$(j)="" or srvest$(j)="Unused" then disable=1 else disable=0
22500     fnChk(j+2,mylen,srvest$(j),0,1) ! add disable here
22510     ! fnTxt(J+2,MYLEN+4,2,0,0,"30",DISABLE,EMPTY$,1)
22520     fnTxt(j+2,mylen+14,3,0,0,"30",disable,empty$,1)
22530   next j
22540   fnLbl(7,1,"% of average would normally be from 75 to 125 %",52,2,0,1)
22550   fnFra(11,1,3,50,"Account Selection Method")
22560   fnOpt(1,1,"Individual Accounts",0,2)
22570   resp$(7)="False"
22580   fnOpt(2,1,"Route",0,2)
22590   resp$(8)="True"
22600   fnCmdSet(2)
22610   fnAcs(sn$,0,mat resp$,ck)
22620   if ck=cancel then goto MENU1
22630   for j=1 to 3
22640     if uprc$(resp$(j*2-1))=uprc$("True") then est1(j,1)=1 else est1(j,1)=0
22650     est1(j,2)=val(resp$(j*2)) conv EST1
22660   next j
22670   if est1(1,1)=0 and est1(2,1)=0 and est1(3,1)=0 then goto L6520 else goto L6530
22680   L6520: !
22690   mat message$(1)
22700   message$(1)="You must select at least one service to estimate"
22710   fnmsgbox(mat message$,resp$,'',0)
22720   goto ASK_EST
22730   L6530: for j=1 to 3
22740     if est1(j,1)=0 then goto L6570
22750     if est1(j,2)<50 or est1(j,2)>150 then goto L6560 else goto L6570
22760   L6560: !
22770     mat message$(1)
22780     message$(1)="You percent must be between 50% and 150%"
22790     fnmsgbox(mat message$)
22800     goto ASK_EST
22810   L6570: !
22820   next j
22830   if ck=cancel then goto MENU1
22840   if uprc$(resp$(7))=uprc$("True") then est1=1
22850   if uprc$(resp$(8))=uprc$("True") then est1=2 ! select route #
22860   fn_est_dates
22870   if est1=1 then goto EST3 ! selecting individual accounts to estimate
22880   if est1=2 then goto ASK_ROUTE
22890 goto ASK_EST ! /r
22910 EST3: ! r:
22920   fnTos(sn$="ubipchg-est3")
22930   mylen=27 : mypos=mylen+2
22940   fnLbl(1,1,"Account to Estimate:",mylen,1)
22950   fncmbact(1,mypos)
22960   resp$(1)=""
22970   if ex$<>"" then
22980     fnLbl(3,1,"Last Account entered:",mylen,1)
22990     fnTxt(3,mypos,10,0,0,empty$,1)
23000     resp$(2)=ex$
23010   end if
23020   fnCmdSet(11)
23030   fnAcs(sn$,0,mat resp$,ck)
23040   x$=lpad$(trim$(resp$(1)(1:10)),10)
23050   if ck=cancel or trim$(x$)="" then goto MENU1
23060   x$=lpad$(trim$(x$),10) conv EST3
23070   read #hCustomer1,using F_CUSTOMER_A,key=x$,release: x$,e2$,mat a,f,final,mat d,mat extra,extra$(3) nokey EST3
23072   fnapply_default_rates(mat extra, mat a)
23080   F_CUSTOMER_A: form pos 1,c 10,pos 41,c 30,pos 143,7*pd 2,pos 296,pd 4,pos 1821,n 1,pos 217,15*pd 5,pos 1741,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,c 30,7*c 12,3*c 30
23090   gosub EST2
23100   ex$=x$
23110   goto EST3 ! /r
23130 ASK_ROUTE: ! r:
23140   fnTos(sn$="ubipchg-ask_Route")
23150   mylen=21 : mypos=mylen+2 : respc=0
23160   fnLbl(1,1,"Route to Estimate:",mylen,1)
23170   fncmbrt2(1,mypos,0)
23180   resp$(respc+=1)="1"
23190   if eb2>0 then
23200     fnLbl(3,1,"Last Route estimated:")
23210     fnTxt(3,mypos,4)
23220     resp$(respc+=1)=str$(eb2)
23230   end if
23240   fnCmdSet(11)
23250   fnAcs(sn$,0,mat resp$,ck)
23260   if resp$(1)="[All]" then eb1=0 : goto L6890
23270   eb1=val(resp$(1))
23280 L6890: !
23290   if ck=cancel then goto MENU1 ! finish
23300   restore #hCustomer1:
23310 READ_CUSTOMER: !
23320   read #hCustomer1,using F_CUSTOMER_A,release: x$,e2$,mat a,f,final,mat d,mat extra,extra$(3) eof ASK_ROUTE
23330   if final=1 or final=2 or final=3 then goto READ_CUSTOMER
23332   fnapply_default_rates(mat extra, mat a)
23340 ! fn_US1
23350   if eb1>0 and extra(1)><eb1 then goto READ_CUSTOMER ! if route selected and does not match route
23360   if final=1 or final=3 then goto READ_CUSTOMER ! SKIP FINAL BILLED
23370   gosub EST2
23380   eb2=eb1
23390   goto READ_CUSTOMER ! /r
23400 EST2: ! r:
23410   mat x=(0) ! actually calculate the estimated usage
23420   EST2B: !
23430   a1=est4=0
23440   read #hWork,using F_WORK,key=x$: x$,mat x nokey L7060
23450   a1=1
23460   t(1)-=1 ! Reverse Proof Totals
23470   for j=1 to 15 : t(j+1)=t(j+1)-x(j) : next j
23480   L7060: !
23490   for j=1 to 3
23500     if j=1 and a(1)=0 then
23510       goto L7140
23520     else if j=2 and a(3)=0 then
23530       goto L7140
23540     else if j=3 and a(4)=0 then
23550       goto L7140 ! took this off front 71509  If EST1(J,1)=0 Then Goto 6790 Else
23560     end if
23570     fn_est5
23580     if f=d1 then oldwatread=d(2) else oldwatread=d(1) ! old water reading equals the prior reading if recalculation else current reading if new calculation
23590     if f=d1 then oldelecread=d(6) else oldelecread=d(5) ! old electric reading equals the prior reading if recalculation else current reading if new calculation
23600     if f=d1 then oldgasread=d(10) else oldgasread=d(9) ! old gas reading equals the prior reading if recalculation else current reading if new calculation
23610     if j=1 then
23620       x(1)=oldwatread+watavg
23630     else if j=2 then
23640       x(3)=oldelecread+elecavg
23650     else if j=3 then
23660       x(2)=oldgasread+gasavg
23670     end if
23680     est4=1
23690     L7140: !
23700   next j
23710   ! If A(2)>0 AND EST1(1,1)=1 Then eST4=1 ! Sewer
23720   if est4=0 then goto L7220
23730   if addmethod=from_holding_file then goto L7220 ! FROM Hand Held
23740   if a1=1 then
23750     rewrite #hWork,using F_WORK: trim$(x$),mat x
23760     goto L7210
23770   end if
23780   fn_writeWork(hWork,x$,mat x)
23790   rewrite #hCustomer1,using "Form pos 1831,n 9",key=x$: d1 ! write billing date into bill estimated field  extra(19) any time bill estimated
23800   L7210: !
23810   fn_accumulateprooftotals
23820   L7220: !
23830 return  ! /r
34040 XIT: fnxit
34050 IGNORE: continue
34320 def fn_us1
34340   rc1=0 ! SET USAGE FIELDS
34360   wr1=1 : er1=5 : gr1=9
34380   read #hCustomer1,using "Form POS 296,PD 4",key=x$,release: f nokey US1_XIT
34400   if f><d1 then goto US1_XIT
34420   wr1=2 : er1=6 : gr1=10 : rc1=1 ! Re-Calculation
34440   US1_XIT: !
34460 fnend
34480 def fn_rmk1
34500   ! rk$=x$(1:10)
34520   if ft$="" then goto RMK1_XIT
34540   ft$="*"&ft$
34560   ! Read #note1,Using "Form POS 1,C 10,2*PD 3",Key=RK$: RK$,MAT RA Nokey 6580
34580   r32=ra(1)
34600   RMK1_L8110: !
34620   if r32=0 then goto RMK1_L8190
34640   ! Read #note2,Using "Form POS 1,C 10,C 60,PD 3",Rec=R32: K32$,RM$,N32
34660   if rm$(1:1)><"*" then goto RMK1_L8160
34680   ! Rewrite #note2,Using "Form POS 1,C 10,C 60,PD 3",Rec=R32: K32$,FT$
34700   goto RMK1_XIT
34720   RMK1_L8160: !
34740   r32=n32
34760   goto RMK1_L8110
34780   mat ra=(0)
34800   ! Write #note1,Using "Form POS 1,C 10,2*PD 3": RK$,MAT RA
34820   RMK1_L8190: !
34840   r32=lrec(32)+1
34860   ! Write #note2,Using "Form POS 1,C 10,C 60,PD 3",Rec=R32: RK$,FT$,0
34880   rn=rn+1
34900   ! If RA(2)>0 Then
34920   ! Rewrite #note2,Using "Form POS 68,PD 3",Rec=RA(2): R32
34940   ! end if
34960   if ra(1)=0 then ra(1)=r32
34980   ra(2)=r32
35000   ! Rewrite #note1,Using "Form POS 11,2*PD 3",Key=RK$: MAT RA
35020   RMK1_XIT: !
35040 fnend
35060 ! <Updateable Region: ERTN>
35080 ERTN: fnerror(program$,err,line,act$,"XIT")
35100   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
35120   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
35140   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
35160 ERTN_EXEC_ACT: execute act$ : goto ERTN
35180 ! /region
35200 def fn_est_dates
35220   EST_DATES: !
35240   fnTos(sn$="estimate-1")
35260   mylen=51 : mypos=mylen+2
35280   fnLbl(2,70,"",0,1)
35300   fnLbl(1,1,"Billing Dates of Months to be Averaged:",mylen,1)
35320   for j=1 to 8
35340     fnTxt(j,mypos,10,0,0,"3")
35360     resp$(j)=""
35380   next j
35400   fnCmdSet(2)
35420   fnAcs(sn$,0,mat resp$,ckey)
35440   if ckey=cancel then goto XIT
35460   for j=1 to 8
35480     cd1(j)=val(resp$(j)) conv EST_DATES
35500   next j
35520   if cd1(1)=0 then
35540     mat message$(1)
35560     message$(1)="You must enter at least one date!"
35580     fnmsgbox(mat message$)
35600     goto EST_DATES
35620   end if
35640 fnend
35660 def fn_est5(;___,j) ! calculate averages
35680   watermonths=elecmonths=gasmonths=watused=elecused=gasused=0
35700   restore #hTrans,key>=x$&"         ": nokey EST5_XIT ! no average but active customer (use 0 usage)
35720   EST5_READ_TRANS: !
35740   read #hTrans,using F_TRANS: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof EST5_FINIS
35760   F_TRANS: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
35780   if p$<>x$ then goto EST5_FINIS
35800   if tcode<>1 then goto EST5_READ_TRANS ! only charge transactions
35820   for j=1 to 8
35840     if est1(1,1)=1 and cd1(j)=tdate then watermonths+=1: watused+=wu
35860     if est1(2,1)=1 and cd1(j)=tdate then elecmonths+=1: elecused+=eu
35880     if est1(3,1)=1 and cd1(j)=tdate then gasmonths+=1 : gasused+=gu
35900   next j
35920   ! probably a mat x proof total problem right here
35940   goto EST5_READ_TRANS
35960   EST5_FINIS: !
35980   watavg=elecavg=gasavg=0
36000   if watermonths>0 then watavg=int(watused/watermonths)
36020   if elecmonths>0 then elecavg=int(elecused/elecmonths)
36040   if gasmonths>0 then gasavg=int(gasused/gasmonths)
36060   EST5_XIT: ! write enter readings entry
36080 fnend
38700 def fn_rewrite_usage
38720   if servicetype$="WA" then x(12)=usage
38740   if servicetype$="GA" then x(14)=usage
38760   if servicetype$="EL" then x(13)=usage
38780   rewrite #hWork,using F_WORK: trim$(x$),mat x
38800 fnend
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
40160     x=val(a$(1:3)) conv IT_TEXT_READ
40180     z$=""
40200     for j=1 to 8
40220       x=val(a$(j:j)) conv IT_L1060
40240       z$=z$&a$(j:j)
40260     next j
40280 IT_L1060: !
40300     z=val(z$)
40320     z$=cnvrt$("pic(zzzzzzz.##",z)
40340     reading$=""
40360     for j1=1 to 20
40380       x=val(a$(j1+j:j1+j)) conv IT_L1120
40400       reading$=reading$&a$(j1+j:j1+j)
40420 IT_L1120: !
40440     next j1
40460     pr #h_readings_tmp,using "form pos 1,c 10,c 9": z$,trim$(reading$)
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
40700     x$=lpad$(rtrm$(ln$(1:10)),10) conv IT_W_NEXT ! Account Key
40720     ti1=1 ! water
40740     if env$('client')="Lovington" then x(ti1)=0: x(ti1)=val(ln$(11:19)) conv IT_L5870 : goto IT_L5870
40780     x(ti1)=0: x(ti1)=val(ln$(11:20)) conv ignore
40800 IT_L5870: !
40820     read #hCustomer1,using F_CUSTOMER_C,key=x$,release: x$,aname$,mat a nokey ignore
40860 !       goto HH_CONTINUE
40880 !   ! ______________________________________________________________________
40900 !   HH_CONTINUE: ! Continue with standard Hand Held routine
40920     read #hCustomer1,using F_CUSTOMER_C,key=x$,release: x$,aname$,mat a,final,mat d,alp$,mat extra,extra$(3) nokey IT_W_NEXT
40930     fnapply_default_rates(mat extra, mat a)
40940     fn_us1
40960     mat est1=(0)
40980     if x(1)=999999 then est1(1,1)=1 : est1(1,2)=100
41000     if x(2)=999999 then est1(3,1)=1 : est1(3,2)=100
41020     if x(3)=999999 then est1(2,1)=1 : est1(2,2)=100
41040     if sum(est1)<>0 then
41060       read #hCustomer1,using F_CUSTOMER_A,key=x$,release: x$,e2$,mat a,f,final,mat d,mat extra,extra$(3) nokey IT_W_NEXT
41070       fnapply_default_rates(mat extra, mat a)
41080       gosub EST2B
41100     end if
41120     gosub CHECK_UNUSUAL
41160     fn_writeWork(hWork,x$,mat x)
41180     fn_accumulateprooftotals
41200     fn_rmk1
41220 IT_W_NEXT: !
41240   loop
41260 ! /r
41280 IT_FINIS: !
41300   fncloseprn
41320   addmethod=2 ! set back to regular readings
41340   close #h_readings,free:
41350   fnFree(workFileIndex$)
41360 IT_XIT: !
41380   goto MENU1 ! /r
42000 MENU1: ! r:
42020   editmode=0
42060   addmethod=0
42080   fnTos(sn$="readings-1b")
42100   mylen=28 : mypos=mylen+3
42120   frame_bd_witdh=42
42140   fnFra(1,1,5,frame_bd_witdh,"Batch Data")
42160   disable=0 ! If LREC(hWork)>1 Then dISABLE=1 Else dISABLE=0
42180   fnLbl(2,2,"Billing Date:",mylen,1,0,1)
42200   fnTxt(2,mypos,8,0,0,"1001",disable,empty$,1)
42220   resp$(1)=str$(d1)
42240   fnLbl(4,2,"Meter Reading Date:",mylen,1,0,1)
42260   fnTxt(4,mypos,8,0,0,"1",disable,empty$,1)
42280   resp$(2)=str$(d2)
42300 !
42640   moe_button_width=frame_bd_witdh-1
42660   fnFra(8,1,20,moe_button_width+1,"Add Readings") : frame_current=2 : frame_line=0
42680   fnLbl(frame_line+=2,2,"Individuals:",0,0,0,frame_current)
42700   fnButton(frame_line+=1,1,"Display customers in route sequence",2001,'Display each customer in route sequence',0,moe_button_width,frame_current)
42720   fnButton(frame_line+=1,1,"Ask Account, then enter Reading",2002,'Ask Account, then enter Reading',0,moe_button_width,frame_current)
42740 !
42760   fnLbl(frame_line+=2,2,"Bulk:",0,0,0,frame_current)
42780   fnButton(frame_line+=1,1,"Load Holding File",2003,'Retrieve readings previously saved to a Holding File',0,moe_button_width,frame_current)
42800   fnButton(frame_line+=1,1,"Estimate Readings",2004,'',0,moe_button_width,frame_current)
42820   fnButton(frame_line+=1,1,"Import from Tab Delimited Text File",2006,'',0,moe_button_width,frame_current)
42840 !
42860   if fnregistered_for_hh then
42880     fnLbl(frame_line+=2,2,"Hand Held:",0,0,0,frame_current)
42900     fnButton(frame_line+=1,1,"Import from Hand Held to Book",2007,'Retrieve Hand Held File',0,moe_button_width,frame_current)
42920     fnButton(frame_line+=1,1,"Load Hand Held Book",2005,'Generally for use after "Retreive (Import) from Hand Held to Book"',0,moe_button_width,frame_current)
42940   end if  ! fnregistered_for_hh
44000 ! r: add the grid
44010 !
44020   chc=0
44030   mat colhdr$(30)
44040   mat cm$(30)
44050   mat item$(30)
44060   colhdr$(chc+=1)="Account"
44070   reporth$="   Account  Customer Name              Customer Address           "
44080   form$="Form pos 1,c 10,x 2,c 25,x 2,c 25"
44090 ! r: Service 1 - Water
44100   if service_enabled(1) then
44110     colhdr$(chc+=1)=srvnam$(1)&" Reading"
44120     cm$(chc)="20"
44130     colhdr$(chc+=1)=srvnam$(1)&" Charge"
44140     cm$(chc)="10"
44150     colhdr$(chc+=1)=srvnam$(1)&" Usage"
44160     cm$(chc)="20"
44170 !
44180     reporth$=reporth$&srvnam$(1)(1:2)&" Read   "&srvnam$(1)(1:2)&" Chg  "&srvnam$(1)(1:2)&" Usage "
44190     form$=form$&",n 9,n 9.2,n 9"
44200   end if
44210 ! /r
44220 ! r: Service 2 - Sewer
44230   if service_enabled(2) then
44240     colhdr$(chc+=1)=srvnam$(2)&" Charge"
44250     cm$(chc)="10"
44260 !
44270     reporth$=reporth$&" "&srvnam$(2)(1:2)&" Chg  "
44280     form$=form$&",n 9.2"
44290   end if
44300 ! /r
44310 ! r: Service 3 - Electric
44320   if service_type(3)=3 then
44330     colhdr$(chc+=1)=srvnam$(3)&" Reading"
44340     cm$(chc)="20"
44350     colhdr$(chc+=1)=srvnam$(3)&" Charge"
44360     cm$(chc)="10"
44370     colhdr$(chc+=1)=srvnam$(3)&" Usage"
44380     cm$(chc)="20"
44390     colhdr$(chc+=1)="Demand"
44400     cm$(chc)="20"
44410 !
44420     reporth$=reporth$&srvnam$(3)(1:2)&" Read   "&srvnam$(3)(1:2)
44430     reporth$=reporth$&" Chg  "&srvnam$(3)(1:2)&" Usage   Demand "
44440     form$=form$&",n 9,n 9.2,n 9,n 9"
44450   else if service_type(3)=3.1 then
44460     colhdr$(chc+=1)=srvnam$(3)&" Reading"
44470     cm$(chc)="20"
44480     colhdr$(chc+=1)=srvnam$(3)&" Charge"
44490     cm$(chc)="10"
44500     colhdr$(chc+=1)=srvnam$(3)&" Usage"
44510     cm$(chc)="20"
44520 !
44530     reporth$=reporth$&srvnam$(3)(1:2)&" Read   "&srvnam$(3)(1:2)
44540     reporth$=reporth$&" Chg  "&srvnam$(3)(1:2)&" Usage "
44550     form$=form$&",n 9,n 9.2,n 9"
44560   else if service_type(3)=3.2 then
44570     colhdr$(chc+=1)=srvnam$(3)&" Usage"
44580     cm$(chc)="20"
44590 !
44600 ! LOOKS LIKE SOMETHING IS MiSSING HERE
44610   end if
44620 ! /r
44630 ! r: Service 4 - Gas
44640   if service_enabled(4) then ! if service_type(4)=4 then
44650     colhdr$(chc+=1)=srvnam$(4)&" Reading"
44660     cm$(chc)="20"
44670     colhdr$(chc+=1)=srvnam$(4)&" Charge"
44680     cm$(chc)="10"
44690     colhdr$(chc+=1)=srvnam$(4)&" Usage"
44700     cm$(chc)="20"
44710 !
44720     reporth$=reporth$&srvnam$(4)(1:2)&" Read   "&srvnam$(4)(1:2)
44730     reporth$=reporth$&" Chg  "&srvnam$(4)(1:2)&" Usage "
44740     form$=form$&",n 9,n 9.2,n 9"
44750   end if
44760 ! /r
44770 ! r: Service 5 - Oother
44780   if service_enabled(5) then ! always show "Other Charge"
44790     colhdr$(chc+=1)=srvnam$(5)&" Charge"
44800     cm$(chc)="10"
44810 !
44820     reporth$=reporth$&" "&srvnam$(5)(1:2)&" Chg  "
44830     form$=form$&",n 9.2"
44840   end if
44850 ! /r
44860 ! r: Service 6
44870   if service_enabled(6) then ! always show "Other Charge"
44880     colhdr$(chc+=1)=srvnam$(6)&" Charge"
44890     cm$(chc)="10"
44892 !
44900     reporth$=reporth$&" "&srvnam$(6)(1:2)&" Chg  "
44910     form$=form$&",n 9.2"
44920   end if
44930 ! /r
44940 ! r: Service 7
44950   if service_enabled(7) then ! always show "Other Charge"
44960     colhdr$(chc+=1)=srvnam$(7)&" Charge"
44970     cm$(chc)="10"
44980 !
44990     reporth$=reporth$&" "&srvnam$(7)(1:2)&" Chg  "
45000     form$=form$&",n 9.2"
45010   end if
45020 ! /r
45030 ! r: Service 8
45040   if service_enabled(8) then ! always show "Other Charge"
45050     colhdr$(chc+=1)=srvnam$(8)&" Charge"
45060     cm$(chc)="10"
45070 !
45080     reporth$=reporth$&" "& srvnam$(8)(1:2)&" Chg  "
45090     form$=form$&",n 9.2"
45100   end if
45110 ! /r
45120 ! r: final billing code
45130   colhdr$(chc+=1)=" F/B"
45140   cm$(chc)="30"
45150 !
45160   reporth$=reporth$&" Final "
45170 !
45180   form$=form$&",n 9"
45190 ! /r
45200   mat colhdr$(chc) : mat cm$(chc)
45210 !
45220   fnflexinit1("Work",2,frame_bd_witdh+3,28,74,mat colhdr$,mat cm$,1)
45750    entryCount=0
45760   ic=0
45770   restore #hWork:
45780   batchtot=0
45790   do
45800     read #hWork,using F_WORK: x$,mat x eof MENU1READWORKEOF
45810     ic=0
45820     item$(ic+=1)=x$
45830     batchtot+=val(x$) conv L1100
45840     L1100: !
45850     if service_enabled(1) then
45860       item$(ic+=1)=str$(x(01))
45870       item$(ic+=1)=str$(x(09))
45880       item$(ic+=1)=str$(x(12)) ! water
45890     end if
45900     if service_enabled(2) then
45910       item$(ic+=1)=str$(x(05)) ! sewer
45920     end if
45930     if service_type(3)=3.2 then
45940       item$(ic+=1)=str$(x(13)) ! eletric
45950     else if service_type(3)=3 then
45960       item$(ic+=1)=str$(x(03))
45970       item$(ic+=1)=str$(x(10))
45980       item$(ic+=1)=str$(x(13))
45990       item$(ic+=1)=str$(x(04)) ! eletric
46000     end if
46010     if service_type(3)=3.1 then
46020       item$(ic+=1)=str$(x(03))
46030       item$(ic+=1)=str$(x(10))
46040       item$(ic+=1)=str$(x(13))
46050     end if
46060     if service_enabled(4) then
46070       item$(ic+=1)=str$(x(02))
46080       item$(ic+=1)=str$(x(11))
46090       item$(ic+=1)=str$(x(14)) ! gas
46100     end if
46110     if service_enabled(5) then ! service 5
46120       item$(ic+=1)=str$(x(06))
46130     end if
46140     if service_enabled(6) then ! service 6
46150       item$(ic+=1)=str$(x(07))
46160     end if
46170     if service_enabled(7) then ! service 7
46180       item$(ic+=1)='' ! str$(x(07))  ! x(??)   07 is used in another place for service 7 but it is also used for service 6
46190     end if
46200     if service_enabled(8) then ! service 8
46210       item$(ic+=1)=str$(x(08))
46220     end if
46230     item$(ic+=1)=str$(x(15)) ! final billing code
46232     entryCount+=1
46240     fnflexadd1(mat item$) ! pr mat item$ : pause
46250   loop
47580 MENU1READWORKEOF: ! /r
47590   fnLbl(1,frame_bd_witdh+4,'Entry Count: '&str$(entryCount))
47592   fnButton(1,frame_bd_witdh+21,'Clear All',fky_clearAll:=2008)
47600   if lrec(hWork)>0 then
47620 !   fnCmdKey("&Add",1)
47640     fnCmdKey("E&dit",2,1,0,'Edit highlighted record by clicking this button, pressing enter or double clicking the record.')
47660     fnCmdKey("&Print",4,0,0,'Print a proof listing of the entered records.')
47680     fnCmdKey("Save to &Holding File",fkey_saveToHoldingFile:=6,0,0,'Save entered readings to a Holding File for later calculation.')
47700     fnCmdKey("&Delete",8)
47720     fnCmdKey("&Close",5,0,1)
47740     fnCmdKey("&Meter Change",9,0,0,"Calculates usage on meter change out.")
47760     fnCmdKey("&Finish and Calculate",10,0,0,'Calculate entered readings')
47780   else
47800     fnCmdKey("&Close",5,0,1)
47801     ! fnCmdSet(1)
47820   end if
47840   fnAcs(sn$,0,mat resp$,ck)
47860   if ck=cancel then
47920     goto XIT
47940   end if
47960   d1=val(resp$(1))
47980   d2=val(resp$(2))
48000   fnLastBillingDate(d1,1)
48020   fncreg_write('Meter Reading Date Current',str$(d2))
48240   x$=lpad$(trim$(resp$(3)(1:10)),10) ! formerly resp$(9)
48260   if lrec(hWork)>0 and ck=2 then
48280     goto MAKE_CORRECTIONS
48300   end if
49000   if ck=4 then
49020     fn_print_readings(hWork)
49040   else if fkey_saveToHoldingFile and ck=fkey_saveToHoldingFile then ! add to holding file
49060     if fn_holdingFileSave(hWork) then goto XIT
49080   else if ck=8 then
49100     delete #hWork,key=x$:
49140   else if ck=9 then
49160     if fn_meter_change_out=3 then goto ENTER_READING3
49180   else if ck=10 then
49200     fnchain("S:\acsUB\ubCalk") ! goto CALCULATE
49220   else if ck=2001 then
49240     addmethod=1
49260     goto AUTO_REC
49280   else if ck=1 or ck=2002 then
49300     addmethod=2
49320     goto SEL_ACC
49340   else if ck=2003 then
49360     addmethod=3
49380     fn_loadBookOrHoldingFile(addmethod)
49400   else if ck=2004 then
49420     addmethod=4
49440     goto EST1
49460   else if ck=2005 then
49480     addmethod=from_hh_file
49500     fn_loadBookOrHoldingFile(addmethod)
49520   else if ck=2006 then
49540     addmethod=6
49560     goto INPUT_TEXT
49580   else if ck=2007 then
49600     fnRetrieveHandHeldFile
49610     fntop(program$)
49612   else if fky_clearAll and ck=fky_clearAll then
49613     close #hWork:
49614     fnFree(workFile$)
49616     fnFree(workFileIndex$)
49618     open #hWork:=fngethandle: "Name="&workFile$&",KFName="&workFileIndex$&",Shr,Use,RecL=74,KPs=1,KLn=10",internal,outIn,keyed
49620   end if
49640   goto MENU1
49680 ! /r MENU1

52000 def fn_holdingFileLoad
52020   holdingFile$="[Q]\UBmstr\IpHold"&ip1$&".h[cno]"
52040   open #hld9=9: "Name="&holdingFile$,internal,input ioerr L7460
52060   do
52080     read #hld9,using F_WORK: x$,mat x eof IPHOLD_EO_HLD9
52100     fn_writeWork(hWork,x$,mat x, 1)
52120     fn_accumulateprooftotals
52140   loop
52160   IPHOLD_EO_HLD9: !
52180   close #hld9:
52200   L7460: !
52220   addmethod=1 ! set addmethod back to 1 once holding file read in
52240 fnend
56000 def fn_holdingFileSave(hWork) ! probably requires more than just hWork
56020   holdingFileSaveReturn=0
56040   HoldingFileSave: !
56060   fnTos(sn$="holding")
56080   mylen=19 : mypos=mylen+2
56100   fnLbl(1,1,"Holding File Number:",mylen)
56120   fnTxt(1,mypos,3,0,0,"30")
56140   resp$(1)=""
56160   fnFra(4,1,3,94,"Create new file or append to existing file","If you have a different file for each route, you will always take the option to create a new file.  If you only use one file, clean it on the first batch of readings and append the rest.")
56180   fnOpt(1,1,"Create new file (deletes all previous readings in holding file)",0,1)
56200   resp$(respc_CreateNew:=2)="False"
56220   fnOpt(2,1,"Append to existing file (retain previous readings, merge new ones in, overwrites duplicates)",0,1)
56240   resp$(3)="True"
56260   fnCmdKey("&Save",1,1)
56280   fnCmdKey("&Cancel",5,0,1)
56300   fnAcs(sn$,0,mat resp$,ck)
56320   if ck<>cancel then
56340     holdingFileSaveReturn=1
56360     bk1=val(resp$(1)) conv HoldingFileSave
56380     if bk1<=0 then goto HoldingFileSave
56400     if uprc$(resp$(respc_CreateNew))=uprc$("True") and exists('[Q]\UBmstr\IpHold'&str$(bk1)&'.h[cno]') then ! Create New Holding File
56420       fnFree('[Q]\UBmstr\IpHold'&str$(bk1)&'.h[cno]')
56440     end if
56460     ! Append to Existing Holding File
56480     dim holdingFile$*256
56500     dim holdingFileIndex$*256
56520     holdingFile$="[Q]\UBmstr\IpHold"&str$(bk1)&".h[cno]"
56540     holdingFileIndex$=env$('temp')&"\acs\IpHold"&str$(bk1)&"-Index.h[cno]"
56560     fnindex_it(holdingFile$,holdingFileIndex$,'1 10')
56580     open #hld8:=fngethandle: "Name="&holdingFile$&",KFName="&holdingFileIndex$&',Shr,Use,RecL=74,KPs=1,KLn=10',internal,outIn,keyed
56600     restore #hWork: ! ,search>="": nokey AppendFinis
56620     do
56640       read #hWork,using F_WORK: x$,mat x eof AppendFinis
56660       fn_writeWork(hld8,x$,mat x, 1)
56680     loop
56700     AppendFinis: !
56720     close #hld8:
56740     fnStatusClose
56760     close #hWork:
56780     fnFree(workFile$)
56800     fnFree(workFileIndex$)
56820   end if
56840   fn_holdingFileSave=holdingFileSaveReturn
56860 fnend
58000 def fn_loadBookOrHoldingFile(&addmethod)
58020   dim ihDirFileMask$*64
58040   !
58060   if addmethod=from_hh_file then
58080     book_or_holding_file$='Book'
58100     ihDirFileMask$='Readings.*'
58120   else if addmethod=from_holding_file then
58140     book_or_holding_file$='Holding File'
58160     ihDirFileMask$='IPHold*.h[cno]'
58180   else
58200     pr bell;'addmethod not recognized by INPUT_HAND routine.' : goto IH_XIT
58220   end if
58240   INPUT_HAND: !
58260   fnTos(sn$="ubipchg-inh")
58280   txt$="Select "&book_or_holding_file$&" for Input:"
58300   mylen=len(txt$)+1 : mypos=mylen+2
58320   fnLbl(1,1,txt$,mylen,1)
58340   ! r: book or holding file grid
58360   colhdr$(1)=book_or_holding_file$ ! "Book"
58380   colhdr$(2)="Size"
58400   colhdr$(3)="Date"
58420   colhdr$(4)="Time"
58440   mat bookItem$(4)
58460   mat colhdr$(4)
58480   ihFileCount=fngetdir2('[Q]\UBmstr\',mat ihFilename$, '',ihDirFileMask$,mat ihFileDate$,mat ihFileTime$,0,mat ihFileSize)
58500   fnflexinit1("book_"&book_or_holding_file$(1:1),1,mypos,10,32,mat colhdr$,mat cm2$,1)
58520   ! open #ih_file_dir=9: "Name="&ih_file_dir$,display,input
58540   for ihFileItem=1 to ihFileCount
58560     if book_or_holding_file$='Book' then
58580       ! pause
58600       tmpBookNumber=val(ihFilename$(ihFileItem)(10:len(ihFilename$(ihFileItem)))) conv ihInvalidFile
58620       bookItem$(1)=str$(tmpBookNumber)
58640       bookItem$(2)=cnvrt$("pic(zzz,zzz,zzz,zzz)",ihFileSize(ihFileItem))
58660       bookItem$(3)=ihFileDate$(ihFileItem)
58680       bookItem$(4)=ihFileTime$(ihFileItem)
58700       fnflexadd1(mat bookItem$)
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
60000   ! IH_FILE_DIR_EOF: !
60020   ! close #ih_file_dir: ioerr IH_XIT ! /r
60040   fnLbl(11,1," ",15,1)
60060   fnCmdKey("&Next",1,1)
60080   fnCmdKey("&Delete",4)
60100   fnCmdKey("&Print",6)
60120   fnCmdKey("&Cancel",5,0,1)
60140   fnAcs(sn$,0,mat resp$,ck)
60160   holdingFile$=""
60180   ip1$=resp$(1)
60300   ! listonly=0
60320   ! if ck=6 then listonly=1: ck=1
60340   if ck=cancel or ip1$='' then
60360     goto IH_XIT
60380   else if ck=6 then
60400     if book_or_holding_file$='Holding File' then
60404       open #hpHoldingFile:=fngethandle: "Name=[Q]\UBmstr\IpHold"&ip1$&".h[cno]",internal,outIn,relative
60406       fn_print_readings(hpHoldingFile, 'Holding File '&ip1$)
60408       close #hpHoldingFile:
60420       ! fn_holdingFilePrint(ip1$) ! pr for Holding Files
60440     else
60460       fn_hh_readings(ip1$, 1) ! pr for Books
60480     end if
60490     goto INPUT_HAND
60500   else if ck=4 then
60520     mat txt$(1)
60540     txt$(1)="Are you sure you wish to delete "&book_or_holding_file$&" "&ip1$&"?"
60560     fnmsgbox(mat txt$,resp$,'',36)
60580     if resp$="Yes" then
60600       if addmethod=from_hh_file then
60620         fnFree("[Q]\UBmstr\Readings."&ip1$)
60640       else if addmethod=from_holding_file then
60660         fnFree("[Q]\UBmstr\IPHold"&ip1$&".h[cno]")
60680       end if
60700     end if
60720     goto INPUT_HAND
61000   else if addmethod=from_holding_file then
61020     fn_holdingFileLoad
61040   else
61060     fn_hh_readings(ip1$)
61080   end if
61100   IH_XIT: !
61120 fnend
70000 ENTER_READING: ! r:
70010   if alp$="*" then goto READ_ROUTE_SEQUENCE
70020   ENTER_READING2: !
70030   fn_us1
70040   ENTER_READING3: !
70050   fnTos(sn$="enter_reading")
70060   rc=0 : frac=0
70070   !
70080   fnFra(1,1,3,39,"Account Data")
70090   mylen=15 : mypos=mylen+2 : fraad=frac+=1
70100   fnLbl(1,1,"Account:",mylen,1,0,fraad)
70110   fnTxt(1,mypos,10,0,0,empty$,1,empty$,1)
70120   resp$(rc+=1)=x$
70130   fnLbl(2,1,"Name:",mylen,1,0,fraad)
70140   fnTxt(2,mypos,30,30,0,empty$,1,empty$,fraad)
70150   resp$(rc+=1)=aname$
70160   fnLbl(3,1,"Meter Address:",mylen,1,0,fraad)
70170   fnTxt(3,mypos,10,0,0,empty$,1,empty$,fraad)
70180   resp$(rc+=1)=e2$
70190   !
70200   fnFra(7,1,12,60,"Readings & Overrides")
70210   mylen=0 : for j=1 to 8 : mylen=max(mylen,len(srvnam$(j))) : next j
70220   mypos1=mylen+2 : mypos2=mypos1+12
70230   mypos3=mypos2+12 : mypos4=mypos3+12 : mypos5=mypos4+12+4
70240   lc=0 : fraro=frac+=1
70250   fnLbl(lc+=1,mypos1,"Reading",10,2,0,fraro)
70260   fnLbl(lc,mypos2,"Charge",10,2,0,fraro)
70270   fnLbl(lc,mypos3,"Usage",10,2,0,fraro)
70280   if srvnam$(3)="Electric" then
70290     fnLbl(lc,mypos4,"Demand",10,2,0,fraro)
70300   end if
70310   ! r: Service 1
70312   tmpService=1
70320   first_read_rc=rc
70330   if a(1)=0 and a(2)=0 then disa=1 else disa=0 ! water and sewer rate codes
70332   if onlyMonth(tmpService)>0 and onlyMonth(tmpService)<>date(days(d1,'mmddyy'),'mm') then disa=1
70340   ! water
70350   if service_enabled(tmpService) then
70360     lc+=1
70370     fnLbl(lc,1,srvnamc$(1),mylen,1,0,2)
70380     fnTxt(lc,mypos1,10,11,1,"20",disa,empty$,fraro) ! reading
70390     fnTxt(lc,mypos2,10,10,1,"10",disa,empty$,fraro) ! charge
70400     fnTxt(lc,mypos3,10,11,1,"20",disa,empty$,fraro) ! usage
70410     if editmode=1 then
70420       resp$(rc+=1)=str$(x(tmpService)) ! water reading
70430       resp$(rc+=1)=str$(x(09)) ! water charge
70440       resp$(rc+=1)=str$(x(12)) ! water used
70450     else
70460       resp$(rc+=1)=''
70470       resp$(rc+=1)=''
70480       resp$(rc+=1)=''
70490     end if
70500   end if ! /r
70510   ! r: Service 2 - Sewer
70512   tmpService=2
70520   if a(tmpService)=0 then disa=1 else disa=0 ! sewer rate code
70522   if onlyMonth(tmpService)>0 and onlyMonth(tmpService)<>date(days(d1,'mmddyy'),'mm') then disa=1
70540   if service_enabled(tmpService) then
70550     fnLbl(lc+=1,1,srvnamc$(tmpService),mylen,1,0,2)
70560     fnTxt(lc,mypos2,10,0,1,"10",disa,empty$,fraro) ! charge
70570     if editmode=1 then
70580       resp$(rc+=1)=str$(x(05)) ! sewer charge
70590     else
70600       resp$(rc+=1)=''
70610     end if
70620   end if ! /r
70630   ! r: Service 3 - Electric
70632   tmpService=3
70640   if a(tmpService)=0 then disa=1 else disa=0 ! electric rate code
70642   if onlyMonth(tmpService)>0 and onlyMonth(tmpService)<>date(days(d1,'mmddyy'),'mm') then disa=1
70650   !
70660   if service_type(tmpService)=3 then
70670     fnLbl(lc+=1,1,srvnamc$(tmpService),mylen,1,0,2)
70680     fnTxt(lc,mypos1,10,11,1,"20",disa,empty$,fraro) ! reading
70690     fnTxt(lc,mypos2,10,10,1,"10",disa,empty$,fraro) ! charge
70700     fnTxt(lc,mypos3,10,11,1,"20",disa,empty$,fraro) ! usage
70710     fnTxt(lc,mypos4,10,11,1,"20",disa,empty$,fraro) ! demand
70720     if editmode=1 then
70730       resp$(rc+=1)=str$(x(tmpService)) ! electric reading
70740       resp$(rc+=1)=str$(x(10)) ! electric charge
70750       resp$(rc+=1)=str$(x(13)) ! electric usage
70760       resp$(rc+=1)=str$(x(04)) ! electric demand
70770     else
70780       resp$(rc+=1)=""
70790       resp$(rc+=1)=""
70800       resp$(rc+=1)=""
70810       resp$(rc+=1)=""
70820     end if
70830   else if service_type(tmpService)=3.1 then
70840     fnLbl(lc+=1,1,srvnamc$(tmpService),mylen,1,0,2)
70850     fnTxt(lc,mypos1,10,11,1,"20",disa,empty$,fraro) ! reading
70860     fnTxt(lc,mypos2,10,10,1,"10",disa,empty$,fraro) ! charge
70870     fnTxt(lc,mypos3,10,11,1,"20",disa,empty$,fraro) ! usage
70880     if editmode=1 then
70890       resp$(rc+=1)=str$(x(03)) ! electric reading
70900       resp$(rc+=1)=str$(x(10)) ! electric charge
70910       resp$(rc+=1)=str$(x(13)) ! electric usage
70920     else
70930       resp$(rc+=1)=""
70940       resp$(rc+=1)=""
70950       resp$(rc+=1)=""
70960     end if
70970   else if service_type(tmpService)=3.2 then
70980     if a(1)=0 and a(2)=0 then disa=1 else disa=0 ! water rate code
70982     if onlyMonth(tmpService)>0 and onlyMonth(tmpService)<>date(days(d1,'mmddyy'),'mm') then disa=1
70990     fnLbl(lc+=1,1,srvnamc$(tmpService),mylen,1,0,2)
71000     fnTxt(lc,mypos3,10,11,1,"20",disa,empty$,fraro) ! usage
71010     if editmode=1 then
71020       resp$(rc+=1)=str$(x(13)) ! Reduction Usage
71030     else
71040       resp$(rc+=1)=""
71050     end if
71060   end if
71070   ! /r
71080   ! r: Service 4 - Gas
71082   tmpService=4
71090   if service_enabled(tmpService) then
71100     if a(tmpService)=0 then disa=1 else disa=0 ! gas rate code
71102     if onlyMonth(tmpService)>0 and onlyMonth(tmpService)<>date(days(d1,'mmddyy'),'mm') then disa=1
71110     lc+=1
71120     fnLbl(lc,1,srvnamc$(tmpService),mylen,1,0,2)
71130     fnTxt(lc,mypos1,10,11,1,"20",disa,empty$,fraro) ! reading
71140     fnTxt(lc,mypos2,10,10,1,"10",disa,empty$,fraro) ! charge
71150     fnTxt(lc,mypos3,10,11,1,"20",disa,empty$,fraro) ! usage
71160     if editmode=1 then
71170       resp$(rc+=1)=str$(x(02))
71180       resp$(rc+=1)=str$(x(11))
71190       resp$(rc+=1)=str$(x(14))
71200     else
71210       resp$(rc+=1)='' ! gas reading
71220       resp$(rc+=1)='' ! gas charge
71230       resp$(rc+=1)='' ! gas usage
71240     end if
71250   end if
71260   ! /r
71270   ! r: service 5
71272   tmpService=5
71280   if service_enabled(tmpService) then
71290     if a(tmpService)=0 then disa=1 else disa=0 ! service 5 rate code
71292     if onlyMonth(tmpService)>0 and onlyMonth(tmpService)<>date(days(d1,'mmddyy'),'mm') then disa=1
71300     if trim$(srvnam$(tmpService))="Reconnect Fee" then disa=0
71310     fnLbl(lc+=1,1,srvnamc$(tmpService),mylen,1,0,fraro)
71320     fnTxt(lc,mypos2,10,0,1,"10",disa,empty$,fraro)
71330     if editmode=1 then
71340       resp$(rc+=1)=str$(x(tmpService)) ! Service 5 charge
71350     else
71360       resp$(rc+=1)=''
71370     end if
71380   end if
71390   ! /r
71400   ! r: service 6
71402   tmpService=6
71410   if service_enabled(6) then
71420     if service_type(6)=5 or service_type(6)=7 or service_type(6)=8 then ! service 6 rate code
71430       disa=0
71440     else if extra(11)=0 then
71450       disa=1
71460     else
71470       disa=0
71480     end if
71490     fnLbl(lc+=1,1,srvnamc$(6),mylen,1,0,fraro)
71500     fnTxt(lc,mypos2,10,0,1,"10",disa,empty$,fraro) ! charge
71510     if editmode=1 then ! Service 6 charge
71520       resp$(rc+=1)=str$(x(tmpService))
71530     else
71540       resp$(rc+=1)=''
71550     end if
71560   end if
71570   ! /r
71580   ! r: Service 7
71582   tmpService=7
71590   if service_enabled(tmpService) then
71600     if service_type(tmpService)=5 then
71610       disa=0 ! don't disable other charge
71620     else if extra(12)=0 then
71630       disa=1
71640     else
71650       disa=0 ! service 7 rate code
71660     end if
71662     if onlyMonth(tmpService)>0 and onlyMonth(tmpService)<>date(days(d1,'mmddyy'),'mm') then disa=1
71670     fnLbl(lc+=1,1,srvnamc$(tmpService),mylen,1,0,2)
71690     fnTxt(lc,mypos2,10,0,1,"10",disa,empty$,2) ! charge
71700     if editmode=1 then ! Service 7 charge
71710       resp$(rc+=1)=str$(x(tmpService))
71720     else
71730       resp$(rc+=1)=''
71740     end if
71750   end if
71760   ! /r
71770   ! r: service 8
71772   tmpService=8
71780   if service_enabled(tmpService) then
71790     if service_type(tmpService)=5 or service_type(tmpService)=6 then ! don't disable other charge nor gas connect
71800       disa=0
71810     else if extra(13)=0 then
71820       disa=1
71830     else
71840       disa=0 ! service 8 rate code
71850     end if
71852     if onlyMonth(tmpService)>0 and onlyMonth(tmpService)<>date(days(d1,'mmddyy'),'mm') then disa=1
71860     lc+=1
71870     fnLbl(lc,1,srvnamc$(tmpService),mylen,1,0,2)
71880     fnTxt(lc,mypos2,10,0,1,"10",disa,empty$,fraro) ! charge
71890     if editmode=1 then
71900       resp$(rc+=1)=str$(x(tmpService)) ! Service 8 charge
71910     else
71920       resp$(rc+=1)=''
71930     end if
71940   end if
71950   ! /r
71960   !
71970   lc=lc+2
71980   fnLbl(lc,1,"Final Billing Code:",mylen+8,1,0,2)
71990   fncomboa("finalbill",lc,24,mat opt_final_billing$,"Used to record final billing code in customer record",28,fraro) ! final billing code
72000   resp_fianl_billing_code=(rc+=1)
72010   if editmode=1 then
72020     resp$(resp_fianl_billing_code)=str$(x(15)) ! Final Billing Code
72030   else
74000     resp$(resp_fianl_billing_code)=opt_final_billing$(final+1)
74020   end if
74040   if editmode=1 and x(15)=1 then resp$(resp_fianl_billing_code)=opt_final_billing$(2)
74060   if editmode=1 and x(15)=2 then resp$(resp_fianl_billing_code)=opt_final_billing$(3)
74080   begdate=fndate_mmddyy_to_ccyymmdd(d1)-20000
74100   fn_flexRead(1,mypos5+2,hTrans,x$,begdate,0,fraro) ! beginning date=billing date less one year
74120   fnCmdKey("&Meter Change",9,0,0,"Calculates usage on meter change out.")
74140   fnCmdKey("&Review Customer Record",8,0,0,"Allow you to review any customer while entering readings.")
74180   if addmethod=1 or addmethod=from_hh_file then let fnCmdSet(17) else let fnCmdSet(11) ! kj   3/24/06
74200   fnAcs(sn$,0,mat resp$,ck)
74220   if ck=8 then
74240     fncustomer(x): read #hCustomer1,using F_CUSTOMER_C,key=x$,release: x$,aname$,mat a,final,mat d,alp$,mat extra,extra$(3)
74242     fnapply_default_rates(mat extra, mat a)
74260     goto ENTER_READING3
74280   end if
74300   rc=first_read_rc
74320   ! If PASSCHECK=CKFAIL Then eDITMODE=0 ! xxx Ken
74340   if ck=3 then done_with_readings=1 ! code as done with entering readings is select finish
74360   if service_enabled(1) then ! Service 1 - Water
74380     x(01)=val(resp$(rc+=1))
74400     x(09)=val(resp$(rc+=1))
74420     x(12)=val(resp$(rc+=1))
74440   end if
74460   if service_enabled(2) then ! Service 2 - Sewer
74480     x(05)=val(resp$(rc+=1))
74500   end if
74520   if service_type(3)=3 then ! electric
74540     x(03)=val(resp$(rc+=1))
74560     x(10)=val(resp$(rc+=1))
74580     x(13)=val(resp$(rc+=1))
74600     x(04)=val(resp$(rc+=1)) ! electric/lawn meter
74620   else if service_type(3)=3.1 then ! lawn meter
74640     x(03)=val(resp$(rc+=1))
74660     x(10)=val(resp$(rc+=1))
74680     x(13)=val(resp$(rc+=1))
74700   else if service_type(3)=3.2 then
74720     x(13)=val(resp$(rc+=1))
74740   end if  ! if srvnam$(3)=...
74760   if service_type(3)=3.1 and x(03)=0 and d(5)>0 and a(3)>0 then
74780     x(03)=d(5) ! if they skip reading the lawn meters, just write the previous reading into the current reading
74800   end if
74820   if service_enabled(4) then
74840     x(02)=val(resp$(rc+=1))
74860     x(11)=val(resp$(rc+=1))
74880     x(14)=val(resp$(rc+=1)) ! gas
74900   end if
74920   !
74940   if service_enabled(5)=1 then ! service 5
74960     x(06)=val(resp$(rc+=1))
74980   end if
75000   !
75020   if service_enabled(6)=1 then ! service 6
75040     x(07)=val(resp$(rc+=1))
75060   end if
75080   !
75100   if service_enabled(7) then ! service 7
75120     x(07)=val(resp$(rc+=1))
75140   end if
75160   !
75180   if service_enabled(8) then ! service 8
75200     x(08)=val(resp$(rc+=1))
75220   end if
75240   ! pause
75260   rc+=1
75280   x(15)=val(resp$(resp_fianl_billing_code)(1:1)) ! final billing code
75300   if ck=2 and addmethod=from_hh_file then
75320     skiprec=1
75340     goto L2910 ! if choose skip on pulling from hh file, then skip writing the record   ! kj 3/24/06
75360   else if addmethod=from_holding_file then
75380     goto CHECK_UNUSUAL
75400   else if ck=2 and editmode=0 then
75420     goto SEL_ACC
75440   else if ck=2 and editmode=1 then
75460     goto MENU1
75480   else if ck=3 or ck=cancel then
75500     addmethod=0
75520     goto MENU1
75540   else if ck=9 then
75560     if fn_meter_change_out=3 then goto ENTER_READING3
75580     goto MENU1
75600   end if
75620   CHECK_UNUSUAL: !
75640   if addmethod<>3 then mat mroll=(0)
75660   passcheck=ckpass=0 : ckfail=1 : ckcancel=2
75670   !
75680   fn_checkwater
75700   if passcheck=ckfail then
75720     editmode=1
75740     goto ENTER_READING3
75760   else if passcheck=ckcancel then
75780     goto ERXIT
75800   end if
75810   !
75820   fn_checkgas
75840   if passcheck=ckfail then
75860     editmode=1
75880     goto ENTER_READING3
75900   else if passcheck=ckcancel then
75920     editmode=1
75940     goto ENTER_READING3 ! Then Goto ERXIT
75960   end if
75970   !
75980   fn_checkelec
76000   if passcheck=ckfail then
76020     editmode=1
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
76260   if addmethod=1 and done_with_readings=0 then editmode=0 ! set editmode back after any corrections during the addmethod 1
76280   if addmethod=1 and editmode=0 then goto READ_ROUTE_SEQUENCE
76300   if addmethod=1 and editmode=1 then goto MENU1
76320   ! If ADDMETHOD=1 AND EDITMODE=1 Then Goto READ_ROUTE_SEQUENCE ! MENU1
76340   if addmethod=2 and (editmode=0 or editmode=1) then mat x=(0): goto SEL_ACC ! kj 92407
76360   ! If ADDMETHOD=2 AND EDITMODE=1 Then Goto MENU1 ! kj 92407
76380 goto MENU1 ! /r
80000 def fn_setupFlexRead
80020   if ~setupFlexRead then
80040     setupFlexRead=1
80080     dim colmask$(30),frColHdr$(30)*20,serviceName$(10)*20,item$(25)*70
80100     dim tg(11),a(7)
80120     fnget_services(mat serviceName$)
80140     tcode$(1)="Charge"
80160     tcode$(2)="Penalty"
80180     tcode$(3)="Collect"
80200     tcode$(4)="C/M"
80220     tcode$(5)="D/M"
80240   end if
80260 fnend
82000 def fn_flexRead(myline,mypos,filnum,z$,begdate,enddate,selcode) ! library ready
82020   if ~setupFlexRead then let fn_setupFlexRead
82040   z$=trim$(z$)
82060   if z$<>'' then
82080     open #tmp=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed
82100     z$=lpad$(trim$(z$),10)
82120     read #tmp,using "Form Pos 143,7*pd 2",key=z$: mat a
82140     close #tmp:
82160   end if
82180   mat frColHdr$(30) : mat colmask$(30)
82200   frColHdr$(headers=1)="Date" : colmask$(headers)="3"
82220   if trim$(serviceName$(1))<>"" and (z$<>'' and a(1)>0) then
82240     frColHdr$(headers+=1)="Water Reading" : colmask$(headers)="20"
82260     frColHdr$(headers+=1)="Water Usage" : colmask$(headers)="20"
82280   end if
82300   if trim$(serviceName$(3))="Electric" and (z$<>'' and a(3)>0) then
82320     frColHdr$(headers+=1)="Electric Reading" : colmask$(headers)="20"
82340     frColHdr$(headers+=1)="Electric Usage" : colmask$(headers)="20"
82360   end if
82380   if trim$(serviceName$(3))="Lawn Meter" and (z$<>'' and a(3)>0) then
82400     frColHdr$(headers+=1)="Lawn Meter Reading" : colmask$(headers)="20"
82420     frColHdr$(headers+=1)="Lawn Meter Usage" : colmask$(headers)="20"
82440   end if
82460   if trim$(serviceName$(4))="Gas" and (z$<>'' and a(4)>0) then
82480     frColHdr$(headers+=1)="Gas Reading" : colmask$(headers)="20"
82500     frColHdr$(headers+=1)="Gas Usage" : colmask$(headers)="20"
82520   end if
82540   mat frColHdr$(headers)
82560   mat colmask$(headers)
82580   fnflexinit1("ubread",myline,mypos,13,30,mat frColHdr$,mat colmask$,1)
82600   items=0
82620   mat item$=('')
82640   restore #filnum,key>=rpad$(z$,kln(filnum)): nokey NO_RECORDS_FOUND
82660   do
82680     FlexReadCustomerRead: !
82700     read #filnum,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1',release: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof FlexReadXit
82720     if p$<>z$ then goto FlexReadXit ! not same account
82740     if (selcode>1 and tcode<>selcode-1) or (begdate>0 and tdate<begdate) or (enddate>0 and tdate>enddate) then goto FlexReadCustomerRead
82760     if tcode=0 then tcode=1 ! temporary to prevent bad transaction codes
82780     item$(1)=str$(tdate)
82800     items=1
82820     if trim$(serviceName$(1))<>"" and (z$<>'' and a(1)>0) then
82840       item$(items+=1)=str$(wr)
82860       item$(items+=1)=str$(wu)
82880     end if
82900     if trim$(serviceName$(3))="Electric" and (z$<>'' and a(3)>0) then
82920       item$(items+=1)=str$(er)
82940       item$(items+=1)=str$(eu)
82960     end if
82980     if trim$(serviceName$(3))="Lawn Meter" and (z$<>'' and a(3)>0) then
83000       item$(items+=1)=str$(er)
83020       item$(items+=1)=str$(eu)
83040     end if
83060     if trim$(serviceName$(4))<>"" and (z$<>'' and a(4)>0) then
83080       item$(items+=1)=str$(gr)
83100       item$(items+=1)=str$(gu)
83120     end if
83140     fnflexadd1(mat item$)
83160   loop
83180   ! ______________________________________________________________________
83200   NO_RECORDS_FOUND: !
83220     if items=0 then mat item$=("")
83240     fnflexadd1(mat item$)
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
86200     rctr=lrec(hWork)+1
86220     write #hWork,using F_WORK,rec=rctr: trim$(x$),mat x duprec ww_writeWorkIncriment
86240     F_WORK: form pos 1,cr 10,4*pd 5,7*pd 4.2,3*pd 5,n 1
86260   end if
86280 fnend
88000 def fn_meter_change_out
88020   mco_return=0
88040   do
88060     fnTos(sn$="Method")
88080     rc=0 : lc=0
88100     fnFra(1,1,2,49,"Method of Change Out")
88120     fnOpt(1,1,"Current customer only",0,1)
88140     resp$(1)="True"
88160     fnOpt(2,1,"All Customers",0,1)
88180     resp$(2)="False"
88200     fnLbl(5,1,"Service Type:",18,1)
88220     fncomboa("ServiceType",5,20,mat serviceoption$)
88240     resp$(3)=serviceoption$(1)
88260     fnLbl(6,1,"Beginning Customer:",18,1)
88280     fncmbact(6,20,1)
88300     resp$(4)="[All]"
88320     fnCmdKey("&Next",1,1,0): fnCmdKey("&Cancel",5,0,1)
88340     fnAcs(sn$,0,mat resp$,ckey)
88360     if ckey=5 then goto Mco_Xit
88380     servicetype$=resp$(3)(1:2)
88400     begx$=resp$(4)(1:10)
88420     if resp$(2)="True" then method$="File" : goto MCO_UPDATE_FULL_FILE ! working from a file
88440     if resp$(1)="True" then method$="Customer" : goto MCO_RECORD_READINGS
88460   loop
88480   MCO_RECORD_READINGS: !
88500   fnTos(sn$="Meter_Change")
88520   rc=0 : lc=0: resprc=0
88540   fnLbl(lc+=1,1,x$&"  "&aname$,50,0)
88560   fnLbl(lc+=2,32,"Old Meter",10,2)
88580   fnLbl(lc,55,"New Meter",10,2)
88600   fnLbl(lc+=1,25,"Prior",10,2)
88620   fnLbl(lc,37,"Current",10,2)
88640   fnLbl(lc,49,"Prior",10,2)
88660   fnLbl(lc,61,"Current",10,2)
88680   if trim$(servicetype$)="WA" then
88700     fnLbl(lc+=1,1,srvnamc$(1),20,1)
88720     fnTxt(lc,25,10,11,1,"30",0,"Enter the prior reading on the old meter")
88740     resp$(resprc+=1)=str$(d(1))
88760     fnTxt(lc,37,10,10,1,"30",0,"Enter the current reading on the old meter.")
88780     resp$(resprc+=1)=""
88800     fnTxt(lc,49,10,11,1,"30",0,"Enter the beginning reading the new meter")
88820     resp$(resprc+=1)=""
88840     fnTxt(lc,61,10,11,1,"30",0,"Enter the ending reading on new meter")
88860     resp$(resprc+=1)=str$(x(1))
88880   else if trim$(servicetype$)="EL" then
88900     fnLbl(lc+=1,1,srvnamc$(3),20,1)
88920     fnTxt(lc,25,10,11,1,"30",0,"Enter the prior reading on the old meter")
88940     resp$(resprc+=1)=str$(d(5))
88960     fnTxt(lc,37,10,10,1,"30",0,"Enter the current reading on the old meter.")
88980     resp$(resprc+=1)=""
89000     fnTxt(lc,49,10,11,1,"30",0,"Enter the beginning reading the new meter")
89020     resp$(resprc+=1)=""
89040     fnTxt(lc,61,10,11,1,"30",0,"Enter the ending reading on new meter")
89060     resp$(resprc+=1)=str$(x(3))
89080   else if trim$(servicetype$)="GA" then
89100     fnLbl(lc+=1,1,srvnamc$(4),20,1)
89120     fnTxt(lc,25,10,11,1,"30",0,"Enter the prior reading on the old meter")
89140     resp$(resprc+=1)=str$(d(9))
89160     fnTxt(lc,37,10,10,1,"30",0,"Enter the current reading on the old meter.")
89180     resp$(resprc+=1)=""
89200     fnTxt(lc,49,10,11,1,"30",0,"Enter the beginning reading the new meter")
89220     resp$(resprc+=1)=""
89240     fnTxt(lc,61,10,11,1,"30",0,"Enter the ending reading on new meter")
89260     resp$(resprc+=1)=str$(x(2))
89280   end if
89300   fnCmdKey("&Next",1,1,0): fnCmdKey("&Cancel",5,0,1)
89320   fnCmdKey("&Finish",10)
89340   fnAcs(sn$,0,mat resp$,ckey)
89360   if ckey=5 then goto Mco_Xit
89380   if ckey=10 then goto Mco_Xit
89400   oldmeterprior=val(resp$(1))
89420   oldmetercurrent=val(resp$(2))
89440   newmeterprior=val(resp$(3))
89460   newmetercurrent=val(resp$(4))
89480   usage=oldmetercurrent-oldmeterprior+newmetercurrent-newmeterprior
89500   if usage<0 then
89520     mat txt$(3)
89540     txt$(1)="The readings you entered create a negative uuage."
89560     txt$(2)="Correct one of the readings or choose Cancel to"
89580     txt$(3)="skip this record!"
89600     fnmsgbox(mat txt$,resp$,'',1)
89620     if resp$="OK" then goto MCO_RECORD_READINGS
89640   end if
89660   if method$="File" then let fn_rewrite_usage : goto MCO_WORK_READ ! read new record from readings file
89680   if method$="Customer" and servicetype$="WA" then x(1)=newmetercurrent: x(12)=usage
89700   if method$="Customer" and servicetype$="GA" then x(2)=newmetercurrent: x(14)=usage
89720   if method$="Customer" and servicetype$="EL" then x(3)=newmetercurrent: x(13)=usage
89740   if method$="Customer" then passcheck=ckfail: editmode=1 : goto mco_ENTER_READING3
89760   ! goto somewhere
89780   MCO_UPDATE_FULL_FILE: ! meter change over - update full file
89800   close #hWork: ioerr ignore
89820   open #hWork: "Name="&workFile$,internal,outIn,relative
89840   if lrec(hWork)=0 then goto MCO_L9290
89860   MCO_WORK_READ: !
89880   read #hWork,using F_WORK: x$,mat x eof MCO_L9350
89900   MCO_L9290: !
89920   if trim$(begx$)="" or trim$(begx$)="[All]" then begx$="" : goto MCO_L9320
89940   if trim$(begx$)<>trim$(x$) then goto MCO_WORK_READ
89960   begx$=""
89980   MCO_L9320: !
90000   read #hCustomer1,using MCO_F_CUSTOMER,key=x$,release: aname$, mat d nokey MCO_WORK_READ
90020   MCO_F_CUSTOMER: form pos 41,c 20,pos 217,15*pd 5
90040   goto MCO_RECORD_READINGS
90060   MCO_L9350: !
90080   close #hWork: ioerr ignore
90100   open #hWork: "Name="&workFile$&",KFName="&workFileIndex$,internal,outIn,keyed
90120   goto Mco_Xit
90140   mco_ENTER_READING3: !
90160   mco_return=3
90180   Mco_Xit: !
90200   fn_meter_change_out=mco_return
90220 fnend

92000 def fn_hh_other_type2(listonly)
92020   dim hot_ver$*512,hot_line$*512
92040   dim hotImportDataField$(0)*256
92060   dim hotImportDataValue$(0)*256
92080   hotDataImportAsked=0
92100   open #h_readings:=13: "Name=[Q]\UBmstr\Readings."&ip1$,display,input
92120   linput #h_readings: hot_ver$
92140   hot_ver$=trim$(hot_ver$)
92160   hot_z_prior$=hot_z$=''
92180   if hot_ver$='[ACS Hand Held File Generic Version 2]' then
92200     if listonly=1 then let fnopenprn
92220     do
92240       hotWaterMeterChangeBefore=hotWaterMeterChangeAfter=0
92260       mat hotImportDataField$(0)
92280       mat hotImportDataValue$(0)
92300       mat x=(0)
92320       do
92340         hot_z_prior$=hot_z$
92360         linput #h_readings: hot_line$ eof HOT_EOF
92380         if trim$(srep$(hot_line$,'=',''))<>'' and trim$(hot_line$)(1:1)<>'!' then
92400           ! pr 'before: '&hot_line$ : pause
92420           fn_hot_parse_line(hot_line$,hot_z$,mat x,mat hotImportDataField$,mat hotImportDataValue$,hotWaterMeterChangeBefore,hotWaterMeterChangeAfter)
92440           ! pr 'after ' : pause
92460        end if
92480       loop until hot_z$<>hot_z_prior$ and hot_z_prior$<>''
92481       ! if trim$(hot_z_prior$)='100050.05' then debug=1 else debug=0
92482       ! if debug then 
92490       !   pr 'after loop'
92500       !   pr 'Customer.Number=';hot_z_prior$;'         hot_z_prior$=';hot_z_prior$
92505       !   pr 'Reading.Water=';x(1);'  Usage=';x(12)
92510       !   pr 'MeterAddress.LocationID=';hotLocationID
92560       !   pr 'hotWaterMeterChangeBefore=';hotWaterMeterChangeBefore;'  hotWaterMeterChangeAfter=';hotWaterMeterChangeAfter
92580       !   for x=1 to udim(mat hotImportDataField$)
92600       !     pr hotImportDataField$(x)&'='&hotImportDataValue$(x)
92620       !   nex x
92630       !   pr ''
92640       !   pause
92642       ! end if
92660       fn_hot_calcMeterChangeOut(hot_z_prior$,mat x,hotWaterMeterChangeBefore,hotWaterMeterChangeAfter)
92680       if listonly=1 then
92700         fn_lo_pr_rec(hot_z_prior$,mat x)
92720       else
92740         fn_hot_write_work(hWork,hot_z_prior$,mat x,hotDataImportAsked,hotDataImportEnabled,mat hotImportDataField$,mat hotImportDataValue$)
92760       end if
92780       hot_z_prior$=hot_z$
92800     loop
92820     HOT_EOF: !
92840     fn_hot_calcMeterChangeOut(hot_z$,mat x,hotWaterMeterChangeBefore,hotWaterMeterChangeAfter)
92860     if listonly=1 then
92880       fn_lo_pr_rec(hot_z$,mat x)
92900     else
92920       fn_hot_write_work(hWork,hot_z$,mat x,hotDataImportAsked,hotDataImportEnabled,mat hotImportDataField$,mat hotImportDataValue$)
92940     end if  ! hot_ver$='[ACS Hand Held File Generic Version 2]'
92960     if listonly=1 then let fncloseprn
92980   end if  ! hot_ver$='[ACS Hand Held File Generic Version 2]'
93000 fnend
94000 def fn_hot_parse_line(line$*512,&hot_z$,mat x,mat importDataField$,mat importDataValue$,&hotWaterMeterChangeBefore,&hotWaterMeterChangeAfter)
94020   ! sets any one of the following local variables each call:
94040   ! hot_z$, mat x
94060   pos_equal=pos(line$,'=')
94080   dim hpField$*256
94100   dim hpValue$*256
94120   hpField$=line$(1:pos_equal-1)
94140   hpValue$=line$(pos_equal+1:len(line$))
94150   hpValue$=trim$(hpValue$,'"')
94160   hpValueN=0
94180   hpValueN=val(hpValue$) conv ignore
94200   hpField$=lwrc$(trim$(hpField$))
94220   str2mat(hpField$,mat lfItem$,'.')
94230   if lfItem$(1)='source file' or lfItem$(1)(1:1)='!' then 
94232     ! do nothing     goto hpFinis
94234   else
94240     if lfItem$(2)='kwh' then lfItem$(2)="electric"
94260     if lfItem$(1)="customer" then
94280       if lfItem$(2)="number" then
94300         hot_z$=lpad$(trim$(hpValue$),10)
94320       end if
94340     else if lfItem$(1)="meterchangeout" then
94360       if lfItem$(2)='readingbefore' then
94380         if lfItem$(3)='water' then
94400           hotWaterMeterChangeBefore=hpValueN
94420         else
94440           pr 'encountered '&hpField$&' - add code to process it' : pause
94460         end if
94480       else if lfItem$(2)='readingafter' then
94500         if lfItem$(3)='water' then
94520           hotWaterMeterChangeAfter=hpValueN
94540         else
94560           pr 'encountered '&hpField$&' - add code to process it' : pause
94580         end if
94600       end if
94620     else if lfItem$(1)="reading" then
94640       if lfItem$(2)="water" then
94660         x(1)=hpValueN
94680       else if lfItem$(2)="gas" then
94700         x(2)=hpValueN
94720       else if lfItem$(2)="electric" then
94740         x(3)=hpValueN
94760       else if lfItem$(2)="demand" then
94780         x(4)=hpValueN
94800       end if
94820     else if lfItem$(1)="charge" then
94840       if lfItem$(2)="sewer" then
94860         x(5)=hpValueN
94880       else if lfItem$(2)="sanitation" then
94900         x(6)=hpValueN
94920       else if lfItem$(2)="fire protection" then
94940         x(7)=hpValueN
94960       else if lfItem$(2)="other" then
94980         x(8)=hpValueN
95000       else if lfItem$(2)="water" then
95020         x(9)=hpValueN
95040       else if lfItem$(2)="electric" then
95060         x(10)=hpValueN
95080       else if lfItem$(2)="gas" then
95100         x(11)=hpValueN
95120       end if
95140     else if lfItem$(1)="used" or lfItem$(1)="usage" then
95160       if lfItem$(2)="water" then
95180         x(12)=hpValueN
95200       else if lfItem$(2)="kwh" then
95220         x(13)=hpValueN
95240       else if lfItem$(2)="gas" then
95260         x(14)=hpValueN
95280       end if
95300     else if lfItem$(1)="final billing code" then
95320       x(15)=hpValueN
95340     else if lfItem$(1)="meter" then
95360       if lfItem$(2)="tamper" then
95380         fn_write_tamper(hot_z$,hpValueN)
95400       else if lfItem$(2)="meter number" or lfItem$(2)="transmitter" or lfItem$(2)="longitude" or lfItem$(2)="latitude" then
95420         if lfItem$(3)='water' then
95440           fnAddOneC(mat hotImportDataField$,hpField$)
95460           fnAddOneC(mat hotImportDataValue$,hpValue$)
95480         else
95500           goto HpDidNotCodeItYet
95520         end if
95540       end if
95560     else if lfItem$(1)='meteraddress' and lfItem$(2)='locationid' then
95570       hotLocationID=hpValueN ! really just setting this variable so we can check the value of it later during debugging
95580       hot_zFromLocationID$=fnAccountFromLocationId$(hotLocationID)
95600       if hot_z$<>hot_zFromLocationID$ then
95620         pr 'Customer.Number specified conflicts with Customer.Number derived from Location ID.'
95640         pr '  Customer.Number='&hot_z$
95660         pr '  MeterAddress.LocationID='&hpValue$
95680         pr '  Customer Number derived from Location ID='&hot_zFromLocationID$
95700         hot_z$=hot_zFromLocationID$
95720       end if
95740     else
95760       HpDidNotCodeItYet: !
95780       pr 'code needed for lines like: "'&line$&'"' : pause
95800     end if
95810   end if
95820 fnend
96000 def fn_hot_calcMeterChangeOut(hcmcoAccount$,mat x,hotWaterMeterChangeBefore,hotWaterMeterChangeAfter)
96020   if hotWaterMeterChangeBefore<>0 then
96040     if x(12)>0 then
96060       pr ' meter roll specified but usage was also specified.' : pause
96080     else
96100       read #hcustomer1,using 'form pos 217,pd 5',key=lpad$(trim$(hcmcoaccount$),10),release: hotwaterreadingprior
96120       ! pr ' need to read customers water reading here' : pause
96122       ! usage=oldmetercurrent-oldmeterprior+newmetercurrent-newmeterprior
96124       if debug then 
96126         pr 'hcmcoaccount$='&hcmcoaccount$
96128         pr '  oldMeterCurrent=';hotWaterMeterChangeBefore
96130         pr '  oldMeterCurrent=';hotWaterMeterChangeBefore
96132         pr '  oldMeterPrior  =';hotWaterReadingPrior
96134         pr '  newMeterCurrent=';x(1)
96136         pr '  newMeterPrior  =';hotWaterMeterChangeAfter
96138         pr '  usage=oldmetercurrent-oldmeterprior+newmetercurrent-newmeterprior'
96140         pr '  usage=';hotWaterMeterChangeBefore-hotWaterReadingPrior+x(1)-hotWaterMeterChangeAfter
96142         pause
96144       end if
96160       x(12)=hotWaterMeterChangeBefore-hotWaterReadingPrior+x(1)-hotWaterMeterChangeAfter
96180       ! if  x(12)<0 then 
96200       !   pr 'negative usage ('&str$(x(12))&') calculated on '&hcmcoAccount$
96220       !   pr '  Water MeterChange Before= ';hotWaterMeterChangeBefore
96240       !   pr '  Water MeterChange  After= ';hotWaterMeterChangeAfter
96260       !   pr '       Water Reading Prior= ';hotWaterReadingPrior;' (from customer file)'
96280       !   pr '         Reading from Book= ';x(1)
96300       !   pause
96320       ! end if
96340     end if
96360   end if
96380 fnend
97000 def fn_hot_write_work(hWork,hwwAccount$,mat x,&hotDataImportAsked,&hotDataImportEnabled,mat hotImportDataField$,mat hotImportDataValue$)
97010   if udim(mat hotImportDataField$)>0 then
97020     if ~hotDataImportAsked then ! r: ask if they want to import data (non reading/usage)
97030       mat message$(0)
97040       fnAddOneC(mat message$,'This book contains (non reading/usage) data to import into the customer records.')
97050       fnAddOneC(mat message$,'Import the data now?')
97060       fnAddOneC(mat message$,'')
97070       fnAddOneC(mat message$,'Yes'&chr$(9)&'Updates customer and/or meter records with the new data')
97080       fnAddOneC(mat message$,'No'&chr$(9)&'Only load the readings from this file, ommits import data')
97090       fnmsgbox(mat message$, resp$,'',32+4)
97100       hotDataImportAsked=1
97110       if resp$='Yes' then
97120         hotDataImportEnabled=1
97130       else
97140         hotDataImportEnabled=0
97150       end if
97160     end if ! /r
97170     hwwAccount$=rpad$(trim$(hwwAccount$),10)
97180     if hotDataImportEnabled then ! r: import the data
97190       if ~hLocation then
97192         dim location$(0)*128,locationN(0)
97200         hLocation:=fn_open('U4 Meter Location',mat location$,mat locationN,mat form$, 0,4)
97210       end if
97220       mat location$=('')
97230       mat locationN=(0)
97240       location$(loc_activeCustomer)=hwwAccount$
97250       location$(loc_serviceId)='WA'
97260       locationKey$=fnbuildkey$('U4 Meter Location',mat location$,mat locationN,4)
97270       locationRecordDidChange=0
97280       for hotIdX=1 to udim(mat hotImportDataField$)
97290         if hotImportDataField$(hotIdX)='meter.transmitter.water' then
97300           ! fn_HwwMeterMakeRecIfNone(hMeter,hwwAccount$,'WA')
97310           read #hLocation,using form$(hLocation),key=locationKey$: mat location$,mat locationN
97320           location$(loc_transmitter)=hotImportDataValue$(hotIdX)
97330           rewrite #hLocation,using form$(hLocation),key=locationKey$: mat location$,mat locationN
97340         else if hotImportDataField$(hotIdX)='meter.meter number.water' then
97350           read #hLocation,using form$(hLocation),key=locationKey$: mat location$,mat locationN
97360           location$(loc_transmitter)=hotImportDataValue$(hotIdX)
97370           rewrite #hLocation,using form$(hLocation),key=locationKey$: mat location$,mat locationN
97380         else if hotImportDataField$(hotIdX)='meter.longitude.water' then
97390           read #hLocation,using form$(hLocation),key=locationKey$: mat location$,mat locationN
97400           location$(loc_longitude)=hotImportDataValue$(hotIdX)
97410           rewrite #hLocation,using form$(hLocation),key=locationKey$: mat location$,mat locationN
97420         else if hotImportDataField$(hotIdX)='meter.latitude.water' then
97430           read #hLocation,using form$(hLocation),key=locationKey$: mat location$,mat locationN
97440           location$(loc_latitude)=hotImportDataValue$(hotIdX)
97450           rewrite #hLocation,using form$(hLocation),key=locationKey$: mat location$,mat locationN
97460         else
97470           pr ' add code to update '&hotImportDataField$(hotIdX)
97480           pause
97490         end if
97500       nex hotIdX
97510     end if ! /r
97520   end if
97530   finalBillingCode=val(fnCustomerData$(hwwAccount$,'Final Billing Code',1))
97540   if finalBillingCode and ~hotFinaledImportAsked then ! r: ask if they want to import data (non reading/usage)
97550     mat message$(0)
97560     fnAddOneC(mat message$,'This book contains accounts that are final billed.')
97570     fnAddOneC(mat message$,'Skip loading their readings?')
97580     fnmsgbox(mat message$, resp$,'',32+4)
97590     hotFinaledImportAsked=1
97600     if resp$='Yes' then
97610       hotFinaledImportEnabled=0
97620     else
97630       hotFinaledImportEnabled=1
97640     end if
97650   end if ! /r
97660   if ~finalBillingCode or hotFinaledImportEnabled then
97670     fn_writeWork(hWork,hot_z_prior$,mat x, 1)
97680   end if
97690   if hotDataImportEnabled then
97700     fnCloseFile(hLocation,'U4 Meter Location')
97710     hLocation=0
97720   end if
97730 fnend
99200 ! <updateable region: fn_open (supressprompt:=2)>  
99220 def fn_open(filename$*255, mat f$, mat fn, mat form$; inputonly, keynum, dont_sort_subs, path$*255, mat descr$, mat field_widths,dontupdate,___,index)
99240   dim _fileiosubs$(1)*800, loadedsubs$(1)*32,form$(0)*2048
99260   fn_open=fnOpenFile(filename$, mat f$, mat fn, mat form$, inputonly, keynum, dont_sort_subs, path$, mat descr$, mat field_widths, mat _fileiosubs$,supressprompt:=2)
99280   if ~max(srch(loadedsubs$,uprc$(filename$)),0) then 
99300     mat loadedsubs$(udim(loadedsubs$)+1) 
99320     loadedsubs$(udim(loadedsubs$))=uprc$(filename$)
99340     for index=1 to udim(mat _fileiosubs$) 
99360       execute (_fileiosubs$(index)) 
99380     next index
99400   end if
99420 fnend
99440 ! </updateable region: fnopen>