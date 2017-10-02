10020 ! -- pr Customer Labels
10040 let fn_setup
10100   let fntop(program$)
14000 SCR1: ! r:
14020   let fntos(sn$="ublabel-1b")
14040   let fnlbl(1,1,"Sort by:",20,1)
14060   let fncomboa("ublabels-ord",1,22,mat sequence_option$,"The labels can be printed in customer # order,customer name order, or in bar code sequence")
14080   let fnreg_read('ublabel.sequence',resp$(1)) : if resp$(1)='' then let resp$(1)=sequence_option$(sequence_account)
14100 ! 
14120   let fnlbl(3,1,"Print Address:",20,1)
14140   let fncomboa("ubnamlst-act",3,22,mat address_option$)
14160   let fnreg_read('ublabel.address',resp$(2)) : if srch(mat address_option$,resp$(2))<=0 then let resp$(2)=address_option$(ao_billing)
14280 ! 
14300   let fnlbl(5,1,"Line 1:",20,1)
14320   let fncomboa("ublabel_tl1",5,22,mat line_x_option$,'',70)
14340   let fnreg_read('ublabel.line 1',resp$(3)) : if resp$(3)='' then let resp$(3)=line_x_option$(1)
14360 ! 
14380   let fnlbl(6,1,"Line 2:",20,1)
14400   let fncomboa("ublabel_tl2",6,22,mat line_x_option$,'',70)
14420   let fnreg_read('ublabel.line 2',resp$(4)) : if resp$(4)='' then let resp$(4)=line_x_option$(2)
14440 ! 
14460   let fnlbl(7,1,"Line 3:",20,1)
14480   let fncomboa("ublabel_tl3",7,22,mat line_x_option$,'',70)
14500   let fnreg_read('ublabel.line 3',resp$(5)) : if resp$(5)='' then let resp$(5)=line_x_option$(3)
14520 ! 
14540   let fnlbl(8,1,"Line 4:",20,1)
14560   let fncomboa("ublabel_tl4",8,22,mat line_x_option$,'',70)
14580   let fnreg_read('ublabel.line 4',resp$(6)) : if resp$(6)='' then let resp$(6)=line_x_option$(4)
14600 ! 
14620   let fnlbl(9,1,"Line 5:",20,1)
14640   let fncomboa("ublabel_tl5",9,22,mat line_x_option$,'',70)
14660   let fnreg_read('ublabel.line 5',resp$(7)) : if resp$(7)='' then let resp$(7)=line_x_option$(5 )
14680 ! 
14700   let fncmdset(2)
14720   let fnacs(sn$,0,mat resp$,ck) ! select order of printing
14740   if ck=5 then goto XIT
14760   let fnreg_write('ublabel.sequence',resp$(1))
14780   let fnreg_write('ublabel.address',resp$(2))
14800   let fnreg_write('ublabel.line 1',resp$(3))
14820   let fnreg_write('ublabel.line 2',resp$(4))
14840   let fnreg_write('ublabel.line 3',resp$(5))
14860   let fnreg_write('ublabel.line 4',resp$(6))
14880   let fnreg_write('ublabel.line 5',resp$(7))
14900   annbc=sequence_name ! default to name sequence
14920   let pt$(5)=""
14940   if resp$(1)=sequence_option$(sequence_account) then 
14960     annbc=sequence_account
14980   else if resp$(1)=sequence_option$(sequence_name) then 
15000     annbc=sequence_name
15020   else if resp$(1)=sequence_option$(sequence_bar_code) then 
15040     annbc=sequence_bar_code
15060     let pt$(5)="BAR"
15080   else if resp$(1)=sequence_option$(sequence_route) then 
15100     annbc=sequence_route
15120   else if resp$(1)=sequence_option$(sequence_grid) then 
15140     annbc=sequence_grid
15160   else if resp$(1)=sequence_option$(sequence_bulk_sort) then 
15180     annbc=sequence_bulk_sort
15200   end if 
15220 ! 
15240   altaddr=srch(mat address_option$,resp$(2))
15340 ! 
15360   let line_1_print=srch(mat line_x_option$,resp$(3))
15380   let line_2_print=srch(mat line_x_option$,resp$(4))
15400   let line_3_print=srch(mat line_x_option$,resp$(5))
15420   let line_4_print=srch(mat line_x_option$,resp$(6))
15440   let line_5_print=srch(mat line_x_option$,resp$(7))
15460 ! 
15480   if annbc=sequence_route then 
15500     let filter_selection=6
15520     gosub OPEN_FILES
15540     goto SELBK ! ROUTE # SEQUENCE
15560   else if annbc=sequence_grid then 
15580     gosub OPEN_FILES
15600     goto READ_FROM_GRID
15620   else if annbc=sequence_bulk_sort then 
15640     gosub BULKSORT
15660     gosub OPEN_FILES
15680     goto BULK_READ
15700   end if 
15720 SCR2: ! 
15740   let fntos(sn$="ublabel-2")
15760   let fnlbl(1,1,"Select by:",12,0,0)
15780   if annbc=sequence_account then 
15800     let filter_option_enabled$(1)=filter_option$(1)
15820     let filter_option_enabled$(2)=filter_option$(2)
15840     let filter_option_enabled$(3)=filter_option$(3)
15860     let filter_option_enabled$(4)=filter_option$(4)
15880     let filter_option_enabled$(5)=filter_option$(5)
15900     let filter_option_enabled$(6)=filter_option$(6)
15920     let filter_option_enabled$(7)=filter_option$(7)
15940     mat filter_option_enabled$(7)
15960   else if annbc=sequence_name then 
15980     let filter_option_enabled$(1)=filter_option$(1)
16000     let filter_option_enabled$(2)=filter_option$(2)
16020     let filter_option_enabled$(3)=filter_option$(3)
16040     let filter_option_enabled$(4)=filter_option$(5)
16060     let filter_option_enabled$(5)=filter_option$(7)
16080     mat filter_option_enabled$(5)
16100   else if annbc=sequence_bar_code then 
16120     let filter_option_enabled$(1)=filter_option$(1)
16140     let filter_option_enabled$(2)=filter_option$(2)
16160     let filter_option_enabled$(3)=filter_option$(3)
16180     let filter_option_enabled$(4)=filter_option$(5)
16200     let filter_option_enabled$(5)=filter_option$(6)
16220     mat filter_option_enabled$(5)
16240   else if annbc=sequence_route then 
16260     let filter_option_enabled$(1)=filter_option$(1)
16280     let filter_option_enabled$(2)=filter_option$(2)
16300     let filter_option_enabled$(3)=filter_option$(3)
16320     let filter_option_enabled$(4)=filter_option$(6)
16340     mat filter_option_enabled$(4)
16360   end if 
16380   let fncomboa("ublabels-ord",1,14,mat filter_option_enabled$,'',30)
16400   let resp$(1)=filter_option_enabled$(1)
16420   gosub OPEN_FILES
16440   let fncmdset(6)
16460   let fnacs(sn$,0,mat resp$,ckey) ! method of selection
16480   let filter_selection=srch(mat filter_option$,resp$(1))
16500   if ckey=5 then goto XIT
16520   if ckey=2 then goto SCR1
16540   if annbc=sequence_route then goto SELBK ! select by route
16560   if filter_selection=4 then goto SELR ! select range of customers
16580   if filter_selection=5 then goto IACC ! select specific accounts
16600   if filter_selection=6 and annbc=sequence_bar_code then goto SELBK
16620   if (filter_selection=1 or filter_selection=2 or filter_selection=3 or filter_selection=7) and annbc=sequence_account then goto SELSTART
16640   if (filter_selection=1 or filter_selection=7) and annbc=sequence_name then goto SCR4F3 ! all customers in name sequence
16660   goto TOP ! /r
20000 ! ___________________________________________________________________
20020 TOP: ! r:
20040   if annbc=sequence_bar_code then 
20060 BARCODE_READ_ADDR: ! 
20080     read #addr,using 'Form POS 1,PD 3': r6 eof DONE
20100     read #6,using "Form POS 1,C 16,C 10",rec=r6: srt$,z$ norec TOP
20120     if rtrm$(x$)<>"" and x$<>z$ then goto BARCODE_READ_ADDR
20140     let x$=""
20160     read #customer,using 'Form POS 1,C 10,4*C 30,POS 296,PD 4,POS 373,C 12,POS 1741,N 2,N 7,pos 1864,C 30,pos 1821,n 1',key=z$: z$,mat e$,f,f3$,route,seq,extra$(1),final nokey TOP
20180     let meter_address$=e$(1)
20200     if annbc=sequence_bar_code and filter_selection=6 and bk>0 and bk<>route then goto BARCODE_READ_ADDR ! skip if barcoded and by route, but not right route
20220   else if annbc=sequence_bulk_sort then 
20240 BULK_READ: ! 
20260     read #6,using 'form pos 22,c 10': z$ eof DONE
20280     if rtrm$(x$)<>"" and x$<>z$ then goto BULK_READ
20300     let x$=""
20320     read #customer,using 'Form POS 1,C 10,4*C 30,POS 296,PD 4,POS 373,C 12,POS 1741,N 2,N 7,pos 1864,C 30,pos 1942,c 12,pos 1821,n 1',key=z$: z$,mat e$,f,f3$,route,seq,extra$(1),bulksort$,final nokey TOP
20340     let meter_address$=e$(1)
20360     if annbc=sequence_bulk_sort and filter_selection=6 and bk>0 and bk<>route then goto BULK_READ ! skip if barcoded and by route, but not right route
20380   else 
20400     read #customer,using 'Form POS 1,C 10,4*C 30,C 12,POS 296,PD 4,POS 373,C 12,POS 1741,N 2,N 7,pos 1864,C 30,pos 1821,n 1': z$,mat e$,f$(1),f,f3$,route,seq,extra$(1),final eof DONE
20420     let meter_address$=e$(1)
20440   end if 
20460 PAST_READ: ! 
20480   if filter_selection=3 and d1=f then goto TOP
20500   if filter_selection=4 and z$>h1$ then goto DONE
20520   if filter_selection=6 and bk>0 and route<>bk then goto TOP
20540   if filter_selection=2 and d1><f then goto TOP
20560 THERE: ! 
20580   let fn_get_address_lines
20700   goto ADDLABEL ! /r
24000 ADDLABEL: ! r:
24020 ! if annbc=sequence_bulk_sort then
24040 !   let labeltext$(1)=labeltext$(1)&"  "&bulksort$ ! if bulk sort than auto add bulk sort code on to the end of the top line
24060 ! else 
24080   let fn_set_line_text(labeltext$(1),line_1_print)
24100 ! end if
24120   let fn_set_line_text(labeltext$(2),line_2_print)
24140   let fn_set_line_text(labeltext$(3),line_3_print)
24160   let fn_set_line_text(labeltext$(4),line_4_print)
24180   let fn_set_line_text(labeltext$(5),line_5_print)
24200   if annbc=sequence_bar_code then 
24220     gosub BARCODE
24240   end if 
24260   if final=0 or filter_selection<>7 then 
24280     let fnaddlabel(mat labeltext$)
24300   end if 
24320   if annbc=sequence_grid then return 
24340   if filter_selection=5 then goto IACC
24360   goto TOP
24380 ! /r
26000 DONE: ! r:
26020   close #1: ioerr ignore
26040   let fnlabel(101,cap$,mat pt$,cp,nw)
26080   goto XIT ! /r
27000   def fn_set_line_text(&labeltext$,line_2_print)
27020     if line_2_print=line_x_blank then 
27040       let labeltext$=''
27060     else if line_2_print=line_x_account_number then 
27080       let labeltext$=z$
27100     else if line_2_print=line_x_meter_address then 
27120       let labeltext$=meter_address$
27140     else if line_2_print=line_x_customer_name then 
27160       let labeltext$=pe$(1)
27180     else if line_2_print=line_x_mailing_address_line_1 then ! Mailing Address Line 1
27200       let labeltext$=pe$(2)
27220     else if line_2_print=line_x_mailing_address_line_2 then ! Mailing Address Line 2 or if blank City State Zip
27240       let labeltext$=pe$(3)
27260     else if line_2_print=line_x_mailing_address_line_3 then ! City State Zip if Mailing Address Line 2 not blank
27280       let labeltext$=pe$(4)
27282     else if line_2_print=line_x_meter_route_and_sequenc then ! City State Zip if Mailing Address Line 2 not blank
27284       let labeltext$=f3$&' '&str$(route)&' '&str$(seq) ! just use meter 3 for now, French Settlement Gas is the only one that uses this option.
27286     else if line_2_print=line_x_account_meter4_and_seq then ! City State Zip if Mailing Address Line 2 not blank
27288       let labeltext$=z$&' '&f3$&' '&str$(seq) ! just use meter 3 for now, French Settlement Gas is the only one that uses this option.
27300     else 
27320       let labeltext$='(invalid selection)'
27340     end if 
27360   fnend 
28000 XIT: let fnxit
30000 SELR: ! r: select range of accounts
30020   let fntos(sn$="ublabel-3")
30040   let fnlbl(1,1,"Starting Account:",20)
30060   let fncmbact(1,22,1)
30080   let resp$(1)=l1$
30100   let fnlbl(2,1,"Ending Account:",20)
30120   let fncmbact(2,22,1)
30140   let resp$(2)=h1$
30160   let fncmdset(22)
30180   let fnacs(sn$,0,mat resp$,ckey) ! select by range
30200   if ckey=5 then goto XIT
30220   let l1$=lpad$(trim$(resp$(1)(1:10)),10)
30240   let h1$=lpad$(trim$(resp$(2)(1:10)),10)
30260   if ckey=6 then let fncustomer_search(resp$(1)) else goto L1470
30280   if trim$(l1$)="" then let l1$=resp$(1) : goto SELR
30300   if trim$(h1$)="" then let h1$=resp$(1) : goto SELR
30320   goto SELR
30340 L1470: ! 
30360   if ckey=2 then goto SCR2
30380   let l1$=lpad$(l1$,10) : let h1$=lpad$(h1$,10)
30400   if h1$<l1$ then 
30420     mat msgline$(1)
30440     let msgline$(1)="You have entered invalid accounts!"
30460     let fnmsgbox(mat msgline$,resp$,cap$,48)
30480     goto SELR
30500   end if 
30520   restore #customer,key=l1$: nokey SELR
30540   goto TOP ! /r
32000 IACC: ! r: select individual accounts
32020   let fntos(sn$="ublabel-4")
32040   let fnlbl(1,1,"Account:",15)
32060   let fncmbact(1,17)
32080   let resp$(1)=selz$
32100   let fnlbl(3,1,"Last selection: "&hz$,35,0)
32120   if trim$(sele$(2))<>"" then let fnlbl(2,17,sele$(2),20,0)
32140   let fncmdset(23)
32160   let fnacs(sn$,0,mat resp$,ckey)
32180   if ckey=2 then 
32190     let fncustomer_search(resp$(1))
32200     let selz$=lpad$(rtrm$(resp$(1)(1:10)),10)
32220     read #customer,using 'Form POS 1,C 10,4*C 30,POS 296,PD 4,POS 373,C 12,POS 1741,N 2,N 7,pos 1864,C 30,pos 1821,n 1',key=selz$: selz$,mat sele$,extra$(1),final nokey ignore
32240     goto IACC
32260   end if 
32280   if ckey=5 then goto XIT
32300 ! if ckey=1 then goto L1660
32320   if ckey=4 then goto DONE
32340 ! L1660: !
32360   let z$=lpad$(rtrm$(resp$(1)(1:10)),10)
32380   let hz$=z$
32400   if rtrm$(z$)="" then goto DONE
32420   let selz$="": mat sele$=("")
32440   read #customer,using 'Form POS 1,C 10,4*C 30,POS 296,PD 4,POS 373,C 12,POS 1741,N 2,N 7,pos 1864,C 30,pos 1821,n 1',key=z$: z$,mat e$,f,f3$,route,seq,extra$(1),final nokey IACC
32460   let meter_address$=e$(1)
32480   goto THERE ! /r
34000 SELBK: ! r: selects by route
34020   let fntos(sn$="ublabel-5")
34040   let respc=0
34060   let fnlbl(1,1,"Route Number:",15,0)
34080   let fncmbrt2(1,17)
34100   let resp$(respc+=1)="1"
34120   let fnlbl(2,1,"Sequence Number:",15,0)
34140   let fntxt(2,17,7,7,1,"20",0,"The sequence number is only required if you wish to start in the middle of a route")
34160   let resp$(respc+=1)=""
34180   let fncmdset(22)
34200   let fnacs(sn$,0,mat resp$,ckey) ! select labels by route
34220   if ckey=5 then goto XIT
34240   if ckey=2 then goto SCR2
34260   bk=0 : bk=val(resp$(1)) conv L1860
34280   let seq=val(resp$(2)) conv L1860
34300   if annbc=sequence_bar_code and filter_selection=6 then goto TOP ! must start at front of file if bar coded and specific route
34320   L1860: ! 
34340   let routekey$=lpad$(str$(bk),2)&lpad$(str$(seq),7)
34360   restore #customer,key>=routekey$: nokey SELBK
34380 goto TOP ! /r
36000 SCR4F3: ! r: select starting account name
36020   let fntos(sn$="ublabel-7")
36040   let fnlbl(1,1,"Customer Name:",15,0)
36060   let fntxt(1,17,30,30,1,"",0,"Search and find the exact customer name if you wish to start with a specific customer")
36080   let resp$(1)=""
36100   if trim$(sele$(2))<>"" then 
36120     let fnlbl(2,17,sele$(2),20,0)
36140   end if 
36160   let fncmdset(21)
36180   let fnacs(sn$,0,mat resp$,ckey) ! select starting customer name
36200   if ckey=5 then goto XIT
36220   if ckey=6 then let fncustomer_search(resp$(1))
36260   restore #customer,key>="       ": nokey ignore
36280   SCR4F3_READ_CUSTOMER: ! 
36300   read #customer,using 'Form POS 1,C 10,4*C 30,POS 296,PD 4,POS 373,C 12,POS 1741,N 2,N 7,pos 1864,C 30,pos 1821,n 1': z$,mat e$,f,f3$,route,seq,extra$(1),final eof SCR4F3_NO_MATCH
36320   let meter_address$=e$(1)
36340   if trim$(resp$(1))="" then goto SCR4F3_FINIS
36360   if lpad$(resp$(1),10)<>z$ then goto SCR4F3_READ_CUSTOMER
36380   SCR4F3_FINIS: ! 
36400 goto PAST_READ
36420 ! ______________________________________________________________________
36440 SCR4F3_NO_MATCH: ! r:
36460   mat msgline$(1)
36480   let msgline$(1)="No matching name found!"
36500   let fnmsgbox(mat msgline$,resp$,cap$,48)
36520 goto SELSTART ! /r
36540 ! /r
38000 SORT1: ! r: SELECT & SORT
38020   gosub OPENCASS
38040   close #6: ioerr ignore
38060   open #6: "Name="&env$('temp')&"\Work."&session$&",Replace,RecL=26",internal,output 
38080   let s5=1
38100   restore #1: 
38120   do 
38140     read #customer,using "Form POS 1,C 10,POS 296,PD 4,pos 1864,C 30,pos 1821,n 1": z$,f,extra$(1),final eof END5
38160     cr$=bc$=""
38180     read #5,using "Form POS 96,C 12,C 4": bc$,cr$ nokey SORT1_NEXT ioerr SORT1_NEXT
38200     write #6,using "Form POS 1,C 16,C 10": bc$(1:5)&cr$&bc$(6:12),z$
38220     SORT1_NEXT: ! 
38240   loop 
38260   END5: ! 
38280   close #6: ioerr ignore
38300   close #9: ioerr ignore
38320   open #9: "Name="&env$('Temp')&"\CONTROL."&session$&",Size=0,RecL=128,Replace",internal,output 
38340   write #9,using 'Form POS 1,C 128': "File "&env$('temp')&"\Work."&session$&",,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
38360   write #9,using 'Form POS 1,C 128': "Mask 1,26,C,A"
38380   close #9: 
38400   execute "Free "&env$('Temp')&"\Addr."&session$&" -n" ioerr ignore
38420   execute "Sort "&env$('Temp')&"\CONTROL."&session$&" -n"
38440   open #6: "Name="&env$('temp')&"\Work."&session$,internal,input,relative 
38460   close #addr: ioerr ignore
38480   open #addr=7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative 
38520 return  ! /r
40000 BARCODE: ! r:
40020   gosub OPENCASS
40040   if file(5)=-1 then goto BARCODE_XIT
40060   let labeltext$(5)=""
40080   read #5,using 'Form POS 1,C 10,POS 96,C 12': z2$,bc$ nokey BARCODE_XIT
40100   for j=1 to 4
40120     let labeltext$(j)=labeltext$(j+1) ! move everything up one to allow for barcode
40140   next j
40160   let labeltext$(5)=rtrm$(bc$)
40180   BARCODE_XIT: ! 
40200 return  ! /r
42000 SELSTART: ! r: select customer to start with
42020   let fntos(sn$="ublabel-6")
42040   let mylen=26 : let mypos=mylen+2
42060   let fnlbl(1,1,"Starting account:",mylen,0)
42080   let fncmbact(1,mypos,1)
42100   if trim$(selz$)='' then let resp$(1)='[All]' else let resp$(1)=selz$
42180   let fncmdset(5)
42200   let fnacs(sn$,0,mat resp$,ckey) ! select starting customer
42220   if ckey=6 then 
42240     let fncustomer_search(resp$(1))
42260     let selz$=lpad$(rtrm$(resp$(1)(1:10)),10)
42280     read #customer,using 'Form POS 1,C 10,4*C 30,POS 296,PD 4,POS 373,C 12,POS 1741,N 2,N 7,pos 1864,C 30,pos 1821,n 1',key=selz$: selz$,mat sele$,extra$(1),final nokey ignore
42300     goto SELSTART
42320   end if 
42340   if ckey=5 then goto XIT
42360   let z$=lpad$(trim$(resp$(1)(1:10)),10)
42380   if trim$(z$)="[All]" then restore #1: : goto TOP
42400   let selz$="": mat sele$=("")
42420   read #customer,using 'Form POS 1,C 10,4*C 30,POS 296,PD 4,POS 373,C 12,POS 1741,N 2,N 7,pos 1864,C 30,pos 1821,n 1',key=z$: z$,mat e$,f,f3$,route,seq,extra$(1),final nokey SELSTART
42440   let meter_address$=e$(1)
42460   goto THERE ! /r
44000 READ_FROM_GRID: ! r: select customers from grid
44020   let fntos(sn$="ublabel-7")
44040   let fnlbl(1,1,"Grid name (including folders):",30,0)
44060   let fntxt(1,30,30,66,0,"70",0,"You must first export a fixed width file from the gird program (remember the name!)")
44080   let resp$(1)=""
44100   let fnlbl(2,40,"",30,0)
44120   let fncmdset(3)
44140   let fnacs(sn$,0,mat resp$,ckey) ! select starting customer #
44160   if ckey=5 then goto XIT
44180   open #6: "Name="&trim$(resp$(1)),display,input ioerr READ_FROM_GRID
44200   LIN6: linput #6: x$ eof DONE
44220   let z$=lpad$(trim$(x$(1:10)),10)
44240   read #customer,using 'Form POS 1,C 10,4*C 30,POS 296,PD 4,POS 373,C 12,POS 1741,N 2,N 7,pos 1864,C 30,pos 1821,n 1',key=z$: z$,mat e$,f,f3$,route,seq,extra$(1),final nokey LIN6
44260   let meter_address$=e$(1)
44280   let fn_get_address_lines
44380   gosub ADDLABEL
44400 goto LIN6 ! /r
46000 def library fncustomer_address(z$*10,mat addr$; ca_address_type)
46020   if ~setup the let fn_setup
46040   fncustomer_address=fn_customer_address(z$,mat addr$, ca_address_type)
46060 fnend
48000 def fn_customer_address(z$*10,mat addr$; ca_address_type)
48020   if ~ca_setup then 
48040     ca_setup=1
48060     open #h_ca_customer:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&',Shr',internal,input,keyed
48080     open #adrbil:=fngethandle: "Name="&env$('Q')&"\UBmstr\ubAdrBil.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\AdrIndex.h"&env$('cno')&",Shr",internal,input,keyed 
48100   end if
48120   if ca_address_type=0 then ca_address_type=ao_billing
48140   altaddr=ca_address_type
48160   mat pe$=('')
48170   z$=lpad$(trim$(z$),kln(h_ca_customer))
48180   read #h_ca_customer,using 'Form POS 11,4*C 30,pos 1864,C 30,pos 1854,PD 5.2',key=lpad$(z$,kln(h_ca_customer)): mat e$,extra$(1),extra(22) nokey CA_FINIS
48200   let fn_get_address_lines
48220   CA_FINIS: !
48230   if trim$(pe$(2))="" then let pe$(2)=pe$(3): let pe$(3)=""
48232   if trim$(pe$(3))="" then let pe$(3)=pe$(4): let pe$(4)=""
48240   mat addr$(4)
48260   mat addr$=pe$
48280 fnend
50000 def fn_get_address_lines
50020   if altaddr=ao_alternate then 
50040     goto GAL_USE_ALT_ADDR
50060   else if altaddr=ao_primary then 
50080     goto GAL_USE_PRIME_ADR
50100   else !   (default)   if altaddr=ao_billing then
50110     if customer then read #customer,using 'Form pos 1854,PD 5.2',key=z$: extra(22) ! else it is called from the library function fncustomer_address, which already read it
50120     if extra(22)=0 or extra(22)=2 then 
50140       let do_not_use_alt_addr=1
50160     else 
50180       let do_not_use_alt_addr=0
50200     end if 
50220     if do_not_use_alt_addr then 
50240       goto GAL_USE_PRIME_ADR
50260     else 
50280       goto GAL_USE_ALT_ADDR
50300     end if 
50320   end if 
50340   goto GAL_XIT
50360   GAL_USE_ALT_ADDR: ! r:
50380   read #adrbil,using "Form POS 11,4*C 30",key=z$: mat ba$ nokey GAL_USE_PRIME_ADR
50400   let fn_alternate_address
50420   goto GAL_XIT ! /r
50440   GAL_USE_PRIME_ADR: ! r:
50460   let fn_primary_address
50480   goto GAL_XIT ! /r
50500   GAL_XIT: ! 
50520 fnend 
52000 def fn_alternate_address
52020   let e1=0
52040   mat pe$=("")
52060   for j=1 to 4
52080     if rtrm$(ba$(j))<>"" then let pe$(e1+=1)=ba$(j)
52100   next j
52120 fnend 
54000 def fn_primary_address
54020   let e1=0 : mat pe$=("")
54040   for j=2 to 4
54060     if rtrm$(e$(j))<>"" then let pe$(e1+=1)=e$(j)
54080   next j
54100   if trim$(extra$(1))<>'' then let pe$(4)=pe$(3) : let pe$(3)=extra$(1)
54120 fnend 
56000 ! <Updateable Region: ERTN>
56020 ERTN: let fnerror(program$,err,line,act$,"xit")
56040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
56060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
56080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
56100 ERTN_EXEC_ACT: execute act$ : goto ERTN
56120 ! /region
58000 BULKSORT: ! r: bulk sort order
58020   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno'),internal,input,keyed  ! open in Account order
58040   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=31",internal,output 
58060   do 
58080     read #1,using "Form POS 1,C 10,pos 1741,n 2,pos 1743,n 7,pos 1942,c 12": z$,route,seq,bulk$ eof BULKSORT_FINIS
58100     write #6,using "Form POS 1,C 12,n 2,n 7,c 10": bulk$,route,seq,z$
58120   loop 
58140 BULKSORT_FINIS: ! 
58160   close #1: ioerr ignore
58180   close #6: ioerr ignore
58200   execute "Index "&env$('Temp')&"\Temp."&wsid$&" "&env$('Temp')&"\Tempidx."&wsid$&" 1,19,Replace,DupKeys -n" ioerr BULKSORT_XIT
58220   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",KFName="&env$('Temp')&"\Tempidx."&wsid$,internal,input,keyed 
58240 BULKSORT_XIT: ! 
58260   return  ! /r
60000 OPEN_FILES: ! r:
60020   close #customer=1: ioerr ignore
60040   if annbc=sequence_account or annbc=sequence_bar_code or annbc=sequence_grid or annbc=sequence_bulk_sort then 
60060     open #customer: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed 
60080   else if annbc=sequence_name then 
60100     open #customer: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\UBIndx2.h"&env$('cno')&",Shr",internal,input,keyed 
60120   else if annbc=sequence_route then 
60140     open #customer: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&env$('cno')&",Shr",internal,input,keyed 
60160   end if 
60180   close #adrbil=3: ioerr ignore
60200   open #adrbil: "Name="&env$('Q')&"\UBmstr\ubAdrBil.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\AdrIndex.h"&env$('cno')&",Shr",internal,input,keyed 
60220   execute "drop "&env$('temp')&"\label.dat -n" ioerr ignore
60240   let fnd1(d1)
60260   if annbc=sequence_bar_code and s5=0 then gosub SORT1
60280   return  ! /r
62000 OPENCASS: ! r:
62020   if file(5)=-1 then 
62040     open #5: "Name="&env$('Q')&"\UBmstr\Cass1.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\Cass1Idx.h"&env$('cno')&",Shr",internal,input,keyed ioerr ignore
62060   end if 
62080   return  ! /r
64000 IGNORE: continue 
68000   def fn_setup
68020   library 'S:\Core\Library': fntop,fnxit, fnerror,fnacs,fncomboa,fnlbl,fntos,fnmsgbox,fntxt,fncustomer_search,fncmbrt2,fncmbact,fnaddlabel,fnlabel,fncmdset,fnd1,fngethandle,fnreg_read,fnreg_write,fnget_services
68040   on error goto ERTN
68060 ! r: constants and dims
68080   on fkey 5 goto DONE
68120   dim filter_option_enabled$(7)*50,e$(4)*30
68140   dim meter_address$*30
68160   dim pe$(4)*30,ba$(4)*30,cap$*128,resp$(10)*80
68180   dim labeltext$(5)*120,x$*512,z$*50
68200   dim extra$(11)*30,extra(23) ! fields from Customer File
68220   dim srvnam$(10)*20,srv$(10)*2
68240 ! 
68260   fnget_services(mat srvnam$, mat srv$)
68280 ! 
68300   dim line_x_option$(9)*70
68320   let line_x_option$(line_x_blank:=1)="(blank)"
68340   let line_x_option$(line_x_account_number:=2)="Account"
68360   let line_x_option$(line_x_meter_address:=3)="Meter Address"
68380   let line_x_option$(line_x_customer_name:=4)="Name"
68400   let line_x_option$(line_x_mailing_address_line_1:=5)="Mailing Address Line 1"
68420   let line_x_option$(line_x_mailing_address_line_2:=6)="Mailing Address Line 2 or if blank City State Zip"
68440   let line_x_option$(line_x_mailing_address_line_3:=7)="City State Zip if Mailing Address Line 2 not blank"
68460   let line_x_option$(line_x_meter_route_and_sequenc:=8)=trim$(srvnam$(4))&" Meter, Route and Sequence Numbers"
68480   let line_x_option$(line_x_account_meter4_and_seq:=9)="Account, "&trim$(srvnam$(4))&" Meter, and Sequence Numbers"
68500 ! 
68520   dim address_option$(3)*30
68540   address_option$(ao_primary:=1)="Primary Address"
68560   address_option$(ao_alternate:=2)="Alternate Billing Address"
68580   address_option$(ao_billing:=3)="Billing Address"
68600 ! 
68620   dim sequence_option$(6)*22
68640   let sequence_option$(sequence_account:=1)="Account"
68660   let sequence_option$(sequence_name:=2)="Customer Name"
68680   let sequence_option$(sequence_bar_code:=3)="Bar Code"
68700   let sequence_option$(sequence_route:=4)="Route"
68720   let sequence_option$(sequence_grid:=5)="Grid"
68740   let sequence_option$(sequence_bulk_sort:=6)="Bulk Sort Code"
68760 ! 
68780   dim filter_option$(7)*50
68800   let filter_option$(1)="[All]"
68820   let filter_option$(2)="Customers billed last month"
68840   let filter_option$(3)="Customers not billed last billing"
68860   let filter_option$(4)="Range of Accounts"
68880   let filter_option$(5)="Individual accounts"
68900   let filter_option$(6)="Route"
68920   let filter_option$(7)="Active Customers"
68940 ! /r
68960 fnend 