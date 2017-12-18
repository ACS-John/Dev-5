00010 ! Replace S:\acsUB\ubNamLst
00020 ! r: setup library, dims, constants, fntop, etc
22000   library 'S:\Core\Library': fnacs, fnopenprn,fncloseprn,fnerror,fncno,fndat,fnlbl,fntxt,fnchk,fntos,fncomboa,fnxit,fncmdset,fntop,fnget_services
22020   on error goto ERTN
22040 ! ______________________________________________________________________
22060   dim z$*10,e$(4)*30,cnam$*40,dat$*20,idx$(5)*40,resp$(10)*80
22080   dim item1$(6)*22,item2$(6)*32,x$*512,cap$*128,text$*50
22120   dim hd1$*30,a2(10),a1(10),a(10)
22130   dim ab$(4)*30,extra$(11)*30
22140 ! ______________________________________________________________________
24000   fntop("S:\acsUB\ubNamLst",cap$="Name and Number List")
24020   fncno(cno,cnam$)
24040   fndat(dat$,1)
24060 ! 
24080   idx$(1)=env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')
24100   idx$(2)=env$('Q')&"\UBmstr\ubIndx2.h"&env$('cno')
24120   idx$(3)=env$('Q')&"\UBmstr\ubIndx3.h"&env$('cno')
24140   idx$(4)=env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')
24160   idx$(5)=env$('Q')&"\UBmstr\ubIndx5.h"&env$('cno')
24180 ! 
24200   item1$(1)="Account"
24220   item1$(2)="Customer Name"
24240   item1$(3)="Street"
24260   item1$(4)="Street - Auto-Reversed"
24280   item1$(5)="Grid Selection"
24300   item1$(6)="Route Sequence"
24320 ! 
24360   item2$(1)="Active Customers Only"
24380   item2$(2)="Inactive Customers Only"
24390   item2$(3)="All Customers (Regular Address)"
24400   item2$(4)="All using Mailing Address"
24420   item2$(5)="Only Alternate Addresses"
24440   item2$(6)="Active, But Not Being Billed"
24450 ! 
24460   open #3: "Name="&env$('Q')&"\UBmstr\ubAdrBil.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\AdrIndex.h"&env$('cno')&",Shr",internal,outin,keyed 
24480 ! /r
34000 ! MENU1: ! r:
34020   fntos("ubnamlst")
34040   respc=0
34060   fnlbl(1,1,"Sequence:",23,1)
34080   fncomboa("ubnamlst-srt",1,25,mat item1$,"The auto-reversed option can turn all addresses around so the streets are sorted by name rather than number (ie Adams Streets together instead of 101s")
34100   resp$(respc+=1)=item1$(1)
34120   fnlbl(2,1,"Report Heading Date:",23,1)
34140   fntxt(2,25,20)
34160   resp$(respc+=1)=dat$
34180   fnlbl(3,1,"Limit by:",23,1)
34200   fncomboa("ubnamlst-act",3,25,mat item2$)
34220   resp$(respc+=1)=item2$(3)
34240   fnchk(5,29,"Print Rate Codes")
34260   resp$(respc+=1)="False"
34280   fnchk(6,29,"Print Address")
34300   resp$(respc+=1)="False"
34320   fnchk(8,29,"Print Balance")
34340   fnlbl(8,45,"(Route Sequence never prints Balance)",23,1)
34360   resp$(resp_print_balance:=respc+=1)="True"
34380   fnchk(9,29,"Print Phone")
34400   resp$(resp_print_phone:=respc+=1)="False"
34420   fnchk(10,29,"Print Cell")
34440   resp$(resp_print_cell:=respc+=1)="False"
34460   fncmdset(2)
34480   fnacs(sn$,0,mat resp$,ck)
38000   if ck=5 then goto XIT
38020   q0=2 ! default to name sequence
38040   if resp$(1)=item1$(1) then 
38060     q0=1 : opt=1 : turn$="N"
38080   else if resp$(1)=item1$(2) then 
38100     q0=2 : opt=2 : turn$="N"
38120   else if resp$(1)=item1$(3) then 
38140     q0=3 : opt=3 : turn$="N"
38160   else if resp$(1)=item1$(4) then 
38180     q0=3 : opt=4 : turn$="Y"
38200   else if resp$(1)=item1$(5) then 
38220     q0=4 : opt=5 : turn$="N"
38240   else if resp$(1)=item1$(6) then 
38260     q0=5 : opt=6 : turn$="N"
38280   end if 
38300   dat$=resp$(2)
38320   fndat(dat$,2)
38340   if resp$(4)="True" then ti3=1 else ti3=0
38360   if resp$(5)="True" then print_address=1 else print_address=0
38380   if resp$(resp_print_balance)="True" then print_balance=1 else print_balance=0
38400   if resp$(resp_print_phone)="True" then print_phone=1 else print_phone=0
38420   if resp$(resp_print_cell)="True" then print_cell=1 else print_cell=0
38440   if resp$(3)=item2$(1) then 
38460     ti2=1
38480   else if resp$(3)=item2$(2) then 
38500     ti2=2
38520   else if resp$(3)=item2$(3) then 
38540     ti2=3
38560   else if resp$(3)=item2$(4) then 
38580     ti2=4
38600   else if resp$(3)=item2$(5) then 
38620     ti2=5
38640   else if resp$(3)=item2$(6) then 
38660     ti2=6
38680   end if 
38700 ! /r
42000   if ti3=1 then gosub GET_AU
42020   on fkey 5 goto DONE
42040   if q0=3 and turn$="Y" then gosub STREET_REVERSE
42080   if uprc$(turn$)="Y" then 
42100     open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&idx$(1)&",Shr",internal,input,keyed 
42120   else 
42140     open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&idx$(q0)&",Shr",internal,input,keyed 
42160     if q0=4 then gosub OPEN_GRID ! OPEN GRID DISPLAY FILE
42180   end if 
42200 F_CUSTOMER: form pos 1,c 10,pos 11,4*c 30,pos 143,5*pd 2,pos 1806,3*n 2,pos 153,2*pd 2,pos 1821,n 1,pos 292,pd 4.2,pos 1741,n 2,n 7,pos 1864,c 30,c 12,pos 1966,c 12

42210   fnopenprn
42220   gosub HEADER
42240 LOOP_TOP: ! 
42260   if q0=4 then 
42280     gosub READ_FROM_GRID
42300     goto L520 ! READ FROM GRID DISPLAY
42320   end if 
42340 L490: ! 
42360   if uprc$(turn$)="Y" then 
42380     read #7,using "Form POS 1,PD 3": addr eof DONE
42400   else 
42420     goto L570
42440   end if 
42460   read #6,using 'form pos 1,c 10',rec=addr: z$ norec LOOP_TOP
42480 L520: ! 
42500   read #1,using F_CUSTOMER,key=z$: z$,mat e$,mat a,final,bal,route,sequence,extra$(1),extra$(2),extra$(8) eof DONE nokey LOOP_TOP
42520   if ti2=2 and final=0 then goto L490 ! skip active
42540   if ti2=4 or ti2=5 then gosub CHECK_ALTERNATE
42560   goto L590
42580 ! ______________________________________________________________________
46000 L570: ! 
46020   read #1,using F_CUSTOMER: z$,mat e$,mat a,final,bal,route,sequence,extra$(1),extra$(2),extra$(8) eof DONE
46040   if ti2=2 and (final=0 or final=3) then ! skip active (including the final code threes who are active but snow birding
46060     goto L570
46080   end if 
46100 L590: ! 
46120   if ti3=1 then 
46140     j1=1 : mat a2=(0)
46160     for j=1 to 10
46180       if a1(j)=1 then 
46200         a2(j1)=a(j)
46220         j1=j1+1
46240       end if 
46260     next j
46270   end if 
46280   if ti2=3 then goto L690
46300   if ti2=1 and final><0 then goto LOOP_TOP
46320   if ti2=4 or ti2=5 then gosub CHECK_ALTERNATE
46340   if ti2=5 and trim$(ab$(1))="" and trim$(ab$(2))="" and trim$(ab$(3))="" and trim$(ab$(4))="" then 
46360     goto LOOP_TOP
46380   end if 
46400   if ti2=6 and final<>3 then goto L570
46420 L690: ! 
46440   if opt=6 then ! route sequence
46460     pr #255,using F_OUT_ROUTE_SEQ: z$,e$(2),e$(1),route,sequence,mat a2 pageoflow PGOF
46470     F_OUT_ROUTE_SEQ: form x 5,c 10,x 5,c 30,x 7,c 30,n 2,x 1,n 7,x 1,10*nz 3
46500   else if print_balance then 
46510     pr #255,using F_OUT_W_BAL: z$,e$(2),e$(1),bal,mat a2 pageoflow PGOF
46520     F_OUT_W_BAL: form x 5,c 10,x 5,c 30,x 7,c 30,n 11.2,x 1,10*nz 3
46530   else 
46540     pr #255,using F_OUT_NOBAL: z$,e$(2),e$(1),mat a2 pageoflow PGOF
46550     F_OUT_NOBAL: form x 5,c 10,x 5,c 30,x 7,c 30,x 12,10*nz 3
46560   end if 
46580   if trim$(e$(3))="" then e$(3)=extra$(1): extra$(1)=""
46600   if trim$(extra$(1))="" then extra$(1)=e$(4): e$(4)=""
46620   if print_address=1 then 
46640     pr #255,using "form pos 21,c 31": e$(3)
46660     pr #255,using "form pos 21,c 31": extra$(1)
46680     pr #255,using "form pos 21,c 30": e$(4)
46700   end if 
46720   if print_phone=1 and trim$(extra$(2))<>'' then 
46740     pr #255,using "form pos 21,c 31": '  Phone: '&extra$(2)
46760   end if
46780   if print_cell=1 and trim$(extra$(8))<>'' then 
46800     pr #255,using "form pos 21,c 31": '   Cell: '&extra$(8)
46820   end if
46840   goto LOOP_TOP
46860 ! ______________________________________________________________________
48000 PGOF: ! 
48010   pr #255: newpage
48020   gosub HEADER
48040   continue 
48060 ! ______________________________________________________________________
50000 HEADER: ! r:
50020   pr #255: "\qc {\b "&env$('cnam')&"}"
50040   p2=p2+1
50060   pr #255: "\qc {\fs28 {\b Name and Number Listing}}"
50080   pr #255: "\qc {\b "&trim$(item1$(opt))&" Order}"
50100   pr #255: "\qc {\b "&trim$(item2$(ti2))&"}"
50120   pr #255,using 'form pos 21,cc 40,pos 71,c 5,pic(zzz)': dat$,"Page ",p2
50160   pr #255: ""
50180   if opt=6 then 
50190     pr #255: "\ql {\ul     Customer No}     {\ul Name                             }    {\ul Meter Address                }  {\ul Rt-Seq   }  {\ul "&hd1$&"}"
50192   else if print_balance then 
50200     pr #255: "\ql {\ul     Customer No}     {\ul Name                             }    {\ul Meter Address                }   {\ul   Balance} {\ul "&hd1$&"}"
50210   else 
50212     pr #255: "\ql {\ul     Customer No}     {\ul Name                             }    {\ul Meter Address                }              {\ul "&hd1$&"}"
50220   end if 
50230   return  ! /r
50240 ! ______________________________________________________________________
52000 DONE: ! 
52010   close #1: ioerr ignore
52020   fncloseprn
52040 XIT: fnxit
52060 IGNORE: continue 
54000 ! <Updateable Region: ERTN>
54020 ERTN: fnerror(program$,err,line,act$,"xit")
54040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
54060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
54080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
54100 ERTN_EXEC_ACT: execute act$ : goto ERTN
54120 ! /region
56000 GET_AU: ! r:
56010   dim servicename$(10)*20,service$(10)*2
56020   fnget_services(mat servicename$,mat service$)
56100   for j=1 to 10
56120     if trim$(service$(j))<>"" then 
56140       hd1$=hd1$&lpad$(service$(j)(1:2),3)
56160       a1(j)=1
56180     end if 
56200   next j
56220   return  ! /r
58000 STREET_REVERSE: ! r: FOR SORTING
58020 ! wIN=101: pFX=0
58040 ! vALUE$="True"
58060 ! tEXT$="Turn address around"
58080 ! fnCHECK(PFX,1,2,0,VALUE$,TEXT$)
58100 ! tEXT$="The system can turn the street address around"
58120 ! fnPRF(PFX,4,1,50,0,TEXT$)
58140 ! tEXT$="for sorting if you have entered street addresses"
58160 ! fnPRF(PFX,5,1,50,0,TEXT$)
58180 ! tEXT$="with the street number first.  This will print"
58200 ! fnPRF(PFX,6,1,50,0,TEXT$)
58220 ! tEXT$="all Adams Streets together rather than all 101s together."
58240 ! tEXT$="together."
58260 ! fnPRF(PFX,8,1,50,0,TEXT$)
58280 ! fnACS(WIN,3,MAT resp$,CK)
58300 ! If resp$(1)="True" Then tURN$="Y" Else tURN$="N"
58320 ! If CK=5 Then Goto XIT
60020   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&idx$(q0)&",Shr",internal,input,keyed 
60040   open #10: "Name="&env$('Temp')&"\Temp."&session$&",RecL=40,Replace",internal,outin 
60060   do 
60080     read #1,using 'form pos 1,c 10,c 30': z$,e$(1) eof SORT1
60100     x=y=0
60120     x=pos(e$(1)," ",1)
60140     if x>0 then y=val(e$(1)(1:x-1)) conv ignore
60160     if y>0 then e$(1)=rtrm$(e$(1)(x+1:30))&" "&e$(1)(1:x-1)
60180     write #10,using 'form pos 1,c 10,c 30': z$,e$(1)
60200   loop 
62000 SORT1: ! 
62020   close #1: 
62040   close #10: 
62060   open #9: "Name="&env$('Temp')&"\Control."&session$&",Size=0,RecL=128,Replace",internal,output 
62100   write #9,using 'form pos 1,c 128': "File "&env$('Temp')&"\Temp."&session$&",,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
62120   write #9,using 'form pos 1,c 128': "Mask 11,30,C,A"
62140   close #9: 
62160   execute "Free "&env$('Temp')&"\Addr."&session$ ioerr ignore
62200   execute "Sort "&env$('Temp')&"\Control."&session$
62220   open #6: "Name="&env$('Temp')&"\Temp."&session$,internal,input,relative 
62240   open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative 
62260   return  ! /r
64000 OPEN_GRID: ! r: select customers from grid
64020   sn$="ublabel-7"
64040   cap$="Reading from grid"
64060   fntos(sn$)
64080   text$="Grid name (including folders):"
64100   fnlbl(1,1,text$,70,0)
64120   fntxt(1,30,70,0,0,"70",0,"You must first export a fixed width file from the gird program (remember the name!)")
64140   resp$(1)=""
64160   fncmdset(3)
64180   fnacs(sn$,0,mat resp$,ckey) ! Select starting customer #
64200   if ckey=5 then goto XIT
64220   open #6: "Name="&trim$(resp$(1)),display,input ioerr OPEN_GRID
64240   return  ! /r
66000 READ_FROM_GRID: ! r: READ CUSTOMER # FROM GRID
66020   linput #6: x$ eof DONE
66040   z$=lpad$(trim$(x$(1:10)),10)
66060   return  ! /r
68000 CHECK_ALTERNATE: ! r: check for alternate billing address
68020   mat ab$=('')
68040   read #3,using "Form POS 11,4*C 30",key=z$: mat ab$ nokey L1690
68060   if trim$(ab$(1))="" and trim$(ab$(2))="" and trim$(ab$(3))="" and trim$(ab$(4))="" then 
68080     goto L1690
68100   end if 
68120   if ti2=4 or ti2=5 then 
68140     e$(2)=ab$(1)
68160     e$(3)=ab$(2)
68180     e$(4)=ab$(4)
68200     extra$(1)=ab$(3)
68220   end if 
68240 L1690: ! 
68260   return  ! /r
