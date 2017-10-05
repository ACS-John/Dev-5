00010 ! Replace S:\acsGL\W2Box16
00020 ! (allows you to enter miscellaneous information to any w-2)
00040 ! r: setup for local
00060   fn_wesupSetup
00080   dim  cap$*128,io1$(30)*64,in1$(30)
00090   fntop(program$,cap$:="W-2 Supplemental Information")
00106   io1$(1)="7,11,C 12,[screen]" 
00108   io1$(2)="7,25,G 10.2,[screen]" 
00110   io1$(3)="7,37,G 1,[screen]" 
00112   io1$(4)="7,43,G 1,[screen]" 
00114   io1$(5)="7,49,G 1,[screen]"
00116   for j=1 to 5
00118     io1$(j+05)="08"&io1$(j)(2:inf)
00120     io1$(j+10)="10"&io1$(j)(2:inf) 
00122     io1$(j+15)="11"&io1$(j)(2:inf)
00124     io1$(j+20)="12"&io1$(j)(2:inf) 
00126     io1$(j+25)="13"&io1$(j)(2:inf)
00128   next j
00196   mat m1ColHeading$(1)
00198   mat m1ColMask$(1)
00200   m1ColHeading$(1)='Employee'
00205 goto MENU1 ! /r

12000 def fn_wesupSetup
12020   if ~w2supSetup then
12040     w2supSetup=1
12060     library 'S:\Core\Library': fntop,fnxit,fnerror,fnopenprn,fncloseprn,fnacs,fncmdset,fnflexadd1,fnflexinit1,fntos,fngethandle,fnlbl,fncombof,fncomboa,fntxt
12080     on error goto ERTN
12100     dim resp$(64)*128
12120     dim t$*8
12180     open #hw2box16:=fngethandle: "Name="&env$('Q')&"\GLmstr\W2Box16.h"&env$('cno')&",Version=0,KFName="&env$('Q')&"\GLmstr\W2Index.h"&env$('cno')&",Use,RecL=158,KPs=1,KLn=8,Shr",internal,outin,keyed
12200     dim fw2box16$*255
12220     fw2box16$="FORM  POS 1,C 8"&rpt$(",C 12,G 10.2,3*G 1",6)
12240   end if
12260 fnend
24000 MENU1: ! r:
24020   fntos(sn$='w2box16.menu1')
24040   fnflexinit1(sn$,2,2,10,10,mat m1ColHeading$,mat m1ColMask$)
24060   restore #hw2box16:
24070   recCount=0
24080   do
24100     read #hw2box16,using fw2box16$: emp$ eof m1EoList
24120     fnflexadd1(emp$)
24130     recCount+=1
24140   loop
24160   m1EoList: !
24180   if recCount then let fncmdset(14) else let fncmdset(15)
24280   fnacs(sn$,0,mat resp$,ckey)
24300   if ckey=1 then ! Add
24320     fn_w2supEdit
24360   else if ckey=2 then ! Edit
24380     fn_w2supEdit(resp$(1))
24420   else if ckey=4 then ! Print
24440     goto PrintProofList
24460   else if ckey=5 then ! Cancel
24480     close #hw2box16:  
24500     goto XIT
24520   end if
24540 ! /r

32000 def library fnW2supEdit(;empNo$)
32020   fn_wesupSetup
32040   fnW2supEdit=fn_w2supEdit( empNo$)
32060 fnend
33000 def fn_w2supEdit(; empNo$)
33100   t$=empNo$
33120   mat in1$=('')
33140   if empNo$<>'' then
33160     t$=rpad$(empNo$,kln(hw2box16))
33180     read #hw2box16,using fw2box16$,key=t$: t$,mat in1$ noKey ignore 
33200   else
34000     fntos(sn$='w2supEdit1')
34020     mylen=15 : mypos=mylen+2
34040     fnlbl(1,1,'Employee:',mylen,1)
34060     fncombof('emp',1,mypos,width,env$('Q')&'\GLmstr\PRmstr.h'&env$('cno'),1,4,5,25, env$('Q')&'\GLmstr\PRIndex.h'&env$('cno'),1)
34080     resp$(1)=t$
34100     fncmdset(2)
34120     fnacs(sn$,0,mat resp$,ckey)
34140     if ckey=5 then goto w2eFinis
34160     empNo$=resp$(1)(1:4)
34180     t$=uprc$(lpad$(rtrm$(t$),8))
34200     mat in1$=('')
34220     read #hw2box16,using fw2box16$,key=t$: t$,mat in1$ nokey ignore
34240   end if
36000   ! r: RecEdit
36020   fntos(sn$='w2supEdit2')
36040   lenCol1=16
36060   posCol2=18 ! 30
36080   posCol3=34 ! 50  
36100   posCol4=46
36120   posCol5=58
36140   posCol6=70
36160   rc=0
36180   fnlbl(1,2,'Employee Number:',lenCol1)
36200   fntxt(1,posCol2,8,0,0,'',1)
36220   resp$(rc+=1)=t$
36240   fnlbl(4,posCol2,'Description')
36260   fnlbl(4,posCol3,'Amount')
36280   fnlbl(3,posCol4,'Effect on Wages')
36300   fnlbl(4,posCol4,'Fed')
36320   fnlbl(4,posCol5,'FICA')
36340   fnlbl(4,posCol6,'State')
36360   dim nasOption$(3)*10
36380   nasOption$(1)='0 None'
36400   nasOption$(2)='1 Add'
36420   nasOption$(3)='2 Subtract'
36440   lc=4
36460   fnlbl(lc+=1,1,"Box 11:",lenCol1,1)
36480     fn_add_this_one(mat in1$(01:05))
36500   fnlbl(lc+=1,1,"Unused:",lenCol1,1)
36520     fn_add_this_one(mat in1$(06:10))
36540   fnlbl(lc+=1,1,"Box 12a:",lenCol1,1)
36560     fn_add_this_one(mat in1$(11:15))
36580   fnlbl(lc+=1,1,"Box 12b:",lenCol1,1)
36600     fn_add_this_one(mat in1$(16:20))
36620   fnlbl(lc+=1,1,"Box 12c:",lenCol1,1)
36640     fn_add_this_one(mat in1$(21:25))
36660   fnlbl(lc+=1,1,"Box 12d:",lenCol1,1)
36680     fn_add_this_one(mat in1$(26:30))
36700   fncmdset(4)
36702   fnacs(sn$,0,mat resp$,ckey)
38000   if cmdkey=5 then goto w2eFinis
38020   rc=0
38040   t$=resp$(rc+=1)
38060   in1Count=0
38080   for in1Item=1 to 30 ! udim(mat in1$)
38100     in1$(in1Item)=resp$(rc+=1)
38120     if in1Item=3 or 4 or 5 then
38140       nasWhich=srch(mat nasOption$,in1$(in1Item))
38160       if nasWhich>0 then
38180         in1$(in1Item)=nasOption$(nasWhich)(1:1)
38200       end if
38220     end if
38240   nex in1Item
39000   rewrite #hw2box16,using fw2box16$,key=rpad$(t$,kln(hw2box16)): t$,mat in1$ nokey w2eWrite
39020   goto w2eFinis
39040   w2eWrite: !
39060   write #hw2box16,using fw2box16$: t$,mat in1$
39080   goto w2eFinis
39100   w2eFinis: ! /r
39120 fnend

42000 def fn_add_this_one(mat in1Five$)
42020   fntxt(lc,posCol2,12) : resp$(rc+=1)=in1Five$(1) ! desc
42040   fntxt(lc,posCol3,11,0,0,'10') : resp$(rc+=1)=in1Five$(2) ! amount
42060   fncomboa('nas',lc,posCol4,mat nasOption$)
42080   resp$(rc+=1)=nasOption$(val(in1Five$(3))+1) !  Fed
42100   fncomboa('nas',lc,posCol5,mat nasOption$)
42120   resp$(rc+=1)=nasOption$(val(in1Five$(4))+1) ! FICA
42140   fncomboa('nas',lc,posCol6,mat nasOption$)
42160   resp$(rc+=1)=nasOption$(val(in1Five$(5))+1) ! State
42180 fnend
52000 PrintProofList: ! r:
52020   restore #hw2box16: 
52040   pg=0
52060   fnopenprn
52080   gosub HEADER
52100   do
52120     read #hw2box16,using fw2box16$: t$,mat in1$ eof END_OF_PROOF_LIST
52140     pr #255: 
52160     pr #255: "Employee Number: ";t$
52180     pr #255,using L1820: "Box 11",in1$(1),val(in1$(2)),val(in1$(3)),val(in1$(4)),val(in1$(5))
52200     pr #255,using L1820: "Unused",in1$(6),val(in1$(7)),val(in1$(8)),val(in1$(9)),val(in1$(10))
52220     pr #255,using L1820: "Box 12a",in1$(11),val(in1$(12)),val(in1$(13)),val(in1$(14)),val(in1$(15))
52240     pr #255,using L1820: "Box 12b",in1$(16),val(in1$(17)),val(in1$(18)),val(in1$(19)),val(in1$(20))
52260     pr #255,using L1820: "Box 12c",in1$(21),val(in1$(22)),val(in1$(23)),val(in1$(24)),val(in1$(25))
52280     pr #255,using L1820: "Box 12d",in1$(26),val(in1$(27)),val(in1$(28)),val(in1$(29)),val(in1$(30))
52300     L1820: form pos 2,c 9,c 12,n 12.2,3*n 6
52320   loop
52340   END_OF_PROOF_LIST: ! 
52360   fncloseprn
52380 goto MENU1 ! /r
52400 HEADER: ! r:
52420   pr #255,using 'pos 1,c 10,cc 51': date$("mm/dd/ccyy"),env$('cnam')
52440   pg=pg+1
52460   pr #255,using L2040: time$,"W-2 Supplemental Information Proof List","Page",pg,date$("Month DD, CCYY")
52480   L2040: form pos 1,c 8,cc 52,skip 1,pos 1,c 5,n 3,cc 52,skip 1
52500 return ! /r
52520 XIT: fnxit
68000 ! <Updateable Region: ERTN>
68020 ERTN: fnerror(program$,err,line,act$,"xit")
68040   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
68060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
68080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
68100 ERTN_EXEC_ACT: execute act$ : goto ERTN
68120 ! /region