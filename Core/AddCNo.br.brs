10000 ! Replace S:\Core\AddCNo
10020 ! add a new company program for [cursys] (copies it from 99999) then it chains to Company Information
10040 ! ______________________________________________________________________
10060   library 'S:\Core\Library': fnchain,fnTos,fnLbl,fnmsgbox,fnAcs,fnTxt,fnCmdKey,fntop,fncmbcno,fnCmdSet,fnCopy,fnindex_sys,fnerror,fncreg_write,fnSystemName$
10080   on error goto ERTN
10100   dim ml$(10)*80,resp$(40)*128,cap$*128
10120 ! ______________________________________________________________________
10150   fntop(program$,cap$="Add New "&env$('cursys')&" Company [cno]")
12000   if exists('S:\'&fnSystemName$&'\mstr\*.h99999') then
12020     fnCopy('S:\'&fnSystemName$&'\mstr\*.h99999','[Q]\'&env$('cursys')&'mstr\*.h[cno]')
12040   else if exists('S:\acs'&env$('cursys')&'\mstr\*.h99999') then
12060     fnCopy('S:\acs'&env$('cursys')&'\mstr\*.h99999','[Q]\'&env$('cursys')&'mstr\*.h[cno]')
12080   end if
12100 ! 
18000   if env$('cursys')='CL' then ! r:
18020     mat ml$(5)
18040     ml$(1)='Would you like to import data from an old'
18060     ml$(2)='ACS Accounts Payable system?'
18080     ml$(3)='This is only chance.'
18100     fnmsgbox(mat ml$,resp$,cap$,36)
18120     if resp$='Yes' then let fnchain("S:\acsCL\Conversion\APmstr-Cnv")
18140 ! /r
20000   else if env$('cursys')='UB' then ! r:
20020     fnTos(sn$="ub_add_cno")
20040     mylen=32 : mypos=mylen+2 : lc=0
20180     fnLbl(lc+=1,1,"Copy Service Types from Company:",mylen,1)
20200     fncmbcno(lc,mypos)
20220     resp$(1)=''
20240     fnCmdKey("&Next",1,1,1)
20280     fnAcs(sn$,0,mat resp$,ckey)
20300     if ck=5 then goto XIT
20360     copytoscno=val(resp$(1)(43:47))
20622     if copytoscno=0 then
20624       fnCopy("S:\acsUB\mstr\UBData\*.h99999","[Q]\UBmstr\UBData\*.h[cno]")
20626     else
20628       fnCopy("[Q]\UBmstr\UBData\*.h"&str$(copytoscno),"[Q]\UBmstr\UBData\*.h[cno]")
20630     end if
20660     open #1: "Name=[Q]\UBmstr\Company.h[cno],RecL=129,Replace,Shr",internal,outIn,relative 
20680     write #1,using "Form POS 1,C 40",rec=1: empty$
20700     close #1: 
20720     open #1: "Name=[Q]\UBmstr\Customer.h[cno],Size=0,RecL=2067,Replace",internal,output 
20740     close #1: 
20760     open #2: "Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\ubTrIndx.h[cno],Replace,RecL=102,KPs=1,KLn=19",internal,outIn,keyed 
20780     close #2: 
20800     open #3: "Name=[Q]\UBmstr\ubAdrBil.h[cno],KFName=[Q]\UBmstr\AdrIndex.h[cno],Use,RecL=130,KPs=1,KLn=10",internal,outIn,keyed 
20820     close #3: 
20840     open #1: "Name=[Q]\UBmstr\Cass1.h[cno],RecL=111,Replace",internal,output 
20860     close #1: 
20880     ! open #1: "Name=[Q]\UBmstr\Deposit1.h[cno],KFName=[Q]\UBmstr\DepIdx1.h[cno],Replace,RecL=16,KPs=1,KLn=10",internal,outIn,keyed 
20900     ! close #1: 
20920     open #1: "Name=[Q]\UBmstr\Deposit2.h[cno],Replace,RecL=73",internal,outIn,relative 
20940     close #1: 
20960     open #1: "Name=[Q]\UBmstr\BudMstr.h[cno],KFName=[Q]\UBmstr\BudIdx1.h[cno],Replace,RecL=80,KPs=1,KLn=10",internal,outIn,keyed 
20980     close #1: 
21000     open #1: "Name=[Q]\UBmstr\BudTrans.h[cno],Replace,RecL=149",internal,outIn,relative 
21020     close #1: 
21120     open #1: "Name=[Q]\UBmstr\UBAdrBil.h[cno],KFName=[Q]\UBmstr\NoteIdx1.h[cno],Replace,RecL=130,KPs=1,KLn=10",internal,outIn,keyed 
21140     close #1: 
21160     fncreg_write('Route Low',str$(bkno1)) ! Route Number Range Low
21180     fncreg_write('Route High',str$(bkno2)) ! Route Number Range High
21200 ! ______________________________________________________________________
21220     fnindex_sys(val(env$('cno')),'UB')
21240 ! /r
22000   else if env$('cursys')='GL' then ! r:
22020     dim zer(57)
22040 MENU1: ! 
22060     fnTos(sn$="GLAddCNo")
22080     mylen=37 : mypos=mylen+2
22100     fnLbl(1,1,"Copy Chart of Accounts from Company:",mylen,1)
22120     fncmbcno(1,mypos)
22140     fnCmdSet(2)
22160     fnAcs(sn$,0,mat resp$,ckey)
22180     if ckey=5 then fro_cno=99999: goto L210 ! use company #99999 if no company to copy from
22200     fro_cno=val(resp$(1)(43:47))
22220     if fro_cno=0 then fro_cno=99999
22240 L210: ! 
22260     if cno<1 or cno=fro_cno then goto MENU1
22280 ! ___________________________
22300     execute "Copy [Q]\GLmstr\*.h"&str$(fro_cno)&' '&"[Q]\GLmstr\*.h[cno] -n"
22320     open #20: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],NoShr",internal,outIn,keyed 
22330     do 
22340       read #20,using 'Form POS 87,PD 6.2': cb eof EO_GLMSTR
22360       rewrite #20,using 'Form POS 81,42*PD 6.2,POS 333,2*PD 3,13*pd 6.2': mat zer
22380     loop 
22400 EO_GLMSTR: close #20: 
22420 ! ___________________________
22440     execute "drop [Q]\GLmstr\GLTrans.H[cno]"
22480     open #1: "Name=[Q]\GLmstr\ACTrans.h[cno],Size=0,RecL=72,Replace,NoShr",internal,output 
22500     close #1: 
50000   end if  ! /r
52000   if exists('S:\'&fnSystemName$&'\Company.br') then
52020     fnchain('S:\'&fnSystemName$&'\Company.br') 
52040   else
52060     fnchain('S:\acs'&env$('cursys')&'\Company')
52080   end if
80110 ! <Updateable Region: ERTN>
80120 ERTN: fnerror(program$,err,line,act$,"xit")
80130   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
80140   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
80150   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
80160 ERTN_EXEC_ACT: execute act$ : goto ERTN
80170 ! /region
80180 ! ______________________________________________________________________
80640 XIT: fnchain("S:\Core\Programs\Select Company")
